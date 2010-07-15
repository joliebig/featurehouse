using System;
using System.Text;
using System.IO;

namespace Be.Windows.Forms
{







    public sealed class DynamicFileByteProvider : IByteProvider, IDisposable
    {
        const int COPY_BLOCK_SIZE = 4096;

        string _fileName;
        FileStream _fileStream;
        DataMap _dataMap;
        long _totalLength;
        bool _readOnly;





        public DynamicFileByteProvider(string fileName) : this(fileName, false)
        {}





        public DynamicFileByteProvider(string fileName, bool readOnly)
        {
            _fileName = fileName;

            if (!readOnly)
            {
                _fileStream = File.Open(fileName, FileMode.Open, FileAccess.ReadWrite, FileShare.Read);
            }
            else
            {
                _fileStream = File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
            }

            _readOnly = readOnly;

            ReInitialize();
        }





        public event EventHandler LengthChanged;




        public event EventHandler Changed;




        public byte ReadByte(long index)
        {
            long blockOffset;
            DataBlock block = GetDataBlock(index, out blockOffset);
            FileDataBlock fileBlock = block as FileDataBlock;
            if (fileBlock != null)
            {
                return ReadByteFromFile(fileBlock.FileOffset + index - blockOffset);
            }
            else
            {
                MemoryDataBlock memoryBlock = (MemoryDataBlock)block;
                return memoryBlock.Data[index - blockOffset];
            }
        }




        public void WriteByte(long index, byte value)
        {
            try
            {

                long blockOffset;
                DataBlock block = GetDataBlock(index, out blockOffset);


                MemoryDataBlock memoryBlock = block as MemoryDataBlock;
                if (memoryBlock != null)
                {
                    memoryBlock.Data[index - blockOffset] = value;
                    return;
                }

                FileDataBlock fileBlock = (FileDataBlock)block;


                if (blockOffset == index && block.PreviousBlock != null)
                {
                    MemoryDataBlock previousMemoryBlock = block.PreviousBlock as MemoryDataBlock;
                    if (previousMemoryBlock != null)
                    {
                        previousMemoryBlock.AddByteToEnd(value);
                        fileBlock.RemoveBytesFromStart(1);
                        if (fileBlock.Length == 0)
                        {
                            _dataMap.Remove(fileBlock);
                        }
                        return;
                    }
                }


                if (blockOffset + fileBlock.Length - 1 == index && block.NextBlock != null)
                {
                    MemoryDataBlock nextMemoryBlock = block.NextBlock as MemoryDataBlock;
                    if (nextMemoryBlock != null)
                    {
                        nextMemoryBlock.AddByteToStart(value);
                        fileBlock.RemoveBytesFromEnd(1);
                        if (fileBlock.Length == 0)
                        {
                            _dataMap.Remove(fileBlock);
                        }
                        return;
                    }
                }


                FileDataBlock prefixBlock = null;
                if (index > blockOffset)
                {
                    prefixBlock = new FileDataBlock(fileBlock.FileOffset, index - blockOffset);
                }

                FileDataBlock suffixBlock = null;
                if (index < blockOffset + fileBlock.Length - 1)
                {
                    suffixBlock = new FileDataBlock(
                        fileBlock.FileOffset + index - blockOffset + 1,
                        fileBlock.Length - (index - blockOffset + 1));
                }

    block = _dataMap.Replace(block, new MemoryDataBlock(value));

                if (prefixBlock != null)
                {
                    _dataMap.AddBefore(block, prefixBlock);
                }

                if (suffixBlock != null)
                {
                    _dataMap.AddAfter(block, suffixBlock);
                }
            }
            finally
            {
                OnChanged(EventArgs.Empty);
            }
        }




        public void InsertBytes(long index, byte[] bs)
        {
            try
            {

                long blockOffset;
                DataBlock block = GetDataBlock(index, out blockOffset);


                MemoryDataBlock memoryBlock = block as MemoryDataBlock;
                if (memoryBlock != null)
                {
                    memoryBlock.InsertBytes(index - blockOffset, bs);
                    return;
                }

                FileDataBlock fileBlock = (FileDataBlock)block;


                if (blockOffset == index && block.PreviousBlock != null)
                {
                    MemoryDataBlock previousMemoryBlock = block.PreviousBlock as MemoryDataBlock;
                    if (previousMemoryBlock != null)
                    {
                        previousMemoryBlock.InsertBytes(previousMemoryBlock.Length, bs);
                        return;
                    }
                }


                FileDataBlock prefixBlock = null;
                if (index > blockOffset)
                {
                    prefixBlock = new FileDataBlock(fileBlock.FileOffset, index - blockOffset);
                }

                FileDataBlock suffixBlock = null;
                if (index < blockOffset + fileBlock.Length)
                {
                    suffixBlock = new FileDataBlock(
                        fileBlock.FileOffset + index - blockOffset,
                        fileBlock.Length - (index - blockOffset));
                }

    block = _dataMap.Replace(block, new MemoryDataBlock(bs));

                if (prefixBlock != null)
                {
                    _dataMap.AddBefore(block, prefixBlock);
                }

                if (suffixBlock != null)
                {
                    _dataMap.AddAfter(block, suffixBlock);
                }
            }
            finally
            {
                _totalLength += bs.Length;
                OnLengthChanged(EventArgs.Empty);
                OnChanged(EventArgs.Empty);
            }
        }




        public void DeleteBytes(long index, long length)
        {
            try
            {
                long bytesToDelete = length;


                long blockOffset;
                DataBlock block = GetDataBlock(index, out blockOffset);


                while (bytesToDelete > 0)
                {
                    long blockLength = block.Length;
                    DataBlock nextBlock = block.NextBlock;


                    long count = Math.Min(bytesToDelete, blockLength - (index - blockOffset));
                    block.RemoveBytes(index - blockOffset, count);

                    if (block.Length == 0)
                    {
                        _dataMap.Remove(block);
                        if (_dataMap.FirstBlock == null)
                        {
                            _dataMap.AddFirst(new MemoryDataBlock(new byte[0]));
                        }
                    }

                    bytesToDelete -= count;
                    blockOffset += block.Length;
                    block = (bytesToDelete > 0) ? nextBlock : null;
                }
            }
            finally
            {
                _totalLength -= length;
                OnLengthChanged(EventArgs.Empty);
                OnChanged(EventArgs.Empty);
            }
        }




        public long Length
        {
            get
            {
                return _totalLength;
            }
        }




        public bool HasChanges()
        {
            if (_readOnly)
                return false;

            if (_totalLength != _fileStream.Length)
            {
                return true;
            }

            long offset = 0;
            for (DataBlock block = _dataMap.FirstBlock; block != null; block = block.NextBlock)
            {
                FileDataBlock fileBlock = block as FileDataBlock;
                if (fileBlock == null)
                {
                    return true;
                }

                if (fileBlock.FileOffset != offset)
                {
                    return true;
                }

                offset += fileBlock.Length;
            }
            return (offset != _fileStream.Length);
        }




        public void ApplyChanges()
        {
            if (_readOnly)
                throw new OperationCanceledException("File is in read-only mode");





            if (_totalLength > _fileStream.Length)
            {
                _fileStream.SetLength(_totalLength);
            }


            long dataOffset = 0;
            for (DataBlock block = _dataMap.FirstBlock; block != null; block = block.NextBlock)
            {
                FileDataBlock fileBlock = block as FileDataBlock;
                if (fileBlock != null && fileBlock.FileOffset != dataOffset)
                {
                    MoveFileBlock(fileBlock, dataOffset);
                }
                dataOffset += block.Length;
            }


            dataOffset = 0;
            for (DataBlock block = _dataMap.FirstBlock; block != null; block = block.NextBlock)
            {
                MemoryDataBlock memoryBlock = block as MemoryDataBlock;
                if (memoryBlock != null)
                {
                    _fileStream.Position = dataOffset;
                    for (int memoryOffset = 0; memoryOffset < memoryBlock.Length; memoryOffset += COPY_BLOCK_SIZE)
                    {
                        _fileStream.Write(memoryBlock.Data, memoryOffset, (int)Math.Min(COPY_BLOCK_SIZE, memoryBlock.Length - memoryOffset));
                    }
                }
                dataOffset += block.Length;
            }


            _fileStream.SetLength(_totalLength);
            ReInitialize();
        }




        public bool SupportsWriteByte()
        {
            return !_readOnly;
        }




        public bool SupportsInsertBytes()
        {
            return !_readOnly;
        }




        public bool SupportsDeleteBytes()
        {
            return !_readOnly;
        }






        ~DynamicFileByteProvider()
        {
            Dispose();
        }




        public void Dispose()
        {
            if (_fileStream != null)
            {
                _fileStream.Close();
                _fileStream = null;
            }
            _fileName = null;
            _dataMap = null;
            GC.SuppressFinalize(this);
        }


        public bool ReadOnly
        {
            get { return _readOnly; }
            set { _readOnly = value; }
        }

        void OnLengthChanged(EventArgs e)
        {
            if (LengthChanged != null)
                LengthChanged(this, e);
        }

        void OnChanged(EventArgs e)
        {
            if (Changed != null)
            {
                Changed(this, e);
            }
        }

        DataBlock GetDataBlock(long findOffset, out long blockOffset)
        {
            if (findOffset < 0 || findOffset > _totalLength)
            {
                throw new ArgumentOutOfRangeException("index");
            }


            blockOffset = 0;
            for (DataBlock block = _dataMap.FirstBlock; block != null; block = block.NextBlock)
            {
                if ((blockOffset <= findOffset && blockOffset + block.Length > findOffset) || block.NextBlock == null)
                {
                    return block;
                }
                blockOffset += block.Length;
            }
            return null;
        }

        FileDataBlock GetNextFileDataBlock(DataBlock block, long dataOffset, out long nextDataOffset)
        {

            nextDataOffset = dataOffset + block.Length;
            block = block.NextBlock;
            while (block != null)
            {
                FileDataBlock fileBlock = block as FileDataBlock;
                if (fileBlock != null)
                {
                    return fileBlock;
                }
                nextDataOffset += block.Length;
                block = block.NextBlock;
            }
            return null;
        }

        byte ReadByteFromFile(long fileOffset)
        {

            if (_fileStream.Position != fileOffset)
            {
                _fileStream.Position = fileOffset;
            }
            return (byte)_fileStream.ReadByte();
        }

        void MoveFileBlock(FileDataBlock fileBlock, long dataOffset)
        {

            long nextDataOffset;
   FileDataBlock nextFileBlock = GetNextFileDataBlock(fileBlock, dataOffset, out nextDataOffset);
            if (nextFileBlock != null && dataOffset + fileBlock.Length > nextFileBlock.FileOffset)
            {

                MoveFileBlock(nextFileBlock, nextDataOffset);
            }


            if (fileBlock.FileOffset > dataOffset)
            {

                byte[] buffer = new byte[COPY_BLOCK_SIZE];
                for (long relativeOffset = 0; relativeOffset < fileBlock.Length; relativeOffset += buffer.Length)
                {
                    long readOffset = fileBlock.FileOffset + relativeOffset;
                    int bytesToRead = (int)Math.Min(buffer.Length, fileBlock.Length - relativeOffset);
                    _fileStream.Position = readOffset;
                    _fileStream.Read(buffer, 0, bytesToRead);

                    long writeOffset = dataOffset + relativeOffset;
                    _fileStream.Position = writeOffset;
                    _fileStream.Write(buffer, 0, bytesToRead);
                }
            }
            else
            {

                byte[] buffer = new byte[COPY_BLOCK_SIZE];
                for (long relativeOffset = 0; relativeOffset < fileBlock.Length; relativeOffset += buffer.Length)
                {
                    int bytesToRead = (int)Math.Min(buffer.Length, fileBlock.Length - relativeOffset);
                    long readOffset = fileBlock.FileOffset + fileBlock.Length - relativeOffset - bytesToRead;
                    _fileStream.Position = readOffset;
                    _fileStream.Read(buffer, 0, bytesToRead);

                    long writeOffset = dataOffset + fileBlock.Length - relativeOffset - bytesToRead;
                    _fileStream.Position = writeOffset;
                    _fileStream.Write(buffer, 0, bytesToRead);
                }
            }


   fileBlock.SetFileOffset(dataOffset);
        }

        void ReInitialize()
        {
            _dataMap = new DataMap();
            _dataMap.AddFirst(new FileDataBlock(0, _fileStream.Length));
            _totalLength = _fileStream.Length;
        }
    }
}
