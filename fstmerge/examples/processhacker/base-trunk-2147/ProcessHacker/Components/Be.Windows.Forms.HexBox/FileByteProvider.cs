

using System;
using System.IO;
using System.Collections;

namespace Be.Windows.Forms
{



 public class FileByteProvider : IByteProvider, IDisposable
 {




  class WriteCollection : DictionaryBase
  {



   public byte this[long index]
   {
    get { return (byte)this.Dictionary[index]; }
    set { Dictionary[index] = value; }
   }






   public void Add(long index, byte value)
   { Dictionary.Add(index, value); }






   public bool Contains(long index)
   { return Dictionary.Contains(index); }

  }





  public event EventHandler Changed;




  WriteCollection _writes = new WriteCollection();




  string _fileName;



  FileStream _fileStream;



        bool _readOnly;





  public FileByteProvider(string fileName)
  {
   _fileName = fileName;

            try
            {

                _fileStream = File.Open(fileName, FileMode.Open, FileAccess.ReadWrite, FileShare.Read);
            }
            catch
            {

                try
                {
                    _fileStream = File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
                    _readOnly = true;
                }
                catch
                {
                    throw;
                }
            }
  }




  ~FileByteProvider()
  {
   Dispose();
  }





  void OnChanged(EventArgs e)
  {
   if(Changed != null)
    Changed(this, e);
  }




  public string FileName
  {
   get { return _fileName; }
  }





  public bool HasChanges()
  {
   return (_writes.Count > 0);
  }




  public void ApplyChanges()
  {
            if (this._readOnly)
            {
                throw new Exception("File is in read-only mode.");
            }

   if(!HasChanges())
    return;

   IDictionaryEnumerator en = _writes.GetEnumerator();
   while(en.MoveNext())
   {
    long index = (long)en.Key;
    byte value = (byte)en.Value;
    if(_fileStream.Position != index)
     _fileStream.Position = index;
    _fileStream.Write(new byte[]{value}, 0, 1);
   }
   _writes.Clear();
  }




  public void RejectChanges()
  {
   _writes.Clear();
  }






        public event EventHandler LengthChanged;






  public byte ReadByte(long index)
  {
   if(_writes.Contains(index))
    return _writes[index];

   if(_fileStream.Position != index)
    _fileStream.Position = index;

   byte res = (byte)_fileStream.ReadByte();
   return res;
  }




  public long Length
  {
   get
   {
    return _fileStream.Length;
   }
  }




  public void WriteByte(long index, byte value)
  {
   if(_writes.Contains(index))
    _writes[index] = value;
   else
    _writes.Add(index, value);

   OnChanged(EventArgs.Empty);
  }




  public void DeleteBytes(long index, long length)
  {
   throw new NotSupportedException("FileByteProvider.DeleteBytes");
  }




  public void InsertBytes(long index, byte[] bs)
  {
   throw new NotSupportedException("FileByteProvider.InsertBytes");
  }




  public bool SupportsWriteByte()
  {
   return !_readOnly;
  }




  public bool SupportsInsertBytes()
  {
   return false;
  }




  public bool SupportsDeleteBytes()
  {
   return false;
  }






  public void Dispose()
  {
   if(_fileStream != null)
   {
    _fileName = null;

    _fileStream.Close();
    _fileStream = null;
   }

   GC.SuppressFinalize(this);
  }

 }
}
