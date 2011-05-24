

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using ProcessHacker.Common.Objects;

namespace ProcessHacker.Native
{
    public class MemoryRegion : BaseObject
    {
        private static Dictionary<Type, int> _sizeCache = new Dictionary<Type, int>();

        public static implicit operator IntPtr(MemoryRegion memory)
        {
            return memory.Memory;
        }

        public unsafe static implicit operator void*(MemoryRegion memory)
        {
            return memory.Memory.ToPointer();
        }

        private MemoryRegion _parent;
        private IntPtr _memory;
        private int _size;





        protected MemoryRegion()
        { }

        public MemoryRegion(IntPtr memory)
            : this(memory, 0)
        { }

        public MemoryRegion(IntPtr memory, int offset)
            : this(memory, offset, 0)
        { }

        public MemoryRegion(IntPtr memory, int offset, int size)
            : this(memory.Increment(offset), size, false)
        { }

        protected MemoryRegion(IntPtr memory, int size, bool owned)
            : this(null, memory, size, owned)
        { }

        protected MemoryRegion(MemoryRegion parent, IntPtr memory, int size, bool owned)
            : base(owned)
        {
            if (parent != null)
                parent.Reference();

            _parent = parent;
            _memory = memory;
            _size = size;
        }

        protected sealed override void DisposeObject(bool disposing)
        {
            this.Free();

            if (_parent != null)
                _parent.Dereference(disposing);
        }

        protected virtual void Free()
        { }




        public IntPtr Memory
        {
            get { return _memory; }
            protected set { _memory = value; }
        }

        public MemoryRegion Parent
        {
            get { return _parent; }
        }




        public virtual int Size
        {
            get { return _size; }
            protected set { _size = value; }
        }

        public void Fill(int offset, int length, byte value)
        {
            ProcessHacker.Native.Api.Win32.RtlFillMemory(
                _memory.Increment(offset),
                length.ToIntPtr(),
                value
                );
        }

        public MemoryRegionStream GetStream()
        {
            return new MemoryRegionStream(this);
        }

        private int GetStructSizeCached(Type structType)
        {
            if (!_sizeCache.ContainsKey(structType))
                _sizeCache.Add(structType, Marshal.SizeOf(structType));

            return _sizeCache[structType];
        }

        public MemoryRegion MakeChild(int offset, int size)
        {
            return new MemoryRegion(this, _memory.Increment(offset), size, true);
        }

        public string ReadAnsiString(int offset)
        {
            return Marshal.PtrToStringAnsi(_memory.Increment(offset));
        }

        public string ReadAnsiString(int offset, int length)
        {
            return Marshal.PtrToStringAnsi(_memory.Increment(offset), length);
        }

        public byte[] ReadBytes(int length)
        {
            return this.ReadBytes(0, length);
        }

        public byte[] ReadBytes(int offset, int length)
        {
            byte[] buffer = new byte[length];

            this.ReadBytes(offset, buffer, 0, length);

            return buffer;
        }

        public void ReadBytes(byte[] buffer, int startIndex, int length)
        {
            this.ReadBytes(0, buffer, startIndex, length);
        }

        public void ReadBytes(int offset, byte[] buffer, int startIndex, int length)
        {
            Marshal.Copy(_memory.Increment(offset), buffer, startIndex, length);
        }






        public int ReadInt32(int offset)
        {
            return this.ReadInt32(offset, 0);
        }







        public int ReadInt32(int offset, int index)
        {
            return Marshal.ReadInt32(_memory, offset + index * sizeof(int));
        }

        public IntPtr ReadIntPtr(int offset)
        {
            return this.ReadIntPtr(offset, 0);
        }

        public IntPtr ReadIntPtr(int offset, int index)
        {
            return Marshal.ReadIntPtr(_memory, offset + index * IntPtr.Size);
        }

        public void ReadMemory(IntPtr buffer, int destOffset, int srcOffset, int length)
        {
            ProcessHacker.Native.Api.Win32.RtlMoveMemory(
                buffer.Increment(destOffset),
                _memory.Increment(srcOffset),
                length.ToIntPtr()
                );
        }






        public uint ReadUInt32(int offset)
        {
            return this.ReadUInt32(offset, 0);
        }







        public uint ReadUInt32(int offset, int index)
        {
            return (uint)this.ReadInt32(offset, index);
        }






        public T ReadStruct<T>()
            where T : struct
        {
            return this.ReadStruct<T>(0);
        }
        public T ReadStruct<T>(int index)
            where T : struct
        {
            return this.ReadStruct<T>(0, index);
        }
        public T ReadStruct<T>(int offset, int index)
            where T : struct
        {
            return (T)Marshal.PtrToStructure(
                _memory.Increment(offset + this.GetStructSizeCached(typeof(T)) * index), typeof(T));
        }
        public string ReadUnicodeString(int offset)
        {
            return Marshal.PtrToStringUni(_memory.Increment(offset));
        }
        public string ReadUnicodeString(int offset, int length)
        {
            return Marshal.PtrToStringUni(_memory.Increment(offset), length);
        }
        public void WriteByte(int offset, byte b)
        {
            Marshal.WriteByte(this, offset, b);
        }
        public void WriteBytes(int offset, byte[] b)
        {
            Marshal.Copy(b, 0, _memory.Increment(offset), b.Length);
        }
        public void WriteInt16(int offset, short i)
        {
            Marshal.WriteInt16(this, offset, i);
        }
        public void WriteInt32(int offset, int i)
        {
            Marshal.WriteInt32(this, offset, i);
        }
        public void WriteIntPtr(int offset, IntPtr i)
        {
            Marshal.WriteIntPtr(this, offset, i);
        }
        public void WriteMemory(int destOffset, IntPtr buffer, int srcOffset, int length)
        {
            ProcessHacker.Native.Api.Win32.RtlMoveMemory(
                _memory.Increment(destOffset),
                buffer.Increment(srcOffset),
                length.ToIntPtr()
                );
        }
        public void WriteStruct<T>(T s)
            where T : struct
        {
            this.WriteStruct<T>(0, s);
        }
        public void WriteStruct<T>(int index, T s)
            where T : struct
        {
            this.WriteStruct<T>(0, index, s);
        }
        public void WriteStruct<T>(int offset, int index, T s)
            where T : struct
        {
            Marshal.StructureToPtr(s,
                _memory.Increment(offset + this.GetStructSizeCached(typeof(T)) * index), false);
        }
        public void WriteUnicodeString(int offset, string s)
        {
            byte[] b = UnicodeEncoding.Unicode.GetBytes(s);
            for (int i = 0; i < b.Length; i++)
                Marshal.WriteByte(this.Memory, offset + i, b[i]);
        }
        public void Zero(int offset, int length)
        {
            ProcessHacker.Native.Api.Win32.RtlZeroMemory(
                _memory.Increment(offset),
                length.ToIntPtr()
                );
        }
    }
}
