

using System;
using System.ComponentModel;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native
{



    public sealed class LsaMemoryAlloc : MemoryAlloc
    {
        private bool _secur32;

        public LsaMemoryAlloc(IntPtr memory)
            : this(memory, false)
        { }

        public LsaMemoryAlloc(IntPtr memory, bool secur32)
            : this(memory, secur32, true)
        { }







        public LsaMemoryAlloc(IntPtr memory, bool secur32, bool owned)
            : base(memory, owned)
        {
            _secur32 = secur32;
        }

        protected override void Free()
        {
            if (!_secur32)
                Win32.LsaFreeMemory(this);
            else
                Win32.LsaFreeReturnBuffer(this);
        }

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override void Resize(int newSize)
        {
            throw new NotSupportedException();
        }

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int Size
        {
            get
            {
                throw new NotSupportedException();
            }
        }
    }
}
