

using System;
using System.ComponentModel;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native
{



    public class WtsMemoryAlloc : MemoryAlloc
    {
        public WtsMemoryAlloc(IntPtr memory)
            : this(memory, true)
        { }






        public WtsMemoryAlloc(IntPtr memory, bool owned)
            : base(memory, owned)
        { }

        protected override void Free()
        {
            Win32.WTSFreeMemory(this);
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
