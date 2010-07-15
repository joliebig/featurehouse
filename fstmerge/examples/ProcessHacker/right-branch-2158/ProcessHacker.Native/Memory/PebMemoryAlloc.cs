

using System;
using System.ComponentModel;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native
{



    public sealed class PebMemoryAlloc : MemoryAlloc
    {
        public PebMemoryAlloc(int size)
        {
            NtStatus status;
            IntPtr block;

            if ((status = Win32.RtlAllocateFromPeb(size, out block)) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            this.Memory = block;
            this.Size = size;
        }

        protected override void Free()
        {
            NtStatus status;

            if ((status = Win32.RtlFreeToPeb(this, this.Size)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override void Resize(int newSize)
        {
            throw new NotSupportedException();
        }
    }
}
