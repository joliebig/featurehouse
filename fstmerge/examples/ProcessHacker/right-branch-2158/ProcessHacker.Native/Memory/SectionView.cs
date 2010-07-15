

using System;
using System.ComponentModel;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;

namespace ProcessHacker.Native
{



    public sealed class SectionView : MemoryAlloc
    {
        internal SectionView(IntPtr baseAddress, IntPtr commitSize)
        {
            this.Memory = baseAddress;
            this.Size = commitSize.ToInt32();
        }

        protected override void Free()
        {
            NtStatus status;

            if ((status = Win32.NtUnmapViewOfSection(ProcessHandle.Current, this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }





        public NtStatus Flush()
        {
            return ProcessHandle.Current.FlushMemory(this, this.Size);
        }







        public bool IsSameFile(SectionView mappedAsFile)
        {
            if ((uint)Win32.NtAreMappedFilesTheSame(this, mappedAsFile) == this.Memory.ToUInt32())
                return true;
            else
                return false;
        }

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override void Resize(int newSize)
        {
            throw new NotSupportedException();
        }
    }
}
