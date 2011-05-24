using System;
using ProcessHacker.Common.Objects;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;

namespace ProcessHacker.Native.Memory
{



    public sealed class PhysicalPages : BaseObject
    {
        private ProcessHandle _processHandle;
        private int _count;
        private IntPtr[] _pfnArray;





        public PhysicalPages(int pageCount)
            : this(pageCount, true)
        { }
        public PhysicalPages(int count, bool pages)
            : this(ProcessHandle.Current, count, pages)
        { }
        public PhysicalPages(ProcessHandle processHandle, int pageCount)
            : this(processHandle, pageCount, true)
        { }
        public PhysicalPages(ProcessHandle processHandle, int count, bool pages)
        {
            if (pages)
                _count = count;
            else
                _count = Windows.BytesToPages(count);
            IntPtr pageCount = new IntPtr(_count);
            _pfnArray = new IntPtr[_count];
            if (!Win32.AllocateUserPhysicalPages(processHandle, ref pageCount, _pfnArray))
                Win32.ThrowLastError();
            if (pageCount.ToInt32() != _count)
                throw new Exception("Could not allocate all pages.");
            _processHandle = processHandle;
            _processHandle.Reference();
        }
        protected override void DisposeObject(bool disposing)
        {
            IntPtr freedPages = new IntPtr(_count);
            _processHandle.Dereference();
            if (!Win32.FreeUserPhysicalPages(_processHandle, ref freedPages, _pfnArray))
                Win32.ThrowLastError();
            if (freedPages.ToInt32() != _count)
                throw new Exception("Could not free all pages.");
        }
        public PhysicalPagesMapping Map(MemoryProtection protection)
        {
            return this.Map(IntPtr.Zero, protection);
        }
        public PhysicalPagesMapping Map(IntPtr address, MemoryProtection protection)
        {
            IntPtr allocAddress = ProcessHandle.Current.AllocateMemory(
                address,
                _count * Windows.PageSize,
                MemoryFlags.Reserve | MemoryFlags.Physical,
                protection
                );
            if (!Win32.MapUserPhysicalPages(
                allocAddress,
                new IntPtr(_count),
                _pfnArray
                ))
                Win32.ThrowLastError();
            return new PhysicalPagesMapping(this, allocAddress);
        }
        internal void Unmap(IntPtr address)
        {
            if (!Win32.MapUserPhysicalPages(
                 address,
                 new IntPtr(_count),
                 null
                 ))
                Win32.ThrowLastError();
            ProcessHandle.Current.FreeMemory(address, 0, false);
        }
    }
}
