

using System;
using System.Collections.Generic;
using ProcessHacker.Common.Objects;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;

namespace ProcessHacker.Native.Debugging
{
    public delegate bool DebugEnumHeapsDelegate(HeapInformation heapInfo);
    public delegate bool DebugEnumLocksDelegate(LockInformation lockInfo);
    public delegate bool DebugEnumModulesDelegate(ModuleInformation moduleInfo);




    public sealed class DebugBuffer : BaseObject
    {
        private IntPtr _buffer;




        public DebugBuffer()
        {
            _buffer = Win32.RtlCreateQueryDebugBuffer(0, true);

            if (_buffer == IntPtr.Zero)
            {
                this.DisableOwnership(false);
                throw new WindowsException(NtStatus.Unsuccessful);
            }
        }

        protected override void DisposeObject(bool disposing)
        {
            Win32.RtlDestroyQueryDebugBuffer(_buffer);
        }





        public void EnumHeaps(DebugEnumHeapsDelegate callback)
        {
            var debugInfo = this.GetDebugInformation();

            if (debugInfo.Heaps == IntPtr.Zero)
                throw new InvalidOperationException("Heap information does not exist.");

            MemoryRegion heapInfo = new MemoryRegion(debugInfo.Heaps);
            var heaps = heapInfo.ReadStruct<RtlProcessHeaps>();

            for (int i = 0; i < heaps.NumberOfHeaps; i++)
            {
                var heap = heapInfo.ReadStruct<RtlHeapInformation>(RtlProcessHeaps.HeapsOffset, i);

                if (!callback(new HeapInformation(heap)))
                    break;
            }
        }





        public void EnumLocks(DebugEnumLocksDelegate callback)
        {
            var debugInfo = this.GetDebugInformation();

            if (debugInfo.Locks == IntPtr.Zero)
                throw new InvalidOperationException("Lock information does not exist.");

            MemoryRegion locksInfo = new MemoryRegion(debugInfo.Locks);
            var locks = locksInfo.ReadStruct<RtlProcessLocks>();

            for (int i = 0; i < locks.NumberOfLocks; i++)
            {
                var lock_ = locksInfo.ReadStruct<RtlProcessLockInformation>(sizeof(int), i);

                if (!callback(new LockInformation(lock_)))
                    break;
            }
        }





        public void EnumModules(DebugEnumModulesDelegate callback)
        {
            var debugInfo = this.GetDebugInformation();

            if (debugInfo.Modules == IntPtr.Zero)
                throw new InvalidOperationException("Module information does not exist.");

            MemoryRegion modulesInfo = new MemoryRegion(debugInfo.Modules);
            var modules = modulesInfo.ReadStruct<RtlProcessModules>();

            for (int i = 0; i < modules.NumberOfModules; i++)
            {
                var module = modulesInfo.ReadStruct<RtlProcessModuleInformation>(RtlProcessModules.ModulesOffset, i);

                if (!callback(new ModuleInformation(module)))
                    break;
            }
        }





        private RtlDebugInformation GetDebugInformation()
        {
            MemoryRegion data = new MemoryRegion(_buffer);

            return data.ReadStruct<RtlDebugInformation>();
        }





        public HeapInformation[] GetHeaps()
        {
            List<HeapInformation> heaps = new List<HeapInformation>();

            this.EnumHeaps((heap) =>
            {
                heaps.Add(heap);
                return true;
            });

            return heaps.ToArray();
        }





        public LockInformation[] GetLocks()
        {
            List<LockInformation> locks = new List<LockInformation>();

            this.EnumLocks((lock_) =>
            {
                locks.Add(lock_);
                return true;
            });

            return locks.ToArray();
        }





        public ModuleInformation[] GetModules()
        {
            List<ModuleInformation> modules = new List<ModuleInformation>();

            this.EnumModules((module) =>
            {
                modules.Add(module);
                return true;
            });

            return modules.ToArray();
        }





        public void Query(RtlQueryProcessDebugFlags flags)
        {
            this.Query(ProcessHandle.GetCurrentId(), flags);
        }






        public void Query(int pid, RtlQueryProcessDebugFlags flags)
        {
            NtStatus status;

            if ((status = Win32.RtlQueryProcessDebugInformation(
                pid.ToIntPtr(),
                flags,
                _buffer
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }




        public void QueryBackTraces()
        {
            NtStatus status;

            if ((status = Win32.RtlQueryProcessBackTraceInformation(_buffer)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }




        public void QueryHeaps()
        {
            NtStatus status;

            if ((status = Win32.RtlQueryProcessHeapInformation(_buffer)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }




        public void QueryLocks()
        {
            NtStatus status;

            if ((status = Win32.RtlQueryProcessLockInformation(_buffer)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
    }
}
