

using System;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native.Debugging
{
    public class LockInformation
    {
        internal LockInformation(RtlProcessLockInformation lockInfo)
        {
            this.Address = lockInfo.Address;
            this.Type = lockInfo.Type;
            this.OwningThreadId = lockInfo.OwningThread.ToInt32();
            this.LockCount = lockInfo.LockCount;
            this.ContentionCount = lockInfo.ContentionCount;
            this.EntryCount = lockInfo.EntryCount;

            this.RecursionCount = lockInfo.RecursionCount;

            this.SharedWaiters = lockInfo.NumberOfWaitingShared;
            this.ExclusiveWaiters = lockInfo.NumberOfWaitingExclusive;
        }

        public IntPtr Address { get; private set; }
        public RtlLockType Type { get; private set; }
        public int OwningThreadId { get; private set; }
        public int LockCount { get; private set; }
        public int ContentionCount { get; private set; }
        public int EntryCount { get; private set; }

        public int RecursionCount { get; private set; }

        public int SharedWaiters { get; private set; }
        public int ExclusiveWaiters { get; private set; }
    }
}
