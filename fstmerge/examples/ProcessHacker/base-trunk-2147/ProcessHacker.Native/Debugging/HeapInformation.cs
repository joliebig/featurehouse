

using System;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native.Debugging
{
    public class HeapInformation
    {
        internal HeapInformation(RtlHeapInformation heapInfo)
        {
            this.Address = heapInfo.BaseAddress;
            this.BytesAllocated = heapInfo.BytesAllocated.ToInt64();
            this.BytesCommitted = heapInfo.BytesCommitted.ToInt64();
            this.TagCount = heapInfo.NumberOfTags;
            this.EntryCount = heapInfo.NumberOfEntries;
            this.PseudoTagCount = heapInfo.NumberOfPseudoTags;
        }

        public HeapInformation(
            IntPtr address,
            long bytesAllocated,
            long bytesCommitted,
            int tagCount,
            int entryCount,
            int pseudoTagCount)
        {
            this.Address = address;
            this.BytesAllocated = bytesAllocated;
            this.BytesCommitted = bytesCommitted;
            this.TagCount = tagCount;
            this.EntryCount = entryCount;
            this.PseudoTagCount = pseudoTagCount;
        }

        public IntPtr Address { get; private set; }
        public long BytesAllocated { get; private set; }
        public long BytesCommitted { get; private set; }
        public int TagCount { get; private set; }
        public int EntryCount { get; private set; }
        public int PseudoTagCount { get; private set; }
    }
}
