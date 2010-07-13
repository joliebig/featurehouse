



using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using ProcessHacker.Common.Objects;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native
{



    public class MemoryAlloc : MemoryRegion
    {
        private static int _allocatedCount = 0;
        private static int _freedCount = 0;
        private static int _reallocatedCount = 0;


        private static Heap _privateHeap = new Heap(HeapFlags.Class1 | HeapFlags.Growable);
        private static Heap _processHeap = Heap.GetDefault();

        public static int AllocatedCount
        {
            get { return _allocatedCount; }
        }

        public static new int FreedCount
        {
            get { return _freedCount; }
        }

        public static Heap PrivateHeap
        {
            get { return _privateHeap; }
        }

        public static int ReallocatedCount
        {
            get { return _reallocatedCount; }
        }





        protected MemoryAlloc()
            : base()
        { }

        public MemoryAlloc(IntPtr memory)
            : this(memory, true)
        { }

        public MemoryAlloc(IntPtr memory, bool owned)
            : this(memory, 0, owned)
        { }

        public MemoryAlloc(IntPtr memory, int size, bool owned)
            : base(memory, size, owned)
        { }





        public MemoryAlloc(int size)
            : this(size, 0)
        { }






        public MemoryAlloc(int size, HeapFlags flags)
        {
            this.Memory = _privateHeap.Allocate(flags, size);
            this.Size = size;




        }

        protected override void Free()
        {
            _privateHeap.Free(0, this);




        }





        public virtual void Resize(int newSize)
        {
            this.Memory = _privateHeap.Reallocate(0, this.Memory, newSize);
            this.Size = newSize;




        }
    }
}
