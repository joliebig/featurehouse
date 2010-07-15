

using System.Threading;

namespace ProcessHacker.Common
{



    public class FreeList<T>
        where T : IResettable, new()
    {
        private class FreeListEntry<U>
            where U : IResettable, new()
        {
            public U Object;
            public FreeListEntry<U> Next;
        }

        private FreeListEntry<T> _listHead = null;
        private int _count = 0;
        private int _maximumCount = 0;

        public int Count
        {
            get { return _count; }
        }

        public int MaximumCount
        {
            get { return _maximumCount; }
            set { _maximumCount = value; }
        }

        public T Allocate()
        {
            FreeListEntry<T> listHead;



            while (true)
            {
                listHead = _listHead;



                if (listHead == null)
                    break;


                if (Interlocked.CompareExchange<FreeListEntry<T> >(
                    ref _listHead,
                    listHead.Next,
                    listHead
                    ) == listHead)
                {

                    _count--;
                    return listHead.Object;
                }
            }

            return this.AllocateNew();
        }

        private T AllocateNew()
        {
            T obj = new T();
            obj.ResetObject();
            return obj;
        }

        public void Free(T obj)
        {
            FreeListEntry<T> listHead;
            FreeListEntry<T> listEntry;



            if (_count < _maximumCount || _maximumCount == 0)
            {
                listEntry = new FreeListEntry<T>();

                listEntry.Object = obj;


                while (true)
                {
                    listHead = _listHead;
                    listEntry.Next = listHead;

                    if (Interlocked.CompareExchange<FreeListEntry<T> >(
                        ref _listHead,
                        listEntry,
                        listHead
                        ) == listHead)
                    {

                        _count++;
                        break;
                    }
                }
            }
        }
    }
}
