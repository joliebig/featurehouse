using System;
using System.Collections;
using System.Collections.Generic;

namespace ProcessHacker.Common.Threading
{
    public class FastQueue<T> : IEnumerable<T>
    {
        private class FastQueueNode<U>
        {
            public U Value;
            public FastQueueNode<U> Next;
        }

        private int _count = 0;


        private FastQueueNode<T> _head;

        private FastQueueNode<T> _tail;



        public FastQueue()
        {
            _head = new FastQueueNode<T>();
            _tail = _head;
            _tail.Next = null;
        }

        public int Count
        {
            get { return _count; }
        }

        public T Dequeue()
        {
            throw new NotImplementedException();
        }

        public void Enqueue(T value)
        {
            throw new NotImplementedException();
        }
        public IEnumerator<T> GetEnumerator()
        {
            return null;
        }
        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable<T>)this).GetEnumerator();
        }
    }
}
