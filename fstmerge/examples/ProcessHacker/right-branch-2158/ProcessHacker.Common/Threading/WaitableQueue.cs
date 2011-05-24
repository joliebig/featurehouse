using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace ProcessHacker.Common.Threading
{
    public class WaitableQueue<T> : IEnumerable, IEnumerable<T>
    {
        private Queue<T> _queue = new Queue<T>();
        private SemaphorePair _pair;

        public WaitableQueue()
            : this(int.MaxValue)
        { }

        public WaitableQueue(int maximumCount)
        {
            _pair = new SemaphorePair(maximumCount);
        }

        public int Count
        {
            get { return _queue.Count; }
        }

        public void Clear()
        {
            lock (_queue)
                _queue.Clear();
        }

        public bool Contains(T item)
        {
            lock (_queue)
                return _queue.Contains(item);
        }

        public T Dequeue()
        {

            _pair.WaitRead();

            _pair.ReleaseWrite();

            lock (_queue)
                return _queue.Dequeue();
        }

        public bool Dequeue(int timeout, out T item)
        {
            bool waitResult = true;


            waitResult = _pair.WaitRead(timeout);



            if (waitResult)
            {
                lock (_queue)
                    item = _queue.Dequeue();



                _pair.ReleaseWrite();
            }
            else
            {
                item = default(T);
            }

            return waitResult;
        }

        public void Enqueue(T item)
        {

            _pair.WaitWrite();


            lock (_queue)
                _queue.Enqueue(item);


            _pair.ReleaseRead();
        }

        public IEnumerator GetEnumerator()
        {
            return _queue.GetEnumerator();
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return _queue.GetEnumerator();
        }

        public T[] ToArray()
        {
            lock (_queue)
                return _queue.ToArray();
        }

        public void TrimExcess()
        {
            lock (_queue)
                _queue.TrimExcess();
        }
    }
}
