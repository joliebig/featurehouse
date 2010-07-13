using System.Collections;
using System.Collections.Generic;
using System.Threading;
using System;

namespace ProcessHacker.Common.Threading
{
    public class FastStack<T> : IEnumerable<T>
    {
        private class FastStackNode<U>
        {
            public U Value;
            public FastStackNode<U> Next;
        }

        private int _count = 0;
        private FastStackNode<T> _bottom = null;

        public int Count
        {
            get { return _count; }
        }

        public T Peek()
        {
            FastStackNode<T> bottom;

            bottom = _bottom;

            if (bottom == null)
                throw new InvalidOperationException("The stack is empty.");

            return bottom.Value;
        }

        public T Pop()
        {
            FastStackNode<T> bottom;


            while (true)
            {
                bottom = _bottom;



                if (bottom == null)
                    throw new InvalidOperationException("The stack is empty.");


                if (Interlocked.CompareExchange<FastStackNode<T> >(
                    ref _bottom,
                    bottom.Next,
                    bottom
                    ) == bottom)
                {

                    return bottom.Value;
                }
            }
        }

        public void Push(T value)
        {
            FastStackNode<T> bottom;
            FastStackNode<T> entry;

            entry = new FastStackNode<T>();
            entry.Value = value;


            while (true)
            {
                bottom = _bottom;
                entry.Next = bottom;


                if (Interlocked.CompareExchange<FastStackNode<T> >(
                    ref _bottom,
                    entry,
                    bottom
                    ) == bottom)
                {

                    break;
                }
            }
        }

        public IEnumerator<T> GetEnumerator()
        {
            FastStackNode<T> entry;

            entry = _bottom;


            while (entry != null)
            {
                yield return entry.Value;
                entry = entry.Next;
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable<T>)this).GetEnumerator();
        }
    }
}
