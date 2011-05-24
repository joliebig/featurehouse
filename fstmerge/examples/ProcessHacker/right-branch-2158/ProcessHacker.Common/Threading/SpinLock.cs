

using System;
using System.Threading;

namespace ProcessHacker.Common.Threading
{



    public sealed class SpinLock
    {
        public struct SpinLockContext : IDisposable
        {
            private bool _disposed;
            private SpinLock _spinLock;

            internal SpinLockContext(SpinLock spinLock)
            {
                _spinLock = spinLock;
                _spinLock.Acquire();
                _disposed = false;
            }

            public void Dispose()
            {
                if (!_disposed)
                {
                    _spinLock.Release();
                    _disposed = true;
                }
            }
        }

        private int _value = 0;
        private bool _spin;
        private int _acquireCount = 0;
        private int _spinCount = 0;




        public SpinLock()
        {

            if (Environment.ProcessorCount == 1)
                _spin = false;
            else
                _spin = true;
        }




        public void Acquire()
        {
            Thread.BeginCriticalRegion();

            Interlocked.Increment(ref _acquireCount);

            if (_spin)
            {
                while (Interlocked.CompareExchange(ref _value, 1, 0) == 1)
                    Thread.SpinWait((_spinCount++ % Thread.VolatileRead(ref _acquireCount)) + 1);
            }
            else
            {
                while (Interlocked.CompareExchange(ref _value, 1, 0) == 1)
                    Thread.Sleep(0);
            }

            Thread.EndCriticalRegion();
        }





        public SpinLockContext AcquireContext()
        {
            return new SpinLockContext(this);
        }




        public void Release()
        {
            Thread.BeginCriticalRegion();
            Interlocked.Exchange(ref _value, 0);
            Interlocked.Decrement(ref _acquireCount);
            Thread.EndCriticalRegion();
        }
    }
}
