using System;
using System.Threading;

namespace ProcessHacker.Common.Threading
{





    public sealed class FastMutex
    {



        public struct FastMutexContext : IDisposable
        {
            private bool _disposed;
            private FastMutex _fastMutex;

            internal FastMutexContext(FastMutex fastMutex)
            {
                _fastMutex = fastMutex;
                _disposed = false;
            }




            public void Dispose()
            {
                if (!_disposed)
                {
                    _fastMutex.Release();
                    _disposed = true;
                }
            }
        }

        private object _lock = new object();






        public void Acquire()
        {
            Monitor.Enter(_lock);
        }






        public FastMutexContext AcquireContext()
        {
            this.Acquire();
            return new FastMutexContext(this);
        }




        public void Release()
        {
            Monitor.Exit(_lock);
        }






        public bool TryAcquire()
        {
            return Monitor.TryEnter(_lock);
        }







        public bool TryAcquire(int millisecondsTimeout)
        {
            return Monitor.TryEnter(_lock, millisecondsTimeout);
        }
    }
}
