using System;
using System.Threading;

namespace ProcessHacker.Common.Threading
{



    public sealed class RundownProtection
    {
        private object _rundownLock = new object();
        private volatile bool _rundownActive = false;
        private int _refCount = 0;





        public bool Acquire()
        {
            Thread.BeginCriticalRegion();

            try
            {
                lock (_rundownLock)
                {
                    if (_rundownActive)
                        return false;

                    Interlocked.Increment(ref _refCount);

                    return true;
                }
            }
            finally
            {
                Thread.EndCriticalRegion();
            }
        }




        public void Release()
        {
            Thread.BeginCriticalRegion();

            try
            {
                lock (_rundownLock)
                {
                    int newRefCount = Interlocked.Decrement(ref _refCount);

                    if (newRefCount < 0)
                        throw new InvalidOperationException("Reference count cannot be negative.");

                    if (_rundownActive)
                    {

                        if (newRefCount == 0)
                            Monitor.PulseAll(_rundownLock);
                    }
                }
            }
            finally
            {
                Thread.EndCriticalRegion();
            }
        }





        public void Wait()
        {
            this.Wait(-1);
        }







        public bool Wait(int timeout)
        {
            lock (_rundownLock)
            {
                _rundownActive = true;


                if (Thread.VolatileRead(ref _refCount) == 0)
                    return true;


                return Monitor.Wait(_rundownLock, timeout);
            }
        }
    }
}
