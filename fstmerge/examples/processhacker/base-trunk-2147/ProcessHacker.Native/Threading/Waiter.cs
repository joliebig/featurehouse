

using System.Collections.Generic;
using System.Threading;
using ProcessHacker.Common.Objects;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Threading
{
    public delegate void ObjectSignaledDelegate(ISynchronizable obj);




    public sealed class Waiter : BaseObject
    {
        private class WaiterThread : BaseObject
        {
            public event ObjectSignaledDelegate ObjectSignaled;

            private Waiter _owner;
            private bool _terminating = false;
            private Thread _thread;
            private bool _threadInitialized = false;
            private ThreadHandle _threadHandle;
            private List<ISynchronizable> _waitObjects = new List<ISynchronizable>();

            public WaiterThread(Waiter owner)
            {
                _owner = owner;


                _thread = new Thread(this.WaiterThreadStart);
                _thread.IsBackground = true;
                _thread.SetApartmentState(ApartmentState.STA);
                _thread.Start();


                lock (_thread)
                {
                    if (!_threadInitialized)
                        Monitor.Wait(_thread);
                }
            }

            protected override void DisposeObject(bool disposing)
            {
                lock (_thread)
                {
                    if (_threadInitialized)
                    {

                        this.Terminate();
                    }
                }

                if (_threadHandle != null)
                {

                    _threadHandle.Dispose();
                }


                lock (_waitObjects)
                    _waitObjects.Clear();
            }

            public int Count
            {
                get
                {
                    lock (_waitObjects)
                        return _waitObjects.Count;
                }
            }

            public ISynchronizable[] Objects
            {
                get
                {
                    lock (_waitObjects)
                        return _waitObjects.ToArray();
                }
            }

            public bool Add(ISynchronizable obj)
            {
                lock (_waitObjects)
                {

                    if (_waitObjects.Count >= Win32.MaximumWaitObjects)
                        return false;

                    _waitObjects.Add(obj);
                    this.NotifyChange();
                    return true;
                }
            }

            public void NotifyChange()
            {
                _threadHandle.Alert();
            }

            private void OnObjectSignaled(ISynchronizable obj)
            {
                if (this.ObjectSignaled != null)
                    this.ObjectSignaled(obj);
            }

            public bool Remove(ISynchronizable obj)
            {
                lock (_waitObjects)
                {
                    if (!_waitObjects.Contains(obj))
                        return false;

                    _waitObjects.Remove(obj);
                    this.NotifyChange();
                    return true;
                }
            }

            public void Terminate()
            {
                _terminating = true;
                this.NotifyChange();
            }

            private void WaiterThreadStart()
            {
                ISynchronizable[] waitObjects = null;


                _threadHandle = ThreadHandle.OpenCurrent(ThreadAccess.Alert);


                lock (_thread)
                {
                    _threadInitialized = true;
                    Monitor.PulseAll(_thread);
                }

                while (!_terminating)
                {
                    bool doWait;

                    lock (_waitObjects)
                    {


                        if (_waitObjects.Count > 0)
                        {
                            waitObjects = _waitObjects.ToArray();
                            doWait = true;
                        }
                        else
                        {
                            doWait = false;
                        }
                    }

                    NtStatus waitStatus;

                    if (doWait)
                    {

                        waitStatus = NativeHandle.WaitAny(waitObjects, true, long.MinValue, false);
                    }
                    else
                    {

                        waitStatus = ThreadHandle.Sleep(true, long.MinValue, false);
                    }

                    if (waitStatus == NtStatus.Alerted)
                    {


                        continue;
                    }
                    else if (waitStatus >= NtStatus.Wait0 && waitStatus <= NtStatus.Wait63)
                    {

                        ISynchronizable signaledObject = waitObjects[(int)(waitStatus - NtStatus.Wait0)];


                        lock (_waitObjects)
                        {

                            if (_waitObjects.Contains(signaledObject))
                                _waitObjects.Remove(signaledObject);
                        }


                        OnObjectSignaled(signaledObject);


                        _owner.BalanceWaiterThreads();
                    }
                }
            }
        }




        public event ObjectSignaledDelegate ObjectSignaled;

        private List<WaiterThread> _waiterThreads = new List<WaiterThread>();
        private List<ISynchronizable> _waitObjects = new List<ISynchronizable>();




        public Waiter()
        {

        }

        protected override void DisposeObject(bool disposing)
        {

            foreach (var waiterThread in _waiterThreads)
                waiterThread.Terminate();
            _waiterThreads.Clear();
        }

        public int Count
        {
            get
            {
                lock (_waitObjects)
                    return _waitObjects.Count;
            }
        }

        public ISynchronizable[] Objects
        {
            get
            {
                lock (_waitObjects)
                    return _waitObjects.ToArray();
            }
        }





        public void Add(ISynchronizable obj)
        {
            lock (_waitObjects)
                _waitObjects.Add(obj);

            foreach (var waiterThread in this.GetWaiterThreads())
            {
                if (waiterThread.Add(obj))
                    return;
            }



            this.CreateWaiterThread(obj);
        }

        internal void BalanceWaiterThreads()
        {
            lock (_waitObjects)
            {

                foreach (var waiterThread in this.GetWaiterThreads())
                {
                    if (waiterThread.Count == 0)
                        this.DeleteWaiterThread(waiterThread);
                }
            }
        }

        private WaiterThread CreateWaiterThread()
        {
            return this.CreateWaiterThread(null);
        }

        private WaiterThread CreateWaiterThread(ISynchronizable obj)
        {
            WaiterThread waiterThread = new WaiterThread(this);

            waiterThread.ObjectSignaled += this.OnObjectSignaled;

            if (obj != null)
                waiterThread.Add(obj);

            lock (_waiterThreads)
                _waiterThreads.Add(waiterThread);

            return waiterThread;
        }

        private void DeleteWaiterThread(WaiterThread waiterThread)
        {
            lock (_waiterThreads)
            {
                _waiterThreads.Remove(waiterThread);
                waiterThread.ObjectSignaled -= this.OnObjectSignaled;
                waiterThread.Dispose();
            }
        }

        private WaiterThread[] GetWaiterThreads()
        {
            lock (_waiterThreads)
                return _waiterThreads.ToArray();
        }

        private void OnObjectSignaled(ISynchronizable obj)
        {
            if (ObjectSignaled != null)
                ObjectSignaled(obj);
        }






        public bool Remove(ISynchronizable obj)
        {
            foreach (var waiterThread in this.GetWaiterThreads())
            {
                if (waiterThread.Remove(obj))
                {
                    lock (_waitObjects)
                        _waitObjects.Remove(obj);

                    this.BalanceWaiterThreads();
                    return true;
                }
            }


            return false;
        }
    }
}
