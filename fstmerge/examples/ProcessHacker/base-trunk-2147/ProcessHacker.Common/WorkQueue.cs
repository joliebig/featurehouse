

using System;
using System.Collections.Generic;
using System.Threading;

namespace ProcessHacker.Common
{



    public sealed class WorkQueue
    {



        public sealed class WorkItem
        {
            private WorkQueue _owner;
            private string _tag;
            private Delegate _work;
            private object[] _args;
            private bool _enabled = true;
            private bool _completed = false;
            private object _completedEventLock = new object();
            private ManualResetEvent _completedEvent;
            private object _result;
            private Exception _exception;

            internal WorkItem(WorkQueue owner, Delegate work, object[] args)
                : this(owner, work, args, null)
            { }

            internal WorkItem(WorkQueue owner, Delegate work, object[] args, string tag)
            {
                _owner = owner;
                _work = work;
                _args = args;
                _tag = tag;
            }

            public Delegate Work
            {
                get { return _work; }
            }

            public object[] Arguments
            {
                get { return _args; }
            }




            public string Tag
            {
                get { return _tag; }
            }




            internal bool Enabled
            {
                get { return _enabled; }
                set { _enabled = value; }
            }




            public bool Completed
            {
                get { return _completed; }
            }




            public object Result
            {
                get { return _result; }
            }




            public Exception Exception
            {
                get { return _exception; }
            }






            public bool Abort()
            {
                return _owner.RemoveQueuedWorkItem(this);
            }





            public object GetResult()
            {
                this.WaitOne();
                return _result;
            }




            internal void PerformWork()
            {
                if (!_enabled)
                    return;

                try
                {
                    if (_args == null)
                        _result = _work.Method.Invoke(_work.Target, null);
                    else
                        _result = _work.Method.Invoke(_work.Target, _args.Length != 0 ? _args : null);
                }
                catch (Exception ex)
                {
                    _exception = ex;
                }

                _completed = true;

                lock (_completedEventLock)
                {
                    if (_completedEvent != null)
                        _completedEvent.Set();
                }
            }





            public bool WaitOne()
            {
                return this.WaitOne(-1);
            }
            public bool WaitOne(int timeout)
            {
                lock (_completedEventLock)
                {
                    if (_completed)
                        return true;
                    if (_completedEvent == null)
                        _completedEvent = new ManualResetEvent(false);
                }
                return _completedEvent.WaitOne(timeout, false);
            }
        }
        private static WorkQueue _globalWorkQueue = new WorkQueue();
        public static WorkQueue GlobalWorkQueue
        {
            get { return _globalWorkQueue; }
        }
        public static WorkItem GlobalQueueWorkItem(Delegate work)
        {
            return _globalWorkQueue.QueueWorkItem(work);
        }
        public static WorkItem GlobalQueueWorkItem(Delegate work, params object[] args)
        {
            return _globalWorkQueue.QueueWorkItemTag(work, null, true, args);
        }
        public static WorkItem GlobalQueueWorkItemTag(Delegate work, string tag)
        {
            return _globalWorkQueue.QueueWorkItemTag(work, tag, true, null);
        }
        public static WorkItem GlobalQueueWorkItemTag(Delegate work, string tag, params object[] args)
        {
            return _globalWorkQueue.QueueWorkItemTag(work, tag, true, args);
        }
        private Queue<WorkItem> _workQueue = new Queue<WorkItem>();
        private int _maxWorkerThreads = 1;
        private int _minWorkerThreads = 0;
        private Dictionary<int, Thread> _workerThreads = new Dictionary<int, Thread>();
        private int _busyCount = 0;
        private int _noWorkTimeout = 1000;
        private volatile bool _isJoining = false;
        public WorkQueue()
        { }
        public int BusyCount
        {
            get { return _busyCount; }
        }
        public int MaxWorkerThreads
        {
            get { return _maxWorkerThreads; }
            set { _maxWorkerThreads = value; }
        }
        public int MinWorkerThreads
        {
            get { return _minWorkerThreads; }
            set { _minWorkerThreads = value; }
        }
        public int NoWorkTimeout
        {
            get { return _noWorkTimeout; }
            set { _noWorkTimeout = value; }
        }
        public int QueuedCount
        {
            get { return _workQueue.Count; }
        }
        public int WorkerCount
        {
            get { return _workerThreads.Count; }
        }
        public void CreateMinimumWorkerThreads()
        {
            if (_workerThreads.Count < _minWorkerThreads)
            {
                lock (_workerThreads)
                {
                    while (_workerThreads.Count < _minWorkerThreads)
                        this.CreateWorkerThread();
                }
            }
        }
        private void CreateWorkerThread()
        {
            Thread workThread = new Thread(this.WorkerThreadStart);
            workThread.IsBackground = true;
            workThread.Priority = ThreadPriority.Lowest;
            workThread.SetApartmentState(ApartmentState.STA);
            _workerThreads.Add(workThread.ManagedThreadId, workThread);
            workThread.Start();
        }
        private void DestroyWorkerThread()
        {
            _workerThreads.Remove(Thread.CurrentThread.ManagedThreadId);
        }
        public WorkItem[] GetQueuedWorkItems()
        {
            lock (_workQueue)
                return _workQueue.ToArray();
        }
        public void JoinAll()
        {
            _isJoining = true;
            while (_workQueue.Count > 0)
            {
                WorkItem workItem = null;
                lock (_workQueue)
                {
                    if (_workQueue.Count > 0)
                        workItem = _workQueue.Peek();
                    else
                        continue;
                }
                workItem.WaitOne();
            }
        }
        public bool RemoveQueuedWorkItem(WorkItem workItem)
        {
            lock (_workQueue)
            {
                if (_workQueue.Contains(workItem))
                {
                    workItem.Enabled = false;
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }
        public void ResetJoin()
        {
            _isJoining = false;
        }
        public WorkItem QueueWorkItem(Delegate work)
        {
            return this.QueueWorkItemTag(work, null, true, null);
        }
        public WorkItem QueueWorkItem(Delegate work, params object[] args)
        {
            return this.QueueWorkItemTag(work, null, true, args);
        }
        public WorkItem QueueWorkItemTag(Delegate work, string tag)
        {
            return this.QueueWorkItemTag(work, tag, true, null);
        }
        public WorkItem QueueWorkItemTag(Delegate work, string tag, params object[] args)
        {
            return this.QueueWorkItemTag(work, tag, true, args);
        }
        public WorkItem QueueWorkItemTag(Delegate work, string tag, bool isArray, object[] args)
        {
            WorkItem workItem;
            if (_isJoining)
                return null;
            lock (_workQueue)
            {
                _workQueue.Enqueue(workItem = new WorkItem(this, work, args, tag));
                Monitor.Pulse(_workQueue);
            }
            if (Thread.VolatileRead(ref _busyCount) == _workerThreads.Count)
            {
                if (_workerThreads.Count < _maxWorkerThreads)
                {
                    lock (_workerThreads)
                    {
                        if (_workerThreads.Count < _maxWorkerThreads)
                        {
                            this.CreateWorkerThread();
                        }
                    }
                }
            }
            return workItem;
        }
        private void WorkerThreadStart()
        {
            while (true)
            {
                if (_workerThreads.Count > _maxWorkerThreads)
                {
                    lock (_workerThreads)
                    {
                        if (_workerThreads.Count > _maxWorkerThreads &&
                            _workerThreads.Count > _minWorkerThreads)
                        {
                            this.DestroyWorkerThread();
                            return;
                        }
                    }
                }
                if (_workQueue.Count > 0)
                {
                    WorkItem workItem = null;
                    lock (_workQueue)
                    {
                        if (_workQueue.Count > 0)
                            workItem = _workQueue.Dequeue();
                        else
                            continue;
                    }
                    Interlocked.Increment(ref _busyCount);
                    workItem.PerformWork();
                    Interlocked.Decrement(ref _busyCount);
                }
                else
                {
                    bool workArrived = false;
                    lock (_workQueue)
                        workArrived = Monitor.Wait(_workQueue, _noWorkTimeout);
                    if (workArrived)
                    {
                        continue;
                    }
                    else
                    {
                        lock (_workerThreads)
                        {
                            if (_workerThreads.Count > _minWorkerThreads)
                                this.DestroyWorkerThread();
                        }
                        return;
                    }
                }
            }
        }
    }
}
