

using System;
using System.Windows.Forms;
using System.Threading;
using System.ComponentModel;

namespace ProcessHacker.FormHelper
{



    public class AlreadyRunningException : System.ApplicationException
    {
        public AlreadyRunningException() : base("Operation already running")
        { }
    }

    public abstract class AsyncOperation
    {
        private Thread _asyncThread;
        private object _asyncLock = new object();

        public AsyncOperation(ISynchronizeInvoke target)
        {
            isiTarget = target;
            isRunning = false;
        }

        public void Start()
        {
            lock (_asyncLock)
            {
                if (isRunning)
                {
                    throw new AlreadyRunningException();
                }
                isRunning = true;
            }

            _asyncThread = new Thread(InternalStart);
            _asyncThread.Start();
        }

        public void Cancel()
        {
            lock (_asyncLock)
            {
                cancelledFlag = true;
            }
        }

        public bool CancelAndWait()
        {
            lock (_asyncLock)
            {
                cancelledFlag = true;

                while (!IsDone)
                {
                    Monitor.Wait(_asyncLock, 1000);
                }
            }

            return !HasCompleted;
        }

        public bool WaitUntilDone()
        {
            lock (_asyncLock)
            {




                while (!IsDone)
                {
                    Monitor.Wait(_asyncLock, 1000);
                }
            }

            return HasCompleted;
        }

        public bool IsDone
        {
            get
            {
                lock (_asyncLock)
                {
                    return completeFlag || cancelAcknowledgedFlag || failedFlag;
                }
            }
        }

        public event EventHandler Completed;
        public event EventHandler Cancelled;
        public event System.Threading.ThreadExceptionEventHandler Failed;

        private ISynchronizeInvoke isiTarget;
        protected ISynchronizeInvoke Target
        {
            get { return isiTarget; }
        }




        protected abstract void DoWork();

        private bool cancelledFlag;
        protected bool CancelRequested
        {
            get
            {
                lock (_asyncLock) { return cancelledFlag; }
            }
        }

        private bool completeFlag;
        protected bool HasCompleted
        {
            get
            {
                lock (_asyncLock) { return completeFlag; }
            }
        }

        protected void AcknowledgeCancel()
        {
            lock (_asyncLock)
            {
                cancelAcknowledgedFlag = true;
                isRunning = false;
                Monitor.Pulse(_asyncLock);
                FireAsync(Cancelled, this, EventArgs.Empty);
            }
        }

        private bool cancelAcknowledgedFlag;

        private bool failedFlag;

        private bool isRunning;

        private void InternalStart()
        {
            cancelledFlag = false;
            completeFlag = false;
            cancelAcknowledgedFlag = false;
            failedFlag = false;

            try
            {
                DoWork();
            }
            catch (Exception e)
            {
                try
                {
                    FailOperation(e);
                }
                catch
                { }

                if (e is SystemException)
                {
                    throw;
                }
            }

            lock (_asyncLock)
            {

                if (!cancelAcknowledgedFlag && !failedFlag)
                {
                    CompleteOperation();
                }
            }
        }

        private void CompleteOperation()
        {
            lock (_asyncLock)
            {
                completeFlag = true;
                isRunning = false;
                Monitor.Pulse(_asyncLock);
                FireAsync(Completed, this, EventArgs.Empty);
            }
        }

        private void FailOperation(Exception e)
        {
            lock (_asyncLock)
            {
                failedFlag = true;
                isRunning = false;
                Monitor.Pulse(_asyncLock);
                FireAsync(Failed, this, new ThreadExceptionEventArgs(e));
            }
        }

        protected void FireAsync(Delegate dlg, params object[] pList)
        {
            if (dlg != null)
            {
                Target.BeginInvoke(dlg, pList);
            }
        }
    }
}
