

using System;
using System.Collections.Generic;
using System.Threading;
using ProcessHacker.Common;
using ProcessHacker.Common.Objects;

namespace ProcessHacker
{



    public abstract class Provider<TKey, TValue> : BaseObject, IProvider
    {



        public delegate void ProviderUpdateOnce();





        public delegate void ProviderDictionaryAdded(TValue item);





        public delegate void ProviderDictionaryModified(TValue oldItem, TValue newItem);





        public delegate void ProviderDictionaryRemoved(TValue item);





        public delegate void ProviderError(Exception ex);




        protected event ProviderUpdateOnce ProviderUpdate;

        public new event Action<IProvider> Disposed;

        public event ProviderUpdateOnce BeforeUpdate;




        public event ProviderUpdateOnce Updated;




        public event ProviderDictionaryAdded DictionaryAdded;




        public event ProviderDictionaryModified DictionaryModified;




        public event ProviderDictionaryRemoved DictionaryRemoved;




        public event ProviderError Error;

        private string _name = string.Empty;
        private Thread _thread;
        private IDictionary<TKey, TValue> _dictionary;

        private object _busyLock = new object();
        private bool _disposing = false;
        private bool _busy = false;
        private bool _createThread = true;
        private bool _enabled = false;
        private int _runCount = 0;
        private int _interval;




        public Provider()
            : this(new Dictionary<TKey, TValue>())
        { }





        public Provider(IEqualityComparer<TKey> comparer)
            : this(new Dictionary<TKey, TValue>(comparer))
        { }





        public Provider(IDictionary<TKey, TValue> dictionary)
        {
            if (dictionary == null)
                throw new ArgumentNullException("dictionary");

            _dictionary = dictionary;
        }

        protected override void DisposeObject(bool disposing)
        {
            Logging.Log(Logging.Importance.Information, "Provider (" + this.Name + "): disposing (" + disposing.ToString() + ")");

            _disposing = true;

            if (disposing)
                Monitor.Enter(_busyLock);







            if (this.Disposed != null)
            {
                try
                {
                    this.Disposed(this);
                }
                catch (Exception ex)
                {
                    Logging.Log(ex);
                }
            }

            if (disposing)
                Monitor.Exit(_busyLock);

            Logging.Log(Logging.Importance.Information, "Provider (" + this.Name + "): finished disposing (" + disposing.ToString() + ")");
        }

        public string Name
        {
            get { return _name; }
            protected set
            {
                _name = value;
                if (_name == null)
                    _name = string.Empty;
            }
        }




        public bool Busy
        {
            get { return _busy; }
        }




        public bool CreateThread
        {
            get { return _createThread; }
            set { _createThread = value; }
        }




        public bool Enabled
        {
            get { return _enabled; }
            set
            {
                _enabled = value;

                if (_enabled && _createThread && _thread == null)
                {
                    _thread = new Thread(new ThreadStart(Update));
                    _thread.IsBackground = true;
                    _thread.SetApartmentState(ApartmentState.STA);
                    _thread.Start();
                    _thread.Priority = ThreadPriority.Lowest;
                }
            }
        }




        public int RunCount
        {
            get { return _runCount; }
        }




        public int Interval
        {
            get { return _interval; }
            set { _interval = value; }
        }




        public IDictionary<TKey, TValue> Dictionary
        {
            get { return _dictionary; }
            protected set { _dictionary = value; }
        }




        private void Update()
        {
            while (true)
            {
                if (_enabled && !_disposing)
                {
                    this.RunOnce();
                }

                Thread.Sleep(_interval);
            }
        }




        public void RunOnce()
        {
            lock (_busyLock)
            {

                if (_disposing)
                {
                    Logging.Log(Logging.Importance.Warning, "Provider (" + _name + "): RunOnce: currently disposing");
                    return;
                }

                _busy = true;

                if (ProviderUpdate != null)
                {
                    try
                    {
                        if (BeforeUpdate != null)
                            BeforeUpdate();
                    }
                    catch
                    { }

                    try
                    {
                        ProviderUpdate();
                        _runCount++;
                    }
                    catch (Exception ex)
                    {
                        try
                        {
                            if (Error != null)
                                Error(ex);
                        }
                        catch
                        { }

                        Logging.Log(ex);
                    }

                    try
                    {
                        if (Updated != null)
                            Updated();
                    }
                    catch
                    { }
                }

                _busy = false;
            }
        }




        public void RunOnceAsync()
        {
            WorkQueue.GlobalQueueWorkItemTag(new Action(this.RunOnce), "provider-runonceasync");
        }




        public void InterlockedExecute(Delegate action, params object[] args)
        {
            this.InterlockedExecute(action, -1, args);
        }




        public void InterlockedExecute(Delegate action, int timeout, params object[] args)
        {
            lock (_busyLock)
                action.DynamicInvoke(args);
        }





        public void Wait()
        {
            this.Wait(-1);
        }







        public bool Wait(int timeout)
        {
            if (Monitor.TryEnter(_busyLock, timeout))
            {
                Monitor.Exit(_busyLock);
                return true;
            }

            return false;
        }

        private void CallEvent(Delegate e, params object[] args)
        {
            if (e != null)
            {
                try
                {
                    e.DynamicInvoke(args);
                }
                catch (Exception ex)
                {
                    Logging.Log(ex);
                }
            }
        }

        protected void OnDictionaryAdded(TValue item)
        {
            this.CallEvent(this.DictionaryAdded, item);
        }

        protected void OnDictionaryModified(TValue oldItem, TValue newItem)
        {
            this.CallEvent(this.DictionaryModified, oldItem, newItem);
        }

        protected void OnDictionaryRemoved(TValue item)
        {
            this.CallEvent(this.DictionaryRemoved, item);
        }
    }
}
