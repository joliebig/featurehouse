

using System;
using System.Collections.Generic;
using System.Threading;
using ProcessHacker.Common;
using ProcessHacker.Common.Messaging;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;
using ProcessHacker.Native.Symbols;

namespace ProcessHacker
{
    public class ThreadItem : ICloneable
    {
        public object Clone()
        {
            return this.MemberwiseClone();
        }

        public int RunId;
        public int Tid;

        public long ContextSwitches;
        public long ContextSwitchesDelta;
        public ulong Cycles;
        public ulong CyclesDelta;
        public int PriorityI;
        public string Priority;
        public IntPtr StartAddressI;
        public string StartAddress;
        public string FileName;
        public SymbolResolveLevel StartAddressLevel;
        public KWaitReason WaitReason;
        public bool IsGuiThread;
        public bool JustResolved;

        public ThreadHandle ThreadQueryLimitedHandle;
    }

    public class ThreadProvider : Provider<int, ThreadItem>
    {
        private class ResolveMessage : Message
        {
            public int Tid;
            public string Symbol;
            public string FileName;
            public SymbolResolveLevel ResolveLevel;
        }

        public delegate void LoadingStateChangedDelegate(bool loading);
        private delegate void ResolveThreadStartAddressDelegate(int tid, ulong startAddress);

        private static readonly WorkQueue _symbolsWorkQueue = new WorkQueue() { MaxWorkerThreads = 1 };

        public event LoadingStateChangedDelegate LoadingStateChanged;

        private ProcessHandle _processHandle;
        private ProcessAccess _processAccess;
        private SymbolProvider _symbols;
        private bool _kernelSymbolsLoaded = false;
        private int _pid;
        private int _loading = 0;
        private MessageQueue _messageQueue = new MessageQueue();
        private EventWaitHandle _moduleLoadCompletedEvent = new EventWaitHandle(false, EventResetMode.ManualReset);
        private bool _waitedForLoad = false;

        public ThreadProvider(int pid)
            : base()
        {
            this.Name = this.GetType().Name;
            _pid = pid;

            _messageQueue.AddListener(
                new MessageQueueListener<ResolveMessage>((message) =>
                {
                    if (message.Symbol != null)
                    {
                        this.Dictionary[message.Tid].StartAddress = message.Symbol;
                        this.Dictionary[message.Tid].FileName = message.FileName;
                        this.Dictionary[message.Tid].StartAddressLevel = message.ResolveLevel;
                        this.Dictionary[message.Tid].JustResolved = true;
                    }
                }));

            this.ProviderUpdate += new ProviderUpdateOnce(UpdateOnce);
            this.Disposed += ThreadProvider_Disposed;

            try
            {

                try
                {
                    _processAccess = ProcessAccess.QueryInformation | ProcessAccess.VmRead;
                    _processHandle = new ProcessHandle(_pid, _processAccess);
                }
                catch
                {
                    try
                    {
                        if (KProcessHacker.Instance != null)
                        {
                            _processAccess = Program.MinProcessReadMemoryRights;
                            _processHandle = new ProcessHandle(_pid, _processAccess);
                        }
                        else
                        {
                            _processAccess = Program.MinProcessQueryRights;
                            _processHandle = new ProcessHandle(_pid, _processAccess);
                        }
                    }
                    catch (WindowsException ex)
                    {
                        Logging.Log(ex);
                    }
                }


                _symbolsWorkQueue.QueueWorkItemTag(new Action(() =>
                {
                    try
                    {

                        Win32.SymbolServerSetOptions(SymbolServerOption.Unattended, 0);
                    }
                    catch (Exception ex)
                    {
                        Logging.Log(ex);
                    }

                    try
                    {

                        if (_processHandle != null)
                            _symbols = new SymbolProvider(_processHandle);
                        else
                            _symbols = new SymbolProvider();

                        SymbolProvider.Options = SymbolOptions.DeferredLoads |
                            (Properties.Settings.Default.DbgHelpUndecorate ? SymbolOptions.UndName : 0);

                        if (Properties.Settings.Default.DbgHelpSearchPath != "")
                            _symbols.SearchPath = Properties.Settings.Default.DbgHelpSearchPath;

                        try
                        {
                            if (_pid != 4)
                            {
                                using (var phandle =
                                    new ProcessHandle(_pid, Program.MinProcessQueryRights | Program.MinProcessReadMemoryRights))
                                {
                                    if (IntPtr.Size == 4 || !phandle.IsWow64())
                                    {

                                        try { _symbols.LoadProcessModules(phandle); }
                                        catch { }
                                    }
                                    else
                                    {

                                        try { _symbols.LoadProcessWow64Modules(_pid); }
                                        catch { }
                                    }



                                    if (phandle.GetKnownProcessType() == KnownProcess.WindowsSubsystem)
                                        this.LoadKernelSymbols(true);
                                }
                            }
                            else
                            {
                                this.LoadKernelSymbols(true);
                            }
                        }
                        catch (WindowsException ex)
                        {


                            try
                            {
                                ProcessHandle.Current.EnumModules((module) =>
                                {
                                    if (module.BaseName == "kernel32.dll" || module.BaseName == "ntdll.dll")
                                    {
                                        _symbols.LoadModule(module.FileName, module.BaseAddress, module.Size);
                                    }

                                    return true;
                                });
                            }
                            catch (Exception ex2)
                            {
                                Logging.Log(ex2);
                            }

                            Logging.Log(ex);
                        }
                        catch (Exception ex)
                        {
                            Logging.Log(ex);
                        }
                    }
                    finally
                    {
                        lock (_moduleLoadCompletedEvent)
                        {
                            if (!_moduleLoadCompletedEvent.SafeWaitHandle.IsClosed)
                                _moduleLoadCompletedEvent.Set();
                        }
                    }
                }), "symbols-load");
            }
            catch (Exception ex)
            {
                Logging.Log(ex);
            }
        }

        public ProcessAccess ProcessAccess
        {
            get { return _processAccess; }
        }

        public ProcessHandle ProcessHandle
        {
            get { return _processHandle; }
        }

        public void LoadKernelSymbols()
        {
            this.LoadKernelSymbols(false);
        }

        public void LoadKernelSymbols(bool force)
        {
            lock (_symbols)
            {
                if (!_kernelSymbolsLoaded)
                {
                    if (KProcessHacker.Instance != null || force)
                        _symbols.LoadKernelModules();

                    _kernelSymbolsLoaded = true;
                }
            }
        }

        private void ThreadProvider_Disposed(IProvider provider)
        {
            if (_symbols != null)
                _symbols.Dispose();
            if (_processHandle != null)
                _processHandle.Dispose();
            _symbols = null;

            lock (_moduleLoadCompletedEvent)
                _moduleLoadCompletedEvent.Close();

            foreach (int tid in this.Dictionary.Keys)
            {
                ThreadItem item = this.Dictionary[tid];

                if (item.ThreadQueryLimitedHandle != null)
                    item.ThreadQueryLimitedHandle.Dispose();
            }
        }

        private void ResolveThreadStartAddress(int tid, ulong startAddress)
        {
            ResolveMessage result = new ResolveMessage();

            result.Tid = tid;

            if (!_moduleLoadCompletedEvent.SafeWaitHandle.IsClosed)
            {
                try
                {
                    _moduleLoadCompletedEvent.WaitOne();
                }
                catch
                { }
            }

            if (_symbols == null)
                return;

            try
            {
                Interlocked.Increment(ref _loading);

                if (this.LoadingStateChanged != null)
                    this.LoadingStateChanged(Thread.VolatileRead(ref _loading) > 0);

                try
                {
                    SymbolFlags flags;
                    string fileName;

                    result.Symbol = _symbols.GetSymbolFromAddress(
                        startAddress,
                        out result.ResolveLevel,
                        out flags,
                        out fileName
                        );
                    result.FileName = fileName;
                    _messageQueue.Enqueue(result);
                }
                catch
                { }
            }
            finally
            {
                Interlocked.Decrement(ref _loading);

                if (this.LoadingStateChanged != null)
                    this.LoadingStateChanged(Thread.VolatileRead(ref _loading) > 0);
            }
        }

        public void QueueThreadResolveStartAddress(int tid)
        {
            this.QueueThreadResolveStartAddress(tid, this.Dictionary[tid].StartAddressI.ToUInt64());
        }

        public void QueueThreadResolveStartAddress(int tid, ulong startAddress)
        {
            _symbolsWorkQueue.QueueWorkItemTag(
                new ResolveThreadStartAddressDelegate(this.ResolveThreadStartAddress),
                "thread-resolve",
                tid, startAddress
                );
        }

        private string GetThreadBasicStartAddress(ulong startAddress, out SymbolResolveLevel level)
        {
            ulong modBase;
            string fileName = _symbols.GetModuleFromAddress(startAddress, out modBase);

            if (fileName == null)
            {
                level = SymbolResolveLevel.Address;
                return "0x" + startAddress.ToString("x");
            }
            else
            {
                level = SymbolResolveLevel.Module;
                return System.IO.Path.GetFileName(fileName) + "+0x" +
                    (startAddress - modBase).ToString("x");
            }
        }

        private void UpdateOnce()
        {
            var threads = Windows.GetProcessThreads(_pid);
            Dictionary<int, ThreadItem> newdictionary = new Dictionary<int, ThreadItem>(this.Dictionary);

            if (threads == null)
                threads = new Dictionary<int, SystemThreadInformation>();


            foreach (int tid in Dictionary.Keys)
            {
                if (!threads.ContainsKey(tid))
                {
                    ThreadItem item = this.Dictionary[tid];

                    if (item.ThreadQueryLimitedHandle != null)
                        item.ThreadQueryLimitedHandle.Dispose();

                    this.OnDictionaryRemoved(item);
                    newdictionary.Remove(tid);
                }
            }


            _messageQueue.Listen();


            foreach (int tid in threads.Keys)
            {
                var t = threads[tid];

                if (!Dictionary.ContainsKey(tid))
                {
                    ThreadItem item = new ThreadItem();

                    item.RunId = this.RunCount;
                    item.Tid = tid;
                    item.ContextSwitches = t.ContextSwitchCount;
                    item.WaitReason = t.WaitReason;

                    try
                    {
                        item.ThreadQueryLimitedHandle = new ThreadHandle(tid, Program.MinThreadQueryRights);

                        try
                        {
                            item.PriorityI = (int)item.ThreadQueryLimitedHandle.GetBasePriorityWin32();
                            item.Priority = item.ThreadQueryLimitedHandle.GetBasePriorityWin32().ToString();
                        }
                        catch
                        { }

                        if (KProcessHacker.Instance != null)
                        {
                            try
                            {
                                item.IsGuiThread = KProcessHacker.Instance.KphGetThreadWin32Thread(item.ThreadQueryLimitedHandle) != 0;
                            }
                            catch
                            { }
                        }

                        if (OSVersion.HasCycleTime)
                        {
                            try
                            {
                                item.Cycles = item.ThreadQueryLimitedHandle.GetCycleTime();
                            }
                            catch
                            { }
                        }
                    }
                    catch
                    { }

                    if (KProcessHacker.Instance != null && item.ThreadQueryLimitedHandle != null)
                    {
                        try
                        {
                            item.StartAddressI =
                                KProcessHacker.Instance.GetThreadStartAddress(item.ThreadQueryLimitedHandle).ToIntPtr();
                        }
                        catch
                        { }
                    }
                    else
                    {
                        try
                        {
                            using (ThreadHandle thandle =
                                new ThreadHandle(tid, ThreadAccess.QueryInformation))
                            {
                                item.StartAddressI = thandle.GetWin32StartAddress();
                            }
                        }
                        catch
                        {
                            item.StartAddressI = t.StartAddress;
                        }
                    }

                    if (!_waitedForLoad)
                    {
                        _waitedForLoad = true;

                        try
                        {
                            if (_moduleLoadCompletedEvent.WaitOne(0, false))
                            {
                                item.StartAddress = this.GetThreadBasicStartAddress(
                                    item.StartAddressI.ToUInt64(), out item.StartAddressLevel);
                            }
                        }
                        catch
                        { }
                    }

                    if (string.IsNullOrEmpty(item.StartAddress))
                    {
                        item.StartAddress = Utils.FormatAddress(item.StartAddressI);
                        item.StartAddressLevel = SymbolResolveLevel.Address;
                    }

                    this.QueueThreadResolveStartAddress(tid, item.StartAddressI.ToUInt64());

                    newdictionary.Add(tid, item);
                    this.OnDictionaryAdded(item);
                }

                else
                {
                    ThreadItem item = Dictionary[tid];
                    ThreadItem newitem = item.Clone() as ThreadItem;

                    newitem.JustResolved = false;
                    newitem.ContextSwitchesDelta = t.ContextSwitchCount - newitem.ContextSwitches;
                    newitem.ContextSwitches = t.ContextSwitchCount;
                    newitem.WaitReason = t.WaitReason;

                    try
                    {
                        newitem.PriorityI = (int)newitem.ThreadQueryLimitedHandle.GetBasePriorityWin32();
                        newitem.Priority = newitem.ThreadQueryLimitedHandle.GetBasePriorityWin32().ToString();
                    }
                    catch
                    { }

                    if (KProcessHacker.Instance != null)
                    {
                        try
                        {
                            newitem.IsGuiThread = KProcessHacker.Instance.KphGetThreadWin32Thread(newitem.ThreadQueryLimitedHandle) != 0;
                        }
                        catch
                        { }
                    }

                    if (OSVersion.HasCycleTime)
                    {
                        try
                        {
                            ulong thisCycles = newitem.ThreadQueryLimitedHandle.GetCycleTime();

                            newitem.CyclesDelta = thisCycles - newitem.Cycles;
                            newitem.Cycles = thisCycles;
                        }
                        catch
                        { }
                    }

                    if (newitem.StartAddressLevel == SymbolResolveLevel.Address)
                    {
                        if (_moduleLoadCompletedEvent.WaitOne(0, false))
                        {
                            newitem.StartAddress = this.GetThreadBasicStartAddress(
                                newitem.StartAddressI.ToUInt64(), out newitem.StartAddressLevel);
                        }




                        if (
                            item.StartAddressLevel == SymbolResolveLevel.Address &&
                            item.JustResolved)
                        {
                            if (item.StartAddressI != t.StartAddress)
                            {
                                item.StartAddressI = t.StartAddress;
                                this.QueueThreadResolveStartAddress(tid, item.StartAddressI.ToUInt64());
                            }
                        }
                    }

                    if (
                        newitem.ContextSwitches != item.ContextSwitches ||
                        newitem.ContextSwitchesDelta != item.ContextSwitchesDelta ||
                        newitem.Cycles != item.Cycles ||
                        newitem.CyclesDelta != item.CyclesDelta ||
                        newitem.IsGuiThread != item.IsGuiThread ||
                        newitem.Priority != item.Priority ||
                        newitem.StartAddress != item.StartAddress ||
                        newitem.WaitReason != item.WaitReason ||
                        item.JustResolved
                        )
                    {
                        newdictionary[tid] = newitem;
                        this.OnDictionaryModified(item, newitem);
                    }
                }
            }

            Dictionary = newdictionary;
        }

        public SymbolProvider Symbols
        {
            get { return _symbols; }
        }

        public int Pid
        {
            get { return _pid; }
        }
    }
}
