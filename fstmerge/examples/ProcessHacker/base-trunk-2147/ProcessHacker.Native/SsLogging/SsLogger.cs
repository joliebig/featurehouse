using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;
using System.Threading;
using ProcessHacker.Native.Threading;
using System.Runtime.InteropServices;

namespace ProcessHacker.Native.SsLogging
{
    public delegate void ArgumentBlockReceivedDelegate(SsData argBlock);
    public delegate void EventBlockReceivedDelegate(SsEvent eventBlock);

    public sealed class SsLogger
    {
        private const int _highBlockSize = 0x200;

        internal static string ReadWString(MemoryRegion data)
        {
            KphSsWString wString = data.ReadStruct<KphSsWString>();

            return data.ReadUnicodeString(KphSsWString.BufferOffset, wString.Length / 2);
        }

        public event ArgumentBlockReceivedDelegate ArgumentBlockReceived;
        public event EventBlockReceivedDelegate EventBlockReceived;

        private bool _started = false;
        private object _startLock = new object();

        private bool _terminating = false;
        private Thread _bufferWorkerThread;
        private ThreadHandle _bufferWorkerThreadHandle;
        private Event _bufferWorkerThreadReadyEvent = new Event(true, false);

        private VirtualMemoryAlloc _buffer;
        private SemaphoreHandle _readSemaphore;
        private SemaphoreHandle _writeSemaphore;
        private KphSsClientEntryHandle _clientEntryHandle;
        private KphSsRuleSetEntryHandle _ruleSetEntryHandle;

        public SsLogger(int bufferedBlockCount, bool includeAll)
        {

            _buffer = new VirtualMemoryAlloc(_highBlockSize * bufferedBlockCount);




            _readSemaphore = SemaphoreHandle.Create(SemaphoreAccess.All, 0, bufferedBlockCount);

            _writeSemaphore = SemaphoreHandle.Create(SemaphoreAccess.All, bufferedBlockCount, bufferedBlockCount);


            _clientEntryHandle = KProcessHacker.Instance.SsCreateClientEntry(
                ProcessHandle.Current,
                _readSemaphore,
                _writeSemaphore,
                _buffer,
                _buffer.Size
                );


            _ruleSetEntryHandle = KProcessHacker.Instance.SsCreateRuleSetEntry(
                _clientEntryHandle,
                includeAll ? KphSsFilterType.Include : KphSsFilterType.Exclude,
                KphSsRuleSetAction.Log
                );
        }

        public IntPtr AddNumberRule(FilterType filterType, int number)
        {
            return KProcessHacker.Instance.SsAddNumberRule(
                _ruleSetEntryHandle,
                filterType.ToKphSs(),
                number
                );
        }

        public IntPtr AddPreviousModeRule(FilterType filterType, KProcessorMode previousMode)
        {
            return KProcessHacker.Instance.SsAddPreviousModeRule(
                _ruleSetEntryHandle,
                filterType.ToKphSs(),
                previousMode
                );
        }

        public IntPtr AddProcessIdRule(FilterType filterType, int pid)
        {
            return KProcessHacker.Instance.SsAddProcessIdRule(
                _ruleSetEntryHandle,
                filterType.ToKphSs(),
                pid.ToIntPtr()
                );
        }

        public IntPtr AddThreadIdRule(FilterType filterType, int tid)
        {
            return KProcessHacker.Instance.SsAddProcessIdRule(
                _ruleSetEntryHandle,
                filterType.ToKphSs(),
                tid.ToIntPtr()
                );
        }

        private void BufferWorkerThreadStart()
        {
            int cursor = 0;



            _bufferWorkerThreadHandle = ThreadHandle.OpenCurrent(ThreadAccess.All);


            _bufferWorkerThreadReadyEvent.Set();

            while (!_terminating)
            {
                NtStatus status;
                KphSsBlockHeader blockHeader;



                status = _readSemaphore.Wait(true);


                if (status == NtStatus.Alerted)
                    return;


                if (_buffer.Size - cursor < Marshal.SizeOf(typeof(KphSsBlockHeader)))
                    cursor = 0;


                blockHeader = _buffer.ReadStruct<KphSsBlockHeader>(cursor, 0);


                if (blockHeader.Type == KphSsBlockType.Reset)
                {
                    cursor = 0;
                    blockHeader = _buffer.ReadStruct<KphSsBlockHeader>(cursor, 0);
                }


                if (blockHeader.Type == KphSsBlockType.Event)
                {
                    var eventBlock = _buffer.ReadStruct<KphSsEventBlock>(cursor, 0);
                    int[] arguments;
                    IntPtr[] stackTrace;



                    arguments = new int[eventBlock.NumberOfArguments];
                    stackTrace = new IntPtr[eventBlock.TraceCount];

                    for (int i = 0; i < arguments.Length; i++)
                        arguments[i] = _buffer.ReadInt32(cursor + eventBlock.ArgumentsOffset, i);
                    for (int i = 0; i < stackTrace.Length; i++)
                        stackTrace[i] = _buffer.ReadIntPtr(cursor + eventBlock.TraceOffset, i);


                    SsEvent ssEvent = new SsEvent();


                    ssEvent.Time = DateTime.FromFileTime(eventBlock.Time);
                    ssEvent.ThreadId = eventBlock.ClientId.ThreadId;
                    ssEvent.ProcessId = eventBlock.ClientId.ProcessId;
                    ssEvent.Arguments = arguments;
                    ssEvent.StackTrace = stackTrace;


                    ssEvent.ArgumentsCopyFailed =
                        (eventBlock.Flags & KphSsEventFlags.CopyArgumentsFailed) == KphSsEventFlags.CopyArgumentsFailed;
                    ssEvent.ArgumentsProbeFailed =
                        (eventBlock.Flags & KphSsEventFlags.ProbeArgumentsFailed) == KphSsEventFlags.ProbeArgumentsFailed;
                    ssEvent.CallNumber = eventBlock.Number;

                    if ((eventBlock.Flags & KphSsEventFlags.UserMode) == KphSsEventFlags.UserMode)
                        ssEvent.Mode = KProcessorMode.UserMode;
                    else
                        ssEvent.Mode = KProcessorMode.KernelMode;


                    if (this.EventBlockReceived != null)
                        this.EventBlockReceived(ssEvent);
                }
                else if (blockHeader.Type == KphSsBlockType.Argument)
                {
                    var argBlock = _buffer.ReadStruct<KphSsArgumentBlock>(cursor, 0);
                    MemoryRegion dataRegion;
                    SsData ssArg = null;

                    dataRegion = new MemoryRegion(_buffer, cursor + KphSsArgumentBlock.DataOffset);


                    switch (argBlock.Type)
                    {
                        case KphSsArgumentType.Int8:
                            {
                                SsSimple simpleArg = new SsSimple();

                                simpleArg.Argument = argBlock.Data.Int8;
                                simpleArg.Type = typeof(Byte);
                                ssArg = simpleArg;
                            }
                            break;
                        case KphSsArgumentType.Int16:
                            {
                                SsSimple simpleArg = new SsSimple();

                                simpleArg.Argument = argBlock.Data.Int16;
                                simpleArg.Type = typeof(Int16);
                                ssArg = simpleArg;
                            }
                            break;
                        case KphSsArgumentType.Int32:
                            {
                                SsSimple simpleArg = new SsSimple();

                                simpleArg.Argument = argBlock.Data.Int32;
                                simpleArg.Type = typeof(Int32);
                                ssArg = simpleArg;
                            }
                            break;
                        case KphSsArgumentType.Int64:
                            {
                                SsSimple simpleArg = new SsSimple();

                                simpleArg.Argument = argBlock.Data.Int64;
                                simpleArg.Type = typeof(Int64);
                                ssArg = simpleArg;
                            }
                            break;
                        case KphSsArgumentType.Handle:
                            {
                                ssArg = new SsHandle(dataRegion);
                            }
                            break;
                        case KphSsArgumentType.UnicodeString:
                            {
                                ssArg = new SsUnicodeString(dataRegion);
                            }
                            break;
                        case KphSsArgumentType.ObjectAttributes:
                            {
                                ssArg = new SsObjectAttributes(dataRegion);
                            }
                            break;
                        case KphSsArgumentType.ClientId:
                            {
                                ssArg = new SsClientId(dataRegion);
                            }
                            break;
                    }

                    ssArg.Index = argBlock.Index;


                    if (ssArg != null)
                    {
                        if (this.ArgumentBlockReceived != null)
                            this.ArgumentBlockReceived(ssArg);
                    }
                }


                cursor += blockHeader.Size;

                _writeSemaphore.Release();
            }
        }

        public void GetStatistics(out int blocksWritten, out int blocksDropped)
        {
            KphSsClientInformation info;
            int retLength;

            KProcessHacker.Instance.SsQueryClientEntry(
                _clientEntryHandle,
                out info,
                Marshal.SizeOf(typeof(KphSsClientInformation)),
                out retLength
                );

            blocksWritten = info.NumberOfBlocksWritten;
            blocksDropped = info.NumberOfBlocksDropped;
        }

        public void RemoveRule(IntPtr handle)
        {
            KProcessHacker.Instance.SsRemoveRule(_ruleSetEntryHandle, handle);
        }

        public void Start()
        {
            lock (_startLock)
            {
                if (!_started)
                {
                    KProcessHacker.Instance.SsRef();
                    KProcessHacker.Instance.SsEnableClientEntry(_clientEntryHandle, true);
                    _started = true;

                    _terminating = false;


                    _bufferWorkerThread = new Thread(this.BufferWorkerThreadStart);
                    _bufferWorkerThread.IsBackground = true;
                    _bufferWorkerThread.Start();

                    _bufferWorkerThreadReadyEvent.Wait();
                }
            }
        }

        public void Stop()
        {
            lock (_startLock)
            {
                if (_started)
                {
                    KProcessHacker.Instance.SsEnableClientEntry(_clientEntryHandle, false);
                    KProcessHacker.Instance.SsUnref();
                    _started = false;


                    _terminating = true;

                    _bufferWorkerThreadHandle.Alert();

                    _bufferWorkerThreadHandle.Wait();

                    _bufferWorkerThreadHandle.Dispose();
                }
            }
        }
    }
}
