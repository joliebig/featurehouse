

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using ProcessHacker.Common;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{



    public sealed class ThreadHandle : NativeHandle<ThreadAccess>, IWithToken
    {
        public delegate bool WalkStackDelegate(ThreadStackFrame stackFrame);

        private static readonly ThreadHandle _current = new ThreadHandle(new IntPtr(-2), false);




        public static ThreadHandle Current
        {
            get { return _current; }
        }

        public static ThreadHandle Create(
            ThreadAccess access,
            string name,
            ObjectFlags objectFlags,
            DirectoryHandle rootDirectory,
            ProcessHandle processHandle,
            out ClientId clientId,
            ref Context threadContext,
            ref InitialTeb initialTeb,
            bool createSuspended
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateThread(
                    out handle,
                    access,
                    ref oa,
                    processHandle,
                    out clientId,
                    ref threadContext,
                    ref initialTeb,
                    createSuspended
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new ThreadHandle(handle, true);
        }

        public static ThreadHandle CreateUserThread(ProcessHandle processHandle, IntPtr startAddress, IntPtr parameter)
        {
            return CreateUserThread(processHandle, false, startAddress, parameter);
        }

        public static ThreadHandle CreateUserThread(
            ProcessHandle processHandle,
            bool createSuspended,
            IntPtr startAddress,
            IntPtr parameter
            )
        {
            ClientId clientId;

            return CreateUserThread(processHandle, createSuspended, 0, 0, startAddress, parameter, out clientId);
        }

        public static ThreadHandle CreateUserThread(
            ProcessHandle processHandle,
            bool createSuspended,
            int maximumStackSize,
            int initialStackSize,
            IntPtr startAddress,
            IntPtr parameter,
            out ClientId clientId
            )
        {
            NtStatus status;
            IntPtr threadHandle;

            if ((status = Win32.RtlCreateUserThread(
                processHandle,
                IntPtr.Zero,
                createSuspended,
                0,
                maximumStackSize.ToIntPtr(),
                initialStackSize.ToIntPtr(),
                startAddress,
                parameter,
                out threadHandle,
                out clientId
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return new ThreadHandle(threadHandle, true);
        }







        public static ThreadHandle FromHandle(IntPtr handle)
        {
            return new ThreadHandle(handle, false);
        }





        public static ThreadHandle GetCurrent()
        {
            return Current;
        }





        public static ClientId GetCurrentCid()
        {
            return new ClientId(ProcessHandle.GetCurrentId(), ThreadHandle.GetCurrentId());
        }





        public static int GetCurrentId()
        {
            return Win32.GetCurrentThreadId();
        }





        public unsafe static Teb* GetCurrentTeb()
        {
            return (Teb*)Win32.NtCurrentTeb();
        }






        public static ThreadHandle OpenCurrent(ThreadAccess access)
        {
            return new ThreadHandle(GetCurrentId(), access);
        }

        public static ThreadHandle OpenWithAnyAccess(int tid)
        {
            try
            {
                return new ThreadHandle(tid, OSVersion.MinThreadQueryInfoAccess);
            }
            catch
            {
                try
                {
                    return new ThreadHandle(tid, (ThreadAccess)StandardRights.Synchronize);
                }
                catch
                {
                    try
                    {
                        return new ThreadHandle(tid, (ThreadAccess)StandardRights.ReadControl);
                    }
                    catch
                    {
                        try
                        {
                            return new ThreadHandle(tid, (ThreadAccess)StandardRights.WriteDac);
                        }
                        catch
                        {
                            return new ThreadHandle(tid, (ThreadAccess)StandardRights.WriteOwner);
                        }
                    }
                }
            }
        }





        public static void RegisterTerminationPort(PortHandle portHandle)
        {
            NtStatus status;

            if ((status = Win32.NtRegisterThreadTerminatePort(portHandle)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }







        public static NtStatus Sleep(long timeout, bool relative)
        {
            return Sleep(false, timeout, relative);
        }
        public static NtStatus Sleep(bool alertable, long timeout, bool relative)
        {
            if (timeout == 0)
            {
                Yield();
                return NtStatus.Success;
            }
            long realTime = relative ? -timeout : timeout;
            return Win32.NtDelayExecution(alertable, ref realTime);
        }
        public static NtStatus TestAlert()
        {
            NtStatus status;
            if ((status = Win32.NtTestAlert()) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return status;
        }
        public static void Yield()
        {
            Win32.NtYieldExecution();
        }
        internal ThreadHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }
        public ThreadHandle(int tid)
            : this(tid, ThreadAccess.All)
        { }
        public ThreadHandle(int tid, ThreadAccess access)
        {
            if (KProcessHacker.Instance != null)
            {
                try
                {
                    this.Handle = new IntPtr(KProcessHacker.Instance.KphOpenThread(tid, access));
                }
                catch (WindowsException)
                {
                    this.Handle = new IntPtr(KProcessHacker.Instance.KphOpenThread(tid,
                        (ThreadAccess)StandardRights.Synchronize));
                    KProcessHacker.Instance.KphSetHandleGrantedAccess(this.Handle, (int)access);
                }
            }
            else
            {
                this.Handle = Win32.OpenThread(access, false, tid);
            }
            if (this.Handle == IntPtr.Zero)
            {
                this.MarkAsInvalid();
                Win32.ThrowLastError();
            }
        }
        public ThreadHandle(
            string name,
            ObjectFlags objectFlags,
            DirectoryHandle rootDirectory,
            ClientId clientId,
            ThreadAccess access
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;
            try
            {
                if (clientId.ProcessId == 0 && clientId.ThreadId == 0)
                {
                    if ((status = Win32.NtOpenThread(
                        out handle,
                        access,
                        ref oa,
                        IntPtr.Zero
                        )) >= NtStatus.Error)
                        Win32.ThrowLastError(status);
                }
                else
                {
                    if ((status = Win32.NtOpenThread(
                        out handle,
                        access,
                        ref oa,
                        ref clientId
                        )) >= NtStatus.Error)
                        Win32.ThrowLastError(status);
                }
            }
            finally
            {
                oa.Dispose();
            }
            this.Handle = handle;
        }
        public ThreadHandle(string name, ThreadAccess access)
            : this(name, 0, null, new ClientId(), access)
        { }
        public void Alert()
        {
            NtStatus status;
            if ((status = Win32.NtAlertThread(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public int AlertResume()
        {
            NtStatus status;
            int suspendCount;
            if ((status = Win32.NtAlertResumeThread(this, out suspendCount)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return suspendCount;
        }
        public IntPtr[] CaptureKernelStack()
        {
            return this.CaptureKernelStack(0);
        }
        public IntPtr[] CaptureKernelStack(int skipCount)
        {
            IntPtr[] stack = new IntPtr[62 - skipCount];
            int hash;
            int captured = KProcessHacker.Instance.KphCaptureStackBackTraceThread(
                this,
                skipCount,
                stack.Length,
                stack,
                out hash
                );
            IntPtr[] newStack = new IntPtr[captured];
            Array.Copy(stack, 0, newStack, 0, captured);
            return newStack;
        }
        public ThreadStackFrame[] CaptureUserStack()
        {
            return this.CaptureUserStack(0);
        }
        public ThreadStackFrame[] CaptureUserStack(int skipCount)
        {
            List<ThreadStackFrame> frames = new List<ThreadStackFrame>();
            this.WalkStack((frame) => { frames.Add(frame); return true; });
            if (frames.Count <= skipCount)
                return new ThreadStackFrame[0];
            ThreadStackFrame[] newFrames = new ThreadStackFrame[frames.Count - skipCount];
            Array.Copy(frames.ToArray(), skipCount, newFrames, 0, newFrames.Length);
            return newFrames;
        }
        public void DangerousTerminate(NtStatus exitStatus)
        {
            KProcessHacker.Instance.KphDangerousTerminateThread(this, exitStatus);
        }
        public int GetBasePriority()
        {
            return this.GetInformationInt32(ThreadInformationClass.ThreadBasePriority);
        }
        public ThreadPriorityLevel GetBasePriorityWin32()
        {
            int priority = Win32.GetThreadPriority(this);
            if (priority == 0x7fffffff)
                Win32.ThrowLastError();
            return (ThreadPriorityLevel)priority;
        }
        public ThreadBasicInformation GetBasicInformation()
        {
            NtStatus status;
            ThreadBasicInformation basicInfo = new ThreadBasicInformation();
            int retLen;
            if ((status = Win32.NtQueryInformationThread(this, ThreadInformationClass.ThreadBasicInformation,
                ref basicInfo, Marshal.SizeOf(basicInfo), out retLen)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return basicInfo;
        }
        public Context GetContext(ContextFlags flags)
        {
            Context context = new Context();
            context.ContextFlags = flags;
            this.GetContext(ref context);
            return context;
        }
        public unsafe void GetContext(ref Context context)
        {
            if (KProcessHacker.Instance != null)
            {
                fixed (Context* contextPtr = &context)
                    KProcessHacker.Instance.KphGetContextThread(this, contextPtr);
            }
            else
            {
                NtStatus status;
                if ((status = Win32.NtGetContextThread(this, ref context)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
        }
        public ContextAmd64 GetContext(ContextFlagsAmd64 flags)
        {
            ContextAmd64 context = new ContextAmd64();
            context.ContextFlags = flags;
            this.GetContext(ref context);
            return context;
        }
        public void GetContext(ref ContextAmd64 context)
        {
            NtStatus status;
            using (var data = new AlignedMemoryAlloc(Utils.SizeOf<ContextAmd64>(16), 16))
            {
                data.WriteStruct<ContextAmd64>(context);
                if ((status = Win32.NtGetContextThread(this, data)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                context = data.ReadStruct<ContextAmd64>();
            }
        }
        public void GetContextWow64(ref Context context)
        {
            NtStatus status;
            if ((status = Win32.RtlWow64GetThreadContext(this, ref context)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public ulong GetCycleTime()
        {
            ulong cycles;
            if (!Win32.QueryThreadCycleTime(this, out cycles))
                Win32.ThrowLastError();
            return cycles;
        }
        public int GetExitCode()
        {
            int exitCode;
            if (!Win32.GetExitCodeThread(this, out exitCode))
                Win32.ThrowLastError();
            return exitCode;
        }
        public NtStatus GetExitStatus()
        {
            return this.GetBasicInformation().ExitStatus;
        }
        private int GetInformationInt32(ThreadInformationClass infoClass)
        {
            NtStatus status;
            int value;
            int retLength;
            if ((status = Win32.NtQueryInformationThread(
                this, infoClass, out value, sizeof(int), out retLength)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return value;
        }
        private IntPtr GetInformationIntPtr(ThreadInformationClass infoClass)
        {
            NtStatus status;
            IntPtr value;
            int retLength;
            if ((status = Win32.NtQueryInformationThread(
                this, infoClass, out value, IntPtr.Size, out retLength)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return value;
        }
        public int GetIoPriority()
        {
            return this.GetInformationInt32(ThreadInformationClass.ThreadIoPriority);
        }
        public int GetLastSystemCall()
        {
            int firstArgument;
            return this.GetLastSystemCall(out firstArgument);
        }
        public unsafe int GetLastSystemCall(out int firstArgument)
        {
            NtStatus status;
            int* data = stackalloc int[2];
            int retLength;
            if ((status = Win32.NtQueryInformationThread(
                this, ThreadInformationClass.ThreadLastSystemCall, data, sizeof(int) * 2, out retLength)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            firstArgument = data[0];
            return data[1];
        }
        public int GetPagePriority()
        {
            return this.GetInformationInt32(ThreadInformationClass.ThreadPagePriority);
        }
        public int GetPriority()
        {
            return this.GetInformationInt32(ThreadInformationClass.ThreadPriority);
        }
        public ProcessHandle GetProcess(ProcessAccess access)
        {
            return new ProcessHandle(this, access);
        }
        public int GetProcessId()
        {
            return this.GetBasicInformation().ClientId.ProcessId;
        }
        public int GetThreadId()
        {
            return this.GetBasicInformation().ClientId.ThreadId;
        }
        public TokenHandle GetToken()
        {
            return GetToken(TokenAccess.All);
        }
        public TokenHandle GetToken(TokenAccess access)
        {
            return new TokenHandle(this, access);
        }
        public IntPtr GetWin32StartAddress()
        {
            return this.GetInformationIntPtr(ThreadInformationClass.ThreadQuerySetWin32StartAddress);
        }
        public void Impersonate(ThreadHandle clientThreadHandle, SecurityImpersonationLevel impersonationLevel)
        {
            NtStatus status;
            SecurityQualityOfService securityQos =
                new SecurityQualityOfService(impersonationLevel, false, false);
            if ((status = Win32.NtImpersonateThread(this, clientThreadHandle, ref securityQos)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public void ImpersonateAnonymous()
        {
            NtStatus status;
            if ((status = Win32.NtImpersonateAnonymousToken(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public bool IsCritical()
        {
            return this.GetInformationInt32(ThreadInformationClass.ThreadBreakOnTermination) != 0;
        }
        public bool IsIoPending()
        {
            return this.GetInformationInt32(ThreadInformationClass.ThreadIsIoPending) != 0;
        }
        public bool IsLastThread()
        {
            return this.GetInformationInt32(ThreadInformationClass.ThreadAmILastThread) != 0;
        }
        public bool IsPriorityBoostEnabled()
        {
            return this.GetInformationInt32(ThreadInformationClass.ThreadPriorityBoost) == 0;
        }
        public bool IsTerminated()
        {
            return this.GetInformationInt32(ThreadInformationClass.ThreadIsTerminated) != 0;
        }
        public void QueueApc(IntPtr address, IntPtr parameter)
        {
            if (!Win32.QueueUserAPC(address, this, parameter))
                Win32.ThrowLastError();
        }
        public void QueueApc(ApcRoutine action, IntPtr parameter)
        {
            if (!Win32.QueueUserAPC(action, this, parameter))
                Win32.ThrowLastError();
        }
        public void QueueApc(IntPtr address, IntPtr param1, IntPtr param2, IntPtr param3)
        {
            NtStatus status;
            if ((status = Win32.NtQueueApcThread(
                this,
                address,
                param1,
                param2,
                param3
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public void RemoteCall(IntPtr address, IntPtr[] arguments)
        {
            this.RemoteCall(address, arguments, false);
        }
        public void RemoteCall(IntPtr address, IntPtr[] arguments, bool alreadySuspended)
        {
            ProcessHandle processHandle;
            if (KProcessHacker.Instance != null)
                processHandle = this.GetProcess(ProcessAccess.VmWrite);
            else
                processHandle = new ProcessHandle(this.GetProcessId(), ProcessAccess.VmWrite);
            using (processHandle)
                this.RemoteCall(processHandle, address, arguments, alreadySuspended);
        }
        public void RemoteCall(ProcessHandle processHandle, IntPtr address, IntPtr[] arguments, bool alreadySuspended)
        {
            NtStatus status;
            if ((status = Win32.RtlRemoteCall(
                processHandle,
                this,
                address,
                arguments.Length,
                arguments,
                false,
                alreadySuspended
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public int Resume()
        {
            NtStatus status;
            int suspendCount;
            if ((status = Win32.NtResumeThread(this, out suspendCount)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return suspendCount;
        }
        public void SetBasePriority(int basePriority)
        {
            this.SetInformationInt32(ThreadInformationClass.ThreadBasePriority, basePriority);
        }
        public void SetBasePriorityWin32(ThreadPriorityLevel basePriority)
        {
            if (!Win32.SetThreadPriority(this, (int)basePriority))
                Win32.ThrowLastError();
        }
        public unsafe void SetContext(Context context)
        {
            if (KProcessHacker.Instance != null)
            {
                KProcessHacker.Instance.KphSetContextThread(this, &context);
            }
            else
            {
                NtStatus status;
                if ((status = Win32.NtSetContextThread(this, ref context)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
        }
        public void SetContext(ContextAmd64 context)
        {
            NtStatus status;
            using (var data = new AlignedMemoryAlloc(Utils.SizeOf<ContextAmd64>(16), 16))
            {
                data.WriteStruct<ContextAmd64>(context);
                if ((status = Win32.NtSetContextThread(this, data)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
        }
        public void SetContextWow64(Context context)
        {
            NtStatus status;
            if ((status = Win32.RtlWow64SetThreadContext(this, ref context)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public void SetCritical(bool critical)
        {
            this.SetInformationInt32(ThreadInformationClass.ThreadBreakOnTermination, critical ? 1 : 0);
        }
        private void SetInformationInt32(ThreadInformationClass infoClass, int value)
        {
            NtStatus status;
            if ((status = Win32.NtSetInformationThread(
                this, infoClass, ref value, sizeof(int))) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        private void SetInformationIntPtr(ThreadInformationClass infoClass, IntPtr value)
        {
            NtStatus status;
            if ((status = Win32.NtSetInformationThread(
                this, infoClass, ref value, sizeof(int))) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public void SetPriority(int priority)
        {
            this.SetInformationInt32(ThreadInformationClass.ThreadPriority, priority);
        }
        public void SetPriorityBoost(bool enabled)
        {
            this.SetInformationInt32(ThreadInformationClass.ThreadPriorityBoost, enabled ? 0 : 1);
        }
        public void SetToken(TokenHandle tokenHandle)
        {
            this.SetInformationIntPtr(ThreadInformationClass.ThreadImpersonationToken, tokenHandle ?? IntPtr.Zero);
        }
        public int Suspend()
        {
            NtStatus status;
            int suspendCount;
            if ((status = Win32.NtSuspendThread(this, out suspendCount)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return suspendCount;
        }
        public void Terminate()
        {
            this.Terminate(NtStatus.Success);
        }
        public void Terminate(NtStatus exitStatus)
        {
            if (KProcessHacker.Instance != null)
            {
                try
                {
                    KProcessHacker.Instance.KphTerminateThread(this, exitStatus);
                    return;
                }
                catch (WindowsException ex)
                {
                    if (ex.ErrorCode != Win32Error.NotSupported)
                        throw ex;
                }
            }
            NtStatus status;
            if ((status = Win32.NtTerminateThread(this, exitStatus)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public void WalkStack(WalkStackDelegate walkStackCallback)
        {
            this.WalkStack(walkStackCallback, OSVersion.Architecture);
        }
        public void WalkStack(WalkStackDelegate walkStackCallback, OSArch architecture)
        {
            if (KProcessHacker.Instance != null)
            {
                using (var phandle = this.GetProcess(ProcessAccess.QueryInformation | ProcessAccess.VmRead))
                    this.WalkStack(phandle, walkStackCallback, architecture);
            }
            else
            {
                using (var dupThreadHandle = this.Duplicate(OSVersion.MinThreadQueryInfoAccess))
                using (var phandle = new ProcessHandle(
                    ThreadHandle.FromHandle(dupThreadHandle).GetBasicInformation().ClientId.ProcessId,
                    ProcessAccess.QueryInformation | ProcessAccess.VmRead
                    ))
                {
                    this.WalkStack(phandle, walkStackCallback, architecture);
                }
            }
        }
        public unsafe void WalkStack(ProcessHandle parentProcess, WalkStackDelegate walkStackCallback)
        {
            this.WalkStack(parentProcess, walkStackCallback, OSVersion.Architecture);
        }
        public unsafe void WalkStack(ProcessHandle parentProcess, WalkStackDelegate walkStackCallback, OSArch architecture)
        {
            bool suspended = false;
            try
            {
                this.Suspend();
                suspended = true;
            }
            catch (WindowsException)
            {
                suspended = false;
            }
            ReadProcessMemoryProc64 readMemoryProc = null;
            if (KProcessHacker.Instance != null)
            {
                readMemoryProc = new ReadProcessMemoryProc64(
                    delegate(IntPtr processHandle, ulong baseAddress, IntPtr buffer, int size, out int bytesRead)
                    {
                        return KProcessHacker.Instance.KphReadVirtualMemorySafe(
                            ProcessHandle.FromHandle(processHandle), (int)baseAddress, buffer, size, out bytesRead);
                    });
            }
            try
            {
                if (IntPtr.Size == 4 || (IntPtr.Size == 8 && architecture == OSArch.I386))
                {
                    Context context = new Context();
                    context.ContextFlags = ContextFlags.All;
                    if (IntPtr.Size == 4)
                    {
                        this.GetContext(ref context);
                    }
                    else
                    {
                        this.GetContextWow64(ref context);
                    }
                    var stackFrame = new StackFrame64();
                    stackFrame.AddrPC.Mode = AddressMode.AddrModeFlat;
                    stackFrame.AddrPC.Offset = (ulong)context.Eip;
                    stackFrame.AddrStack.Mode = AddressMode.AddrModeFlat;
                    stackFrame.AddrStack.Offset = (ulong)context.Esp;
                    stackFrame.AddrFrame.Mode = AddressMode.AddrModeFlat;
                    stackFrame.AddrFrame.Offset = (ulong)context.Ebp;
                    while (true)
                    {
                        using (Win32.DbgHelpLock.AcquireContext())
                        {
                            if (!Win32.StackWalk64(
                                MachineType.I386,
                                parentProcess,
                                this,
                                ref stackFrame,
                                ref context,
                                readMemoryProc,
                                Win32.SymFunctionTableAccess64,
                                Win32.SymGetModuleBase64,
                                IntPtr.Zero
                                ))
                                break;
                        }
                        if (stackFrame.AddrPC.Offset == 0)
                            break;
                        if (!walkStackCallback(new ThreadStackFrame(ref stackFrame)))
                            break;
                    }
                }
                else if (IntPtr.Size == 8)
                {
                    ContextAmd64 context = new ContextAmd64();
                    context.ContextFlags = ContextFlagsAmd64.All;
                    this.GetContext(ref context);
                    var stackFrame = new StackFrame64();
                    stackFrame.AddrPC.Mode = AddressMode.AddrModeFlat;
                    stackFrame.AddrPC.Offset = (ulong)context.Rip;
                    stackFrame.AddrStack.Mode = AddressMode.AddrModeFlat;
                    stackFrame.AddrStack.Offset = (ulong)context.Rsp;
                    stackFrame.AddrFrame.Mode = AddressMode.AddrModeFlat;
                    stackFrame.AddrFrame.Offset = (ulong)context.Rbp;
                    while (true)
                    {
                        using (Win32.DbgHelpLock.AcquireContext())
                        {
                            if (!Win32.StackWalk64(
                                MachineType.Amd64,
                                parentProcess,
                                this,
                                ref stackFrame,
                                ref context,
                                readMemoryProc,
                                Win32.SymFunctionTableAccess64,
                                Win32.SymGetModuleBase64,
                                IntPtr.Zero
                                ))
                                break;
                        }
                        if (stackFrame.AddrPC.Offset == 0)
                            break;
                        if (!walkStackCallback(new ThreadStackFrame(ref stackFrame)))
                            break;
                    }
                }
            }
            finally
            {
                if (suspended)
                {
                    try
                    {
                        this.Resume();
                    }
                    catch (WindowsException)
                    { }
                }
            }
        }
    }
    public class ThreadStackFrame
    {
        private IntPtr _pcAddress;
        private IntPtr _returnAddress;
        private IntPtr _frameAddress;
        private IntPtr _stackAddress;
        private IntPtr _bStoreAddress;
        private IntPtr[] _params;
        internal ThreadStackFrame(ref StackFrame64 stackFrame)
        {
            _pcAddress = new IntPtr((long)stackFrame.AddrPC.Offset);
            _returnAddress = new IntPtr((long)stackFrame.AddrReturn.Offset);
            _frameAddress = new IntPtr((long)stackFrame.AddrFrame.Offset);
            _stackAddress = new IntPtr((long)stackFrame.AddrStack.Offset);
            _bStoreAddress = new IntPtr((long)stackFrame.AddrBStore.Offset);
            _params = new IntPtr[4];
            for (int i = 0; i < 4; i++)
                _params[i] = new IntPtr(stackFrame.Params[i]);
        }
        public IntPtr PcAddress { get { return _pcAddress; } }
        public IntPtr ReturnAddress { get { return _returnAddress; } }
        public IntPtr FrameAddress { get { return _frameAddress; } }
        public IntPtr StackAddress { get { return _stackAddress; } }
        public IntPtr BStoreAddress { get { return _bStoreAddress; } }
        public IntPtr[] Params { get { return _params; } }
    }
}
