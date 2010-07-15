

using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Runtime.InteropServices;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public sealed class ProcessHandle : NativeHandle<ProcessAccess>, IWithToken
    {
        public delegate bool EnumMemoryDelegate(MemoryBasicInformation info);
        public delegate bool EnumModulesDelegate(ProcessModule module);
        private static readonly ProcessHandle _current = new ProcessHandle(new IntPtr(-1), false);
        public static ProcessHandle Current
        {
            get { return _current; }
        }
        public static ProcessHandle Create(
            ProcessAccess access,
            ProcessHandle parentProcess,
            bool inheritHandles,
            SectionHandle sectionHandle)
        {
            return Create(access, null, 0, null, parentProcess, inheritHandles, sectionHandle, null);
        }
        public static ProcessHandle Create(
            ProcessAccess access,
            string name,
            ObjectFlags objectFlags,
            DirectoryHandle rootDirectory,
            ProcessHandle parentProcess,
            bool inheritHandles,
            SectionHandle sectionHandle,
            DebugObjectHandle debugPort
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;
            try
            {
                if ((status = Win32.NtCreateProcess(
                    out handle,
                    access,
                    ref oa,
                    parentProcess ?? IntPtr.Zero,
                    inheritHandles,
                    sectionHandle ?? IntPtr.Zero,
                    debugPort ?? IntPtr.Zero,
                    IntPtr.Zero
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }
            return new ProcessHandle(handle, true);
        }
        public static ProcessHandle CreateExtended(
            string fileName,
            ProcessHandle parentProcess,
            ProcessCreationFlags creationFlags,
            bool inheritHandles,
            string currentDirectory,
            StartupInfo startupInfo,
            out ClientId clientId,
            out ThreadHandle threadHandle
            )
        {
            return CreateExtended(
                fileName,
                parentProcess,
                creationFlags,
                true,
                inheritHandles,
                EnvironmentBlock.GetCurrent(),
                currentDirectory,
                startupInfo,
                out clientId,
                out threadHandle
                );
        }
        public static ProcessHandle CreateExtended(
            string fileName,
            ProcessHandle parentProcess,
            ProcessCreationFlags creationFlags,
            bool notifyCsr,
            bool inheritHandles,
            EnvironmentBlock environment,
            string currentDirectory,
            StartupInfo startupInfo,
            out ClientId clientId,
            out ThreadHandle threadHandle
            )
        {
            ProcessHandle phandle;
            ThreadHandle thandle;
            SectionImageInformation imageInfo;
            if (startupInfo.Desktop == null)
                startupInfo.Desktop = ProcessHandle.Current.GetPebString(PebOffset.DesktopName);
            using (var fhandle = new FileHandle(
                fileName,
                FileShareMode.Read | FileShareMode.Delete,
                FileAccess.Execute | (FileAccess)StandardRights.Synchronize
                ))
            {
                using (var shandle = SectionHandle.Create(
                    SectionAccess.All,
                    SectionAttributes.Image,
                    MemoryProtection.Execute,
                    fhandle
                    ))
                {
                    imageInfo = shandle.GetImageInformation();
                    phandle = Create(
                        ProcessAccess.All,
                        parentProcess,
                        inheritHandles,
                        shandle
                        );
                }
            }
            IntPtr peb = phandle.GetBasicInformation().PebBaseAddress;
            NativeUtils.CopyProcessParameters(
                phandle,
                peb,
                creationFlags,
                FileUtils.GetFileName(fileName),
                ProcessHandle.Current.GetPebString(PebOffset.DllPath),
                currentDirectory,
                fileName,
                environment,
                startupInfo.Title != null ? startupInfo.Title : fileName,
                startupInfo.Desktop != null ? startupInfo.Desktop : "",
                startupInfo.Reserved != null ? startupInfo.Reserved : "",
                "",
                ref startupInfo
                );
            thandle = ThreadHandle.CreateUserThread(
                phandle,
                true,
                imageInfo.StackCommit.Increment(imageInfo.StackReserved).ToInt32(),
                imageInfo.StackCommit.ToInt32(),
                imageInfo.TransferAddress,
                IntPtr.Zero,
                out clientId
                );
            if (notifyCsr)
            {
                BaseCreateProcessMsg processMsg = new BaseCreateProcessMsg();
                processMsg.ProcessHandle = phandle;
                processMsg.ThreadHandle = thandle;
                processMsg.ClientId = clientId;
                processMsg.CreationFlags = creationFlags;
                if ((creationFlags & (ProcessCreationFlags.DebugProcess |
                    ProcessCreationFlags.DebugOnlyThisProcess)) != 0)
                {
                    NtStatus status;
                    status = Win32.DbgUiConnectToDbg();
                    if (status >= NtStatus.Error)
                    {
                        phandle.Terminate(status);
                        Win32.ThrowLastError(status);
                    }
                    processMsg.DebuggerClientId = ThreadHandle.GetCurrentCid();
                }
                if (imageInfo.ImageSubsystem == 2)
                    processMsg.ProcessHandle = processMsg.ProcessHandle.Or((1 | 2).ToIntPtr());
                if ((startupInfo.Flags & StartupFlags.ForceOnFeedback) ==
                    StartupFlags.ForceOnFeedback)
                    processMsg.ProcessHandle = processMsg.ProcessHandle.Or((1).ToIntPtr());
                if ((startupInfo.Flags & StartupFlags.ForceOffFeedback) ==
                    StartupFlags.ForceOffFeedback)
                    processMsg.ProcessHandle = processMsg.ProcessHandle.And((1).ToIntPtr().Not());
                using (var data = new MemoryAlloc(
                    CsrApiMsg.ApiMessageDataOffset + Marshal.SizeOf(typeof(BaseCreateProcessMsg))
                    ))
                {
                    data.WriteStruct<BaseCreateProcessMsg>(CsrApiMsg.ApiMessageDataOffset, 0, processMsg);
                    Win32.CsrClientCallServer(
                        data,
                        IntPtr.Zero,
                        Win32.CsrMakeApiNumber(Win32.BaseSrvServerDllIndex, (int)BaseSrvApiNumber.BasepCreateProcess),
                        Marshal.SizeOf(typeof(BaseCreateProcessMsg))
                        );
                    NtStatus status = (NtStatus)data.ReadStruct<CsrApiMsg>().ReturnValue;
                    if (status >= NtStatus.Error)
                    {
                        phandle.Terminate(status);
                        Win32.ThrowLastError(status);
                    }
                }
            }
            if ((creationFlags & ProcessCreationFlags.CreateSuspended) == 0)
                thandle.Resume();
            threadHandle = thandle;
            return phandle;
        }
        public static ProcessHandle CreateUserProcess(string fileName, out ClientId clientId, out ThreadHandle threadHandle)
        {
            NtStatus status;
            UnicodeString fileNameStr = new UnicodeString(fileName);
            RtlUserProcessParameters processParams = new RtlUserProcessParameters();
            RtlUserProcessInformation processInfo;
            processParams.Length = Marshal.SizeOf(processParams);
            processParams.MaximumLength = processParams.Length;
            processParams.ImagePathName = new UnicodeString(fileName);
            processParams.CommandLine = new UnicodeString(fileName);
            Win32.RtlCreateEnvironment(true, out processParams.Environment);
            try
            {
                if ((status = Win32.RtlCreateUserProcess(
                    ref fileNameStr,
                    0,
                    ref processParams,
                    IntPtr.Zero,
                    IntPtr.Zero,
                    IntPtr.Zero,
                    false,
                    IntPtr.Zero,
                    IntPtr.Zero,
                    out processInfo
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                clientId = processInfo.ClientId;
                threadHandle = new ThreadHandle(processInfo.Thread, true);
                return new ProcessHandle(processInfo.Process, true);
            }
            finally
            {
                fileNameStr.Dispose();
                processParams.ImagePathName.Dispose();
                processParams.CommandLine.Dispose();
                Win32.RtlDestroyEnvironment(processParams.Environment);
            }
        }
        public static ProcessHandle CreateWin32(
            string applicationName,
            string commandLine,
            bool inheritHandles,
            ProcessCreationFlags creationFlags,
            EnvironmentBlock environment,
            string currentDirectory,
            StartupInfo startupInfo,
            out ClientId clientId,
            out ThreadHandle threadHandle
            )
        {
            ProcessInformation processInformation;
            if (!Win32.CreateProcess(
                applicationName,
                commandLine,
                IntPtr.Zero,
                IntPtr.Zero,
                inheritHandles,
                creationFlags,
                environment,
                currentDirectory,
                ref startupInfo,
                out processInformation
                ))
                Win32.ThrowLastError();
            clientId = new ClientId(processInformation.ProcessId, processInformation.ThreadId);
            threadHandle = new ThreadHandle(processInformation.ThreadHandle, true);
            return new ProcessHandle(processInformation.ProcessHandle, true);
        }
        public static ProcessHandle FromHandle(IntPtr handle)
        {
            return new ProcessHandle(handle, false);
        }
        public static ProcessHandle GetCurrent()
        {
            return Current;
        }
        public static int GetCurrentId()
        {
            return Win32.GetCurrentProcessId();
        }
        public unsafe static Peb* GetCurrentPeb()
        {
            return (Peb*)ThreadHandle.GetCurrentTeb()->ProcessEnvironmentBlock;
        }
        public unsafe static RtlUserProcessParameters* GetCurrentProcessParameters()
        {
            return (RtlUserProcessParameters*)GetCurrentPeb()->ProcessParameters;
        }
        private static int GetPebOffset(PebOffset offset)
        {
            switch (offset)
            {
                case PebOffset.CommandLine:
                    return RtlUserProcessParameters.CommandLineOffset;
                case PebOffset.CurrentDirectoryPath:
                    return RtlUserProcessParameters.CurrentDirectoryOffset;
                case PebOffset.DesktopName:
                    return RtlUserProcessParameters.DesktopInfoOffset;
                case PebOffset.DllPath:
                    return RtlUserProcessParameters.DllPathOffset;
                case PebOffset.ImagePathName:
                    return RtlUserProcessParameters.ImagePathNameOffset;
                case PebOffset.RuntimeData:
                    return RtlUserProcessParameters.RuntimeDataOffset;
                case PebOffset.ShellInfo:
                    return RtlUserProcessParameters.ShellInfoOffset;
                case PebOffset.WindowTitle:
                    return RtlUserProcessParameters.WindowTitleOffset;
                default:
                    throw new ArgumentException("offset");
            }
        }
        public static ProcessHandle[] OpenByName(string processName, ProcessAccess access)
        {
            var processes = Windows.GetProcesses();
            List<ProcessHandle> processHandles = new List<ProcessHandle>();
            foreach (var process in processes.Values)
            {
                if (string.Equals(process.Name, processName, StringComparison.InvariantCultureIgnoreCase))
                {
                    try
                    {
                        processHandles.Add(new ProcessHandle(process.Process.ProcessId, access));
                    }
                    catch
                    { }
                }
            }
            return processHandles.ToArray();
        }
        public static ProcessHandle OpenCurrent(ProcessAccess access)
        {
            return new ProcessHandle(GetCurrentId(), access);
        }
        public static ProcessHandle OpenWithAnyAccess(int pid)
        {
            try
            {
                return new ProcessHandle(pid, OSVersion.MinProcessQueryInfoAccess);
            }
            catch
            {
                try
                {
                    return new ProcessHandle(pid, (ProcessAccess)StandardRights.Synchronize);
                }
                catch
                {
                    try
                    {
                        return new ProcessHandle(pid, (ProcessAccess)StandardRights.ReadControl);
                    }
                    catch
                    {
                        try
                        {
                            return new ProcessHandle(pid, (ProcessAccess)StandardRights.WriteDac);
                        }
                        catch
                        {
                            return new ProcessHandle(pid, (ProcessAccess)StandardRights.WriteOwner);
                        }
                    }
                }
            }
        }
        private ProcessHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }
        public ProcessHandle(int pid)
            : this(pid, ProcessAccess.All)
        { }
        public ProcessHandle(int pid, ProcessAccess access)
        {
            if (KProcessHacker.Instance != null)
            {
                try
                {
                    this.Handle = new IntPtr(KProcessHacker.Instance.KphOpenProcess(pid, access));
                }
                catch (WindowsException)
                {
                    this.Handle = new IntPtr(KProcessHacker.Instance.KphOpenProcess(pid,
                        (ProcessAccess)StandardRights.Synchronize));
                    KProcessHacker.Instance.KphSetHandleGrantedAccess(this.Handle, (int)access);
                }
            }
            else
            {
                this.Handle = Win32.OpenProcess(access, false, pid);
            }
            if (this.Handle == IntPtr.Zero)
            {
                this.MarkAsInvalid();
                Win32.ThrowLastError();
            }
        }
        public ProcessHandle(ThreadHandle threadHandle, ProcessAccess access)
        {
            if (KProcessHacker.Instance == null)
                throw new NotSupportedException();
            this.Handle = new IntPtr(KProcessHacker.Instance.KphOpenThreadProcess(threadHandle, access));
        }
        public ProcessHandle(
            string name,
            ObjectFlags objectFlags,
            DirectoryHandle rootDirectory,
            ClientId clientId,
            ProcessAccess access
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;
            try
            {
                if (name != null)
                {
                    if ((status = Win32.NtOpenProcess(
                        out handle,
                        access,
                        ref oa,
                        IntPtr.Zero
                        )) >= NtStatus.Error)
                        Win32.ThrowLastError(status);
                }
                else
                {
                    if ((status = Win32.NtOpenProcess(
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
        public ProcessHandle(string name, ProcessAccess access)
            : this(name, 0, null, new ClientId(), access)
        { }
        public ProcessHandle(ClientId clientId, ProcessAccess access)
            : this(null, 0, null, clientId, access)
        { }
        public IntPtr AllocateMemory(int size, MemoryProtection protection)
        {
            return this.AllocateMemory(size, MemoryFlags.Commit, protection);
        }
        public IntPtr AllocateMemory(int size, MemoryFlags type, MemoryProtection protection)
        {
            return this.AllocateMemory(IntPtr.Zero, size, type, protection);
        }
        public IntPtr AllocateMemory(IntPtr baseAddress, int size, MemoryFlags type, MemoryProtection protection)
        {
            IntPtr sizeIntPtr = new IntPtr(size);
            return this.AllocateMemory(baseAddress, ref sizeIntPtr, type, protection);
        }
        public IntPtr AllocateMemory(IntPtr baseAddress, ref IntPtr size, MemoryFlags type, MemoryProtection protection)
        {
            NtStatus status;
            if ((status = Win32.NtAllocateVirtualMemory(
                this,
                ref baseAddress,
                IntPtr.Zero,
                ref size,
                type,
                protection
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return baseAddress;
        }
        public void AssignToJobObject(JobObjectHandle job)
        {
            if (!Win32.AssignProcessToJobObject(job, this))
                Win32.ThrowLastError();
        }
        public ThreadHandle CreateThread(IntPtr startAddress, IntPtr parameter)
        {
            return this.CreateThread(startAddress, parameter, false);
        }
        public ThreadHandle CreateThread(IntPtr startAddress, IntPtr parameter, bool createSuspended)
        {
            int threadId;
            return this.CreateThread(startAddress, parameter, createSuspended, out threadId);
        }
        public ThreadHandle CreateThread(IntPtr startAddress, IntPtr parameter, bool createSuspended, out int threadId)
        {
            ClientId cid;
            ThreadHandle thandle = ThreadHandle.CreateUserThread(
                this,
                createSuspended,
                0,
                0,
                startAddress,
                parameter,
                out cid
                );
            threadId = cid.ThreadId;
            return thandle;
        }
        public ThreadHandle CreateThreadWin32(IntPtr startAddress, IntPtr parameter)
        {
            return this.CreateThreadWin32(startAddress, parameter, false);
        }
        public ThreadHandle CreateThreadWin32(IntPtr startAddress, IntPtr parameter, bool createSuspended)
        {
            int threadId;
            return this.CreateThreadWin32(startAddress, parameter, createSuspended, out threadId);
        }
        public ThreadHandle CreateThreadWin32(IntPtr startAddress, IntPtr parameter, bool createSuspended, out int threadId)
        {
            IntPtr threadHandle;
            if ((threadHandle = Win32.CreateRemoteThread(
                this,
                IntPtr.Zero,
                IntPtr.Zero,
                startAddress,
                parameter,
                createSuspended ? ProcessCreationFlags.CreateSuspended : 0,
                out threadId
                )) == IntPtr.Zero)
                Win32.ThrowLastError();
            return new ThreadHandle(threadHandle, true);
        }
        public void Debug(DebugObjectHandle debugObjectHandle)
        {
            NtStatus status;
            if ((status = Win32.NtDebugActiveProcess(this, debugObjectHandle)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public void DisableHandleTracing()
        {
            NtStatus status;
            if ((status = Win32.NtSetInformationProcess(
                this,
                ProcessInformationClass.ProcessHandleTracing,
                IntPtr.Zero,
                0
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public void EmptyWorkingSet()
        {
            if (!Win32.EmptyWorkingSet(this))
                Win32.ThrowLastError();
        }
        public void EnableHandleTracing()
        {
            NtStatus status;
            ProcessHandleTracingEnable phte = new ProcessHandleTracingEnable();
            if ((status = Win32.NtSetInformationProcess(
                this,
                ProcessInformationClass.ProcessHandleTracing,
                ref phte,
                Marshal.SizeOf(phte)
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public void EnumMemory(EnumMemoryDelegate enumMemoryCallback)
        {
            IntPtr address = IntPtr.Zero;
            MemoryBasicInformation mbi = new MemoryBasicInformation();
            int mbiSize = Marshal.SizeOf(mbi);
            while (Win32.VirtualQueryEx(this, address, out mbi, mbiSize) != 0)
            {
                if (!enumMemoryCallback(mbi))
                    break;
                address = address.Increment(mbi.RegionSize);
            }
        }
        public void EnumModules(EnumModulesDelegate enumModulesCallback)
        {
            this.EnumModulesNative(enumModulesCallback);
        }
        private void EnumModulesApi(EnumModulesDelegate enumModulesCallback)
        {
            IntPtr[] moduleHandles;
            int requiredSize;
            Win32.EnumProcessModules(this, null, 0, out requiredSize);
            moduleHandles = new IntPtr[requiredSize / 4];
            if (!Win32.EnumProcessModules(this, moduleHandles, requiredSize, out requiredSize))
                Win32.ThrowLastError();
            for (int i = 0; i < moduleHandles.Length; i++)
            {
                ModuleInfo moduleInfo = new ModuleInfo();
                StringBuilder baseName = new StringBuilder(0x400);
                StringBuilder fileName = new StringBuilder(0x400);
                if (!Win32.GetModuleInformation(this, moduleHandles[i], moduleInfo, Marshal.SizeOf(moduleInfo)))
                    Win32.ThrowLastError();
                if (Win32.GetModuleBaseName(this, moduleHandles[i], baseName, baseName.Capacity * 2) == 0)
                    Win32.ThrowLastError();
                if (Win32.GetModuleFileNameEx(this, moduleHandles[i], fileName, fileName.Capacity * 2) == 0)
                    Win32.ThrowLastError();
                if (!enumModulesCallback(new ProcessModule(
                    moduleInfo.BaseOfDll, moduleInfo.SizeOfImage, moduleInfo.EntryPoint, 0,
                    baseName.ToString(), FileUtils.GetFileName(fileName.ToString())
                    )))
                    break;
            }
        }
        private unsafe void EnumModulesNative(EnumModulesDelegate enumModulesCallback)
        {
            byte* buffer = stackalloc byte[IntPtr.Size];
            this.ReadMemory(this.GetBasicInformation().PebBaseAddress.Increment(Peb.LdrOffset), buffer, IntPtr.Size);
            IntPtr loaderData = *(IntPtr*)buffer;
            PebLdrData* data = stackalloc PebLdrData[1];
            this.ReadMemory(loaderData, data, Marshal.SizeOf(typeof(PebLdrData)));
            if (!data->Initialized)
                throw new Exception("Loader data is not initialized.");
            IntPtr currentLink = data->InLoadOrderModuleList.Flink;
            IntPtr startLink = currentLink;
            LdrDataTableEntry* currentEntry = stackalloc LdrDataTableEntry[1];
            int i = 0;
            while (currentLink != IntPtr.Zero)
            {
                if (i > 0 && currentLink == startLink)
                    break;
                if (i > 0x800)
                    break;
                this.ReadMemory(currentLink, currentEntry, Marshal.SizeOf(typeof(LdrDataTableEntry)));
                if (currentEntry->DllBase != IntPtr.Zero)
                {
                    string baseDllName = null;
                    string fullDllName = null;
                    try
                    {
                        baseDllName = currentEntry->BaseDllName.Read(this).TrimEnd('\0');
                    }
                    catch
                    { }
                    try
                    {
                        fullDllName = FileUtils.GetFileName(currentEntry->FullDllName.Read(this).TrimEnd('\0'));
                    }
                    catch
                    { }
                    if (!enumModulesCallback(new ProcessModule(
                        currentEntry->DllBase,
                        currentEntry->SizeOfImage,
                        currentEntry->EntryPoint,
                        currentEntry->Flags,
                        baseDllName,
                        fullDllName
                        )))
                        break;
                }
                currentLink = currentEntry->InLoadOrderLinks.Flink;
                i++;
            }
        }
        public NtStatus FlushMemory(IntPtr baseAddress, int size)
        {
            NtStatus status;
            IntPtr sizeIntPtr = size.ToIntPtr();
            IoStatusBlock isb;
            if ((status = Win32.NtFlushVirtualMemory(
                this,
                ref baseAddress,
                ref sizeIntPtr,
                out isb
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return isb.Status;
        }
        public void FreeMemory(IntPtr baseAddress, int size)
        {
            this.FreeMemory(baseAddress, size, false);
        }
        public void FreeMemory(IntPtr baseAddress, int size, bool reserveOnly)
        {
            NtStatus status;
            IntPtr sizeIntPtr = size.ToIntPtr();
            if (!reserveOnly)
                sizeIntPtr = IntPtr.Zero;
            if ((status = Win32.NtFreeVirtualMemory(
                this,
                ref baseAddress,
                ref sizeIntPtr,
                reserveOnly ? MemoryFlags.Decommit : MemoryFlags.Release
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public long GetAffinityMask()
        {
            long systemMask;
            return this.GetAffinityMask(out systemMask);
        }
        public long GetAffinityMask(out long systemMask)
        {
            IntPtr processMaskTemp;
            IntPtr systemMaskTemp;
            if (!Win32.GetProcessAffinityMask(this, out processMaskTemp, out systemMaskTemp))
                Win32.ThrowLastError();
            systemMask = systemMaskTemp.ToInt64();
            return processMaskTemp.ToInt64();
        }
        public int GetBasePriority()
        {
            return this.GetInformationInt32(ProcessInformationClass.ProcessBasePriority);
        }
        public ProcessBasicInformation GetBasicInformation()
        {
            NtStatus status;
            ProcessBasicInformation pbi;
            int retLen;
            if ((status = Win32.NtQueryInformationProcess(this, ProcessInformationClass.ProcessBasicInformation,
                out pbi, Marshal.SizeOf(typeof(ProcessBasicInformation)), out retLen)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return pbi;
        }
        public string GetCommandLine()
        {
            if (!this.IsPosix())
                return this.GetPebString(PebOffset.CommandLine);
            else
                return this.GetPosixCommandLine();
        }
        public int GetCookie()
        {
            return this.GetInformationInt32(ProcessInformationClass.ProcessCookie);
        }
        public DateTime GetCreateTime()
        {
            return DateTime.FromFileTime(this.GetTimes()[0]);
        }
        public ulong GetCycleTime()
        {
            ulong cycles;
            if (!Win32.QueryProcessCycleTime(this, out cycles))
                Win32.ThrowLastError();
            return cycles;
        }
        public DebugObjectHandle GetDebugObject()
        {
            IntPtr handle;
            handle = this.GetDebugObjectHandle();
            if (handle == IntPtr.Zero)
                return null;
            return new DebugObjectHandle(handle, true);
        }
        internal IntPtr GetDebugObjectHandle()
        {
            return this.GetInformationIntPtr(ProcessInformationClass.ProcessDebugObjectHandle);
        }
        public DepStatus GetDepStatus()
        {
            MemExecuteOptions options;
            if (IntPtr.Size == 8)
            {
                if (!this.IsWow64())
                    return DepStatus.Enabled | DepStatus.Permanent;
            }
            options = (MemExecuteOptions)this.GetInformationInt32(ProcessInformationClass.ProcessExecuteFlags);
            DepStatus depStatus = 0;
            if ((options & MemExecuteOptions.ExecuteEnable) == MemExecuteOptions.ExecuteEnable)
                return 0;
            if ((options & MemExecuteOptions.ExecuteDisable) == MemExecuteOptions.ExecuteDisable)
                depStatus = DepStatus.Enabled;
            else if ((options & MemExecuteOptions.ExecuteDisable) == 0 &&
                (options & MemExecuteOptions.ExecuteEnable) == 0)
                depStatus = DepStatus.Enabled;
            if ((options & MemExecuteOptions.DisableThunkEmulation) == MemExecuteOptions.DisableThunkEmulation)
                depStatus |= DepStatus.AtlThunkEmulationDisabled;
            if ((options & MemExecuteOptions.Permanent) == MemExecuteOptions.Permanent)
                depStatus |= DepStatus.Permanent;
            return depStatus;
        }
        public unsafe IDictionary<string, string> GetEnvironmentVariables()
        {
            IntPtr pebBaseAddress = this.GetBasicInformation().PebBaseAddress;
            byte* buffer = stackalloc byte[IntPtr.Size];
            this.ReadMemory(pebBaseAddress.Increment(Peb.ProcessParametersOffset), buffer, IntPtr.Size);
            IntPtr processParameters = *(IntPtr*)buffer;
            this.ReadMemory(processParameters.Increment(RtlUserProcessParameters.EnvironmentOffset), buffer, IntPtr.Size);
            IntPtr envBase = *(IntPtr*)buffer;
            int length = 0;
            {
                MemoryBasicInformation mbi = this.QueryMemory(envBase);
                if (mbi.Protect == MemoryProtection.NoAccess)
                    throw new WindowsException();
                length = mbi.RegionSize.Decrement(envBase.Decrement(mbi.BaseAddress)).ToInt32();
            }
            byte[] memory = this.ReadMemory(envBase, length);
            Dictionary<string, string> vars = new Dictionary<string, string>();
            StringBuilder currentVariable = new StringBuilder();
            int i = 0;
            while (true)
            {
                if (i >= memory.Length)
                    break;
                char currentChar =
                    UnicodeEncoding.Unicode.GetChars(memory, i, 2)[0];
                i += 2;
                if (currentChar == '\0')
                {
                    if (currentVariable.Length == 0)
                        break;
                    string[] s = currentVariable.ToString().Split(new char[] { '=' }, 2);
                    if (!vars.ContainsKey(s[0]) && s.Length > 1)
                        vars.Add(s[0], s[1]);
                    currentVariable = new StringBuilder();
                }
                else
                {
                    currentVariable.Append(currentChar);
                }
            }
            return vars;
        }
        public int GetExitCode()
        {
            int exitCode;
            if (!Win32.GetExitCodeProcess(this, out exitCode))
                Win32.ThrowLastError();
            return exitCode;
        }
        public NtStatus GetExitStatus()
        {
            return this.GetBasicInformation().ExitStatus;
        }
        public DateTime GetExitTime()
        {
            return DateTime.FromFileTime(this.GetTimes()[1]);
        }
        public int GetGuiResources(bool userObjects)
        {
            return Win32.GetGuiResources(this, userObjects ? 1 : 0);
        }
        public int GetHandleCount()
        {
            return this.GetInformationInt32(ProcessInformationClass.ProcessHandleCount);
        }
        public ProcessHandleInformation[] GetHandles()
        {
            int returnLength = 0;
            int attempts = 0;
            using (var data = new MemoryAlloc(0x1000))
            {
                while (true)
                {
                    try
                    {
                        KProcessHacker.Instance.KphQueryProcessHandles(this, data, data.Size, out returnLength);
                    }
                    catch (WindowsException ex)
                    {
                        if (attempts > 3)
                            throw ex;
                        if (
                            ex.Status == NtStatus.BufferTooSmall &&
                            returnLength > data.Size
                            )
                            data.Resize(returnLength);
                        attempts++;
                        continue;
                    }
                    int handleCount = data.ReadInt32(0);
                    ProcessHandleInformation[] handles = new ProcessHandleInformation[handleCount];
                    for (int i = 0; i < handleCount; i++)
                        handles[i] = data.ReadStruct<ProcessHandleInformation>(sizeof(int), i);
                    return handles;
                }
            }
        }
        public ProcessHandleTraceCollection GetHandleTraces()
        {
            return this.GetHandleTraces(IntPtr.Zero);
        }
        public ProcessHandleTraceCollection GetHandleTraces(IntPtr handle)
        {
            NtStatus status = NtStatus.Success;
            int retLength;
            using (var data = new MemoryAlloc(0x10000))
            {
                var query = new ProcessHandleTracingQuery();
                query.Handle = handle;
                data.WriteStruct<ProcessHandleTracingQuery>(query);
                for (int i = 0; i < 8; i++)
                {
                    status = Win32.NtQueryInformationProcess(
                        this,
                        ProcessInformationClass.ProcessHandleTracing,
                        data,
                        data.Size,
                        out retLength
                        );
                    if (status == NtStatus.InfoLengthMismatch)
                    {
                        data.Resize(data.Size * 4);
                        continue;
                    }
                    if (status >= NtStatus.Error)
                        Win32.ThrowLastError(status);
                    return new ProcessHandleTraceCollection(data);
                }
                Win32.ThrowLastError(status);
                return null;
            }
        }
        public unsafe IntPtr GetHeap()
        {
            IntPtr heap;
            this.ReadMemory(
                this.GetBasicInformation().PebBaseAddress.Increment(Peb.ProcessHeapOffset),
                &heap,
                IntPtr.Size
                );
            return heap;
        }
        public string GetImageFileName()
        {
            return this.GetInformationUnicodeString(ProcessInformationClass.ProcessImageFileName);
        }
        public string GetImageFileNameWin32()
        {
            return this.GetInformationUnicodeString(ProcessInformationClass.ProcessImageFileNameWin32);
        }
        private int GetInformationInt32(ProcessInformationClass infoClass)
        {
            NtStatus status;
            int value;
            int retLength;
            if ((status = Win32.NtQueryInformationProcess(
                this, infoClass, out value, sizeof(int), out retLength)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return value;
        }
        private IntPtr GetInformationIntPtr(ProcessInformationClass infoClass)
        {
            NtStatus status;
            IntPtr value;
            int retLength;
            if ((status = Win32.NtQueryInformationProcess(
                this, infoClass, out value, IntPtr.Size, out retLength)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return value;
        }
        private string GetInformationUnicodeString(ProcessInformationClass infoClass)
        {
            NtStatus status;
            int retLen;
            Win32.NtQueryInformationProcess(this, infoClass, IntPtr.Zero, 0, out retLen);
            using (MemoryAlloc data = new MemoryAlloc(retLen))
            {
                if ((status = Win32.NtQueryInformationProcess(this, infoClass, data, retLen, out retLen)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                return data.ReadStruct<UnicodeString>().Read();
            }
        }
        public int GetIoPriority()
        {
            return this.GetInformationInt32(ProcessInformationClass.ProcessIoPriority);
        }
        public IoCounters GetIoStatistics()
        {
            NtStatus status;
            IoCounters counters;
            int retLength;
            if ((status = Win32.NtQueryInformationProcess(
                this,
                ProcessInformationClass.ProcessIoCounters,
                out counters,
                Marshal.SizeOf(typeof(IoCounters)),
                out retLength
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return counters;
        }
        public JobObjectHandle GetJobObject(JobObjectAccess access)
        {
            try
            {
                return new JobObjectHandle(this, access);
            }
            catch (WindowsException ex)
            {
                if (ex.Status == NtStatus.ProcessNotInJob)
                    return null;
                else
                    throw ex;
            }
        }
        public KnownProcess GetKnownProcessType()
        {
            if (this.GetBasicInformation().UniqueProcessId.Equals(4))
                return KnownProcess.System;
            string fileName = FileUtils.GetFileName(this.GetImageFileName());
            if (fileName.ToLower().StartsWith(Environment.SystemDirectory.ToLower()))
            {
                string baseName = fileName.Remove(0, Environment.SystemDirectory.Length).TrimStart('\\').ToLower();
                switch (baseName)
                {
                    case "smss.exe":
                        return KnownProcess.SessionManager;
                    case "csrss.exe":
                        return KnownProcess.WindowsSubsystem;
                    case "wininit.exe":
                        return KnownProcess.WindowsStartup;
                    case "services.exe":
                        return KnownProcess.ServiceControlManager;
                    case "lsass.exe":
                        return KnownProcess.LocalSecurityAuthority;
                    case "lsm.exe":
                        return KnownProcess.LocalSessionManager;
                    default:
                        return KnownProcess.None;
                }
            }
            else
            {
                return KnownProcess.None;
            }
        }
        public ProcessModule GetMainModule()
        {
            ProcessModule mainModule = null;
            this.EnumModules((module) =>
            {
                mainModule = module;
                return false;
            });
            return mainModule;
        }
        public string GetMappedFileName(IntPtr address)
        {
            StringBuilder sb = new StringBuilder(0x400);
            int length = Win32.GetMappedFileName(this, address, sb, sb.Capacity);
            if (length > 0)
            {
                string fileName = sb.ToString(0, length);
                return FileUtils.GetFileName(fileName, true);
            }
            return null;
        }
        public VmCounters GetMemoryStatistics()
        {
            NtStatus status;
            VmCounters counters;
            int retLength;
            if ((status = Win32.NtQueryInformationProcess(
                this,
                ProcessInformationClass.ProcessVmCounters,
                out counters,
                Marshal.SizeOf(typeof(VmCounters)),
                out retLength
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return counters;
        }
        public ProcessModule[] GetModules()
        {
            List<ProcessModule> modules = new List<ProcessModule>();
            this.EnumModules((module) =>
            {
                modules.Add(module);
                return true;
            });
            return modules.ToArray();
        }
        public ProcessHandle GetNextProcess(ProcessAccess access)
        {
            NtStatus status;
            IntPtr handle;
            if ((status = Win32.NtGetNextProcess(
                this,
                access,
                0,
                0,
                out handle
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            if (handle != IntPtr.Zero)
                return new ProcessHandle(handle, true);
            else
                return null;
        }
        public ThreadHandle GetNextThread(ThreadHandle threadHandle, ThreadAccess access)
        {
            NtStatus status;
            IntPtr handle;
            if ((status = Win32.NtGetNextThread(
                this,
                threadHandle != null ? threadHandle : IntPtr.Zero,
                access,
                0,
                0,
                out handle
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            if (handle != IntPtr.Zero)
                return new ThreadHandle(handle, true);
            else
                return null;
        }
        public int GetPagePriority()
        {
            return this.GetInformationInt32(ProcessInformationClass.ProcessPagePriority);
        }
        public int GetParentPid()
        {
            return this.GetBasicInformation().InheritedFromUniqueProcessId.ToInt32();
        }
        public unsafe string GetPebString(PebOffset offset)
        {
            byte* buffer = stackalloc byte[IntPtr.Size];
            IntPtr pebBaseAddress = this.GetBasicInformation().PebBaseAddress;
            this.ReadMemory(pebBaseAddress.Increment(Peb.ProcessParametersOffset), buffer, IntPtr.Size);
            IntPtr processParameters = *(IntPtr*)buffer;
            int realOffset = GetPebOffset(offset);
            UnicodeString pebStr;
            this.ReadMemory(processParameters.Increment(realOffset), &pebStr, Marshal.SizeOf(typeof(UnicodeString)));
            return UnicodeEncoding.Unicode.GetString(
                this.ReadMemory(pebStr.Buffer, pebStr.Length), 0, pebStr.Length);
        }
        public unsafe string GetPosixCommandLine()
        {
            byte* buffer = stackalloc byte[IntPtr.Size];
            IntPtr pebBaseAddress = this.GetBasicInformation().PebBaseAddress;
            this.ReadMemory(pebBaseAddress.Increment(Peb.ProcessParametersOffset), buffer, IntPtr.Size);
            IntPtr processParameters = *(IntPtr*)buffer;
            UnicodeString commandLineUs;
            this.ReadMemory(
                processParameters.Increment(GetPebOffset(PebOffset.CommandLine)),
                &commandLineUs,
                Marshal.SizeOf(typeof(UnicodeString))
                );
            IntPtr stringAddr = commandLineUs.Buffer;
            List<IntPtr> strPointers = new List<IntPtr>();
            bool zeroReached = false;
            int i = 0;
            while (true)
            {
                this.ReadMemory(stringAddr.Increment(i), buffer, IntPtr.Size);
                IntPtr value = *(IntPtr*)buffer;
                if (value != IntPtr.Zero)
                    strPointers.Add(value);
                i += IntPtr.Size;
                if (zeroReached)
                    break;
                else if (value == IntPtr.Zero)
                    zeroReached = true;
            }
            IntPtr lastPointer = strPointers[strPointers.Count - 1];
            int partsSize = lastPointer.Decrement(strPointers[0]).ToInt32();
            StringBuilder commandLine = new StringBuilder();
            for (i = 0; i < strPointers.Count - 1; i++)
            {
                byte[] data = this.ReadMemory(strPointers[i], partsSize);
                commandLine.Append(ASCIIEncoding.ASCII.GetString(data, 0, Array.IndexOf<byte>(data, 0)) + " ");
            }
            string commandLineStr = commandLine.ToString();
            if (commandLineStr.EndsWith(" "))
                commandLineStr = commandLineStr.Remove(commandLineStr.Length - 1, 1);
            return commandLineStr;
        }
        public ProcessPriorityClass GetPriorityClass()
        {
            switch (Win32.GetPriorityClass(this))
            {
                case ProcessPriorityClassWin32.AboveNormal:
                    return ProcessPriorityClass.AboveNormal;
                case ProcessPriorityClassWin32.BelowNormal:
                    return ProcessPriorityClass.BelowNormal;
                case ProcessPriorityClassWin32.High:
                    return ProcessPriorityClass.High;
                case ProcessPriorityClassWin32.Idle:
                    return ProcessPriorityClass.Idle;
                case ProcessPriorityClassWin32.Normal:
                    return ProcessPriorityClass.Normal;
                case ProcessPriorityClassWin32.RealTime:
                    return ProcessPriorityClass.RealTime;
                default:
                    Win32.ThrowLastError();
                    return ProcessPriorityClass.Unknown;
            }
        }
        public int GetProcessId()
        {
            return this.GetBasicInformation().UniqueProcessId.ToInt32();
        }
        public int GetSessionId()
        {
            return this.GetInformationInt32(ProcessInformationClass.ProcessSessionInformation);
        }
        private LargeInteger[] GetTimes()
        {
            LargeInteger[] times = new LargeInteger[4];
            if (!Win32.GetProcessTimes(this, out times[0], out times[1], out times[2], out times[3]))
                Win32.ThrowLastError();
            return times;
        }
        public TokenHandle GetToken()
        {
            return this.GetToken(TokenAccess.All);
        }
        public TokenHandle GetToken(TokenAccess access)
        {
            return new TokenHandle(this, access);
        }
        public void InjectDll(string path)
        {
            this.InjectDll(path, 0xffffffff);
        }
        public void InjectDll(string path, uint timeout)
        {
            IntPtr stringPage = this.AllocateMemory(path.Length * 2 + 2, MemoryProtection.ReadWrite);
            this.WriteMemory(stringPage, UnicodeEncoding.Unicode.GetBytes(path));
            if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
            {
                using (var thandle = this.CreateThread(
                    Loader.GetProcedure("kernel32.dll", "LoadLibraryW"),
                    stringPage
                    ))
                    thandle.Wait(timeout * Win32.TimeMsTo100Ns);
            }
            else
            {
                using (var thandle = this.CreateThreadWin32(
                    Loader.GetProcedure("kernel32.dll", "LoadLibraryW"),
                    stringPage
                    ))
                    thandle.Wait(timeout * Win32.TimeMsTo100Ns);
            }
            this.FreeMemory(stringPage, path.Length * 2 + 2, false);
        }
        public bool IsBeingDebugged()
        {
            return this.GetInformationIntPtr(ProcessInformationClass.ProcessDebugPort) != IntPtr.Zero;
        }
        public bool IsCritical()
        {
            return this.GetInformationInt32(ProcessInformationClass.ProcessBreakOnTermination) != 0;
        }
        public bool IsInJob()
        {
            bool result;
            if (!Win32.IsProcessInJob(this, IntPtr.Zero, out result))
                Win32.ThrowLastError();
            return result;
        }
        public bool IsInJob(JobObjectHandle jobObjectHandle)
        {
            bool result;
            if (!Win32.IsProcessInJob(this, jobObjectHandle, out result))
                Win32.ThrowLastError();
            return result;
        }
        public bool IsNtVdmProcess()
        {
            return this.GetInformationInt32(ProcessInformationClass.ProcessWx86Information) != 0;
        }
        public unsafe bool IsPosix()
        {
            int subsystem;
            IntPtr pebBaseAddress = this.GetBasicInformation().PebBaseAddress;
            this.ReadMemory(pebBaseAddress.Increment(Peb.ImageSubsystemOffset), &subsystem, sizeof(int));
            return subsystem == 7;
        }
        public bool IsPriorityBoostEnabled()
        {
            return this.GetInformationInt32(ProcessInformationClass.ProcessPriorityBoost) == 0;
        }
        public bool IsWow64()
        {
            return this.GetInformationIntPtr(ProcessInformationClass.ProcessWow64Information) != IntPtr.Zero;
        }
        public MemoryProtection ProtectMemory(IntPtr baseAddress, int size, MemoryProtection protection)
        {
            NtStatus status;
            IntPtr sizeIntPtr = size.ToIntPtr();
            MemoryProtection oldProtection;
            if ((status = Win32.NtProtectVirtualMemory(
                this,
                ref baseAddress,
                ref sizeIntPtr,
                protection,
                out oldProtection
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return oldProtection;
        }
        public MemoryBasicInformation QueryMemory(IntPtr baseAddress)
        {
            NtStatus status;
            MemoryBasicInformation mbi;
            IntPtr retLength;
            if ((status = Win32.NtQueryVirtualMemory(
                this,
                baseAddress,
                MemoryInformationClass.MemoryBasicInformation,
                out mbi,
                Marshal.SizeOf(typeof(MemoryBasicInformation)).ToIntPtr(),
                out retLength
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return mbi;
        }
        public byte[] ReadMemory(IntPtr baseAddress, int length)
        {
            byte[] buffer = new byte[length];
            this.ReadMemory(baseAddress, buffer, length);
            return buffer;
        }
        public unsafe int ReadMemory(IntPtr baseAddress, byte[] buffer, int length)
        {
            fixed (byte* bufferPtr = buffer)
                return this.ReadMemory(baseAddress, bufferPtr, length);
        }
        public unsafe int ReadMemory(IntPtr baseAddress, void* buffer, int length)
        {
            return this.ReadMemory(baseAddress, new IntPtr(buffer), length);
        }
        public int ReadMemory(IntPtr baseAddress, IntPtr buffer, int length)
        {
            int retLength;
            if (this.Handle == Current)
            {
                Win32.RtlMoveMemory(buffer, baseAddress, length.ToIntPtr());
                return length;
            }
            if (KProcessHacker.Instance != null)
            {
                KProcessHacker.Instance.KphReadVirtualMemory(this, baseAddress.ToInt32(), buffer, length, out retLength);
            }
            else
            {
                NtStatus status;
                IntPtr retLengthIntPtr;
                if ((status = Win32.NtReadVirtualMemory(
                    this,
                    baseAddress,
                    buffer,
                    length.ToIntPtr(),
                    out retLengthIntPtr
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                retLength = retLengthIntPtr.ToInt32();
            }
            return retLength;
        }
        public ThreadHandle RemoteCall(IntPtr address, IntPtr[] arguments)
        {
            IntPtr rtlExitUserThread = Loader.GetProcedure("ntdll.dll", "RtlExitUserThread");
            var thandle = this.CreateThread(rtlExitUserThread, IntPtr.Zero, true);
            thandle.RemoteCall(this, address, arguments, true);
            thandle.Resume();
            return thandle;
        }
        public void RemoveDebug(DebugObjectHandle debugObjectHandle)
        {
            NtStatus status;
            if ((status = Win32.NtRemoveProcessDebug(this, debugObjectHandle)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public void Resume()
        {
            if (KProcessHacker.Instance != null && OSVersion.HasPsSuspendResumeProcess)
            {
                KProcessHacker.Instance.KphResumeProcess(this);
            }
            else
            {
                NtStatus status;
                if ((status = Win32.NtResumeProcess(this)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
        }
        public void SetAffinityMask(long processMask)
        {
            if (!Win32.SetProcessAffinityMask(this, new IntPtr(processMask)))
                Win32.ThrowLastError();
        }
        public void SetBasePriority(int basePriority)
        {
            this.SetInformationInt32(ProcessInformationClass.ProcessBasePriority, basePriority);
        }
        public void SetCritical(bool critical)
        {
            this.SetInformationInt32(ProcessInformationClass.ProcessBreakOnTermination, critical ? 1 : 0);
        }
        public void SetDepStatus(DepStatus depStatus)
        {
            MemExecuteOptions executeOptions = 0;
            if ((depStatus & DepStatus.Enabled) == DepStatus.Enabled)
                executeOptions |= MemExecuteOptions.ExecuteDisable;
            else
                executeOptions |= MemExecuteOptions.ExecuteEnable;
            if ((depStatus & DepStatus.AtlThunkEmulationDisabled) == DepStatus.AtlThunkEmulationDisabled)
                executeOptions |= MemExecuteOptions.DisableThunkEmulation;
            if ((depStatus & DepStatus.Permanent) == DepStatus.Permanent)
                executeOptions |= MemExecuteOptions.Permanent;
            KProcessHacker.Instance.SetExecuteOptions(this, executeOptions);
        }
        private void SetInformationInt32(ProcessInformationClass infoClass, int value)
        {
            NtStatus status;
            if ((status = Win32.NtSetInformationProcess(
                this, infoClass, ref value, sizeof(int))) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public unsafe void SetModuleReferenceCount(IntPtr baseAddress, ushort count)
        {
            byte* buffer = stackalloc byte[IntPtr.Size];
            this.ReadMemory(
                this.GetBasicInformation().PebBaseAddress.Increment(Peb.LdrOffset),
                buffer,
                IntPtr.Size
                );
            IntPtr loaderData = *(IntPtr*)buffer;
            PebLdrData* data = stackalloc PebLdrData[1];
            this.ReadMemory(loaderData, data, Marshal.SizeOf(typeof(PebLdrData)));
            if (!data->Initialized)
                throw new Exception("Loader data is not initialized.");
            List<ProcessModule> modules = new List<ProcessModule>();
            IntPtr currentLink = data->InLoadOrderModuleList.Flink;
            IntPtr startLink = currentLink;
            LdrDataTableEntry* currentEntry = stackalloc LdrDataTableEntry[1];
            int i = 0;
            while (currentLink != IntPtr.Zero)
            {
                if (modules.Count > 0 && currentLink == startLink)
                    break;
                if (i > 0x800)
                    break;
                this.ReadMemory(currentLink, currentEntry, Marshal.SizeOf(typeof(LdrDataTableEntry)));
                if (currentEntry->DllBase == baseAddress)
                {
                    this.WriteMemory(currentLink.Increment(LdrDataTableEntry.LoadCountOffset), &count, 2);
                    break;
                }
                currentLink = currentEntry->InLoadOrderLinks.Flink;
                i++;
            }
        }
        public void SetPriorityBoost(bool enabled)
        {
            this.SetInformationInt32(ProcessInformationClass.ProcessPriorityBoost, enabled ? 0 : 1);
        }
        public void SetPriorityClass(ProcessPriorityClass priorityClass)
        {
            ProcessPriorityClassWin32 pcWin32;
            switch (priorityClass)
            {
                case ProcessPriorityClass.AboveNormal:
                    pcWin32 = ProcessPriorityClassWin32.AboveNormal;
                    break;
                case ProcessPriorityClass.BelowNormal:
                    pcWin32 = ProcessPriorityClassWin32.BelowNormal;
                    break;
                case ProcessPriorityClass.High:
                    pcWin32 = ProcessPriorityClassWin32.High;
                    break;
                case ProcessPriorityClass.Idle:
                    pcWin32 = ProcessPriorityClassWin32.Idle;
                    break;
                case ProcessPriorityClass.Normal:
                    pcWin32 = ProcessPriorityClassWin32.Normal;
                    break;
                case ProcessPriorityClass.RealTime:
                    pcWin32 = ProcessPriorityClassWin32.RealTime;
                    break;
                default:
                    throw new ArgumentException("priorityClass");
            }
            if (!Win32.SetPriorityClass(this, pcWin32))
                Win32.ThrowLastError();
        }
        public void Suspend()
        {
            if (KProcessHacker.Instance != null && OSVersion.HasPsSuspendResumeProcess)
            {
                KProcessHacker.Instance.KphSuspendProcess(this);
            }
            else
            {
                NtStatus status;
                if ((status = Win32.NtSuspendProcess(this)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
        }
        public void Terminate()
        {
            this.Terminate(NtStatus.Success);
        }
        public void Terminate(NtStatus exitStatus)
        {
            if (KProcessHacker.Instance != null)
            {
                KProcessHacker.Instance.KphTerminateProcess(this, exitStatus);
            }
            else
            {
                NtStatus status;
                if ((status = Win32.NtTerminateProcess(this, exitStatus)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
        }
        public void WriteDump(string fileName)
        {
            this.WriteDump(fileName,
                MinidumpType.WithFullMemory |
                MinidumpType.WithHandleData |
                MinidumpType.WithUnloadedModules |
                MinidumpType.WithFullMemoryInfo |
                MinidumpType.WithThreadInfo
                );
        }
        public void WriteDump(string fileName, MinidumpType type)
        {
            using (var fhandle = FileHandle.CreateWin32(fileName, FileAccess.GenericWrite))
                this.WriteDump(fhandle, type);
        }
        public void WriteDump(FileHandle fileHandle, MinidumpType type)
        {
            if (!Win32.MiniDumpWriteDump(
                this,
                this.GetProcessId(),
                fileHandle,
                type,
                IntPtr.Zero,
                IntPtr.Zero,
                IntPtr.Zero
                ))
                Win32.ThrowLastError();
        }
        public int WriteMemory(IntPtr baseAddress, byte[] buffer)
        {
            unsafe
            {
                fixed (byte* dataPtr = buffer)
                {
                    return WriteMemory(baseAddress, dataPtr, buffer.Length);
                }
            }
        }
        public unsafe int WriteMemory(IntPtr baseAddress, void* buffer, int length)
        {
            return this.WriteMemory(baseAddress, new IntPtr(buffer), length);
        }
        public int WriteMemory(IntPtr baseAddress, IntPtr buffer, int length)
        {
            int retLength;
            if (this.Handle == Current)
            {
                Win32.RtlMoveMemory(baseAddress, buffer, length.ToIntPtr());
                return length;
            }
            if (KProcessHacker.Instance != null)
            {
                KProcessHacker.Instance.KphWriteVirtualMemory(this, baseAddress.ToInt32(), buffer, length, out retLength);
            }
            else
            {
                NtStatus status;
                IntPtr retLengthIntPtr;
                if ((status = Win32.NtWriteVirtualMemory(
                    this,
                    baseAddress,
                    buffer,
                    length.ToIntPtr(),
                    out retLengthIntPtr
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                retLength = retLengthIntPtr.ToInt32();
            }
            return retLength;
        }
    }
    public class ProcessHandleTrace
    {
        private ClientId _clientId;
        private IntPtr _handle;
        private IntPtr[] _stack;
        private HandleTraceType _type;
        internal ProcessHandleTrace(ProcessHandleTracingEntry entry)
        {
            _clientId = entry.ClientId;
            _handle = entry.Handle;
            _type = entry.Type;
            int zeroIndex = Array.IndexOf<IntPtr>(entry.Stacks, IntPtr.Zero);
            if (zeroIndex == -1)
                zeroIndex = entry.Stacks.Length;
            _stack = new IntPtr[zeroIndex];
            Array.Copy(entry.Stacks, 0, _stack, 0, zeroIndex);
        }
        public ClientId ClientId
        {
            get { return _clientId; }
        }
        public IntPtr Handle
        {
            get { return _handle; }
        }
        public IntPtr[] Stack
        {
            get { return _stack; }
        }
        public HandleTraceType Type
        {
            get { return _type; }
        }
    }
    public class ProcessHandleTraceCollection : ReadOnlyCollection<ProcessHandleTrace>
    {
        private IntPtr _handle;
        internal ProcessHandleTraceCollection(MemoryAlloc data)
            : base(new List<ProcessHandleTrace>())
        {
            if (data.Size < Marshal.SizeOf(typeof(ProcessHandleTracingQuery)))
                throw new ArgumentException("Data memory allocation is too small.");
            var query = data.ReadStruct<ProcessHandleTracingQuery>();
            _handle = query.Handle;
            IList<ProcessHandleTrace> traces = this.Items;
            for (int i = 0; i < query.TotalTraces; i++)
            {
                var entry = data.ReadStruct<ProcessHandleTracingEntry>(
                    ProcessHandleTracingQuery.HandleTraceOffset,
                    i
                    );
                traces.Add(new ProcessHandleTrace(entry));
            }
        }
        public IntPtr Handle
        {
            get { return _handle; }
        }
    }
    public class ProcessModule : ILoadedModule
    {
        public ProcessModule(
            IntPtr baseAddress,
            int size,
            IntPtr entryPoint,
            LdrpDataTableEntryFlags flags,
            string baseName,
            string fileName
            )
        {
            this.BaseAddress = baseAddress;
            this.Size = size;
            this.EntryPoint = entryPoint;
            this.Flags = flags;
            this.BaseName = baseName;
            this.FileName = fileName;
        }
        public IntPtr BaseAddress { get; private set; }
        public int Size { get; private set; }
        public IntPtr EntryPoint { get; private set; }
        public LdrpDataTableEntryFlags Flags { get; private set; }
        public string BaseName { get; private set; }
        public string FileName { get; private set; }
    }
    [Flags]
    public enum DepStatus
    {
        Enabled = 0x1,
        Permanent = 0x2,
        AtlThunkEmulationDisabled = 0x4
    }
    public enum KnownProcess
    {
        None,
        Idle,
        System,
        SessionManager,
        WindowsSubsystem,
        WindowsStartup,
        ServiceControlManager,
        LocalSecurityAuthority,
        LocalSessionManager
    }
    public enum PebOffset
    {
        CurrentDirectoryPath,
        DllPath,
        ImagePathName,
        CommandLine,
        WindowTitle,
        DesktopName,
        ShellInfo,
        RuntimeData
    }
}
