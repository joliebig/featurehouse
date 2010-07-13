

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Security;
using System.Text;
using ProcessHacker.Common.Threading;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Api
{
    public delegate bool EnumWindowsProc(IntPtr hWnd, uint param);
    public delegate bool EnumChildProc(IntPtr hWnd, uint param);
    public delegate bool EnumThreadWndProc(IntPtr hWnd, uint param);
    public delegate IntPtr WndProcDelegate(IntPtr hWnd, WindowMessage msg, IntPtr wParam, IntPtr lParam);

    public delegate bool SymEnumSymbolsProc(IntPtr SymInfo, int SymbolSize, int UserContext);
    public unsafe delegate bool ReadProcessMemoryProc64(IntPtr ProcessHandle, ulong BaseAddress, IntPtr Buffer,
        int Size, out int BytesRead);
    public delegate IntPtr FunctionTableAccessProc64(IntPtr ProcessHandle, ulong AddrBase);
    public delegate ulong GetModuleBaseProc64(IntPtr ProcessHandle, ulong Address);




    [SuppressUnmanagedCodeSecurity]
    public static partial class Win32
    {
        private static FastMutex _dbgHelpLock = new FastMutex();




        public static FastMutex DbgHelpLock
        {
            get { return _dbgHelpLock; }
        }



        public const int DontResolveDllReferences = 0x1;
        public const int ErrorNoMoreItems = 259;
        public const int SeeMaskInvokeIdList = 0xc;
        public const uint ServiceNoChange = 0xffffffff;
        public const uint ShgFiIcon = 0x100;
        public const uint ShgFiLargeIcon = 0x0;
        public const uint ShgFiSmallIcon = 0x1;
        public static readonly int SymbolInfoNameOffset = Marshal.OffsetOf(typeof(SymbolInfo), "Name").ToInt32();





        public static Win32Error GetLastErrorCode()
        {
            return (Win32Error)Marshal.GetLastWin32Error();
        }





        public static string GetLastErrorMessage()
        {
            return GetLastErrorCode().GetMessage();
        }




        public static void ThrowLastError()
        {
            ThrowLastError(GetLastErrorCode());
        }

        public static void ThrowLastError(NtStatus status)
        {
            throw new WindowsException(status);
        }

        public static void ThrowLastError(int error)
        {
            ThrowLastError((Win32Error)error);
        }

        public static void ThrowLastError(Win32Error error)
        {
            throw new WindowsException(error);
        }





        public unsafe static void DuplicateObject(
            IntPtr sourceProcessHandle,
            IntPtr sourceHandle,
            int desiredAccess,
            HandleFlags handleAttributes,
            DuplicateOptions options
            )
        {
            IntPtr dummy;

            DuplicateObject(
                sourceProcessHandle,
                sourceHandle,
                IntPtr.Zero,
                out dummy,
                desiredAccess,
                handleAttributes,
                options
                );
        }

        public unsafe static void DuplicateObject(
            IntPtr sourceProcessHandle,
            IntPtr sourceHandle,
            IntPtr targetProcessHandle,
            out IntPtr targetHandle,
            int desiredAccess,
            HandleFlags handleAttributes,
            DuplicateOptions options
            )
        {
            if (KProcessHacker.Instance != null)
            {
                int target;

                KProcessHacker.Instance.KphDuplicateObject(
                    sourceProcessHandle.ToInt32(),
                    sourceHandle.ToInt32(),
                    targetProcessHandle.ToInt32(),
                    out target,
                    desiredAccess,
                    handleAttributes,
                    options);
                targetHandle = new IntPtr(target);
            }
            else
            {
                NtStatus status;

                if ((status = NtDuplicateObject(
                    sourceProcessHandle,
                    sourceHandle,
                    targetProcessHandle,
                    out targetHandle,
                    desiredAccess,
                    handleAttributes,
                    options)) >= NtStatus.Error)
                    ThrowLastError(status);
            }
        }





        public static int GetProcessSessionId(int ProcessId)
        {
            int sessionId;

            try
            {
                if (!ProcessIdToSessionId(ProcessId, out sessionId))
                    ThrowLastError();
            }
            catch
            {
                using (ProcessHandle phandle = new ProcessHandle(ProcessId, OSVersion.MinProcessQueryInfoAccess))
                {
                    return phandle.GetToken(TokenAccess.Query).GetSessionId();
                }
            }

            return sessionId;
        }





        public static MibTcpStats GetTcpStats()
        {
            MibTcpStats tcpStats;
            GetTcpStatistics(out tcpStats);
            return tcpStats;
        }

        public static MibTcpTableOwnerPid GetTcpTable()
        {
            MibTcpTableOwnerPid table = new MibTcpTableOwnerPid();
            int length = 0;

            GetExtendedTcpTable(IntPtr.Zero, ref length, false, AiFamily.INet, TcpTableClass.OwnerPidAll, 0);

            using (MemoryAlloc mem = new MemoryAlloc(length))
            {
                GetExtendedTcpTable(mem, ref length, false, AiFamily.INet, TcpTableClass.OwnerPidAll, 0);

                int count = mem.ReadInt32(0);

                table.NumEntries = count;
                table.Table = new MibTcpRowOwnerPid[count];

                for (int i = 0; i < count; i++)
                    table.Table[i] = mem.ReadStruct<MibTcpRowOwnerPid>(sizeof(int), i);
            }

            return table;
        }





        public struct WtsEnumProcessesFastData
        {
            public int[] PIDs;
            public IntPtr[] SIDs;
            public WtsMemoryAlloc Memory;
        }

        public unsafe static WtsEnumProcessesFastData TSEnumProcessesFast()
        {
            IntPtr processes;
            int count;
            int[] pids;
            IntPtr[] sids;

            WTSEnumerateProcesses(IntPtr.Zero, 0, 1, out processes, out count);

            pids = new int[count];
            sids = new IntPtr[count];

            WtsMemoryAlloc data = new WtsMemoryAlloc(processes);
            WtsProcessInfo* dataP = (WtsProcessInfo*)data.Memory;

            for (int i = 0; i < count; i++)
            {
                pids[i] = dataP[i].ProcessId;
                sids[i] = dataP[i].Sid;
            }

            return new WtsEnumProcessesFastData() { PIDs = pids, SIDs = sids, Memory = data };
        }





        public static MibUdpStats GetUdpStats()
        {
            MibUdpStats udpStats;
            GetUdpStatistics(out udpStats);
            return udpStats;
        }

        public static MibUdpTableOwnerPid GetUdpTable()
        {
            MibUdpTableOwnerPid table = new MibUdpTableOwnerPid();
            int length = 0;

            GetExtendedUdpTable(IntPtr.Zero, ref length, false, AiFamily.INet, UdpTableClass.OwnerPid, 0);

            using (MemoryAlloc mem = new MemoryAlloc(length))
            {
                GetExtendedUdpTable(mem, ref length, false, AiFamily.INet, UdpTableClass.OwnerPid, 0);

                int count = mem.ReadInt32(0);

                table.NumEntries = count;
                table.Table = new MibUdpRowOwnerPid[count];

                for (int i = 0; i < count; i++)
                    table.Table[i] = mem.ReadStruct<MibUdpRowOwnerPid>(sizeof(int), i);
            }

            return table;
        }
        public unsafe static string[] GetMultiString(IntPtr ptr)
        {
            List<string> list = new List<string>();
            char* chptr = (char*)ptr;
            StringBuilder currentString = new StringBuilder();
            while (true)
            {
                while (*chptr != 0)
                {
                    currentString.Append(*chptr);
                    chptr++;
                }
                string str = currentString.ToString();
                if (str == "")
                {
                    break;
                }
                else
                {
                    list.Add(str);
                    currentString = new StringBuilder();
                }
            }
            return list.ToArray();
        }
    }
}
