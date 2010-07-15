using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using ProcessHacker.Common;
using Microsoft.Win32.SafeHandles;
using System.Runtime.InteropServices;
using System.Diagnostics;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;

namespace ProcessHacker
{
    public partial class WaitChainWindow : Form
    {
        int processPid;
        string processName;

        TreeNode threadNode;

        public WaitChainWindow(string procName, int procPid)
        {
            this.SetPhParent();
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            processPid = procPid;
            processName = procName;
        }

        private void moreInfoLink_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            Program.TryStart("http://go.microsoft.com/fwlink/?LinkID=136333");
        }

        private void WaitChainWindow_Load(object sender, EventArgs e)
        {
            using (WaitChainTraversal wct = new WaitChainTraversal())
            {
                ShowProcessWaitChains(wct, false);
            }
        }

        private void ShowProcessWaitChains(WaitChainTraversal wct, bool showAllData)
        {
            var threads = Windows.GetProcessThreads(processPid);

            if (threads == null)
            {
                PhUtils.ShowWarning(string.Format("The process ID {0} does not exist", processPid));
                this.Close();
            }

            textDescription.AppendText(string.Format("Process: {0}, PID: {1}", processName, processPid));

            threadNode = threadTree.Nodes.Add(string.Format("Process: {0}, PID: {1}", processName, processPid));

            foreach (var thread in threads)
            {

                int currThreadId = thread.Key;

                WaitData data = wct.GetThreadWaitChain(currThreadId);

                if (data != null)
                {
                    DisplayThreadData(data, showAllData);
                }
                else
                {
                    threadNode.Nodes.Add(string.Format("TID:{0} Unable to retrieve wait chains for this thread without Admin rights", currThreadId));
                    threadNode.ExpandAll();
                }
            }
        }

        private void DisplayThreadData(WaitData data, bool allData)
        {



            int startingPID = data.Nodes[0].ProcessId;
            StringBuilder sb = new StringBuilder();

            if (data.IsDeadlock)
            {
                sb.Append("DEADLOCKED: ");
            }

            for (int i = 0; i < data.NodeCount; i++)
            {
                WaitChainNativeMethods.WAITCHAIN_NODE_INFO node = data.Nodes[i];

                if (WaitChainNativeMethods.WCT_OBJECT_TYPE.Thread == node.ObjectType)
                {
                    var processes = Windows.GetProcesses();
                    String procName = processes.ContainsKey(node.ProcessId) ? processes[node.ProcessId].Name : "???";

                    switch (node.ObjectStatus)
                    {
                        case WaitChainNativeMethods.WCT_OBJECT_STATUS.PidOnly:
                        case WaitChainNativeMethods.WCT_OBJECT_STATUS.PidOnlyRpcss:
                            sb.Append(string.Format(" PID: {0} {1}", node.ProcessId, procName));
                            break;
                        default:
                            {
                                sb.Append(string.Format(" TID: {0}", node.ThreadId));


                                if ((i > 0) && (startingPID != node.ProcessId))
                                {

                                    sb.Append(string.Format(" PID:{0} {1}", node.ProcessId, procName));
                                }

                                if (allData)
                                {
                                    sb.Append(string.Format(" Status: {0} Wait: {1} CS: {2:N0}", node.ObjectStatus, node.WaitTime, node.ContextSwitches));
                                }
                                else if (node.ObjectStatus != WaitChainNativeMethods.WCT_OBJECT_STATUS.Blocked)
                                {
                                    sb.Append(string.Format(" Status: {0}", node.ObjectStatus));
                                }
                                break;
                            }
                    }
                }
                else
                {
                    switch (node.ObjectType)
                    {
                        case WaitChainNativeMethods.WCT_OBJECT_TYPE.CriticalSection:
                        case WaitChainNativeMethods.WCT_OBJECT_TYPE.SendMessage:
                        case WaitChainNativeMethods.WCT_OBJECT_TYPE.Mutex:
                        case WaitChainNativeMethods.WCT_OBJECT_TYPE.Alpc:
                        case WaitChainNativeMethods.WCT_OBJECT_TYPE.COM:
                        case WaitChainNativeMethods.WCT_OBJECT_TYPE.ThreadWait:
                        case WaitChainNativeMethods.WCT_OBJECT_TYPE.ProcessWait:
                        case WaitChainNativeMethods.WCT_OBJECT_TYPE.COMActivation:
                        case WaitChainNativeMethods.WCT_OBJECT_TYPE.Unknown:
                            {
                                sb.Append(string.Format(" {0} Status: {1}", node.ObjectType, node.ObjectStatus));

                                String name = node.ObjectName();

                                if (!String.IsNullOrEmpty(name))
                                {
                                    sb.Append(string.Format(" Name: {0}", name));
                                }
                            }
                            break;
                        default:
                            {
                                sb.Append(string.Format(" UNKNOWN Object Type Enum: {0}", node.ObjectType.ToString()));
                                break;
                            }
                    }
                }
                threadNode.Nodes.Add(sb.ToString());
                threadNode.ExpandAll();
            }
        }

        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void buttonProperties_Click(object sender, EventArgs e)
        {
            try
            {
                ProcessWindow pForm = Program.GetProcessWindow(Program.HackerWindow.processP.Dictionary[processPid],
                    new Program.PWindowInvokeAction(delegate(ProcessWindow f)
                    {
                        Properties.Settings.Default.ProcessWindowSelectedTab = "tabThreads";
                        f.Show();
                        f.Activate();
                    }));
            }
            catch (Exception ex)
            {
                PhUtils.ShowException("Unable to inspect the process", ex);
            }
        }
    }




    public static partial class WaitChainNativeMethods
    {

        private static SafeModuleHandle oleModule;


        public const int WCT_MAX_NODE_COUNT = 16;

        private const int WCT_OBJNAME_LENGTH = 128;

        public static SafeWaitChainHandle OpenThreadWaitChainSession()
        {

            if (oleModule == null)
            {
                oleModule = LoadLibraryW("OLE32.DLL");
                IntPtr coGetCallState = GetProcAddress(oleModule, "CoGetCallState");
                IntPtr coGetActivationState = GetProcAddress(oleModule, "CoGetActivationState");

                RegisterWaitChainCOMCallback(coGetCallState, coGetActivationState);
            }

            SafeWaitChainHandle wctHandle = RealOpenThreadWaitChainSession(0, IntPtr.Zero);
            if (wctHandle.IsInvalid == true)
            {
                throw new InvalidOperationException("Unable to open the Wait Thread Chain.");
            }
            return (wctHandle);
        }

        public static bool GetThreadWaitChain(SafeWaitChainHandle chainHandle, int threadId, ref int NodeCount, WAITCHAIN_NODE_INFO[] NodeInfoArray, out int IsCycle)
        {
            return RealGetThreadWaitChain(chainHandle, IntPtr.Zero, WCT_FLAGS.All, threadId, ref NodeCount, NodeInfoArray, out IsCycle);
        }

        public static void CloseThreadWaitChainSession(IntPtr handle)
        {
            RealCloseThreadWaitChainSession(handle);
        }
        [StructLayout(LayoutKind.Explicit, Size = 280)]
        public unsafe struct WAITCHAIN_NODE_INFO
        {
            [FieldOffset(0x0)]
            public WCT_OBJECT_TYPE ObjectType;
            [FieldOffset(0x4)]
            public WCT_OBJECT_STATUS ObjectStatus;
            [FieldOffset(0x8)]
            private fixed ushort RealObjectName[WCT_OBJNAME_LENGTH];
            [FieldOffset(0x108)]
            public int TimeOutLowPart;
            [FieldOffset(0x10C)]
            public int TimeOutHiPart;
            [FieldOffset(0x110)]
            public int Alertable;
            [FieldOffset(0x8)]
            public int ProcessId;
            [FieldOffset(0xC)]
            public int ThreadId;
            [FieldOffset(0x10)]
            public int WaitTime;
            [FieldOffset(0x14)]
            public int ContextSwitches;
            public String ObjectName()
            {
                fixed (WAITCHAIN_NODE_INFO* p = &this)
                {
                    string str = (p->RealObjectName[0] != '\0') ? new string((char*)p->RealObjectName) : string.Empty;
                    return str;
                }
            }
        }
        [Flags]
        public enum WCT_OBJECT_TYPE
        {
            CriticalSection = 1,
            SendMessage,
            Mutex,
            Alpc,
            COM,
            ThreadWait,
            ProcessWait,
            Thread,
            COMActivation,
            Unknown,
        } ;
        [Flags]
        public enum WCT_OBJECT_STATUS
        {
            NoAccess = 1,
            Running,
            Blocked,
            PidOnly,
            PidOnlyRpcss,
            Owned,
            NotOwned,
            Abandoned,
            Unknown,
            Error,
        } ;
        [Flags]
        public enum WCT_FLAGS
        {
            Flag = 0x1,
            COM = 0x2,
            Proc = 0x4,
            All = Flag | COM | Proc
        }
        [DllImport("advapi32.dll", SetLastError = true, ExactSpelling = true, CharSet = CharSet.Unicode, EntryPoint = "CloseThreadWaitChainSession")]
        private static extern void RealCloseThreadWaitChainSession(IntPtr wctHandle);
        [DllImport("advapi32.dll", EntryPoint = "OpenThreadWaitChainSession")]
        private static extern SafeWaitChainHandle RealOpenThreadWaitChainSession(int flags, IntPtr callback);
        [DllImport("advapi32.dll", ExactSpelling = true, SetLastError = true)]
        private static extern void RegisterWaitChainCOMCallback(IntPtr callStateCallback, IntPtr activationStateCallback);
        [DllImport("advapi32.dll", EntryPoint = "GetThreadWaitChain")]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool RealGetThreadWaitChain(SafeWaitChainHandle WctHandle, IntPtr Context, WCT_FLAGS Flags, int ThreadId, ref int NodeCount, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)] [In, Out] WAITCHAIN_NODE_INFO[] NodeInfoArray, out int IsCycle);
        [DllImport("kernel32.dll", SetLastError = true, ExactSpelling = true, CharSet = CharSet.Unicode)]
        internal static extern SafeModuleHandle LoadLibraryW(String lpFileName);
        [DllImport("kernel32.dll", SetLastError = true, ExactSpelling = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool FreeLibrary(IntPtr hModule);
        [DllImport("kernel32.dll", SetLastError = true, ExactSpelling = true, CharSet = CharSet.Ansi)]
        internal static extern IntPtr GetProcAddress(SafeModuleHandle hModule, string lpProcName);
    }
    public class SafeModuleHandle : SafeHandleZeroOrMinusOneIsInvalid
    {
        public SafeModuleHandle()
            : base(true)
        {
        }
        protected override Boolean ReleaseHandle()
        {
            return (WaitChainNativeMethods.FreeLibrary(this.handle));
        }
    }
    public class SafeWaitChainHandle : SafeHandleZeroOrMinusOneIsInvalid
    {
        private SafeWaitChainHandle()
            : base(true)
        {
        }
        protected override bool ReleaseHandle()
        {
            WaitChainNativeMethods.CloseThreadWaitChainSession(this.handle);
            return (true);
        }
    }
    public sealed class WaitData
    {
        private WaitChainNativeMethods.WAITCHAIN_NODE_INFO[] data;
        private bool isDeadlock;
        private int nodeCount;
        public WaitData(WaitChainNativeMethods.WAITCHAIN_NODE_INFO[] data, int nodeCount, bool isDeadlock)
        {
            this.data = data;
            this.nodeCount = nodeCount;
            this.isDeadlock = isDeadlock;
        }
        public WaitChainNativeMethods.WAITCHAIN_NODE_INFO[] Nodes
        {
            get
            {
                return (data);
            }
        }
        public int NodeCount
        {
            get
            {
                return (nodeCount);
            }
        }
        public bool IsDeadlock
        {
            get
            {
                return (isDeadlock);
            }
        }
    }
    public sealed class WaitChainTraversal : IDisposable
    {
        private SafeWaitChainHandle waitChainHandle;
        public WaitChainTraversal()
        {
            waitChainHandle = WaitChainNativeMethods.OpenThreadWaitChainSession();
        }
        public WaitData GetThreadWaitChain(int threadId)
        {
            WaitChainNativeMethods.WAITCHAIN_NODE_INFO[] data = new WaitChainNativeMethods.WAITCHAIN_NODE_INFO[WaitChainNativeMethods.WCT_MAX_NODE_COUNT];
            int isDeadlock = 0;
            int nodeCount = WaitChainNativeMethods.WCT_MAX_NODE_COUNT;
            WaitData retData = null;
            if (WaitChainNativeMethods.GetThreadWaitChain(waitChainHandle, threadId, ref nodeCount, data, out isDeadlock))
            {
                retData = new WaitData(data, (int)nodeCount, isDeadlock == 1);
            }
            return (retData);
        }
        public void Dispose()
        {
            waitChainHandle.Dispose();
        }
    }
}
