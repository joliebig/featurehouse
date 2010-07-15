


using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;

namespace ProcessHacker
{
    public class ExtendedListView : ListView
    {
        private const int LVM_First = 0x1000;
        private const int LVM_SetGroupInfo = (LVM_First + 147);
        private const int LVM_SetExtendedListViewStyle = (LVM_First + 54);
        private const int LVS_Ex_DoubleBuffer = 0x00010000;

        private const int LVN_First = -100;
        private const int LVN_LINKCLICK = (LVN_First - 84);

        private delegate void CallBackSetGroupState(ListViewGroup lvGroup, ListViewGroupState lvState, string task);
        private delegate void CallbackSetGroupString(ListViewGroup lvGroup, string value);

        [DllImport("user32.dll", CharSet = CharSet.Auto)]
        private static extern IntPtr SendMessage(IntPtr hWnd, int Msg, int wParam, ref LVGroup lParam);

        public ExtendedListView()
        {



            this.SetStyle(ControlStyles.OptimizedDoubleBuffer | ControlStyles.AllPaintingInWmPaint | ControlStyles.EnableNotifyMessage, true);
        }

        public void SetGroupState(ListViewGroupState state)
        {
            this.SetGroupState(state, null);
        }

        public void SetGroupState(ListViewGroupState state, string taskLabel)
        {
            foreach (ListViewGroup lvg in this.Groups)
            {
                SetGrpState(lvg, state, taskLabel);
            }
        }

        private static int? GetGroupID(ListViewGroup lvGroup)
        {
            int? grpId = null;
            Type grpType = lvGroup.GetType();
            if (grpType != null)
            {
                PropertyInfo pInfo = grpType.GetProperty("ID", BindingFlags.NonPublic | BindingFlags.Instance);
                if (pInfo != null)
                {
                    object tmprtnval = pInfo.GetValue(lvGroup, null);
                    if (tmprtnval != null)
                    {
                        grpId = tmprtnval as int?;
                    }
                }
            }
            return grpId;
        }

        private void SetGrpState(ListViewGroup lvGroup, ListViewGroupState grpState, string task)
        {
            if (OSVersion.IsBelow(WindowsVersion.Vista))
                return;
            if (lvGroup == null || lvGroup.ListView == null)
                return;
            if (lvGroup.ListView.InvokeRequired)
                lvGroup.ListView.Invoke(new CallBackSetGroupState(SetGrpState), lvGroup, grpState, task);
            else
            {
                int? GrpId = GetGroupID(lvGroup);
                int gIndex = lvGroup.ListView.Groups.IndexOf(lvGroup);
                LVGroup group = new LVGroup();
                group.CbSize = Marshal.SizeOf(group);
                group.Mask |= ListViewGroupMask.Task
                    | ListViewGroupMask.State
                    | ListViewGroupMask.Align;

                IntPtr taskString = Marshal.StringToHGlobalAuto(task);

                if (task.Length > 1)
                {
                    group.Task = taskString;
                    group.CchTask = task.Length;
                }

                group.GroupState = grpState;

                if (GrpId != null)
                {
                    group.GroupId = GrpId.Value;
                    SendMessage(base.Handle, LVM_SetGroupInfo, GrpId.Value, ref group);
                }
                else
                {
                    group.GroupId = gIndex;
                    SendMessage(base.Handle, LVM_SetGroupInfo, gIndex, ref group);
                }
                lvGroup.ListView.Refresh();

                Marshal.FreeHGlobal(taskString);
            }
        }

        protected override void OnNotifyMessage(Message m)
        {






            if (m.Msg != 0x14)
            {
                base.OnNotifyMessage(m);
            }
        }

        private const int WM_NOTIFY = 0x004E;

        protected override void WndProc(ref Message m)
        {
            switch (m.Msg)
            {
                case 0x1:
                    {
                        SubclassHWnd(base.Handle);

                        HResult setThemeResult = Win32.SetWindowTheme(base.Handle, "explorer", null);
                        setThemeResult.ThrowIf();

                        unchecked
                        {
                            Win32.SendMessage(base.Handle, (WindowMessage)LVM_SetExtendedListViewStyle, LVS_Ex_DoubleBuffer, LVS_Ex_DoubleBuffer);
                        }

                        break;
                    }
                case 0x202:
                case 0x205:
                case 520:
                case 0x203:
                case 0x2a1:
                    {
                        base.DefWndProc(ref m);
                        return;
                    }
            }

            base.WndProc(ref m);
        }


        [DllImport("user32")]
        private static extern IntPtr SetWindowLong(IntPtr hWnd, int nIndex, Win32WndProc newProc);
        [DllImport("user32")]
        private static extern int CallWindowProc(IntPtr lpPrevWndFunc, IntPtr hWnd, int Msg, int wParam, int lParam);


        private delegate int Win32WndProc(IntPtr hWnd, int Msg, int wParam, int lParam);


        private const int GWL_WNDPROC = -4;
        private const int WM_LBUTTONDOWN = 0x0201;


        private IntPtr oldWndProc = IntPtr.Zero;
        private Win32WndProc newWndProc = null;

        void SubclassHWnd(IntPtr hWnd)
        {

            newWndProc = new Win32WndProc(MyWndProc);

            oldWndProc = SetWindowLong(hWnd, GWL_WNDPROC, newWndProc);
        }


        private int MyWndProc(IntPtr hWnd, int Msg, int wParam, int lParam)
        {
            System.Diagnostics.Debug.WriteLine(Msg.ToString());


            switch (Msg)
            {
                case 0x4e:
                    {
                        unsafe
                        {
                            NMHDR* hdr = (NMHDR*)lParam;

                            if (hdr->code == LVN_LINKCLICK)
                            {
                                MessageBox.Show("Link clicked!");
                                return 0;
                            }
                        }
                        break;
                    }
                default:
                    break;
            }

            return CallWindowProc(oldWndProc, hWnd, Msg, wParam, lParam);
        }



        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
        private struct LVGroup
        {



            public int CbSize;




            public ListViewGroupMask Mask;





            public IntPtr pszHeader;




            public int CchHeader;





            public IntPtr pszFooter;




            public int CchFooter;




            public int GroupId;




            public uint stateMask;




            public ListViewGroupState GroupState;




            public uint uAlign;





            public IntPtr PszSubtitle;




            public uint CchSubtitle;





            public IntPtr Task;




            public int CchTask;





            public IntPtr DescriptionTop;




            public uint CchDescriptionTop;





            public IntPtr DescriptionBottom;




            public uint CchDescriptionBottom;




            public int ITitleImage;




            public int IExtendedImage;




            public int IFirstItem;




            public uint CItems;





            public IntPtr PszSubsetTitle;




            public uint CchSubsetTitle;
        }






        [StructLayout(LayoutKind.Sequential)]
        private struct NMHDR
        {



            public IntPtr hwndFrom;



            public IntPtr idFrom;



            public int code;
        }

    }

    [Flags]
    public enum ListViewGroupMask : uint
    {
        None = 0x00000,
        Header = 0x00001,
        Footer = 0x00002,
        State = 0x00004,
        Align = 0x00008,
        GroupId = 0x00010,
        SubTitle = 0x00100,
        Task = 0x00200,
        DescriptionTop = 0x00400,
        DescriptionBottom = 0x00800,
        TitleImage = 0x01000,
        ExtendedImage = 0x02000,
        Items = 0x04000,
        Subset = 0x08000,
        SubsetItems = 0x10000
    }

    [Flags]
    public enum ListViewGroupState : uint
    {



        Normal = 0,



        Collapsed = 1,



        Hidden = 2,



        NoHeader = 4,



        Collapsible = 8,



        Focused = 16,



        Selected = 32,



        SubSeted = 64,



        SubSetLinkFocused = 128,
    }

}
