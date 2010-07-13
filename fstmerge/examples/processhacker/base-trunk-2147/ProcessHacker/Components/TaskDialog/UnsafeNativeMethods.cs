namespace ProcessHacker.Components
{
    using System;
    using System.Diagnostics.CodeAnalysis;
    using System.Runtime.InteropServices;
    internal static partial class UnsafeNativeMethods
    {
        internal const uint WM_USER = 0x0400;
        internal delegate int TaskDialogCallback([In] IntPtr hwnd, [In] uint msg, [In] UIntPtr wParam, [In] IntPtr lParam, [In] IntPtr refData);
        [Flags]
        internal enum TASKDIALOG_FLAGS
        {
            TDF_ENABLE_HYPERLINKS = 0x0001,
            TDF_USE_HICON_MAIN = 0x0002,
            TDF_USE_HICON_FOOTER = 0x0004,
            TDF_ALLOW_DIALOG_CANCELLATION = 0x0008,
            TDF_USE_COMMAND_LINKS = 0x0010,
            TDF_USE_COMMAND_LINKS_NO_ICON = 0x0020,
            TDF_EXPAND_FOOTER_AREA = 0x0040,
            TDF_EXPANDED_BY_DEFAULT = 0x0080,
            TDF_VERIFICATION_FLAG_CHECKED = 0x0100,
            TDF_SHOW_PROGRESS_BAR = 0x0200,
            TDF_SHOW_MARQUEE_PROGRESS_BAR = 0x0400,
            TDF_CALLBACK_TIMER = 0x0800,
            TDF_POSITION_RELATIVE_TO_WINDOW = 0x1000,
            TDF_RTL_LAYOUT = 0x2000,
            TDF_NO_DEFAULT_RADIO_BUTTON = 0x4000,
            TDF_CAN_BE_MINIMIZED = 0x8000
        }
        internal enum TASKDIALOG_ELEMENTS
        {
            TDE_CONTENT,
            TDE_EXPANDED_INFORMATION,
            TDE_FOOTER,
            TDE_MAIN_INSTRUCTION
        }
        internal enum TASKDIALOG_ICON_ELEMENTS
        {
            TDIE_ICON_MAIN,
            TDIE_ICON_FOOTER
        }
        internal enum TASKDIALOG_MESSAGES : uint
        {
            TDM_CLICK_BUTTON = WM_USER + 102,
            TDM_SET_MARQUEE_PROGRESS_BAR = WM_USER + 103,
            TDM_SET_PROGRESS_BAR_STATE = WM_USER + 104,
            TDM_SET_PROGRESS_BAR_RANGE = WM_USER + 105,
            TDM_SET_PROGRESS_BAR_POS = WM_USER + 106,
            TDM_SET_PROGRESS_BAR_MARQUEE = WM_USER + 107,
            TDM_SET_ELEMENT_TEXT = WM_USER + 108,
            TDM_CLICK_RADIO_BUTTON = WM_USER + 110,
            TDM_ENABLE_BUTTON = WM_USER + 111,
            TDM_ENABLE_RADIO_BUTTON = WM_USER + 112,
            TDM_CLICK_VERIFICATION = WM_USER + 113,
            TDM_UPDATE_ELEMENT_TEXT = WM_USER + 114,
            TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE = WM_USER + 115,
            TDM_UPDATE_ICON = WM_USER + 116
        }
        [DllImport("comctl32.dll", CharSet = CharSet.Unicode, PreserveSig = false)]
        internal static extern void TaskDialogIndirect(
            [In] ref TASKDIALOGCONFIG pTaskConfig,
            [Out] out int pnButton,
            [Out] out int pnRadioButton,
            [MarshalAs(UnmanagedType.Bool)]
            [Out] out bool pfVerificationFlagChecked);
        [DllImport("user32.dll")]
        internal static extern IntPtr SendMessage(IntPtr hWnd, uint Msg, IntPtr wParam, IntPtr lParam);
        [DllImport("user32.dll", EntryPoint="SendMessage")]
        internal static extern IntPtr SendMessageWithString(IntPtr hWnd, uint Msg, IntPtr wParam, [MarshalAs(UnmanagedType.LPWStr)] string lParam);
        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode, Pack = 1)]
        internal struct TASKDIALOGCONFIG
        {
            public uint cbSize;
            [SuppressMessage("Microsoft.Reliability", "CA2006:UseSafeHandleToEncapsulateNativeResources")]
            public IntPtr hwndParent;
            [SuppressMessage("Microsoft.Reliability", "CA2006:UseSafeHandleToEncapsulateNativeResources")]
            public IntPtr hInstance;
            public TASKDIALOG_FLAGS dwFlags;
            public TaskDialogCommonButtons dwCommonButtons;
            [MarshalAs(UnmanagedType.LPWStr)]
            public string pszWindowTitle;
            [SuppressMessage("Microsoft.Reliability", "CA2006:UseSafeHandleToEncapsulateNativeResources")]
            public IntPtr MainIcon;
            [MarshalAs(UnmanagedType.LPWStr)]
            public string pszMainInstruction;
            [MarshalAs(UnmanagedType.LPWStr)]
            public string pszContent;
            public uint cButtons;
            [SuppressMessage("Microsoft.Reliability", "CA2006:UseSafeHandleToEncapsulateNativeResources")]
            public IntPtr pButtons;
            public int nDefaultButton;
            public uint cRadioButtons;
            [SuppressMessage("Microsoft.Reliability", "CA2006:UseSafeHandleToEncapsulateNativeResources")]
            public IntPtr pRadioButtons;
            public int nDefaultRadioButton;
            [MarshalAs(UnmanagedType.LPWStr)]
            public string pszVerificationText;
            [MarshalAs(UnmanagedType.LPWStr)]
            public string pszExpandedInformation;
            [MarshalAs(UnmanagedType.LPWStr)]
            public string pszExpandedControlText;
            [MarshalAs(UnmanagedType.LPWStr)]
            public string pszCollapsedControlText;
            [SuppressMessage("Microsoft.Reliability", "CA2006:UseSafeHandleToEncapsulateNativeResources")]
            public IntPtr FooterIcon;
            [MarshalAs(UnmanagedType.LPWStr)]
            public string pszFooter;
            public TaskDialogCallback pfCallback;
            [SuppressMessage("Microsoft.Reliability", "CA2006:UseSafeHandleToEncapsulateNativeResources")]
            public IntPtr lpCallbackData;
            public uint cxWidth;
        }
    }
}
