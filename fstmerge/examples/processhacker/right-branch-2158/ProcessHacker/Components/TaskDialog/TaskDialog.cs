namespace ProcessHacker.Components
{
    using System;
    using System.Diagnostics.CodeAnalysis;
    using System.Drawing;
    using System.Runtime.InteropServices;
    using System.Windows.Forms;
    public delegate bool TaskDialogCallback(ActiveTaskDialog taskDialog, TaskDialogNotificationArgs args, object callbackData);
    [Flags]
    public enum TaskDialogCommonButtons
    {
        None = 0,
        Ok = 0x0001,
        Yes = 0x0002,
        No = 0x0004,
        Cancel = 0x0008,
        Retry = 0x0010,
        Close = 0x0020,
    }
    [SuppressMessage("Microsoft.Design", "CA1028:EnumStorageShouldBeInt32")]
    public enum TaskDialogIcon : uint
    {
        None = 0,
        Warning = 0xFFFF,
        Error = 0xFFFE,
        Information = 0xFFFD,
        Shield = 0xFFFC,
        SecurityStop = UInt16.MaxValue - 1,
        SecurityInformation = UInt16.MaxValue - 2,
        SecurityShield = UInt16.MaxValue - 3,
        SecurityShieldBlue = UInt16.MaxValue - 4,
        SecurityWarning = UInt16.MaxValue - 5,
        SecurityError = UInt16.MaxValue - 6,
        SecuritySuccess = UInt16.MaxValue - 7,
        SecurityShieldGray = UInt16.MaxValue - 8,
        ASecurityWarning = UInt16.MaxValue,
        DefragWithShield = 195,
        VideoIconWithoutVideo = 193,
        WorldIconWithCable = 179,
        MagnifyingGlass = 177,
        FolderwithTwoArrowsPointingInwards = 175,
        AppInstallUninstallIcon = 161,
        AppearanceIcon = 151,
        PerformanceMonitorIcon = 150,
        MyComputerIconWithTick = 149,
        ComputerJumpToComputerIcon = 147,
        MonitorIconWithMagnifingGlass = 145,
        InternetWorldWithClockIcon = 144,
        BriefcaseWithUsericon = 130,
        AppPageWithTicks = 121,
        NetworkingIcon = 120,
        CogWithTicks = 114,
        DegragIcon = 111,
        ShowDesktopIcon = 110,
        ExclamationMarkShield = 107,
        GreenTickShield = 106,
        RedXShield = 105,
        QuestionIcon = 104,
        MonitorIcon = 101,
        RunBoxIcon = 100,
        CirleQuestion = 99,
        CircleX = 98,
        GreySmallX = 97,
        FlashChip = 96,
        TXTandBRIcon = 94,
        BigRedX = 89,
        AppInstallIcon = 87,
        ExIcon = 84,
        KeyIcon = 82,
        InfoIcon = 81,
        Startmenu = 80,
        SharedIcon = 79,
        WindowsDrive = 37,
        CircleandTick = 24,
        Network = 25,
        RecycleBin = 55,
        Padlock = 59,
        DisplayLookingIcon = 65,
        Picture = 70,
        HDDQuestion = 75,
    }
    public enum TaskDialogNotification
    {
        Created = 0,
        ButtonClicked = 2,
        HyperlinkClicked = 3,
        Timer = 4,
        Destroyed = 5,
        RadioButtonClicked = 6,
        DialogConstructed = 7,
        VerificationClicked = 8,
        Help = 9,
        ExpandoButtonClicked = 10
    }
    [SuppressMessage("Microsoft.Design", "CA1008:EnumsShouldHaveZeroValue")]
    public enum ProgressBarState
    {
        Normal = 1,
        Error = 2,
        Paused = 3
    }
    [SuppressMessage("Microsoft.Performance", "CA1815:OverrideEqualsAndOperatorEqualsOnValueTypes")]
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode, Pack = 1)]
    public struct TaskDialogButton
    {
        private int buttonId;
        [MarshalAs(UnmanagedType.LPWStr)]
        private string buttonText;
        public TaskDialogButton(int id, string text)
        {
            this.buttonId = id;
            this.buttonText = text;
        }
        public int ButtonId
        {
            get { return this.buttonId; }
            set { this.buttonId = value; }
        }
        public string ButtonText
        {
            get { return this.buttonText; }
            set { this.buttonText = value; }
        }
    }
    public class TaskDialog
    {
        private string windowTitle;
        private string mainInstruction;
        private string content;
        private TaskDialogCommonButtons commonButtons;
        private TaskDialogIcon mainIcon;
        private Icon customMainIcon;
        private TaskDialogIcon footerIcon;
        private Icon customFooterIcon;
        private TaskDialogButton[] buttons;
        private TaskDialogButton[] radioButtons;
        private UnsafeNativeMethods.TASKDIALOG_FLAGS flags;
        private int defaultButton;
        private int defaultRadioButton;
        private string verificationText;
        private string expandedInformation;
        private string expandedControlText;
        private string collapsedControlText;
        private string footer;
        private TaskDialogCallback callback;
        private object callbackData;
        private uint width;
        public TaskDialog()
        {
            this.Reset();
        }
        public static bool IsAvailableOnThisOS
        {
            get
            {
                OperatingSystem os = Environment.OSVersion;
                if (os.Platform != PlatformID.Win32NT)
                {
                    return false;
                }
                return (os.Version.CompareTo(TaskDialog.RequiredOSVersion) >= 0);
            }
        }
        public static Version RequiredOSVersion
        {
            get { return new Version(6, 0, 5243); }
        }
        public string WindowTitle
        {
            get { return this.windowTitle; }
            set { this.windowTitle = value; }
        }
        public string MainInstruction
        {
            get { return this.mainInstruction; }
            set { this.mainInstruction = value; }
        }
        public string Content
        {
            get { return this.content; }
            set { this.content = value; }
        }
        public TaskDialogCommonButtons CommonButtons
        {
            get { return this.commonButtons; }
            set { this.commonButtons = value; }
        }
        public TaskDialogIcon MainIcon
        {
            get { return this.mainIcon; }
            set { this.mainIcon = value; }
        }
        public Icon CustomMainIcon
        {
            get { return this.customMainIcon; }
            set { this.customMainIcon = value; }
        }
        public TaskDialogIcon FooterIcon
        {
            get { return this.footerIcon; }
            set { this.footerIcon = value; }
        }
        public Icon CustomFooterIcon
        {
            get { return this.customFooterIcon; }
            set { this.customFooterIcon = value; }
        }
        [SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        [SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")]
        public TaskDialogButton[] Buttons
        {
            get
            {
                return this.buttons;
            }
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException("value");
                }
                this.buttons = value;
            }
        }
        [SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        [SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")]
        public TaskDialogButton[] RadioButtons
        {
            get
            {
                return this.radioButtons;
            }
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException("value");
                }
                this.radioButtons = value;
            }
        }
        public bool EnableHyperlinks
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_ENABLE_HYPERLINKS) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_ENABLE_HYPERLINKS, value); }
        }
        public bool AllowDialogCancellation
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_ALLOW_DIALOG_CANCELLATION) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_ALLOW_DIALOG_CANCELLATION, value); }
        }
        public bool UseCommandLinks
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_USE_COMMAND_LINKS) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_USE_COMMAND_LINKS, value); }
        }
        public bool UseCommandLinksNoIcon
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_USE_COMMAND_LINKS_NO_ICON) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_USE_COMMAND_LINKS_NO_ICON, value); }
        }
        public bool ExpandFooterArea
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_EXPAND_FOOTER_AREA) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_EXPAND_FOOTER_AREA, value); }
        }
        public bool ExpandedByDefault
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_EXPANDED_BY_DEFAULT) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_EXPANDED_BY_DEFAULT, value); }
        }
        public bool VerificationFlagChecked
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_VERIFICATION_FLAG_CHECKED) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_VERIFICATION_FLAG_CHECKED, value); }
        }
        public bool ShowProgressBar
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_SHOW_PROGRESS_BAR) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_SHOW_PROGRESS_BAR, value); }
        }
        public bool ShowMarqueeProgressBar
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_SHOW_MARQUEE_PROGRESS_BAR) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_SHOW_MARQUEE_PROGRESS_BAR, value); }
        }
        public bool CallbackTimer
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_CALLBACK_TIMER) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_CALLBACK_TIMER, value); }
        }
        public bool PositionRelativeToWindow
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_POSITION_RELATIVE_TO_WINDOW) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_POSITION_RELATIVE_TO_WINDOW, value); }
        }
        public bool RightToLeftLayout
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_RTL_LAYOUT) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_RTL_LAYOUT, value); }
        }
        public bool NoDefaultRadioButton
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_NO_DEFAULT_RADIO_BUTTON) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_NO_DEFAULT_RADIO_BUTTON, value); }
        }
        public bool CanBeMinimized
        {
            get { return (this.flags & UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_CAN_BE_MINIMIZED) != 0; }
            set { this.SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_CAN_BE_MINIMIZED, value); }
        }
        public int DefaultButton
        {
            get { return this.defaultButton; }
            set { this.defaultButton = value; }
        }
        public int DefaultRadioButton
        {
            get { return this.defaultRadioButton; }
            set { this.defaultRadioButton = value; }
        }
        public string VerificationText
        {
            get { return this.verificationText; }
            set { this.verificationText = value; }
        }
        public string ExpandedInformation
        {
            get { return this.expandedInformation; }
            set { this.expandedInformation = value; }
        }
        public string ExpandedControlText
        {
            get { return this.expandedControlText; }
            set { this.expandedControlText = value; }
        }
        public string CollapsedControlText
        {
            get { return this.collapsedControlText; }
            set { this.collapsedControlText = value; }
        }
        public string Footer
        {
            get { return this.footer; }
            set { this.footer = value; }
        }
        public uint Width
        {
            get { return this.width; }
            set { this.width = value; }
        }
        public TaskDialogCallback Callback
        {
            get { return this.callback; }
            set { this.callback = value; }
        }
        public object CallbackData
        {
            get { return this.callbackData; }
            set { this.callbackData = value; }
        }
        public void Reset()
        {
            this.windowTitle = null;
            this.mainInstruction = null;
            this.content = null;
            this.commonButtons = 0;
            this.mainIcon = TaskDialogIcon.None;
            this.customMainIcon = null;
            this.footerIcon = TaskDialogIcon.None;
            this.customFooterIcon = null;
            this.buttons = new TaskDialogButton[0];
            this.radioButtons = new TaskDialogButton[0];
            this.flags = 0;
            this.defaultButton = 0;
            this.defaultRadioButton = 0;
            this.verificationText = null;
            this.expandedInformation = null;
            this.expandedControlText = null;
            this.collapsedControlText = null;
            this.footer = null;
            this.callback = null;
            this.callbackData = null;
            this.width = 0;
        }
        public int Show()
        {
            bool verificationFlagChecked;
            int radioButtonResult;
            return this.Show(IntPtr.Zero, out verificationFlagChecked, out radioButtonResult);
        }
        public int Show(IWin32Window owner)
        {
            bool verificationFlagChecked;
            int radioButtonResult;
            return this.Show((owner == null ? IntPtr.Zero : owner.Handle), out verificationFlagChecked, out radioButtonResult);
        }
        public int Show(IntPtr hwndOwner)
        {
            bool verificationFlagChecked;
            int radioButtonResult;
            return this.Show(hwndOwner, out verificationFlagChecked, out radioButtonResult);
        }
        public int Show(IWin32Window owner, out bool verificationFlagChecked)
        {
            int radioButtonResult;
            return this.Show((owner == null ? IntPtr.Zero : owner.Handle), out verificationFlagChecked, out radioButtonResult);
        }
        public int Show(IntPtr hwndOwner, out bool verificationFlagChecked)
        {
            int radioButtonResult;
            return this.PrivateShow(hwndOwner, out verificationFlagChecked, out radioButtonResult);
        }
        public int Show(IWin32Window owner, out bool verificationFlagChecked, out int radioButtonResult)
        {
            return this.Show((owner == null ? IntPtr.Zero : owner.Handle), out verificationFlagChecked, out radioButtonResult);
        }
        public int Show(IntPtr hwndOwner, out bool verificationFlagChecked, out int radioButtonResult)
        {
            return this.PrivateShow(hwndOwner, out verificationFlagChecked, out radioButtonResult);
        }
        private int PrivateShow(IntPtr hwndOwner, out bool verificationFlagChecked, out int radioButtonResult)
        {
            verificationFlagChecked = false;
            radioButtonResult = 0;
            int result = 0;
            UnsafeNativeMethods.TASKDIALOGCONFIG config = new UnsafeNativeMethods.TASKDIALOGCONFIG();
            try
            {
                config.cbSize = (uint)Marshal.SizeOf(typeof(UnsafeNativeMethods.TASKDIALOGCONFIG));
                config.hwndParent = hwndOwner;
                config.dwFlags = this.flags;
                config.dwCommonButtons = this.commonButtons;
                if (!string.IsNullOrEmpty(this.windowTitle))
                {
                    config.pszWindowTitle = this.windowTitle;
                }
                config.MainIcon = (IntPtr)this.mainIcon;
                if (this.customMainIcon != null)
                {
                    config.dwFlags |= UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_USE_HICON_MAIN;
                    config.MainIcon = this.customMainIcon.Handle;
                }
                if (!string.IsNullOrEmpty(this.mainInstruction))
                {
                    config.pszMainInstruction = this.mainInstruction;
                }
                if (!string.IsNullOrEmpty(this.content))
                {
                    config.pszContent = this.content;
                }
                TaskDialogButton[] customButtons = this.buttons;
                if (customButtons.Length > 0)
                {
                    int elementSize = Marshal.SizeOf(typeof(TaskDialogButton));
                    config.pButtons = Marshal.AllocHGlobal(elementSize * (int)customButtons.Length);
                    for (int i = 0; i < customButtons.Length; i++)
                    {
                        unsafe
                        {
                            byte* p = (byte*)config.pButtons;
                            Marshal.StructureToPtr(customButtons[i], (IntPtr)(p + (elementSize * i)), false);
                        }
                        config.cButtons++;
                    }
                }
                TaskDialogButton[] customRadioButtons = this.radioButtons;
                if (customRadioButtons.Length > 0)
                {
                    int elementSize = Marshal.SizeOf(typeof(TaskDialogButton));
                    config.pRadioButtons = Marshal.AllocHGlobal(elementSize * (int)customRadioButtons.Length);
                    for (int i = 0; i < customRadioButtons.Length; i++)
                    {
                        unsafe
                        {
                            byte* p = (byte*)config.pRadioButtons;
                            Marshal.StructureToPtr(customRadioButtons[i], (IntPtr)(p + (elementSize * i)), false);
                        }
                        config.cRadioButtons++;
                    }
                }
                config.nDefaultButton = this.defaultButton;
                config.nDefaultRadioButton = this.defaultRadioButton;
                if (!string.IsNullOrEmpty(this.verificationText))
                {
                    config.pszVerificationText = this.verificationText;
                }
                if (!string.IsNullOrEmpty(this.expandedInformation))
                {
                    config.pszExpandedInformation = this.expandedInformation;
                }
                if (!string.IsNullOrEmpty(this.expandedControlText))
                {
                    config.pszExpandedControlText = this.expandedControlText;
                }
                if (!string.IsNullOrEmpty(this.collapsedControlText))
                {
                    config.pszCollapsedControlText = this.CollapsedControlText;
                }
                config.FooterIcon = (IntPtr)this.footerIcon;
                if (this.customFooterIcon != null)
                {
                    config.dwFlags |= UnsafeNativeMethods.TASKDIALOG_FLAGS.TDF_USE_HICON_FOOTER;
                    config.FooterIcon = this.customFooterIcon.Handle;
                }
                if (!string.IsNullOrEmpty(this.footer))
                {
                    config.pszFooter = this.footer;
                }
                if (this.callback != null)
                {
                    config.pfCallback = new UnsafeNativeMethods.TaskDialogCallback(this.PrivateCallback);
                }
                config.cxWidth = this.width;
                UnsafeNativeMethods.TaskDialogIndirect(ref config, out result, out radioButtonResult, out verificationFlagChecked);
            }
            finally
            {
                if (config.pButtons != IntPtr.Zero)
                {
                    int elementSize = Marshal.SizeOf(typeof(TaskDialogButton));
                    for (int i = 0; i < config.cButtons; i++)
                    {
                        unsafe
                        {
                            byte* p = (byte*)config.pButtons;
                            Marshal.DestroyStructure((IntPtr)(p + (elementSize * i)), typeof(TaskDialogButton));
                        }
                    }
                    Marshal.FreeHGlobal(config.pButtons);
                }
                if (config.pRadioButtons != IntPtr.Zero)
                {
                    int elementSize = Marshal.SizeOf(typeof(TaskDialogButton));
                    for (int i = 0; i < config.cRadioButtons; i++)
                    {
                        unsafe
                        {
                            byte* p = (byte*)config.pRadioButtons;
                            Marshal.DestroyStructure((IntPtr)(p + (elementSize * i)), typeof(TaskDialogButton));
                        }
                    }
                    Marshal.FreeHGlobal(config.pRadioButtons);
                }
            }
            return result;
        }
        private int PrivateCallback([In] IntPtr hwnd, [In] uint msg, [In] UIntPtr wparam, [In] IntPtr lparam, [In] IntPtr refData)
        {
            TaskDialogCallback callback = this.callback;
            if (callback != null)
            {
                ActiveTaskDialog activeDialog = new ActiveTaskDialog(hwnd);
                TaskDialogNotificationArgs args = new TaskDialogNotificationArgs();
                args.Notification = (TaskDialogNotification)msg;
                switch (args.Notification)
                {
                    case TaskDialogNotification.ButtonClicked:
                    case TaskDialogNotification.RadioButtonClicked:
                        args.ButtonId = (int)wparam;
                        break;
                    case TaskDialogNotification.HyperlinkClicked:
                        args.Hyperlink = Marshal.PtrToStringUni(lparam);
                        break;
                    case TaskDialogNotification.Timer:
                        args.TimerTickCount = (uint)wparam;
                        break;
                    case TaskDialogNotification.VerificationClicked:
                        args.VerificationFlagChecked = (wparam != UIntPtr.Zero);
                        break;
                    case TaskDialogNotification.ExpandoButtonClicked:
                        args.Expanded = (wparam != UIntPtr.Zero);
                        break;
                }
                return (callback(activeDialog, args, this.callbackData) ? 1 : 0);
            }
            return 0;
        }
        private void SetFlag(UnsafeNativeMethods.TASKDIALOG_FLAGS flag, bool value)
        {
            if (value)
            {
                this.flags |= flag;
            }
            else
            {
                this.flags &= ~flag;
            }
        }
    }
}
