namespace ProcessHacker.Components
{
    using System;
    using System.Drawing;
    using System.Windows.Forms;
    using System.Runtime.InteropServices;
    using System.Diagnostics.CodeAnalysis;
    public class ActiveTaskDialog : IWin32Window
    {
        [SuppressMessage("Microsoft.Reliability", "CA2006:UseSafeHandleToEncapsulateNativeResources")]
        private IntPtr handle;
        internal ActiveTaskDialog(IntPtr handle)
        {
            if (handle == IntPtr.Zero)
            {
                throw new ArgumentNullException("handle");
            }
            this.handle = handle;
        }
        public IntPtr Handle
        {
            get { return this.handle; }
        }
        public bool ClickButton(int buttonId)
        {
            return UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_CLICK_BUTTON,
                (IntPtr)buttonId,
                IntPtr.Zero) != IntPtr.Zero;
        }
        public bool SetMarqueeProgressBar(bool marquee)
        {
            return UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_SET_MARQUEE_PROGRESS_BAR,
                (marquee ? (IntPtr)1 : IntPtr.Zero),
                IntPtr.Zero) != IntPtr.Zero;
        }
        public bool SetProgressBarState(ProgressBarState newState)
        {
            return UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_SET_PROGRESS_BAR_STATE,
                (IntPtr)newState,
                IntPtr.Zero) != IntPtr.Zero;
        }
        public bool SetProgressBarRange(Int16 minRange, Int16 maxRange)
        {
            IntPtr lparam = (IntPtr)((((Int32)minRange) & 0xffff) | ((((Int32)maxRange) & 0xffff) << 16));
            return UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_SET_PROGRESS_BAR_RANGE,
                IntPtr.Zero,
                lparam) != IntPtr.Zero;
        }
        public int SetProgressBarPosition(int newPosition)
        {
            return (int)UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_SET_PROGRESS_BAR_POS,
                (IntPtr)newPosition,
                IntPtr.Zero);
        }
        public void SetProgressBarMarquee(bool startMarquee, uint speed)
        {
            UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_SET_PROGRESS_BAR_MARQUEE,
                (startMarquee ? new IntPtr(1) : IntPtr.Zero),
                (IntPtr)speed);
        }
        public bool SetContent(string content)
        {
            return UnsafeNativeMethods.SendMessageWithString(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_SET_ELEMENT_TEXT,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ELEMENTS.TDE_CONTENT,
                content) != IntPtr.Zero;
        }
        public bool SetExpandedInformation(string expandedInformation)
        {
            return UnsafeNativeMethods.SendMessageWithString(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_SET_ELEMENT_TEXT,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ELEMENTS.TDE_EXPANDED_INFORMATION,
                expandedInformation) != IntPtr.Zero;
        }
        public bool SetFooter(string footer)
        {
            return UnsafeNativeMethods.SendMessageWithString(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_SET_ELEMENT_TEXT,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ELEMENTS.TDE_FOOTER,
                footer) != IntPtr.Zero;
        }
        public bool SetMainInstruction(string mainInstruction)
        {
            return UnsafeNativeMethods.SendMessageWithString(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_SET_ELEMENT_TEXT,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ELEMENTS.TDE_MAIN_INSTRUCTION,
                mainInstruction) != IntPtr.Zero;
        }
        public void ClickRadioButton(int buttonId)
        {
            UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_CLICK_RADIO_BUTTON,
                (IntPtr)buttonId,
                IntPtr.Zero);
        }
        public void EnableButton(int buttonId, bool enable)
        {
            UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_ENABLE_BUTTON,
                (IntPtr)buttonId,
                (IntPtr)(enable ? 1 : 0));
        }
        public void EnableRadioButton(int buttonId, bool enable)
        {
            UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_ENABLE_RADIO_BUTTON,
                (IntPtr)buttonId,
                (IntPtr)(enable ? 1 : 0));
        }
        public void ClickVerification(bool checkedState, bool setKeyboardFocusToCheckBox)
        {
            UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_CLICK_VERIFICATION,
                (checkedState ? new IntPtr(1) : IntPtr.Zero),
                (setKeyboardFocusToCheckBox ? new IntPtr(1) : IntPtr.Zero));
        }
        public void UpdateContent(string content)
        {
            UnsafeNativeMethods.SendMessageWithString(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_UPDATE_ELEMENT_TEXT,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ELEMENTS.TDE_CONTENT,
                content);
        }
        public void UpdateExpandedInformation(string expandedInformation)
        {
            UnsafeNativeMethods.SendMessageWithString(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_UPDATE_ELEMENT_TEXT,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ELEMENTS.TDE_EXPANDED_INFORMATION,
                expandedInformation);
        }
        public void UpdateFooter(string footer)
        {
            UnsafeNativeMethods.SendMessageWithString(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_UPDATE_ELEMENT_TEXT,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ELEMENTS.TDE_FOOTER,
                footer);
        }
        public void UpdateMainInstruction(string mainInstruction)
        {
            UnsafeNativeMethods.SendMessageWithString(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_UPDATE_ELEMENT_TEXT,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ELEMENTS.TDE_MAIN_INSTRUCTION,
                mainInstruction);
        }
        public void SetButtonElevationRequiredState(int buttonId, bool elevationRequired)
        {
            UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE,
                (IntPtr)buttonId,
                (IntPtr)(elevationRequired ? new IntPtr(1) : IntPtr.Zero));
        }
        public void UpdateMainIcon(TaskDialogIcon icon)
        {
            UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_UPDATE_ICON,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ICON_ELEMENTS.TDIE_ICON_MAIN,
                (IntPtr)icon);
        }
        public void UpdateMainIcon(Icon icon)
        {
            UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_UPDATE_ICON,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ICON_ELEMENTS.TDIE_ICON_MAIN,
                (icon == null ? IntPtr.Zero : icon.Handle));
        }
        public void UpdateFooterIcon(TaskDialogIcon icon)
        {
            UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_UPDATE_ICON,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ICON_ELEMENTS.TDIE_ICON_FOOTER,
                (IntPtr)icon);
        }
        public void UpdateFooterIcon(Icon icon)
        {
            UnsafeNativeMethods.SendMessage(
                this.handle,
                (uint)UnsafeNativeMethods.TASKDIALOG_MESSAGES.TDM_UPDATE_ICON,
                (IntPtr)UnsafeNativeMethods.TASKDIALOG_ICON_ELEMENTS.TDIE_ICON_FOOTER,
                (icon == null ? IntPtr.Zero : icon.Handle));
        }
    }
}
