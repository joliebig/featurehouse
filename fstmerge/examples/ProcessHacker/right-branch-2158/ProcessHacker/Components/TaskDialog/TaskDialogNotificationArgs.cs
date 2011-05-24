namespace ProcessHacker.Components
{
    using System;
    using System.Drawing;
    using System.Windows.Forms;
    using System.Runtime.InteropServices;
    public class TaskDialogNotificationArgs
    {
        private TaskDialogNotification notification;
        private int buttonId;
        private string hyperlink;
        private uint timerTickCount;
        private bool verificationFlagChecked;
        private bool expanded;
        public TaskDialogNotification Notification
        {
            get { return this.notification; }
            set { this.notification = value; }
        }
        public int ButtonId
        {
            get { return this.buttonId; }
            set { this.buttonId = value; }
        }
        public string Hyperlink
        {
            get { return this.hyperlink; }
            set { this.hyperlink = value; }
        }
        public uint TimerTickCount
        {
            get { return this.timerTickCount; }
            set { this.timerTickCount = value; }
        }
        public bool VerificationFlagChecked
        {
            get { return this.verificationFlagChecked; }
            set { this.verificationFlagChecked = value; }
        }
        public bool Expanded
        {
            get { return this.expanded; }
            set { this.expanded = value; }
        }
    }
}
