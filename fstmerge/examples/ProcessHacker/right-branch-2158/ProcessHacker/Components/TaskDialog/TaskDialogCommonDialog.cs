namespace ProcessHacker.Components
{
    using System;
    using System.Windows.Forms;
    public class TaskDialogCommonDialog : CommonDialog
    {
        private TaskDialog taskDialog;
        private int taskDialogResult;
        private bool verificationFlagCheckedResult;
        public TaskDialogCommonDialog(TaskDialog taskDialog)
        {
            if (taskDialog == null)
            {
                throw new ArgumentNullException("taskDialog");
            }
            this.taskDialog = taskDialog;
        }
        public TaskDialog TaskDialog
        {
            get { return this.taskDialog; }
        }
        public int TaskDialogResult
        {
            get { return this.taskDialogResult; }
        }
        public bool VerificationFlagCheckedResult
        {
            get { return this.verificationFlagCheckedResult; }
        }
        public override void Reset()
        {
            this.taskDialog.Reset();
        }
        protected override bool RunDialog(IntPtr hwndOwner)
        {
            this.taskDialogResult = this.taskDialog.Show(hwndOwner, out this.verificationFlagCheckedResult);
            return (this.taskDialogResult != (int)DialogResult.Cancel);
        }
    }
}
