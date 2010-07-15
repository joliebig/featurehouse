

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace ProcessHacker
{
    public partial class ProcessPickerWindow : Form
    {
        public ProcessPickerWindow()
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();
        }

        public int SelectedPid { get; private set; }

        public string Label
        {
            get { return labelLabel.Text; }
            set { labelLabel.Text = value; }
        }

        private void ProcessPickerWindow_Load(object sender, EventArgs e)
        {
            treeProcesses.Tree.SelectionMode = Aga.Controls.Tree.TreeSelectionMode.Single;
            treeProcesses.Provider = Program.ProcessProvider;
        }

        private void ProcessPickerWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            treeProcesses.Provider = null;
        }

        private void buttonOK_Click(object sender, EventArgs e)
        {
            this.SelectedPid = treeProcesses.SelectedNodes[0].Pid;
            this.DialogResult = DialogResult.OK;
            this.Close();
        }

        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.DialogResult = DialogResult.Cancel;
            this.Close();
        }

        private void treeProcesses_SelectionChanged(object sender, EventArgs e)
        {
            if (treeProcesses.SelectedNodes.Count == 1)
                buttonOK.Enabled = true;
            else
                buttonOK.Enabled = false;
        }

        private void treeProcesses_DoubleClick(object sender, EventArgs e)
        {
            if (treeProcesses.SelectedNodes.Count == 1)
                buttonOK_Click(sender, e);
        }
    }
}
