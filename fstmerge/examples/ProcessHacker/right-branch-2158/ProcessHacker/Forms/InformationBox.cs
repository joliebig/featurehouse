

using System;
using System.Windows.Forms;

namespace ProcessHacker
{
    public partial class InformationBox : Form
    {
        public InformationBox(string values)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            if (!Program.BadConfig)
                this.Size = Properties.Settings.Default.InformationBoxSize;

            textValues.Text = values;
            textValues.Select(0, 0);
        }

        private void InformationBox_Load(object sender, EventArgs e)
        {

            textValues.Select();
            textValues.ScrollToCaret();
        }

        private void InformationBox_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (!Program.BadConfig)
                Properties.Settings.Default.InformationBoxSize = this.Size;
        }

        public TextBox TextBox { get { return textValues; } }

        public string DefaultFileName { get; set; }

        public string Title
        {
            get { return this.Text; }
            set { this.Text = value; }
        }

        public bool ShowSaveButton
        {
            get { return buttonSave.Visible; }
            set { buttonSave.Visible = value; }
        }

        private void buttonClose_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void buttonSave_Click(object sender, EventArgs e)
        {
            SaveFileDialog sfd = new SaveFileDialog();

            sfd.FileName = DefaultFileName;
            sfd.Filter = "Text Files (*.txt)|*.txt|All Files (*.*)|*.*";

            if (sfd.ShowDialog() == DialogResult.OK)
                System.IO.File.WriteAllText(sfd.FileName, textValues.Text);
        }

        private void buttonCopy_Click(object sender, EventArgs e)
        {
            if (textValues.Text.Length == 0)
                return;

            if (textValues.SelectionLength == 0)
            {
                Clipboard.SetText(textValues.Text);
                textValues.Select();
                textValues.SelectAll();
            }
            else
            {
                Clipboard.SetText(textValues.SelectedText);
            }
        }

        private void InformationBox_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Control && e.KeyCode == Keys.A)
            {
                textValues.SelectAll();
                e.Handled = true;
            }
        }
    }
}
