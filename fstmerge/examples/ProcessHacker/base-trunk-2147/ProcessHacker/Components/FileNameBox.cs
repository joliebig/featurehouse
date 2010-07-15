

using System;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.Native;

namespace ProcessHacker.Components
{
    public partial class FileNameBox : UserControl
    {
        public FileNameBox()
        {
            InitializeComponent();
        }

        public event EventHandler TextBoxLeave;

        public bool TextBoxFocused
        {
            get { return textFileName.Focused; }
        }

        public bool ReadOnly
        {
            get { return textFileName.ReadOnly; }
            set { textFileName.ReadOnly = value; }
        }

        public override string Text
        {
            get
            {
                return textFileName.Text;
            }
            set
            {
                textFileName.Text = value;
            }
        }

        private void buttonProperties_Click(object sender, EventArgs e)
        {
            try
            {
                FileUtils.ShowProperties(textFileName.Text);
            }
            catch (Exception ex)
            {
                PhUtils.ShowException("Unable to show properties for the file", ex);
            }
        }

        private void buttonExplore_Click(object sender, EventArgs e)
        {
            try
            {
                Utils.ShowFileInExplorer(textFileName.Text);
            }
            catch (Exception ex)
            {
                PhUtils.ShowException("Unable to show the file", ex);
            }
        }

        private void textFileName_Leave(object sender, EventArgs e)
        {
            if (TextBoxLeave != null)
                TextBoxLeave(sender, e);
        }
    }
}
