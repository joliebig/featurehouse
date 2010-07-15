

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace ProcessHacker
{
    public partial class PromptBox : Form
    {
        private string _value;
        public static string LastValue;

        public string Value
        {
            get { return _value; }
        }

        public PromptBox() : this("", false) { }

        public PromptBox(string value) : this(value, false) { }

        public PromptBox(bool multiline) : this("", multiline) { }

        public PromptBox(string value, bool multiline)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            if (value == "")
            {
                textValue.Text = LastValue;
            }
            else
            {
                textValue.Text = value;
            }

            if (multiline)
            {
                textValue.Multiline = true;
                textValue.ScrollBars = ScrollBars.Vertical;
                this.Size = new Size(this.Size.Width, this.Size.Height + 100);
                this.AcceptButton = null;
            }
            else
            {
                this.AcceptButton = buttonOK;
            }
        }

        public TextBox TextBox
        {
            get { return textValue; }
        }

        private void buttonOK_Click(object sender, EventArgs e)
        {
            _value = textValue.Text;
            LastValue = _value;
            this.DialogResult = DialogResult.OK;
            this.Close();
        }

        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.DialogResult = DialogResult.Cancel;
            this.Close();
        }
    }
}
