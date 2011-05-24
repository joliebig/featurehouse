

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace ProcessHacker
{
    public partial class ComboBoxPickerWindow : Form
    {
        public ComboBoxPickerWindow(string[] items)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            comboBox.Items.AddRange(items);

            if (comboBox.Items.Count > 0)
                comboBox.SelectedItem = comboBox.Items[0];
        }

        public string SelectedItem
        {
            get { return comboBox.SelectedItem as string; }
            set { comboBox.SelectedItem = value; }
        }

        public string Message
        {
            get
            {
                return labelText.Text;
            }
            set
            {
                labelText.Text = value;
            }
        }

        private void buttonOK_Click(object sender, EventArgs e)
        {
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
