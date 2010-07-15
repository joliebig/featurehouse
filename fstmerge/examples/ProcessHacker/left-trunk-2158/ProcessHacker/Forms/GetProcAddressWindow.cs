

using System;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.Native.Api;

namespace ProcessHacker
{
    public partial class GetProcAddressWindow : Form
    {
        private string _fileName;

        public GetProcAddressWindow(string fileName)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            _fileName = fileName;

            textProcName.Select();
        }

        private void buttonLookup_Click(object sender, EventArgs e)
        {
            IntPtr module = Win32.LoadLibraryEx(_fileName, IntPtr.Zero, Win32.DontResolveDllReferences);
            IntPtr address = IntPtr.Zero;
            int ordinal = 0;

            if (module == IntPtr.Zero)
            {
                textProcAddress.Text = "Could not load library!";
            }

            if ((textProcName.Text.Length > 0) &&
                (textProcName.Text[0] >= '0' && textProcName.Text[0] <= '9'))
                ordinal = (int)BaseConverter.ToNumberParse(textProcName.Text, false);

            if (ordinal != 0)
            {
                address = Win32.GetProcAddress(module, (ushort)ordinal);
            }
            else
            {
                address = Win32.GetProcAddress(module, textProcName.Text);
            }

            if (address != IntPtr.Zero)
            {
                textProcAddress.Text = "0x" + address.ToString("x");
                textProcAddress.SelectAll();
                textProcAddress.Focus();
            }
            else
            {
                textProcAddress.Text = Win32.GetLastErrorMessage();
            }


            if (module != IntPtr.Zero)
                Win32.FreeLibrary(module);
        }

        private void textProcName_Enter(object sender, EventArgs e)
        {
            this.AcceptButton = buttonLookup;
        }

        private void textProcName_Leave(object sender, EventArgs e)
        {
            this.AcceptButton = null;
        }

        private void buttonClose_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}
