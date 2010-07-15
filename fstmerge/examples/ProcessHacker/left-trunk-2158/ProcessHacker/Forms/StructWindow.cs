

using System;
using System.Windows.Forms;
using ProcessHacker.Components;
using ProcessHacker.Structs;

namespace ProcessHacker
{
    public partial class StructWindow : Form
    {
        private int _pid;
        private IntPtr _address;
        private StructDef _struct;

        public StructWindow(int pid, IntPtr address, StructDef struc)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            _pid = pid;
            _address = address;
            _struct = struc;
        }

        private void StructWindow_Load(object sender, EventArgs e)
        {
            StructViewer sv = new StructViewer(_pid, _address, _struct);

            if (sv.Error)
                this.Close();

            sv.Dock = DockStyle.Fill;
            panel.Controls.Add(sv);
        }

        private void buttonClose_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}
