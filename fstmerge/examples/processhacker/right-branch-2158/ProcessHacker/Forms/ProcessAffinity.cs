

using System;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker
{
    public partial class ProcessAffinity : Form
    {
        private int _pid;

        public ProcessAffinity(int pid)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            _pid = pid;

            try
            {
                using (ProcessHandle phandle = new ProcessHandle(pid, ProcessAccess.QueryInformation))
                {
                    long systemMask;
                    long processMask;

                    processMask = phandle.GetAffinityMask(out systemMask);

                    for (int i = 0; (systemMask & (1 << i)) != 0; i++)
                    {
                        CheckBox c = new CheckBox();

                        c.Name = "cpu" + i.ToString();
                        c.Text = "CPU " + i.ToString();
                        c.Tag = i;

                        c.FlatStyle = FlatStyle.System;
                        c.Checked = (processMask & (1 << i)) != 0;
                        c.Margin = new Padding(3, 3, 3, 0);

                        flowPanel.Controls.Add(c);
                    }
                }
            }
            catch (Exception ex)
            {
                PhUtils.ShowException("Unable to get process affinity", ex);

                this.Close();
                return;
            }
        }

        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void buttonOK_Click(object sender, EventArgs e)
        {
            long newMask = 0;

            for (int i = 0; i < flowPanel.Controls.Count; i++)
            {
                CheckBox c = (CheckBox)flowPanel.Controls["cpu" + i.ToString()];

                newMask |= ((long)(c.Checked ? 1 : 0) << i);
            }

            try
            {
                using (ProcessHandle phandle = new ProcessHandle(_pid, ProcessAccess.SetInformation))
                    phandle.SetAffinityMask(newMask);

                this.Close();
            }
            catch (Exception ex)
            {
                PhUtils.ShowException("Unable to set process affinity", ex);
            }
        }
    }
}
