

using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using ProcessHacker.Native.Api;

namespace ProcessHacker
{
    public partial class MiniSysInfo : Form
    {
        [StructLayout(LayoutKind.Sequential)]
        struct MARGINS
        {
            public int Left;
            public int Right;
            public int Top;
            public int Bottom;
        }

        [DllImport("dwmapi.dll", SetLastError = true)]
        static extern int DwmExtendFrameIntoClientArea(IntPtr hWnd, ref MARGINS inset);

        MARGINS margins = new MARGINS() { Left = -1, Right = -1, Top = -1, Bottom = -1 };

        public MiniSysInfo()
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            DwmExtendFrameIntoClientArea(this.Handle, ref margins);

            plotterCPU.BackColor = Color.FromArgb(255, 0, 0, 0);
            plotterCPU.Draw();
            plotterCPU.Data1 = Program.ProcessProvider.FloatHistory["Kernel"];
            plotterCPU.Data2 = Program.ProcessProvider.FloatHistory["User"];

            plotterIO.BackColor = Color.FromArgb(255, 0, 0, 0);
            plotterIO.Draw();
            plotterIO.LongData1 = Program.ProcessProvider.LongHistory[SystemStats.IoReadOther];
            plotterIO.LongData2 = Program.ProcessProvider.LongHistory[SystemStats.IoWrite];

            Program.ProcessProvider.Updated += new ProcessSystemProvider.ProviderUpdateOnce(ProcessProvider_Updated);
        }

        protected override void WndProc(ref Message m)
        {
            base.WndProc(ref m);

            if (m.Msg == (int)WindowMessage.NcCalcSize)
            {
                if (m.WParam.ToInt32() != 0)
                {
                    m.Result = new IntPtr(0);
                }
            }
        }

        private void MiniSysInfo_Deactivate(object sender, EventArgs e)
        {
            this.Close();
        }

        private void MiniSysInfo_FormClosing(object sender, FormClosingEventArgs e)
        {
            Program.ProcessProvider.Updated -= new ProcessSystemProvider.ProviderUpdateOnce(ProcessProvider_Updated);
        }

        private void ProcessProvider_Updated()
        {
            this.BeginInvoke(new MethodInvoker(delegate
            {
                plotterCPU.LineColor1 = Properties.Settings.Default.PlotterCPUKernelColor;
                plotterCPU.LineColor2 = Properties.Settings.Default.PlotterCPUUserColor;
                plotterCPU.MoveGrid();
                plotterCPU.Draw();

                plotterIO.LineColor1 = Properties.Settings.Default.PlotterIOROColor;
                plotterIO.LineColor2 = Properties.Settings.Default.PlotterIOWColor;
                plotterIO.MoveGrid();
                plotterIO.Draw();
            }));
        }
    }
}
