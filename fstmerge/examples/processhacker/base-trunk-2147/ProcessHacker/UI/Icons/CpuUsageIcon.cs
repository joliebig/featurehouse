

using System.Drawing;

namespace ProcessHacker
{
    public class CpuUsageIcon : UsageIcon
    {
        private ProcessSystemProvider _provider = Program.ProcessProvider;
        private bool _enabled = false;

        public CpuUsageIcon()
        { }

        public override void Dispose()
        {
            this.Enabled = false;
            base.Dispose();
        }

        private void ProcessProvider_Updated()
        {

            this.ProviderUpdated();
        }

        public bool Enabled
        {
            get { return _enabled; }
            set
            {
                if (value != _enabled)
                {
                    if (value)
                        Program.ProcessProvider.Updated += ProcessProvider_Updated;
                    else
                        Program.ProcessProvider.Updated -= ProcessProvider_Updated;
                }

                _enabled = value;
            }
        }

        private ProcessSystemProvider Provider
        {
            get { return Program.ProcessProvider; }
        }

        private void ProviderUpdated()
        {
            float k = _provider.CurrentCpuKernelUsage;
            float u = _provider.CurrentCpuUserUsage;
            int height = this.Size.Height;
            int width = this.Size.Width;

            using (Bitmap b = new Bitmap(width, height))
            {
                using (Graphics g = Graphics.FromImage(b))
                {
                    int kl = (int)(k * height);
                    int ul = (int)(u * height);
                    Color kline = Properties.Settings.Default.PlotterCPUKernelColor;
                    Color kfill = Color.FromArgb(100, kline);
                    Color uline = Properties.Settings.Default.PlotterCPUUserColor;
                    Color ufill = Color.FromArgb(100, uline);

                    g.FillRectangle(new SolidBrush(Color.Black), g.ClipBounds);

                    if (kl + ul == 0)
                        g.DrawLine(new Pen(uline), 0, height - 1, width - 1, height - 1);

                    g.FillRectangle(new SolidBrush(ufill), 0, height - (ul + kl), width, ul);
                    g.DrawLine(new Pen(uline), 0, height - (ul + kl) - 1, width, height - (ul + kl) - 1);

                    if (kl > 0)
                    {
                        g.FillRectangle(new SolidBrush(kfill), 0, height - kl, width, kl);
                        g.DrawLine(new Pen(kline), 0, height - kl - 1, width, height - kl - 1);
                    }
                }

                var newIcon = Icon.FromHandle(b.GetHicon());
                var oldIcon = this.Icon;

                this.Icon = newIcon;
                ProcessHacker.Native.Api.Win32.DestroyIcon(oldIcon.Handle);
            }

            string mostCpuProcess = _provider.MostCpuHistory[0];

            string text = "CPU Usage: " + ((k + u) * 100).ToString("N2") + "%"



                ;

            if (text.Length + mostCpuProcess.Length + 1 < 64)
                text += "\n" + mostCpuProcess;

            this.Text = text;
        }
    }
}
