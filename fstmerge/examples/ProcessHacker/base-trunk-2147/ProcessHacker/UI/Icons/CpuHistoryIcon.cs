

using System;
using System.Collections.Generic;
using System.Text;

namespace ProcessHacker
{
    public class CpuHistoryIcon : ProviderIcon
    {
        public CpuHistoryIcon()
        {
            this.UseSecondLine = true;
        }

        protected override void ProviderUpdated()
        {
            this.LineColor1 = Properties.Settings.Default.PlotterCPUKernelColor;
            this.LineColor2 = Properties.Settings.Default.PlotterCPUUserColor;
            this.Update(this.Provider.CurrentCpuKernelUsage, this.Provider.CurrentCpuUserUsage);
            this.Redraw();

            string text = "CPU Usage: " + (this.Provider.CurrentCpuUsage * 100).ToString("F2") + "%";

            string mostCpuText = this.Provider.MostCpuHistory[0];

            if (text.Length + mostCpuText.Length + 1 < 64)
                text += "\n" + mostCpuText;

            this.Text = text;
        }
    }
}
