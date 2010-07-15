

using System.Runtime.InteropServices;
using ProcessHacker.Common;
using ProcessHacker.Native.Api;

namespace ProcessHacker
{
    public class PhysMemHistoryIcon : ProviderIcon
    {
        public PhysMemHistoryIcon()
        {
            this.UseSecondLine = false;
            this.UseLongData = true;

            PerformanceInformation info = new PerformanceInformation();

            info.Size = Marshal.SizeOf(info);
            Win32.GetPerformanceInfo(out info, info.Size);
            this.MinMaxValue = info.PhysicalTotal.ToInt64();
        }

        protected override void ProviderUpdated()
        {
            this.LineColor1 = Properties.Settings.Default.PlotterMemoryWSColor;
            this.Update(this.MinMaxValue - this.Provider.Performance.AvailablePages, 0);
            this.Redraw();

            this.Text = "Physical Memory: " + Utils.FormatSize(
                (long)(this.MinMaxValue - this.Provider.Performance.AvailablePages) *
                this.Provider.System.PageSize);
        }
    }
}
