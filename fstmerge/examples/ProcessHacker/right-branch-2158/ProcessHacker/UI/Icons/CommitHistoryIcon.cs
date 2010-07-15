

using System.Runtime.InteropServices;
using ProcessHacker.Common;
using ProcessHacker.Native.Api;

namespace ProcessHacker
{
    public class CommitHistoryIcon : ProviderIcon
    {
        public CommitHistoryIcon()
        {
            this.UseSecondLine = false;
            this.UseLongData = true;

            PerformanceInformation info = new PerformanceInformation();

            info.Size = Marshal.SizeOf(info);
            Win32.GetPerformanceInfo(out info, info.Size);
            this.MinMaxValue = info.CommitLimit.ToInt64();
        }

        protected override void ProviderUpdated()
        {
            this.LineColor1 = Properties.Settings.Default.PlotterMemoryPrivateColor;
            this.Update(this.Provider.Performance.CommittedPages, 0);
            this.Redraw();

            this.Text = "Commit: " + Utils.FormatSize(
                (long)this.Provider.Performance.CommittedPages * this.Provider.System.PageSize);
        }
    }
}
