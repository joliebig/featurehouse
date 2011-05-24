

using ProcessHacker.Common;

namespace ProcessHacker
{
    public class IoHistoryIcon : ProviderIcon
    {
        public IoHistoryIcon()
        {
            this.UseSecondLine = true;
            this.UseLongData = true;
            this.OverlaySecondLine = true;
            this.MinMaxValue = 128 * 1024;
        }

        protected override void ProviderUpdated()
        {
            if (this.Provider.RunCount < 2)
                return;

            this.LineColor1 = Properties.Settings.Default.PlotterIOROColor;
            this.LineColor2 = Properties.Settings.Default.PlotterIOWColor;

            this.Update(
                this.Provider.LongDeltas[SystemStats.IoRead] +
                this.Provider.LongDeltas[SystemStats.IoOther],
                this.Provider.LongDeltas[SystemStats.IoWrite]
                );

            this.Redraw();

            string text = "R: " + Utils.FormatSize(this.Provider.LongDeltas[SystemStats.IoRead]) +
                "\nW: " + Utils.FormatSize(this.Provider.LongDeltas[SystemStats.IoWrite]) +
                "\nO: " + Utils.FormatSize(this.Provider.LongDeltas[SystemStats.IoOther]);

            if (this.Provider.Dictionary.ContainsKey(this.Provider.PIDWithMostIoActivity))
            {
                string mostIoName = this.Provider.Dictionary[this.Provider.PIDWithMostIoActivity].Name;

                if (text.Length + mostIoName.Length + 1 < 64)
                    text += "\n" + mostIoName;
            }

            this.Text = text;
        }
    }
}
