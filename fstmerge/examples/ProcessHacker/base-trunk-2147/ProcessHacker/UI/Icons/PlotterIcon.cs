

using System.Drawing;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.Components;

namespace ProcessHacker
{
    public abstract class PlotterIcon : UsageIcon
    {
        private HistoryManager<bool, float> _floatHistory = new HistoryManager<bool, float>();
        private HistoryManager<bool, long> _longHistory = new HistoryManager<bool, long>();
        private Plotter _plotter;

        public PlotterIcon()
        {
            _floatHistory.Add(true);
            _floatHistory.Add(false);
            _longHistory.Add(true);
            _longHistory.Add(false);

            _plotter = new Plotter()
            {
                Size = this.Size,
                ShowGrid = false,
                BackColor = Color.Black,
                MoveStep = 2,
                Data1 = _floatHistory[true],
                Data2 = _floatHistory[false],
                LongData1 = _longHistory[true],
                LongData2 = _longHistory[false]
            };
        }

        public override void Dispose()
        {
            _plotter.Dispose();
            base.Dispose();
        }

        protected void Update(float v1, float v2)
        {
            _floatHistory.Update(true, v1);
            _floatHistory.Update(false, v2);
        }

        protected void Update(long v1, long v2)
        {
            _longHistory.Update(true, v1);
            _longHistory.Update(false, v2);
        }

        public void Redraw()
        {
            Icon newIcon;
            Icon oldIcon = this.Icon;

            using (Bitmap bm = new Bitmap(this.Size.Width, this.Size.Height))
            {

                if (_plotter.Size != this.Size)
                    _plotter.Size = this.Size;

                using (Graphics g = Graphics.FromImage(bm))
                    _plotter.Draw(g);

                newIcon = Icon.FromHandle(bm.GetHicon());
            }

            this.Icon = newIcon;
            ProcessHacker.Native.Api.Win32.DestroyIcon(oldIcon.Handle);
        }

        protected bool UseLongData
        {
            get { return _plotter.UseLongData; }
            set { _plotter.UseLongData = value; }
        }

        protected bool UseSecondLine
        {
            get { return _plotter.UseSecondLine; }
            set { _plotter.UseSecondLine = value; }
        }

        protected bool OverlaySecondLine
        {
            get { return _plotter.OverlaySecondLine; }
            set { _plotter.OverlaySecondLine = value; }
        }

        protected long MinMaxValue
        {
            get { return _plotter.MinMaxValue; }
            set { _plotter.MinMaxValue = value; }
        }

        protected Color BackColor
        {
            get { return _plotter.BackColor; }
            set { _plotter.BackColor = value; }
        }

        protected Color LineColor1
        {
            get { return _plotter.LineColor1; }
            set { _plotter.LineColor1 = value; }
        }

        protected Color LineColor2
        {
            get { return _plotter.LineColor2; }
            set { _plotter.LineColor2 = value; }
        }
    }
}
