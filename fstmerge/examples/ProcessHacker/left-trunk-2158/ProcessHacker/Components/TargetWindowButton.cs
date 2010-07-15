

using System;
using System.Windows.Forms;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Components
{
    public delegate void TargetWindowFoundDelegate(int pid, int tid);

    public class TargetWindowButton : ToolStripButton
    {
        public event TargetWindowFoundDelegate TargetWindowFound;

        private Control _parent;
        private Control _dummy;
        private IntPtr _currentHWnd;
        private bool _targeting = false;

        public TargetWindowButton()
        {
            this.Image = Properties.Resources.application;
            this.DisplayStyle = ToolStripItemDisplayStyle.Image;
            this.Text = "Find Window";
            this.ToolTipText = "Find Window";

            _dummy = new Control();
            _dummy.MouseMove += dummy_MouseMove;
            _dummy.MouseUp += dummy_MouseUp;
        }

        private Form FindParentForm(Control c)
        {
            if (c == null)
                return null;
            if (c is Form)
                return c as Form;

            return this.FindParentForm(c.Parent);
        }

        private void DrawWindowRectangle(IntPtr hWnd)
        {
            Rect rect;

            Win32.GetWindowRect(hWnd, out rect);

            IntPtr windowDc = Win32.GetWindowDC(hWnd);

            if (windowDc != IntPtr.Zero)
            {

                int penWidth = Win32.GetSystemMetrics(5) * 3;

                int oldDc = Win32.SaveDC(windowDc);

                Win32.SetROP2(windowDc, GdiBlendMode.Not);


                IntPtr pen = Win32.CreatePen(GdiPenStyle.InsideFrame, penWidth, IntPtr.Zero);
                Win32.SelectObject(windowDc, pen);

                IntPtr brush = Win32.GetStockObject(GdiStockObject.NullBrush);
                Win32.SelectObject(windowDc, brush);

                Win32.Rectangle(windowDc, 0, 0, rect.Right - rect.Left, rect.Bottom - rect.Top);


                Win32.DeleteObject(pen);

                Win32.RestoreDC(windowDc, oldDc);
                Win32.ReleaseDC(hWnd, windowDc);
            }
        }

        private void RedrawWindow(IntPtr hWnd)
        {
            this.RedrawWindow(hWnd, true);
        }

        private void RedrawWindow(IntPtr hWnd, bool workaround)
        {
            if (!Win32.RedrawWindow(
               hWnd,
               IntPtr.Zero,
               IntPtr.Zero,
               RedrawWindowFlags.Invalidate |
               RedrawWindowFlags.Erase |
               RedrawWindowFlags.UpdateNow |
               RedrawWindowFlags.AllChildren |
               RedrawWindowFlags.Frame
               ) && workaround)
            {

                DrawWindowRectangle(hWnd);
            }
        }

        protected override void OnParentChanged(ToolStrip oldParent, ToolStrip newParent)
        {
            _parent = newParent;
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {

            Win32.SetCapture(_dummy.Handle);
            _targeting = true;
            this.FindParentForm(_parent).SendToBack();

            dummy_MouseMove(null, null);
        }

        protected override void OnClick(EventArgs e)
        {

            dummy_MouseUp(null, null);
        }

        void dummy_MouseMove(object sender, MouseEventArgs e)
        {
            if (!_targeting)
                return;

            IntPtr oldHWnd = _currentHWnd;


            _currentHWnd = Win32.WindowFromPoint(Control.MousePosition);


            if (_currentHWnd == oldHWnd)
                return;


            if (oldHWnd != IntPtr.Zero)
                this.RedrawWindow(oldHWnd);

            bool isPhWindow = false;
            int pid, tid;

            tid = Win32.GetWindowThreadProcessId(_currentHWnd, out pid);
            isPhWindow = pid == Program.CurrentProcessId;


            if (
                _currentHWnd != IntPtr.Zero &&
                !isPhWindow
                )
                this.DrawWindowRectangle(_currentHWnd);
        }

        void dummy_MouseUp(object sender, MouseEventArgs e)
        {
            this.FindParentForm(_parent).BringToFront();
            _targeting = false;
            Win32.ReleaseCapture();

            if (_currentHWnd != IntPtr.Zero)
            {

                this.RedrawWindow(_currentHWnd, false);

                int pid, tid;

                tid = Win32.GetWindowThreadProcessId(_currentHWnd, out pid);

                if (this.TargetWindowFound != null)
                    this.TargetWindowFound(pid, tid);
            }
        }
    }
}
