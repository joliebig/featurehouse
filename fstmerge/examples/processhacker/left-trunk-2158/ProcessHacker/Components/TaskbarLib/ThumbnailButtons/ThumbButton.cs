

using System;
using System.Drawing;
using TaskbarLib.Interop;

namespace TaskbarLib
{



    public sealed class ThumbButton
    {
        private ThumbButtonManager _manager;

        internal ThumbButton(ThumbButtonManager manager, int id, Icon icon, string tooltip)
        {
            _manager = manager;

            Id = id;
            Icon = icon;
            Tooltip = tooltip;
        }





        public event EventHandler Click;




        public int Id { get; set; }




        public Icon Icon { get; set; }




        public string Tooltip { get; set; }

        internal ThumbnailButtonFlags Flags { get; set; }

        internal THUMBBUTTON Win32ThumbButton
        {
            get
            {
                THUMBBUTTON win32ThumbButton = new THUMBBUTTON();
                win32ThumbButton.iId = Id;
                win32ThumbButton.szTip = Tooltip;
                win32ThumbButton.hIcon = Icon.Handle;
                win32ThumbButton.dwFlags = Flags | ThumbnailButtonFlags.DISMISSONCLICK;

                win32ThumbButton.dwMask = ThumbnailButtonMask.Flags;
                if (Tooltip != null)
                    win32ThumbButton.dwMask |= ThumbnailButtonMask.Tooltip;
                if (Icon != null)
                    win32ThumbButton.dwMask |= ThumbnailButtonMask.Icon;

                return win32ThumbButton;
            }
        }




        public bool Visible
        {
            get
            {
                return (this.Flags & ThumbnailButtonFlags.HIDDEN) == 0;
            }
            set
            {
                if (value)
                {
                    this.Flags &= ~(ThumbnailButtonFlags.HIDDEN);
                }
                else
                {
                    this.Flags |= ThumbnailButtonFlags.HIDDEN;
                }
                _manager.RefreshThumbButtons();
            }
        }

        public bool NoIconBackground
        {
            get
            {
                return (this.Flags & ThumbnailButtonFlags.NOBACKGROUND) == 0;
            }
            set
            {
                if (value)
                {
                    this.Flags &= ~(ThumbnailButtonFlags.NOBACKGROUND);
                }
                else
                {
                    this.Flags |= ThumbnailButtonFlags.NOBACKGROUND;
                }
                _manager.RefreshThumbButtons();
            }
        }




        public bool Enabled
        {
            get
            {
                return (this.Flags & ThumbnailButtonFlags.DISABLED) == 0;
            }
            set
            {
                if (value)
                {
                    this.Flags &= ~(ThumbnailButtonFlags.DISABLED);
                }
                else
                {
                    this.Flags |= ThumbnailButtonFlags.DISABLED;
                }
                _manager.RefreshThumbButtons();
            }
        }

        internal void OnClick()
        {
            if (Click != null)
                Click(this, EventArgs.Empty);
        }
    }

}
