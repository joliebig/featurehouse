

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;
using TaskbarLib.Interop;

namespace TaskbarLib
{



    public sealed class ThumbButtonManager : IDisposable
    {
        private sealed class MessageFilter : IMessageFilter
        {
            private ThumbButtonManager _manager;

            public MessageFilter(ThumbButtonManager manager)
            {
                _manager = manager;
            }

            public bool PreFilterMessage(ref Message m)
            {
                if (m.Msg == (int)Windows7Taskbar.TaskbarButtonCreatedMessage)
                {
                    _manager.OnTaskbarButtonCreated();
                    return true;
                }
                else if (m.Msg == (int)ProcessHacker.Native.Api.WindowMessage.Command)
                {
                    _manager.OnCommand(m.WParam);
                }

                return false;
            }
        }

        public event EventHandler TaskbarButtonCreated;

        private Form _form;
        private MessageFilter _filter;
        private bool _disposed;





        public ThumbButtonManager(Form form)
        {
            _form = form;

            Application.AddMessageFilter(_filter = new MessageFilter(this));
        }

        public void Dispose()
        {
            if (!_disposed)
            {
                Application.RemoveMessageFilter(_filter);
                _disposed = true;
            }
        }
        public ThumbButton CreateThumbButton(int id, Icon icon, string tooltip)
        {
            return new ThumbButton(this, id, icon, tooltip);
        }
        public void AddThumbButtons(params ThumbButton[] buttons)
        {
            Array.ForEach(buttons, b => _thumbButtons.Add(b.Id, b));
            RefreshThumbButtons();
        }
        public ThumbButton this[int id]
        {
            get
            {
                return _thumbButtons[id];
            }
        }
        internal void OnCommand(IntPtr wParam)
        {
            if (((wParam.ToInt32() >> 16) & 0xffff) == SafeNativeMethods.THBN_CLICKED)
            {
                _thumbButtons[wParam.ToInt32() & 0xffff].OnClick();
            }
        }
        internal void OnTaskbarButtonCreated()
        {
            if (this.TaskbarButtonCreated != null)
                this.TaskbarButtonCreated(this, new EventArgs());
            _buttonsLoaded = false;
            this.RefreshThumbButtons();
        }
        private bool _buttonsLoaded;
        internal void RefreshThumbButtons()
        {
            THUMBBUTTON[] win32Buttons = (from thumbButton in _thumbButtons.Values select thumbButton.Win32ThumbButton).ToArray();
            if (_buttonsLoaded)
            {
                Windows7Taskbar.TaskbarList.ThumbBarUpdateButtons(_form.Handle, win32Buttons.Length, win32Buttons);
            }
            else
            {
                Windows7Taskbar.TaskbarList.ThumbBarAddButtons(_form.Handle, win32Buttons.Length, win32Buttons);
                _buttonsLoaded = true;
            }
        }
        private Dictionary<int, ThumbButton> _thumbButtons = new Dictionary<int, ThumbButton>();
    }
}
