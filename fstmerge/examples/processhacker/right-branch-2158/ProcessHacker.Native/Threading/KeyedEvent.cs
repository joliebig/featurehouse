

using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Threading
{



    public sealed class KeyedEvent : NativeObject<KeyedEventHandle>
    {



        public KeyedEvent()
            : this(null)
        { }
        public KeyedEvent(string name)
        {
            this.Handle = KeyedEventHandle.Create(
                KeyedEventAccess.All,
                name,
                ObjectFlags.OpenIf,
                null
                );
        }
        public void ReleaseKey(int key)
        {
            this.Handle.ReleaseKey(new IntPtr(key), false, long.MinValue, false);
        }
        public void ReleaseKey(int key, int timeout)
        {
            this.Handle.ReleaseKey(new IntPtr(key), false, timeout * Win32.TimeMsTo100Ns, true);
        }
        public void ReleaseKey(int key, DateTime timeout)
        {
            this.Handle.ReleaseKey(new IntPtr(key), false, timeout.ToFileTime(), false);
        }
        public void WaitKey(int key)
        {
            this.Handle.WaitKey(new IntPtr(key), false, long.MinValue, false);
        }
        public void WaitKey(int key, int timeout)
        {
            this.Handle.WaitKey(new IntPtr(key), false, timeout * Win32.TimeMsTo100Ns, true);
        }
        public void WaitKey(int key, DateTime timeout)
        {
            this.Handle.WaitKey(new IntPtr(key), false, timeout.ToFileTime(), false);
        }
    }
}
