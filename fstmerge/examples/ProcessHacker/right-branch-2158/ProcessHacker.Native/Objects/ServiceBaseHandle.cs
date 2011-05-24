

using ProcessHacker.Native.Api;
using System;

namespace ProcessHacker.Native.Objects
{



    public class ServiceBaseHandle<TAccess> : NativeHandle<TAccess>
        where TAccess : struct
    {
        public ServiceBaseHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        protected ServiceBaseHandle()
        { }

        protected override void Close()
        {
            Win32.CloseServiceHandle(this);
        }
    }
}
