

using System;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security.AccessControl;

namespace ProcessHacker.Native.Objects
{
    public abstract class UserHandle<TAccess> : NativeHandle<TAccess>
        where TAccess : struct
    {
        protected UserHandle()
            : base()
        { }

        protected UserHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public override SecurityDescriptor GetSecurity(SecurityInformation securityInformation)
        {
            return this.GetSecurity(SeObjectType.WindowObject, securityInformation);
        }

        public override void SetSecurity(SecurityInformation securityInformation, SecurityDescriptor securityDescriptor)
        {
            this.SetSecurity(SeObjectType.WindowObject, securityInformation, securityDescriptor);
        }
    }
}
