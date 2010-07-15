

using System;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security.AccessControl;

namespace ProcessHacker.Native.Objects
{



    public class LsaHandle<TAccess> : NativeHandle<TAccess>
        where TAccess : struct
    {
        protected LsaHandle()
        { }

        protected LsaHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        protected override void Close()
        {
            Win32.LsaClose(this);
        }

        public void Delete()
        {
            NtStatus status;

            if ((status = Win32.LsaDelete(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public override SecurityDescriptor GetSecurity(SecurityInformation securityInformation)
        {
            NtStatus status;
            IntPtr securityDescriptor;

            if ((status = Win32.LsaQuerySecurityObject(
                this,
                securityInformation,
                out securityDescriptor
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return new SecurityDescriptor(new LsaMemoryAlloc(securityDescriptor));
        }

        public override void SetSecurity(SecurityInformation securityInformation, SecurityDescriptor securityDescriptor)
        {
            NtStatus status;

            if ((status = Win32.LsaSetSecurityObject(
                this,
                securityInformation,
                securityDescriptor
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
    }
}
