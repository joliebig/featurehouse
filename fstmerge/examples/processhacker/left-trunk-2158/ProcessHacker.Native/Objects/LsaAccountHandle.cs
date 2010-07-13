

using System;
using System.Collections.Generic;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{



    public sealed class LsaAccountHandle : LsaHandle<LsaAccountAccess>
    {
        public static LsaAccountHandle Create(LsaAccountAccess access, LsaPolicyHandle policyHandle, Sid sid)
        {
            NtStatus status;
            IntPtr handle;

            if ((status = Win32.LsaCreateAccount(
                policyHandle,
                sid,
                access,
                out handle
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return new LsaAccountHandle(handle, true);
        }

        private LsaAccountHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }







        public LsaAccountHandle(LsaPolicyHandle policyHandle, Sid sid, LsaAccountAccess access)
        {
            NtStatus status;
            IntPtr handle;

            if ((status = Win32.LsaOpenAccount(
                policyHandle,
                sid,
                access,
                out handle
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            this.Handle = handle;
        }

        public void AddPrivileges(PrivilegeSet privileges)
        {
            NtStatus status;

            using (var privilegeSetMemory = privileges.ToMemory())
            {
                if ((status = Win32.LsaAddPrivilegesToAccount(
                    this,
                    privilegeSetMemory
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
        }

        public PrivilegeSet GetPrivileges()
        {
            NtStatus status;
            IntPtr privileges;

            if ((status = Win32.LsaEnumeratePrivilegesOfAccount(
                this,
                out privileges
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            using (var privilegesAlloc = new LsaMemoryAlloc(privileges))
            {
                return new PrivilegeSet(privilegesAlloc);
            }
        }

        public QuotaLimits GetQuotas()
        {
            NtStatus status;
            QuotaLimits quotas;

            if ((status = Win32.LsaGetQuotasForAccount(
                this,
                out quotas
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return quotas;
        }

        public SecuritySystemAccess GetSystemAccess()
        {
            NtStatus status;
            SecuritySystemAccess access;

            if ((status = Win32.LsaGetSystemAccessAccount(
                this,
                out access
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return access;
        }

        public void RemovePrivileges()
        {
            NtStatus status;

            if ((status = Win32.LsaRemovePrivilegesFromAccount(
                this,
                true,
                IntPtr.Zero
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        private void RemovePrivileges(PrivilegeSet privileges)
        {
            NtStatus status;

            using (var privilegeSetMemory = privileges.ToMemory())
            {
                if ((status = Win32.LsaRemovePrivilegesFromAccount(
                    this,
                    false,
                    privilegeSetMemory
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
        }

        public void SetQuotas(QuotaLimits quotas)
        {
            NtStatus status;

            if ((status = Win32.LsaSetQuotasForAccount(
                this,
                ref quotas
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void SetSystemAccess(SecuritySystemAccess access)
        {
            NtStatus status;

            if ((status = Win32.LsaSetSystemAccessAccount(
                this,
                access
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
    }
}
