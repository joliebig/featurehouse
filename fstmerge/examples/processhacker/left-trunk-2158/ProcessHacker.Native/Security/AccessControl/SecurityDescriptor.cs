

using System;
using ProcessHacker.Common.Objects;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;

namespace ProcessHacker.Native.Security.AccessControl
{



    public sealed class SecurityDescriptor : BaseObject
    {






        public static SecurityDescriptor GetSecurity(IntPtr handle, SecurityInformation securityInformation)
        {
            NtStatus status;
            int retLength;

            using (var data = new MemoryAlloc(0x100))
            {
                status = Win32.NtQuerySecurityObject(
                    handle,
                    securityInformation,
                    data,
                    data.Size,
                    out retLength
                    );

                if (status == NtStatus.BufferTooSmall)
                {
                    data.Resize(retLength);

                    status = Win32.NtQuerySecurityObject(
                        handle,
                        securityInformation,
                        data,
                        data.Size,
                        out retLength
                        );
                }

                if (status >= NtStatus.Error)
                    Win32.ThrowLastError(status);

                return new SecurityDescriptor(data);
            }
        }
        public static SecurityDescriptor GetSecurity(IntPtr handle, SeObjectType objectType, SecurityInformation securityInformation)
        {
            Win32Error result;
            IntPtr dummy, securityDescriptor;
            if ((result = Win32.GetSecurityInfo(
                handle,
                objectType,
                securityInformation,
                out dummy, out dummy, out dummy, out dummy,
                out securityDescriptor
                )) != 0)
                Win32.ThrowLastError(result);
            return new SecurityDescriptor(new LocalMemoryAlloc(securityDescriptor));
        }
        public static void SetSecurity(IntPtr handle, SecurityInformation securityInformation, SecurityDescriptor securityDescriptor)
        {
            NtStatus status;
            if ((status = Win32.NtSetSecurityObject(
                handle,
                securityInformation,
                securityDescriptor
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public static void SetSecurity(IntPtr handle, SeObjectType objectType, SecurityInformation securityInformation, SecurityDescriptor securityDescriptor)
        {
            Win32Error result;
            IntPtr dacl = IntPtr.Zero;
            IntPtr group = IntPtr.Zero;
            IntPtr owner = IntPtr.Zero;
            IntPtr sacl = IntPtr.Zero;
            if ((securityInformation & SecurityInformation.Dacl) == SecurityInformation.Dacl)
                dacl = securityDescriptor.Dacl ?? IntPtr.Zero;
            if ((securityInformation & SecurityInformation.Group) == SecurityInformation.Group)
                group = securityDescriptor.Group;
            if ((securityInformation & SecurityInformation.Owner) == SecurityInformation.Owner)
                owner = securityDescriptor.Owner;
            if ((securityInformation & SecurityInformation.Sacl) == SecurityInformation.Sacl)
                sacl = securityDescriptor.Sacl ?? IntPtr.Zero;
            if ((result = Win32.SetSecurityInfo(
                handle,
                objectType,
                securityInformation,
                owner,
                group,
                dacl,
                sacl
                )) != 0)
                Win32.ThrowLastError(result);
        }
        public static implicit operator IntPtr(SecurityDescriptor securityDescriptor)
        {
            return securityDescriptor.Memory;
        }
        private MemoryRegion _memory;
        private Acl _dacl;
        private Acl _sacl;
        private Sid _owner;
        private Sid _group;
        public SecurityDescriptor()
        {
            NtStatus status;
            _memory = new MemoryAlloc(Win32.SecurityDescriptorMinLength);
            if ((status = Win32.RtlCreateSecurityDescriptor(
                _memory,
                Win32.SecurityDescriptorRevision
                )) >= NtStatus.Error)
            {
                _memory.Dispose();
                _memory = null;
                this.DisableOwnership(false);
                Win32.ThrowLastError(status);
            }
            _memory.Reference();
            _memory.Dispose();
        }
        public SecurityDescriptor(Sid owner, Sid group, Acl dacl, Acl sacl)
            : this()
        {
            this.Owner = owner;
            this.Group = group;
            this.Dacl = dacl;
            this.Sacl = sacl;
        }
        public SecurityDescriptor(MemoryRegion memory)
        {
            _memory = memory;
            _memory.Reference();
            this.Read();
        }
        protected override void DisposeObject(bool disposing)
        {
            if (_dacl != null)
                _dacl.Dereference(disposing);
            if (_sacl != null)
                _sacl.Dereference(disposing);
            if (_owner != null)
                _owner.Dereference(disposing);
            if (_group != null)
                _group.Dereference(disposing);
            if (_memory != null)
                _memory.Dereference(disposing);
        }
        public SecurityDescriptorControlFlags ControlFlags
        {
            get
            {
                NtStatus status;
                SecurityDescriptorControlFlags control;
                int revision;
                if ((status = Win32.RtlGetControlSecurityDescriptor(
                    this,
                    out control,
                    out revision
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                return control;
            }
            set
            {
                NtStatus status;
                if ((status = Win32.RtlSetControlSecurityDescriptor(
                    this,
                    value,
                    value
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
        }
        public Acl Dacl
        {
            get { return _dacl; }
            set
            {
                NtStatus status;
                if ((status = Win32.RtlSetDaclSecurityDescriptor(
                    this,
                    value != null,
                    value ?? IntPtr.Zero,
                    false
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                this.SwapDacl(value);
            }
        }
        public bool DaclDefaulted
        {
            get
            {
                return (this.ControlFlags & SecurityDescriptorControlFlags.DaclDefaulted) ==
                    SecurityDescriptorControlFlags.DaclDefaulted;
            }
            set
            {
                if (value)
                    this.ControlFlags |= SecurityDescriptorControlFlags.DaclDefaulted;
                else
                    this.ControlFlags &= ~SecurityDescriptorControlFlags.DaclDefaulted;
            }
        }
        public Sid Group
        {
            get { return _group; }
            set
            {
                NtStatus status;
                if ((status = Win32.RtlSetGroupSecurityDescriptor(
                    this,
                    value,
                    false
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                this.SwapGroup(value);
            }
        }
        public bool GroupDefaulted
        {
            get
            {
                return (this.ControlFlags & SecurityDescriptorControlFlags.GroupDefaulted) ==
                    SecurityDescriptorControlFlags.GroupDefaulted;
            }
            set
            {
                if (value)
                    this.ControlFlags |= SecurityDescriptorControlFlags.GroupDefaulted;
                else
                    this.ControlFlags &= ~SecurityDescriptorControlFlags.GroupDefaulted;
            }
        }
        public int Length
        {
            get { return Win32.RtlLengthSecurityDescriptor(this); }
        }
        public IntPtr Memory
        {
            get { return _memory; }
        }
        public Sid Owner
        {
            get { return _owner; }
            set
            {
                NtStatus status;
                if ((status = Win32.RtlSetOwnerSecurityDescriptor(
                    this,
                    value,
                    false
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                this.SwapOwner(value);
            }
        }
        public bool OwnerDefaulted
        {
            get
            {
                return (this.ControlFlags & SecurityDescriptorControlFlags.OwnerDefaulted) ==
                    SecurityDescriptorControlFlags.OwnerDefaulted;
            }
            set
            {
                if (value)
                    this.ControlFlags |= SecurityDescriptorControlFlags.OwnerDefaulted;
                else
                    this.ControlFlags &= ~SecurityDescriptorControlFlags.OwnerDefaulted;
            }
        }
        public Acl Sacl
        {
            get { return _sacl; }
            set
            {
                NtStatus status;
                if ((status = Win32.RtlSetSaclSecurityDescriptor(
                    this,
                    value != null,
                    value ?? IntPtr.Zero,
                    false
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                this.SwapSacl(value);
            }
        }
        public bool SaclDefaulted
        {
            get
            {
                return (this.ControlFlags & SecurityDescriptorControlFlags.SaclDefaulted) ==
                    SecurityDescriptorControlFlags.SaclDefaulted;
            }
            set
            {
                if (value)
                    this.ControlFlags |= SecurityDescriptorControlFlags.SaclDefaulted;
                else
                    this.ControlFlags &= ~SecurityDescriptorControlFlags.SaclDefaulted;
            }
        }
        public bool SelfRelative
        {
            get
            {
                return (this.ControlFlags & SecurityDescriptorControlFlags.SelfRelative) ==
                    SecurityDescriptorControlFlags.SelfRelative;
            }
        }
        public NtStatus CheckAccess(TokenHandle tokenHandle, int desiredAccess, GenericMapping genericMapping, out int grantedAccess)
        {
            NtStatus status;
            NtStatus accessStatus;
            int privilegeSetLength = 0;
            if ((status = Win32.NtAccessCheck(
                this,
                tokenHandle,
                desiredAccess,
                ref genericMapping,
                IntPtr.Zero,
                ref privilegeSetLength,
                out grantedAccess,
                out accessStatus
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return accessStatus;
        }
        public bool IsValid()
        {
            return Win32.RtlValidSecurityDescriptor(this);
        }
        private void Read()
        {
            NtStatus status;
            bool present, defaulted;
            IntPtr dacl, group, owner, sacl;
            if ((status = Win32.RtlGetDaclSecurityDescriptor(
                this,
                out present,
                out dacl,
                out defaulted
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            if (present)
                this.SwapDacl(new Acl(Acl.FromPointer(dacl)));
            else
                this.SwapDacl(null);
            if ((status = Win32.RtlGetSaclSecurityDescriptor(
                this,
                out present,
                out sacl,
                out defaulted
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            if (present)
                this.SwapSacl(new Acl(Acl.FromPointer(sacl)));
            else
                this.SwapSacl(null);
            if ((status = Win32.RtlGetGroupSecurityDescriptor(
                this,
                out group,
                out defaulted
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            if (group != IntPtr.Zero)
                this.SwapGroup(new Sid(group));
            else
                this.SwapGroup(null);
            if ((status = Win32.RtlGetOwnerSecurityDescriptor(
                this,
                out owner,
                out defaulted
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            if (owner != IntPtr.Zero)
                this.SwapOwner(new Sid(owner));
            else
                this.SwapOwner(null);
        }
        private void SwapDacl(Acl dacl)
        {
            BaseObject.SwapRef<Acl>(ref _dacl, dacl);
        }
        private void SwapGroup(Sid group)
        {
            BaseObject.SwapRef<Sid>(ref _group, group);
        }
        private void SwapOwner(Sid owner)
        {
            BaseObject.SwapRef<Sid>(ref _owner, owner);
        }
        private void SwapSacl(Acl sacl)
        {
            BaseObject.SwapRef<Acl>(ref _sacl, sacl);
        }
        public SecurityDescriptor ToSelfRelative()
        {
            NtStatus status;
            int retLength;
            using (var data = new MemoryAlloc(Win32.SecurityDescriptorMinLength))
            {
                retLength = data.Size;
                status = Win32.RtlMakeSelfRelativeSD(this, data, ref retLength);
                if (status == NtStatus.BufferTooSmall)
                {
                    data.Resize(retLength);
                    status = Win32.RtlMakeSelfRelativeSD(this, data, ref retLength);
                }
                if (status >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                return new SecurityDescriptor(data);
            }
        }
    }
}
