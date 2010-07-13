

using System;
using System.Collections.Generic;
using ProcessHacker.Common;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{



    public sealed class LsaPolicyHandle : LsaHandle<LsaPolicyAccess>
    {
        private static WeakReference<LsaPolicyHandle> _lookupPolicyHandle;
        private static int _lookupPolicyHandleMisses = 0;

        public static LsaPolicyHandle LookupPolicyHandle
        {
            get
            {
                WeakReference<LsaPolicyHandle> weakRef = _lookupPolicyHandle;
                LsaPolicyHandle policyHandle = null;

                if (weakRef != null)
                {
                    policyHandle = weakRef.Target;
                }

                if (policyHandle == null)
                {
                    System.Threading.Interlocked.Increment(ref _lookupPolicyHandleMisses);

                    policyHandle = new LsaPolicyHandle(LsaPolicyAccess.LookupNames);

                    if (policyHandle != null)
                        _lookupPolicyHandle = new WeakReference<LsaPolicyHandle>(policyHandle);
                }

                return policyHandle;
            }
        }

        public static int LookupPolicyHandleMisses
        {
            get { return _lookupPolicyHandleMisses; }
        }

        public delegate bool EnumAccountsDelegate(Sid sid);
        public delegate bool EnumPrivilegesDelegate(Privilege privilege);





        public LsaPolicyHandle(LsaPolicyAccess access)
            : this(null, access)
        { }






        public LsaPolicyHandle(string systemName, LsaPolicyAccess access)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes();
            UnicodeString systemNameStr;
            IntPtr handle;

            systemNameStr = new UnicodeString(systemName);

            try
            {
                if ((status = Win32.LsaOpenPolicy(
                    ref systemNameStr,
                    ref oa,
                    access,
                    out handle
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                systemNameStr.Dispose();
            }

            this.Handle = handle;
        }






        public void EnumAccounts(EnumAccountsDelegate callback)
        {
            NtStatus status;
            int enumerationContext = 0;
            IntPtr buffer;
            int count;

            while (true)
            {
                status = Win32.LsaEnumerateAccounts(
                    this,
                    ref enumerationContext,
                    out buffer,
                    0x100,
                    out count
                    );

                if (status == NtStatus.NoMoreEntries)
                    break;
                if (status >= NtStatus.Error)
                    Win32.ThrowLastError(status);

                using (var bufferAlloc = new LsaMemoryAlloc(buffer))
                {
                    for (int i = 0; i < count; i++)
                    {
                        if (!callback(new Sid(bufferAlloc.ReadIntPtr(0, i))))
                            return;
                    }
                }
            }
        }
        public void EnumAccountsWithPrivilege(string privilegeName, EnumAccountsDelegate callback)
        {
            NtStatus status;
            UnicodeString privilegeNameStr;
            IntPtr buffer;
            int count;
            privilegeNameStr = new UnicodeString(privilegeName);
            try
            {
                if ((status = Win32.LsaEnumerateAccountsWithUserRight(
                    this,
                    ref privilegeNameStr,
                    out buffer,
                    out count
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                privilegeNameStr.Dispose();
            }
            Sid[] sids = new Sid[count];
            using (var bufferAlloc = new LsaMemoryAlloc(buffer))
            {
                for (int i = 0; i < count; i++)
                {
                    if (!callback(new Sid(bufferAlloc.ReadIntPtr(0, i))))
                        break;
                }
            }
        }
        public void EnumPrivileges(EnumPrivilegesDelegate callback)
        {
            NtStatus status;
            int enumerationContext = 0;
            IntPtr buffer;
            int count;
            while (true)
            {
                status = Win32.LsaEnumeratePrivileges(
                    this,
                    ref enumerationContext,
                    out buffer,
                    0x100,
                    out count
                    );
                if (status == NtStatus.NoMoreEntries)
                    break;
                if (status >= NtStatus.Error)
                    Win32.ThrowLastError(status);
                using (var bufferAlloc = new LsaMemoryAlloc(buffer))
                {
                    for (int i = 0; i < count; i++)
                    {
                        if (!callback(new Privilege(bufferAlloc.ReadStruct<PolicyPrivilegeDefinition>(i).Name.Read())))
                            return;
                    }
                }
            }
        }
        public Sid[] GetAccounts()
        {
            List<Sid> sids = new List<Sid>();
            this.EnumAccounts((sid) =>
                {
                    sids.Add(sid);
                    return true;
                });
            return sids.ToArray();
        }
        public Sid[] GetAccountsWithPrivilege(string privilegeName)
        {
            List<Sid> sids = new List<Sid>();
            this.EnumAccountsWithPrivilege(privilegeName, (sid) =>
            {
                sids.Add(sid);
                return true;
            });
            return sids.ToArray();
        }
        public Privilege[] GetPrivileges()
        {
            List<Privilege> privileges = new List<Privilege>();
            this.EnumPrivileges((privilege) =>
            {
                privileges.Add(privilege);
                return true;
            });
            return privileges.ToArray();
        }
        public string LookupName(Sid sid)
        {
            SidNameUse nameUse;
            return this.LookupName(sid, out nameUse);
        }
        public string LookupName(Sid sid, out SidNameUse nameUse)
        {
            string domainName;
            return this.LookupName(sid, out nameUse, out domainName);
        }
        public string LookupName(Sid sid, out string domainName)
        {
            SidNameUse nameUse;
            return this.LookupName(sid, out nameUse, out domainName);
        }
        public string LookupName(Sid sid, out SidNameUse nameUse, out string domainName)
        {
            NtStatus status;
            IntPtr referencedDomains;
            IntPtr names;
            if ((status = Win32.LsaLookupSids(
                    this,
                    1,
                    new IntPtr[] { sid },
                    out referencedDomains,
                    out names
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            using (var referencedDomainsAlloc = new LsaMemoryAlloc(referencedDomains))
            using (var namesAlloc = new LsaMemoryAlloc(names))
            {
                LsaTranslatedName translatedName = namesAlloc.ReadStruct<LsaTranslatedName>();
                nameUse = translatedName.Use;
                if (nameUse == SidNameUse.Invalid || nameUse == SidNameUse.Unknown)
                {
                    domainName = null;
                    return null;
                }
                if (translatedName.DomainIndex != -1)
                {
                    LsaReferencedDomainList domains = referencedDomainsAlloc.ReadStruct<LsaReferencedDomainList>();
                    MemoryRegion trustArray = new MemoryRegion(domains.Domains);
                    LsaTrustInformation trustInfo = trustArray.ReadStruct<LsaTrustInformation>(translatedName.DomainIndex);
                    domainName = trustInfo.Name.Read();
                }
                else
                {
                    domainName = null;
                }
                return translatedName.Name.Read();
            }
        }
        public string LookupPrivilegeDisplayName(Luid value)
        {
            return this.LookupPrivilegeDisplayName(this.LookupPrivilegeName(value));
        }
        public string LookupPrivilegeDisplayName(string name)
        {
            NtStatus status;
            UnicodeString nameStr;
            IntPtr displayName;
            short language;
            nameStr = new UnicodeString(name);
            try
            {
                if ((status = Win32.LsaLookupPrivilegeDisplayName(
                    this,
                    ref nameStr,
                    out displayName,
                    out language
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                nameStr.Dispose();
            }
            using (var displayNameAlloc = new LsaMemoryAlloc(displayName))
            {
                return displayNameAlloc.ReadStruct<UnicodeString>().Read();
            }
        }
        public string LookupPrivilegeName(Luid value)
        {
            NtStatus status;
            IntPtr name;
            if ((status = Win32.LsaLookupPrivilegeName(
                this,
                ref value,
                out name
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            using (var nameAlloc = new LsaMemoryAlloc(name))
            {
                return nameAlloc.ReadStruct<UnicodeString>().Read();
            }
        }
        public Luid LookupPrivilegeValue(string name)
        {
            NtStatus status;
            UnicodeString nameStr;
            Luid luid;
            nameStr = new UnicodeString(name);
            try
            {
                if ((status = Win32.LsaLookupPrivilegeValue(
                    this,
                    ref nameStr,
                    out luid
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                nameStr.Dispose();
            }
            return luid;
        }
        public Sid LookupSid(string name)
        {
            SidNameUse nameUse;
            return this.LookupSid(name, out nameUse);
        }
        public Sid LookupSid(string name, out SidNameUse nameUse)
        {
            string domainName;
            return this.LookupSid(name, out nameUse, out domainName);
        }
        public Sid LookupSid(string name, out string domainName)
        {
            SidNameUse nameUse;
            return this.LookupSid(name, out nameUse, out domainName);
        }
        public Sid LookupSid(string name, out SidNameUse nameUse, out string domainName)
        {
            NtStatus status;
            UnicodeString nameStr;
            IntPtr referencedDomains;
            IntPtr sids;
            nameStr = new UnicodeString(name);
            try
            {
                if ((status = Win32.LsaLookupNames2(
                    this,
                    0,
                    1,
                    new UnicodeString[] { nameStr },
                    out referencedDomains,
                    out sids
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                nameStr.Dispose();
            }
            using (var referencedDomainsAlloc = new LsaMemoryAlloc(referencedDomains))
            using (var sidsAlloc = new LsaMemoryAlloc(sids))
            {
                LsaTranslatedSid2 translatedSid = sidsAlloc.ReadStruct<LsaTranslatedSid2>();
                nameUse = translatedSid.Use;
                if (nameUse == SidNameUse.Invalid || nameUse == SidNameUse.Unknown)
                {
                    domainName = null;
                    return null;
                }
                if (translatedSid.DomainIndex != -1)
                {
                    LsaReferencedDomainList domains = referencedDomainsAlloc.ReadStruct<LsaReferencedDomainList>();
                    MemoryRegion trustArray = new MemoryRegion(domains.Domains);
                    LsaTrustInformation trustInfo = trustArray.ReadStruct<LsaTrustInformation>(translatedSid.DomainIndex);
                    domainName = trustInfo.Name.Read();
                }
                else
                {
                    domainName = null;
                }
                return new Sid(translatedSid.Sid);
            }
        }
    }
}
