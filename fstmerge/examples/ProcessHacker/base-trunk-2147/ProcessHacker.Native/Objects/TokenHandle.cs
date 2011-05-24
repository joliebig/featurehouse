

using System;
using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;
using ProcessHacker.Native.Security.AccessControl;

namespace ProcessHacker.Native.Objects
{



    public sealed class TokenHandle : NativeHandle<TokenAccess>, IEquatable<TokenHandle>
    {
        private static readonly TokenSource _phTokenSource = new TokenSource("PROCHACK", Luid.Allocate());

        public static TokenHandle Create(
            TokenAccess access,
            TokenType tokenType,
            Sid user,
            Sid[] groups,
            PrivilegeSet privileges
            )
        {
            using (var administratorsSid = Sid.GetWellKnownSid(WellKnownSidType.WinBuiltinAdministratorsSid))
            using (var thandle = TokenHandle.OpenCurrentPrimary(TokenAccess.Query))
                return Create(access, 0, thandle, tokenType, user, groups, privileges, administratorsSid, administratorsSid);
        }

        public static TokenHandle Create(
            TokenAccess access,
            ObjectFlags objectFlags,
            TokenHandle existingTokenHandle,
            TokenType tokenType,
            Sid user,
            Sid[] groups,
            PrivilegeSet privileges,
            Sid owner,
            Sid primaryGroup
            )
        {
            var statistics = existingTokenHandle.GetStatistics();

            return Create(
                access,
                null,
                objectFlags,
                null,
                tokenType,
                statistics.AuthenticationId,
                statistics.ExpirationTime,
                user,
                groups,
                privileges,
                owner,
                primaryGroup,
                null,
                _phTokenSource
                );
        }

        public static TokenHandle Create(
            TokenAccess access,
            string name,
            ObjectFlags objectFlags,
            DirectoryHandle rootDirectory,
            TokenType tokenType,
            Luid authenticationId,
            long expirationTime,
            Sid user,
            Sid[] groups,
            PrivilegeSet privileges,
            Sid owner,
            Sid primaryGroup,
            Acl defaultDacl,
            TokenSource source
            )
        {
            NtStatus status;
            TokenUser tokenUser = new TokenUser(user);
            TokenGroups tokenGroups = new TokenGroups(groups);
            TokenPrivileges tokenPrivileges = new TokenPrivileges(privileges);
            TokenOwner tokenOwner = new TokenOwner(owner);
            TokenPrimaryGroup tokenPrimaryGroup = new TokenPrimaryGroup(primaryGroup);
            TokenDefaultDacl tokenDefaultDacl = new TokenDefaultDacl(defaultDacl);
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateToken(
                    out handle,
                    access,
                    ref oa,
                    tokenType,
                    ref authenticationId,
                    ref expirationTime,
                    ref tokenUser,
                    ref tokenGroups,
                    ref tokenPrivileges,
                    ref tokenOwner,
                    ref tokenPrimaryGroup,
                    ref tokenDefaultDacl,
                    ref source
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new TokenHandle(handle, true);
        }







        public static TokenHandle FromHandle(IntPtr handle)
        {
            return new TokenHandle(handle, false);
        }

        public static TokenHandle Logon(string username, string domain, string password, LogonType logonType, LogonProvider logonProvider)
        {
            IntPtr token;

            if (!Win32.LogonUser(username, domain, password, logonType, logonProvider, out token))
                Win32.ThrowLastError();

            return new TokenHandle(token, true);
        }

        public static TokenHandle OpenCurrent(TokenAccess access)
        {
            return new TokenHandle(ThreadHandle.GetCurrent(), access, false);
        }

        public static TokenHandle OpenCurrentPrimary(TokenAccess access)
        {
            return new TokenHandle(ProcessHandle.Current, access);
        }

        public static TokenHandle OpenSelf(TokenAccess access)
        {
            return new TokenHandle(ThreadHandle.GetCurrent(), access, true);
        }

        public static TokenHandle OpenSystemToken(TokenAccess access)
        {
            using (var phandle = new ProcessHandle(4, OSVersion.MinProcessQueryInfoAccess))
            {
                return phandle.GetToken(access);
            }
        }

        public static TokenHandle OpenSystemToken(TokenAccess access, SecurityImpersonationLevel impersonationLevel, TokenType type)
        {
            using (var phandle = new ProcessHandle(4, OSVersion.MinProcessQueryInfoAccess))
            {
                using (var thandle = phandle.GetToken(TokenAccess.Duplicate | access))
                {
                    return thandle.Duplicate(access, impersonationLevel, type);
                }
            }
        }

        public TokenHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }






        public TokenHandle(ProcessHandle handle, TokenAccess access)
        {
            IntPtr h;

            if (KProcessHacker.Instance != null)
            {
                h = new IntPtr(KProcessHacker.Instance.KphOpenProcessToken(handle, access));
            }
            else
            {
                if (!Win32.OpenProcessToken(handle, access, out h))
                {
                    this.MarkAsInvalid();
                    Win32.ThrowLastError();
                }
            }

            this.Handle = h;
        }






        public TokenHandle(ThreadHandle handle, TokenAccess access)
            : this(handle, access, false)
        { }







        public TokenHandle(ThreadHandle handle, TokenAccess access, bool openAsSelf)
        {
            IntPtr h;

            if (!Win32.OpenThreadToken(handle, access, openAsSelf, out h))
            {
                this.MarkAsInvalid();
                Win32.ThrowLastError();
            }

            this.Handle = h;
        }

        public void AdjustGroups(Sid[] groups)
        {
            TokenGroups tokenGroups = new TokenGroups();

            tokenGroups.GroupCount = groups.Length;
            tokenGroups.Groups = new SidAndAttributes[groups.Length];

            for (int i = 0; i < groups.Length; i++)
                tokenGroups.Groups[i] = groups[i].ToSidAndAttributes();

            if (!Win32.AdjustTokenGroups(this, false, ref tokenGroups, 0, IntPtr.Zero, IntPtr.Zero))
                Win32.ThrowLastError();
        }

        public void AdjustPrivileges(PrivilegeSet privileges)
        {
            var tokenPrivileges = privileges.ToTokenPrivileges();

            Win32.AdjustTokenPrivileges(this, false, ref tokenPrivileges, 0, IntPtr.Zero, IntPtr.Zero);

            if (Marshal.GetLastWin32Error() != 0)
                Win32.ThrowLastError();
        }

        public bool CheckPrivileges(PrivilegeSet privileges)
        {
            NtStatus status;
            bool result;

            using (var privilegesMemory = privileges.ToMemory())
            {
                if ((status = Win32.NtPrivilegeCheck(
                    this,
                    privilegesMemory,
                    out result
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);

                return result;
            }
        }
        public TokenHandle Duplicate(TokenAccess access, SecurityImpersonationLevel impersonationLevel, TokenType type)
        {
            IntPtr token;
            if (!Win32.DuplicateTokenEx(this, access, IntPtr.Zero, impersonationLevel, type, out token))
                Win32.ThrowLastError();
            return new TokenHandle(token, true);
        }
        public bool Equals(TokenHandle other)
        {
            NtStatus status;
            bool equal;
            if ((status = Win32.NtCompareTokens(
                this,
                other,
                out equal
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return equal;
        }
        public TokenElevationType GetElevationType()
        {
            return (TokenElevationType)this.GetInformationInt32(TokenInformationClass.TokenElevationType);
        }
        public Sid[] GetGroups()
        {
            return this.GetGroupsInternal(TokenInformationClass.TokenGroups);
        }
        private Sid[] GetGroupsInternal(TokenInformationClass infoClass)
        {
            int retLen = 0;
            Win32.GetTokenInformation(this, infoClass, IntPtr.Zero, 0, out retLen);
            using (MemoryAlloc data = new MemoryAlloc(retLen))
            {
                if (!Win32.GetTokenInformation(this, infoClass, data,
                    data.Size, out retLen))
                    Win32.ThrowLastError();
                int count = data.ReadStruct<TokenGroups>().GroupCount;
                Sid[] sids = new Sid[count];
                for (int i = 0; i < count; i++)
                {
                    var saa = data.ReadStruct<SidAndAttributes>(TokenGroups.GroupsOffset, i);
                    sids[i] = new Sid(saa.Sid, saa.Attributes);
                }
                return sids;
            }
        }
        private int GetInformationInt32(TokenInformationClass infoClass)
        {
            int value;
            int retLen;
            if (!Win32.GetTokenInformation(this, infoClass, out value, sizeof(int), out retLen))
                Win32.ThrowLastError();
            return value;
        }
        public Sid GetOwner()
        {
            int retLen;
            Win32.GetTokenInformation(this, TokenInformationClass.TokenOwner, IntPtr.Zero, 0, out retLen);
            using (MemoryAlloc data = new MemoryAlloc(retLen))
            {
                if (!Win32.GetTokenInformation(this, TokenInformationClass.TokenOwner, data,
                    data.Size, out retLen))
                    Win32.ThrowLastError();
                return new Sid(data.ReadIntPtr(0));
            }
        }
        public Sid GetPrimaryGroup()
        {
            int retLen;
            Win32.GetTokenInformation(this, TokenInformationClass.TokenPrimaryGroup, IntPtr.Zero, 0, out retLen);
            using (MemoryAlloc data = new MemoryAlloc(retLen))
            {
                if (!Win32.GetTokenInformation(this, TokenInformationClass.TokenPrimaryGroup, data,
                    data.Size, out retLen))
                    Win32.ThrowLastError();
                return new Sid(data.ReadIntPtr(0));
            }
        }
        public Privilege[] GetPrivileges()
        {
            int retLen;
            Win32.GetTokenInformation(this, TokenInformationClass.TokenPrivileges, IntPtr.Zero, 0, out retLen);
            using (MemoryAlloc data = new MemoryAlloc(retLen))
            {
                if (!Win32.GetTokenInformation(this, TokenInformationClass.TokenPrivileges, data,
                    data.Size, out retLen))
                    Win32.ThrowLastError();
                uint count = data.ReadUInt32(0);
                Privilege[] privileges = new Privilege[count];
                for (int i = 0; i < count; i++)
                {
                    var laa = data.ReadStruct<LuidAndAttributes>(sizeof(int), i);
                    privileges[i] = new Privilege(this, laa.Luid, laa.Attributes);
                }
                return privileges;
            }
        }
        public Sid[] GetRestrictingGroups()
        {
            return this.GetGroupsInternal(TokenInformationClass.TokenRestrictedSids);
        }
        public int GetSessionId()
        {
            return this.GetInformationInt32(TokenInformationClass.TokenSessionId);
        }
        public TokenSource GetSource()
        {
            TokenSource source;
            int retLen;
            if (!Win32.GetTokenInformation(this, TokenInformationClass.TokenSource,
                 out source, Marshal.SizeOf(typeof(TokenSource)), out retLen))
                Win32.ThrowLastError();
            return source;
        }
        public TokenStatistics GetStatistics()
        {
            TokenStatistics statistics;
            int retLen;
            if (!Win32.GetTokenInformation(this, TokenInformationClass.TokenStatistics,
                out statistics, Marshal.SizeOf(typeof(TokenStatistics)), out retLen))
                Win32.ThrowLastError();
            return statistics;
        }
        public Sid GetUser()
        {
            int retLen;
            Win32.GetTokenInformation(this, TokenInformationClass.TokenUser, IntPtr.Zero, 0, out retLen);
            using (MemoryAlloc data = new MemoryAlloc(retLen))
            {
                if (!Win32.GetTokenInformation(this.Handle, TokenInformationClass.TokenUser, data,
                    data.Size, out retLen))
                    Win32.ThrowLastError();
                TokenUser user = data.ReadStruct<TokenUser>();
                return new Sid(user.User.Sid, user.User.Attributes);
            }
        }
        public bool IsElevated()
        {
            return this.GetInformationInt32(TokenInformationClass.TokenElevation) != 0;
        }
        public bool IsVirtualizationAllowed()
        {
            return this.GetInformationInt32(TokenInformationClass.TokenVirtualizationAllowed) != 0;
        }
        public bool IsVirtualizationEnabled()
        {
            return this.GetInformationInt32(TokenInformationClass.TokenVirtualizationEnabled) != 0;
        }
        public void SetPrivilege(string privilegeName, SePrivilegeAttributes attributes)
        {
            Luid privilegeLuid;
            if (!Win32.LookupPrivilegeValue(null, privilegeName, out privilegeLuid))
                throw new Exception("Invalid privilege name '" + privilegeName + "'.");
            this.SetPrivilege(privilegeLuid, attributes);
        }
        public void SetPrivilege(Luid privilegeLuid, SePrivilegeAttributes attributes)
        {
            TokenPrivileges tkp = new TokenPrivileges();
            tkp.Privileges = new LuidAndAttributes[1];
            tkp.PrivilegeCount = 1;
            tkp.Privileges[0].Attributes = attributes;
            tkp.Privileges[0].Luid = privilegeLuid;
            Win32.AdjustTokenPrivileges(this, false, ref tkp, 0, IntPtr.Zero, IntPtr.Zero);
            if (Marshal.GetLastWin32Error() != 0)
                Win32.ThrowLastError();
        }
        public void SetVirtualizationEnabled(bool enabled)
        {
            int value = enabled ? 1 : 0;
            if (!Win32.SetTokenInformation(this, TokenInformationClass.TokenVirtualizationEnabled, ref value, 4))
            {
                Win32.ThrowLastError();
            }
        }
    }
}
