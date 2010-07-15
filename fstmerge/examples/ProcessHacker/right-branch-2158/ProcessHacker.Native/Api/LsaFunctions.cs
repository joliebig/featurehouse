

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Api
{
    public static partial class Win32
    {


        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaAddAccountRights(
            [In] IntPtr PolicyHandle,
            [In] IntPtr AccountSid,
            [In] UnicodeString[] UserRights,
            [In] int CountOfRights
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaAddPrivilegesToAccount(
            [In] IntPtr AccountHandle,
            [In] IntPtr Privileges
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaClearAuditLog(
            [In] IntPtr PolicyHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaClose(
            [In] IntPtr ObjectHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaChangePassword(
            [In] ref UnicodeString ServerName,
            [In] ref UnicodeString DomainName,
            [In] ref UnicodeString AccountName,
            [In] ref UnicodeString OldPassword,
            [In] ref UnicodeString NewPassword
            );

        [DllImport("secur32.dll")]
        public static extern NtStatus LsaConnectUntrusted(
            [Out] out IntPtr LsaHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaCreateAccount(
            [In] IntPtr PolicyHandle,
            [In] IntPtr AccountSid,
            [In] LsaAccountAccess DesiredAccess,
            [Out] out IntPtr AccountHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaCreateSecret(
            [In] IntPtr PolicyHandle,
            [In] ref UnicodeString SecretName,
            [In] LsaSecretAccess DesiredAccess,
            [Out] out IntPtr SecretHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaCreateTrustedDomain(
            [In] IntPtr PolicyHandle,
            [In] ref LsaTrustInformation TrustedDomainInformation,
            [In] LsaTrustedAccess DesiredAccess,
            [Out] out IntPtr TrustedDomainHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaDelete(
            [In] IntPtr ObjectHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaDeleteTrustedDomain(
            [In] IntPtr PolicyHandle,
            [In] IntPtr TrustedDomainSid
            );

        [DllImport("secur32.dll")]
        public static extern NtStatus LsaDeregisterLogonProcess(
            [In] IntPtr LsaHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaEnumerateAccounts(
            [In] IntPtr PolicyHandle,
            ref int EnumerationContext,
            [Out] out IntPtr Buffer,
            [In] int PreferredMaximumLength,
            [Out] out int CountReturned
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaEnumerateAccountsWithUserRight(
            [In] IntPtr PolicyHandle,
            [In] [Optional] ref UnicodeString UserRight,
            [Out] out IntPtr Buffer,
            [Out] out int CountReturned
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaEnumerateAccountRights(
            [In] IntPtr PolicyHandle,
            [In] IntPtr AccountSid,
            [Out] IntPtr UserRights,
            [Out] out int CountOfRights
            );

        [DllImport("secur32.dll")]
        public static extern NtStatus LsaEnumerateLogonSessions(
            [Out] out int LogonSessionCount,
            [Out] out IntPtr LogonSessionList
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaEnumeratePrivileges(
            [In] IntPtr PolicyHandle,
            ref int EnumerationContext,
            [Out] out IntPtr Buffer,
            [In] int PreferredMaximumLength,
            [Out] out int CountReturned
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaEnumeratePrivilegesOfAccount(
            [In] IntPtr AccountHandle,
            [Out] out IntPtr Privileges
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaEnumerateTrustedDomains(
            [In] IntPtr PolicyHandle,
            ref int EnumerationContext,
            [Out] out IntPtr Buffer,
            [In] int PreferredMaximumLength,
            [Out] out int CountReturned
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaEnumerateTrustedDomainsEx(
            [In] IntPtr PolicyHandle,
            ref int EnumerationContext,
            [Out] out IntPtr Buffer,
            [In] int PreferredMaximumLength,
            [Out] out int CountReturned
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaFreeMemory(
            [In] IntPtr Buffer
            );

        [DllImport("secur32.dll")]
        public static extern NtStatus LsaFreeReturnBuffer(
            [In] IntPtr Buffer
            );

        [DllImport("secur32.dll")]
        public static extern NtStatus LsaGetLogonSessionData(
            [In] ref Luid LogonId,
            [Out] out IntPtr LogonSessionData
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaGetQuotasForAccount(
            [In] IntPtr AccountHandle,
            [Out] out QuotaLimits QuotaLimits
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaGetRemoteUserName(
            [In] [Optional] ref UnicodeString SystemName,
            [Out] out IntPtr UserName,
            [Out] [Optional] out IntPtr DomainName
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaGetSystemAccessAccount(
            [In] IntPtr AccountHandle,
            [Out] out SecuritySystemAccess SystemAccess
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaGetUserName(
            [Out] out IntPtr UserName,
            [Out] [Optional] out IntPtr DomainName
            );

        [DllImport("secur32.dll")]
        public static extern NtStatus LsaLookupAuthenticationPackage(
            [In] IntPtr LsaHandle,
            [In] ref AnsiString PackageName,
            [Out] out int AuthenticationPackage
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaLookupNames(
            [In] IntPtr PolicyHandle,
            [In] int Count,
            [In] UnicodeString[] Names,
            [Out] out IntPtr ReferencedDomains,
            [Out] out IntPtr Sids
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaLookupNames2(
            [In] IntPtr PolicyHandle,
            [In] int Flags,
            [In] int Count,
            [In] UnicodeString[] Names,
            [Out] out IntPtr ReferencedDomains,
            [Out] out IntPtr Sids
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaLookupPrivilegeDisplayName(
            [In] IntPtr PolicyHandle,
            [In] ref UnicodeString Name,
            [Out] out IntPtr DisplayName,
            [Out] out short LanguageReturned
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaLookupPrivilegeName(
            [In] IntPtr PolicyHandle,
            [In] ref Luid Value,
            [Out] out IntPtr Name
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaLookupPrivilegeValue(
            [In] IntPtr PolicyHandle,
            [In] ref UnicodeString Name,
            [Out] out Luid Value
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaLookupSids(
            [In] IntPtr PolicyHandle,
            [In] int Count,
            [In] IntPtr[] Sids,
            [Out] out IntPtr ReferencedDomains,
            [Out] out IntPtr Names
            );

        [DllImport("advapi32.dll")]
        public static extern int LsaNtStatusToWinError(
            [In] NtStatus Status
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaOpenAccount(
            [In] IntPtr PolicyHandle,
            [In] IntPtr AccountSid,
            [In] LsaAccountAccess DesiredAccess,
            [Out] out IntPtr AccountHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaOpenPolicy(
            [In] [Optional] ref UnicodeString SystemName,
            [In] ref ObjectAttributes ObjectAttributes,
            [In] LsaPolicyAccess DesiredAccess,
            [Out] out IntPtr PolicyHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaOpenPolicySce(
            [In] [Optional] ref UnicodeString SystemName,
            [In] ref ObjectAttributes ObjectAttributes,
            [In] LsaPolicyAccess DesiredAccess,
            [Out] out IntPtr PolicyHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaOpenSecret(
            [In] IntPtr PolicyHandle,
            [In] ref UnicodeString SecretName,
            [In] LsaSecretAccess DesiredAccess,
            [Out] out IntPtr SecretHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaOpenTrustedDomain(
            [In] IntPtr PolicyHandle,
            [In] IntPtr TrustedDomainSid,
            [In] LsaTrustedAccess DesiredAccess,
            [Out] out IntPtr TrustedDomainHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaOpenTrustedDomainByName(
            [In] IntPtr PolicyHandle,
            [In] ref UnicodeString TrustedDomainName,
            [In] LsaTrustedAccess DesiredAccess,
            [Out] out IntPtr TrustedDomainHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaQueryDomainInformationPolicy(
            [In] IntPtr PolicyHandle,
            [In] PolicyDomainInformationClass InformationClass,
            [Out] out IntPtr Buffer
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaQueryInformationPolicy(
            [In] IntPtr PolicyHandle,
            [In] PolicyInformationClass InformationClass,
            [Out] out IntPtr Buffer
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaQueryInfoTrustedDomain(
            [In] IntPtr TrustedDomainHandle,
            [In] TrustedInformationClass InformationClass,
            [Out] out IntPtr Buffer
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaQuerySecret(
            [In] IntPtr SecretHandle,
            [Out] [Optional] out IntPtr CurrentValue,
            [Out] [Optional] out long CurrentValueSetTime,
            [Out] [Optional] out IntPtr OldValue,
            [Out] [Optional] out long OldValueSetTime
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaQuerySecurityObject(
            [In] IntPtr ObjectHandle,
            [In] SecurityInformation SecurityInformation,
            [Out] out IntPtr SecurityDescriptor
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaQueryTrustedDomainInfo(
            [In] IntPtr PolicyHandle,
            [In] IntPtr TrustedDomainSid,
            [In] TrustedInformationClass InformationClass,
            [Out] out IntPtr Buffer
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaQueryTrustedDomainInfoByName(
            [In] IntPtr PolicyHandle,
            [In] ref UnicodeString TrustedDomainName,
            [In] TrustedInformationClass InformationClass,
            [Out] out IntPtr Buffer
            );

        [DllImport("secur32.dll")]
        public static extern NtStatus LsaRegisterLogonProcess(
            [In] ref AnsiString LogonProcessName,
            [Out] out IntPtr LsaHandle,
            [Out] out LsaOperationalMode SecurityMode
            );

        [DllImport("secur32.dll")]
        public static extern NtStatus LsaRegisterPolicyChangeNotification(
            [In] PolicyNotificationInformationClass InformationClass,
            [In] IntPtr NotificationEventHandle
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaRemoveAccountRights(
            [In] IntPtr PolicyHandle,
            [In] IntPtr AccountSid,
            [In] bool AllRights,
            [In] UnicodeString[] UserRights,
            [In] int CountOfRights
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaRemovePrivilegesFromAccount(
            [In] IntPtr AccountHandle,
            [In] bool AllPrivileges,
            [In] [Optional] IntPtr Privileges
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaRetrievePrivateData(
            [In] IntPtr PolicyHandle,
            [In] ref UnicodeString KeyName,
            [Out] out IntPtr PrivateData
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaSetDomainInformationPolicy(
            [In] IntPtr PolicyHandle,
            [In] PolicyDomainInformationClass InformationClass,
            [In] [Optional] IntPtr Buffer
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaSetInformationPolicy(
            [In] IntPtr PolicyHandle,
            [In] PolicyInformationClass InformationClass,
            [In] IntPtr Buffer
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaSetInformationTrustedDomain(
            [In] IntPtr TrustedDomainHandle,
            [In] TrustedInformationClass InformationClass,
            [In] IntPtr Buffer
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaSetQuotasForAccount(
            [In] IntPtr AccountHandle,
            [In] ref QuotaLimits QuotaLimits
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaSetSecret(
            [In] IntPtr SecretHandle,
            [In] [Optional] ref UnicodeString CurrentValue,
            [In] [Optional] ref UnicodeString OldValue
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaSetSecurityObject(
            [In] IntPtr ObjectHandle,
            [In] SecurityInformation SecurityInformation,
            [In] IntPtr SecurityDescriptor
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaSetSystemAccessAccount(
            [In] IntPtr AccountHandle,
            [In] SecuritySystemAccess SystemAccess
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaSetTrustedDomainInformation(
            [In] IntPtr PolicyHandle,
            [In] IntPtr TrustedDomainSid,
            [In] TrustedInformationClass InformationClass,
            [In] IntPtr Buffer
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaSetTrustedDomainInfoByName(
            [In] IntPtr PolicyHandle,
            [In] ref UnicodeString TrustedDomainName,
            [In] TrustedInformationClass InformationClass,
            [In] IntPtr Buffer
            );

        [DllImport("advapi32.dll")]
        public static extern NtStatus LsaStorePrivateData(
            [In] IntPtr PolicyHandle,
            [In] ref UnicodeString KeyName,
            [In] [Optional] ref UnicodeString PrivateData
            );

        [DllImport("secur32.dll")]
        public static extern NtStatus LsaUnregisterPolicyChangeNotification(
            [In] PolicyNotificationInformationClass InformationClass,
            [In] IntPtr NotificationEventHandle
            );
    }
}
