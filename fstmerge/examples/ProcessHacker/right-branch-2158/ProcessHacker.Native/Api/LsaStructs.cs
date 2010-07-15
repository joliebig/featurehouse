

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;

namespace ProcessHacker.Native.Api
{
    [StructLayout(LayoutKind.Sequential)]
    public struct LsaReferencedDomainList
    {
        public int Entries;
        public IntPtr Domains;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct LsaTranslatedName
    {
        public SidNameUse Use;
        public UnicodeString Name;
        public int DomainIndex;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct LsaTranslatedSid
    {
        public SidNameUse Use;
        public int RelativeId;
        public int DomainIndex;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct LsaTranslatedSid2
    {
        public SidNameUse Use;
        public IntPtr Sid;
        public int DomainIndex;
        public int Flags;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct LsaTrustInformation
    {
        public UnicodeString Name;
        public IntPtr Sid;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct PolicyPrivilegeDefinition
    {
        public UnicodeString Name;
        public Luid LocalValue;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct QuotaLimits
    {
        public IntPtr PagedPoolLimit;
        public IntPtr NonPagedPoolLimit;
        public IntPtr MinimumWorkingSetSize;
        public IntPtr MaximumWorkingSetSize;
        public IntPtr PagefileLimit;
        public long TimeLimit;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct SecurityLogonSessionData
    {
        public int Size;
        public Luid LogonId;
        public UnicodeString UserName;
        public UnicodeString LogonDomain;
        public UnicodeString AuthenticationPackage;
        public LogonType LogonType;
        public int Session;
        public IntPtr Sid;
        public long LogonTime;
        public UnicodeString LogonServer;
        public UnicodeString DnsDomainName;
        public UnicodeString Upn;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct TrustedDomainInformationEx
    {
        public UnicodeString Name;
        public UnicodeString FlatName;
        public IntPtr Sid;
        public int TrustDirection;
        public int TrustType;
        public int TrustAttributes;
    }
}
