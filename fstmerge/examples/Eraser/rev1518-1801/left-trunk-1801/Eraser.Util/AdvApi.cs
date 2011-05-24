

using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using System.Security.Principal;
using System.Runtime.InteropServices;

namespace Eraser.Util
{
 public static class AdvApi
 {






  public static bool IsAdministrator()
  {
   WindowsPrincipal principal = new WindowsPrincipal(WindowsIdentity.GetCurrent());
   return principal.IsInRole(WindowsBuiltInRole.Administrator);
  }






  public static bool UacEnabled()
  {

   if (Environment.OSVersion.Platform != PlatformID.Win32NT ||
    Environment.OSVersion.Version < new Version(6, 0))
   {

    return false;
   }


   SafeTokenHandle hToken = new SafeTokenHandle();
   bool result = NativeMethods.OpenProcessToken(KernelApi.NativeMethods.GetCurrentProcess(),
    NativeMethods.TOKEN_QUERY, out hToken);
   if (!result || hToken.IsInvalid)
    throw KernelApi.GetExceptionForWin32Error(Marshal.GetLastWin32Error());

   IntPtr pElevationType = Marshal.AllocHGlobal(Marshal.SizeOf(
    typeof(NativeMethods.TOKEN_ELEVATION_TYPE)));
   try
   {

    uint returnSize = 0;
    result = NativeMethods.GetTokenInformation(hToken,
     NativeMethods.TOKEN_INFORMATION_CLASS.TokenElevationType,
     pElevationType, sizeof(NativeMethods.TOKEN_ELEVATION_TYPE),
     out returnSize);


    if (!result)
     throw KernelApi.GetExceptionForWin32Error(Marshal.GetLastWin32Error());

    NativeMethods.TOKEN_ELEVATION_TYPE elevationType =
     (NativeMethods.TOKEN_ELEVATION_TYPE)Marshal.PtrToStructure(
      pElevationType, typeof(NativeMethods.TOKEN_ELEVATION_TYPE));
    return elevationType != NativeMethods.TOKEN_ELEVATION_TYPE.TokenElevationTypeDefault;
   }
   finally
   {
    Marshal.FreeHGlobal(pElevationType);
   }
  }




  internal static class NativeMethods
  {
   [DllImport("Advapi32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool CryptAcquireContext(out SafeCryptHandle phProv,
    string pszContainer, string pszProvider, uint dwProvType, uint dwFlags);
   [DllImport("Advapi32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool CryptGenRandom(SafeCryptHandle hProv, uint dwLen,
    byte[] pbBuffer);
   [DllImport("Advapi32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool CryptReleaseContext(IntPtr hProv, uint dwFlags);
   public const uint PROV_RSA_FULL = 1;
   public const uint PROV_RSA_SIG = 2;
   public const uint PROV_DSS = 3;
   public const uint PROV_FORTEZZA = 4;
   public const uint PROV_MS_EXCHANGE = 5;
   public const uint PROV_SSL = 6;
   public const uint PROV_RSA_SCHANNEL = 12;
   public const uint PROV_DSS_DH = 13;
   public const uint PROV_EC_ECDSA_SIG = 14;
   public const uint PROV_EC_ECNRA_SIG = 15;
   public const uint PROV_EC_ECDSA_FULL = 16;
   public const uint PROV_EC_ECNRA_FULL = 17;
   public const uint PROV_DH_SCHANNEL = 18;
   public const uint PROV_SPYRUS_LYNKS = 20;
   public const uint PROV_RNG = 21;
   public const uint PROV_INTEL_SEC = 22;
   public const int NTE_BAD_KEYSET = unchecked((int)0x80090016);
   public const uint CRYPT_VERIFYCONTEXT = 0xF0000000;
   public const uint CRYPT_NEWKEYSET = 0x00000008;
   public const uint CRYPT_DELETEKEYSET = 0x00000010;
   public const uint CRYPT_MACHINE_KEYSET = 0x00000020;
   public const uint CRYPT_SILENT = 0x00000040;
   [DllImport("Advapi32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool GetTokenInformation(SafeTokenHandle TokenHandle,
    TOKEN_INFORMATION_CLASS TokenInformationClass, IntPtr TokenInformation,
    uint TokenInformationLength, out uint ReturnLength);
   [DllImport("Advapi32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool OpenProcessToken(IntPtr ProcessHandle,
    UInt32 DesiredAccess, out SafeTokenHandle TokenHandle);
   public const uint STANDARD_RIGHTS_REQUIRED = 0xF0000;
   public const uint TOKEN_ASSIGN_PRIMARY = 0x00001;
   public const uint TOKEN_DUPLICATE = 0x00002;
   public const uint TOKEN_IMPERSONATE = 0x00004;
   public const uint TOKEN_QUERY = 0x00008;
   public const uint TOKEN_QUERY_SOURCE = 0x00010;
   public const uint TOKEN_ADJUST_PRIVILEGES = 0x00020;
   public const uint TOKEN_ADJUST_GROUPS = 0x00040;
   public const uint TOKEN_ADJUST_DEFAULT = 0x00080;
   public const uint TOKEN_ADJUST_SESSIONID = 0x00100;
   public const uint TOKEN_ALL_ACCESS_P = (STANDARD_RIGHTS_REQUIRED |
    TOKEN_ASSIGN_PRIMARY | TOKEN_DUPLICATE | TOKEN_IMPERSONATE | TOKEN_QUERY |
    TOKEN_QUERY_SOURCE | TOKEN_ADJUST_PRIVILEGES | TOKEN_ADJUST_GROUPS |
    TOKEN_ADJUST_DEFAULT);
   public enum TOKEN_INFORMATION_CLASS
   {
    TokenUser = 1,
    TokenGroups = 2,
    TokenPrivileges = 3,
    TokenOwner = 4,
    TokenPrimaryGroup = 5,
    TokenDefaultDacl = 6,
    TokenSource = 7,
    TokenType = 8,
    TokenImpersonationLevel = 9,
    TokenStatistics = 10,
    TokenRestrictedSids = 11,
    TokenSessionId = 12,
    TokenGroupsAndPrivileges = 13,
    TokenSessionReference = 14,
    TokenSandBoxInert = 15,
    TokenAuditPolicy = 16,
    TokenOrigin = 17,
    TokenElevationType = 18,
    TokenLinkedToken = 19,
    TokenElevation = 20,
    TokenHasRestrictions = 21,
    TokenAccessInformation = 22,
    TokenVirtualizationAllowed = 23,
    TokenVirtualizationEnabled = 24,
    TokenIntegrityLevel = 25,
    TokenUIAccess = 26,
    TokenMandatoryPolicy = 27,
    TokenLogonSid = 28,
    MaxTokenInfoClass = 29
   }
   public enum TOKEN_ELEVATION_TYPE
   {
    TokenElevationTypeDefault = 1,
    TokenElevationTypeFull = 2,
    TokenElevationTypeLimited = 3,
   }
  }
 }
 public sealed class CryptApi : IDisposable
 {
  private CryptApi()
  {
   const string IntelDefaultProvider = "Intel Hardware Cryptographic Service Provider";
   handle = new SafeCryptHandle();
   if (AdvApi.NativeMethods.CryptAcquireContext(out handle, null,
    IntelDefaultProvider, AdvApi.NativeMethods.PROV_INTEL_SEC, 0))
   {
    return;
   }
   else if (AdvApi.NativeMethods.CryptAcquireContext(out handle, null,
    null, AdvApi.NativeMethods.PROV_RSA_FULL, 0))
   {
    return;
   }
   else if (Marshal.GetLastWin32Error() == AdvApi.NativeMethods.NTE_BAD_KEYSET)
   {
    if (AdvApi.NativeMethods.CryptAcquireContext(out handle, null,
     null, AdvApi.NativeMethods.PROV_RSA_FULL,
     AdvApi.NativeMethods.CRYPT_NEWKEYSET))
    {
     return;
    }
   }
   throw new NotSupportedException("Unable to acquire a cryptographic service provider.");
  }
  ~CryptApi()
  {
   Dispose(false);
  }
  public void Dispose(bool disposing)
  {
   if (disposing)
    handle.Close();
  }
  public void Dispose()
  {
   Dispose(true);
   GC.SuppressFinalize(this);
  }
  public static bool CryptGenRandom(byte[] buffer)
  {
   return AdvApi.NativeMethods.CryptGenRandom(instance.handle,
    (uint)buffer.Length, buffer);
  }
  private SafeCryptHandle handle;
  private static CryptApi instance = new CryptApi();
 }
 internal class SafeCryptHandle : SafeHandle
 {
  public SafeCryptHandle()
   : base(IntPtr.Zero, true)
  {
  }
  public override bool IsInvalid
  {
   get { return handle == IntPtr.Zero; }
  }
  protected override bool ReleaseHandle()
  {
   AdvApi.NativeMethods.CryptReleaseContext(handle, 0u);
   handle = IntPtr.Zero;
   return true;
  }
 }
 internal class SafeTokenHandle : SafeHandle
 {
  public SafeTokenHandle()
   : base(IntPtr.Zero, true)
  {
  }
  public override bool IsInvalid
  {
   get { return handle == IntPtr.Zero; }
  }
  protected override bool ReleaseHandle()
  {
   KernelApi.NativeMethods.CloseHandle(handle);
   handle = IntPtr.Zero;
   return true;
  }
 }
}
