

using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using System.Security.Principal;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;

namespace Eraser.Util
{
 public static class Security
 {






  public static bool IsAdministrator()
  {
   WindowsPrincipal principal = new WindowsPrincipal(WindowsIdentity.GetCurrent());
   return principal.IsInRole(WindowsBuiltInRole.Administrator);
  }






  public static bool VerifyAuthenticode(string pathToFile)
  {
   IntPtr unionPointer = IntPtr.Zero;

   try
   {
    NativeMethods.WINTRUST_FILE_INFO fileinfo = new NativeMethods.WINTRUST_FILE_INFO();
    fileinfo.cbStruct = (uint)Marshal.SizeOf(typeof(NativeMethods.WINTRUST_FILE_INFO));
    fileinfo.pcwszFilePath = pathToFile;

    NativeMethods.WINTRUST_DATA data = new NativeMethods.WINTRUST_DATA();
    data.cbStruct = (uint)Marshal.SizeOf(typeof(NativeMethods.WINTRUST_DATA));
    data.dwUIChoice = NativeMethods.WINTRUST_DATA.UIChoices.WTD_UI_NONE;
    data.fdwRevocationChecks = NativeMethods.WINTRUST_DATA.RevocationChecks.WTD_REVOKE_NONE;
    data.dwUnionChoice = NativeMethods.WINTRUST_DATA.UnionChoices.WTD_CHOICE_FILE;
    unionPointer = data.pUnion = Marshal.AllocHGlobal((int)fileinfo.cbStruct);
    Marshal.StructureToPtr(fileinfo, data.pUnion, false);

    Guid guid = NativeMethods.WINTRUST_ACTION_GENERIC_VERIFY_V2;
    return NativeMethods.WinVerifyTrust(IntPtr.Zero, ref guid, ref data) == 0;
   }
   finally
   {
    if (unionPointer != IntPtr.Zero)
     Marshal.FreeHGlobal(unionPointer);
   }
  }
  public static bool VerifyStrongName(string assemblyPath)
  {
   bool wasVerified = false;
   return NativeMethods.StrongNameSignatureVerificationEx(assemblyPath, false,
    out wasVerified) && wasVerified;
  }
  public static bool Randomise(byte[] buffer)
  {
   return CryptApi.CryptGenRandom(buffer);
  }
 }
 internal sealed class CryptApi : IDisposable
 {
  private CryptApi()
  {
   const string IntelDefaultProvider = "Intel Hardware Cryptographic Service Provider";
   handle = new SafeCryptHandle();
   if (NativeMethods.CryptAcquireContext(out handle, null,
    IntelDefaultProvider, NativeMethods.PROV_INTEL_SEC, 0))
   {
    return;
   }
   else if (NativeMethods.CryptAcquireContext(out handle, null,
    null, NativeMethods.PROV_RSA_FULL, 0))
   {
    return;
   }
   else if (Marshal.GetLastWin32Error() == NativeMethods.NTE_BAD_KEYSET)
   {
    if (NativeMethods.CryptAcquireContext(out handle, null, null,
     NativeMethods.PROV_RSA_FULL, NativeMethods.CRYPT_NEWKEYSET))
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
   return NativeMethods.CryptGenRandom(instance.handle, (uint)buffer.Length, buffer);
  }
  private SafeCryptHandle handle;
  private static CryptApi instance = new CryptApi();
 }
 internal class SafeCryptHandle : SafeHandleZeroOrMinusOneIsInvalid
 {
  public SafeCryptHandle()
   : base(true)
  {
  }
  protected override bool ReleaseHandle()
  {
   NativeMethods.CryptReleaseContext(handle, 0u);
   handle = IntPtr.Zero;
   return true;
  }
 }
}
