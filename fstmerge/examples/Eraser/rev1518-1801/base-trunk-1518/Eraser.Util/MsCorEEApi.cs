using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
namespace Eraser.Util
{
 public static class MsCorEEApi
 {
  public static bool VerifyStrongName(string assemblyPath)
  {
   bool wasVerified = false;
   return NativeMethods.StrongNameSignatureVerificationEx(assemblyPath, false,
    out wasVerified) && wasVerified;
  }
  internal static class NativeMethods
  {
   [DllImport("MsCoree.dll", CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool StrongNameSignatureVerificationEx(
    string wszFilePath, [MarshalAs(UnmanagedType.Bool)] bool fForceVerification,
    [MarshalAs(UnmanagedType.Bool)] out bool pfWasVerified);
  }
 }
}
