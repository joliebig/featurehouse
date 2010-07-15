

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace Eraser.Util
{



 internal static partial class NativeMethods
 {
  [DllImport("MsCoree.dll", CharSet = CharSet.Unicode)]
  [return: MarshalAs(UnmanagedType.Bool)]
  public static extern bool StrongNameSignatureVerificationEx(
   string wszFilePath, [MarshalAs(UnmanagedType.Bool)] bool fForceVerification,
   [MarshalAs(UnmanagedType.Bool)] out bool pfWasVerified);
 }
}
