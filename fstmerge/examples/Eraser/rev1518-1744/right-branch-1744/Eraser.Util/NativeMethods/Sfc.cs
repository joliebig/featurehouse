using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
namespace Eraser.Util
{
 internal static partial class NativeMethods
 {
  [DllImport("Sfc.dll", CharSet = CharSet.Unicode)]
  [return: MarshalAs(UnmanagedType.Bool)]
  public static extern bool SfcIsFileProtected(IntPtr RpcHandle,
   string ProtFileName);
 }
}
