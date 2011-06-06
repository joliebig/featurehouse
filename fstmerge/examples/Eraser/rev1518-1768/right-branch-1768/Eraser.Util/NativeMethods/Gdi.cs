using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
namespace Eraser.Util
{
 internal static partial class NativeMethods
 {
  [DllImport("Gdi32.dll")]
  public extern static IntPtr SelectObject(IntPtr hdc, IntPtr hgdiobj);
 }
}
