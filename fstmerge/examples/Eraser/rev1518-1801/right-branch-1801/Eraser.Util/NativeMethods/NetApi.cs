using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
namespace Eraser.Util
{
 internal static partial class NativeMethods
 {
  [DllImport("Netapi32.dll", CharSet = CharSet.Unicode)]
  public static extern uint NetStatisticsGet(string server, string service,
   uint level, uint options, out IntPtr bufptr);
  [DllImport("Netapi32.dll")]
  public static extern uint NetApiBufferSize(IntPtr Buffer, out uint ByteCount);
  [DllImport("Netapi32.dll")]
  public static extern uint NetApiBufferFree(IntPtr Buffer);
  private const uint NERR_Success = 0;
 }
}
