

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;

namespace Eraser.Util
{



 internal static partial class NativeMethods
 {
  [DllImport("User32.dll", SetLastError = true)]
  [return: MarshalAs(UnmanagedType.Bool)]
  public static extern bool GetCaretPos(out Point lpPoint);
  [DllImport("User32.dll")]
  public static extern uint GetMessagePos();
  [DllImport("User32.dll")]
  public static extern int GetMessageTime();
  [DllImport("User32.dll", SetLastError = true)]
  public static extern IntPtr SendMessage(IntPtr hWnd, uint Msg, UIntPtr wParam,
   IntPtr lParam);
  public const uint LVS_EX_DOUBLEBUFFER = 0x00010000u;
  public const uint LVM_FIRST = 0x1000;
  public const uint LVM_SETEXTENDEDLISTVIEWSTYLE = (LVM_FIRST + 54);
 }
}
