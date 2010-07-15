

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace Eraser.Util
{



 internal static partial class NativeMethods
 {
  [DllImport("Mpr.dll", CharSet = CharSet.Unicode)]
  public static extern uint WNetOpenEnum(uint dwScope, uint dwType, uint dwUsage,
   IntPtr lpNetResource, out IntPtr lphEnum);
  [DllImport("Mpr.dll", CharSet = CharSet.Unicode)]
  public static extern uint WNetEnumResource(IntPtr hEnum, ref uint lpcCount,
   IntPtr lpBuffer, ref uint lpBufferSize);
  [DllImport("Mpr.dll", CharSet = CharSet.Unicode)]
  public static extern uint WNetCloseEnum(IntPtr hEnum);
  [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
  public struct NETRESOURCE
  {
   public uint dwScope;
   public uint dwType;
   public uint dwDisplayType;
   public uint dwUsage;
   public string lpLocalName;
   public string lpRemoteName;
   public string lpComment;
   public string lpProvider;
  }
  public const int RESOURCE_CONNECTED = 0x00000001;
  public const int RESOURCETYPE_DISK = 0x00000001;
  [DllImport("Mpr.dll", CharSet = CharSet.Unicode)]
  public static extern uint WNetGetConnection(string lpLocalName,
   StringBuilder lpRemoteName, ref uint lpnLength);
 }
}
