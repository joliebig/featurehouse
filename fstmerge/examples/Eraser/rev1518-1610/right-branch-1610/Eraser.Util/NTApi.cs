using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
namespace Eraser.Util
{
 public static class NTApi
 {
  [Obsolete]
  public static uint NtQuerySystemInformation(uint type, byte[] data,
   uint maxSize, out uint dataSize)
  {
   return NativeMethods.NtQuerySystemInformation(type, data, maxSize,
    out dataSize);
  }
 }
}
