using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
using System.ComponentModel;
namespace Eraser.Util
{
 public static class SystemInfo
 {
  public static long PerformanceCounter
  {
   get
   {
    long result = 0;
    if (NativeMethods.QueryPerformanceCounter(out result))
     return result;
    return 0;
   }
  }
  public static ProcessorArchitecture ProcessorArchitecture
  {
   get
   {
    NativeMethods.SYSTEM_INFO info = new NativeMethods.SYSTEM_INFO();
    NativeMethods.GetSystemInfo(out info);
    switch (info.processorArchitecture)
    {
     case NativeMethods.SYSTEM_INFO.ProcessorArchitecture.PROCESSOR_ARCHITECTURE_AMD64:
      return ProcessorArchitecture.Amd64;
     case NativeMethods.SYSTEM_INFO.ProcessorArchitecture.PROCESSOR_ARCHITECTURE_IA64:
      return ProcessorArchitecture.IA64;
     case NativeMethods.SYSTEM_INFO.ProcessorArchitecture.PROCESSOR_ARCHITECTURE_INTEL:
      return ProcessorArchitecture.X86;
     default:
      return ProcessorArchitecture.None;
    }
   }
  }
 }
}
