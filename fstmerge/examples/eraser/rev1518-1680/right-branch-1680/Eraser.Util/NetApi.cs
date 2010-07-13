

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace Eraser.Util
{
 public static class NetApi
 {
  public static byte[] NetStatisticsGet(string server, NetApiService service,
   uint level, uint options)
  {
   IntPtr netAPIStats = IntPtr.Zero;
   string serviceName = "Lanman" + service.ToString();
   if (NativeMethods.NetStatisticsGet(server, serviceName, level, options, out netAPIStats) == 0)
   {
    try
    {
     uint size = 0;
     NativeMethods.NetApiBufferSize(netAPIStats, out size);
     byte[] result = new byte[size];
     Marshal.Copy(result, 0, netAPIStats, result.Length);
     return result;
    }
    finally
    {
     NativeMethods.NetApiBufferFree(netAPIStats);
    }
   }
   return null;
  }
 }
 public enum NetApiService
 {
  Workstation,
  Server
 }
}
