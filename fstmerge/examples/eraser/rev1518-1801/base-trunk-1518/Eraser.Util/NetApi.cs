

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
  internal static class NativeMethods
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
 public enum NetApiService
 {
  Workstation,
  Server
 }
}
