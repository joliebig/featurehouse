using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Win32.SafeHandles;
using System.Runtime.InteropServices;
namespace Eraser.Util
{
 public static class NtfsApi
 {
  public static long GetMftValidSize(VolumeInfo volume)
  {
   return GetNtfsVolumeData(volume).MftValidDataLength;
  }
  public static long GetMftRecordSegmentSize(VolumeInfo volume)
  {
   return GetNtfsVolumeData(volume).BytesPerFileRecordSegment;
  }
  internal static NativeMethods.NTFS_VOLUME_DATA_BUFFER GetNtfsVolumeData(VolumeInfo volume)
  {
   using (SafeFileHandle volumeHandle = NativeMethods.CreateFile(
    volume.VolumeId.Remove(volume.VolumeId.Length - 1),
    NativeMethods.GENERIC_READ, NativeMethods.FILE_SHARE_READ |
    NativeMethods.FILE_SHARE_WRITE, IntPtr.Zero, NativeMethods.OPEN_EXISTING,
    0, IntPtr.Zero))
   {
    uint resultSize = 0;
    NativeMethods.NTFS_VOLUME_DATA_BUFFER volumeData =
     new NativeMethods.NTFS_VOLUME_DATA_BUFFER();
    if (NativeMethods.DeviceIoControl(volumeHandle,
     NativeMethods.FSCTL_GET_NTFS_VOLUME_DATA, IntPtr.Zero, 0, out volumeData,
     (uint)Marshal.SizeOf(volumeData), out resultSize, IntPtr.Zero))
    {
     return volumeData;
    }
    throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
   }
  }
 }
}
