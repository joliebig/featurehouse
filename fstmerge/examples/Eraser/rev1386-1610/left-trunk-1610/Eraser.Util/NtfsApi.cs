using System;
using System.Collections.Generic;
using System.Text;
namespace Eraser.Util
{
 public static class NtfsApi
 {
  public static long GetMftValidSize(VolumeInfo volume)
  {
   NTApi.NativeMethods.NTFS_VOLUME_DATA_BUFFER data =
    NTApi.NativeMethods.GetNtfsVolumeData(volume);
   return data.MftValidDataLength;
  }
  public static long GetMftRecordSegmentSize(VolumeInfo volume)
  {
   NTApi.NativeMethods.NTFS_VOLUME_DATA_BUFFER data =
    NTApi.NativeMethods.GetNtfsVolumeData(volume);
   return data.BytesPerFileRecordSegment;
  }
 }
}
