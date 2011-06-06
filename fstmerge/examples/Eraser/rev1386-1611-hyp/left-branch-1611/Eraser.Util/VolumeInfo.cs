using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.ComponentModel;
using System.IO;
using Microsoft.Win32.SafeHandles;
using System.Collections.ObjectModel;
namespace Eraser.Util
{
 public class VolumeInfo
 {
  public VolumeInfo(string volumeId)
  {
   VolumeId = volumeId;
   string pathNames;
   {
    uint returnLength = 0;
    StringBuilder pathNamesBuffer = new StringBuilder();
    pathNamesBuffer.EnsureCapacity(NativeMethods.MaxPath);
    while (!NativeMethods.GetVolumePathNamesForVolumeName(VolumeId,
     pathNamesBuffer, (uint)pathNamesBuffer.Capacity, out returnLength))
    {
     int errorCode = Marshal.GetLastWin32Error();
     switch (errorCode)
     {
      case Win32ErrorCode.MoreData:
       pathNamesBuffer.EnsureCapacity((int)returnLength);
       break;
      default:
       throw Win32ErrorCode.GetExceptionForWin32Error(errorCode);
     }
    }
    if (pathNamesBuffer.Length < returnLength)
     pathNamesBuffer.Length = (int)returnLength;
    pathNames = pathNamesBuffer.ToString().Substring(0, (int)returnLength);
   }
   for (int lastIndex = 0, i = 0; i != pathNames.Length; ++i)
   {
    if (pathNames[i] == '\0')
    {
     if (i - lastIndex == 0)
      break;
     mountPoints.Add(pathNames.Substring(lastIndex, i - lastIndex));
     lastIndex = i + 1;
     if (pathNames[lastIndex] == '\0')
      break;
    }
   }
   StringBuilder volumeName = new StringBuilder(NativeMethods.MaxPath * sizeof(char)),
    fileSystemName = new StringBuilder(NativeMethods.MaxPath * sizeof(char));
   uint serialNumber, maxComponentLength, filesystemFlags;
   if (!NativeMethods.GetVolumeInformation(volumeId, volumeName, NativeMethods.MaxPath,
    out serialNumber, out maxComponentLength, out filesystemFlags, fileSystemName,
    NativeMethods.MaxPath))
   {
    int lastError = Marshal.GetLastWin32Error();
    switch (lastError)
    {
     case Win32ErrorCode.Success:
     case Win32ErrorCode.NotReady:
     case Win32ErrorCode.InvalidParameter:
     case Win32ErrorCode.UnrecognizedVolume:
      break;
     default:
      throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
    }
   }
   else
   {
    IsReady = true;
    VolumeLabel = volumeName.ToString();
    VolumeFormat = fileSystemName.ToString();
    if (VolumeFormat == "FAT")
    {
     uint clusterSize, sectorSize, freeClusters, totalClusters;
     if (NativeMethods.GetDiskFreeSpace(VolumeId, out clusterSize,
      out sectorSize, out freeClusters, out totalClusters))
     {
      if (totalClusters <= 0xFF0)
       VolumeFormat += "12";
      else
       VolumeFormat += "16";
     }
    }
   }
  }
  public static IList<VolumeInfo> Volumes
  {
   get
   {
    List<VolumeInfo> result = new List<VolumeInfo>();
    StringBuilder nextVolume = new StringBuilder(NativeMethods.LongPath * sizeof(char));
    SafeHandle handle = NativeMethods.FindFirstVolume(nextVolume, NativeMethods.LongPath);
    if (handle.IsInvalid)
     return result;
    try
    {
     do
      result.Add(new VolumeInfo(nextVolume.ToString()));
     while (NativeMethods.FindNextVolume(handle, nextVolume, NativeMethods.LongPath));
    }
    finally
    {
     NativeMethods.FindVolumeClose(handle);
    }
    return result.AsReadOnly();
   }
  }
  public static VolumeInfo FromMountPoint(string mountPoint)
  {
   DirectoryInfo mountpointDir = new DirectoryInfo(mountPoint);
   StringBuilder volumeID = new StringBuilder(50 * sizeof(char));
   do
   {
    string currentDir = mountpointDir.FullName;
    if (currentDir.Length > 0 && currentDir[currentDir.Length - 1] != '\\')
     currentDir += '\\';
    if (!NativeMethods.GetVolumeNameForVolumeMountPoint(currentDir, volumeID, 50))
    {
     int errorCode = Marshal.GetLastWin32Error();
     switch (errorCode)
     {
      case Win32ErrorCode.InvalidFunction:
      case Win32ErrorCode.FileNotFound:
      case Win32ErrorCode.PathNotFound:
      case Win32ErrorCode.NotAReparsePoint:
       break;
      default:
       throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
     }
    }
    else
    {
     return new VolumeInfo(volumeID.ToString());
    }
    mountpointDir = mountpointDir.Parent;
   }
   while (mountpointDir != null);
   throw Win32ErrorCode.GetExceptionForWin32Error(Win32ErrorCode.NotAReparsePoint);
  }
  public string VolumeId { get; private set; }
  public string VolumeLabel { get; private set; }
  public string VolumeFormat { get; private set; }
  public DriveType VolumeType
  {
   get
   {
    return (DriveType)NativeMethods.GetDriveType(VolumeId);
   }
  }
  public int ClusterSize
  {
   get
   {
    uint clusterSize, sectorSize, freeClusters, totalClusters;
    if (NativeMethods.GetDiskFreeSpace(VolumeId, out clusterSize,
     out sectorSize, out freeClusters, out totalClusters))
    {
     return (int)(clusterSize * sectorSize);
    }
    throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
   }
  }
  public int SectorSize
  {
   get
   {
    uint clusterSize, sectorSize, freeClusters, totalClusters;
    if (NativeMethods.GetDiskFreeSpace(VolumeId, out clusterSize,
     out sectorSize, out freeClusters, out totalClusters))
    {
     return (int)sectorSize;
    }
    throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
   }
  }
  public bool HasQuota
  {
   get
   {
    ulong freeBytesAvailable, totalNumberOfBytes, totalNumberOfFreeBytes;
    if (NativeMethods.GetDiskFreeSpaceEx(VolumeId, out freeBytesAvailable,
     out totalNumberOfBytes, out totalNumberOfFreeBytes))
    {
     return totalNumberOfFreeBytes != freeBytesAvailable;
    }
    else if (Marshal.GetLastWin32Error() == Win32ErrorCode.NotReady)
    {
     return false;
    }
    throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
   }
  }
  public bool IsReady { get; private set; }
  public long TotalFreeSpace
  {
   get
   {
    ulong result, dummy;
    if (NativeMethods.GetDiskFreeSpaceEx(VolumeId, out dummy, out dummy, out result))
    {
     return (long)result;
    }
    throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
   }
  }
  public long TotalSize
  {
   get
   {
    ulong result, dummy;
    if (NativeMethods.GetDiskFreeSpaceEx(VolumeId, out dummy, out result, out dummy))
    {
     return (long)result;
    }
    throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
   }
  }
  public long AvailableFreeSpace
  {
   get
   {
    ulong result, dummy;
    if (NativeMethods.GetDiskFreeSpaceEx(VolumeId, out result, out dummy, out dummy))
    {
     return (long)result;
    }
    throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
   }
  }
  public IList<VolumeInfo> MountedVolumes
  {
   get
   {
    List<VolumeInfo> result = new List<VolumeInfo>();
    StringBuilder nextMountpoint = new StringBuilder(NativeMethods.LongPath * sizeof(char));
    SafeHandle handle = NativeMethods.FindFirstVolumeMountPoint(VolumeId,
     nextMountpoint, NativeMethods.LongPath);
    if (handle.IsInvalid)
     return result;
    try
    {
     while (NativeMethods.FindNextVolumeMountPoint(handle, nextMountpoint,
      NativeMethods.LongPath))
     {
      result.Add(new VolumeInfo(nextMountpoint.ToString()));
     }
    }
    finally
    {
     NativeMethods.FindVolumeMountPointClose(handle);
    }
    return result.AsReadOnly();
   }
  }
  public ReadOnlyCollection<string> MountPoints
  {
   get
   {
    return mountPoints.AsReadOnly();
   }
  }
  public bool IsMounted
  {
   get { return MountPoints.Count != 0; }
  }
  public FileStream Open(FileAccess access)
  {
   return Open(access, FileShare.None, FileOptions.None);
  }
  public FileStream Open(FileAccess access, FileShare share)
  {
   return Open(access, share, FileOptions.None);
  }
  public FileStream Open(FileAccess access, FileShare share, FileOptions options)
  {
   SafeFileHandle handle = OpenHandle(access, share, options);
   if (handle.IsInvalid)
    throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
   return new FileStream(handle, access);
  }
  private SafeFileHandle OpenHandle(FileAccess access, FileShare share, FileOptions options)
  {
   uint iAccess = 0;
   switch (access)
   {
    case FileAccess.Read:
     iAccess = NativeMethods.GENERIC_READ;
     break;
    case FileAccess.ReadWrite:
     iAccess = NativeMethods.GENERIC_READ | NativeMethods.GENERIC_WRITE;
     break;
    case FileAccess.Write:
     iAccess = NativeMethods.GENERIC_WRITE;
     break;
   }
   if ((share & FileShare.Inheritable) != 0)
    throw new NotSupportedException("Inheritable handles are not supported.");
   if ((options & FileOptions.Asynchronous) != 0)
    throw new NotSupportedException("Asynchronous handles are not implemented.");
   string openPath = VolumeId;
   if (openPath.Length > 0 && openPath[openPath.Length - 1] == '\\')
    openPath = openPath.Remove(openPath.Length - 1);
   SafeFileHandle result = NativeMethods.CreateFile(openPath, iAccess,
    (uint)share, IntPtr.Zero, (uint)FileMode.Open, (uint)options, IntPtr.Zero);
   if (result.IsInvalid)
   {
    int errorCode = Marshal.GetLastWin32Error();
    result.Close();
    throw Win32ErrorCode.GetExceptionForWin32Error(errorCode);
   }
   return result;
  }
  public DiskPerformanceInfo Performance
  {
   get
   {
    using (SafeFileHandle handle = OpenHandle(FileAccess.Read,
     FileShare.ReadWrite, FileOptions.None))
    {
     if (handle.IsInvalid)
      throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
     NativeMethods.DiskPerformanceInfoInternal result =
      new NativeMethods.DiskPerformanceInfoInternal();
     uint bytesReturned = 0;
     if (NativeMethods.DeviceIoControl(handle, NativeMethods.IOCTL_DISK_PERFORMANCE,
      IntPtr.Zero, 0, out result, (uint)Marshal.SizeOf(result),
      out bytesReturned, IntPtr.Zero))
     {
      return new DiskPerformanceInfo(result);
     }
     return null;
    }
   }
  }
  public VolumeLock LockVolume(FileStream stream)
  {
   return new VolumeLock(stream);
  }
  private List<string> mountPoints = new List<string>();
 }
 public class VolumeLock : IDisposable
 {
  internal VolumeLock(FileStream stream)
  {
   uint result = 0;
   for (int i = 0; !NativeMethods.DeviceIoControl(stream.SafeFileHandle,
     NativeMethods.FSCTL_LOCK_VOLUME, IntPtr.Zero, 0, IntPtr.Zero,
     0, out result, IntPtr.Zero); ++i)
   {
    if (i > 100)
     throw new IOException("Could not lock volume.");
    System.Threading.Thread.Sleep(100);
   }
   Stream = stream;
  }
  ~VolumeLock()
  {
   Dispose(false);
  }
  public void Dispose()
  {
   Dispose(true);
   GC.SuppressFinalize(this);
  }
  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "disposing")]
  void Dispose(bool disposing)
  {
   Stream.Flush();
   uint result = 0;
   if (!NativeMethods.DeviceIoControl(Stream.SafeFileHandle,
    NativeMethods.FSCTL_UNLOCK_VOLUME, IntPtr.Zero, 0, IntPtr.Zero,
    0, out result, IntPtr.Zero))
   {
    throw new IOException("Could not unlock volume.");
   }
  }
  private FileStream Stream;
 }
 public class DiskPerformanceInfo
 {
  internal DiskPerformanceInfo(NativeMethods.DiskPerformanceInfoInternal info)
  {
   BytesRead = info.BytesRead;
   BytesWritten = info.BytesWritten;
   ReadTime = info.ReadTime;
   WriteTime = info.WriteTime;
   IdleTime = info.IdleTime;
   ReadCount = info.ReadCount;
   WriteCount = info.WriteCount;
   QueueDepth = info.QueueDepth;
   SplitCount = info.SplitCount;
   QueryTime = info.QueryTime;
   StorageDeviceNumber = info.StorageDeviceNumber;
   StorageManagerName = info.StorageManagerName;
  }
  public long BytesRead { get; private set; }
  public long BytesWritten { get; private set; }
  public long ReadTime { get; private set; }
  public long WriteTime { get; private set; }
  public long IdleTime { get; private set; }
  public uint ReadCount { get; private set; }
  public uint WriteCount { get; private set; }
  public uint QueueDepth { get; private set; }
  public uint SplitCount { get; private set; }
  public long QueryTime { get; private set; }
  public uint StorageDeviceNumber { get; private set; }
  public string StorageManagerName { get; private set; }
 }
}
