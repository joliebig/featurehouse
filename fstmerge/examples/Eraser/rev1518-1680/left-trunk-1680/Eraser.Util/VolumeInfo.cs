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
   IntPtr pathNamesBuffer = IntPtr.Zero;
   string pathNames = string.Empty;
   try
   {
    uint currentBufferSize = KernelApi.NativeMethods.MaxPath;
    uint returnLength = 0;
    pathNamesBuffer = Marshal.AllocHGlobal((int)(currentBufferSize * sizeof(char)));
    while (!KernelApi.NativeMethods.GetVolumePathNamesForVolumeName(VolumeId,
     pathNamesBuffer, currentBufferSize, out returnLength))
    {
     if (Marshal.GetLastWin32Error() == 234)
     {
      Marshal.FreeHGlobal(pathNamesBuffer);
      currentBufferSize *= 2;
      pathNamesBuffer = Marshal.AllocHGlobal((int)(currentBufferSize * sizeof(char)));
     }
     else
      throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
    }
    pathNames = Marshal.PtrToStringUni(pathNamesBuffer, (int)returnLength);
   }
   finally
   {
    if (pathNamesBuffer != IntPtr.Zero)
     Marshal.FreeHGlobal(pathNamesBuffer);
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
   StringBuilder volumeName = new StringBuilder(KernelApi.NativeMethods.MaxPath * sizeof(char)),
    fileSystemName = new StringBuilder(KernelApi.NativeMethods.MaxPath * sizeof(char));
   uint serialNumber, maxComponentLength, filesystemFlags;
   if (!KernelApi.NativeMethods.GetVolumeInformation(volumeId, volumeName,
    KernelApi.NativeMethods.MaxPath, out serialNumber, out maxComponentLength,
    out filesystemFlags, fileSystemName, KernelApi.NativeMethods.MaxPath))
   {
    int lastError = Marshal.GetLastWin32Error();
    switch (lastError)
    {
     case 0:
     case 21:
     case 87:
     case 1005:
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
     if (KernelApi.NativeMethods.GetDiskFreeSpace(VolumeId, out clusterSize,
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
  public static ICollection<VolumeInfo> Volumes
  {
   get
   {
    List<VolumeInfo> result = new List<VolumeInfo>();
    StringBuilder nextVolume = new StringBuilder(
     KernelApi.NativeMethods.LongPath * sizeof(char));
    SafeHandle handle = KernelApi.NativeMethods.FindFirstVolume(nextVolume,
     KernelApi.NativeMethods.LongPath);
    if (handle.IsInvalid)
     return result;
    do
     result.Add(new VolumeInfo(nextVolume.ToString()));
    while (KernelApi.NativeMethods.FindNextVolume(handle, nextVolume,
     KernelApi.NativeMethods.LongPath));
    if (Marshal.GetLastWin32Error() == 18 )
     KernelApi.NativeMethods.FindVolumeClose(handle);
    return result.AsReadOnly();
   }
  }
  public static VolumeInfo FromMountpoint(string mountpoint)
  {
   DirectoryInfo mountpointDir = new DirectoryInfo(mountpoint);
   StringBuilder volumeID = new StringBuilder(50 * sizeof(char));
   if (!mountpointDir.Exists)
    throw new DirectoryNotFoundException();
   do
   {
    string currentDir = mountpointDir.FullName;
    if (currentDir.Length > 0 && currentDir[currentDir.Length - 1] != '\\')
     currentDir += '\\';
    if (KernelApi.NativeMethods.GetVolumeNameForVolumeMountPoint(currentDir,
     volumeID, 50))
    {
     return new VolumeInfo(volumeID.ToString());
    }
    else
    {
     switch (Marshal.GetLastWin32Error())
     {
      case 1:
      case 2:
      case 3:
      case 4390:
       break;
      default:
       throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
     }
    }
    mountpointDir = mountpointDir.Parent;
   }
   while (mountpointDir != null);
   throw Marshal.GetExceptionForHR(KernelApi.GetHRForWin32Error(
    4390 ));
  }
  public string VolumeId { get; private set; }
  public string VolumeLabel { get; private set; }
  public string VolumeFormat { get; private set; }
  public DriveType VolumeType
  {
   get
   {
    return (DriveType)KernelApi.NativeMethods.GetDriveType(VolumeId);
   }
  }
  public int ClusterSize
  {
   get
   {
    uint clusterSize, sectorSize, freeClusters, totalClusters;
    if (KernelApi.NativeMethods.GetDiskFreeSpace(VolumeId, out clusterSize,
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
    if (KernelApi.NativeMethods.GetDiskFreeSpace(VolumeId, out clusterSize,
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
    if (KernelApi.NativeMethods.GetDiskFreeSpaceEx(VolumeId, out freeBytesAvailable,
     out totalNumberOfBytes, out totalNumberOfFreeBytes))
    {
     return totalNumberOfFreeBytes != freeBytesAvailable;
    }
    else if (Marshal.GetLastWin32Error() == 21 )
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
    if (KernelApi.NativeMethods.GetDiskFreeSpaceEx(VolumeId, out dummy,
     out dummy, out result))
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
    if (KernelApi.NativeMethods.GetDiskFreeSpaceEx(VolumeId, out dummy,
     out result, out dummy))
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
    if (KernelApi.NativeMethods.GetDiskFreeSpaceEx(VolumeId, out result,
     out dummy, out dummy))
    {
     return (long)result;
    }
    throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
   }
  }
  public ICollection<VolumeInfo> MountedVolumes
  {
   get
   {
    List<VolumeInfo> result = new List<VolumeInfo>();
    StringBuilder nextMountpoint = new StringBuilder(
     KernelApi.NativeMethods.LongPath * sizeof(char));
    SafeHandle handle = KernelApi.NativeMethods.FindFirstVolumeMountPoint(VolumeId,
     nextMountpoint, KernelApi.NativeMethods.LongPath);
    if (handle.IsInvalid)
     return result;
    while (KernelApi.NativeMethods.FindNextVolumeMountPoint(handle,
     nextMountpoint, KernelApi.NativeMethods.LongPath))
    {
     result.Add(new VolumeInfo(nextMountpoint.ToString()));
    }
    if (Marshal.GetLastWin32Error() == 18 )
     KernelApi.NativeMethods.FindVolumeMountPointClose(handle);
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
     iAccess = KernelApi.NativeMethods.GENERIC_READ;
     break;
    case FileAccess.ReadWrite:
     iAccess = KernelApi.NativeMethods.GENERIC_READ |
      KernelApi.NativeMethods.GENERIC_WRITE;
     break;
    case FileAccess.Write:
     iAccess = KernelApi.NativeMethods.GENERIC_WRITE;
     break;
   }
   if ((share & FileShare.Inheritable) != 0)
    throw new NotSupportedException("Inheritable handles are not supported.");
   if ((options & FileOptions.Asynchronous) != 0)
    throw new NotSupportedException("Asynchronous handles are not implemented.");
   string openPath = VolumeId;
   if (openPath.Length > 0 && openPath[openPath.Length - 1] == '\\')
    openPath = openPath.Remove(openPath.Length - 1);
   SafeFileHandle result = KernelApi.NativeMethods.CreateFile(openPath, iAccess,
    (uint)share, IntPtr.Zero, (uint)FileMode.Open, (uint)options, IntPtr.Zero);
   if (result.IsInvalid)
    throw KernelApi.GetExceptionForWin32Error(Marshal.GetLastWin32Error());
   return result;
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
   for (int i = 0; !KernelApi.NativeMethods.DeviceIoControl(stream.SafeFileHandle,
     KernelApi.NativeMethods.FSCTL_LOCK_VOLUME, IntPtr.Zero, 0, IntPtr.Zero,
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
  }
  void Dispose(bool disposing)
  {
   if (disposing)
    GC.SuppressFinalize(this);
   Stream.Flush();
   uint result = 0;
   if (!KernelApi.NativeMethods.DeviceIoControl(Stream.SafeFileHandle,
    KernelApi.NativeMethods.FSCTL_UNLOCK_VOLUME, IntPtr.Zero, 0, IntPtr.Zero,
    0, out result, IntPtr.Zero))
   {
    throw new IOException("Could not unlock volume.");
   }
  }
  private FileStream Stream;
 }
}
