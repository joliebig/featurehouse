using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
using System.ComponentModel;
namespace Eraser.Util
{
 public static class KernelApi
 {
  public static bool AllocConsole()
  {
   return NativeMethods.AllocConsole();
  }
  public static bool FreeConsole()
  {
   return NativeMethods.FreeConsole();
  }
  private static DateTime FileTimeToDateTime(System.Runtime.InteropServices.ComTypes.FILETIME value)
  {
   long time = (long)((((ulong)value.dwHighDateTime) << sizeof(int) * 8) |
    (uint)value.dwLowDateTime);
   return DateTime.FromFileTime(time);
  }
  private static System.Runtime.InteropServices.ComTypes.FILETIME DateTimeToFileTime(DateTime value)
  {
   long time = value.ToFileTime();
   System.Runtime.InteropServices.ComTypes.FILETIME result =
    new System.Runtime.InteropServices.ComTypes.FILETIME();
   result.dwLowDateTime = (int)(time & 0xFFFFFFFFL);
   result.dwHighDateTime = (int)(time >> 32);
   return result;
  }
  internal static int GetHRForWin32Error(int errorCode)
  {
   const uint FACILITY_WIN32 = 7;
   return errorCode <= 0 ? errorCode :
    (int)((((uint)errorCode) & 0x0000FFFF) | (FACILITY_WIN32 << 16) | 0x80000000);
  }
  internal static Exception GetExceptionForWin32Error(int errorCode)
  {
   int HR = GetHRForWin32Error(errorCode);
   return Marshal.GetExceptionForHR(HR);
  }
  public static void GetFileTime(SafeFileHandle file, out DateTime creationTime,
   out DateTime accessedTime, out DateTime modifiedTime)
  {
   System.Runtime.InteropServices.ComTypes.FILETIME accessedTimeNative =
    new System.Runtime.InteropServices.ComTypes.FILETIME();
   System.Runtime.InteropServices.ComTypes.FILETIME modifiedTimeNative =
    new System.Runtime.InteropServices.ComTypes.FILETIME();
   System.Runtime.InteropServices.ComTypes.FILETIME createdTimeNative =
    new System.Runtime.InteropServices.ComTypes.FILETIME();
   if (!NativeMethods.GetFileTime(file, out createdTimeNative, out accessedTimeNative,
    out modifiedTimeNative))
   {
    throw GetExceptionForWin32Error(Marshal.GetLastWin32Error());
   }
   creationTime = FileTimeToDateTime(createdTimeNative);
   accessedTime = FileTimeToDateTime(accessedTimeNative);
   modifiedTime = FileTimeToDateTime(modifiedTimeNative);
  }
  public static void SetFileTime(SafeFileHandle file, DateTime creationTime,
   DateTime accessedTime, DateTime modifiedTime)
  {
   System.Runtime.InteropServices.ComTypes.FILETIME accessedTimeNative =
    new System.Runtime.InteropServices.ComTypes.FILETIME();
   System.Runtime.InteropServices.ComTypes.FILETIME modifiedTimeNative =
    new System.Runtime.InteropServices.ComTypes.FILETIME();
   System.Runtime.InteropServices.ComTypes.FILETIME createdTimeNative =
    new System.Runtime.InteropServices.ComTypes.FILETIME();
   if (!NativeMethods.GetFileTime(file, out createdTimeNative,
    out accessedTimeNative, out modifiedTimeNative))
   {
    throw KernelApi.GetExceptionForWin32Error(Marshal.GetLastWin32Error());
   }
   if (creationTime != DateTime.MinValue)
    createdTimeNative = DateTimeToFileTime(creationTime);
   if (accessedTime != DateTime.MinValue)
    accessedTimeNative = DateTimeToFileTime(accessedTime);
   if (modifiedTime != DateTime.MinValue)
    modifiedTimeNative = DateTimeToFileTime(modifiedTime);
   if (!NativeMethods.SetFileTime(file, ref createdTimeNative,
    ref accessedTimeNative, ref modifiedTimeNative))
   {
    throw KernelApi.GetExceptionForWin32Error(Marshal.GetLastWin32Error());
   }
  }
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
  public static ThreadExecutionState SetThreadExecutionState(
   ThreadExecutionState executionState)
  {
   return (ThreadExecutionState)NativeMethods.SetThreadExecutionState(
    (NativeMethods.EXECUTION_STATE)executionState);
  }
  public class DiskPerformanceInfo
  {
   unsafe internal DiskPerformanceInfo(NativeMethods.DiskPerformanceInfoInternal info)
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
    StorageManagerName = new string((char*)info.StorageManagerName);
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
  public static DiskPerformanceInfo QueryDiskPerformanceInfo(SafeFileHandle diskHandle)
  {
   if (diskHandle.IsInvalid)
    throw new ArgumentException("The disk handle must not be invalid.");
   NativeMethods.DiskPerformanceInfoInternal result =
    new NativeMethods.DiskPerformanceInfoInternal();
   uint bytesReturned = 0;
   if (NativeMethods.DeviceIoControl(diskHandle, NativeMethods.IOCTL_DISK_PERFORMANCE,
    IntPtr.Zero, 0, out result, (uint)Marshal.SizeOf(result), out bytesReturned, IntPtr.Zero))
   {
    return new DiskPerformanceInfo(result);
   }
   return null;
  }
  internal static class NativeMethods
  {
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool CloseHandle(IntPtr hObject);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool DeleteFile(string lpFileName);
   [DllImport("Kernel32.dll", SetLastError = true)]
   public static extern IntPtr GetCurrentProcess();
   [DllImport("Kernel32.dll")]
   public static extern void GetSystemInfo(out SYSTEM_INFO lpSystemInfo);
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool QueryPerformanceCounter(out long lpPerformanceCount);
   [StructLayout(LayoutKind.Sequential)]
   internal struct SYSTEM_INFO
   {
    public enum ProcessorArchitecture : ushort
    {
     PROCESSOR_ARCHITECTURE_AMD64 = 9,
     PROCESSOR_ARCHITECTURE_IA64 = 6,
     PROCESSOR_ARCHITECTURE_INTEL = 0,
     PROCESSOR_ARCHITECTURE_UNKNOWN = 0xffff
    }
    public ProcessorArchitecture processorArchitecture;
    private const ushort reserved = 0;
    public uint pageSize;
    public IntPtr minimumApplicationAddress;
    public IntPtr maximumApplicationAddress;
    public IntPtr activeProcessorMask;
    public uint numberOfProcessors;
    public uint processorType;
    public uint allocationGranularity;
    public ushort processorLevel;
    public ushort processorRevision;
   }
   [DllImport("Kernel32.dll")]
   public static extern EXECUTION_STATE SetThreadExecutionState(EXECUTION_STATE esFlags);
   [Flags]
   public enum EXECUTION_STATE : uint
   {
    ES_AWAYMODE_REQUIRED = 0x00000040,
    ES_CONTINUOUS = 0x80000000,
    ES_DISPLAY_REQUIRED = 0x00000002,
    ES_SYSTEM_REQUIRED = 0x00000001,
    ES_USER_PRESENT = 0x00000004
   }
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool AllocConsole();
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool FreeConsole();
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   public static extern SafeFileHandle CreateFile(string lpFileName, uint dwDesiredAccess,
    uint dwShareMode, IntPtr SecurityAttributes, uint dwCreationDisposition,
    uint dwFlagsAndAttributes, IntPtr hTemplateFile);
   public const uint GENERIC_READ = 0x80000000;
   public const uint GENERIC_WRITE = 0x40000000;
   public const uint GENERIC_EXECUTE = 0x20000000;
   public const uint GENERIC_ALL = 0x10000000;
   public const uint FILE_SHARE_READ = 0x00000001;
   public const uint FILE_SHARE_WRITE = 0x00000002;
   public const uint FILE_SHARE_DELETE = 0x00000004;
   public const uint CREATE_NEW = 1;
   public const uint CREATE_ALWAYS = 2;
   public const uint OPEN_EXISTING = 3;
   public const uint OPEN_ALWAYS = 4;
   public const uint TRUNCATE_EXISTING = 5;
   public const uint FILE_FLAG_WRITE_THROUGH = 0x80000000;
   public const uint FILE_FLAG_OVERLAPPED = 0x40000000;
   public const uint FILE_FLAG_NO_BUFFERING = 0x20000000;
   public const uint FILE_FLAG_RANDOM_ACCESS = 0x10000000;
   public const uint FILE_FLAG_SEQUENTIAL_SCAN = 0x08000000;
   public const uint FILE_FLAG_DELETE_ON_CLOSE = 0x04000000;
   public const uint FILE_FLAG_BACKUP_SEMANTICS = 0x02000000;
   public const uint FILE_FLAG_POSIX_SEMANTICS = 0x01000000;
   public const uint FILE_FLAG_OPEN_REPARSE_POINT = 0x00200000;
   public const uint FILE_FLAG_OPEN_NO_RECALL = 0x00100000;
   public const uint FILE_FLAG_FIRST_PIPE_INSTANCE = 0x00080000;
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public extern static bool DeviceIoControl(SafeFileHandle hDevice,
    uint dwIoControlCode, IntPtr lpInBuffer, uint nInBufferSize,
    out ushort lpOutBuffer, uint nOutBufferSize, out uint lpBytesReturned,
    IntPtr lpOverlapped);
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public extern static bool DeviceIoControl(SafeFileHandle hDevice,
    uint dwIoControlCode, ref ushort lpInBuffer, uint nInBufferSize,
    IntPtr lpOutBuffer, uint nOutBufferSize, out uint lpBytesReturned,
    IntPtr lpOverlapped);
   public const uint FSCTL_GET_COMPRESSION = 0x9003C;
   public const uint FSCTL_SET_COMPRESSION = 0x9C040;
   public const ushort COMPRESSION_FORMAT_NONE = 0x0000;
   public const ushort COMPRESSION_FORMAT_DEFAULT = 0x0001;
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public extern static bool DeviceIoControl(SafeFileHandle hDevice,
    uint dwIoControlCode, IntPtr lpInBuffer, uint nInBufferSize,
    IntPtr lpOutBuffer, uint nOutBufferSize, out uint lpBytesReturned,
    IntPtr lpOverlapped);
   public const uint FSCTL_LOCK_VOLUME = 0x90018;
   public const uint FSCTL_UNLOCK_VOLUME = 0x9001C;
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public extern static bool DeviceIoControl(SafeFileHandle hDevice,
    uint dwIoControlCode, IntPtr lpInBuffer, uint nInBufferSize,
    out DiskPerformanceInfoInternal lpOutBuffer, uint nOutBufferSize, out uint lpBytesReturned,
    IntPtr lpOverlapped);
   public const uint IOCTL_DISK_PERFORMANCE = ((0x00000007) << 16) | ((0x0008) << 2);
   public unsafe struct DiskPerformanceInfoInternal
   {
    public long BytesRead;
    public long BytesWritten;
    public long ReadTime;
    public long WriteTime;
    public long IdleTime;
    public uint ReadCount;
    public uint WriteCount;
    public uint QueueDepth;
    public uint SplitCount;
    public long QueryTime;
    public uint StorageDeviceNumber;
    public fixed short StorageManagerName[8];
   }
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   public static extern uint GetFileAttributes(string lpFileName);
   [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2205:UseManagedEquivalentsOfWin32Api")]
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool SetFileAttributes(string lpFileName,
    uint dwFileAttributes);
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool GetFileSizeEx(SafeFileHandle hFile, out long lpFileSize);
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool GetFileTime(SafeFileHandle hFile,
    out System.Runtime.InteropServices.ComTypes.FILETIME lpCreationTime,
    out System.Runtime.InteropServices.ComTypes.FILETIME lpLastAccessTime,
    out System.Runtime.InteropServices.ComTypes.FILETIME lpLastWriteTime);
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool SetFileTime(SafeFileHandle hFile,
    ref System.Runtime.InteropServices.ComTypes.FILETIME lpCreationTime,
    ref System.Runtime.InteropServices.ComTypes.FILETIME lpLastAccessTime,
    ref System.Runtime.InteropServices.ComTypes.FILETIME lpLastWriteTime);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   public static extern SafeFileHandle FindFirstVolume(StringBuilder lpszVolumeName,
    uint cchBufferLength);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool FindNextVolume(SafeHandle hFindVolume,
    StringBuilder lpszVolumeName, uint cchBufferLength);
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool FindVolumeClose(SafeHandle hFindVolume);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   public static extern SafeFileHandle FindFirstVolumeMountPoint(
    string lpszRootPathName, StringBuilder lpszVolumeMountPoint,
    uint cchBufferLength);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool FindNextVolumeMountPoint(
    SafeHandle hFindVolumeMountPoint, StringBuilder lpszVolumeMountPoint,
    uint cchBufferLength);
   [DllImport("Kernel32.dll", SetLastError = true)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool FindVolumeMountPointClose(SafeHandle hFindVolumeMountPoint);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool GetDiskFreeSpace(
    string lpRootPathName, out UInt32 lpSectorsPerCluster, out UInt32 lpBytesPerSector,
    out UInt32 lpNumberOfFreeClusters, out UInt32 lpTotalNumberOfClusters);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool GetDiskFreeSpaceEx(
    string lpDirectoryName,
    out UInt64 lpFreeBytesAvailable,
    out UInt64 lpTotalNumberOfBytes,
    out UInt64 lpTotalNumberOfFreeBytes);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   public static extern uint GetDriveType(string lpRootPathName);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool GetVolumeInformation(
    string lpRootPathName,
    StringBuilder lpVolumeNameBuffer,
    uint nVolumeNameSize,
    out uint lpVolumeSerialNumber,
    out uint lpMaximumComponentLength,
    out uint lpFileSystemFlags,
    StringBuilder lpFileSystemNameBuffer,
    uint nFileSystemNameSize);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool GetVolumeNameForVolumeMountPoint(
    string lpszVolumeMountPoint, StringBuilder lpszVolumeName,
    uint cchBufferLength);
   [DllImport("Kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool GetVolumePathNamesForVolumeName(
    string lpszVolumeName, IntPtr lpszVolumePathNames, uint cchBufferLength,
    out uint lpcchReturnLength);
   public const int MaxPath = 260;
   public const int LongPath = 32768;
  }
 }
 public enum ThreadExecutionState
 {
  None = 0,
  AwayModeRequired = (int)KernelApi.NativeMethods.EXECUTION_STATE.ES_AWAYMODE_REQUIRED,
  Continuous = unchecked((int)KernelApi.NativeMethods.EXECUTION_STATE.ES_CONTINUOUS),
  DisplayRequired = (int)KernelApi.NativeMethods.EXECUTION_STATE.ES_DISPLAY_REQUIRED,
  SystemRequired = (int)KernelApi.NativeMethods.EXECUTION_STATE.ES_SYSTEM_REQUIRED,
  UserPresent = (int)KernelApi.NativeMethods.EXECUTION_STATE.ES_USER_PRESENT
 }
}
