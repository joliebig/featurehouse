

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;

namespace Eraser.Util
{
 public static class NTApi
 {
  public static uint NtQuerySystemInformation(uint type, byte[] data,
   uint maxSize, out uint dataSize)
  {
   return NativeMethods.NtQuerySystemInformation(type, data, maxSize,
    out dataSize);
  }
  internal static class NativeMethods
  {
   [DllImport("NtDll.dll")]
   public static extern uint NtQuerySystemInformation(uint dwType, byte[] dwData,
    uint dwMaxSize, out uint dwDataSize);
   [DllImport("NtDll.dll")]
   private static extern uint NtQueryInformationFile(SafeFileHandle FileHandle,
    ref IO_STATUS_BLOCK IoStatusBlock, IntPtr FileInformation, uint Length,
    FILE_INFORMATION_CLASS FileInformationClass);
   public static FILE_STREAM_INFORMATION[] NtQueryInformationFile(SafeFileHandle FileHandle)
   {
    IO_STATUS_BLOCK status = new IO_STATUS_BLOCK();
    IntPtr fileInfoPtr = IntPtr.Zero;
    try
    {
     FILE_STREAM_INFORMATION streamInfo = new FILE_STREAM_INFORMATION();
     int fileInfoPtrLength = (Marshal.SizeOf(streamInfo) + 32768) / 2;
     uint ntStatus = 0;
     do
     {
      fileInfoPtrLength *= 2;
      if (fileInfoPtr != IntPtr.Zero)
       Marshal.FreeHGlobal(fileInfoPtr);
      fileInfoPtr = Marshal.AllocHGlobal(fileInfoPtrLength);
      ntStatus = NtQueryInformationFile(FileHandle, ref status, fileInfoPtr,
       (uint)fileInfoPtrLength, FILE_INFORMATION_CLASS.FileStreamInformation);
     }
     while (ntStatus != 0 && ntStatus == 0x80000005 );
     List<FILE_STREAM_INFORMATION> result = new List<FILE_STREAM_INFORMATION>();
     unsafe
     {
      for (byte* i = (byte*)fileInfoPtr; streamInfo.NextEntryOffset != 0;
       i += streamInfo.NextEntryOffset)
      {
       byte* currStreamPtr = i;
       streamInfo.NextEntryOffset = *(uint*)currStreamPtr;
       currStreamPtr += sizeof(uint);
       streamInfo.StreamNameLength = *(uint*)currStreamPtr;
       currStreamPtr += sizeof(uint);
       streamInfo.StreamSize = *(long*)currStreamPtr;
       currStreamPtr += sizeof(long);
       streamInfo.StreamAllocationSize = *(long*)currStreamPtr;
       currStreamPtr += sizeof(long);
       streamInfo.StreamName = Marshal.PtrToStringUni((IntPtr)currStreamPtr,
        (int)streamInfo.StreamNameLength / 2);
       result.Add(streamInfo);
      }
     }
     return result.ToArray();
    }
    finally
    {
     Marshal.FreeHGlobal(fileInfoPtr);
    }
   }
   public struct IO_STATUS_BLOCK
   {
    public IntPtr PointerStatus;
    public UIntPtr Information;
   }
   public struct FILE_STREAM_INFORMATION
   {
    public uint NextEntryOffset;
    public uint StreamNameLength;
    public long StreamSize;
    public long StreamAllocationSize;
    public string StreamName;
   }
   public enum FILE_INFORMATION_CLASS
   {
    FileDirectoryInformation = 1,
    FileFullDirectoryInformation,
    FileBothDirectoryInformation,
    FileBasicInformation,
    FileStandardInformation,
    FileInternalInformation,
    FileEaInformation,
    FileAccessInformation,
    FileNameInformation,
    FileRenameInformation,
    FileLinkInformation,
    FileNamesInformation,
    FileDispositionInformation,
    FilePositionInformation,
    FileFullEaInformation,
    FileModeInformation,
    FileAlignmentInformation,
    FileAllInformation,
    FileAllocationInformation,
    FileEndOfFileInformation,
    FileAlternateNameInformation,
    FileStreamInformation,
    FilePipeInformation,
    FilePipeLocalInformation,
    FilePipeRemoteInformation,
    FileMailslotQueryInformation,
    FileMailslotSetInformation,
    FileCompressionInformation,
    FileCopyOnWriteInformation,
    FileCompletionInformation,
    FileMoveClusterInformation,
    FileQuotaInformation,
    FileReparsePointInformation,
    FileNetworkOpenInformation,
    FileObjectIdInformation,
    FileTrackingInformation,
    FileOleDirectoryInformation,
    FileContentIndexInformation,
    FileInheritContentIndexInformation,
    FileOleInformation,
    FileMaximumInformation
   }
   [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
   public struct NTFS_VOLUME_DATA_BUFFER
   {
    public long VolumeSerialNumber;
    public long NumberSectors;
    public long TotalClusters;
    public long FreeClusters;
    public long TotalReserved;
    public uint BytesPerSector;
    public uint BytesPerCluster;
    public uint BytesPerFileRecordSegment;
    public uint ClustersPerFileRecordSegment;
    public long MftValidDataLength;
    public long MftStartLcn;
    public long Mft2StartLcn;
    public long MftZoneStart;
    public long MftZoneEnd;
    public uint ByteCount;
    public ushort MajorVersion;
    public ushort MinorVersion;
   }
   public const int FSCTL_GET_NTFS_VOLUME_DATA = (9 << 16) | (25 << 2);
   public static NTFS_VOLUME_DATA_BUFFER GetNtfsVolumeData(VolumeInfo volume)
   {
    using (SafeFileHandle volumeHandle = KernelApi.NativeMethods.CreateFile(
     volume.VolumeId.Remove(volume.VolumeId.Length - 1),
     KernelApi.NativeMethods.GENERIC_READ, KernelApi.NativeMethods.FILE_SHARE_READ |
     KernelApi.NativeMethods.FILE_SHARE_WRITE, IntPtr.Zero,
     KernelApi.NativeMethods.OPEN_EXISTING, 0, IntPtr.Zero))
    {
     uint resultSize = 0;
     NTFS_VOLUME_DATA_BUFFER volumeData = new NTFS_VOLUME_DATA_BUFFER();
     if (DeviceIoControl(volumeHandle, FSCTL_GET_NTFS_VOLUME_DATA,
      IntPtr.Zero, 0, out volumeData, (uint)Marshal.SizeOf(volumeData),
      out resultSize, IntPtr.Zero))
     {
      return volumeData;
     }
     throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
    }
   }
   [DllImport("Kernel32.dll", CharSet = CharSet.Unicode)]
   [return: MarshalAs(UnmanagedType.Bool)]
   public static extern bool DeviceIoControl(SafeFileHandle hDevice,
    uint dwIoControlCode, IntPtr lpInBuffer, uint nInBufferSize,
    out NTFS_VOLUME_DATA_BUFFER lpOutBuffer, uint nOutBufferSize,
    out uint lpBytesReturned, IntPtr lpOverlapped);
  }
 }
}
