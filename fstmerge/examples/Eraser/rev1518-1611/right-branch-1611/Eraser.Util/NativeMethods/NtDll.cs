using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
namespace Eraser.Util
{
 internal static partial class NativeMethods
 {
  [DllImport("NtDll.dll")]
  public static extern uint NtQuerySystemInformation(uint dwType, byte[] dwData,
   uint dwMaxSize, out uint dwDataSize);
  [DllImport("NtDll.dll")]
  public static extern uint NtQueryInformationFile(SafeFileHandle FileHandle,
   ref IO_STATUS_BLOCK IoStatusBlock, IntPtr FileInformation, uint Length,
   FILE_INFORMATION_CLASS FileInformationClass);
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
 }
}
