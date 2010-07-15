

using System;
using System.Collections.Generic;
using System.Text;

using System.IO;
using System.Drawing;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
using System.Globalization;
using System.Windows.Forms;

namespace Eraser.Util.ExtensionMethods
{



 public static class IO
 {
  public static DirectoryInfo GetParent(this FileSystemInfo info)
  {
   FileInfo file = info as FileInfo;
   DirectoryInfo directory = info as DirectoryInfo;
   if (file != null)
    return file.Directory;
   else if (directory != null)
    return directory.Parent;
   else
    throw new ArgumentException("Unknown FileSystemInfo type.");
  }
  public static void MoveTo(this FileSystemInfo info, string path)
  {
   FileInfo file = info as FileInfo;
   DirectoryInfo directory = info as DirectoryInfo;
   if (file != null)
    file.MoveTo(path);
   else if (directory != null)
    directory.MoveTo(path);
   else
    throw new ArgumentException("Unknown FileSystemInfo type.");
  }
  public static string GetCompactPath(this FileSystemInfo info, int newWidth, Font drawFont)
  {
   using (Control ctrl = new Control())
   {
    Graphics g = ctrl.CreateGraphics();
    string longPath = info.FullName;
    int width = g.MeasureString(longPath, drawFont).ToSize().Width;
    if (width <= newWidth)
     return longPath;
    int aveCharWidth = width / longPath.Length;
    int charCount = newWidth / aveCharWidth;
    StringBuilder builder = new StringBuilder();
    builder.Append(longPath);
    builder.EnsureCapacity(charCount);
    while (g.MeasureString(builder.ToString(), drawFont).Width > newWidth)
    {
     if (!NativeMethods.PathCompactPathEx(builder, longPath,
      (uint)charCount--, 0))
     {
      return string.Empty;
     }
    }
    return builder.ToString();
   }
  }
  public static bool IsCompressed(this FileSystemInfo info)
  {
   ushort compressionStatus = 0;
   uint bytesReturned = 0;
   using (SafeFileHandle handle = NativeMethods.CreateFile(info.FullName,
    NativeMethods.GENERIC_READ | NativeMethods.GENERIC_WRITE,
    0, IntPtr.Zero, NativeMethods.OPEN_EXISTING,
    NativeMethods.FILE_FLAG_BACKUP_SEMANTICS, IntPtr.Zero))
   {
    if (NativeMethods.DeviceIoControl(handle, NativeMethods.FSCTL_GET_COMPRESSION,
     IntPtr.Zero, 0, out compressionStatus, sizeof(ushort), out bytesReturned,
     IntPtr.Zero))
    {
     return compressionStatus != NativeMethods.COMPRESSION_FORMAT_NONE;
    }
   }
   return false;
  }
  public static bool Compress(this FileSystemInfo info)
  {
   return SetCompression(info.FullName, true);
  }
  public static bool Uncompress(this FileSystemInfo info)
  {
   return SetCompression(info.FullName, false);
  }
  private static bool SetCompression(string path, bool compressed)
  {
   ushort compressionStatus = compressed ?
    NativeMethods.COMPRESSION_FORMAT_DEFAULT :
    NativeMethods.COMPRESSION_FORMAT_NONE;
   uint bytesReturned = 0;
   using (SafeFileHandle handle = NativeMethods.CreateFile(path,
    NativeMethods.GENERIC_READ | NativeMethods.GENERIC_WRITE,
    0, IntPtr.Zero, NativeMethods.OPEN_EXISTING,
    NativeMethods.FILE_FLAG_BACKUP_SEMANTICS, IntPtr.Zero))
   {
    return NativeMethods.DeviceIoControl(handle, NativeMethods.FSCTL_SET_COMPRESSION,
     ref compressionStatus, sizeof(ushort), IntPtr.Zero, 0, out bytesReturned,
     IntPtr.Zero);
   }
  }
  public static string GetDescription(this FileSystemInfo info)
  {
   NativeMethods.SHFILEINFO shfi = new NativeMethods.SHFILEINFO();
   NativeMethods.SHGetFileInfo(info.FullName, 0, ref shfi, Marshal.SizeOf(shfi),
    NativeMethods.SHGetFileInfoFlags.SHGFI_DISPLAYNAME);
   return shfi.szDisplayName;
  }
  public static Icon GetIcon(this FileSystemInfo info)
  {
   NativeMethods.SHFILEINFO shfi = new NativeMethods.SHFILEINFO();
   NativeMethods.SHGetFileInfo(info.FullName, 0, ref shfi, Marshal.SizeOf(shfi),
    NativeMethods.SHGetFileInfoFlags.SHGFI_SMALLICON |
    NativeMethods.SHGetFileInfoFlags.SHGFI_ICON);
   if (shfi.hIcon != IntPtr.Zero)
    return Icon.FromHandle(shfi.hIcon);
   else
    throw new IOException(string.Format(CultureInfo.CurrentCulture,
     "Could not load file icon from {0}", info.FullName),
     Win32ErrorCode.GetExceptionForWin32Error(Marshal.GetLastWin32Error()));
  }
  public static bool IsProtectedSystemFile(this FileSystemInfo info)
  {
   return NativeMethods.SfcIsFileProtected(IntPtr.Zero, info.FullName);
  }
  [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1011:ConsiderPassingBaseTypesAsParameters")]
  public static IList<string> GetADSes(this FileInfo info)
  {
   List<string> result = new List<string>();
   using (FileStream stream = new StreamInfo(info.FullName).Open(FileMode.Open,
    FileAccess.Read, FileShare.ReadWrite))
   using (SafeFileHandle streamHandle = stream.SafeFileHandle)
   {
    NativeMethods.FILE_STREAM_INFORMATION[] streams = GetADSes(streamHandle);
    foreach (NativeMethods.FILE_STREAM_INFORMATION streamInfo in streams)
    {
     string streamName = streamInfo.StreamName.Substring(1,
      streamInfo.StreamName.LastIndexOf(':') - 1);
     if (streamName.Length != 0)
      result.Add(streamName);
    }
   }
   return result.AsReadOnly();
  }
  private static NativeMethods.FILE_STREAM_INFORMATION[] GetADSes(SafeFileHandle FileHandle)
  {
   NativeMethods.IO_STATUS_BLOCK status = new NativeMethods.IO_STATUS_BLOCK();
   IntPtr fileInfoPtr = IntPtr.Zero;
   try
   {
    NativeMethods.FILE_STREAM_INFORMATION streamInfo =
     new NativeMethods.FILE_STREAM_INFORMATION();
    int fileInfoPtrLength = (Marshal.SizeOf(streamInfo) + 32768) / 2;
    uint ntStatus = 0;
    do
    {
     fileInfoPtrLength *= 2;
     if (fileInfoPtr != IntPtr.Zero)
      Marshal.FreeHGlobal(fileInfoPtr);
     fileInfoPtr = Marshal.AllocHGlobal(fileInfoPtrLength);
     ntStatus = NativeMethods.NtQueryInformationFile(FileHandle, ref status,
      fileInfoPtr, (uint)fileInfoPtrLength,
      NativeMethods.FILE_INFORMATION_CLASS.FileStreamInformation);
    }
    while (ntStatus != 0 && ntStatus == 0x80000005 );
    List<NativeMethods.FILE_STREAM_INFORMATION> result =
     new List<NativeMethods.FILE_STREAM_INFORMATION>();
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
 }
}
