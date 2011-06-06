using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.ComponentModel;
using System.Windows.Forms;
using System.Drawing;
using System.IO;
using Microsoft.Win32.SafeHandles;
using System.Globalization;
namespace Eraser.Util
{
 public static class File
 {
  public static ICollection<string> GetADSes(FileInfo info)
  {
   List<string> result = new List<string>();
   using (FileStream stream = new StreamInfo(info.FullName).Open(FileMode.Open,
    FileAccess.Read, FileShare.ReadWrite))
   using (SafeFileHandle streamHandle = stream.SafeFileHandle)
   {
    NTApi.NativeMethods.FILE_STREAM_INFORMATION[] streams =
     NTApi.NativeMethods.NtQueryInformationFile(streamHandle);
    foreach (NTApi.NativeMethods.FILE_STREAM_INFORMATION streamInfo in streams)
    {
     string streamName = streamInfo.StreamName.Substring(1,
      streamInfo.StreamName.LastIndexOf(':') - 1);
     if (streamName.Length != 0)
      result.Add(streamName);
    }
   }
   return result;
  }
  public static string GetFileDescription(string path)
  {
   ShellApi.NativeMethods.SHFILEINFO shfi = new ShellApi.NativeMethods.SHFILEINFO();
   ShellApi.NativeMethods.SHGetFileInfo(path, 0, ref shfi, Marshal.SizeOf(shfi),
    ShellApi.NativeMethods.SHGetFileInfoFlags.SHGFI_DISPLAYNAME);
   return shfi.szDisplayName;
  }
  public static Icon GetFileIcon(string path)
  {
   ShellApi.NativeMethods.SHFILEINFO shfi = new ShellApi.NativeMethods.SHFILEINFO();
   ShellApi.NativeMethods.SHGetFileInfo(path, 0, ref shfi, Marshal.SizeOf(shfi),
    ShellApi.NativeMethods.SHGetFileInfoFlags.SHGFI_SMALLICON |
    ShellApi.NativeMethods.SHGetFileInfoFlags.SHGFI_ICON);
   if (shfi.hIcon != IntPtr.Zero)
    return Icon.FromHandle(shfi.hIcon);
   else
    throw new IOException(string.Format(CultureInfo.CurrentCulture,
     "Could not load file icon from {0}", path),
     Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error()));
  }
  public static string GetCompactPath(string longPath, int newWidth, Font drawFont)
  {
   using (Control ctrl = new Control())
   {
    Graphics g = ctrl.CreateGraphics();
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
     if (!ShellApi.NativeMethods.PathCompactPathEx(builder, longPath,
      (uint)charCount--, 0))
     {
      return string.Empty;
     }
    }
    return builder.ToString();
   }
  }
  public static bool IsProtectedSystemFile(string filePath)
  {
   return SfcIsFileProtected(IntPtr.Zero, filePath);
  }
  public static bool IsCompressed(string path)
  {
   ushort compressionStatus = 0;
   uint bytesReturned = 0;
   using (FileStream strm = new FileStream(
    KernelApi.NativeMethods.CreateFile(path,
    KernelApi.NativeMethods.GENERIC_READ | KernelApi.NativeMethods.GENERIC_WRITE,
    0, IntPtr.Zero, KernelApi.NativeMethods.OPEN_EXISTING,
    KernelApi.NativeMethods.FILE_FLAG_BACKUP_SEMANTICS, IntPtr.Zero), FileAccess.Read))
   {
    if (KernelApi.NativeMethods.DeviceIoControl(strm.SafeFileHandle,
     KernelApi.NativeMethods.FSCTL_GET_COMPRESSION, IntPtr.Zero, 0,
     out compressionStatus, sizeof(ushort), out bytesReturned, IntPtr.Zero))
    {
     return compressionStatus != KernelApi.NativeMethods.COMPRESSION_FORMAT_NONE;
    }
   }
   return false;
  }
  public static bool SetCompression(string path, bool compressed)
  {
   ushort compressionStatus = compressed ?
    KernelApi.NativeMethods.COMPRESSION_FORMAT_DEFAULT :
    KernelApi.NativeMethods.COMPRESSION_FORMAT_NONE;
   uint bytesReturned = 0;
   using (FileStream strm = new FileStream(
    KernelApi.NativeMethods.CreateFile(path,
    KernelApi.NativeMethods.GENERIC_READ | KernelApi.NativeMethods.GENERIC_WRITE,
    0, IntPtr.Zero, KernelApi.NativeMethods.OPEN_EXISTING,
    KernelApi.NativeMethods.FILE_FLAG_BACKUP_SEMANTICS, IntPtr.Zero), FileAccess.ReadWrite))
   {
    return KernelApi.NativeMethods.DeviceIoControl(strm.SafeFileHandle,
     KernelApi.NativeMethods.FSCTL_SET_COMPRESSION, ref compressionStatus,
     sizeof(ushort), IntPtr.Zero, 0, out bytesReturned, IntPtr.Zero);
   }
  }
  [DllImport("Sfc.dll", CharSet = CharSet.Unicode)]
  [return: MarshalAs(UnmanagedType.Bool)]
  private static extern bool SfcIsFileProtected(IntPtr RpcHandle,
   string ProtFileName);
  public static string GetHumanReadableFilesize(long bytes)
  {
   string[] units = new string[] {
    "bytes",
    "KB",
    "MB",
    "GB",
    "TB",
    "PB",
    "EB"
   };
   double dBytes = (double)bytes;
   for (int i = 0; i != units.Length; ++i)
   {
    if (dBytes < 1020.0)
     if (i <= 1)
      return string.Format(CultureInfo.CurrentCulture,
       "{0} {1}", (int)dBytes, units[i]);
     else
      return string.Format(CultureInfo.CurrentCulture,
       "{0:0.00} {1}", dBytes, units[i]);
    dBytes /= 1024.0;
   }
   return string.Format(CultureInfo.CurrentCulture, "{0, 2} {1}",
    dBytes, units[units.Length - 1]);
  }
 }
}
