

using System;
using System.Collections.Generic;
using System.Text;

using Microsoft.Win32.SafeHandles;
using System.IO;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace Eraser.Util
{
 public class StreamInfo
 {






  public StreamInfo(string path)
  {

   if (path.IndexOf(':') != path.LastIndexOf(':'))
   {
    int streamNameColon = path.IndexOf(':', path.IndexOf(':') + 1);
    fileName = path.Substring(0, streamNameColon);
    streamName = path.Substring(streamNameColon + 1);
   }
   else
   {
    fileName = path;
   }
  }




  public DirectoryInfo Directory
  {
   get
   {
    return new DirectoryInfo(DirectoryName);
   }
  }




  public string DirectoryName
  {
   get
   {
    return fileName.Substring(0, fileName.LastIndexOf(Path.DirectorySeparatorChar) + 1);
   }
  }




  public string FullName
  {
   get
   {
    if (streamName != null)
     return fileName + ':' + streamName;
    return fileName;
   }
  }




  public string Name
  {
   get { return fileName; }
  }





  public FileInfo File
  {
   get
   {
    if (streamName == null)
     return new FileInfo(fileName);
    return null;
   }
  }




  public FileAttributes Attributes
  {
   get { return (FileAttributes)KernelApi.NativeMethods.GetFileAttributes(FullName); }
   set { KernelApi.NativeMethods.SetFileAttributes(FullName, (uint)value); }
  }




  public bool Exists
  {
   get
   {
    using (SafeFileHandle handle = KernelApi.NativeMethods.CreateFile(
     FullName, KernelApi.NativeMethods.GENERIC_READ,
     KernelApi.NativeMethods.FILE_SHARE_READ, IntPtr.Zero,
     KernelApi.NativeMethods.OPEN_EXISTING, 0, IntPtr.Zero))
    {
     if (!handle.IsInvalid)
      return true;
     else
      switch (Marshal.GetLastWin32Error())
      {
       case 5:
       case 32:
        return true;

       case 2:
       case 3:
       default:
        return false;
      }
    }
   }
  }




  public bool IsReadOnly
  {
   get
   {
    return (Attributes & FileAttributes.ReadOnly) != 0;
   }

   set
   {
    if (value)
     Attributes |= FileAttributes.ReadOnly;
    else
     Attributes &= ~FileAttributes.ReadOnly;
   }
  }




  public long Length
  {
   get
   {
    long fileSize;
    using (SafeFileHandle handle = fileHandle)
     if (KernelApi.NativeMethods.GetFileSizeEx(handle, out fileSize))
      return fileSize;

    return 0;
   }
  }

  public DateTime LastAccessTime
  {
   get
   {
    DateTime creationTime, lastAccess, lastWrite;
    GetFileTime(out creationTime, out lastAccess, out lastWrite);
    return lastAccess;
   }
   set
   {
    SetFileTime(DateTime.MinValue, value, DateTime.MinValue);
   }
  }

  public DateTime LastWriteTime
  {
   get
   {
    DateTime creationTime, lastAccess, lastWrite;
    GetFileTime(out creationTime, out lastAccess, out lastWrite);
    return lastWrite;
   }
   set
   {
    SetFileTime(DateTime.MinValue, DateTime.MinValue, value);
   }
  }

  public DateTime CreationTime
  {
   get
   {
    DateTime creationTime, lastAccess, lastWrite;
    GetFileTime(out creationTime, out lastAccess, out lastWrite);
    return creationTime;
   }
   set
   {
    SetFileTime(value, DateTime.MinValue, DateTime.MinValue);
   }
  }

  private void GetFileTime(out DateTime creationTime, out DateTime lastAccess,
   out DateTime lastWrite)
  {
   SafeFileHandle handle = exclusiveHandle;
   bool ownsHandle = false;
   try
   {
    if (handle == null || handle.IsClosed || handle.IsInvalid)
    {
     handle = fileHandle;
     ownsHandle = true;
    }
   }
   catch (ObjectDisposedException)
   {
    handle = fileHandle;
    ownsHandle = true;
   }

   try
   {
    KernelApi.GetFileTime(handle, out creationTime, out lastAccess, out lastWrite);
   }
   finally
   {
    if (ownsHandle)
     handle.Close();
   }
  }

  private void SetFileTime(DateTime creationTime, DateTime lastAccess, DateTime lastWrite)
  {
   SafeFileHandle handle = exclusiveHandle;
   bool ownsHandle = false;
   try
   {
    if (handle == null || handle.IsClosed || handle.IsInvalid)
    {
     handle = fileHandle;
     ownsHandle = true;
    }
   }
   catch (ObjectDisposedException)
   {
    handle = fileHandle;
    ownsHandle = true;
   }

   try
   {
    KernelApi.SetFileTime(handle, creationTime, lastAccess, lastWrite);
   }
   finally
   {
    if (ownsHandle)
     handle.Close();
   }
  }




  public void Delete()
  {
   if (streamName == null)
    File.Delete();
   else
    if (!KernelApi.NativeMethods.DeleteFile(FullName))
     throw KernelApi.GetExceptionForWin32Error(Marshal.GetLastWin32Error());
  }
  public FileStream Open(FileMode mode)
  {
   return Open(mode, FileAccess.ReadWrite, FileShare.None, FileOptions.None);
  }
  public FileStream Open(FileMode mode, FileAccess access)
  {
   return Open(mode, access, FileShare.None, FileOptions.None);
  }
  public FileStream Open(FileMode mode, FileAccess access, FileShare share)
  {
   return Open(mode, access, share, FileOptions.None);
  }
  public FileStream Open(FileMode mode, FileAccess access, FileShare share,
   FileOptions options)
  {
   SafeFileHandle handle = OpenHandle(mode, access, share, options);
   if (handle.IsInvalid)
    throw Marshal.GetExceptionForHR(Marshal.GetHRForLastWin32Error());
   return new FileStream(handle, access);
  }
  private SafeFileHandle OpenHandle(FileMode mode, FileAccess access, FileShare share,
   FileOptions options)
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
   SafeFileHandle result = KernelApi.NativeMethods.CreateFile(FullName, iAccess,
    (uint)share, IntPtr.Zero, (uint)mode, (uint)options, IntPtr.Zero);
   if (result.IsInvalid)
    throw KernelApi.GetExceptionForWin32Error(Marshal.GetLastWin32Error());
   if (share == FileShare.None)
    exclusiveHandle = result;
   return result;
  }
  public override string ToString()
  {
   return FullName;
  }
  private SafeFileHandle fileHandle
  {
   get
   {
    return OpenHandle(FileMode.Open, FileAccess.Read, FileShare.ReadWrite |
     FileShare.Delete, FileOptions.None);
   }
  }
  private SafeFileHandle exclusiveHandle;
  private string fileName;
  private string streamName;
 }
}
