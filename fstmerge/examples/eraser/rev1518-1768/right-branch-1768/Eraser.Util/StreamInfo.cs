

using System;
using System.Collections.Generic;
using System.Text;

using Microsoft.Win32.SafeHandles;
using System.IO;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace Eraser.Util
{




 public class StreamInfo : FileSystemInfo
 {




  public StreamInfo(string filename)
   : this(filename, null)
  {
  }







  public StreamInfo(string filename, string streamName)
  {
   OriginalPath = filename;
   FullPath = Path.GetFullPath(filename);
   FileName = FullPath;
   StreamName = streamName;

   if (!string.IsNullOrEmpty(streamName))
   {
    OriginalPath += ":" + streamName;
    FullPath += ":" + streamName;
   }

   Refresh();
  }




  public override string FullName
  {
   get
   {
    return FullPath;
   }
  }




  public override bool Exists
  {
   get
   {
    bool result = System.IO.File.Exists(FullName);
    return result &&
     (string.IsNullOrEmpty(StreamName) || true);
   }
  }




  public String DirectoryName
  {
   get
   {
    return Path.GetDirectoryName(FullPath);
   }
  }




  public DirectoryInfo Directory
  {
   get
   {
    return new DirectoryInfo(DirectoryName);
   }
  }




  public FileInfo File
  {
   get
   {
    return new FileInfo(FileName);
   }
  }




  public string FileName
  {
   get;
   private set;
  }




  public override string Name
  {
   get { return StreamName; }
  }




  public bool IsReadOnly
  {
   get { return (Attributes & FileAttributes.ReadOnly) != 0; }
   set
   {
    Attributes = value ?
     (Attributes | FileAttributes.ReadOnly) :
     (Attributes & ~FileAttributes.ReadOnly);
   }
  }




  public long Length
  {
   get
   {
    long fileSize;
    using (SafeFileHandle handle = OpenHandle(
     FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None))
    {
     if (NativeMethods.GetFileSizeEx(handle, out fileSize))
      return fileSize;
    }

    return 0;
   }
  }





  public FileStream Create()
  {
   return Open(FileMode.Create, FileAccess.ReadWrite, FileShare.None, FileOptions.None);
  }





  public override void Delete()
  {
   if (!NativeMethods.DeleteFile(FullName))
   {
    int errorCode = Marshal.GetLastWin32Error();
    switch (errorCode)
    {
     case Win32ErrorCode.PathNotFound:
      break;
     default:
      throw Win32ErrorCode.GetExceptionForWin32Error(errorCode);
    }
   }
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
    throw Win32ErrorCode.GetExceptionForWin32Error(Marshal.GetLastWin32Error());
   return new FileStream(handle, access);
  }
  public FileStream OpenRead()
  {
   return Open(FileMode.Open, FileAccess.Read, FileShare.Read, FileOptions.None);
  }
  public FileStream OpenWrite()
  {
   return Open(FileMode.OpenOrCreate, FileAccess.Write, FileShare.None, FileOptions.None);
  }
  private SafeFileHandle OpenHandle(FileMode mode, FileAccess access, FileShare share,
   FileOptions options)
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
   SafeFileHandle result = NativeMethods.CreateFile(FullName, iAccess,
    (uint)share, IntPtr.Zero, (uint)mode, (uint)options, IntPtr.Zero);
   if (result.IsInvalid)
   {
    int errorCode = Marshal.GetLastWin32Error();
    result.Close();
    throw Win32ErrorCode.GetExceptionForWin32Error(errorCode);
   }
   return result;
  }
  public override string ToString()
  {
   return OriginalPath;
  }
  private string StreamName;
 }
}
