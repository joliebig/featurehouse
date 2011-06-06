using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 public class NtfsFileSystem : WindowsFileSystem
 {
  public override bool Supports(string fileSystemName)
  {
   if (fileSystemName == "NTFS")
    return true;
   return false;
  }
  public override void EraseOldFileSystemResidentFiles(VolumeInfo volume,
   DirectoryInfo tempDirectory, ErasureMethod method,
   FileSystemEntriesEraseProgress callback)
  {
   try
   {
    long oldMFTSize = NtfsApi.GetMftValidSize(volume);
    for ( ; ; )
    {
     using (FileStream strm = new FileStream(
      GenerateRandomFileName(tempDirectory, 18), FileMode.CreateNew,
      FileAccess.Write, FileShare.None, 8, FileOptions.WriteThrough))
     {
      long streamSize = 0;
      try
      {
       while (true)
       {
        strm.SetLength(++streamSize);
        method.Erase(strm, long.MaxValue,
         PrngManager.GetInstance(ManagerLibrary.Settings.ActivePrng),
         null);
        if (callback != null)
         callback(0, 1);
       }
      }
      catch (IOException)
      {
       if (streamSize == 1)
        return;
      }
     }
     if (NtfsApi.GetMftValidSize(volume) > oldMFTSize)
      break;
    }
   }
   catch (IOException)
   {
   }
  }
  public override void EraseDirectoryStructures(VolumeInfo info,
   FileSystemEntriesEraseProgress callback)
  {
   DirectoryInfo tempDir = new DirectoryInfo(FileSystem.GenerateRandomFileName(
    new DirectoryInfo(info.MountPoints[0]), 32));
   tempDir.Create();
   try
   {
    long mftSize = NtfsApi.GetMftValidSize(info);
    long mftRecordSegmentSize = NtfsApi.GetMftRecordSegmentSize(info);
    int pollingInterval = (int)Math.Min(Math.Max(1, mftSize / info.ClusterSize / 20), 128);
    int totalFiles = (int)Math.Max(1L, mftSize / mftRecordSegmentSize);
    int filesCreated = 0;
    while (true)
    {
     ++filesCreated;
     using (FileStream strm = new FileStream(FileSystem.GenerateRandomFileName(
      tempDir, 220), FileMode.CreateNew, FileAccess.Write))
     {
     }
     if (filesCreated % pollingInterval == 0)
     {
      if (callback != null)
      {
       int halfFilesCreated = filesCreated / 2;
       callback(halfFilesCreated, Math.Max(halfFilesCreated, totalFiles));
      }
      if (mftSize < NtfsApi.GetMftValidSize(info))
       break;
     }
    }
   }
   catch (IOException)
   {
   }
   finally
   {
    FileInfo[] files = tempDir.GetFiles("*", SearchOption.AllDirectories);
    for (int i = 0; i < files.Length; ++i)
    {
     if (callback != null && i % 50 == 0)
      callback(files.Length + i, files.Length * 2);
     DeleteFile(files[i]);
    }
    DeleteFolder(tempDir);
   }
  }
  public override void EraseFileSystemObject(StreamInfo info, ErasureMethod method,
   ErasureMethodProgressFunction callback)
  {
   VolumeInfo volume = VolumeInfo.FromMountpoint(info.DirectoryName);
   if (info.Length < Math.Max(volume.ClusterSize, 1024))
   {
    using (FileStream strm = info.Open(FileMode.Open, FileAccess.Write,
     FileShare.None))
    {
     method.Erase(strm, long.MaxValue,
      PrngManager.GetInstance(ManagerLibrary.Settings.ActivePrng), null);
    }
   }
   long fileArea = GetFileArea(info.FullName);
   if (fileArea == 0)
    return;
   using (FileStream strm = info.Open(FileMode.Open, FileAccess.Write,
    FileShare.None, FileOptions.WriteThrough))
   {
    strm.SetLength(fileArea);
    method.Erase(strm, long.MaxValue,
     PrngManager.GetInstance(ManagerLibrary.Settings.ActivePrng),
     callback
    );
    strm.Seek(0, SeekOrigin.Begin);
    strm.SetLength(0);
   }
  }
  protected override DateTime MinTimestamp
  {
   get
   {
    return new DateTime(1601, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc);
   }
  }
 }
}
