using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.IO;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 public abstract class FatFileSystem : WindowsFileSystem
 {
  public override void EraseOldFileSystemResidentFiles(VolumeInfo volume,
   DirectoryInfo tempDirectory, ErasureMethod method,
   FileSystemEntriesEraseProgress callback)
  {
  }
  public override void EraseFileSystemObject(StreamInfo info, ErasureMethod method,
   ErasureMethodProgressFunction callback)
  {
   long fileArea = GetFileArea(info.FullName);
   using (FileStream strm = info.Open(FileMode.Open, FileAccess.Write,
    FileShare.None, FileOptions.WriteThrough))
   {
    strm.SetLength(fileArea);
    if (strm.Length != 0)
    {
     method.Erase(strm, long.MaxValue,
      ManagerLibrary.Instance.PrngRegistrar[ManagerLibrary.Settings.ActivePrng],
      callback
     );
    }
    strm.Seek(0, SeekOrigin.Begin);
    strm.SetLength(0);
   }
  }
  public override void EraseDirectoryStructures(VolumeInfo info,
   FileSystemEntriesEraseProgress callback)
  {
   using (FileStream stream = info.Open(FileAccess.ReadWrite, FileShare.ReadWrite))
   {
    int directoriesCleaned = 0;
    FatApi api = GetFatApi(info, stream);
    HashSet<uint> eraseQueueClusters = new HashSet<uint>();
    List<FatDirectoryEntry> eraseQueue = new List<FatDirectoryEntry>();
    {
     FatDirectoryEntry entry = api.LoadDirectory(string.Empty);
     eraseQueue.Add(entry);
     eraseQueueClusters.Add(entry.Cluster);
    }
    using (VolumeLock volumeLock = info.LockVolume(stream))
    {
     while (eraseQueue.Count != 0)
     {
      if (callback != null)
       callback(directoriesCleaned, directoriesCleaned + eraseQueue.Count);
      FatDirectoryBase currentDir = api.LoadDirectory(eraseQueue[0].FullName);
      eraseQueue.RemoveAt(0);
      foreach (KeyValuePair<string, FatDirectoryEntry> entry in currentDir.Items)
       if (entry.Value.EntryType == FatDirectoryEntryType.Directory)
       {
        if (eraseQueueClusters.Contains(entry.Value.Cluster))
         continue;
        eraseQueueClusters.Add(entry.Value.Cluster);
        eraseQueue.Add(entry.Value);
       }
      currentDir.ClearDeletedEntries();
      ++directoriesCleaned;
     }
    }
   }
  }
  protected override DateTime MinTimestamp
  {
   get
   {
    return new DateTime(1980, 1, 1, 0, 0, 0);
   }
  }
  protected abstract FatApi GetFatApi(VolumeInfo info, FileStream stream);
 }
 [Guid("36C78D78-7EE4-4304-8068-10755651AF2D")]
 public class Fat12FileSystem : FatFileSystem
 {
  public override Guid Guid
  {
   get { return GetType().GUID; }
  }
  public override string Name
  {
   get { return "FAT12"; }
  }
  protected override FatApi GetFatApi(VolumeInfo info, FileStream stream)
  {
   return new Fat12Api(info, stream);
  }
 }
 [Guid("8C9DF746-1CD6-435d-8D04-3FE40A0A1C83")]
 public class Fat16FileSystem : FatFileSystem
 {
  public override Guid Guid
  {
   get { return GetType().GUID; }
  }
  public override string Name
  {
   get { return "FAT16"; }
  }
  protected override FatApi GetFatApi(VolumeInfo info, FileStream stream)
  {
   return new Fat16Api(info, stream);
  }
 }
 [Guid("1FCD66DC-179D-4402-8FF8-D19F74A4C398")]
 public class Fat32FileSystem : FatFileSystem
 {
  public override Guid Guid
  {
   get { return GetType().GUID; }
  }
  public override string Name
  {
   get { return "FAT32"; }
  }
  protected override FatApi GetFatApi(VolumeInfo info, FileStream stream)
  {
   return new Fat32Api(info, stream);
  }
 }
}
