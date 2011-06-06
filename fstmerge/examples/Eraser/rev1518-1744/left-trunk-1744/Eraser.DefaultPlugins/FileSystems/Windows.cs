using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Threading;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser.DefaultPlugins
{
 public abstract class WindowsFileSystem : FileSystem
 {
  public override void DeleteFile(FileInfo info)
  {
   info.Attributes = FileAttributes.NotContentIndexed;
   info.CreationTime = info.LastWriteTime = info.LastAccessTime = MinTimestamp;
   string newPath = GenerateRandomFileName(info.Directory, info.Name.Length);
   for (int i = 0, tries = 0; i < FileNameErasePasses; ++tries)
   {
    try
    {
     info.MoveTo(newPath);
     ++i;
    }
    catch (IOException e)
    {
     switch (System.Runtime.InteropServices.Marshal.GetLastWin32Error())
     {
      case 5:
       throw new UnauthorizedAccessException(S._("The file {0} could not " +
        "be erased because the file's permissions prevent access to the file.",
        info.FullName));
      case 32:
       if (tries > FileNameEraseTries)
        throw new IOException(S._("The file {0} is currently in use and " +
         "cannot be removed.", info.FullName), e);
       Thread.Sleep(100);
       break;
      default:
       throw;
     }
    }
   }
   if (Manager.ManagerLibrary.Settings.PlausibleDeniability)
   {
    using (FileStream fileStream = info.OpenWrite())
     CopyPlausibleDeniabilityFile(fileStream);
   }
   for (int i = 0; i < FileNameEraseTries; ++i)
    try
    {
     info.Delete();
     break;
    }
    catch (IOException e)
    {
     switch (System.Runtime.InteropServices.Marshal.GetLastWin32Error())
     {
      case 5:
       throw new UnauthorizedAccessException(S._("The file {0} could not " +
        "be erased because the file's permissions prevent access to the file.",
        info.FullName), e);
      case 32:
       if (i > FileNameEraseTries)
        throw new IOException(S._("The file {0} is currently in use and " +
         "cannot be removed.", info.FullName), e);
       Thread.Sleep(100);
       break;
      default:
       throw;
     }
    }
  }
  public override void DeleteFolder(DirectoryInfo info, bool recursive)
  {
   if (!recursive && info.GetFileSystemInfos().Length != 0)
    throw new InvalidOperationException(S._("The folder {0} cannot be deleted as it is " +
     "not empty."));
   if (!info.Exists)
    return;
   foreach (DirectoryInfo dir in info.GetDirectories())
    DeleteFolder(dir);
   foreach (FileInfo file in info.GetFiles())
    DeleteFile(file);
   if (info.Parent == null)
    return;
   for (int i = 0, tries = 0; i < FileNameErasePasses; ++tries)
   {
    string newPath = GenerateRandomFileName(info.Parent, info.Name.Length);
    try
    {
     info.MoveTo(newPath);
     ++i;
    }
    catch (IOException e)
    {
     switch (System.Runtime.InteropServices.Marshal.GetLastWin32Error())
     {
      case 5:
       throw new UnauthorizedAccessException(S._("The file {0} could not " +
        "be erased because the file's permissions prevent access to the file.",
        info.FullName), e);
      case 32:
       if (tries > FileNameEraseTries)
        throw new IOException(S._("The file {0} is currently in use and " +
         "cannot be removed.", info.FullName), e);
       Thread.Sleep(100);
       break;
      default:
       throw;
     }
    }
   }
   info.CreationTime = info.LastWriteTime = info.LastAccessTime = MinTimestamp;
   info.Delete(true);
  }
  public override void EraseClusterTips(VolumeInfo info, ErasureMethod method,
   Logger log, ClusterTipsSearchProgress searchCallback,
   ClusterTipsEraseProgress eraseCallback)
  {
   List<string> files = new List<string>();
   if (!info.IsMounted)
    throw new InvalidOperationException(S._("Could not erase cluster tips in {0} " +
     "as the volume is not mounted.", info.VolumeId));
   ListFiles(new DirectoryInfo(info.MountPoints[0]), files, log, searchCallback);
   for (int i = 0, j = files.Count; i != j; ++i)
   {
    StreamInfo streamInfo = new StreamInfo(files[i]);
    FileAttributes fileAttr = streamInfo.Attributes;
    try
    {
     streamInfo.Attributes = FileAttributes.Normal;
     EraseFileClusterTips(files[i], method);
    }
    catch (UnauthorizedAccessException)
    {
     log.LastSessionEntries.Add(new LogEntry(S._("{0} did not have its " +
      "cluster tips erased because you do not have the required permissions to " +
      "erase the file cluster tips.", files[i]), LogLevel.Information));
    }
    catch (IOException e)
    {
     log.LastSessionEntries.Add(new LogEntry(S._("{0} did not have its " +
      "cluster tips erased. The error returned was: {1}", files[i],
      e.Message), LogLevel.Error));
    }
    finally
    {
     streamInfo.Attributes = fileAttr;
    }
    eraseCallback(i, files.Count, files[i]);
   }
  }
  private void ListFiles(DirectoryInfo info, List<string> files, Logger log,
   ClusterTipsSearchProgress searchCallback)
  {
   try
   {
    if ((info.Attributes & FileAttributes.ReparsePoint) != 0)
    {
     log.LastSessionEntries.Add(new LogEntry(S._("Files in {0} did " +
      "not have their cluster tips erased because it is a hard link or " +
      "a symbolic link.", info.FullName), LogLevel.Information));
     return;
    }
    foreach (FileInfo file in info.GetFiles())
     if (Util.File.IsProtectedSystemFile(file.FullName))
      log.LastSessionEntries.Add(new LogEntry(S._("{0} did not have " +
       "its cluster tips erased, because it is a system file",
       file.FullName), LogLevel.Information));
     else if ((file.Attributes & FileAttributes.ReparsePoint) != 0)
      log.LastSessionEntries.Add(new LogEntry(S._("{0} did not have " +
       "its cluster tips erased because it is a hard link or a " +
       "symbolic link.", file.FullName), LogLevel.Information));
     else if ((file.Attributes & FileAttributes.Compressed) != 0 ||
      (file.Attributes & FileAttributes.Encrypted) != 0 ||
      (file.Attributes & FileAttributes.SparseFile) != 0)
     {
      log.LastSessionEntries.Add(new LogEntry(S._("{0} did not have " +
       "its cluster tips erased because it is compressed, encrypted " +
       "or a sparse file.", file.FullName), LogLevel.Information));
     }
     else
     {
      try
      {
       foreach (string i in Util.File.GetADSes(file))
        files.Add(file.FullName + ':' + i);
       files.Add(file.FullName);
      }
      catch (UnauthorizedAccessException e)
      {
       log.LastSessionEntries.Add(new LogEntry(S._("{0} did not " +
        "have its cluster tips erased because of the following " +
        "error: {1}", info.FullName, e.Message), LogLevel.Error));
      }
      catch (IOException e)
      {
       log.LastSessionEntries.Add(new LogEntry(S._("{0} did not " +
        "have its cluster tips erased because of the following " +
        "error: {1}", info.FullName, e.Message), LogLevel.Error));
      }
     }
    foreach (DirectoryInfo subDirInfo in info.GetDirectories())
    {
     searchCallback(subDirInfo.FullName);
     ListFiles(subDirInfo, files, log, searchCallback);
    }
   }
   catch (UnauthorizedAccessException e)
   {
    log.LastSessionEntries.Add(new LogEntry(S._("{0} did not have its " +
     "cluster tips erased because of the following error: {1}",
     info.FullName, e.Message), LogLevel.Error));
   }
   catch (IOException e)
   {
    log.LastSessionEntries.Add(new LogEntry(S._("{0} did not have its " +
     "cluster tips erased because of the following error: {1}",
     info.FullName, e.Message), LogLevel.Error));
   }
  }
  private void EraseFileClusterTips(string file, ErasureMethod method)
  {
   StreamInfo streamInfo = new StreamInfo(file);
   DateTime lastAccess = streamInfo.LastAccessTime;
   DateTime lastWrite = streamInfo.LastWriteTime;
   DateTime created = streamInfo.CreationTime;
   long fileArea = GetFileArea(file);
   long fileLength = streamInfo.Length;
   if (fileArea == fileLength)
    return;
   using (FileStream stream = streamInfo.Open(FileMode.Open, FileAccess.Write,
    FileShare.None, FileOptions.WriteThrough))
   {
    try
    {
     stream.SetLength(fileArea);
     stream.Seek(fileLength, SeekOrigin.Begin);
     method.Erase(stream, long.MaxValue, PrngManager.GetInstance(
      ManagerLibrary.Settings.ActivePrng), null);
    }
    finally
    {
     stream.SetLength(fileLength);
     streamInfo.LastAccessTime = lastAccess;
     streamInfo.LastWriteTime = lastWrite;
     streamInfo.CreationTime = created;
    }
   }
  }
  public override long GetFileArea(string filePath)
  {
   StreamInfo info = new StreamInfo(filePath);
   VolumeInfo volume = VolumeInfo.FromMountpoint(info.Directory.FullName);
   long clusterSize = volume.ClusterSize;
   return (info.Length + (clusterSize - 1)) & ~(clusterSize - 1);
  }
  protected abstract DateTime MinTimestamp { get; }
 }
}
