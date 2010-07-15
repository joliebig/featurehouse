

using System;
using System.Collections.Generic;
using System.Text;
using Eraser.Util;
using System.IO;

namespace Eraser.Manager
{



 public abstract class FileSystem
 {
  public static string GenerateRandomFileName(DirectoryInfo info, int length)
  {
   Prng prng = PrngManager.GetInstance(ManagerLibrary.Settings.ActivePrng);
   string resultPrefix = info == null ? string.Empty : info.FullName +
    Path.DirectorySeparatorChar;
   byte[] resultAry = new byte[length];
   string result = string.Empty;
   do
   {
    prng.NextBytes(resultAry);
    string validFileNameChars = "0123456789abcdefghijklmnopqrstuvwxyz" +
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ _+=-()[]{}',`~!";
    for (int j = 0, k = resultAry.Length; j < k; ++j)
     resultAry[j] = (byte)validFileNameChars[
      (int)resultAry[j] % validFileNameChars.Length];
    result = Encoding.UTF8.GetString(resultAry);
   }
   while (info != null && (Directory.Exists(resultPrefix + result) ||
    File.Exists(resultPrefix + result)));
   return resultPrefix + result;
  }
  public static string GetRandomFile(DirectoryInfo info)
  {
   FileSystemInfo[] entries = null;
   try
   {
    entries = info.GetFileSystemInfos();
   }
   catch (DirectoryNotFoundException)
   {
    return string.Empty;
   }
   if (entries.Length == 0)
    return string.Empty;
   Prng prng = PrngManager.GetInstance(ManagerLibrary.Settings.ActivePrng);
   string result = string.Empty;
   while (result.Length == 0)
   {
    int index = prng.Next(entries.Length - 1);
    if (entries[index] is DirectoryInfo)
     result = GetRandomFile((DirectoryInfo)entries[index]);
    else
     result = ((FileInfo)entries[index]).FullName;
   }
   return result;
  }
  protected static void CopyPlausibleDeniabilityFile(Stream stream)
  {
   FileInfo shadowFileInfo;
   {
    string shadowFile = null;
    List<string> entries = new List<string>(
     ManagerLibrary.Settings.PlausibleDeniabilityFiles);
    Prng prng = PrngManager.GetInstance(ManagerLibrary.Settings.ActivePrng);
    do
    {
     if (entries.Count == 0)
      throw new FatalException(S._("Plausible deniability was selected, " +
       "but no decoy files were found. The current file has been only " +
       "replaced with random data."));
     int index = prng.Next(entries.Count - 1);
     if ((File.GetAttributes(entries[index]) & FileAttributes.Directory) != 0)
     {
      DirectoryInfo dir = new DirectoryInfo(entries[index]);
      FileInfo[] files = dir.GetFiles("*", SearchOption.AllDirectories);
      foreach (FileInfo f in files)
       entries.Add(f.FullName);
     }
     else
      shadowFile = entries[index];
     entries.RemoveAt(index);
    }
    while (shadowFile == null || shadowFile.Length == 0 || !File.Exists(shadowFile));
    shadowFileInfo = new FileInfo(shadowFile);
   }
   long amountToCopy = Math.Min(stream.Length,
    Math.Min(4 * 1024 * 1024, shadowFileInfo.Length));
   using (FileStream shadowFileStream = shadowFileInfo.OpenRead())
   {
    while (stream.Position < amountToCopy)
    {
     byte[] buf = new byte[524288];
     int bytesRead = shadowFileStream.Read(buf, 0, buf.Length);
     if (bytesRead == 0)
      break;
     stream.Write(buf, 0,
      (int)Math.Min(bytesRead, amountToCopy - stream.Position));
    }
   }
  }
  public abstract bool Supports(string fileSystemName);
  public abstract void DeleteFile(FileInfo info);
  public abstract void DeleteFolder(DirectoryInfo info, bool recursive);
  public void DeleteFolder(DirectoryInfo info)
  {
   DeleteFolder(info, true);
  }
  public abstract void EraseClusterTips(VolumeInfo info, ErasureMethod method,
   Logger log, ClusterTipsSearchProgress searchCallback,
   ClusterTipsEraseProgress eraseCallback);
  public abstract void EraseOldFileSystemResidentFiles(VolumeInfo volume,
   DirectoryInfo tempDirectory, ErasureMethod method,
   FileSystemEntriesEraseProgress callback);
  public abstract void EraseDirectoryStructures(VolumeInfo info,
   FileSystemEntriesEraseProgress callback);
  public abstract void EraseFileSystemObject(StreamInfo info, ErasureMethod method,
   ErasureMethodProgressFunction callback);
  public abstract long GetFileArea(string filePath);
  public const int FileNameErasePasses = 7;
  public const int FileNameEraseTries = 50;
 }
 public delegate void ClusterTipsSearchProgress(string currentPath);
 public delegate void ClusterTipsEraseProgress(int currentFile, int totalFiles,
  string currentFilePath);
 public delegate void FileSystemEntriesEraseProgress(int currentFile, int totalFiles);
 public class FileSystemManager
 {
  public static FileSystem Get(VolumeInfo volume)
  {
   lock (ManagerLibrary.Instance.FileSystemManager.FileSystems)
    foreach (FileSystem filesystem in ManagerLibrary.Instance.FileSystemManager.FileSystems)
     if (filesystem.Supports(volume.VolumeFormat))
      return filesystem;
   throw new NotSupportedException(S._("The file system on the drive {0} is not " +
    "supported.", volume));
  }
  public static void Register(FileSystem filesystem)
  {
   lock (ManagerLibrary.Instance.FileSystemManager.FileSystems)
   {
    ManagerLibrary.Instance.FileSystemManager.FileSystems.Add(filesystem);
   }
  }
  private List<FileSystem> FileSystems = new List<FileSystem>();
 }
}
