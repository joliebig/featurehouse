using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
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
 }
}
