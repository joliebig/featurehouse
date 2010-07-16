

using Simias;
using Simias.Storage;

using System;
using System.IO;

namespace Novell.iFolder.Web
{




 [Serializable]
 public class Conflict
 {
  private const long kilobyte = 1024;
  private const long megabyte = 1048576;




  public string iFolderID;




  public string ConflictID;




  public string LocalName;




  public string LocalDate;




  public string LocalSize;




  public string LocalFullPath;




  public bool IsNameConflict;




  public string ServerName;




  public string ServerDate;




  public string ServerSize;




  public string ServerFullPath;




  public Conflict()
  {
  }
  public Conflict(Collection col, Node node)
  {
   iFolderID = col.ID;
   ConflictID = node.ID;
   Simias.Sync.Conflict conflict = new Simias.Sync.Conflict(col, node);
   if(conflict.IsFileNameConflict)
   {
    IsNameConflict = true;
    FileNode fileNode = node as FileNode;
    if (fileNode != null)
    {
     string name = Path.GetFileName(conflict.NonconflictedPath);
     if (name.Equals(Path.GetFileName(conflict.FileNameConflictPath)))
     {
      LocalName = name;
      LocalDate = fileNode.LastWriteTime.ToString();
      LocalSize = formatFileSize(fileNode.Length);
      LocalFullPath = conflict.FileNameConflictPath;
     }
     else
     {
      ServerName = name;
      ServerDate = fileNode.LastWriteTime.ToString();
      ServerSize = formatFileSize(fileNode.Length);
      ServerFullPath = conflict.NonconflictedPath;
     }
    }
    else
    {
     DirNode dn = node as DirNode;
     if (dn != null)
     {
      if (dn.Name.Equals(Path.GetFileName(conflict.FileNameConflictPath)))
      {
       LocalName = dn.Name;
       LocalDate = null;
       LocalSize = null;
       LocalFullPath = conflict.FileNameConflictPath;
      }
      else
      {
       ServerName = dn.Name;
       ServerDate = null;
       ServerSize = null;
       ServerFullPath = conflict.NonconflictedPath;
      }
     }
    }
   }
   else
   {
    IsNameConflict = false;
    FileNode localFileNode = new FileNode(node);
    Node serverNode = col.GetNodeFromCollision(node);
    FileNode serverFileNode = new FileNode(serverNode);
    LocalName = localFileNode.GetFileName();
    LocalDate = localFileNode.LastWriteTime.ToString();
    LocalSize = formatFileSize(localFileNode.Length);
    LocalFullPath = conflict.NonconflictedPath;
    ServerName = serverFileNode.GetFileName();
    ServerDate = serverFileNode.LastWriteTime.ToString();
    ServerSize = formatFileSize(serverFileNode.Length);
    ServerFullPath = conflict.UpdateConflictPath;
   }
  }
  public static void Resolve(Collection col, Node node,
          bool localChangesWin)
  {
   Simias.Sync.Conflict conflict = new Simias.Sync.Conflict(col, node);
   if(conflict.IsFileNameConflict)
   {
    throw new Exception("Resolve must be called with a new file name in the case of a Name Conflict");
   }
   else
    conflict.Resolve(localChangesWin);
  }
  public static void Resolve(Collection col, Node node,
          string newNodeName)
  {
   Simias.Sync.Conflict conflict = new Simias.Sync.Conflict(col, node);
   if(conflict.IsFileNameConflict)
   {
    conflict.Resolve(newNodeName);
   }
   else
    throw new Exception("Resolve must be called with a boolean option of which version wins, server or local.  This call is for Name conflicts");
  }
  public static void RenameConflictingAndResolve(Collection col, Node node, string newFileName)
  {
   Simias.Sync.Conflict conflict = new Simias.Sync.Conflict(col, node);
   if ((conflict != null) && conflict.IsFileNameConflict)
   {
    conflict.RenameConflictingFile(newFileName);
    conflict.Resolve(Path.GetFileName(conflict.NonconflictedPath));
   }
   else
   {
    throw new Exception("RenameConflictingAndResolve can only be called on a name collision conflict.");
   }
  }
  private string formatFileSize(long fileLength)
  {
   string fileSize;
   if (fileLength < kilobyte)
   {
    fileSize = fileLength.ToString() + " bytes";
   }
   else if (fileLength < megabyte)
   {
    fileSize = Math.Round((double)fileLength / kilobyte, 4).ToString() + " KB";
   }
   else
   {
    fileSize = Math.Round((double)fileLength / megabyte, 4).ToString() + " MB";
   }
   return fileSize;
  }
 }
}
