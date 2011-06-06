using System;
using System.Runtime.InteropServices;
namespace Novell.iFolderCom
{
 [ComVisible(false)]
 public class iFolderInfo
 {
  private string localPath;
  private string id;
  public iFolderInfo()
  {
  }
  public string LocalPath
  {
   get { return localPath; }
   set { localPath = value; }
  }
  public string ID
  {
   get { return id; }
   set { id = value; }
  }
  public override string ToString()
  {
   return localPath;
  }
 }
}
