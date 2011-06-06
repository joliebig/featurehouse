using System;
using System.Runtime.InteropServices;
namespace Novell.iFolderCom
{
 [ComVisible(false)]
 public class ShareListMember
 {
  private iFolderUser ifolderUser;
  private bool added = false;
  private bool changed = false;
  public ShareListMember()
  {
  }
  public bool Added
  {
   get { return added; }
   set { added = value; }
  }
  public bool Changed
  {
   get { return changed; }
   set { changed = value; }
  }
  public iFolderUser iFolderUser
  {
   get { return ifolderUser; }
   set { ifolderUser = value; }
  }
  public string Name
  {
   get
   {
    return (ifolderUser.FN != null) && !ifolderUser.FN.Equals(string.Empty) ? ifolderUser.FN : ifolderUser.Name;
   }
  }
 }
}
