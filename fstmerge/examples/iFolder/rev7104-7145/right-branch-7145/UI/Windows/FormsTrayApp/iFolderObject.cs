using System;
using Novell.iFolder.Web;
namespace Novell.FormsTrayApp
{
 public enum iFolderState
 {
  Initial,
  Normal,
  Synchronizing,
  FailedSync,
  SynchronizingLocal,
  Disconnected,
        NoPassphrase
 }
 public class iFolderObject
 {
  private iFolderWeb ifolderWeb;
  private iFolderState ifolderState;
  public iFolderObject(iFolderWeb ifolderWeb, iFolderState ifolderState)
  {
   this.ifolderWeb = ifolderWeb;
   this.ifolderState = ifolderState;
  }
  public iFolderWeb iFolderWeb
  {
   get { return ifolderWeb; }
   set { ifolderWeb = value; }
  }
  public iFolderState iFolderState
  {
   get { return ifolderState; }
   set { ifolderState = value; }
  }
  public string ID
  {
   get { return ifolderWeb.ID; }
  }
 }
}
