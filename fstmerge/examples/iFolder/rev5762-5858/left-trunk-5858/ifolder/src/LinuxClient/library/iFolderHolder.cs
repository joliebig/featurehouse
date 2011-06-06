using System;
using System.IO;
using System.Collections;
using System.Text;
using Gtk;
using Simias.Client;
using Simias.Client.Event;
using Novell.iFolder.Events;
using Novell.iFolder.Controller;
namespace Novell.iFolder
{
 public enum iFolderState
 {
  Initial,
  Normal,
  Synchronizing,
  FailedSync,
  SynchronizingLocal,
  Disconnected
 }
 public class iFolderHolder
 {
  private iFolderWeb ifolder;
  private iFolderState state;
  private string stateString;
  private string path;
  private uint objectsToSync;
  public iFolderHolder(iFolderWeb ifolder)
  {
   this.ifolder = ifolder;
   this.state = iFolderState.Initial;
   this.objectsToSync = 0;
   UpdateDisplayData();
  }
  protected iFolderHolder()
  {
   this.ifolder = null;
   this.state = iFolderState.Initial;
   this.objectsToSync = 0;
  }
  public iFolderWeb iFolder
  {
   get{ return ifolder; }
   set
   {
    this.ifolder = value;
    UpdateDisplayData();
   }
  }
  public string Path
  {
   get{ return path; }
  }
  public string StateString
  {
   get{ return stateString; }
  }
  public iFolderState State
  {
   get{ return state; }
   set
   {
    this.state = value;
    UpdateDisplayData();
   }
  }
  public uint ObjectsToSync
  {
   get
   {
    return objectsToSync;
   }
   set
   {
    objectsToSync = value;
    UpdateDisplayData();
   }
  }
  private void UpdateDisplayData()
  {
   if (state == iFolderState.Synchronizing)
   {
    if (objectsToSync > 0)
     stateString = string.Format(Util.GS("{0} items to synchronize"), objectsToSync);
    else
     stateString = Util.GS("Synchronizing");
   }
   else if (state == iFolderState.SynchronizingLocal)
   {
    stateString = Util.GS("Checking for changes");
   }
   else
   {
    if (iFolder.HasConflicts)
    {
     stateString = Util.GS("Has conflicts");
    }
    else
    {
     switch (state)
     {
      case iFolderState.Initial:
       switch (iFolder.State)
       {
        case "Available":
         stateString = Util.GS("Not set up");
         break;
        case "WaitConnect":
         stateString = Util.GS("Waiting to connect");
         break;
        case "WaitSync":
        case "Local":
         stateString = Util.GS("Waiting to synchronize");
         break;
        default:
         stateString = Util.GS("Unknown");
         break;
       }
       break;
      case iFolderState.FailedSync:
       stateString = Util.GS("Incomplete synchronization");
       break;
      case iFolderState.Disconnected:
       stateString = Util.GS("Server unavailable");
       break;
      case iFolderState.Normal:
      default:
       if (objectsToSync > 0)
        stateString = string.Format(Util.GS("{0} items not synchronized"), objectsToSync);
       else
        stateString = Util.GS("OK");
       break;
     }
    }
   }
   if(iFolder.IsSubscription)
   {
    if(iFolder.State == "Available")
     path = iFolder.Owner;
   }
   else
   {
    path = iFolder.UnManagedPath;
   }
  }
 }
}
