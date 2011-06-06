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
  Disconnected,
  NoPassphrase,
  RevertAndDelete,
  SyncDisabled
 }
 public class iFolderHolder
 {
  private iFolderWeb ifolder;
  private iFolderState state;
  private string stateString;
  private string path;
  public uint objectsToSync;
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
   get
   {
    UpdateDisplayData();
    return stateString;
   }
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
      case iFolderState.NoPassphrase:
       stateString = Util.GS("Passphrase not provided");
       break;
      case iFolderState.Disconnected:
       stateString = Util.GS("Server unavailable");
       break;
      case iFolderState.RevertAndDelete:
       stateString = Util.GS("Deletion in progress");
       break;
      case iFolderState.SyncDisabled:
       stateString = Util.GS("Synchronization disabled");
       break;
      case iFolderState.Normal:
      default:
       if (objectsToSync > 0)
        stateString = string.Format(Util.GS("{0} items not synchronized"), objectsToSync);
       else
       {
        string lastSyncTime = iFolder.LastSyncTime;
        if (lastSyncTime == null || lastSyncTime.Length == 0)
         stateString = Util.GS("OK");
        else
        {
         try
         {
          DateTime dateTime =
           DateTime.Parse(lastSyncTime);
          string friendlyTime =
           GetFriendlyTime(dateTime);
          stateString =
           string.Format(
            Util.GS("Synchronized: {0}"),
            friendlyTime);
         }
         catch
         {
          stateString =
           string.Format(
            Util.GS("Synchronized: {0}"),
            lastSyncTime);
         }
        }
       }
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
  private static string GetFriendlyTime(DateTime dateTime)
  {
   DateTime now = DateTime.Now;
   TimeSpan span = now.Subtract(dateTime);
   if (span.TotalSeconds < 60)
    return Util.GS("Less than a minute ago");
   int totalMinutes = (int)span.TotalMinutes;
   if (totalMinutes == 1)
    return Util.GS("1 minute ago");
   if (totalMinutes < 60)
    return string.Format(Util.GS("{0} minutes ago"),
            totalMinutes);
   int lastSyncDay = dateTime.DayOfYear;
   int nowDay = now.DayOfYear;
   if (lastSyncDay == nowDay)
   {
    if (span.Minutes == 0)
    {
     if (span.Hours == 1)
      return Util.GS("1 hour ago");
     else
      return string.Format(
       Util.GS("{0} hours ago"),
       span.Hours);
    }
    else if (span.Minutes == 1)
    {
     if (span.Hours == 1)
      return Util.GS("1 hour, 1 minute ago");
     else
      return string.Format(
       Util.GS("{0} hours, 1 minute ago"),
       span.Hours);
    }
    else
    {
     if (span.Hours == 1)
      return string.Format(
       Util.GS("1 hour, {0} minutes ago"),
       span.Minutes);
     else
      return string.Format(
       Util.GS("{0} hour, {1} minutes ago"),
       span.Hours,
       span.Minutes);
    }
   }
   else if ((nowDay - lastSyncDay) == 1)
   {
    return Util.GS("Yesterday");
   }
   else
    return dateTime.ToShortDateString();
  }
 }
}
