

using System;
using System.Collections;
using System.Collections.Specialized;
using System.IO;
using System.Text;
using System.Xml;
using System.Threading;

using Simias.Client;
using Simias.Client.Event;
using Novell.iFolder.Events;
using Novell.iFolder.Controller;

using Gtk;

namespace Novell.iFolder
{



 public sealed class iFolderData
 {



  private static iFolderData instance = null;




  private static object instanceLock = new object();





  private iFolderWebService ifws;





  private SimiasWebService simws;

  private Manager simiasManager;





  private SimiasEventBroker eventBroker;


  private DomainController domainController;

  private Hashtable ifolderIters = null;
  private Hashtable subToiFolderMap = null;






  private ListStore iFolderListStore;







  private uint objectsToSync = 0;
  private bool startingSync = false;

  public ListStore iFolders
  {
   get
   {
    return iFolderListStore;
   }
  }





  private iFolderData()
  {
Console.WriteLine("=====> iFolderData being constructed! <=====");
Console.WriteLine("=====> iFolderData HashCode: {0} <=====", this.GetHashCode());
Console.WriteLine("Current Thread: {0}", System.Threading.Thread.CurrentThread.Name);
Console.WriteLine("Stack Trace:");
Console.WriteLine(Environment.StackTrace);
   simiasManager = Util.GetSimiasManager();

   try
   {
    ifws = new iFolderWebService();
    ifws.Url =
     simiasManager.WebServiceUri.ToString() +
     "/iFolder.asmx";
    LocalService.Start(ifws, simiasManager.WebServiceUri, simiasManager.DataPath);
   }
   catch(Exception e)
   {
    ifws = null;
    throw new Exception("Unable to create ifolder web service");
   }
   try
   {
    simws = new SimiasWebService();
    simws.Url =
     simiasManager.WebServiceUri.ToString() +
     "/Simias.asmx";
    LocalService.Start(simws, simiasManager.WebServiceUri, simiasManager.DataPath);
   }
   catch(Exception e)
   {
    simws = null;
    throw new Exception("Unable to create simias web service");
   }

   domainController = DomainController.GetDomainController();

   iFolderListStore = new ListStore(typeof(iFolderHolder));

   ifolderIters = new Hashtable();
   subToiFolderMap = new Hashtable();


   if (domainController != null)
   {
    domainController.DomainAdded +=
     new DomainAddedEventHandler(OnDomainAddedEvent);
    domainController.DomainDeleted +=
     new DomainDeletedEventHandler(OnDomainDeletedEvent);
   }


   eventBroker = SimiasEventBroker.GetSimiasEventBroker();
   if (eventBroker != null)
   {



    eventBroker.CollectionSyncEventFired += OniFolderSyncEvent;
    eventBroker.FileSyncEventFired += OniFolderFileSyncEvent;
   }

   Refresh();
  }
  static public iFolderData GetData()
  {
   lock (instanceLock)
   {
    if (instance == null)
    {
     instance = new iFolderData();
    }
    return instance;
   }
  }
  public void Refresh()
  {
   lock (instanceLock)
   {
    ClearOrphanediFolders();
    iFolderWeb[] ifolders;
    try
    {
     ifolders = ifws.GetAlliFolders();
    }
    catch(Exception e)
    {
     ifolders = null;
    }
    if(ifolders != null)
    {
     foreach (iFolderWeb ifolder in ifolders)
     {
      string ifolderID =
       ifolder.IsSubscription ?
        ifolder.CollectionID :
        ifolder.ID;
      if (ifolderIters.ContainsKey(ifolderID))
      {
       TreeIter iter = (TreeIter)ifolderIters[ifolderID];
       iFolderHolder existingHolder = (iFolderHolder)
        iFolderListStore.GetValue(iter, 0);
       if (existingHolder != null)
       {
        existingHolder.iFolder = ifolder;
        TreePath path = iFolderListStore.GetPath(iter);
        iFolderListStore.EmitRowChanged(path, iter);
       }
       else
       {
Console.WriteLine("*** SOMETHING WENT BAD IN iFolderData.Refresh() ***");
       }
      }
      else
      {
       AddiFolder(ifolder);
      }
     }
    }
   }
  }
  private void ClearOrphanediFolders()
  {
Console.WriteLine("iFolderData.ClearOrphanediFolders()");
   lock(instanceLock)
   {
    TreeIter iter;
    if (iFolderListStore.GetIterFirst(out iter))
    {
     ArrayList itersToRemove = new ArrayList();
     iFolderHolder holder =
      (iFolderHolder)iFolderListStore.GetValue(iter, 0);
     do
     {
      DomainInformation domain =
       domainController.GetDomain(holder.iFolder.DomainID);
      if (domain == null)
      {
Console.WriteLine("\tQueuing removal of: {0}", holder.iFolder.Name);
       itersToRemove.Add(iter);
      }
      else
      {
Console.WriteLine("\tdomain is NOT null");
      }
     } while (iFolderListStore.IterNext(ref iter));
     foreach(TreeIter tmpIter in itersToRemove)
     {
      holder = (iFolderHolder)iFolderListStore.GetValue(tmpIter, 0);
Console.WriteLine("\tRemoving: {0}", holder.iFolder.Name);
      TreeIter iterToRemove = tmpIter;
      iFolderListStore.Remove(ref iterToRemove);
     }
    }
   }
  }
  private iFolderHolder AddiFolder(iFolderWeb ifolder)
  {
Console.WriteLine("AddiFolder()");
Console.WriteLine(Environment.StackTrace);
   lock (instanceLock)
   {
    iFolderHolder ifHolder = null;
if (ifolder.CollectionID == null)
{
 Console.WriteLine("**** CollectionID is null ****");
}
else if (ifolder.ID == null)
{
 Console.WriteLine("**** ID is null ****");
}
Console.WriteLine("\t1");
    string ifolderID =
     ifolder.IsSubscription ?
      ifolder.CollectionID :
      ifolder.ID;
Console.WriteLine("\t{0}", ifolderID);
    if (ifolderIters.ContainsKey(ifolderID))
    {
Console.WriteLine("\t3");
     TreeIter iter = (TreeIter)ifolderIters[ifolderID];
Console.WriteLine("\t4");
     ifHolder = (iFolderHolder)
      iFolderListStore.GetValue(iter, 0);
Console.WriteLine("\t5");
     if (ifHolder != null)
     {
Console.WriteLine("\t6");
      ifHolder.iFolder = ifolder;
      TreePath path = iFolderListStore.GetPath(iter);
Console.WriteLine("\t7");
      if (path != null)
      {
Console.WriteLine("\t8");
       iFolderChangedHandler changedHandler =
        new iFolderChangedHandler(
         path, iter, iFolderListStore);
Console.WriteLine("\t9");
       GLib.Idle.Add(changedHandler.IdleHandler);
      }
     }
     else
     {
Console.WriteLine("*** SOMETHING WENT BAD IN iFolderData.AddiFolder() ***");
     }
    }
    else
    {
Console.WriteLine("\t10");
     ifHolder = new iFolderHolder(ifolder);
Console.WriteLine("\t11");
     iFolderAddHandler addHandler =
      new iFolderAddHandler(ifHolder, this);
Console.WriteLine("\t12");
     GLib.Idle.Add(addHandler.IdleHandler);
    }
Console.WriteLine("\t13");
    return ifHolder;
   }
  }
  private void ProtectedAddiFolder(iFolderHolder holder)
  {
Console.WriteLine("ProtectedAddiFolder()");
Console.WriteLine(Environment.StackTrace);
   if (holder == null) return;
   lock(instanceLock)
   {
    iFolderWeb ifolder = holder.iFolder;
    string ifolderID =
     ifolder.IsSubscription ?
      ifolder.CollectionID :
      ifolder.ID;
Console.WriteLine("\t{0}", ifolderID);
    if (!ifolderIters.ContainsKey(ifolderID))
    {
     TreeIter iter = iFolderListStore.AppendValues(holder);
     if (ifolder.IsSubscription)
     {
Console.WriteLine("\tSubscription: {0}", ifolder.Name);
      ifolderIters[ifolder.CollectionID] = iter;
      subToiFolderMap[ifolder.ID] = ifolder.CollectionID;
     }
     else
     {
Console.WriteLine("\tiFolder: {0}", ifolder.Name);
      ifolderIters[ifolder.ID] = iter;
     }
    }
   }
  }
  public void DeliFolder(string ifolderID)
  {
Console.WriteLine("iFolderData.DeliFolder()");
Console.WriteLine(Environment.StackTrace);
   lock (instanceLock)
   {
    iFolderDeleteHandler deleteHandler =
     new iFolderDeleteHandler(ifolderID, this);
    GLib.Idle.Add(deleteHandler.IdleHandler);
   }
  }
  private void ProtectedDeliFolder(string ifolderID)
  {
Console.WriteLine("iFolderData.ProtectedDeliFolder()");
Console.WriteLine(Environment.StackTrace);
   lock (instanceLock)
   {
    string realID = ifolderID;
    if(!IsiFolder(realID))
    {
     realID = GetiFolderID(ifolderID);
     if( (realID == null) || (!IsiFolder(realID)) )
      return;
     subToiFolderMap.Remove(ifolderID);
    }
    if (ifolderIters.ContainsKey(realID))
    {
     TreeIter iter = (TreeIter)ifolderIters[realID];
     iFolderListStore.Remove(ref iter);
     ifolderIters.Remove(realID);
    }
   }
  }
  public bool IsiFolder(string ifolderID)
  {
   if (ifolderID == null) return false;
   lock(instanceLock)
   {
    return ifolderIters.ContainsKey(ifolderID);
   }
  }
  public bool ISPOBox(string poBoxID)
  {
   lock(instanceLock)
   {
    DomainInformation[] domainsA = domainController.GetDomains();
    foreach(DomainInformation domain in domainsA)
    {
     if (domain.POBoxID.Equals(poBoxID))
      return true;
    }
    return false;
   }
  }
  public string GetiFolderID(string subscriptionID)
  {
   lock(instanceLock)
   {
    return (string)subToiFolderMap[subscriptionID];
   }
  }
  public iFolderHolder GetiFolder(string ifolderID)
  {
   lock(instanceLock)
   {
    iFolderHolder ifHolder = null;
    if (ifolderIters.ContainsKey(ifolderID))
    {
     TreeIter iter = (TreeIter)ifolderIters[ifolderID];
     ifHolder = (iFolderHolder)
      iFolderListStore.GetValue(iter, 0);
    }
    return ifHolder;
   }
  }
  public iFolderHolder ReadiFolder(string ifolderID)
  {
Console.WriteLine("iFolderData.ReadiFolder()");
Console.WriteLine(Environment.StackTrace);
   lock(instanceLock)
   {
    iFolderHolder ifHolder = null;
    if (ifolderIters.ContainsKey(ifolderID))
    {
     TreeIter iter = (TreeIter)ifolderIters[ifolderID];
     ifHolder = (iFolderHolder)
      iFolderListStore.GetValue(iter, 0);
    }
    try
    {
     iFolderWeb ifolder =
      ifws.GetiFolder(ifolderID);
     if (ifolder != null)
     {
      if (ifHolder != null)
      {
       ifHolder.iFolder = ifolder;
       if (ifolderIters.ContainsKey(ifolder.ID))
       {
        TreeIter iter = (TreeIter)
         ifolderIters[ifolder.ID];
        TreePath path = iFolderListStore.GetPath(iter);
        if (path != null)
        {
         iFolderChangedHandler changedHandler =
          new iFolderChangedHandler(
           path, iter, iFolderListStore);
         GLib.Idle.Add(changedHandler.IdleHandler);
        }
        else
        {
Console.WriteLine("*** SOMETHING WENT BAD IN iFolderData.ReadiFolder() ***");
        }
       }
      }
      else
      {
       ifHolder = AddiFolder(ifolder);
      }
     }
    }
    catch(Exception e)
    {
     ifHolder = null;
    }
    return ifHolder;
   }
  }
  public iFolderHolder[] GetiFolders()
  {
   lock(instanceLock)
   {
    ArrayList arrayList = new ArrayList();
    TreeIter iter;
    if (iFolderListStore.GetIterFirst(out iter))
    {
     do
     {
      iFolderHolder ifHolder =
       (iFolderHolder)iFolderListStore.GetValue(iter, 0);
      if (ifHolder != null && ifHolder.iFolder != null)
       arrayList.Add(ifHolder);
     } while (iFolderListStore.IterNext(ref iter));
    }
    iFolderHolder[] ifolderA = (iFolderHolder[])arrayList.ToArray(typeof(iFolderHolder));
    return ifolderA;
   }
  }
  public iFolderHolder GetAvailableiFolder(string ifolderID)
  {
   lock(instanceLock)
   {
    string realID;
    iFolderHolder ifHolder = null;
    realID = GetiFolderID(ifolderID);
    if(realID != null)
    {
     if (ifolderIters.ContainsKey(realID))
     {
      TreeIter iter = (TreeIter)ifolderIters[realID];
      ifHolder =
       (iFolderHolder)iFolderListStore.GetValue(iter, 0);
     }
    }
    return ifHolder;
   }
  }
  public iFolderHolder ReadAvailableiFolder( string ifolderID,
             string collectionID)
  {
Console.WriteLine("iFolderData.ReadAvailableiFolder()");
Console.WriteLine(Environment.StackTrace);
   lock(instanceLock)
   {
    iFolderHolder ifHolder = GetAvailableiFolder(ifolderID);
    try
    {
     iFolderWeb ifolder = ifws.GetiFolderInvitation(
        collectionID, ifolderID);
     if (ifolder == null)
      return null;
     if(ifHolder != null)
     {
      if(!IsiFolder(ifolder.CollectionID))
      {
       ifHolder.iFolder = ifolder;
       if (ifolderIters.ContainsKey(ifolder.CollectionID))
       {
        TreeIter iter = (TreeIter)
         ifolderIters[ifolder.CollectionID];
        TreePath path = iFolderListStore.GetPath(iter);
        if (path != null)
        {
         iFolderChangedHandler changedHandler =
          new iFolderChangedHandler(
           path, iter, iFolderListStore);
         GLib.Idle.Add(changedHandler.IdleHandler);
        }
        else
        {
Console.WriteLine("*** SOMETHING WENT BAD IN iFolderData.ReadAvailableiFolder() ***");
        }
       }
      }
     }
     else
     {
      if(!IsiFolder(ifolder.CollectionID))
       ifHolder = AddiFolder(ifolder);
     }
    }
    catch(Exception e)
    {
     ifHolder = null;
    }
    return ifHolder;
   }
  }
  public iFolderHolder CreateiFolder(string path, string domainID)
  {
   lock(instanceLock)
   {
       iFolderWeb newiFolder =
        ifws.CreateiFolderInDomain(path, domainID);
    if (newiFolder == null)
    {
Console.WriteLine("ifws.CreateiFolderInDomain(\"{0}\") returned null", path);
     return null;
    }
    iFolderHolder ifHolder = AddiFolder(newiFolder);
    return ifHolder;
   }
  }
  public iFolderHolder AcceptiFolderInvitation( string ifolderID,
              string domainID,
              string localPath)
  {
Console.WriteLine("iFolderData.AcceptiFolderInvitation()");
Console.WriteLine(Environment.StackTrace);
   lock(instanceLock)
   {
    iFolderHolder ifHolder = null;
    string collectionID = GetiFolderID(ifolderID);
        iFolderWeb newifolder = ifws.AcceptiFolderInvitation(
           domainID,
           ifolderID,
           localPath);
    if (newifolder == null)
    {
Console.WriteLine("ifws.AcceptiFolderInvitation(?, ?, \"{0}\") returned null", localPath);
     return null;
    }
    if(newifolder.ID != ifolderID)
    {
     subToiFolderMap.Remove(ifolderID);
     if (newifolder.IsSubscription)
     {
      subToiFolderMap[newifolder.ID]
       = newifolder.CollectionID;
     }
    }
    ifHolder = GetiFolder(collectionID);
    ifHolder.iFolder = newifolder;
    Refresh();
    return ifHolder;
   }
  }
  public iFolderHolder RevertiFolder(string ifolderID)
  {
Console.WriteLine("iFolderData.RevertiFolder()");
Console.WriteLine(Environment.StackTrace);
   lock(instanceLock)
   {
    iFolderHolder ifHolder = null;
    ifHolder = GetiFolder(ifolderID);
    if(ifHolder == null)
    {
     throw new Exception("iFolder did not exist");
    }
    try
    {
        iFolderWeb reviFolder =
      ifws.RevertiFolder(ifHolder.iFolder.ID);
     if (reviFolder == null)
     {
Console.WriteLine("ifws.RevertiFolder() returned null");
      return null;
     }
     ifHolder.iFolder = reviFolder;
     if(reviFolder.IsSubscription)
     {
      subToiFolderMap[reviFolder.ID] = reviFolder.CollectionID;
      ifHolder = ReadAvailableiFolder(reviFolder.ID, reviFolder.CollectionID);
     }
     Refresh();
    }
    catch{}
    return ifHolder;
   }
  }
  public void DeleteiFolder(string ifolderID)
  {
Console.WriteLine("iFolderData.DeleteiFolder()");
Console.WriteLine(Environment.StackTrace);
   lock(instanceLock)
   {
    iFolderHolder ifHolder = null;
    iFolderWeb ifolder = null;
    if(IsiFolder(ifolderID))
    {
     ifHolder = GetiFolder(ifolderID);
     ifolder = ifHolder.iFolder;
     if (ifolder.Role.Equals("Master"))
     {
      string realID = ifolder.ID;
      ifws.DeleteiFolder(realID);
      return;
     }
     ifHolder = RevertiFolder(ifolderID);
    }
    else
    {
     string realID = GetiFolderID(ifolderID);
     if(realID != null)
     {
      ifHolder = GetiFolder(realID);
     }
    }
    if (ifHolder != null)
    {
     ifolder = ifHolder.iFolder;
     if (ifolder != null && ifolder.IsSubscription)
     {
      string realID = ifolder.ID;
      ifws.DeclineiFolderInvitation(
       ifolder.DomainID, realID);
     }
    }
   }
  }
  public iFolderUser GetiFolderUserFromNodeID(string collectionID,
              string nodeID)
  {
   lock(instanceLock)
   {
    iFolderUser user = null;
    try
    {
     user = ifws.GetiFolderUserFromNodeID(
      collectionID, nodeID);
    }
    catch(Exception e)
    {
     user = null;
    }
    return user;
   }
  }
  public bool IsCurrentUser(string UserID)
  {
   lock(instanceLock)
   {
    DomainInformation[] domainsA = domainController.GetDomains();
    foreach(DomainInformation domain in domainsA)
    {
     if(domain.MemberUserID.Equals(UserID))
      return true;
    }
    return false;
   }
  }
  public DiskSpace GetUserDiskSpace(string UserID)
  {
   lock(instanceLock)
   {
    DiskSpace ds = null;
    try
    {
     ds = ifws.GetUserDiskSpace(UserID);
    }
    catch(Exception e)
    {
    }
    return ds;
   }
  }
  private void OnDomainAddedEvent(object o, DomainEventArgs args)
  {
   Refresh();
   domainController.CheckForNewiFolders();
  }
  private void OnDomainDeletedEvent(object o, DomainEventArgs args)
  {
Console.WriteLine("iFolderData.OnDomainDeletedEvent()");
   Refresh();
  }
  private void OniFolderSyncEvent(object o, CollectionSyncEventArgs args)
  {
   if (args == null || args.ID == null || args.Name == null)
    return;
   TreeIter iter = TreeIter.Zero;
   iFolderHolder ifHolder = null;
   if (ifolderIters.ContainsKey(args.ID))
   {
    iter = (TreeIter)ifolderIters[args.ID];
    ifHolder = (iFolderHolder)iFolderListStore.GetValue(iter, 0);
   }
   if (ifHolder == null) return;
   switch(args.Action)
   {
    case Simias.Client.Event.Action.StartLocalSync:
     ifHolder.State = iFolderState.SynchronizingLocal;
     break;
    case Simias.Client.Event.Action.StartSync:
     startingSync = true;
     ifHolder.State = iFolderState.Synchronizing;
     break;
    case Simias.Client.Event.Action.StopSync:
     try
     {
      ReadiFolder(args.ID);
      SyncSize syncSize = ifws.CalculateSyncSize(args.ID);
      objectsToSync = syncSize.SyncNodeCount;
      ifHolder.ObjectsToSync = objectsToSync;
     }
     catch
     {}
     if (ifHolder.ObjectsToSync > 0)
      ifHolder.State = iFolderState.Normal;
     else
     {
      if (args.Connected)
       ifHolder.State = iFolderState.Normal;
      else
       ifHolder.State = iFolderState.Disconnected;
     }
     objectsToSync = 0;
     break;
    default:
     break;
   }
   TreePath path = iFolderListStore.GetPath(iter);
   if (path != null)
    iFolderListStore.EmitRowChanged(path, iter);
   else
   {
Console.WriteLine("*** SOMETHING WENT BAD IN iFolderData.OniFolderSyncEvent() ***");
   }
  }
  private void OniFolderFileSyncEvent(object o, FileSyncEventArgs args)
  {
   if (args == null || args.CollectionID == null || args.Name == null)
    return;
   if (args.SizeRemaining == args.SizeToSync)
   {
    if (startingSync || (objectsToSync <= 0))
    {
     startingSync = false;
     try
     {
      SyncSize syncSize = ifws.CalculateSyncSize(args.CollectionID);
      objectsToSync = syncSize.SyncNodeCount;
     }
     catch(Exception e)
     {
      objectsToSync = 1;
     }
    }
    if (!args.Direction.Equals(Simias.Client.Event.Direction.Local))
    {
     if (objectsToSync <= 0)
      objectsToSync = 0;
     else
      objectsToSync--;
     TreeIter iter = TreeIter.Zero;
     iFolderHolder ifHolder = null;
     if (ifolderIters.ContainsKey(args.CollectionID))
     {
      iter = (TreeIter)ifolderIters[args.CollectionID];
      ifHolder = (iFolderHolder)iFolderListStore.GetValue(iter, 0);
     }
     if (ifHolder != null)
     {
      ifHolder.ObjectsToSync = objectsToSync;
      TreePath path = iFolderListStore.GetPath(iter);
      if (path != null)
       iFolderListStore.EmitRowChanged(path, iter);
      else
Console.WriteLine("*** SOMETHING WENT BAD IN iFolderData.OniFolderFileSyncEvent() ***");
     }
    }
   }
  }
  private class iFolderAddHandler
  {
   iFolderHolder holder;
   iFolderData ifdata;
   public iFolderAddHandler(iFolderHolder holder, iFolderData ifdata)
   {
    this.holder = holder;
    this.ifdata = ifdata;
   }
   public bool IdleHandler()
   {
    if (holder != null && ifdata != null)
     ifdata.ProtectedAddiFolder(holder);
    return false;
   }
  }
  private class iFolderDeleteHandler
  {
   string ifolderID;
   iFolderData ifdata;
   public iFolderDeleteHandler(string ifolderID, iFolderData ifdata)
   {
    this.ifolderID = ifolderID;
    this.ifdata = ifdata;
   }
   public bool IdleHandler()
   {
    if (ifolderID != null && ifdata != null)
     ifdata.ProtectedDeliFolder(ifolderID);
    return false;
   }
  }
 }
 public class iFolderChangedHandler
 {
  TreePath path;
  TreeIter iter;
  ListStore list;
  public iFolderChangedHandler(TreePath path, TreeIter iter, ListStore list)
  {
   this.path = path;
   this.iter = iter;
   this.list = list;
  }
  public bool IdleHandler()
  {
   if (list != null && path != null)
   {
    list.EmitRowChanged(path, iter);
   }
   return false;
  }
 }
}
