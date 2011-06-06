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
  private Hashtable deletediFolders = null;
  private ListStore iFolderListStore;
  private uint objectsToSync = 0;
  private bool startingSync = false;
  private bool bFileSyncFailed = false;
  public ListStore iFolders
  {
   get
   {
    return iFolderListStore;
   }
  }
  private iFolderData()
  {
   simiasManager = Util.GetSimiasManager();
   try
   {
    Debug.PrintLine(String.Format("Url = {0}", simiasManager.WebServiceUri));
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
   iFolderListStore.SetSortFunc(
    0,
    new TreeIterCompareFunc(TreeModelSortFunction));
   iFolderListStore.SetSortColumnId(0, SortType.Ascending);
   ifolderIters = new Hashtable();
   deletediFolders = new Hashtable();
   if (domainController != null)
   {
    domainController.DomainAdded +=
     new DomainAddedEventHandler(OnDomainAddedEvent);
    domainController.DomainDeleted +=
     new DomainDeletedEventHandler(OnDomainDeletedEvent);
    domainController.DomainLoggedOut +=
     new DomainLoggedOutEventHandler(OnDomainLoggedOutEvent);
   }
   eventBroker = SimiasEventBroker.GetSimiasEventBroker();
   if (eventBroker != null)
   {
    eventBroker.CollectionSyncEventFired += OniFolderSyncEvent;
    eventBroker.FileSyncEventFired += OniFolderFileSyncEvent;
   }
  }
  private int TreeModelSortFunction(TreeModel model, TreeIter a, TreeIter b)
  {
   iFolderHolder holderA = (iFolderHolder)model.GetValue(a, 0);
   iFolderHolder holderB = (iFolderHolder)model.GetValue(b, 0);
   if (holderA == null || holderB == null)
    return 0;
   iFolderWeb ifolderA = holderA.iFolder;
   iFolderWeb ifolderB = holderB.iFolder;
   if (ifolderA == null || ifolderB == null)
    return 0;
   string nameA = ifolderA.Name;
   string nameB = ifolderB.Name;
   if (nameA == null || nameB == null)
    return 0;
   return string.Compare(nameA, nameB, true);
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
    if(ifolders != null && ifolders.Length >0)
    {
     Hashtable ifolderItersClone = null;
     try
     {
      ifolderItersClone = (Hashtable) ifolderIters.Clone();
     }
     catch(Exception ex)
     {
      Debug.PrintLine(string.Format("Exception while cloning hashtable. {0}", ex.Message));
     }
     foreach (iFolderWeb ifolder in ifolders)
     {
      string ifolderID =
       ifolder.IsSubscription ?
        ifolder.CollectionID :
        ifolder.ID;
      if (ifolderIters.ContainsKey(ifolderID))
      {
       TreeIter iter = (TreeIter)ifolderIters[ifolderID];
       iFolderHolder existingHolder = null;
       try
       {
        existingHolder = (iFolderHolder)
         iFolderListStore.GetValue(iter, 0);
       }
       catch(Exception e)
       {
        Debug.PrintLine(e.Message);
       }
       if ((existingHolder != null) && (existingHolder.iFolder.OwnerID != ifolder.OwnerID))
       {
        existingHolder.iFolder = ifolder;
        TreePath path = iFolderListStore.GetPath(iter);
        if (path != null)
        {
         iFolderChangedHandler changedHandler =
            new iFolderChangedHandler( path, iter, iFolderListStore);
                                        GLib.Idle.Add(changedHandler.IdleHandler);
                                    }
       }
       try
       {
        if( ifolderItersClone.ContainsKey(ifolderID))
         ifolderItersClone.Remove( ifolderID);
       }
       catch(Exception ex)
       {
        Debug.PrintLine(string.Format("Exception while removing from ht. {0}", ex.Message));
       }
      }
      else
      {
       AddiFolder(ifolder);
      }
     }
     try
     {
      foreach(string key in ifolderItersClone.Keys)
      {
       try
       {
        Debug.PrintLine(string.Format("Removing entry: {0}", key));
        try
        {
         if( ifws.CheckiFolder( key ) == false)
          throw new Exception( "Invalid iFolderID");
        }
        catch(Exception ex)
        {
         DeliFolder(key);
        }
       }
       catch(Exception ex)
       {
        Debug.PrintLine(string.Format("exception in real delete. {0}", ex.Message));
       }
      }
     }
     catch(Exception ex)
     {
      Debug.PrintLine(string.Format("Exception in removing. Outer for only. {0}", ex.Message));
     }
    }
   }
  }
  private void ClearOrphanediFolders()
  {
   lock(instanceLock)
   {
    TreeIter iter;
    if (iFolderListStore.GetIterFirst(out iter))
    {
     ArrayList itersToRemove = new ArrayList();
     iFolderHolder holder;
     do
     {
      holder =
       (iFolderHolder)iFolderListStore.GetValue(iter, 0);
      DomainInformation domain =
       domainController.GetDomain(holder.iFolder.DomainID);
      if (domain == null || (!domain.Authenticated && holder.iFolder.IsSubscription))
      {
       itersToRemove.Add(holder.iFolder.ID);
      }
     } while (iFolderListStore.IterNext(ref iter));
     foreach(string ifolderID in itersToRemove)
     {
      DeliFolder(ifolderID);
     }
    }
   }
  }
  private iFolderHolder AddiFolder(iFolderWeb ifolder)
  {
   lock (instanceLock)
   {
    iFolderHolder ifHolder = null;
    string ifolderID =
     ifolder.IsSubscription ?
      ifolder.CollectionID :
      ifolder.ID;
    if (ifolderIters.ContainsKey(ifolderID))
    {
     TreeIter iter = (TreeIter)ifolderIters[ifolderID];
     ifHolder = (iFolderHolder)
      iFolderListStore.GetValue(iter, 0);
     if (ifHolder != null)
     {
      ifHolder.iFolder = ifolder;
      TreePath path = iFolderListStore.GetPath(iter);
      if (path != null)
      {
       iFolderChangedHandler changedHandler =
        new iFolderChangedHandler(
         path, iter, iFolderListStore);
       GLib.Idle.Add(changedHandler.IdleHandler);
      }
     }
    }
    else
    {
     ifHolder = new iFolderHolder(ifolder);
     ifHolder.State = iFolderState.Initial;
     iFolderAddHandler addHandler =
      new iFolderAddHandler(ifHolder, this);
     GLib.Idle.Add(addHandler.IdleHandler);
    }
    return ifHolder;
   }
  }
  private void ProtectedAddiFolder(iFolderHolder holder)
  {
   if (holder == null) return;
   lock(instanceLock)
   {
    iFolderWeb ifolder = holder.iFolder;
    string ifolderID =
     ifolder.IsSubscription ?
      ifolder.CollectionID :
      ifolder.ID;
    if (!ifolderIters.ContainsKey(ifolderID))
    {
     TreeIter iter = iFolderListStore.AppendValues(holder);
     if (ifolder.IsSubscription)
     {
      ifolderIters[ifolder.CollectionID] = iter;
     }
     else
     {
      ifolderIters[ifolder.ID] = iter;
     }
    }
   }
  }
  public void DeliFolder(string ifolderID)
  {
   lock (instanceLock)
   {
    iFolderDeleteHandler deleteHandler =
     new iFolderDeleteHandler(ifolderID, this);
    GLib.Idle.Add(deleteHandler.IdleHandler);
   }
  }
  private void RealDelete(string ifolderID, bool deleteImmediately)
  {
   lock (instanceLock)
   {
    if (!deleteImmediately && deletediFolders.Contains(ifolderID))
    {
     deletediFolders.Remove(ifolderID);
     return;
    }
    string realID = ifolderID;
    if(!IsiFolder(realID))
    {
     if( (realID == null))
      return;
    }
    if (ifolderIters.ContainsKey(realID))
    {
     TreeIter iter = (TreeIter)ifolderIters[realID];
     iFolderListStore.Remove(ref iter);
     ifolderIters.Remove(realID);
    }
    if (deleteImmediately)
     deletediFolders[realID] = realID;
   }
  }
  private void QuickDelete(string ifolderID)
  {
   RealDelete(ifolderID, true);
  }
  private void ProtectedDeliFolder(string ifolderID)
  {
   RealDelete(ifolderID, false);
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
     if (domain.POBoxID != null && domain.POBoxID.Equals(poBoxID))
      return true;
    }
    return false;
   }
  }
  public string GetiFolderID(string subscriptionID)
  {
   lock(instanceLock)
   {
    return subscriptionID;
   }
  }
  public iFolderHolder GetiFolder(string ifolderID)
  {
   lock(instanceLock)
   {
    iFolderHolder ifHolder = null;
    Debug.PrintLine("In GetiFolder");
    if (ifolderIters.ContainsKey(ifolderID))
    {
     TreeIter iter = (TreeIter)ifolderIters[ifolderID];
     ifHolder = (iFolderHolder)
      iFolderListStore.GetValue(iter, 0);
    }
    return ifHolder;
   }
  }
  public iFolderWeb GetDefaultiFolder( string iFolderID)
  {
   return ifws.GetMinimaliFolder(iFolderID, 1);
  }
  public iFolderHolder ReadiFolder(string ifolderID)
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
      try
      {
       ifHolder =
        (iFolderHolder)iFolderListStore.GetValue(iter, 0);
      }
      catch(Exception e)
      {
       Debug.PrintLine(e.Message);
      }
     }
    }
    return ifHolder;
   }
  }
  public iFolderHolder ReadAvailableiFolder( string ifolderID,
             string collectionID)
  {
   lock(instanceLock)
   {
    iFolderHolder ifHolder = GetAvailableiFolder(ifolderID);
    try
    {
     iFolderWeb ifolder = ifws.GetiFolderInvitation(
        collectionID, ifolderID);
     if (ifolder == null || !ifolder.State.Equals("Available"))
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
       }
      }
     }
     else
     {
      if (!deletediFolders.Contains(ifolder.CollectionID)
       && !IsiFolder(ifolder.CollectionID))
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
  public iFolderHolder CreateiFolder(string path, string domainID, bool SSL, string EncryptionAlgorithm)
  {
   lock(instanceLock)
   {
    iFolderWeb newiFolder;
    if( EncryptionAlgorithm == null)
    {
     Debug.PrintLine("Creating unencrypted folder");
     newiFolder = ifws.CreateiFolderInDomainEncr(path, domainID,
                                                                                        SSL, null, null);
    }
    else
    {
     Debug.PrintLine("Creating encrypted folder");
     string Passphrase = simws.GetPassPhrase(domainID);
        newiFolder = ifws.CreateiFolderInDomainEncr(path, domainID,
           SSL, EncryptionAlgorithm, Passphrase);
    }
    if (newiFolder == null)
    {
     return null;
    }
    iFolderHolder ifHolder = AddiFolder(newiFolder);
    return ifHolder;
   }
  }
  public int GetSecurityPolicy(string domainID)
  {
   return ifws.GetSecurityPolicy(domainID);
  }
  public iFolderHolder AcceptiFolderInvitation( string ifolderID,
              string domainID,
              string localPath)
  {
   return AcceptiFolderInvitation( ifolderID, domainID, localPath, false);
  }
  public iFolderHolder AcceptiFolderInvitation( string ifolderID,
              string domainID,
              string localPath,
              bool merge)
  {
   lock(instanceLock)
   {
    iFolderHolder ifHolder = null;
    string collectionID = GetiFolderID(ifolderID);
    iFolderWeb newifolder = null;
    if(merge == true)
    {
     newifolder = ifws.MergeiFolder(
           domainID,
           ifolderID,
           localPath);
    }
    else
    {
     newifolder = ifws.AcceptiFolderInvitation(
           domainID,
           ifolderID,
           localPath);
    }
    if (newifolder == null)
    {
     return null;
    }
    ifHolder = GetiFolder(ifolderID);
    if( ifHolder == null)
    {
     Debug.PrintLine("Calling addiFolder");
     ifHolder = AddiFolder(newifolder);
    }
    ifHolder.iFolder = newifolder;
    return ifHolder;
   }
  }
  public iFolderHolder RevertiFolder(string ifolderID)
  {
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
      ifHolder = null;
      return ifHolder;
     }
     reviFolder.IsSubscription = true;
     ifHolder.iFolder = reviFolder;
        if( ifolderIters == null)
     {
      ifolderIters = new Hashtable();
     }
     if (ifolderIters.ContainsKey(ifolderID))
     {
      TreeIter iter = (TreeIter)
      ifolderIters[ifolderID];
      iFolderListStore.SetValue(iter, 0, ifHolder);
     }
    }
    catch(Exception e)
    {
     if ( String.Compare( e.Message, "Invalid CollectionID" ) == 0 )
     {
      ifHolder = null;
      return ifHolder;
     }
     throw new Exception(Util.GS("An exception occurred while attempting to revert the iFolder to a normal folder.  Exception message: ") + e.Message);
    }
    return ifHolder;
   }
  }
  public void DeleteiFolder(string ifolderID)
  {
   lock(instanceLock)
   {
    iFolderHolder ifHolder = null;
    iFolderWeb ifolder = null;
    ifHolder = GetiFolder(ifolderID);
    bool IsOwner = (ifHolder.iFolder.CurrentUserID.Equals (ifHolder.iFolder.OwnerID));
    if(IsiFolder(ifolderID) && IsOwner)
    {
     ifolder = ifHolder.iFolder;
     if (ifolder.Role == null || ifolder.Role.Equals("Master"))
     {
      string realID = ifolder.ID;
      ifws.DeleteiFolder(ifolder.DomainID, realID);
      QuickDelete(realID);
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
      QuickDelete(realID);
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
  }
  private void OnDomainDeletedEvent(object o, DomainEventArgs args)
  {
  }
  private void OnDomainLoggedOutEvent(object o, DomainEventArgs args)
  {
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
     bFileSyncFailed = false;
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
     {
      if (bFileSyncFailed)
       ifHolder.State = iFolderState.FailedSync;
      else
       ifHolder.State = iFolderState.Normal;
     }
     else
     {
      if (args.Connected || ifHolder.iFolder.Role == "Master")
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
   {
     iFolderChangedHandler changedHandler =
       new iFolderChangedHandler( path, iter, iFolderListStore);
     GLib.Idle.Add(changedHandler.IdleHandler);
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
      if (ifHolder.State != iFolderState.Synchronizing)
       ifHolder.State = iFolderState.Synchronizing;
      ifHolder.ObjectsToSync = objectsToSync;
      TreePath path = iFolderListStore.GetPath(iter);
      if (path != null)
      {
       iFolderChangedHandler changedHandler =
          new iFolderChangedHandler(path, iter, iFolderListStore);
       GLib.Idle.Add(changedHandler.IdleHandler);
      }
     }
    }
   }
   if (args.Status != SyncStatus.Success)
    bFileSyncFailed = true;
  }
  public void PrintDebugState()
  {
   lock(instanceLock)
   {
    Debug.PrintLine("************************** iFolderData Data Inspection **************************");
    Debug.PrintLine(String.Format("ifolderIters Hashtable (All items in iFolderListStore, {0}):", ifolderIters.Count));
    foreach(TreeIter treeIter in ifolderIters.Values)
    {
     iFolderHolder ifHolder = null;
     try
     {
      ifHolder = (iFolderHolder)iFolderListStore.GetValue(treeIter, 0);
     }
     catch{}
     if (ifHolder == null)
      Debug.PrintLine("\tIter does not exist in iFolderListStore");
     else
      Debug.PrintLine(String.Format("\t{0}, {1}", ifHolder.iFolder.ID, ifHolder.iFolder.Name));
    }
    Debug.PrintLine(String.Format("iFolderListStore Contents ({0}):", iFolderListStore.IterNChildren()));
    TreeIter iter;
    if (iFolderListStore.GetIterFirst(out iter))
    {
     iFolderHolder holder;
     do
     {
      holder =
       (iFolderHolder)iFolderListStore.GetValue(iter, 0);
      Debug.PrintLine(String.Format("\t{0}, {1}", holder.iFolder.ID, holder.iFolder.Name));
     } while (iFolderListStore.IterNext(ref iter));
    }
    Debug.PrintLine(String.Format("deletediFolders ({0})", deletediFolders.Count));
    foreach (string id in deletediFolders.Values)
    {
     Debug.PrintLine(String.Format("\t{0}", id));
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
   if (list != null && path != null && list.IterIsValid(iter))
    list.EmitRowChanged(path, iter);
   return false;
  }
 }
 public class Debug
 {
  public static void PrintLine(string str)
  {
  }
 }
}
