

using System;
using System.Collections;
using System.Diagnostics;
using System.Threading;
using Gtk;

using Simias.Client.Event;
using Simias.Client;

using Novell.iFolder.Events;


namespace Novell.iFolder
{

 internal enum SimiasEventType : uint
 {
  NewUser = 0x0001,
  NewiFolder = 0x0002,
  ChangedUser = 0x0003,
  ChangediFolder = 0x0004,
  DelUser = 0x0005,
  DeliFolder = 0x0006,
  NewDomain = 0x0007,
  DelDomain = 0x0008
 }


 internal class SimiasEvent
 {
  private iFolderUser ifUser;
  private string ifolderID;
  private string userID;
  private string domainID;
  private SimiasEventType type;

  public SimiasEvent(string iFolderID, iFolderUser ifldrUser,
     string UserID, SimiasEventType type)
  {
   this.ifUser = ifldrUser;
   this.ifolderID = iFolderID;
   this.userID = UserID;
   this.domainID = null;
   this.type = type;
  }

  public SimiasEvent(string domainID, SimiasEventType type)
  {
   this.ifUser = null;
   this.ifolderID = null;
   this.userID = null;
   this.domainID = domainID;
   this.type = type;
  }

  public iFolderUser iFolderUser
  {
   get{ return this.ifUser; }
  }

  public string iFolderID
  {
   get{return this.ifolderID;}
  }

  public string UserID
  {
   get{return this.userID;}
  }

  public string DomainID
  {
   get{return this.domainID;}
  }

  public SimiasEventType EventType
  {
   get{return this.type;}
  }
 }

 public class SimiasEventBroker
 {
  private static SimiasEventBroker instance = null;

  private iFolderWebService ifws;

  private iFolderData ifdata;
  private IProcEventClient simiasEventClient;

  private Gtk.ThreadNotify SimiasEventFired;
  private Gtk.ThreadNotify SyncEventFired;
  private Gtk.ThreadNotify FileEventFired;
  private Gtk.ThreadNotify GenericEventFired;
  private Queue NodeEventQueue;
  private Queue SyncEventQueue;
  private Queue FileEventQueue;
  private Queue SimiasEventQueue;
  private Queue NotifyEventQueue;
  private bool runEventThread;
  private Thread SEThread;
  private ManualResetEvent SEEvent;
  private Manager simiasManager;




  public event iFolderAddedEventHandler iFolderAdded;
  public event iFolderChangedEventHandler iFolderChanged;
  public event iFolderDeletedEventHandler iFolderDeleted;




  public event iFolderUserAddedEventHandler iFolderUserAdded;
  public event iFolderUserChangedEventHandler iFolderUserChanged;
  public event iFolderUserDeletedEventHandler iFolderUserDeleted;




  public event CollectionSyncEventHandler CollectionSyncEventFired;
  public event FileSyncEventHandler FileSyncEventFired;




  public event DomainAddedEventHandler DomainAdded;
  public event DomainDeletedEventHandler DomainDeleted;
  public event DomainUpEventHandler DomainUpEventFired;

  private SimiasEventBroker()
  {
   this.simiasManager = Util.GetSimiasManager();
   string localServiceUrl =
    simiasManager.WebServiceUri.ToString();
   ifws = new iFolderWebService();
   ifws.Url = localServiceUrl + "/iFolder.asmx";
   LocalService.Start(ifws, simiasManager.WebServiceUri, simiasManager.DataPath);

   NodeEventQueue = new Queue();
   SyncEventQueue = new Queue();
   FileEventQueue = new Queue();
   SimiasEventQueue = new Queue();
   NotifyEventQueue = new Queue();

   SimiasEventFired = new Gtk.ThreadNotify(
       new Gtk.ReadyEvent(OnSimiasEventFired) );
   SyncEventFired = new Gtk.ThreadNotify(
       new Gtk.ReadyEvent(OnSyncEventFired) );
   FileEventFired = new Gtk.ThreadNotify(
       new Gtk.ReadyEvent(OnFileEventFired) );
   GenericEventFired = new Gtk.ThreadNotify(
       new Gtk.ReadyEvent(OnGenericEventFired) );

   SEThread = new Thread(new ThreadStart(SimiasEventThread));
   SEThread.IsBackground = true;
   SEEvent = new ManualResetEvent(false);
  }

  public static SimiasEventBroker GetSimiasEventBroker()
  {
   lock (typeof(SimiasEventBroker))
   {
    if (instance == null)
    {
     instance = new SimiasEventBroker();

     instance.Register();
    }

    return instance;
   }
  }

  private void Register()
  {
   ifdata = iFolderData.GetData();

   simiasEventClient = new IProcEventClient(
     new IProcEventError( ErrorHandler), null);

   simiasEventClient.Register();

   simiasEventClient.SetEvent( IProcEventAction.AddNodeCreated,
    new IProcEventHandler( SimiasEventHandler ) );

   simiasEventClient.SetEvent( IProcEventAction.AddNodeChanged,
    new IProcEventHandler( SimiasEventHandler ) );

   simiasEventClient.SetEvent( IProcEventAction.AddNodeDeleted,
    new IProcEventHandler( SimiasEventHandler ) );

   simiasEventClient.SetEvent( IProcEventAction.AddCollectionSync,
    new IProcEventHandler( SimiasEventSyncCollectionHandler) );

   simiasEventClient.SetEvent( IProcEventAction.AddFileSync,
    new IProcEventHandler( SimiasEventSyncFileHandler) );

   simiasEventClient.SetEvent( IProcEventAction.AddNotifyMessage,
    new IProcEventHandler( SimiasEventNotifyHandler) );

   runEventThread = true;
   SEThread.Start();
  }

  public void Deregister()
  {
   try
   {
    if (simiasEventClient != null)
    {
     simiasEventClient.Deregister();
     simiasEventClient = null;
    }
   }
   catch(Exception e)
   {

   }

   runEventThread = false;
  }

  private void SimiasEventHandler(SimiasEventArgs args)
  {
   lock(SimiasEventQueue)
   {
    SimiasEventQueue.Enqueue(args);
   }
   SEEvent.Set();
  }


  private void SimiasEventThread()
  {
   bool hasmore = false;

   while(runEventThread)
   {
    lock(SimiasEventQueue)
    {
     hasmore = (SimiasEventQueue.Count > 0);
    }

    while(hasmore && runEventThread)
    {
     SimiasEventArgs args;

     lock(NodeEventQueue)
     {
      args = (SimiasEventArgs)SimiasEventQueue.Dequeue();
     }

     NodeEventArgs nargs = args as NodeEventArgs;
     switch(nargs.EventType)
     {
      case Simias.Client.Event.EventType.NodeCreated:
       NodeCreatedHandler(nargs);
       break;
      case Simias.Client.Event.EventType.NodeDeleted:
       NodeDeletedHandler(nargs);
       break;
      case Simias.Client.Event.EventType.NodeChanged:
       NodeChangedHandler(nargs);
       break;
     }

     lock(SimiasEventQueue)
     {
      hasmore = (SimiasEventQueue.Count > 0);
     }
    }
    SEEvent.WaitOne();
    SEEvent.Reset();
   }
  }


  private void ErrorHandler( ApplicationException e, object context )
  {

  }


  private void SimiasEventNotifyHandler(SimiasEventArgs args)
  {
   NotifyEventArgs notifyEventArgs = args as NotifyEventArgs;
   lock(NotifyEventQueue)
   {
    NotifyEventQueue.Enqueue(notifyEventArgs);
    GenericEventFired.WakeupMain();
   }
  }


  private void SimiasEventSyncFileHandler(SimiasEventArgs args)
  {
   FileSyncEventArgs fileSyncArgs = args as FileSyncEventArgs;

   lock(FileEventQueue)
   {
    FileEventQueue.Enqueue(fileSyncArgs);
    FileEventFired.WakeupMain();
   }
  }



  private void SimiasEventSyncCollectionHandler(SimiasEventArgs args)
  {
   CollectionSyncEventArgs syncEventArgs =
    args as CollectionSyncEventArgs;

   if(ifdata.IsiFolder(syncEventArgs.ID))
   {
    iFolderHolder ifHolder =
     ifdata.GetiFolder(syncEventArgs.ID);

    switch (syncEventArgs.Action)
    {
     case Simias.Client.Event.Action.StartLocalSync:
      ifHolder.State = iFolderState.SynchronizingLocal;
      break;
     case Simias.Client.Event.Action.StartSync:
      ifHolder.State = iFolderState.Synchronizing;
      break;
     case Simias.Client.Event.Action.StopSync:
      try
      {
       SyncSize syncSize = ifws.CalculateSyncSize(syncEventArgs.ID);
       ifHolder.ObjectsToSync = syncSize.SyncNodeCount;
      }
      catch
      {}

      if (ifHolder.ObjectsToSync > 0)
       ifHolder.State = iFolderState.Normal;
      else
      {
       if (syncEventArgs.Connected)
        ifHolder.State = iFolderState.Normal;
       else
        ifHolder.State = iFolderState.Disconnected;
      }
      break;
    }

    if( (ifHolder.iFolder.UnManagedPath == null) ||
      (ifHolder.iFolder.UnManagedPath.Length == 0) )
    {


     ifHolder = ifdata.ReadiFolder(syncEventArgs.ID);
     lock(NodeEventQueue)
     {
      NodeEventQueue.Enqueue(new SimiasEvent(
         ifHolder.iFolder.ID, null,
         null, SimiasEventType.ChangediFolder));
      SimiasEventFired.WakeupMain();
     }
    }
   }


   lock(SyncEventQueue)
   {
    SyncEventQueue.Enqueue(syncEventArgs);
    SyncEventFired.WakeupMain();
   }
  }


  private void NodeCreatedHandler(NodeEventArgs nargs)
  {
   switch(nargs.Type)
   {
    case "Subscription":
    {
     if(ifdata.ISPOBox(nargs.Collection))
     {



      iFolderHolder ifHolder =
       ifdata.ReadAvailableiFolder(nargs.ID,
              nargs.Collection);



      if(ifHolder != null)
      {
       lock(NodeEventQueue)
       {
        NodeEventQueue.Enqueue(new SimiasEvent(
         ifHolder.iFolder.CollectionID, null, null,
         SimiasEventType.NewiFolder));
        SimiasEventFired.WakeupMain();
       }
      }
     }

     break;
    }

    case "Member":
    {
     if(ifdata.IsiFolder(nargs.Collection))
     {
      iFolderUser newuser =
       ifdata.GetiFolderUserFromNodeID(
         nargs.Collection, nargs.ID);

      if( (newuser != null) &&
        !ifdata.IsCurrentUser(newuser.UserID) )
      {
       lock(NodeEventQueue)
       {
        NodeEventQueue.Enqueue(new SimiasEvent(
           nargs.Collection, newuser,
           newuser.UserID,
           SimiasEventType.NewUser));
        SimiasEventFired.WakeupMain();
       }
      }
     }
     break;
    }

    case "Collection":
    {
     iFolderHolder ifHolder =
       ifdata.ReadiFolder(nargs.Collection);

     if(ifHolder != null)
     {
      lock(NodeEventQueue)
      {
       NodeEventQueue.Enqueue(new SimiasEvent(
        ifHolder.iFolder.ID, null, null,
        SimiasEventType.NewiFolder));
       SimiasEventFired.WakeupMain();
      }
     }
     break;
    }

    case "Domain":
    {

     lock(NodeEventQueue)
     {
      NodeEventQueue.Enqueue(
       new SimiasEvent(nargs.Collection, SimiasEventType.NewDomain));
      SimiasEventFired.WakeupMain();
     }
     break;
    }
   }
  }




  private void NodeChangedHandler(NodeEventArgs nargs)
  {
   switch(nargs.Type)
   {
    case "Collection":
    {
     iFolderHolder ifHolder =
      ifdata.ReadiFolder(nargs.Collection);

     if( (ifHolder != null) &&
      (ifHolder.iFolder.HasConflicts) )
     {
      lock(NodeEventQueue)
      {
       NodeEventQueue.Enqueue(new SimiasEvent(
        ifHolder.iFolder.ID, null, null,
        SimiasEventType.ChangediFolder));
       SimiasEventFired.WakeupMain();
      }
     }

     break;
    }

    case "Member":
    {
     if(ifdata.IsiFolder(nargs.Collection))
     {
      iFolderUser newuser =
       ifdata.GetiFolderUserFromNodeID(
         nargs.Collection, nargs.ID);

      if( (newuser != null) &&
        !ifdata.IsCurrentUser(newuser.UserID) )
      {
       lock(NodeEventQueue)
       {
        NodeEventQueue.Enqueue(new SimiasEvent(
           nargs.Collection, newuser,
           newuser.UserID,
           SimiasEventType.ChangedUser));
        SimiasEventFired.WakeupMain();
       }
      }
     }
     break;
    }


    case "Subscription":
    {
     if(ifdata.ISPOBox(nargs.Collection))
     {



      iFolderHolder ifHolder =
       ifdata.ReadAvailableiFolder(nargs.ID,
              nargs.Collection);
      if(ifHolder != null)
      {
       lock(NodeEventQueue)
       {
        NodeEventQueue.Enqueue(new SimiasEvent(
         ifHolder.iFolder.CollectionID, null, null,
         SimiasEventType.ChangediFolder));
        SimiasEventFired.WakeupMain();
       }
      }
     }
     break;
    }
   }
  }



  private void NodeDeletedHandler(NodeEventArgs nargs)
  {
   switch(nargs.Type)
   {
    case "Subscription":
    {
     if(ifdata.ISPOBox(nargs.Collection))
     {
      lock(NodeEventQueue)
      {
       ifdata.DeliFolder(nargs.ID);
       NodeEventQueue.Enqueue(new SimiasEvent(
        nargs.Collection, null, null,
        SimiasEventType.DeliFolder));
       SimiasEventFired.WakeupMain();
      }
     }
     break;
    }
    case "Collection":
    {
     lock(NodeEventQueue)
     {
      iFolderHolder ifHolder =
       ifdata.GetiFolder(nargs.Collection);
      if( (ifHolder != null) &&
       (!ifHolder.iFolder.IsSubscription) )
      {
       NodeEventQueue.Enqueue(new SimiasEvent(
        nargs.Collection, null, null,
        SimiasEventType.DeliFolder));
      }
      SimiasEventFired.WakeupMain();
     }
     break;
    }
    case "Member":
    {
     if(ifdata.IsiFolder(nargs.Collection))
     {
      lock(NodeEventQueue)
      {
       NodeEventQueue.Enqueue(new SimiasEvent(
        nargs.Collection, null, nargs.ID,
         SimiasEventType.DelUser));
       SimiasEventFired.WakeupMain();
      }
     }
     break;
    }

    case "Domain":
    {

     lock(NodeEventQueue)
     {
      NodeEventQueue.Enqueue(
       new SimiasEvent(nargs.Collection, SimiasEventType.DelDomain));
      SimiasEventFired.WakeupMain();
     }
     break;
    }
   }
  }


  private void OnGenericEventFired()
  {
   bool hasmore = false;

   lock(NotifyEventQueue)
   {
    hasmore = (NotifyEventQueue.Count > 0);
   }

   while(hasmore)
   {
    NotifyEventArgs args;

    lock(NotifyEventQueue)
    {
     args = (NotifyEventArgs)NotifyEventQueue.Dequeue();
    }

    if (args == null || args.EventData == null || args.Message == null)
     return;
    switch (args.EventData)
    {
     case "Domain-Up":
Console.WriteLine("SimiasEventBroker.OnGenericEventFired: Domain-Up");
      if (DomainUpEventFired != null)
      {
       string domainID = args.Message;
       DomainUpEventFired(this, new DomainEventArgs(domainID));
      }
      break;
     default:
      break;
    }

    lock(NotifyEventQueue)
    {
     hasmore = (NotifyEventQueue.Count > 0);
    }
   }
  }


  private void OnFileEventFired()
  {
   bool hasmore = false;

   lock(FileEventQueue)
   {
    hasmore = (FileEventQueue.Count > 0);
   }
   while(hasmore)
   {
    FileSyncEventArgs args;

    lock(FileEventQueue)
    {
     args = (FileSyncEventArgs)FileEventQueue.Dequeue();
    }

Console.WriteLine("SimiasEventBroker.OnFileEventFired");
    if(FileSyncEventFired != null)
     FileSyncEventFired(this, args);

    lock(FileEventQueue)
    {
     hasmore = (FileEventQueue.Count > 0);
    }
   }

  }


  private void OnSyncEventFired()
  {
   bool hasmore = false;

   lock(SyncEventQueue)
   {
    hasmore = (SyncEventQueue.Count > 0);
   }
   while(hasmore)
   {
    CollectionSyncEventArgs args;

    lock(SyncEventQueue)
    {
     args = (CollectionSyncEventArgs)SyncEventQueue.Dequeue();
    }

Console.WriteLine("SimiasEventBroker.OnSyncEventFired");
    if(CollectionSyncEventFired != null)
     CollectionSyncEventFired(this, args);

    lock(SyncEventQueue)
    {
     hasmore = (SyncEventQueue.Count > 0);
    }
   }
  }



  private void OnSimiasEventFired()
  {
   bool hasmore = false;


   lock(NodeEventQueue)
   {
    hasmore = (NodeEventQueue.Count > 0);
   }

   while(hasmore)
   {
    SimiasEvent sEvent;

    lock(NodeEventQueue)
    {
     sEvent = (SimiasEvent)NodeEventQueue.Dequeue();
    }

    switch(sEvent.EventType)
    {
     case SimiasEventType.NewUser:
Console.WriteLine("SimiasEventBroker.OnSimiasEventFired: NewUser");
      if(iFolderUserAdded != null)
       iFolderUserAdded(this,
        new iFolderUserAddedEventArgs(
          sEvent.iFolderUser,
          sEvent.iFolderID));
      break;
     case SimiasEventType.NewiFolder:
Console.WriteLine("SimiasEventBroker.OnSimiasEventFired: NewiFolder");
      if(iFolderAdded != null)
       iFolderAdded(this,
        new iFolderAddedEventArgs(sEvent.iFolderID));
      break;
     case SimiasEventType.ChangedUser:
Console.WriteLine("SimiasEventBroker.OnSimiasEventFired: ChangedUser");
      if(iFolderUserChanged != null)
       iFolderUserChanged(this,
        new iFolderUserChangedEventArgs(
          sEvent.iFolderUser,
          sEvent.iFolderID));
      break;
     case SimiasEventType.ChangediFolder:
Console.WriteLine("SimiasEventBroker.OnSimiasEventFired: ChangediFolder");
      if(iFolderChanged != null)
       iFolderChanged(this,
        new iFolderChangedEventArgs(sEvent.iFolderID));
      break;
     case SimiasEventType.DelUser:
Console.WriteLine("SimiasEventBroker.OnSimiasEventFired: DelUser");
      if(iFolderUserDeleted != null)
       iFolderUserDeleted(this,
        new iFolderUserDeletedEventArgs(
          sEvent.UserID,
          sEvent.iFolderID));
      break;
     case SimiasEventType.DeliFolder:
Console.WriteLine("SimiasEventBroker.OnSimiasEventFired: DeliFolder");
      if(iFolderDeleted != null)
       iFolderDeleted(this,
        new iFolderDeletedEventArgs(sEvent.iFolderID));
      break;
     case SimiasEventType.NewDomain:
Console.WriteLine("SimiasEventBroker.OnSimiasEventFired: NewDomain");
      if(DomainAdded != null)
       DomainAdded(this,
        new DomainEventArgs(sEvent.DomainID));
      break;
     case SimiasEventType.DelDomain:
Console.WriteLine("SimiasEventBroker.OnSimiasEventFired: DelDomain");
      if(DomainDeleted != null)
       DomainDeleted(this,
        new DomainEventArgs(sEvent.DomainID));
      break;
    }

    lock(NodeEventQueue)
    {
     hasmore = (NodeEventQueue.Count > 0);
    }
   }
  }
 }
}
