

using System;
using System.Collections;
using System.Diagnostics;
using System.Threading;

using Gtk;
using Gdk;
using Gnome;
using GtkSharp;
using GLib;
using Egg;
using DBus;
using NetworkManager;
using Simias.Client.Event;
using Simias.Client;
using Simias.Client.Authentication;
using Novell.iFolder.Events;
using Novell.iFolder.Controller;


namespace Novell.iFolder
{
 public enum iFolderState : uint
 {
  Starting = 0x0001,
  Stopping = 0x0002,
  Running = 0x0003,
  Stopped = 0x0004
 }

 [Interface("com.novell.iFolder")]
 public class iFolderApplication : Gnome.Program
 {
  private static iFolderApplication application = null;

  private Gtk.Image gAppIcon;
  private Gdk.Pixbuf RunningPixbuf;
  private Gdk.Pixbuf StartingPixbuf;
  private Gdk.Pixbuf StoppingPixbuf;
  private Gdk.PixbufAnimation DownloadingPixbuf;
  private Gdk.PixbufAnimation UploadingPixbuf;
  private Gtk.EventBox eBox;
  private TrayIcon tIcon;
  private iFolderWebService ifws;
  private SimiasWebService simws;
  private iFolderData ifdata;

  private iFolderState CurrentState;
  private Gtk.ThreadNotify iFolderStateChanged;
  private SimiasEventBroker simiasEventBroker;
  private iFolderLoginDialog LoginDialog;






  private bool bCollectionIsSynchronizing;

  private int currentIconAnimationDirection;






  private string collectionSynchronizing;
  private Hashtable synchronizationErrors;

  private DomainController domainController;
  private Simias.Client.Manager simiasManager;

  private NotifyWindow startingUpNotifyWindow = null;
  private NotifyWindow shuttingDownNotifyWindow = null;




        private static Service service;
        private static Connection connection;




        private static NetworkDetect networkDetect;

  public SimiasEventBroker EventBroker
  {
   get
   {
    return this.EventBroker;
   }
  }

  public Simias.Client.Manager SimiasManager
  {
   get
   {
    return this.simiasManager;
   }
  }

  public iFolderState State
  {
   get
   {
    return this.CurrentState;
   }
  }

  public iFolderApplication(string[] args)
   : base("ifolder", "1.0", Modules.UI, args)
  {
Console.WriteLine("iFolderApplication Hash Code: " + this.GetHashCode());

   Util.InitCatalog();

   Util.SetQuitiFolderDelegate(new QuitiFolderDelegate(QuitiFolder));

   tIcon = new TrayIcon("iFolder");

   bCollectionIsSynchronizing = false;
   currentIconAnimationDirection = 0;

   eBox = new EventBox();
   eBox.ButtonPressEvent +=
    new ButtonPressEventHandler(trayapp_clicked);

   RunningPixbuf =
     new Pixbuf(Util.ImagesPath("ifolder24.png"));
   StartingPixbuf =
     new Pixbuf(Util.ImagesPath("ifolder-startup.png"));
   StoppingPixbuf =
     new Pixbuf(Util.ImagesPath("ifolder-shutdown.png"));
   DownloadingPixbuf =
     new Gdk.PixbufAnimation(Util.ImagesPath("ifolder24.gif"));
   UploadingPixbuf =
     new Gdk.PixbufAnimation(Util.ImagesPath("ifolder24-upload.gif"));

   gAppIcon = new Gtk.Image(RunningPixbuf);

   eBox.Add(gAppIcon);
   tIcon.Add(eBox);
   tIcon.ShowAll();

   LoginDialog = null;

   collectionSynchronizing = null;
   synchronizationErrors = new Hashtable();

   iFolderStateChanged = new Gtk.ThreadNotify(
       new Gtk.ReadyEvent(OniFolderStateChanged));



   simiasManager = Util.CreateSimiasManager(args);


   networkDetect = NetworkDetect.Instance;
   if (networkDetect != null)
    networkDetect.StateChanged +=
     new NetworkStateChangedHandler(OnNetworkStateChanged);





  }


  static public iFolderApplication GetApplication()
  {
   return application;
  }


  public new void Run()
  {
   System.Threading.Thread startupThread =
     new System.Threading.Thread(new ThreadStart(StartiFolder));
   startupThread.Start();


   base.Run();
  }



  private void StartiFolder()
  {
   bool simiasRunning = false;

   CurrentState = iFolderState.Starting;
   iFolderStateChanged.WakeupMain();

   if(ifws == null)
   {
    simiasManager.Start();

    string localServiceUrl = simiasManager.WebServiceUri.ToString();
    ifws = new iFolderWebService();
    ifws.Url = localServiceUrl + "/iFolder.asmx";
    LocalService.Start(ifws, simiasManager.WebServiceUri, simiasManager.DataPath);

    simws = new SimiasWebService();
    simws.Url = localServiceUrl + "/Simias.asmx";
    LocalService.Start(simws, simiasManager.WebServiceUri, simiasManager.DataPath);


    while(!simiasRunning)
    {
     try
     {
      ifws.Ping();
      simiasRunning = true;
     }
     catch(Exception e)
     {
      simiasRunning = false;
     }


     System.Threading.Thread.Sleep(10);
    }

    try
    {
     simiasEventBroker = SimiasEventBroker.GetSimiasEventBroker();


     ifdata = iFolderData.GetData();

     domainController = DomainController.GetDomainController();
    }
    catch(Exception e)
    {
     Console.WriteLine(e);
     ifws = null;
    }
   }

   CurrentState = iFolderState.Running;
   iFolderStateChanged.WakeupMain();
  }




  private void StopiFolder()
  {
   CurrentState = iFolderState.Stopping;
   iFolderStateChanged.WakeupMain();

   try
   {
    if(simiasEventBroker != null)
     simiasEventBroker.Deregister();
   }
   catch(Exception e)
   {

    Console.WriteLine(e);
   }


   CurrentState = iFolderState.Stopped;
   iFolderStateChanged.WakeupMain();
  }

  private void OnDomainNeedsCredentialsEvent(object sender, DomainEventArgs args)
  {
   ReLogin(args.DomainID);
  }

  private void ReLogin(string domainID)
  {
   if (LoginDialog != Util.CurrentModalWindow) return;

   if (LoginDialog == null)
   {
    DomainInformation dom = domainController.GetDomain(domainID);
    if (dom != null)
    {
     LoginDialog =
      new iFolderLoginDialog(dom.ID, dom.Name, dom.MemberName);

     if (!Util.RegisterModalWindow(LoginDialog))
     {
      LoginDialog.Destroy();
      LoginDialog = null;
      return;
     }

     LoginDialog.Response +=
      new ResponseHandler(OnReLoginDialogResponse);

     LoginDialog.ShowAll();
    }
   }
   else
   {
    LoginDialog.Present();
   }
  }

  private void OnReLoginDialogResponse(object o, ResponseArgs args)
  {
   switch (args.ResponseId)
   {
    case Gtk.ResponseType.Ok:
     Status status =
      domainController.AuthenticateDomain(
       LoginDialog.Domain,
       LoginDialog.Password,
       LoginDialog.ShouldSavePassword);
     if (status == null ||
      (status.statusCode != StatusCodes.Success &&
       status.statusCode != StatusCodes.SuccessInGrace))
     {
      Util.ShowLoginError(LoginDialog, status.statusCode);
     }
     else
     {

      LoginDialog.Hide();
      LoginDialog.Destroy();
      LoginDialog = null;
     }

     break;
    case Gtk.ResponseType.Cancel:
    case Gtk.ResponseType.DeleteEvent:

     domainController.DisableDomainAutoLogin(LoginDialog.Domain);

     LoginDialog.Hide();
     LoginDialog.Destroy();
     LoginDialog = null;
     break;
   }
  }


  private void OniFolderFileSyncEvent(object o, FileSyncEventArgs args)
  {
   if (args == null || args.CollectionID == null || args.Name == null)
    return;

   try
   {


    if (args.Direction == Simias.Client.Event.Direction.Uploading
     && bCollectionIsSynchronizing && currentIconAnimationDirection != 1)
    {
     gAppIcon.FromAnimation = UploadingPixbuf;
     currentIconAnimationDirection = 1;
    }
    else if (args.Direction == Simias.Client.Event.Direction.Downloading
       && bCollectionIsSynchronizing
       && currentIconAnimationDirection != -1)
    {
     gAppIcon.FromAnimation = DownloadingPixbuf;
     currentIconAnimationDirection = -1;
    }

    iFolderWindow ifwin = Util.GetiFolderWindow();
    if(ifwin != null)
     ifwin.HandleFileSyncEvent(args);

    LogWindow logwin = Util.GetLogWindow(simiasManager);
    if(logwin != null)
     logwin.HandleFileSyncEvent(args);
   }
   catch {}



   if (args.Status != SyncStatus.Success)
   {
    string message = null;

    switch(args.Status)
    {
     case SyncStatus.Success:


      if (synchronizationErrors.ContainsKey(args.CollectionID))
       synchronizationErrors.Remove(args.CollectionID);

      break;
     case SyncStatus.PolicyQuota:
      message = Util.GS("The iFolder is full.\n\nClick <a href=\"ShowSyncLog\">here</a> to view the Synchronization Log.");
      break;
     case SyncStatus.ReadOnly:
      message = Util.GS("You have Read-only access to this iFolder.  Files that you place in this iFolder will not be synchronized.\n\nClick <a href=\"ShowSyncLog\">here</a> to view the Synchronization Log.");
      break;
    }
    if (message != null)
    {
     Hashtable collectionSyncErrors = null;
     if (synchronizationErrors.ContainsKey(args.CollectionID))
     {
      collectionSyncErrors = (Hashtable)synchronizationErrors[args.CollectionID];
     }
     else
     {
      collectionSyncErrors = new Hashtable();
      synchronizationErrors[args.CollectionID] = collectionSyncErrors;
     }
     if (!collectionSyncErrors.ContainsKey(args.Status))
     {
      collectionSyncErrors[args.Status] = message;
     }
    }
   }
  }
  private void OniFolderSyncEvent(object o, CollectionSyncEventArgs args)
  {
   if (args == null || args.ID == null || args.Name == null)
    return;
   switch(args.Action)
   {
    case Simias.Client.Event.Action.StartSync:
    {
     bCollectionIsSynchronizing = true;
     collectionSynchronizing = args.ID;
     break;
    }
    case Simias.Client.Event.Action.StopSync:
    {
     bCollectionIsSynchronizing = false;
     currentIconAnimationDirection = 0;
     gAppIcon.Pixbuf = RunningPixbuf;
      if (collectionSynchronizing != null)
      {
       iFolderHolder ifHolder = ifdata.GetiFolder(collectionSynchronizing);
       if (ifHolder != null)
       {
        if (synchronizationErrors.ContainsKey(ifHolder.iFolder.ID))
        {
         Hashtable collectionSyncErrors = (Hashtable)synchronizationErrors[ifHolder.iFolder.ID];
         ICollection errors = collectionSyncErrors.Keys;
         ArrayList keysToClear = new ArrayList();
         foreach(SyncStatus syncStatusKey in errors)
         {
          string errMsg = (string) collectionSyncErrors[syncStatusKey];
          if (errMsg != null && errMsg.Length > 0)
          {
           NotifyWindow notifyWin = new NotifyWindow(
            tIcon, string.Format(Util.GS("Incomplete Synchronization: {0}"), ifHolder.iFolder.Name),
            errMsg,
            Gtk.MessageType.Warning, 10000);
           notifyWin.LinkClicked +=
            new LinkClickedEventHandler(OnNotifyWindowLinkClicked);
           notifyWin.ShowAll();
           keysToClear.Add(syncStatusKey);
          }
         }
         foreach(SyncStatus syncStatusKey in keysToClear)
         {
          collectionSyncErrors[syncStatusKey] = "";
         }
        }
       }
      }
     collectionSynchronizing = null;
     break;
    }
   }
   try
   {
    iFolderWindow ifwin = Util.GetiFolderWindow();
    if(ifwin != null)
     ifwin.HandleSyncEvent(args);
    LogWindow logwin = Util.GetLogWindow(simiasManager);
    if(logwin != null)
     logwin.HandleSyncEvent(args);
   }
   catch {}
  }
  private void OniFolderAddedEvent(object o, iFolderAddedEventArgs args)
  {
   if (args == null || args.iFolderID == null)
    return;
   iFolderHolder ifHolder = ifdata.GetiFolder(args.iFolderID);
   if (ifHolder == null)
    return;
   if (ifHolder.iFolder == null)
    return;
   if(!ifdata.IsCurrentUser(ifHolder.iFolder.OwnerID))
   {
    if(ifHolder.iFolder.IsSubscription &&
     (ClientConfig.Get(ClientConfig.KEY_NOTIFY_IFOLDERS, "true")
       == "true"))
    {
     NotifyWindow notifyWin = new NotifyWindow(
       tIcon,
       string.Format(Util.GS("New iFolder \"{0}\""),
        ifHolder.iFolder.Name),
       string.Format(Util.GS("{0} has invited you to participate in this shared iFolder.\n\nClick <a href=\"SetUpiFolder:{1}\">here</a> to set up this iFolder."),
            ifHolder.iFolder.Owner, ifHolder.iFolder.CollectionID),
       Gtk.MessageType.Info, 10000);
     notifyWin.LinkClicked +=
      new LinkClickedEventHandler(OnNotifyWindowLinkClicked);
     notifyWin.ShowAll();
    }
   }
  }
  private void OniFolderChangedEvent(object o,
         iFolderChangedEventArgs args)
  {
   if (args == null || args.iFolderID == null)
    return;
   iFolderHolder ifHolder = ifdata.GetiFolder(args.iFolderID);
   if (ifHolder == null)
    return;
   if (ifHolder.iFolder == null)
    return;
   if(ifHolder.iFolder.IsSubscription)
   {
   }
   else
   {
    if(ifHolder.iFolder.HasConflicts)
    {
     if(ClientConfig.Get(ClientConfig.KEY_NOTIFY_COLLISIONS,
       "true") == "true")
     {
      string message = string.Format(
       Util.GS("A conflict has been detected in this iFolder.\n\nClick <a href=\"ResolveiFolderConflicts:{0}\">here</a> to resolve the conflicts.\nWhat is a <a href=\"ShowConflictHelp\">conflict</a>?"),
       args.iFolderID);
      Hashtable collectionSyncErrors = null;
      if (synchronizationErrors.ContainsKey(args.iFolderID))
       collectionSyncErrors = (Hashtable)synchronizationErrors[args.iFolderID];
      else
      {
       collectionSyncErrors = new Hashtable();
       synchronizationErrors[args.iFolderID] = collectionSyncErrors;
      }
      if (!collectionSyncErrors.ContainsKey(SyncStatus.FileNameConflict))
      {
       collectionSyncErrors[SyncStatus.FileNameConflict] = message;
      }
     }
    }
   }
  }
  private void OniFolderDeletedEvent(object o,
         iFolderDeletedEventArgs args)
  {
   if (args == null || args.iFolderID == null)
    return;
  }
  private void OniFolderUserAddedEvent(object o,
         iFolderUserAddedEventArgs args)
  {
   if (args == null || args.iFolderID == null || args.iFolderUser == null)
    return;
   if(ClientConfig.Get(ClientConfig.KEY_NOTIFY_USERS, "true")
       == "true")
   {
    string username;
    iFolderHolder ifHolder = ifdata.GetiFolder(args.iFolderID);
    if( (args.iFolderUser.FN != null) &&
     (args.iFolderUser.FN.Length > 0) )
     username = args.iFolderUser.FN;
    else
     username = args.iFolderUser.Name;
    NotifyWindow notifyWin = new NotifyWindow(
     tIcon, Util.GS("New iFolder User"),
     string.Format(Util.GS("{0} has joined the iFolder \"{1}\""), username, ifHolder.iFolder.Name),
     Gtk.MessageType.Info, 10000);
    notifyWin.LinkClicked +=
     new LinkClickedEventHandler(OnNotifyWindowLinkClicked);
    notifyWin.ShowAll();
   }
  }
  private void OniFolderStateChanged()
  {
   switch(CurrentState)
   {
    case iFolderState.Starting:
     gAppIcon.Pixbuf = StartingPixbuf;
     break;
    case iFolderState.Running:
     if(simiasEventBroker != null)
     {
      simiasEventBroker.iFolderAdded +=
       new iFolderAddedEventHandler(
            OniFolderAddedEvent);
      simiasEventBroker.iFolderChanged +=
       new iFolderChangedEventHandler(
            OniFolderChangedEvent);
      simiasEventBroker.iFolderDeleted +=
       new iFolderDeletedEventHandler(
            OniFolderDeletedEvent);
      simiasEventBroker.iFolderUserAdded +=
       new iFolderUserAddedEventHandler(
            OniFolderUserAddedEvent);
      simiasEventBroker.CollectionSyncEventFired +=
       new CollectionSyncEventHandler(
            OniFolderSyncEvent);
      simiasEventBroker.FileSyncEventFired +=
       new FileSyncEventHandler(
            OniFolderFileSyncEvent);
     }
     if (domainController != null)
     {
      domainController.DomainNeedsCredentials +=
       new DomainNeedsCredentialsEventHandler(OnDomainNeedsCredentialsEvent);
     }
     if (startingUpNotifyWindow != null)
     {
      startingUpNotifyWindow.Hide();
      startingUpNotifyWindow.Destroy();
      startingUpNotifyWindow = null;
     }
     gAppIcon.Pixbuf = RunningPixbuf;
     iFolderWindow ifwin = Util.GetiFolderWindow();
     LogWindow logwin = Util.GetLogWindow(simiasManager);
     GLib.Timeout.Add(100, new GLib.TimeoutHandler(ShowiFolderWindows));
     break;
    case iFolderState.Stopping:
     gAppIcon.Pixbuf = StoppingPixbuf;
     break;
    case iFolderState.Stopped:
     System.Threading.Thread th = new System.Threading.Thread (new System.Threading.ThreadStart (GuaranteeShutdown));
     th.IsBackground = true;
     th.Start ();
     try
     {
Console.WriteLine("iFolderApplication.OniFolderStateChanged:Stopped calling SimiasManager.Stop()...");
      simiasManager.Stop();
Console.WriteLine("\tdone calling SimiasManager.Stop()");
     }
     catch(Exception e)
     {
      Console.WriteLine(e);
     }
     Application.Quit();
     break;
   }
  }
  static private void GuaranteeShutdown()
  {
Console.WriteLine("GuaranteeShutdown(): Waiting 10 seconds before calling System.Environment.Exit(1)");
   System.Threading.Thread.Sleep(10000);
Console.WriteLine("GuaranteeShutdown(): Calling System.Environment.Exit(1) now");
   System.Environment.Exit(1);
  }
  private bool ShowiFolderWindows()
  {
   if (domainController == null)
    domainController = DomainController.GetDomainController();
   if (domainController != null)
   {
    DomainInformation[] domains = domainController.GetDomains();
    if (domains.Length < 1)
    {
     ShowAddAccountWizard();
    }
    else
     Util.LoadiFolderWindows();
   }
   else
    Console.WriteLine("DomainController instance is null");
   return false;
  }
  private void ShowAddAccountWizard()
  {
   AddAccountWizard aaw = new AddAccountWizard(simws);
   if (!Util.RegisterModalWindow(aaw))
   {
    try
    {
     Util.CurrentModalWindow.Present();
    }
    catch{}
    aaw.Destroy();
    return;
   }
   aaw.ShowAll();
  }
  private void OnNotifyWindowLinkClicked(object sender, LinkClickedEventArgs args)
  {
   if (args.LinkID != null)
   {
    if (args.LinkID.Equals("ShowSyncLog"))
     Util.ShowLogWindow(simiasManager);
    else if (args.LinkID.StartsWith("SetUpiFolder"))
    {
     int colonPos = args.LinkID.IndexOf(':');
     if (colonPos > 0)
     {
      string ifolderID = args.LinkID.Substring(colonPos + 1);
      iFolderWindow ifwin = Util.GetiFolderWindow();
      ifwin.DownloadiFolder(ifolderID);
     }
    }
    else if (args.LinkID.StartsWith("ResolveiFolderConflicts"))
    {
     int colonPos = args.LinkID.IndexOf(':');
     if (colonPos > 0)
     {
      string ifolderID = args.LinkID.Substring(colonPos + 1);
      iFolderWindow ifwin = Util.GetiFolderWindow();
      ifwin.ResolveConflicts(ifolderID);
     }
    }
    else if (args.LinkID.Equals("ShowAccountsPage"))
    {
     showPrefsPage(1);
    }
    else if (args.LinkID.Equals("ShowConflictHelp"))
    {
     Util.ShowHelp("conflicts.html", null);
    }
    else if (args.LinkID.Equals("CancelStartup"))
    {
     Console.WriteLine("CancelStartup");
    }
    else if (args.LinkID.Equals("ForceShutdown"))
    {
     Console.WriteLine("ForceShutdown");
    }
   }
   NotifyWindow notifyWindow = sender as NotifyWindow;
   notifyWindow.Hide();
   notifyWindow.Destroy();
  }
  private void trayapp_clicked(object obj, ButtonPressEventArgs args)
  {
   if (CurrentState == iFolderState.Starting)
   {
    if (startingUpNotifyWindow == null)
    {
     startingUpNotifyWindow = new NotifyWindow(
      tIcon, Util.GS("iFolder is starting"),
      Util.GS("Please wait for iFolder to start...\n\nPress <a href=\"CancelStartup\">here</a> to cancel."),
      Gtk.MessageType.Info, 0);
     startingUpNotifyWindow.LinkClicked +=
      new LinkClickedEventHandler(OnNotifyWindowLinkClicked);
     startingUpNotifyWindow.ShowAll();
    }
    return;
   }
   else if (CurrentState == iFolderState.Stopping)
   {
    if (shuttingDownNotifyWindow == null)
    {
     shuttingDownNotifyWindow = new NotifyWindow(
      tIcon, Util.GS("iFolder is shutting down"),
      Util.GS("Press <a href=\"ForceShutdown\">here</a> to force iFolder to shut down now."),
      Gtk.MessageType.Info, 0);
     shuttingDownNotifyWindow.LinkClicked +=
      new LinkClickedEventHandler(OnNotifyWindowLinkClicked);
     shuttingDownNotifyWindow.ShowAll();
    }
    return;
   }
   if (Util.CurrentModalWindow != null)
   {
Console.WriteLine("Modal present");
    try
    {
     Util.CurrentModalWindow.Present();
     return;
    }
    catch{}
   }
   switch(args.Event.Button)
   {
    case 1:
     DomainInformation[] domains = domainController.GetDomains();
     if (domains.Length < 1)
     {
      ShowAddAccountWizard();
     }
     else
     {
      iFolderWindow ifwin = Util.GetiFolderWindow();
      if (ifwin == null || !ifwin.IsActive)
       Util.ShowiFolderWindow();
      else
       ifwin.CloseWindow();
     }
     break;
    case 2:
     break;
    case 3:
     show_tray_menu();
     break;
   }
  }
  private void show_tray_menu()
  {
   Menu trayMenu = new Menu();
   MenuItem iFolders_item =
     new MenuItem (Util.GS("iFolders"));
   trayMenu.Append (iFolders_item);
   DomainInformation[] domains = domainController.GetDomains();
   if (domains.Length < 1)
    iFolders_item.Sensitive = false;
   iFolders_item.Activated +=
     new EventHandler(showiFolderWindow);
   MenuItem accounts_item =
     new MenuItem (Util.GS("Account Settings..."));
   trayMenu.Append (accounts_item);
   accounts_item.Activated +=
     new EventHandler(show_accounts);
   MenuItem logview_item =
     new MenuItem (Util.GS("Synchronization Log"));
   trayMenu.Append (logview_item);
   if (domains.Length < 1)
    logview_item.Sensitive = false;
   logview_item.Activated +=
     new EventHandler(showLogWindow);
   trayMenu.Append(new SeparatorMenuItem());
   ImageMenuItem prefs_item = new ImageMenuItem (
           Util.GS("Preferences"));
   prefs_item.Image = new Gtk.Image(Gtk.Stock.Preferences,
           Gtk.IconSize.Menu);
   trayMenu.Append(prefs_item);
   prefs_item.Activated +=
     new EventHandler(show_preferences);
   ImageMenuItem help_item = new ImageMenuItem (
           Util.GS("Help"));
   help_item.Image = new Gtk.Image(Gtk.Stock.Help,
           Gtk.IconSize.Menu);
   trayMenu.Append(help_item);
   help_item.Activated +=
     new EventHandler(show_help);
   ImageMenuItem about_item = new ImageMenuItem (
           Util.GS("About"));
   about_item.Image = new Gtk.Image(Gnome.Stock.About,
           Gtk.IconSize.Menu);
   trayMenu.Append(about_item);
   about_item.Activated +=
     new EventHandler(show_about);
   trayMenu.Append(new SeparatorMenuItem());
   MenuItem ifolderDataDebug =
     new MenuItem ("Print iFolderData Debug Information");
   trayMenu.Append (ifolderDataDebug);
   ifolderDataDebug.Activated +=
     new EventHandler(PrintiFolderDataDebugState);
   trayMenu.Append(new SeparatorMenuItem());
   ImageMenuItem quit_item = new ImageMenuItem (
           Util.GS("Quit"));
   quit_item.Image = new Gtk.Image(Gtk.Stock.Quit,
           Gtk.IconSize.Menu);
   trayMenu.Append(quit_item);
   quit_item.Activated +=
     new EventHandler(quit_ifolder);
   trayMenu.ShowAll();
   trayMenu.Popup(null, null, null, IntPtr.Zero, 3,
     Gtk.Global.CurrentEventTime);
  }
  [Method]
  public virtual void ShowWindow()
  {
Console.WriteLine("iFolderApplication.ShowWindow()");
Console.WriteLine("Hash Code: " + this.GetHashCode());
Console.WriteLine(Environment.StackTrace);
   if (Util.CurrentModalWindow != null)
   {
Console.WriteLine("Modal present");
    try
    {
     Util.CurrentModalWindow.Present();
     return;
    }
    catch{}
   }
   DomainInformation[] domains = domainController.GetDomains();
   if (domains.Length < 1)
   {
    ShowAddAccountWizard();
   }
   else
   {
    iFolderWindow ifwin = Util.GetiFolderWindow();
    if (ifwin == null || !ifwin.IsActive)
     Util.ShowiFolderWindow();
    else
     ifwin.Present();
   }
  }
  private void PrintiFolderDataDebugState(object o, EventArgs args)
  {
   ifdata.PrintDebugState();
  }
  public void QuitiFolder()
  {
   quit_ifolder(null, null);
  }
  private void quit_ifolder(object o, EventArgs args)
  {
   Util.SaveiFolderWindows();
   Util.CloseiFolderWindows();
   if(CurrentState == iFolderState.Stopping)
   {
    try
    {
     simiasManager.Stop();
    }
    catch{}
    System.Environment.Exit(1);
   }
   else
   {
    System.Threading.Thread stopThread =
     new System.Threading.Thread(new ThreadStart(StopiFolder));
    stopThread.Start();
   }
  }
  private void show_help(object o, EventArgs args)
  {
   Util.ShowHelp("front.html", null);
  }
  private void show_about(object o, EventArgs args)
  {
   Util.ShowAbout();
  }
  private void show_preferences(object o, EventArgs args)
  {
   showPrefsPage(0);
  }
  private void show_accounts(object o, EventArgs args)
  {
   showPrefsPage(1);
  }
  private void showPrefsPage(int page)
  {
   Util.ShowPrefsPage(page, simiasManager);
  }
  private void showiFolderWindow(object o, EventArgs args)
  {
   Util.ShowiFolderWindow();
  }
  private void showLogWindow(object o, EventArgs args)
  {
   Util.ShowLogWindow(simiasManager);
  }
  private void OnNetworkStateChanged(object o, NetworkStateChangedArgs args)
  {
   Console.WriteLine("OnNetworkStateChanged: {0}", args.Connected);
  }
  public static iFolderApplication FindInstance()
  {
   Connection connection = Bus.GetSessionBus();
   Service service = Service.Get(connection, "com.novell.iFolder");
   return service.GetObject(
      typeof(iFolderApplication),
      "/com/novell/iFolder/Application")
       as iFolderApplication;
  }
  private static void RegisterWithDBus(iFolderApplication theApp)
  {
   if (theApp == null)
   {
    Console.WriteLine("RegisterWithDBus() called with a null application.");
    return;
   }
   try
   {
    connection = Bus.GetSessionBus();
    if (connection == null)
    {
     Console.WriteLine("Could not get a connection to the D-Bus session");
     return;
    }
    service = new Service(connection, "com.novell.iFolder");
    if (service == null)
    {
     Console.WriteLine("Could not create a D-Bus service instance.");
     return;
    }
    service.RegisterObject(theApp, "/com/novell/iFolder/Application");
   }
   catch(Exception e)
   {
    Console.WriteLine(e);
    Console.WriteLine("Could not connect to D-Bus.  D-Bus support will be disabled for this instance: " + e.Message);
   }
  }
  private static void UnregisterWithDBus()
  {
   try
   {
    if (application != null && service != null)
     service.UnregisterObject(application);
   }
   catch{}
  }
  public static void Main (string[] args)
  {
   try
   {
    iFolderApplication existingApp = iFolderApplication.FindInstance();
    if (existingApp != null)
    {
     try
     {
      existingApp.ShowWindow();
     }
     catch(DBus.DBusException)
     {
      return;
     }
     return;
    }
   }
   catch(Exception e)
   {
    Console.WriteLine(e);
    Process[] processes =
     System.Diagnostics.Process.GetProcessesByName("iFolderClient");
    if (processes.Length > 1)
    {
     Console.WriteLine("iFolder is already running.  If you were trying " +
           "to control the already running instance of iFolder, D-Bus must be enabled.  " +
           "iFolder could not connect to your D-Bus Session Bus.");
     return;
    }
   }
   try
   {
    application = new iFolderApplication(args);
    RegisterWithDBus(application);
    application.Run();
    UnregisterWithDBus();
   }
   catch(Exception bigException)
   {
    Console.WriteLine(bigException);
    iFolderCrashDialog cd = new iFolderCrashDialog(bigException);
    cd.Run();
    cd.Hide();
    cd.Destroy();
    cd = null;
    if(application.EventBroker != null)
     application.EventBroker.Deregister();
Console.Write("iFolderApplication: Inside catch and calling SimiasManager.Stop()...");
    application.SimiasManager.Stop();
Console.WriteLine("\tdone calling SimiasManager.Stop()");
    UnregisterWithDBus();
    Application.Quit();
   }
  }
 }
}
