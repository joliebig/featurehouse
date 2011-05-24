

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


using Simias.Client.Event;
using Simias.Client;
using Simias.Client.Authentication;
using Novell.iFolder.Events;
using Novell.iFolder.Controller;
using System.Reflection;
using System.IO;

namespace Novell.iFolder
{
 public enum iFolderAppState : uint
 {
  Starting = 0x0001,
  Stopping = 0x0002,
  Running = 0x0003,
  Stopped = 0x0004
 }


 public class iFolderApplication : Gnome.Program
 {
  private static iFolderApplication application = null;

  private Gtk.Image gAppIcon;
  private Gdk.Pixbuf RunningPixbuf;
  private Gdk.Pixbuf StartingPixbuf;
  private Gdk.Pixbuf StoppingPixbuf;


  private Gdk.Pixbuf DownloadingPixbuf;
  private Gdk.Pixbuf UploadingPixbuf;
  private Gtk.EventBox eBox;
  private Egg.TrayIcon tIcon;
  private iFolderWebService ifws;
  private SimiasWebService simws;
  private iFolderData ifdata;
  private TimerCallback timerDelegate;
  private Timer splashTimer;
  private iFolderAppState CurrentState;
  private Gtk.ThreadNotify iFolderAppStateChanged;
  private SimiasEventBroker simiasEventBroker;
  private iFolderLoginDialog LoginDialog;







  private int currentIconAnimationDirection;






  private string collectionSynchronizing;
  private Hashtable synchronizationErrors;

  private DomainController domainController;
  private Simias.Client.Manager simiasManager;

  private NotifyWindow startingUpNotifyWindow;
  private NotifyWindow shuttingDownNotifyWindow;

  private bool forceShutdown;

  private iFolderMsgDialog ClientUpgradeDialog;


  private Gtk.Window startupWind;

                private iFolderMsgDialog quitDlg;
                private bool quit_iFolder;
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
  public iFolderAppState State
  {
   get
   {
    return this.CurrentState;
   }
  }
  public iFolderApplication(string[] args)
   : base("ifolder", "1.0", Modules.UI, args)
  {
   Util.InitCatalog();
   Util.SetQuitiFolderDelegate(new QuitiFolderDelegate(QuitiFolder));
   tIcon = new Egg.TrayIcon("iFolder");
   currentIconAnimationDirection = 0;
   eBox = new EventBox();
   eBox.ButtonPressEvent +=
    new ButtonPressEventHandler(trayapp_clicked);
                        quitDlg = new iFolderMsgDialog(
                        null,
                        iFolderMsgDialog.DialogType.Question,
                        iFolderMsgDialog.ButtonSet.YesNo,
                        Util.GS("Exit Novell iFolder"),
                         Util.GS("If you exit the Novell iFolder application, changes in your iFolder will no longer be tracked.\nThe next time you login, Novell iFolder will reconcile any differences between your iFolder and Server.\n\nAre you sure you want to exit the Application ?"),
                        " ");
                         quitDlg.Response += new ResponseHandler(YesNo_Clicked);
   RunningPixbuf =
     new Pixbuf(Util.ImagesPath("ifolder16.png"));
   StartingPixbuf =
     new Pixbuf(Util.ImagesPath("ifolder-waiting16.png"));
   StoppingPixbuf =
     new Pixbuf(Util.ImagesPath("ifolder-waiting16.png"));
   DownloadingPixbuf =
     new Pixbuf(Util.ImagesPath("ifolder-download16.png"));
   UploadingPixbuf =
     new Pixbuf(Util.ImagesPath("ifolder-upload16.png"));
   gAppIcon = new Gtk.Image(RunningPixbuf);
   eBox.Add(gAppIcon);
   tIcon.Add(eBox);
   tIcon.ShowAll();
   LoginDialog = null;
   collectionSynchronizing = null;
   synchronizationErrors = new Hashtable();
   iFolderAppStateChanged = new Gtk.ThreadNotify(
       new Gtk.ReadyEvent(OniFolderAppStateChanged));
   simiasManager = Util.CreateSimiasManager(args);
   startingUpNotifyWindow = null;
   shuttingDownNotifyWindow = null;
   forceShutdown = false;
   ClientUpgradeDialog = null;
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
   CurrentState = iFolderAppState.Starting;
   iFolderAppStateChanged.WakeupMain();
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
     catch(Exception)
     {
      simiasRunning = false;
     }
     if (forceShutdown)
     {
      QuitiFolder();
      return;
     }
     System.Threading.Thread.Sleep(10);
    }
               LogInit(simiasManager.DataPath);
    if (forceShutdown)
     QuitiFolder();
    else
    {
     try
     {
      simiasEventBroker = SimiasEventBroker.GetSimiasEventBroker();
      ifdata = iFolderData.GetData();
      domainController = DomainController.GetDomainController();
     }
     catch(Exception e)
     {
      Debug.PrintLine(e.Message);
      ifws = null;
     }
    }
   }
   CleanUpPassphrase();
   CurrentState = iFolderAppState.Running;
   iFolderAppStateChanged.WakeupMain();
  }
  private void StopiFolder()
  {
   CurrentState = iFolderAppState.Stopping;
   iFolderAppStateChanged.WakeupMain();
   try
   {
    if(simiasEventBroker != null)
     simiasEventBroker.Deregister();
   }
   catch(Exception e)
   {
    Debug.PrintLine(e.Message);
   }
   CurrentState = iFolderAppState.Stopped;
   iFolderAppStateChanged.WakeupMain();
  }
  private void OnDomainNeedsCredentialsEvent(object sender, DomainEventArgs args)
  {
   ReLogin(args.DomainID);
  }
  private void OnClientUpgradeAvailableEvent(object sender, DomainClientUpgradeAvailableEventArgs args)
  {
   if (ClientUpgradeDialog != null)
    return;
   if(DomainController.upgradeStatus.statusCode == StatusCodes.ServerOld)
   {
    ClientUpgradeDialog = new iFolderMsgDialog(
    null,
    iFolderMsgDialog.DialogType.Info,
    iFolderMsgDialog.ButtonSet.Ok,
    Util.GS("iFolder Server Older"),
    Util.GS("The server is running an older version."),
    string.Format(Util.GS("The server needs to be upgraded to be connected from this client")));
   }
   else if(DomainController.upgradeStatus.statusCode == StatusCodes.UpgradeNeeded)
   {
    ClientUpgradeDialog = new iFolderMsgDialog(
    null,
    iFolderMsgDialog.DialogType.Info,
    iFolderMsgDialog.ButtonSet.AcceptDeny,
    Util.GS("iFolder Client Upgrade"),
    Util.GS("Would you like to download new iFolder Client?"),
    string.Format(Util.GS("The client needs to be upgraded to be connected to the server")));
   }
   else
   {
   ClientUpgradeDialog = new iFolderMsgDialog(
    null,
    iFolderMsgDialog.DialogType.Info,
    iFolderMsgDialog.ButtonSet.AcceptDeny,
    Util.GS("iFolder Client Upgrade"),
    Util.GS("Would you like to download new iFolder Client?"),
    string.Format(Util.GS("A newer version \"{0}\" of the iFolder Client is available."), args.NewClientVersion));
   }
   int rc = ClientUpgradeDialog.Run();
   ClientUpgradeDialog.Hide();
   ClientUpgradeDialog.Destroy();
   if (rc == -8)
   {
    bool bUpdateRunning = false;
    Gtk.Window win = new Gtk.Window("");
    string initialPath = (string)System.IO.Path.GetTempPath();
    Debug.PrintLine(String.Format("Initial Path: {0}", initialPath));
    CopyLocation cp = new CopyLocation(win, (string)System.IO.Path.GetTempPath());
    string selectedFolder = "";
                         int rc1 = 0;
                         do
                         {
                                 rc1 = cp.Run();
                                 cp.Hide();
                                 if(rc1 ==(int)ResponseType.Ok)
                                 {
                                         selectedFolder = cp.iFolderPath.Trim();
                                  cp.Destroy();
                                         cp = null;
                                         break;
                                 }
                          }while( rc1 == (int)ResponseType.Ok);
    if( cp != null)
     cp.Destroy();
    win.Hide();
    win.Destroy();
    if( rc1 != (int) ResponseType.Ok)
    {
     Debug.PrintLine("OnClientUpgradeAvailableEvent return");
     ClientUpgradeDialog = null;
     return;
    }
    try
    {
     if(ifws !=null)
     {
      Debug.PrintLine("ifws.RunClientUpdate");
      bUpdateRunning = ifws.RunClientUpdate(args.DomainID, selectedFolder);
     }
    }
    catch(Exception e)
    {
     Debug.PrintLine(String.Format("ifws.RunClientUpdate exception :{0}", e.Message));
     ClientUpgradeDialog = null;
     return;
    }
    if (bUpdateRunning)
    {
    ClientUpgradeDialog = new iFolderMsgDialog(
    null,
    iFolderMsgDialog.DialogType.Info,
    iFolderMsgDialog.ButtonSet.Ok,
    Util.GS("Download Complete..."),
    Util.GS("Download Finished "),
    string.Format(Util.GS("The new client rpm's have been downloaded.")));
    ClientUpgradeDialog.Run();
    ClientUpgradeDialog.Hide();
    ClientUpgradeDialog.Destroy();
    }
    else
    {
     iFolderMsgDialog dialog = new iFolderMsgDialog(
      null,
      iFolderMsgDialog.DialogType.Error,
      iFolderMsgDialog.ButtonSet.None,
      Util.GS("Upgrade Failure"),
      Util.GS("The iFolder client upgrade failed."),
      Util.GS("Please contact your system administrator."));
     dialog.Run();
     dialog.Hide();
     dialog.Destroy();
     dialog = null;
    }
    if( DomainController.upgradeStatus.statusCode == StatusCodes.UpgradeNeeded )
    {
     if( domainController.GetDomain(args.DomainID) != null)
      domainController.RemoveDomain(args.DomainID, false);
    }
   }
   else
   {
    if(DomainController.upgradeStatus.statusCode == StatusCodes.ServerOld || DomainController.upgradeStatus.statusCode == StatusCodes.UpgradeNeeded )
    {
     if( domainController.GetDomain(args.DomainID) != null)
      domainController.RemoveDomain(args.DomainID, false);
    }
   }
   ClientUpgradeDialog = null;
  }
  private void ReLogin(string domainID)
  {
   if (LoginDialog != Util.CurrentModalWindow) return;
   if (LoginDialog == null)
   {
    DomainInformation dom = domainController.GetDomain(domainID);
    if(dom.Authenticated)
    {
     return;
    }
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
     DomainInformation dom = domainController.GetDomain(LoginDialog.Domain);
     if (dom == null)
     {
      iFolderMsgDialog dialog = new iFolderMsgDialog(
       null,
       iFolderMsgDialog.DialogType.Error,
       iFolderMsgDialog.ButtonSet.None,
       Util.GS("Account Error"),
       Util.GS("This account has been removed from your computer."),
       Util.GS("If you wish to connect to this account again, please add it in the Account Settings Dialog."));
      dialog.Run();
      dialog.Hide();
      dialog.Destroy();
      dialog = null;
      LoginDialog.Hide();
      LoginDialog.Destroy();
      LoginDialog = null;
      break;
     }
     try
     {
      string DomainID = LoginDialog.Domain;
      Status status =
       domainController.AuthenticateDomain(
        LoginDialog.Domain,
        LoginDialog.Password,
        LoginDialog.ShouldSavePassword);
      if (status != null)
      {
       switch(status.statusCode)
       {
        case StatusCodes.Success:
        case StatusCodes.SuccessInGrace:
         ifdata.Refresh();
         Debug.PrintLine("Login dialog response- success");
         LoginDialog.Hide();
         LoginDialog.Destroy();
         LoginDialog = null;
      int policy = ifws.GetSecurityPolicy(DomainID);
      if( policy % 2 == 0)
       break;
                                                bool passphraseStatus = simws.IsPassPhraseSet(DomainID);
      if(passphraseStatus == true)
      {
       bool rememberOption = simws.GetRememberOption(DomainID);
       if( rememberOption == false)
       {
        ShowVerifyDialog( DomainID, simws);
       }
       else
       {
        Debug.PrintLine(" remember Option true. Checking for passphrase existence");
        string passphrasecheck;
        passphrasecheck= simws.GetPassPhrase(DomainID);
        if(passphrasecheck == null || passphrasecheck == "")
         ShowVerifyDialog( DomainID, simws);
       }
      }
      else
      {
       iFolderWindow.ShowEnterPassPhraseDialog(DomainID, simws);
      }
         break;
        case StatusCodes.InvalidCertificate:
         if( status.UserName != null)
         {
          dom.Host = status.UserName;
         }
         byte[] byteArray = simws.GetCertificate(dom.Host);
         System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(byteArray);
         iFolderMsgDialog dialog = new iFolderMsgDialog(
          null,
          iFolderMsgDialog.DialogType.Question,
          iFolderMsgDialog.ButtonSet.YesNo,
          "",
          Util.GS("Accept the certificate of this server?"),
          string.Format(Util.GS("iFolder is unable to verify \"{0}\" as a trusted server.  You should examine this server's identity certificate carefully."), dom.Host),
          cert.ToString(true));
         Gdk.Pixbuf certPixbuf = Util.LoadIcon("gnome-mime-application-x-x509-ca-cert", 48);
         if (certPixbuf != null && dialog.Image != null)
          dialog.Image.Pixbuf = certPixbuf;
         int rc = dialog.Run();
         dialog.Hide();
         dialog.Destroy();
         if(rc == -8)
         {
          simws.StoreCertificate(byteArray, dom.Host);
          OnReLoginDialogResponse(o, args);
         }
         else
         {
          domainController.DisableDomainAutoLogin(LoginDialog.Domain);
          LoginDialog.Hide();
          LoginDialog.Destroy();
          LoginDialog = null;
         }
         break;
        case StatusCodes.UserAlreadyMoved:
         OnReLoginDialogResponse(o, args);
         break;
        default:
         Util.ShowLoginError(LoginDialog, status.statusCode);
         break;
       }
      }
     }
     catch(Exception e)
     {
      iFolderMsgDialog dialog = new iFolderMsgDialog(
       null,
       iFolderMsgDialog.DialogType.Error,
       iFolderMsgDialog.ButtonSet.None,
       Util.GS("Account Error"),
       Util.GS("Unable to connect to the iFolder Server"),
       Util.GS("An error was encountered while connecting to the iFolder server.  Please verify the information entered and try again.  If the problem persists, please contact your network administrator."),
       e.Message);
      dialog.Run();
      dialog.Hide();
      dialog.Destroy();
      dialog = null;
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
  private bool ShowVerifyDialog(string DomainID, SimiasWebService simws)
  {
   bool status = false;
   int result;
   Status passPhraseStatus= null;
   VerifyPassPhraseDialog vpd = new VerifyPassPhraseDialog();
   if (!Util.RegisterModalWindow(vpd))
   {
    vpd.Destroy();
    vpd = null;
    return false;
   }
   try
   {
   do
   {
    result = vpd.Run();
    vpd.Hide();
    if( result == (int)ResponseType.Cancel || result == (int)ResponseType.DeleteEvent)
     break;
    if( result == (int)ResponseType.Ok)
    {
     passPhraseStatus = simws.ValidatePassPhrase(DomainID, vpd.PassPhrase);
    }
    if( passPhraseStatus != null)
    {
     if( passPhraseStatus.statusCode == StatusCodes.PassPhraseInvalid)
     {
      Debug.PrintLine("Invalid Passphrase");
      Debug.PrintLine("Invalid Passphrase");
      iFolderMsgDialog dialog = new iFolderMsgDialog(
       null,
       iFolderMsgDialog.DialogType.Error,
       iFolderMsgDialog.ButtonSet.None,
       Util.GS("Invalid PassPhrase"),
       Util.GS("The PassPhrase entered is invalid"),
       Util.GS("Please re-enter the passphrase"));
       dialog.Run();
       dialog.Hide();
       dialog.Destroy();
       dialog = null;
       passPhraseStatus = null;
     }
     else if(passPhraseStatus.statusCode == StatusCodes.Success)
      break;
    }
   }while( result != (int)ResponseType.Cancel && result !=(int)ResponseType.DeleteEvent);
   if(result == (int)ResponseType.Cancel || result ==(int)ResponseType.DeleteEvent)
   {
    status = false;
    simws.StorePassPhrase(DomainID, "", CredentialType.None, false);
   }
   else if( passPhraseStatus != null && passPhraseStatus.statusCode == StatusCodes.Success)
   {
    try
    {
     simws.StorePassPhrase( DomainID, vpd.PassPhrase, CredentialType.Basic, vpd.ShouldSavePassPhrase);
     status = simws.IsPassPhraseSet(DomainID);
    }
    catch(Exception) {}
   }
   }
   catch(Exception)
   {
    return false;
   }
   return status;
  }
  private void OniFolderFileSyncEvent(object o, FileSyncEventArgs args)
  {
   if (args == null || args.CollectionID == null || args.Name == null)
    return;
   try
   {
    if (args.ObjectType == ObjectType.File || args.ObjectType == ObjectType.Directory)
    {
     if (args.Direction == Simias.Client.Event.Direction.Uploading
      && currentIconAnimationDirection != 1)
     {
      gAppIcon.Pixbuf = UploadingPixbuf;
      currentIconAnimationDirection = 1;
     }
     else if (args.Direction == Simias.Client.Event.Direction.Downloading
        && currentIconAnimationDirection != -1)
     {
      gAppIcon.Pixbuf = DownloadingPixbuf;
      currentIconAnimationDirection = -1;
     }
    }
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
     case SyncStatus.UpdateConflict:
     case SyncStatus.FileNameConflict:
      break;
     case SyncStatus.Policy:
      if( (bool)ClientConfig.Get(ClientConfig.KEY_NOTIFY_POLICY_VOILATION) )
       message = Util.GS("A policy prevented complete synchronization.");
      break;
     case SyncStatus.Access:
      message = Util.GS("Insuficient rights prevented complete synchronization.");
      break;
     case SyncStatus.Locked:
      message = Util.GS("The iFolder is locked.");
      break;
     case SyncStatus.PolicyQuota:
      message = Util.GS("The iFolder is full.  Click here to view the Synchronization Log.");
      message = Util.GS("The iFolder is full.\n\nClick <a href=\"ShowSyncLog\">here</a> to view the Synchronization Log.");
      break;
     case SyncStatus.PolicySize:
      if( (bool)ClientConfig.Get(ClientConfig.KEY_NOTIFY_POLICY_VOILATION) )
       message = Util.GS("A size restriction policy prevented complete synchronization.");
      break;
     case SyncStatus.PolicyType:
      if( (bool)ClientConfig.Get(ClientConfig.KEY_NOTIFY_POLICY_VOILATION) )
       message = Util.GS("A file type restriction policy prevented complete synchronization.");
      break;
     case SyncStatus.DiskFull:
      if (args.Direction == Simias.Client.Event.Direction.Uploading)
      {
       message = Util.GS("Insufficient disk space on the server prevented complete synchronization.");
      }
      else
      {
       message = Util.GS("Insufficient disk space on this computer prevented complete synchronization.");
      }
      break;
     case SyncStatus.ReadOnly:
      message = Util.GS("You have Read-only access to this iFolder.  Files that you place in this iFolder will not be synchronized.\n\nClick <a href=\"ShowSyncLog\">here</a> to view the Synchronization Log.");
      break;
     default:
      message = Util.GS("iFolder synchronization failed.");
      break;
    }
    if (message != null)
    {
     Hashtable collectionSyncErrors = null;
     Debug.PrintLine(String.Format("Synchronization errors: {0}, {1}, {2}", args.Name, message, args.CollectionID));
     if (synchronizationErrors.ContainsKey(args.CollectionID))
     {
      collectionSyncErrors = (Hashtable)synchronizationErrors[args.CollectionID];
      Debug.PrintLine(String.Format("collection sync errors exist: {0}", collectionSyncErrors.Count));
     }
     else
     {
      collectionSyncErrors = new Hashtable();
      synchronizationErrors[args.CollectionID] = collectionSyncErrors;
     }
     if (!collectionSyncErrors.ContainsKey(args.Status))
     {
      collectionSyncErrors[args.Status] = message;
      Debug.PrintLine(String.Format("collection sync error count: {0}", collectionSyncErrors.Count));
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
     collectionSynchronizing = args.ID;
     break;
    }
    case Simias.Client.Event.Action.StopSync:
    {
     currentIconAnimationDirection = 0;
      if (collectionSynchronizing != null)
      {
       iFolderHolder ifHolder = ifdata.GetiFolder(collectionSynchronizing);
       if (ifHolder != null)
       {
        if (synchronizationErrors.ContainsKey(ifHolder.iFolder.ID))
        {
         Hashtable collectionSyncErrors = (Hashtable)synchronizationErrors[ifHolder.iFolder.ID];
         ICollection errors = collectionSyncErrors.Keys;
         Debug.PrintLine(String.Format("Number of errors: {0}", errors.Count));
         ArrayList keysToClear = new ArrayList();
         bool showErrorBaloon = false;
         bool showGenericBaloon = true;
         foreach(SyncStatus syncStatusKey in errors)
         {
          string errMsg = (string) collectionSyncErrors[syncStatusKey];
          if (errMsg != null && errMsg.Length > 0)
          {
            showErrorBaloon = true;
            NotifyWindow notifyWin = new NotifyWindow(
            tIcon, string.Format(Util.GS("Incomplete Synchronization: {0}"), ifHolder.iFolder.Name),
            errMsg,
            Gtk.MessageType.Warning, 5000);
            notifyWin.LinkClicked +=
            new LinkClickedEventHandler(OnNotifyWindowLinkClicked);
            notifyWin.ShowAll();
            showGenericBaloon = false;
            keysToClear.Add(syncStatusKey);
          }
         }
         if( showErrorBaloon == true)
         {
          if( showGenericBaloon )
          {
           NotifyWindow notifyWin = new NotifyWindow(
           tIcon, string.Format(Util.GS("Incomplete Synchronization: {0}"), ifHolder.iFolder.Name),
           Util.GS("Synchronization log contains the information regarding the files that are not synchronized"),
           Gtk.MessageType.Warning, 5000);
           notifyWin.LinkClicked +=
           new LinkClickedEventHandler(OnNotifyWindowLinkClicked);
           notifyWin.ShowAll();
          }
         }
         foreach(SyncStatus syncStatusKey in keysToClear)
         {
          collectionSyncErrors.Remove( syncStatusKey);
         }
         Debug.PrintLine(String.Format("After removing keys count: {0}", collectionSyncErrors.Count));
        }
       }
      }
     collectionSynchronizing = null;
     break;
    }
   }
   gAppIcon.Pixbuf = RunningPixbuf;
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
     ((bool)ClientConfig.Get(ClientConfig.KEY_NOTIFY_IFOLDERS)))
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
     if((bool)ClientConfig.Get(ClientConfig.KEY_NOTIFY_COLLISIONS))
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
   if((bool)ClientConfig.Get(ClientConfig.KEY_NOTIFY_USERS))
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
  private void OniFolderAppStateChanged()
  {
   switch(CurrentState)
   {
    case iFolderAppState.Starting:
     gAppIcon.Pixbuf = StartingPixbuf;
     if(!(bool)ClientConfig.Get(ClientConfig.KEY_IFOLDER_WINDOW_HIDE))
      ShowStartupScreen();
     break;
    case iFolderAppState.Running:
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
      domainController.DomainClientUpgradeAvailable +=
       new DomainClientUpgradeAvailableEventHandler(OnClientUpgradeAvailableEvent);
     }
     if (startingUpNotifyWindow != null)
     {
      NotifyWindow notifyWin = startingUpNotifyWindow;
      notifyWin.Hide();
      notifyWin.Destroy();
     }
     gAppIcon.Pixbuf = RunningPixbuf;
     Util.GetiFolderWindow();
                                 Util.GetLogWindow(simiasManager);
     GLib.Timeout.Add(100, new GLib.TimeoutHandler(ShowiFolderWindows));
     break;
    case iFolderAppState.Stopping:
     gAppIcon.Pixbuf = StoppingPixbuf;
     break;
    case iFolderAppState.Stopped:
     System.Threading.Thread th = new System.Threading.Thread (new System.Threading.ThreadStart (GuaranteeShutdown));
     th.IsBackground = true;
     th.Start ();
     try
     {
      simiasManager.Stop();
     }
     catch(Exception e)
     {
      Debug.PrintLine(e.Message);
     }
     Application.Quit();
     break;
   }
  }
  private void ShowStartupScreen()
  {
   timerDelegate = new TimerCallback(splashTimerTick);
   splashTimer = new Timer(timerDelegate,null,3000,System.Threading.Timeout.Infinite);
   startupWind = new Gtk.Window(Gtk.WindowType.Popup);
   Gtk.Image image = new Gtk.Image(new Gdk.Pixbuf(Util.ImagesPath("ifolder-startup-nl.png")));
   startupWind.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));
   startupWind.WindowPosition = Gtk.WindowPosition.Center;
   VBox vbox = new VBox (false, 0);
   startupWind.Add (vbox);
   vbox.PackStart(image, false, false, 0);
   startupWind.ShowAll();
  }
  private void splashTimerTick(object sender)
         {
                 if (this.startupWind != null)
              {
    this.startupWind.Destroy();
                  this.startupWind = null;
              }
              splashTimer.Dispose();
         }
  static private void GuaranteeShutdown()
  {
   System.Threading.Thread.Sleep(10000);
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
    {
     if(! (bool)ClientConfig.Get(ClientConfig.KEY_IFOLDER_WINDOW_HIDE))
      Util.LoadiFolderWindows();
     Debug.PrintLine("Showing the migration prompt");
     if( Util.ShowMigrationPrompt() )
     {
      string str = Mono.Unix.UnixEnvironment.EffectiveUser.HomeDirectory;
      if(System.IO.Directory.Exists(str+"/.novell/ifolder"))
      {
       string[] dirs;
       dirs = System.IO.Directory.GetDirectories(str+"/.novell/ifolder");
       if( dirs.Length > 2)
       {
        iFolderMsgDialog dlg = new iFolderMsgDialog( null, iFolderMsgDialog.DialogType.Info, iFolderMsgDialog.ButtonSet.OkCancel,
            Util.GS("Migration Alert"), Util.GS("There are 2.x iFolders on this machine.") , Util.GS("Do you want to migrate them?") );
        CheckButton dontShowAgain = new CheckButton(Util.GS("Don't show this message again"));
        dlg.ExtraWidget = dontShowAgain;
        int res = dlg.Run();
        dlg.Hide();
        dlg.Destroy();
        if( ((CheckButton)(dlg.ExtraWidget)).Active == true)
        {
         Debug.PrintLine("The check box is checked");
         Util.DontShowMigrationPrompt();
        }
        else
          Debug.PrintLine("The check box is not checked");
        if( res == (int)ResponseType.Ok)
        {
         MigrationWindow migrationWindow = new MigrationWindow( Util.GetiFolderWindow(), ifws, simws);
         migrationWindow.ShowAll();
        }
       }
      }
     }
    }
   }
   else
    Debug.PrintLine("DomainController instance is null");
   return false;
  }
        private void ShowAddAccountWizard()
        {
            bool status = false;
            const string assemblyName = "plugins/Novell.AutoAccount.AutoAccountCreator";
            const string autoAccountClass = "Novell.AutoAccount.AutoAccount";
            const string autoAccountCreateAccountsMethod = "CreateAccounts";
            const string autoAccountPrefMethod = "SetPreferences";
            const string autoAccountFilePath = "AutoAccountFilePath";
            string filePathValue;
            System.Object[] args = new System.Object[1];
            System.Object[] prefArgs = new System.Object[1];
            try
            {
                Assembly idAssembly = Assembly.Load( assemblyName );
                if ( idAssembly != null )
                {
                    Type type = idAssembly.GetType( autoAccountClass );
                    if( null != type )
                    {
                        args[0] = simws;
                        System.Object autoAccount = Activator.CreateInstance(type,args);
                        MethodInfo method = type.GetMethod(autoAccountCreateAccountsMethod);
                        status = (Boolean)method.Invoke(autoAccount, null);
                        iFolderWindow.log.Info("Account Creation Status {0}", status);
                        if( status )
                        {
                            method = type.GetMethod( autoAccountPrefMethod );
                            prefArgs[0] = ifws;
                            method.Invoke(autoAccount, prefArgs);
                            PropertyInfo info = type.GetProperty(autoAccountFilePath);
                            filePathValue = (string)info.GetValue( autoAccount, null );
                            iFolderWindow.log.Debug("File path value is {0}", filePathValue );
                            System.IO.FileInfo fileInfo = new System.IO.FileInfo(filePathValue);
       if( File.Exists(filePathValue+".backup"))
       {
    File.Delete(filePathValue+".backup");
       }
                            fileInfo.MoveTo(filePathValue+".backup");
                        }
                    }
                }
            }
            catch( Exception e )
            {
                iFolderWindow.log.Info("Error: {0}", e.Message);
                iFolderWindow.log.Debug("Exception type {0}\nStackTrace {1}", e.GetType(), e.StackTrace);
            }
            DomainInformation[] domains = null;
            if(null != domainController)
                domains = domainController.GetDomains();
            if(!status)
            {
                if(null != domains && domains.Length < 1)
                {
                    iFolderWindow.log.Debug("Starting account wizard from client...");
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
                else
                {
                    iFolderWindow.log.Debug("Domain count is greater than or equal to 1 now...");
                }
            }
            else
            {
                if(null != domains && domains.Length >= 1)
                 Util.ShowiFolderWindow();
            }
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
     ForceShutdown();
    }
   }
   NotifyWindow notifyWindow = sender as NotifyWindow;
   notifyWindow.Hide();
   notifyWindow.Destroy();
  }
  private void OnStartingUpNotifyWindowHidden(object o, EventArgs args)
  {
   startingUpNotifyWindow = null;
  }
  private void OnShuttingDownNotifyWindowHidden(object o, EventArgs args)
  {
   shuttingDownNotifyWindow = null;
  }
                private void YesNo_Clicked(object o, ResponseArgs args)
                {
                        if (args.ResponseId == Gtk.ResponseType.Yes)
                        {
                              quit_iFolder = true;
    quitDlg.Hide();
                        }
                        else
                        {
                               quit_iFolder = false;
                               quitDlg.Hide();
                        }
                }
  private void trayapp_clicked(object obj, ButtonPressEventArgs args)
  {
   if (CurrentState == iFolderAppState.Starting)
   {
    if (startingUpNotifyWindow == null)
    {
     startingUpNotifyWindow = new NotifyWindow(
      tIcon, Util.GS("iFolder is starting"),
      Util.GS("Please wait for iFolder to start...\n\nPress <a href=\"CancelStartup\">here</a> to cancel."),
      Gtk.MessageType.Info, 0);
     startingUpNotifyWindow.LinkClicked +=
      new LinkClickedEventHandler(OnNotifyWindowLinkClicked);
     startingUpNotifyWindow.Hidden +=
      new EventHandler(OnStartingUpNotifyWindowHidden);
     startingUpNotifyWindow.ShowAll();
    }
    return;
   }
   else if (CurrentState == iFolderAppState.Stopping)
   {
    if (shuttingDownNotifyWindow == null)
    {
     shuttingDownNotifyWindow = new NotifyWindow(
      tIcon, Util.GS("iFolder is shutting down"),
      "",
      Gtk.MessageType.Info, 0);
     shuttingDownNotifyWindow.Hidden +=
      new EventHandler(OnShuttingDownNotifyWindowHidden);
     shuttingDownNotifyWindow.ShowAll();
    }
    return;
   }
   if (Util.CurrentModalWindow != null)
   {
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
     new MenuItem (Util.GS("Account Settings"));
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
   ImageMenuItem prefs_item = new ImageMenuItem (
           Util.GS("Preferences"));
   prefs_item.Image = new Gtk.Image(Gtk.Stock.Preferences,
           Gtk.IconSize.Menu);
   trayMenu.Append(prefs_item);
   prefs_item.Activated +=
     new EventHandler(show_preferences);
   trayMenu.Append(new SeparatorMenuItem());
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
   if((bool)ClientConfig.Get(ClientConfig.KEY_IFOLDER_DEBUG_IFOLDER_DATA))
   {
    trayMenu.Append(new SeparatorMenuItem());
    MenuItem ifolderDataDebug =
      new MenuItem ("Print iFolderData Debug Information");
    trayMenu.Append (ifolderDataDebug);
    ifolderDataDebug.Activated +=
      new EventHandler(PrintiFolderDataDebugState);
   }
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
  public virtual void ShowWindow()
  {
   if (Util.CurrentModalWindow != null)
   {
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
  private void CleanUpPassphrase()
  {
   Debug.PrintLine("In cleanup passphrase");
   try
   {
   DomainInformation[] domains = domainController.GetDomains();
   foreach( DomainInformation domain in domains)
   {
    Debug.PrintLine("Removing Passphrase");
    bool rememberOption = simws.GetRememberOption(domain.ID);
    if( rememberOption == false)
    {
     simws.StorePassPhrase(domain.ID, "", CredentialType.None, false);
    }
   }
   }
   catch(Exception)
   {
   }
  }
  private void quit_ifolder(object o, EventArgs args)
  {
                        quitDlg.Run();
                 if (quit_iFolder)
                 {
    CleanUpPassphrase();
    if (simiasEventBroker != null)
     simiasEventBroker.AbortEventProcessing();
    if (!forceShutdown)
    {
     Util.SaveiFolderWindows();
     Util.CloseiFolderWindows();
    }
    if(CurrentState == iFolderAppState.Stopping)
    {
     try
     {
       simiasManager.Stop();
     }
     catch(Exception e)
     {
      Debug.PrintLine(e.Message);
     }
     System.Environment.Exit(1);
    }
    else
    {
     System.Threading.Thread stopThread =
     new System.Threading.Thread(new ThreadStart(StopiFolder));
     stopThread.Start();
    }
   }
  }
  private void ForceShutdown()
  {
   forceShutdown = true;
  }
  private void show_help(object o, EventArgs args)
  {
   Util.ShowHelp(Util.HelpMainPage, null);
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
        public static void LogInit(string path)
        {
   iFolderLogManager.LogConfDirPath = path;
            if (!Directory.Exists(iFolderLogManager.LogConfDirPath))
            {
                Directory.CreateDirectory(iFolderLogManager.LogConfDirPath);
            }
   if(!File.Exists(iFolderLogManager.LogConfFilePath))
             File.Copy(Path.Combine(SimiasSetup.sysconfdir, iFolderLogManager.LogConfFileName), iFolderLogManager.LogConfFilePath);
            iFolderLogManager.Configure(iFolderLogManager.LogConfDirPath);
            iFolderWindow.log = iFolderLogManager.GetLogger(typeof(System.Object));
        }
  public static void Main (string[] args)
  {
   try
   {
    System.Diagnostics.Process[] processes =
     System.Diagnostics.Process.GetProcessesByName("iFolderClient");
    if(processes.Length > 1)
    {
     Debug.PrintLine("iFolder is already running!");
     return;
    }
   }
   catch (Exception)
   {
    Console.WriteLine("\n GetProcessesByName failed to fetch the list of processes");
   }
   try
   {
    application = new iFolderApplication(args);
    application.Run();
   }
   catch(Exception bigException)
   {
    Debug.PrintLine(bigException.Message);
    iFolderCrashDialog cd = new iFolderCrashDialog(bigException);
    if (!Util.RegisterModalWindow(cd))
    {
     try
     {
      Util.CurrentModalWindow.Destroy();
      Util.RegisterModalWindow(cd);
     }
     catch{}
    }
    cd.Run();
    cd.Destroy();
    cd = null;
     application.SimiasManager.Stop();
    Application.Quit();
   }
  }
 }
}
