using System;
using System.Drawing;
using System.Windows.Forms;
using System.Threading;
using System.Text;
using System.Net;
using System.Diagnostics;
using System.IO;
using System.Globalization;
using System.Collections;
using Novell.iFolderCom;
using Novell.Win32Util;
using Novell.CustomUIControls;
using Simias.Client;
using Simias.Client.Authentication;
using Simias.Client.Event;
using Microsoft.Win32;
namespace Novell.FormsTrayApp
{
 public enum NotifyType
 {
  Subscription,
  NewMember,
  Collision,
  SyncError,
  CreateAccount,
 };
 public class FormsTrayApp : Form
 {
  private delegate void SyncCollectionDelegate(CollectionSyncEventArgs syncEventArgs);
  private SyncCollectionDelegate syncCollectionDelegate;
  private delegate void SyncFileDelegate(FileSyncEventArgs syncEventArgs);
  private SyncFileDelegate syncFileDelegate;
  private delegate void CreateChangeEventDelegate(iFolderWeb ifolder, iFolderUser ifolderUser, string eventData);
  private CreateChangeEventDelegate createChangeEventDelegate;
  private delegate void NotifyMessageDelegate(NotifyEventArgs notifyEventArgs);
  private NotifyMessageDelegate notifyMessageDelegate;
  private System.ComponentModel.IContainer components;
  static private System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(FormsTrayApp));
  private bool shutdown = false;
  private bool simiasRunning = false;
  private Icon trayIcon;
  private Icon startupIcon;
  private Icon shutdownIcon;
        private const int numberOfSyncIcons = 10;
  private Icon[] syncIcons = new Icon[numberOfSyncIcons];
  private int index = 0;
  private bool syncToServer = false;
  private Queue eventQueue;
  private Thread worker = null;
  private Hashtable initialSyncCollections = new Hashtable();
  private Hashtable collectionsNotified = new Hashtable();
  private string currentSyncCollectionName;
  protected AutoResetEvent workEvent = null;
  private ServerInfo serverInfo = null;
  private ShellNotifyIcon shellNotifyIcon;
  private iFolderWeb ifolderFromNotify;
  private NotifyType notifyType;
  private System.Windows.Forms.MenuItem menuItem10;
  private System.Windows.Forms.MenuItem menuSeparator1;
  private System.Windows.Forms.MenuItem menuProperties;
  private System.Windows.Forms.MenuItem menuHelp;
  private System.Windows.Forms.MenuItem menuExit;
  private System.Windows.Forms.ContextMenu contextMenu1;
  private System.Windows.Forms.MenuItem menuEventLogReader;
  private iFolderWebService ifWebService = null;
  private SimiasWebService simiasWebService;
  private IProcEventClient eventClient;
  private GlobalProperties globalProperties;
  private Preferences preferences;
  private SyncLog syncLog;
  private bool eventError = false;
  private System.Windows.Forms.MenuItem menuStoreBrowser;
  private System.Windows.Forms.MenuItem menuTools;
  private System.Windows.Forms.Timer syncAnimateTimer;
  private System.Windows.Forms.MenuItem menuSyncLog;
  private System.Windows.Forms.MenuItem menuPreferences;
  private System.Windows.Forms.MenuItem menuItem1;
  private System.Windows.Forms.MenuItem menuAbout;
  private System.Windows.Forms.MenuItem menuAccounts;
  private Manager simiasManager;
  private string iFolderLogPath;
  static private FormsTrayApp instance;
  [STAThread]
  static void Main(string[] args)
  {
   if (args.Length == 1 && args[0].Equals("-checkautorun"))
   {
    if (!Preferences.IsRunEnabled())
    {
     return;
    }
   }
   instance = new FormsTrayApp();
   Application.Run(instance);
  }
  public FormsTrayApp()
  {
   syncCollectionDelegate = new SyncCollectionDelegate(syncCollection);
   syncFileDelegate = new SyncFileDelegate(syncFile);
   createChangeEventDelegate = new CreateChangeEventDelegate(createChangeEvent);
   notifyMessageDelegate = new NotifyMessageDelegate(notifyMessage);
   string windowName = "iFolder Services " + Environment.UserName;
   Novell.Win32Util.Win32Window window = Novell.Win32Util.Win32Window.FindWindow(null, windowName);
   if (window != null)
   {
    window = Novell.Win32Util.Win32Window.FindWindow(null, resourceManager.GetString("myiFolders"));
    if (window != null)
    {
     window.Visible = true;
    }
    shutdown = true;
   }
   else
   {
    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(@"SOFTWARE\Novell\iFolder");
    string language = regKey.GetValue("language") as String;
    if (language != null)
    {
     try
     {
      Thread.CurrentThread.CurrentUICulture = new CultureInfo(language);
     }
     catch {}
    }
    iFolderLogPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "iFolder");
    if (!Directory.Exists(iFolderLogPath))
    {
     Directory.CreateDirectory(iFolderLogPath);
    }
    this.simiasManager = new Manager( Environment.GetCommandLineArgs() );
    InitializeComponent();
    this.Text = "iFolder Services " + Environment.UserName;
    this.components = new System.ComponentModel.Container();
    this.ClientSize = new System.Drawing.Size(292, 266);
    try
    {
     string basePath = Path.Combine(Application.StartupPath, "res");
     trayIcon = new Icon(Path.Combine(basePath, "ifolder_loaded.ico"));
     startupIcon = new Icon(Path.Combine(basePath, "ifolder-startup.ico"));
     shutdownIcon = new Icon(Path.Combine(basePath, "ifolder-shutdown.ico"));
     syncIcons[0] = new Icon(trayIcon, trayIcon.Size);
     for (int i = 0; i < numberOfSyncIcons; i++)
     {
      string syncIcon = string.Format(Path.Combine(basePath, "ifolder_sync{0}.ico"), i+1);
      syncIcons[i] = new Icon(syncIcon);
     }
     this.ShowInTaskbar = false;
     this.WindowState = FormWindowState.Minimized;
     Win32Window win32Window = new Win32Window();
     win32Window.Handle = this.Handle;
     win32Window.MakeToolWindow();
     shellNotifyIcon = new ShellNotifyIcon(this.Handle);
     shellNotifyIcon.Text = resourceManager.GetString("iFolderServicesStarting");
     shellNotifyIcon.Icon = startupIcon;
     shellNotifyIcon.ContextMenu = contextMenu1;
     shellNotifyIcon.Click += new Novell.CustomUIControls.ShellNotifyIcon.ClickDelegate(shellNotifyIcon_Click);
     shellNotifyIcon.BalloonClick += new Novell.CustomUIControls.ShellNotifyIcon.BalloonClickDelegate(shellNotifyIcon_BalloonClick);
     shellNotifyIcon.ContextMenuPopup += new Novell.CustomUIControls.ShellNotifyIcon.ContextMenuPopupDelegate(shellNotifyIcon_ContextMenuPopup);
    }
    catch
    {
    }
   }
   this.Closing += new System.ComponentModel.CancelEventHandler(this.FormsTrayApp_Closing);
   this.Load += new System.EventHandler(FormsTrayApp_Load);
  }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
    if (components != null)
     components.Dispose();
   base.Dispose( disposing );
  }
  static public bool CheckForClientUpdate(string domainID)
  {
   bool updateStarted = false;
   string version = instance.ifWebService.CheckForUpdatedClient(domainID);
   if ( version != null )
   {
    MyMessageBox mmb = new MyMessageBox(string.Format(resourceManager.GetString("clientUpgradePrompt"), version), resourceManager.GetString("clientUpgradeTitle"), string.Empty, MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question);
    DialogResult result = mmb.ShowDialog();
    if ( result == DialogResult.Yes )
    {
     updateStarted = instance.ifWebService.RunClientUpdate(domainID);
     if ( updateStarted == false )
     {
      mmb = new MyMessageBox(resourceManager.GetString("clientUpgradeFailure"), resourceManager.GetString("upgradeErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
      mmb.ShowDialog();
     }
    }
   }
   return updateStarted;
  }
  private void menuStoreBrowser_Click(object sender, System.EventArgs e)
  {
   Process.Start(Path.Combine(Application.StartupPath, "StoreBrowser.exe"));
  }
  private void menuEventLogReader_Click(object sender, System.EventArgs e)
  {
   Process.Start(Path.Combine(Application.StartupPath, "EventLogReader.exe"));
  }
  private void menuAccounts_Click(object sender, System.EventArgs e)
  {
   if (preferences.Visible)
   {
    preferences.Activate();
   }
   else
   {
    preferences.Show();
   }
   preferences.SelectAccounts(false);
  }
  private void menuProperties_Click(object sender, System.EventArgs e)
  {
   if (globalProperties.Visible)
   {
    globalProperties.Activate();
   }
   else
   {
    globalProperties.Show();
   }
  }
  private void menuPreferences_Click(object sender, System.EventArgs e)
  {
   if (preferences.Visible)
   {
    preferences.Activate();
   }
   else
   {
    preferences.Show();
   }
   preferences.SelectGeneral();
  }
  private void menuSyncLog_Click(object sender, System.EventArgs e)
  {
   if (syncLog.Visible)
   {
    syncLog.Activate();
   }
   else
   {
    syncLog.Show();
   }
  }
  private void menuHelp_Click(object sender, System.EventArgs e)
  {
   new iFolderComponent().ShowHelp(Application.StartupPath, string.Empty);
  }
  private void menuAbout_Click(object sender, System.EventArgs e)
  {
   About about = new About();
   about.CreateControl();
   about.StartPosition = FormStartPosition.CenterScreen;
   about.ShowDialog();
  }
  private void menuExit_Click(object sender, System.EventArgs e)
  {
   menuExit.Enabled = false;
   ShutdownTrayApp(null);
  }
  private void shellNotifyIcon_Click(object sender, EventArgs e)
  {
   if (simiasRunning)
   {
    menuProperties_Click(sender, e);
   }
  }
  private void shellNotifyIcon_BalloonClick(object sender, EventArgs e)
  {
   switch (notifyType)
   {
    case NotifyType.Collision:
     ConflictResolver conflictResolver = new ConflictResolver();
     conflictResolver.StartPosition = FormStartPosition.CenterScreen;
     conflictResolver.iFolder = ifolderFromNotify;
     conflictResolver.iFolderWebService = ifWebService;
     conflictResolver.LoadPath = Application.StartupPath;
     conflictResolver.CreateControl();
     ShellNotifyIcon.SetForegroundWindow(conflictResolver.Handle);
     conflictResolver.Show();
     break;
    case NotifyType.NewMember:
     iFolderAdvanced ifolderAdvanced = new iFolderAdvanced();
     ifolderAdvanced.StartPosition = FormStartPosition.CenterScreen;
     ifolderAdvanced.CurrentiFolder = ifolderFromNotify;
     ifolderAdvanced.LoadPath = Application.StartupPath;
     ifolderAdvanced.ActiveTab = 1;
     ifolderAdvanced.EventClient = eventClient;
     ifolderAdvanced.CreateControl();
     ShellNotifyIcon.SetForegroundWindow(ifolderAdvanced.Handle);
     ifolderAdvanced.ShowDialog();
     ifolderAdvanced.Dispose();
     break;
    case NotifyType.Subscription:
     AcceptInvitation acceptInvitation = new AcceptInvitation(ifWebService, ifolderFromNotify);
     acceptInvitation.StartPosition = FormStartPosition.CenterScreen;
     acceptInvitation.Visible = false;
     acceptInvitation.CreateControl();
     ShellNotifyIcon.SetForegroundWindow(acceptInvitation.Handle);
     acceptInvitation.ShowDialog();
     acceptInvitation.Dispose();
     break;
    case NotifyType.SyncError:
     syncLog.Show();
     break;
    case NotifyType.CreateAccount:
     if (preferences.Visible)
     {
      preferences.Activate();
     }
     else
     {
      preferences.Show();
     }
     preferences.SelectAccounts(true);
     break;
   }
  }
  private void shellNotifyIcon_ContextMenuPopup(object sender, EventArgs e)
  {
   foreach (MenuItem item in this.contextMenu1.MenuItems)
   {
    item.Visible = simiasRunning;
   }
   if (simiasRunning)
   {
    menuStoreBrowser.Visible = File.Exists(Path.Combine(Application.StartupPath, "StoreBrowser.exe"));
    menuEventLogReader.Visible = File.Exists(Path.Combine(Application.StartupPath, "EventLogReader.exe"));
    menuSeparator1.Visible = menuTools.Visible = menuStoreBrowser.Visible | menuEventLogReader.Visible;
   }
  }
  private void FormsTrayApp_Closing(object sender, System.ComponentModel.CancelEventArgs e)
  {
   ShutdownTrayApp(null);
  }
  private void FormsTrayApp_Load(object sender, System.EventArgs e)
  {
   if (shutdown)
   {
    this.Close();
   }
   else
   {
    try
    {
     Environment.CurrentDirectory = Application.StartupPath;
     simiasManager.Start();
     SetWebServiceInformation(simiasManager.WebServiceUri, simiasManager.DataPath);
     ifWebService = new iFolderWebService();
     ifWebService.Url = simiasManager.WebServiceUri + "/iFolder.asmx";
     simiasWebService = new SimiasWebService();
     simiasWebService.Url = simiasManager.WebServiceUri + "/Simias.asmx";
     eventQueue = new Queue();
     workEvent = new AutoResetEvent(false);
     eventClient = new IProcEventClient(new IProcEventError(errorHandler), null);
     eventClient.Register();
     if (!eventError)
     {
      eventClient.SetEvent(IProcEventAction.AddNodeChanged, new IProcEventHandler(trayApp_nodeEventHandler));
      eventClient.SetEvent(IProcEventAction.AddNodeCreated, new IProcEventHandler(trayApp_nodeEventHandler));
      eventClient.SetEvent(IProcEventAction.AddNodeDeleted, new IProcEventHandler(trayApp_nodeEventHandler));
      eventClient.SetEvent(IProcEventAction.AddCollectionSync, new IProcEventHandler(trayApp_collectionSyncHandler));
      eventClient.SetEvent(IProcEventAction.AddFileSync, new IProcEventHandler(trayApp_fileSyncHandler));
      eventClient.SetEvent(IProcEventAction.AddNotifyMessage, new IProcEventHandler(trayApp_notifyMessageHandler));
     }
     preferences = new Preferences(ifWebService, simiasWebService, simiasManager);
     preferences.EnterpriseConnect += new Novell.FormsTrayApp.Preferences.EnterpriseConnectDelegate(preferences_EnterpriseConnect);
     preferences.ChangeDefaultDomain += new Novell.FormsTrayApp.Preferences.ChangeDefaultDomainDelegate(preferences_EnterpriseConnect);
     preferences.RemoveDomain += new Novell.FormsTrayApp.Preferences.RemoveDomainDelegate(preferences_RemoveDomain);
     preferences.ShutdownTrayApp += new Novell.FormsTrayApp.Preferences.ShutdownTrayAppDelegate(preferences_ShutdownTrayApp);
     preferences.UpdateDomain += new Novell.FormsTrayApp.Preferences.UpdateDomainDelegate(preferences_UpdateDomain);
     preferences.CreateControl();
     IntPtr handle = preferences.Handle;
     syncLog = new SyncLog();
     syncLog.CreateControl();
     handle = syncLog.Handle;
     LocalService.Start(simiasWebService, simiasManager.WebServiceUri, simiasManager.DataPath);
     LocalService.Start(ifWebService, simiasManager.WebServiceUri, simiasManager.DataPath);
     simiasRunning = true;
     globalProperties = new GlobalProperties(ifWebService, simiasWebService, eventClient);
     globalProperties.RemoveDomain += new Novell.FormsTrayApp.GlobalProperties.RemoveDomainDelegate(globalProperties_RemoveDomain);
     globalProperties.PreferenceDialog = preferences;
     globalProperties.SyncLogDialog = syncLog;
     globalProperties.CreateControl();
     handle = globalProperties.Handle;
     bool accountPrompt = false;
     try
     {
      globalProperties.InitializeServerList();
      DomainInformation[] domains;
      domains = this.simiasWebService.GetDomains(false);
      foreach(DomainInformation dw in domains)
      {
       try
       {
        if (dw.IsSlave)
        {
         preferences.AddDomainToList(dw);
        }
        globalProperties.AddDomainToList(dw);
        preferences.SetProxyForDomain( dw.HostUrl, false );
       }
       catch {}
      }
      if (domains.Length.Equals(0))
      {
       accountPrompt = true;
      }
     }
     catch {}
     if (worker == null)
     {
      worker = new Thread(new ThreadStart(eventThreadProc));
      worker.Start();
     }
     shellNotifyIcon.Text = resourceManager.GetString("iFolderServices");
     shellNotifyIcon.Icon = trayIcon;
     updateOverlayIcons();
     if (accountPrompt)
     {
      notifyType = NotifyType.CreateAccount;
      shellNotifyIcon.DisplayBalloonTooltip(resourceManager.GetString("createAccountTitle"), resourceManager.GetString("createAccount"), BalloonType.Info);
     }
    }
    catch (Exception ex)
    {
     ShutdownTrayApp(ex);
    }
   }
  }
  private void errorHandler(ApplicationException e, object context)
  {
   try
   {
    string errorFile = Path.Combine(iFolderLogPath, "iFolderApp.log");
    using (StreamWriter sw = new StreamWriter(errorFile, true))
    {
     sw.WriteLine("-------------------");
     sw.WriteLine(DateTime.Now);
     sw.WriteLine(e.Message);
    }
   }
   catch {}
   eventError = true;
  }
  private void trayApp_nodeEventHandler(SimiasEventArgs args)
  {
   try
   {
    NodeEventArgs eventArgs = args as NodeEventArgs;
    lock (eventQueue.SyncRoot)
    {
     eventQueue.Enqueue(eventArgs);
     workEvent.Set();
    }
   }
   catch {}
  }
  private void trayApp_collectionSyncHandler(SimiasEventArgs args)
  {
   try
   {
    CollectionSyncEventArgs syncEventArgs = args as CollectionSyncEventArgs;
    BeginInvoke(syncCollectionDelegate, new object[] {syncEventArgs});
   }
   catch {}
  }
  private void trayApp_fileSyncHandler(SimiasEventArgs args)
  {
   try
   {
    FileSyncEventArgs syncEventArgs = args as FileSyncEventArgs;
    BeginInvoke(syncFileDelegate, new object[] {syncEventArgs});
   }
   catch {}
  }
  private void trayApp_notifyMessageHandler(SimiasEventArgs args)
  {
   try
   {
    NotifyEventArgs notifyEventArgs = args as NotifyEventArgs;
    BeginInvoke(notifyMessageDelegate, new object[] {notifyEventArgs});
   }
   catch {}
  }
  private void serverInfo_EnterpriseConnect(object sender, DomainConnectEventArgs e)
  {
   globalProperties.AddDomainToList(e.DomainInfo);
  }
  private void globalProperties_RemoveDomain(object sender, DomainRemoveEventArgs e)
  {
   preferences.RemoveDomainFromList(e.DomainInfo, e.DefaultDomainID);
  }
  private void preferences_EnterpriseConnect(object sender, DomainConnectEventArgs e)
  {
   globalProperties.AddDomainToList(e.DomainInfo);
  }
  private void preferences_RemoveDomain(object sender, DomainRemoveEventArgs e)
  {
   globalProperties.RemoveDomainFromList(e.DomainInfo, e.DefaultDomainID);
  }
  private void preferences_ShutdownTrayApp(object sender, EventArgs e)
  {
   ShutdownTrayApp(null);
  }
  private void preferences_UpdateDomain(object sender, DomainConnectEventArgs e)
  {
   globalProperties.UpdateDomain(e.DomainInfo);
  }
  private void syncAnimateTimer_Tick(object sender, System.EventArgs e)
  {
   shellNotifyIcon.Icon = syncIcons[index];
   if (syncToServer)
   {
    if (--index < 0)
    {
     index = numberOfSyncIcons - 1;
    }
   }
   else
   {
    if (++index > (numberOfSyncIcons - 1))
    {
     index = 0;
    }
   }
  }
  private void serverInfo_Closed(object sender, EventArgs e)
  {
   if (serverInfo != null)
   {
    if (serverInfo.Cancelled)
    {
     simiasWebService.DisableDomainAutoLogin(serverInfo.DomainInfo.ID);
    }
    else
    {
     preferences.UpdateDomainStatus(new Domain(serverInfo.DomainInfo));
    }
    bool update = serverInfo.UpdateStarted;
    serverInfo.Dispose();
    serverInfo = null;
    if (update)
    {
     ShutdownTrayApp(null);
    }
   }
  }
  private void SetWebServiceInformation(Uri webServiceUri, string simiasDataPath)
  {
   RegistryKey regKey = Registry.CurrentUser.CreateSubKey(@"SOFTWARE\Novell\iFolder");
   if (regKey != null)
   {
    regKey.SetValue("WebServiceUri", webServiceUri.ToString());
    regKey.SetValue("SimiasDataPath", simiasDataPath);
    regKey.Close();
   }
   else
   {
    throw new ApplicationException("Cannot open iFolder registry key to set web service information.");
   }
  }
  private bool authenticate(DomainInformation domainInfo, out string credentials)
  {
   bool result = false;
   credentials = null;
   try
   {
    DomainAuthentication domainAuth =
     new DomainAuthentication(
     "iFolder",
     domainInfo.ID,
     null);
    MyMessageBox mmb;
    Status status = domainAuth.Authenticate(simiasManager.WebServiceUri, simiasManager.DataPath);
    switch (status.statusCode)
    {
     case StatusCodes.InvalidCertificate:
      byte[] byteArray = simiasWebService.GetCertificate(domainInfo.Host);
      System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(byteArray);
      mmb = new MyMessageBox(string.Format(resourceManager.GetString("verifyCert"), domainInfo.Host), resourceManager.GetString("verifyCertTitle"), cert.ToString(true), MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question, MyMessageBoxDefaultButton.Button2);
      if (mmb.ShowDialog() == DialogResult.Yes)
      {
       simiasWebService.StoreCertificate(byteArray, domainInfo.Host);
       result = authenticate(domainInfo, out credentials);
      }
      break;
     case StatusCodes.Success:
      result = true;
      break;
     case StatusCodes.SuccessInGrace:
      mmb = new MyMessageBox(
       string.Format(resourceManager.GetString("graceLogin"), status.RemainingGraceLogins),
       resourceManager.GetString("graceLoginTitle"),
       string.Empty,
       MyMessageBoxButtons.OK,
       MyMessageBoxIcon.Information);
      mmb.StartPosition = FormStartPosition.CenterScreen;
      mmb.ShowDialog();
      result = true;
      break;
     default:
     {
      string userID;
      CredentialType credType = simiasWebService.GetDomainCredentials(domainInfo.ID, out userID, out credentials);
      if ((credType == CredentialType.Basic) && (credentials != null))
      {
       domainAuth = new DomainAuthentication("iFolder", domainInfo.ID, credentials);
       Status authStatus = domainAuth.Authenticate(simiasManager.WebServiceUri, simiasManager.DataPath);
       switch (authStatus.statusCode)
       {
        case StatusCodes.Success:
         result = true;
         break;
        case StatusCodes.SuccessInGrace:
         mmb = new MyMessageBox(
          string.Format(resourceManager.GetString("graceLogin"), status.RemainingGraceLogins),
          resourceManager.GetString("graceLoginTitle"),
          string.Empty,
          MyMessageBoxButtons.OK,
          MyMessageBoxIcon.Information);
         mmb.StartPosition = FormStartPosition.CenterScreen;
         mmb.ShowDialog();
         result = true;
         break;
        case StatusCodes.AccountDisabled:
         mmb = new MyMessageBox(resourceManager.GetString("accountDisabled"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
         mmb.StartPosition = FormStartPosition.CenterScreen;
         mmb.ShowDialog();
         break;
        case StatusCodes.AccountLockout:
         mmb = new MyMessageBox(resourceManager.GetString("accountLockout"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
         mmb.StartPosition = FormStartPosition.CenterScreen;
         mmb.ShowDialog();
         break;
        case StatusCodes.SimiasLoginDisabled:
         mmb = new MyMessageBox(resourceManager.GetString("iFolderAccountDisabled"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
         mmb.StartPosition = FormStartPosition.CenterScreen;
         mmb.ShowDialog();
         break;
        case StatusCodes.UnknownUser:
        case StatusCodes.InvalidPassword:
        case StatusCodes.InvalidCredentials:
         simiasWebService.SetDomainCredentials(domainInfo.ID, null, CredentialType.None);
         mmb = new MyMessageBox(resourceManager.GetString("failedAuth"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
         mmb.StartPosition = FormStartPosition.CenterScreen;
         mmb.ShowDialog();
         break;
       }
      }
      break;
     }
    }
   }
   catch {}
   return result;
  }
  private void InitializeComponent()
  {
   this.components = new System.ComponentModel.Container();
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(FormsTrayApp));
   this.contextMenu1 = new System.Windows.Forms.ContextMenu();
   this.menuTools = new System.Windows.Forms.MenuItem();
   this.menuStoreBrowser = new System.Windows.Forms.MenuItem();
   this.menuEventLogReader = new System.Windows.Forms.MenuItem();
   this.menuSeparator1 = new System.Windows.Forms.MenuItem();
   this.menuProperties = new System.Windows.Forms.MenuItem();
   this.menuAccounts = new System.Windows.Forms.MenuItem();
   this.menuSyncLog = new System.Windows.Forms.MenuItem();
   this.menuItem1 = new System.Windows.Forms.MenuItem();
   this.menuPreferences = new System.Windows.Forms.MenuItem();
   this.menuHelp = new System.Windows.Forms.MenuItem();
   this.menuAbout = new System.Windows.Forms.MenuItem();
   this.menuItem10 = new System.Windows.Forms.MenuItem();
   this.menuExit = new System.Windows.Forms.MenuItem();
   this.syncAnimateTimer = new System.Windows.Forms.Timer(this.components);
   this.contextMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                       this.menuTools,
                       this.menuSeparator1,
                       this.menuProperties,
                       this.menuAccounts,
                       this.menuSyncLog,
                       this.menuItem1,
                       this.menuPreferences,
                       this.menuHelp,
                       this.menuAbout,
                       this.menuItem10,
                       this.menuExit});
   this.contextMenu1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("contextMenu1.RightToLeft")));
   this.menuTools.Enabled = ((bool)(resources.GetObject("menuTools.Enabled")));
   this.menuTools.Index = 0;
   this.menuTools.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                       this.menuStoreBrowser,
                       this.menuEventLogReader});
   this.menuTools.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuTools.Shortcut")));
   this.menuTools.ShowShortcut = ((bool)(resources.GetObject("menuTools.ShowShortcut")));
   this.menuTools.Text = resources.GetString("menuTools.Text");
   this.menuTools.Visible = ((bool)(resources.GetObject("menuTools.Visible")));
   this.menuStoreBrowser.Enabled = ((bool)(resources.GetObject("menuStoreBrowser.Enabled")));
   this.menuStoreBrowser.Index = 0;
   this.menuStoreBrowser.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuStoreBrowser.Shortcut")));
   this.menuStoreBrowser.ShowShortcut = ((bool)(resources.GetObject("menuStoreBrowser.ShowShortcut")));
   this.menuStoreBrowser.Text = resources.GetString("menuStoreBrowser.Text");
   this.menuStoreBrowser.Visible = ((bool)(resources.GetObject("menuStoreBrowser.Visible")));
   this.menuStoreBrowser.Click += new System.EventHandler(this.menuStoreBrowser_Click);
   this.menuEventLogReader.Enabled = ((bool)(resources.GetObject("menuEventLogReader.Enabled")));
   this.menuEventLogReader.Index = 1;
   this.menuEventLogReader.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuEventLogReader.Shortcut")));
   this.menuEventLogReader.ShowShortcut = ((bool)(resources.GetObject("menuEventLogReader.ShowShortcut")));
   this.menuEventLogReader.Text = resources.GetString("menuEventLogReader.Text");
   this.menuEventLogReader.Visible = ((bool)(resources.GetObject("menuEventLogReader.Visible")));
   this.menuEventLogReader.Click += new System.EventHandler(this.menuEventLogReader_Click);
   this.menuSeparator1.Enabled = ((bool)(resources.GetObject("menuSeparator1.Enabled")));
   this.menuSeparator1.Index = 1;
   this.menuSeparator1.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuSeparator1.Shortcut")));
   this.menuSeparator1.ShowShortcut = ((bool)(resources.GetObject("menuSeparator1.ShowShortcut")));
   this.menuSeparator1.Text = resources.GetString("menuSeparator1.Text");
   this.menuSeparator1.Visible = ((bool)(resources.GetObject("menuSeparator1.Visible")));
   this.menuProperties.DefaultItem = true;
   this.menuProperties.Enabled = ((bool)(resources.GetObject("menuProperties.Enabled")));
   this.menuProperties.Index = 2;
   this.menuProperties.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuProperties.Shortcut")));
   this.menuProperties.ShowShortcut = ((bool)(resources.GetObject("menuProperties.ShowShortcut")));
   this.menuProperties.Text = resources.GetString("menuProperties.Text");
   this.menuProperties.Visible = ((bool)(resources.GetObject("menuProperties.Visible")));
   this.menuProperties.Click += new System.EventHandler(this.menuProperties_Click);
   this.menuAccounts.Enabled = ((bool)(resources.GetObject("menuAccounts.Enabled")));
   this.menuAccounts.Index = 3;
   this.menuAccounts.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuAccounts.Shortcut")));
   this.menuAccounts.ShowShortcut = ((bool)(resources.GetObject("menuAccounts.ShowShortcut")));
   this.menuAccounts.Text = resources.GetString("menuAccounts.Text");
   this.menuAccounts.Visible = ((bool)(resources.GetObject("menuAccounts.Visible")));
   this.menuAccounts.Click += new System.EventHandler(this.menuAccounts_Click);
   this.menuSyncLog.Enabled = ((bool)(resources.GetObject("menuSyncLog.Enabled")));
   this.menuSyncLog.Index = 4;
   this.menuSyncLog.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuSyncLog.Shortcut")));
   this.menuSyncLog.ShowShortcut = ((bool)(resources.GetObject("menuSyncLog.ShowShortcut")));
   this.menuSyncLog.Text = resources.GetString("menuSyncLog.Text");
   this.menuSyncLog.Visible = ((bool)(resources.GetObject("menuSyncLog.Visible")));
   this.menuSyncLog.Click += new System.EventHandler(this.menuSyncLog_Click);
   this.menuItem1.Enabled = ((bool)(resources.GetObject("menuItem1.Enabled")));
   this.menuItem1.Index = 5;
   this.menuItem1.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuItem1.Shortcut")));
   this.menuItem1.ShowShortcut = ((bool)(resources.GetObject("menuItem1.ShowShortcut")));
   this.menuItem1.Text = resources.GetString("menuItem1.Text");
   this.menuItem1.Visible = ((bool)(resources.GetObject("menuItem1.Visible")));
   this.menuPreferences.Enabled = ((bool)(resources.GetObject("menuPreferences.Enabled")));
   this.menuPreferences.Index = 6;
   this.menuPreferences.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuPreferences.Shortcut")));
   this.menuPreferences.ShowShortcut = ((bool)(resources.GetObject("menuPreferences.ShowShortcut")));
   this.menuPreferences.Text = resources.GetString("menuPreferences.Text");
   this.menuPreferences.Visible = ((bool)(resources.GetObject("menuPreferences.Visible")));
   this.menuPreferences.Click += new System.EventHandler(this.menuPreferences_Click);
   this.menuHelp.Enabled = ((bool)(resources.GetObject("menuHelp.Enabled")));
   this.menuHelp.Index = 7;
   this.menuHelp.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuHelp.Shortcut")));
   this.menuHelp.ShowShortcut = ((bool)(resources.GetObject("menuHelp.ShowShortcut")));
   this.menuHelp.Text = resources.GetString("menuHelp.Text");
   this.menuHelp.Visible = ((bool)(resources.GetObject("menuHelp.Visible")));
   this.menuHelp.Click += new System.EventHandler(this.menuHelp_Click);
   this.menuAbout.Enabled = ((bool)(resources.GetObject("menuAbout.Enabled")));
   this.menuAbout.Index = 8;
   this.menuAbout.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuAbout.Shortcut")));
   this.menuAbout.ShowShortcut = ((bool)(resources.GetObject("menuAbout.ShowShortcut")));
   this.menuAbout.Text = resources.GetString("menuAbout.Text");
   this.menuAbout.Visible = ((bool)(resources.GetObject("menuAbout.Visible")));
   this.menuAbout.Click += new System.EventHandler(this.menuAbout_Click);
   this.menuItem10.Enabled = ((bool)(resources.GetObject("menuItem10.Enabled")));
   this.menuItem10.Index = 9;
   this.menuItem10.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuItem10.Shortcut")));
   this.menuItem10.ShowShortcut = ((bool)(resources.GetObject("menuItem10.ShowShortcut")));
   this.menuItem10.Text = resources.GetString("menuItem10.Text");
   this.menuItem10.Visible = ((bool)(resources.GetObject("menuItem10.Visible")));
   this.menuExit.Enabled = ((bool)(resources.GetObject("menuExit.Enabled")));
   this.menuExit.Index = 10;
   this.menuExit.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuExit.Shortcut")));
   this.menuExit.ShowShortcut = ((bool)(resources.GetObject("menuExit.ShowShortcut")));
   this.menuExit.Text = resources.GetString("menuExit.Text");
   this.menuExit.Visible = ((bool)(resources.GetObject("menuExit.Visible")));
   this.menuExit.Click += new System.EventHandler(this.menuExit_Click);
   this.syncAnimateTimer.Tick += new System.EventHandler(this.syncAnimateTimer_Tick);
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "FormsTrayApp";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
  }
  private void syncCollection(CollectionSyncEventArgs syncEventArgs)
  {
   try
   {
    string message = null;
    switch (syncEventArgs.Action)
    {
     case Action.StartLocalSync:
      if (!syncEventArgs.Name.StartsWith("POBox:"))
      {
       message = string.Format(resourceManager.GetString("localSync"), syncEventArgs.Name);
      }
      break;
     case Action.StartSync:
     {
      currentSyncCollectionName = syncEventArgs.Name;
      if (syncEventArgs.Name.StartsWith("POBox:"))
      {
       message = string.Format(resourceManager.GetString("checkingForiFolders"), preferences.GetDomainName(syncEventArgs.ID));
      }
      else
      {
       message = string.Format(resourceManager.GetString("synciFolder"), syncEventArgs.Name);
      }
      break;
     }
     case Action.StopSync:
     {
      syncAnimateTimer.Stop();
      shellNotifyIcon.Icon = trayIcon;
      shellNotifyIcon.Text = resourceManager.GetString("iFolderServices");
      if (initialSyncCollections.Contains(syncEventArgs.ID) && syncEventArgs.Connected)
      {
       initialSyncCollections.Remove(syncEventArgs.ID);
      }
      if (syncEventArgs.Name.StartsWith("POBox:"))
      {
       message = string.Format(resourceManager.GetString("doneCheckingForiFolders"), preferences.GetDomainName(syncEventArgs.ID));
      }
      else
      {
       message = string.Format(resourceManager.GetString("syncComplete"), syncEventArgs.Name);
      }
      break;
     }
    }
    syncLog.AddMessageToLog(syncEventArgs.TimeStamp, message);
   }
   catch {}
  }
  private void syncFile(FileSyncEventArgs syncEventArgs)
  {
   try
   {
    syncAnimateTimer.Start();
    syncToServer = syncEventArgs.Direction == Direction.Uploading;
    string message = null;
    switch (syncEventArgs.Status)
    {
     case SyncStatus.Success:
     {
      if (syncEventArgs.SizeRemaining == syncEventArgs.SizeToSync)
      {
       shellNotifyIcon.Text = resourceManager.GetString("iFolderServices") + "\n" +
        string.Format(resourceManager.GetString(syncEventArgs.Direction == Direction.Uploading ? "uploading" : "downloading") , syncEventArgs.Name);
       switch (syncEventArgs.ObjectType)
       {
        case ObjectType.File:
         if (syncEventArgs.Delete)
         {
          message = string.Format(resourceManager.GetString("deleteClientFile"), syncEventArgs.Name);
         }
         else if (syncEventArgs.SizeToSync < syncEventArgs.Size)
         {
          int savings = (int)((1 - ((double)syncEventArgs.SizeToSync / (double)syncEventArgs.Size)) * 100);
          message = string.Format(resourceManager.GetString(syncEventArgs.Direction == Direction.Uploading ? "uploadDeltaSyncFile" : "downloadDeltaSyncFile"), syncEventArgs.Name, savings);
         }
         else
         {
          switch (syncEventArgs.Direction)
          {
           case Direction.Uploading:
            message = string.Format(resourceManager.GetString("uploadFile"), syncEventArgs.Name);
            break;
           case Direction.Downloading:
            message = string.Format(resourceManager.GetString("downloadFile"), syncEventArgs.Name);
            break;
           case Direction.Local:
            message = string.Format(resourceManager.GetString("localFile"), syncEventArgs.Name);
            break;
           default:
            message = string.Format(resourceManager.GetString("syncingFile"), syncEventArgs.Name);
            break;
          }
         }
         break;
        case ObjectType.Directory:
         if (syncEventArgs.Delete)
         {
          message = string.Format(resourceManager.GetString("deleteClientDir"), syncEventArgs.Name);
         }
         else
         {
          switch (syncEventArgs.Direction)
          {
           case Direction.Uploading:
            message = string.Format(resourceManager.GetString("uploadDir"), syncEventArgs.Name);
            break;
           case Direction.Downloading:
            message = string.Format(resourceManager.GetString("downloadDir"), syncEventArgs.Name);
            break;
           case Direction.Local:
            message = string.Format(resourceManager.GetString("localDir"), syncEventArgs.Name);
            break;
           default:
            message = string.Format(resourceManager.GetString("syncingDir"), syncEventArgs.Name);
            break;
          }
         }
         break;
        case ObjectType.Unknown:
         message = string.Format(resourceManager.GetString("deleteUnknown"), syncEventArgs.Name);
         break;
       }
      }
      break;
     }
     case SyncStatus.UpdateConflict:
     case SyncStatus.FileNameConflict:
      message = string.Format(resourceManager.GetString("conflictFailure"), syncEventArgs.Name);
      break;
     case SyncStatus.Policy:
      message = string.Format(resourceManager.GetString("policyFailure"), syncEventArgs.Name);
      break;
     case SyncStatus.Access:
      message = string.Format(resourceManager.GetString("accessFailure"), syncEventArgs.Name);
      break;
     case SyncStatus.Locked:
      message = string.Format(resourceManager.GetString("lockFailure"), syncEventArgs.Name);
      break;
     case SyncStatus.PolicyQuota:
      if (!collectionsNotified.Contains(syncEventArgs.CollectionID))
      {
       collectionsNotified.Add(syncEventArgs.CollectionID, null);
       string title = string.Format(resourceManager.GetString("quotaFailureTitle"), currentSyncCollectionName);
       notifyType = NotifyType.SyncError;
       shellNotifyIcon.DisplayBalloonTooltip(title, resourceManager.GetString("quotaFailureInfo"), BalloonType.Error);
      }
      message = string.Format(resourceManager.GetString("quotaFailure"), syncEventArgs.Name);
      break;
     case SyncStatus.PolicySize:
      message = string.Format(resourceManager.GetString("policySizeFailure"), syncEventArgs.Name);
      break;
     case SyncStatus.PolicyType:
      message = string.Format(resourceManager.GetString("policyTypeFailure"), syncEventArgs.Name);
      break;
     case SyncStatus.DiskFull:
      message = string.Format(syncToServer ? resourceManager.GetString("serverDiskFullFailure") : resourceManager.GetString("clientDiskFullFailure"), syncEventArgs.Name);
      break;
     case SyncStatus.ReadOnly:
      if (!collectionsNotified.Contains(syncEventArgs.CollectionID))
      {
       collectionsNotified.Add(syncEventArgs.CollectionID, null);
       string title = string.Format(resourceManager.GetString("quotaFailureTitle"), currentSyncCollectionName);
       notifyType = NotifyType.SyncError;
       shellNotifyIcon.DisplayBalloonTooltip(title, resourceManager.GetString("readOnlyFailureInfo"), BalloonType.Error);
      }
      message = string.Format(resourceManager.GetString("readOnlyFailure"), syncEventArgs.Name);
      break;
     default:
      message = string.Format(resourceManager.GetString("genericFailure"), syncEventArgs.Name);
      break;
    }
    syncLog.AddMessageToLog(syncEventArgs.TimeStamp, message);
   }
   catch {}
  }
  private void createChangeEvent(iFolderWeb ifolder, iFolderUser ifolderUser, string eventData)
  {
   try
   {
    if (ifolder.HasConflicts)
    {
     if (preferences.NotifyCollisionEnabled)
     {
      ifolderFromNotify = ifolder;
      string message = string.Format(resourceManager.GetString("collisionMessage"), ifolder.Name);
      notifyType = NotifyType.Collision;
      shellNotifyIcon.DisplayBalloonTooltip(resourceManager.GetString("actionRequiredTitle"), message, BalloonType.Info);
     }
    }
    else if (ifolder.State.Equals("Available") && eventData.Equals("NodeCreated"))
    {
     if (preferences.NotifyShareEnabled)
     {
      ifolderFromNotify = ifolder;
      string title = string.Format(resourceManager.GetString("subscriptionTitle"), ifolder.Name);
      string message = string.Format(resourceManager.GetString("subscriptionMessage"), ifolder.Owner);
      notifyType = NotifyType.Subscription;
      shellNotifyIcon.DisplayBalloonTooltip(title, message, BalloonType.Info);
     }
    }
    else if (ifolderUser != null)
    {
     if (preferences.NotifyJoinEnabled)
     {
      ifolderFromNotify = ifolder;
      string message = string.Format(resourceManager.GetString("newMemberMessage"),
       (ifolderUser.FN != null) && !ifolderUser.FN.Equals(string.Empty) ? ifolderUser.FN : ifolderUser.Name,
       ifolder.Name);
      notifyType = NotifyType.NewMember;
      shellNotifyIcon.DisplayBalloonTooltip(resourceManager.GetString("newMemberTitle"), message, BalloonType.Info);
     }
    }
   }
   catch {}
  }
  private void notifyMessage(NotifyEventArgs notifyEventArgs)
  {
   switch (notifyEventArgs.EventData)
   {
    case "Domain-Up":
    {
     try
     {
      DomainInformation domainInfo = simiasWebService.GetDomainInformation(notifyEventArgs.Message);
      string credentials;
      if (!authenticate(domainInfo, out credentials))
      {
       if (serverInfo == null)
       {
        serverInfo = new ServerInfo(simiasManager, domainInfo, credentials);
        serverInfo.Closed += new EventHandler(serverInfo_Closed);
        serverInfo.Show();
        ShellNotifyIcon.SetForegroundWindow(serverInfo.Handle);
       }
      }
      else
      {
       try
       {
        bool update = CheckForClientUpdate(domainInfo.ID);
        if (update)
        {
         ShutdownTrayApp(null);
        }
       }
       catch
       {
       }
       domainInfo.Authenticated = true;
       preferences.UpdateDomainStatus(new Domain(domainInfo));
      }
     }
     catch
     {
     }
     break;
    }
   }
  }
  private void ShutdownTrayApp(Exception ex)
  {
   if (preferences != null)
    preferences.Hide();
   if (globalProperties != null)
    globalProperties.Hide();
   if (syncLog != null)
    syncLog.Hide();
   Cursor.Current = Cursors.WaitCursor;
   if (shellNotifyIcon != null)
   {
    shellNotifyIcon.Text = resourceManager.GetString("iFolderServicesStopping");
    shellNotifyIcon.Icon = shutdownIcon;
   }
   if (ex != null)
   {
    Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("fatalErrorMessage"), resourceManager.GetString("fatalErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
   simiasRunning = false;
   try
   {
    if (eventClient != null)
    {
     eventClient.Deregister();
    }
    simiasManager.Stop();
    if ((worker != null) && worker.IsAlive)
    {
     worker.Abort();
    }
   }
   catch
   {
   }
   Cursor.Current = Cursors.Default;
   Application.Exit();
  }
  private void eventThreadProc()
  {
   while (true)
   {
    NodeEventArgs eventArgs = null;
    int count;
    lock (eventQueue.SyncRoot)
    {
     count = eventQueue.Count;
     if (count > 0)
     {
      eventArgs = (NodeEventArgs)eventQueue.Dequeue();
     }
    }
    iFolderWeb ifolder = null;
    iFolderUser ifolderUser = null;
    try
    {
     switch (eventArgs.EventType)
     {
      case EventType.NodeChanged:
      {
       if (eventArgs.Type.Equals(NodeTypes.CollectionType))
       {
        ifolder = ifWebService.GetiFolder(eventArgs.Collection);
       }
       else if (eventArgs.Type.Equals(NodeTypes.SubscriptionType))
       {
        ifolder = ifWebService.GetiFolderInvitation(eventArgs.Collection, eventArgs.Node);
       }
       break;
      }
      case EventType.NodeCreated:
      {
       switch (eventArgs.Type)
       {
        case "Collection":
        {
         ifolder = ifWebService.GetiFolder(eventArgs.Collection);
         if (ifolder != null)
         {
          if (eventArgs.SlaveRev == 0)
          {
           initialSyncCollections.Add(eventArgs.Collection, null);
          }
         }
         break;
        }
        case "Subscription":
        {
         ifolder = ifWebService.GetiFolderInvitation(eventArgs.Collection, eventArgs.Node);
         if (!ifolder.State.Equals("Available") || (ifWebService.GetiFolder(ifolder.CollectionID) != null))
         {
          ifolder = null;
         }
         break;
        }
        case "Member":
        {
         ifolderUser = ifWebService.GetiFolderUserFromNodeID(eventArgs.Collection, eventArgs.Node);
         if ((ifolderUser != null) && !preferences.IsCurrentUser(ifolderUser.UserID))
         {
          ifolder = ifWebService.GetiFolder(eventArgs.Collection);
         }
         break;
        }
       }
       break;
      }
      case EventType.NodeDeleted:
      {
       BeginInvoke(globalProperties.deleteEventDelegate, new object[] {eventArgs});
       break;
      }
     }
    }
    catch
    {
    }
    if (ifolder != null)
    {
     if (!initialSyncCollections.Contains(eventArgs.Collection))
     {
      BeginInvoke(createChangeEventDelegate, new object[] {ifolder, ifolderUser, eventArgs.EventData});
     }
     BeginInvoke(globalProperties.createChangeEventDelegate, new object[] {ifolder, eventArgs.EventData});
    }
    if (count <= 1)
    {
     workEvent.WaitOne();
    }
   }
  }
  private void updateOverlayIcons()
  {
   try
   {
    iFolderWeb[] ifolderArray = ifWebService.GetAlliFolders();
    foreach (iFolderWeb ifolder in ifolderArray)
    {
     if (!ifolder.IsSubscription)
     {
      Win32Window.ShChangeNotify(Win32Window.SHCNE_UPDATEITEM, Win32Window.SHCNF_PATHW, ifolder.UnManagedPath, IntPtr.Zero);
     }
    }
   }
   catch {}
  }
 }
}
