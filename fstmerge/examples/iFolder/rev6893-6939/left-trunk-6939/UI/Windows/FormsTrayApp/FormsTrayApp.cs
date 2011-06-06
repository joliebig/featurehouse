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
using System.Reflection;
using Microsoft.Win32;
using Novell.iFolder.Web;
using Novell.iFolderCom;
using Novell.Win32Util;
using Novell.CustomUIControls;
using Novell.Wizard;
using Simias.Client;
using Simias.Client.Authentication;
using Simias.Client.Event;
using Novell.iFolder;
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
 public enum UpdateResult
 {
  Latest = 0,
  UpgradeNeeded = 1,
  ServerOld = 2,
  UpgradeAvailable = 3,
  Unknown = 4,
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
  private bool shutdown_msg = false;
  private bool simiasRunning = false;
        private bool simiasStarting = false;
  private bool wizardRunning = false;
        private bool exitFlag = true;
  private Icon trayIcon;
  private Icon startupIcon;
  private Icon shutdownIcon;
  private const int numberOfSyncIcons = 10;
  private Icon[] syncIcons = new Icon[numberOfSyncIcons];
  private int index = 0;
  private bool syncToServer = false;
  private Image trayImage;
  private Queue eventQueue;
  private Thread worker = null;
  private Hashtable initialSyncCollections = new Hashtable();
  private Hashtable collectionsNotified = new Hashtable();
  private string currentSyncCollectionName;
  private bool errorSyncingCurrentCollection;
  protected AutoResetEvent workEvent = null;
  private bool isConnecting = false;
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
        private Novell.iFolderCom.MyMessageBox msb;
  private SyncLog syncLog;
        private SyncLog infolog;
  private bool eventError = false;
  private System.Windows.Forms.MenuItem menuStoreBrowser;
  private System.Windows.Forms.MenuItem menuTools;
  private System.Windows.Forms.Timer syncAnimateTimer;
  private System.Windows.Forms.MenuItem menuSyncLog;
  private System.Windows.Forms.MenuItem menuPreferences;
  private System.Windows.Forms.MenuItem menuItem1;
  private System.Windows.Forms.MenuItem menuAbout;
  private System.Windows.Forms.MenuItem menuHideTrayIcon;
  private System.Windows.Forms.MenuItem menuAccounts;
  private System.Windows.Forms.Form startupWind;
  private Manager simiasManager;
  private string iFolderLogPath;
  static private FormsTrayApp instance;
  private bool NoAccounts;
        public static IiFolderLog log;
        private const int HWND_BOARDCAST = 0xffff;
        private System.Windows.Forms.Timer splashTimer;
        private System.Windows.Forms.Timer SimiasTimer;
        private static bool RegularStart = false;
        private static string lockObject = "SimiasStarting";
        private bool ShowWindow = false;
        internal uint appRestart;
        public static bool disableSyncLogPopup = false;
        public bool WizardStatus
        {
            get
            {
                return wizardRunning;
            }
            set
            {
                wizardRunning = value;
            }
        }
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
            if (args.Length == 1 && args[0].Equals("/r"))
            {
                RegularStart = true;
            }
            foreach (string argument in args)
            {
                if (argument.Equals("-disableSyncLog"))
                {
                    disableSyncLogPopup = true;
                    break;
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
            appRestart = Novell.Win32Util.Win32Window.RegisterWindowMsg("AppCreated");
            RegistryKey regKey = Registry.CurrentUser.CreateSubKey(@"SOFTWARE\Novell\iFolder");
            string language = regKey.GetValue("language") as String;
            if (language != null)
            {
                try
                {
                    Thread.CurrentThread.CurrentUICulture = new CultureInfo(language);
                }
                catch { }
            }
   string windowName = FormsTrayApp.resourceManager.GetString("iFolderServices") + Environment.UserName;
   Novell.Win32Util.Win32Window window = Novell.Win32Util.Win32Window.FindWindow(null, windowName);
   if (window != null)
   {
                Novell.Win32Util.Win32Window iFolderWindow = Novell.Win32Util.Win32Window.FindWindow(null, resourceManager.GetString("myiFolders"));
                if (iFolderWindow != null)
                {
                    iFolderWindow.SendMsg(HWND_BOARDCAST, appRestart, IntPtr.Zero, IntPtr.Zero);
                }
    shutdown = true;
   }
   else
   {
    iFolderLogPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "iFolder");
    if (!Directory.Exists(iFolderLogPath))
    {
     Directory.CreateDirectory(iFolderLogPath);
    }
    this.simiasManager = new Manager( Environment.GetCommandLineArgs() );
    InitializeComponent();
    this.Text = FormsTrayApp.resourceManager.GetString("iFolderServices") + Environment.UserName;
    this.components = new System.ComponentModel.Container();
    this.ClientSize = new System.Drawing.Size(292, 266);
    try
    {
     string basePath = Path.Combine(Application.StartupPath, "res");
     startupIcon = new Icon(Path.Combine(basePath, "ifolder_waiting_16.ico"));
     shutdownIcon = new Icon(Path.Combine(basePath, "ifolder_download_16.ico"));
     trayIcon = new Icon(Path.Combine(basePath, "ifolder_16.ico"));
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
     if( !iFolderComponent.DisplayTrayIconEnabled )
                          shellNotifyIcon.Visible = false;
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
        public static void LogInit()
        {
            try
            {
                if (!File.Exists(iFolderLogManager.LogConfFilePath))
                    File.Copy(Path.Combine(SimiasSetup.sysconfdir, iFolderLogManager.LogConfFileName), iFolderLogManager.LogConfFilePath);
                iFolderLogManager.Configure(iFolderLogManager.LogConfDirPath);
                FormsTrayApp.log = iFolderLogManager.GetLogger(typeof(System.Object));
            }
            catch { }
       }
        public static void SetTrayIconStatus(bool value)
        {
         instance.shellNotifyIcon.Visible = value;
        }
  public static void CloseApp()
  {
   instance.ShutdownTrayApp(null);
  }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
    if (components != null)
     components.Dispose();
   base.Dispose( disposing );
  }
  static public bool ClientUpdates(string domainID, out bool serverOld)
  {
   string newClientVersion = null;
   bool result = false;
   int res;
   serverOld = false;
   string serverVersion = null;
   Novell.FormsTrayApp.UpdateResult update = UpdateResult.Unknown;
   try
   {
    update = (UpdateResult)instance.ifWebService.CheckForUpdate(domainID, out serverVersion);
                newClientVersion = serverVersion;
   }
   catch(Exception ex)
   {
    MessageBox.Show(resourceManager.GetString("ServerOld"),resourceManager.GetString("ServerOldTitle"),MessageBoxButtons.OK);
    serverOld = true;
    return false;
   }
   switch( update )
   {
    case UpdateResult.Latest:
     result = true;
     break;
    case UpdateResult.UpgradeNeeded:
     result = false;
     res = (int) MessageBox.Show(String.Format( resourceManager.GetString("UpgradeClientMsg"),
                        newClientVersion),
                        resourceManager.GetString("UpgradeNeededTitle"),
                        MessageBoxButtons.YesNo);
     if( res == (int) DialogResult.Yes)
     {
      if(instance.ifWebService.RunClientUpdate(domainID, null))
      {
       instance.ShutdownTrayApp(null);
      }
     }
     else
     {
      return false;
     }
     break;
    case UpdateResult.ServerOld:
     serverOld = true;
     MessageBox.Show(resourceManager.GetString("ServerOld"),resourceManager.GetString("ServerOldTitle"),MessageBoxButtons.OK);
     return false;
    case UpdateResult.UpgradeAvailable:
                    res = (int)MessageBox.Show(String.Format(resourceManager.GetString("UpgradeClientMsg"),
                        newClientVersion),
                        resourceManager.GetString("UpgradeNeededTitle"),
                        MessageBoxButtons.YesNo);
                    if (res == (int)DialogResult.Yes)
     {
      if(instance.ifWebService.RunClientUpdate(domainID, null))
      {
       instance.ShutdownTrayApp(null);
      }
     }
     else
     {
      return true;
     }
     break;
    case UpdateResult.Unknown:
     return false;
   }
   return true;
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
     if ( updateStarted == false )
     {
      mmb = new MyMessageBox(resourceManager.GetString("clientUpgradeFailure"), resourceManager.GetString("upgradeErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
      mmb.ShowDialog();
     }
    }
   }
   return updateStarted;
  }
  public static GlobalProperties globalProp()
  {
    return instance.globalProperties;
  }
  private void menuStoreBrowser_Click(object sender, System.EventArgs e)
  {
   Process.Start(Path.Combine(Application.StartupPath, "StoreBrowser.exe"));
  }
  private void menuEventLogReader_Click(object sender, System.EventArgs e)
  {
   Process.Start(Path.Combine(Application.StartupPath, "EventLogReader.exe"));
  }
  private void menuHideTrayIcon_Click(object sender, System.EventArgs e)
       {
             iFolderComponent.DisplayTrayIconEnabled = false;
             shellNotifyIcon.Visible = false;
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
   if (!wizardRunning)
   {
    DomainInformation[] domains;
    domains = this.simiasWebService.GetDomains(false);
    if (domains.Length.Equals(0))
    {
     if (globalProperties.Visible)
      globalProperties.Hide();
     AccountWizard accountWizard = new AccountWizard(ifWebService, simiasWebService, simiasManager, true, this.preferences, this.globalProperties);
                    accountWizard.EnterpriseConnect += new Novell.Wizard.AccountWizard.EnterpriseConnectDelegate(preferences_EnterpriseConnect);
     wizardRunning = true;
     DialogResult result = accountWizard.ShowDialog();
     wizardRunning = false;
     if ( result == DialogResult.OK )
     {
      preferences_DisplayiFolderDialog( this, new EventArgs() );
     }
    }
    else
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
        private void infolog_Show()
        {
            if (infolog.Visible)
            {
                infolog.Activate();
            }
            else if (Preferences.HideSyncLogWindow)
            {
                infolog.Show();
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
            msb.StartPosition = FormStartPosition.CenterScreen;
            DialogResult messageDialogResult = msb.ShowDialog();
            if (messageDialogResult == DialogResult.Yes)
            {
                menuExit.Enabled = false;
                ShutdownTrayApp(null);
            }
            else
            {
            }
  }
  private void shellNotifyIcon_Click(object sender, EventArgs e)
  {
            ShowWindow = true;
            if (simiasRunning)
            {
                menuProperties_Click(sender, e);
            }
            else if (!simiasStarting)
            {
                shellNotifyIcon.DisplayBalloonTooltip(resourceManager.GetString("iFolderServices"), resourceManager.GetString("iFolderServicesStarting"), BalloonType.Info);
                DelayedStart();
                return;
            }
            else
            {
                shellNotifyIcon.DisplayBalloonTooltip(resourceManager.GetString("iFolderServices"), resourceManager.GetString("iFolderServicesStarting"), BalloonType.Info);
            }
  }
  private void shellNotifyIcon_BalloonClick(object sender, EventArgs e)
  {
   bool added;
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
                    ifolderAdvanced.DomainName = (simiasWebService.GetDomainInformation(ifolderFromNotify.DomainID)).Name;
                    ifolderAdvanced.DomainUrl = (simiasWebService.GetDomainInformation(ifolderFromNotify.DomainID)).HostUrl;
     ifolderAdvanced.CreateControl();
     ShellNotifyIcon.SetForegroundWindow(ifolderAdvanced.Handle);
     ifolderAdvanced.ShowDialog();
     ifolderAdvanced.Dispose();
     break;
    case NotifyType.Subscription:
                    if(ifolderFromNotify != null)
         globalProperties.AcceptiFolder( ifolderFromNotify, out added);
     break;
    case NotifyType.SyncError:
                    if (infolog.Visible)
                        infolog.Activate();
                    else
                        syncLog.Show();
                    break;
   }
  }
  private void shellNotifyIcon_ContextMenuPopup(object sender, EventArgs e)
  {
            if (!simiasRunning )
            {
                if( !simiasStarting )
                {
                    shellNotifyIcon.DisplayBalloonTooltip(resourceManager.GetString("iFolderServices"), resourceManager.GetString("iFolderServicesStarting"), BalloonType.Info);
                    DelayedStart();
                }
                foreach (MenuItem item in this.contextMenu1.MenuItems)
                {
                    item.Visible = false;
                }
                shellNotifyIcon.DisplayBalloonTooltip(resourceManager.GetString("iFolderServices"), resourceManager.GetString("iFolderServicesStarting"), BalloonType.Info);
                return;
            }
   foreach (MenuItem item in this.contextMenu1.MenuItems)
   {
    item.Visible = simiasRunning && !wizardRunning;
   }
   if (simiasRunning && !wizardRunning)
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
        public void DelayedStart()
        {
            lock (lockObject)
            {
                if (simiasRunning || simiasStarting)
                    return;
                else
                {
                    simiasStarting = true;
                    iFolderComponent.SimiasStarting = true;
                }
            }
            StartSimias();
            LogInit();
            PostSimiasStart();
        }
        public void StartSimias()
        {
            try
            {
                DateTime starttime = DateTime.Now;
                simiasManager.Start();
                if (simiasManager.WebServiceUri == null || simiasManager.DataPath == null)
                {
                    simiasManager.Start();
                    if (simiasManager.WebServiceUri == null || simiasManager.DataPath == null)
                    {
                        MessageBox.Show("Unable to start xsp web service, iFolder will not be able to work, restart the application.",
                            "iFolder",
                            MessageBoxButtons.OK,
                            MessageBoxIcon.Exclamation);
                        return;
                    }
                }
                SetWebServiceInformation(simiasManager.WebServiceUri, simiasManager.DataPath);
                ifWebService = new iFolderWebService();
                ifWebService.Url = simiasManager.WebServiceUri + "/iFolder.asmx";
                simiasWebService = new SimiasWebService();
                simiasWebService.Url = simiasManager.WebServiceUri + "/Simias.asmx";
                LocalService.Start(simiasWebService, simiasManager.WebServiceUri, simiasManager.DataPath);
                LocalService.Start(ifWebService, simiasManager.WebServiceUri, simiasManager.DataPath);
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
                this.globalProperties.iFWebService = ifWebService;
                this.globalProperties.Simws = simiasWebService;
                this.globalProperties.EventClient = eventClient;
                this.globalProperties.simManager = simiasManager;
                this.preferences.ifolderWebService = ifWebService;
                this.preferences.Simws = simiasWebService;
                this.preferences.simManager = simiasManager;
                DateTime endtime = DateTime.Now;
                simiasRunning = true;
                iFolderComponent.SimiasRunning = true;
                simiasStarting = false;
                iFolderComponent.SimiasStarting = false;
            }
            catch (Exception ex)
            {
                MessageBox.Show(string.Format("Exception in starting simias: {0}--{1}", ex.Message, ex.StackTrace));
            }
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
                    if (!Preferences.HideiFolderInTray)
                    {
                        ShowStartupScreen();
                    }
                    Environment.CurrentDirectory = Application.StartupPath;
                    for (int i = 0; i < 3; i++)
                    {
                        Process[] processArray = Process.GetProcessesByName("simias");
                        foreach (Process proc in processArray)
                        {
                            if (proc != null)
                            {
                                simiasManager.Stop();
                            }
                        }
                    }
                    ifWebService = null;
                    simiasWebService = null;
                    eventQueue = new Queue();
                    workEvent = new AutoResetEvent(false);
                    eventClient = new IProcEventClient(new IProcEventError(errorHandler), null);
                    preferences = new Preferences(ifWebService, simiasWebService, simiasManager);
                    preferences.EnterpriseConnect += new Novell.FormsTrayApp.Preferences.EnterpriseConnectDelegate(preferences_EnterpriseConnect);
                    preferences.ChangeDefaultDomain += new Novell.FormsTrayApp.Preferences.ChangeDefaultDomainDelegate(preferences_EnterpriseConnect);
                    preferences.RemoveDomain += new Novell.FormsTrayApp.Preferences.RemoveDomainDelegate(preferences_RemoveDomain);
                    preferences.ShutdownTrayApp += new Novell.FormsTrayApp.Preferences.ShutdownTrayAppDelegate(preferences_ShutdownTrayApp);
                    preferences.UpdateDomain += new Novell.FormsTrayApp.Preferences.UpdateDomainDelegate(preferences_UpdateDomain);
                    preferences.DisplayiFolderDialog += new Novell.FormsTrayApp.Preferences.DisplayiFolderDialogDelegate(preferences_DisplayiFolderDialog);
                    preferences.CreateControl();
                    IntPtr handle = preferences.Handle;
                    syncLog = new SyncLog();
                    syncLog.CreateControl();
                    handle = syncLog.Handle;
                    infolog = new SyncLog();
                    infolog.Customize();
                    globalProperties = new GlobalProperties(ifWebService, simiasWebService, eventClient);
                    globalProperties.RemoveDomain += new Novell.FormsTrayApp.GlobalProperties.RemoveDomainDelegate(globalProperties_RemoveDomain);
                    globalProperties.PreferenceDialog = preferences;
                    globalProperties.SyncLogDialog = syncLog;
                    globalProperties.CreateControl();
                    handle = globalProperties.Handle;
                    if (RegularStart)
                    {
                        DelayedStart();
                    }
                    else
                    {
                        try
                        {
                            LogInit();
                        }
                        catch { }
                        shellNotifyIcon.Text = resourceManager.GetString("iFolderServices");
                        shellNotifyIcon.Icon = trayIcon;
                        SimiasTimer = new System.Windows.Forms.Timer();
                        SimiasTimer.Interval = 120000;
                        SimiasTimer.Tick += new EventHandler(SimiasTimer_Tick);
                        SimiasTimer.Start();
                    }
                }
                catch (Exception ex)
                {
                    MessageBox.Show(string.Format("Exception: {0}--{1}", ex.Message, ex.StackTrace));
                    ShutdownTrayApp(ex);
                }
                finally
                {
                    CloseStartupScreen();
                }
   }
  }
        public void PostSimiasStart()
        {
                    bool accountPrompt = false;
                    try
                    {
                        globalProperties.InitializeServerList();
                        DomainInformation[] domains;
                        domains = this.simiasWebService.GetDomains(false);
                        foreach (DomainInformation dw in domains)
                        {
                            try
                            {
                                if (dw.IsSlave)
                                {
                                    preferences.AddDomainToList(dw);
                                    if (this.simiasWebService.GetRememberOption(dw.ID) == false)
                                        this.simiasWebService.StorePassPhrase(dw.ID, "", CredentialType.None, false);
                                }
                                BeginInvoke(globalProperties.addDomainToListDelegate, new object[] { dw });
                                preferences.SetProxyForDomain(dw.HostUrl, false);
                            }
                            catch { }
                        }
                        if (domains.Length.Equals(0))
                        {
                            this.NoAccounts = true;
                            accountPrompt = true;
                        }
                    }
                    catch { }
                    if (worker == null)
                    {
                        worker = new Thread(new ThreadStart(eventThreadProc));
                        worker.Name = "FormsTaryApp Event";
                        worker.IsBackground = true;
                        worker.Priority = ThreadPriority.BelowNormal;
                        worker.Start();
                    }
                    shellNotifyIcon.Text = resourceManager.GetString("iFolderServices");
                    shellNotifyIcon.Icon = trayIcon;
                    try
                    {
                        if (accountPrompt == false)
                        {
                            if ((!Preferences.HideiFolderInTray && RegularStart) || ShowWindow )
                            {
                                this.globalProperties.Show();
                            }
                            updateOverlayIcons();
                            string iFolderKey = @"SOFTWARE\Novell\iFolder";
                            RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);
                            int Migration = (int)regKey.GetValue("MigrationPrompt", (int)1);
                            if (Migration == 1)
                            {
                                if (iFolder2Present() == true)
                                {
                                    bool migrate = MigrationPrompt();
                                    if (migrate == true)
                                    {
                                        Novell.FormsTrayApp.MigrationWindow migrationWindow = new MigrationWindow(this.ifWebService, this.simiasWebService);
                                        migrationWindow.ShowDialog(this);
                                    }
                                }
                            }
                        }
                        if (accountPrompt)
                        {
                            bool status = false;
                            string filePathValue;
                            System.Object[] args = new System.Object[5];
                            try
                            {
                                Assembly idAssembly = Assembly.LoadFrom(AutoAccountConstants.assemblyName);
                                if (idAssembly != null)
                                {
                                    Type type = idAssembly.GetType(AutoAccountConstants.autoAccountClass);
                                    if (null != type)
                                    {
                                        args[0] = ifWebService;
                                        args[1] = simiasWebService;
                                        args[2] = simiasManager;
                                        args[3] = globalProperties;
                                        args[4] = this;
                                        System.Object autoAccount = Activator.CreateInstance(type, args);
                                        MethodInfo method = type.GetMethod(AutoAccountConstants.autoAccountCreateAccountsMethod);
                                        status = (bool)method.Invoke(autoAccount, null);
                                        if (status)
                                        {
                                            method = type.GetMethod(AutoAccountConstants.autoAccountPrefMethod);
                                            method.Invoke(autoAccount, null);
                                            PropertyInfo info = type.GetProperty(AutoAccountConstants.autoAccountFilePath);
                                            filePathValue = (string)info.GetValue(autoAccount, null);
                                            FormsTrayApp.log.Debug("File path value is {0}", filePathValue);
                                            System.IO.FileInfo fileInfo = new System.IO.FileInfo(filePathValue);
                                            System.IO.FileInfo backupFileInfo = new System.IO.FileInfo(filePathValue + ".backup");
                                            if(File.Exists(filePathValue + ".backup"))
                                            {
                                                backupFileInfo.Delete();
                                            }
                                            fileInfo.MoveTo(filePathValue + ".backup");
                                        }
                                    }
                                }
                            }
                            catch (Exception ex)
                            {
                                FormsTrayApp.log.Info("Error: {0}", ex.Message);
                                FormsTrayApp.log.Debug("Exception Type {0}", ex.GetType());
                                FormsTrayApp.log.Debug("StackTrace {0}", ex.StackTrace);
                            }
                            if (!status)
                            {
                                DomainInformation[] domains;
                                domains = this.simiasWebService.GetDomains(false);
                                if (domains.Length.Equals(0))
                                {
                                    AccountWizard accountWizard = new AccountWizard(ifWebService, simiasWebService, simiasManager, true, this.preferences, this.globalProperties);
                                    accountWizard.EnterpriseConnect += new Novell.Wizard.AccountWizard.EnterpriseConnectDelegate(preferences_EnterpriseConnect);
                                    wizardRunning = true;
                                    DialogResult result = accountWizard.ShowDialog();
                                    wizardRunning = false;
                                    if (result == DialogResult.OK)
                                    {
                                        preferences_DisplayiFolderDialog(this, new EventArgs());
                                    }
                                }
                            }
                            else if (!wizardRunning)
                            {
                                if (globalProperties.Visible)
                                {
                                    globalProperties.Activate();
                                }
                                else
                                {
                                    DomainInformation[] domains;
                                    domains = this.simiasWebService.GetDomains(false);
                                    if (!domains.Length.Equals(0))
                                        globalProperties.Show();
                                }
                            }
                        }
                    }
                    catch (Exception e1)
                    {
                        MessageBox.Show(string.Format("Exception: {0}--{1}", e1.Message, e1.StackTrace));
                    }
        }
        void SimiasTimer_Tick(object sender, EventArgs e)
        {
            SimiasTimer.Stop();
            if (!simiasRunning && !simiasStarting)
            {
                DelayedStart();
            }
        }
  private void ShowStartupScreen()
  {
            string basePath = Path.Combine(Application.StartupPath, "res");
   startupWind = new Form();
   startupWind.FormBorderStyle = FormBorderStyle.None;
   startupWind.BackgroundImage = Bitmap.FromFile(Path.Combine(basePath, "ifolder_startup_nl.png"));
            startupWind.BackColor = Color.White;
   startupWind.Icon = new Icon(Path.Combine(basePath, "ifolder_16.ico"));
   startupWind.Size = startupWind.BackgroundImage.Size;
   startupWind.StartPosition = FormStartPosition.CenterScreen;
            startupWind.TopMost = false;
   startupWind.Show();
  }
        private void CloseStartupScreen()
        {
            if (startupWind != null)
            {
                this.startupWind.Close();
                this.startupWind.Dispose();
                this.startupWind = null;
            }
        }
        private void splashTimeEvent(Object sender,EventArgs e)
        {
             if (startupWind != null)
             {
                   this.startupWind.Close();
                   this.startupWind.Dispose();
                   this.startupWind = null;
              }
              splashTimer.Stop();
              splashTimer.Enabled = false;
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
   BeginInvoke( globalProperties.addDomainToListDelegate, new object[] {e.DomainInfo} );
  }
  private void globalProperties_RemoveDomain(object sender, DomainRemoveEventArgs e)
  {
   preferences.RemoveDomainFromList(e.DomainInfo, e.DefaultDomainID);
  }
  private void preferences_EnterpriseConnect(object sender, DomainConnectEventArgs e)
  {
   BeginInvoke( globalProperties.addDomainToListDelegate, new object[] {e.DomainInfo} );
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
  private void preferences_DisplayiFolderDialog(object sender, EventArgs e)
  {
   menuProperties_Click( sender, e );
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
   this.menuHideTrayIcon = new System.Windows.Forms.MenuItem();
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
                        this.menuHideTrayIcon,
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
             this.menuHideTrayIcon.Enabled = ((bool)(resources.GetObject("menuHideTrayIcon.Enabled")));
             this.menuHideTrayIcon.Index = 9;
             this.menuHideTrayIcon.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuHideTrayIcon.Shortcut")));
             this.menuHideTrayIcon.ShowShortcut = ((bool)(resources.GetObject("menuHideTrayIcon.ShowShortcut")));
             this.menuHideTrayIcon.Text = resources.GetString("menuHideTrayIcon.Text");
        this.menuHideTrayIcon.Visible = ((bool)(resources.GetObject("menuHideTrayIcon.Visible")));
             this.menuHideTrayIcon.Click += new System.EventHandler(this.menuHideTrayIcon_Click);
   this.menuItem10.Enabled = ((bool)(resources.GetObject("menuItem10.Enabled")));
   this.menuItem10.Index = 10;
   this.menuItem10.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuItem10.Shortcut")));
   this.menuItem10.ShowShortcut = ((bool)(resources.GetObject("menuItem10.ShowShortcut")));
   this.menuItem10.Text = resources.GetString("menuItem10.Text");
   this.menuItem10.Visible = ((bool)(resources.GetObject("menuItem10.Visible")));
   this.menuExit.Enabled = ((bool)(resources.GetObject("menuExit.Enabled")));
   this.menuExit.Index = 11;
   this.menuExit.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuExit.Shortcut")));
   this.menuExit.ShowShortcut = ((bool)(resources.GetObject("menuExit.ShowShortcut")));
   this.menuExit.Text = resources.GetString("menuExit.Text");
   this.menuExit.Visible = ((bool)(resources.GetObject("menuExit.Visible")));
   this.menuExit.Click += new System.EventHandler(this.menuExit_Click);
            msb = new MyMessageBox(resourceManager.GetString("exitMessage"), resourceManager.GetString("exitTitle"), string.Empty, MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question);
            String basepath = Path.Combine(Application.StartupPath, "res");
            Icon icon = new Icon(Path.Combine(basepath, "ifolder_16.ico"));
            msb.Icon = icon;
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
                            errorSyncingCurrentCollection = false;
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
                                if (errorSyncingCurrentCollection == true && ifWebService.CheckiFolder(syncEventArgs.ID) != true)
                                {
                                }
                            }
                            if (errorSyncingCurrentCollection == true)
                            {
                                string title = string.Format(resourceManager.GetString("quotaFailureTitle"), currentSyncCollectionName);
                                notifyType = NotifyType.SyncError;
                                infolog_Show();
                            }
                            break;
                        }
                }
                syncLog.AddMessageToLog(syncEventArgs.TimeStamp, message);
            }
            catch { }
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
                                    string.Format(resourceManager.GetString(syncEventArgs.Direction == Direction.Uploading ? "uploading" : "downloading"), syncEventArgs.Name);
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
                        errorSyncingCurrentCollection = true;
               infolog.AddMessageToLog(syncEventArgs.TimeStamp, message);
                        break;
                    case SyncStatus.Policy:
                        message = string.Format(resourceManager.GetString("policyFailure"), syncEventArgs.Name);
                        errorSyncingCurrentCollection = true;
                        infolog.AddMessageToLog(syncEventArgs.TimeStamp, message);
                        break;
                    case SyncStatus.Access:
                        message = string.Format(resourceManager.GetString("accessFailure"), syncEventArgs.Name);
                        errorSyncingCurrentCollection = true;
                        infolog.AddMessageToLog(syncEventArgs.TimeStamp, message);
                        break;
                    case SyncStatus.Locked:
                        message = string.Format(resourceManager.GetString("lockFailure"), syncEventArgs.Name);
                        errorSyncingCurrentCollection = true;
                        infolog.AddMessageToLog(syncEventArgs.TimeStamp, message);
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
                        errorSyncingCurrentCollection = true;
                        infolog.AddMessageToLog(syncEventArgs.TimeStamp, message);
                        break;
                    case SyncStatus.PolicySize:
                        message = string.Format(resourceManager.GetString("policySizeFailure"), syncEventArgs.Name);
                        errorSyncingCurrentCollection = true;
                        shellNotifyIcon.DisplayBalloonTooltip(string.Format(resourceManager.GetString("quotaFailureTitle"), currentSyncCollectionName), string.Format(resourceManager.GetString("policySizeFailure"), syncEventArgs.Name), BalloonType.Error);
                        infolog.AddMessageToLog(syncEventArgs.TimeStamp, message);
                        break;
                    case SyncStatus.PolicyType:
                        message = string.Format(resourceManager.GetString("policyTypeFailure"), syncEventArgs.Name);
                        if (!(syncEventArgs.Name.EndsWith("thumbs.db") || syncEventArgs.Name.EndsWith("Thumbs.db")))
                        {
                            errorSyncingCurrentCollection = true;
                            infolog.AddMessageToLog(syncEventArgs.TimeStamp, message);
                        }
                        notifyType = NotifyType.SyncError;
                        if (Preferences.HidePolicyVoilationNotification)
                        {
                           shellNotifyIcon.DisplayBalloonTooltip(string.Format(resourceManager.GetString("quotaFailureTitle"), currentSyncCollectionName), string.Format(resourceManager.GetString("policyTypeFailure"), syncEventArgs.Name), BalloonType.Error);
                        }
                        break;
                    case SyncStatus.DiskFull:
                        message = string.Format(syncToServer ? resourceManager.GetString("serverDiskFullFailure") : resourceManager.GetString("clientDiskFullFailure"), syncEventArgs.Name);
                        errorSyncingCurrentCollection = true;
                        infolog.AddMessageToLog(syncEventArgs.TimeStamp, message );
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
                        errorSyncingCurrentCollection = true;
                        infolog.AddMessageToLog(syncEventArgs.TimeStamp, message);
                        break;
                    case SyncStatus.PathTooLong:
                        if (syncEventArgs.Direction == Direction.Downloading)
                        {
                            syncLog.AddMessageToLog(syncEventArgs.TimeStamp, string.Format("Path is too long for the file \"{0}\" to be downloaded.", syncEventArgs.Name));
                        }
                        break;
                    case SyncStatus.Busy:
                        syncLog.AddMessageToLog(syncEventArgs.TimeStamp, string.Format("Could not synchronize because the server is busy: {0}", syncEventArgs.Name));
                        break;
                    case SyncStatus.ClientError:
                        syncLog.AddMessageToLog(syncEventArgs.TimeStamp, string.Format("Client sent bad data and could not synchronize: {0}", syncEventArgs.Name));
                        break;
                    case SyncStatus.InUse:
                        syncLog.AddMessageToLog(syncEventArgs.TimeStamp, string.Format("Could not synchronize because this file is in use: {0}", syncEventArgs.Name));
                        break;
                    case SyncStatus.ServerFailure:
                        syncLog.AddMessageToLog(syncEventArgs.TimeStamp, string.Format("Updating the metadata for this file failed: {0}", syncEventArgs.Name));
                        break;
                    default:
                        message = string.Format(resourceManager.GetString("genericFailure"), syncEventArgs.Name);
                        errorSyncingCurrentCollection = true;
                        infolog.AddMessageToLog(syncEventArgs.TimeStamp, message);
                        break;
                }
                syncLog.AddMessageToLog(syncEventArgs.TimeStamp, message);
            }
            catch { }
  }
  private void createChangeEvent(iFolderWeb ifolder, iFolderUser ifolderUser, string eventData)
  {
   try
   {
    if (ifolder.HasConflicts)
    {
     if (Preferences.NotifyCollisionEnabled)
     {
      ifolderFromNotify = ifolder;
      string message = string.Format(resourceManager.GetString("collisionMessage"), ifolder.Name);
      notifyType = NotifyType.Collision;
      shellNotifyIcon.DisplayBalloonTooltip(resourceManager.GetString("actionRequiredTitle"), message, BalloonType.Info);
     }
    }
    else if (ifolder.State.Equals("Available") && eventData.Equals("NodeCreated"))
    {
     if (Preferences.NotifyShareEnabled)
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
     if (Preferences.NotifyJoinEnabled)
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
     if ( !isConnecting && serverInfo == null )
     {
      isConnecting = true;
      try
      {
       DomainInformation domainInfo = simiasWebService.GetDomainInformation(notifyEventArgs.Message);
       Connecting connecting = new Connecting( this.ifWebService, simiasWebService, simiasManager, domainInfo );
       connecting.StartPosition = FormStartPosition.CenterScreen;
       if ( connecting.ShowDialog() != DialogResult.OK )
       {
        serverInfo = new ServerInfo(this.ifWebService, simiasManager, domainInfo, connecting.Password);
        serverInfo.Closed += new EventHandler(serverInfo_Closed);
        serverInfo.Show();
        ShellNotifyIcon.SetForegroundWindow(serverInfo.Handle);
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
                                globalProperties.AddDomainToUIList(domainInfo);
                                globalProperties.updateifListViewDomainStatus(domainInfo.ID, true);
       }
      }
      catch (Exception ex)
      {
       MessageBox.Show(string.Format("Exception here: {0}--{1}", ex.Message, ex.StackTrace));
      }
      isConnecting = false;
     }
     break;
    }
   }
  }
  public static void ShutdownForms()
  {
   instance.ShutdownTrayApp(null);
  }
  private void ShutdownTrayApp(Exception ex)
  {
   if (preferences != null)
    preferences.Hide();
   if (globalProperties != null)
    globalProperties.Hide();
   if (syncLog != null)
    syncLog.Hide();
            if (infolog != null)
                infolog.Hide();
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
            iFolderComponent.SimiasRunning = false;
   try
   {
    if (eventClient != null)
    {
     eventClient.Deregister();
    }
    if(this.MachineShutdown() == false && globalProperties.MachineShutdown() == false && preferences.MachineShutdown() == false && syncLog.MachineShutdown() == false)
    {
     simiasManager.Stop();
    }
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
  private const int WM_QUERYENDSESSION = 0x0011;
  protected override void WndProc(ref Message m)
  {
   switch (m.Msg)
   {
    case WM_QUERYENDSESSION:
    {
     this.shutdown_msg = true;
     break;
    }
   }
            if (m.Msg == this.appRestart)
            {
                if (this.simiasWebService != null && !wizardRunning)
                {
                    DomainInformation[] domains;
                    domains = this.simiasWebService.GetDomains(false);
                    if (domains.Length.Equals(0))
                    {
                        if (globalProperties.Visible)
                            globalProperties.Hide();
                        AccountWizard accountWizard = new AccountWizard(ifWebService, simiasWebService, simiasManager, true, this.preferences, this.globalProperties);
                        accountWizard.EnterpriseConnect += new Novell.Wizard.AccountWizard.EnterpriseConnectDelegate(preferences_EnterpriseConnect);
                        wizardRunning = true;
                        DialogResult result = accountWizard.ShowDialog();
                        wizardRunning = false;
                        if (result == DialogResult.OK)
                        {
                            preferences_DisplayiFolderDialog(this, new EventArgs());
                        }
                        else
                            globalProperties.Hide();
                    }
                    else
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
                }
            }
   base.WndProc (ref m);
  }
  public bool MachineShutdown()
  {
   return this.shutdown_msg;
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
        ifolder = ifWebService.GetMinimaliFolder(eventArgs.Collection, 1);
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
         ifolder = ifWebService.GetMinimaliFolder(eventArgs.Collection, 1);
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
         if (!ifolder.State.Equals("Available") || (ifWebService.CheckiFolder(ifolder.CollectionID) != true))
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
          ifolder = ifWebService.GetMinimaliFolder(eventArgs.Collection, 1);
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
  private bool iFolder2Present()
  {
   string iFolderRegistryKey = @"Software\Novell iFolder";
   bool status = false;
   RegistryKey iFolderKey = Registry.LocalMachine.OpenSubKey(iFolderRegistryKey);
   if(iFolderKey == null)
    return false;
   string[] AllKeys = new string[iFolderKey.SubKeyCount];
   string User;
   AllKeys = iFolderKey.GetSubKeyNames();
   for(int i=0; i< AllKeys.Length; i++)
   {
    User = iFolderRegistryKey + "\\" + AllKeys[i];
    RegistryKey UserKey = Registry.LocalMachine.OpenSubKey(User);
    if (UserKey == null)
     return false;
    if( UserKey.GetValue("FolderPath") != null)
    {
     status = true;
     break;
    }
    UserKey.Close();
   }
   iFolderKey.Close();
   return status;
  }
  private bool MigrationPrompt()
  {
   Novell.iFolderCom.NewiFolder newif = new Novell.iFolderCom.NewiFolder();
   newif.ShowMigrationPrompt();
   newif.ShowDialog(null);
   bool dontAsk = newif.DontAsk;
   bool result = newif.Migrate;
   newif.Dispose();
   newif.Close();
   if( dontAsk == true)
   {
    string iFolderKey = @"SOFTWARE\Novell\iFolder";
    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);
    regKey.SetValue("MigrationPrompt",0);
    regKey.Close();
   }
   return result;
  }
        private void yes_Clicked(object sender, EventArgs e)
  {
      exitFlag=true;
  }
  private void no_Clicked(object sender, EventArgs e)
  {
   exitFlag=false;
   msb.Hide();
  }
 }
    public class AutoAccountConstants
    {
        public const string assemblyName = @"lib\plugins\AutoAccountCreator.dll";
        public const string autoAccountClass = "Novell.AutoAccount.AutoAccount";
        public const string autoAccountCreateAccountsMethod = "CreateAccounts";
        public const string autoAccountPrefMethod = "SetPreferences";
        public const string autoAccountFilePath = "AutoAccountFilePath";
     }
}
