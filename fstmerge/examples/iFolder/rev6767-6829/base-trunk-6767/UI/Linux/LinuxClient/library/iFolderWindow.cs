using System;
using System.IO;
using System.Collections;
using System.Text;
using System.Threading;
using Gtk;
using Simias.Client;
using Simias.Client.Event;
using Simias.Storage;
using Novell.iFolder.Events;
using Novell.iFolder.Controller;
using System.Reflection;
namespace Novell.iFolder
{
 public class iFolderWindow : Window
 {
  const int ctx = 1;
  private iFolderWebService ifws;
  private SimiasWebService simws;
  private iFolderData ifdata;
  private SimiasEventBroker simiasEventBroker;
  private Statusbar MainStatusBar;
  private ProgressBar SyncBar;
  private ImageMenuItem NewMenuItem;
  private Gtk.MenuItem ShareMenuItem;
  private ImageMenuItem OpenMenuItem;
  private Gtk.MenuItem ConflictMenuItem;
  private Gtk.MenuItem SyncNowMenuItem;
  private ImageMenuItem RevertMenuItem;
  private ImageMenuItem DeleteMenuItem;
  private ImageMenuItem RemoveMenuItem;
  private ImageMenuItem DownloadMenuItem;
  private ImageMenuItem MergeMenuItem;
  private ImageMenuItem PropMenuItem;
  private ImageMenuItem CloseMenuItem;
  private ImageMenuItem QuitMenuItem;
  private ImageMenuItem RefreshMenuItem;
  private ImageMenuItem HelpMenuItem;
  private MenuItem RecoveryMenuItem;
  private MenuItem ExportMenuSubItem;
  private MenuItem ImportMenuSubItem;
         private MenuItem ResetPassMenuItem;
  private MenuItem ResetPasswordMenuItem;
  private ImageMenuItem AboutMenuItem;
  private ImageMenuItem PreferencesMenuItem;
  private Gtk.MenuItem AccountsMenuItem;
  private Gtk.MenuItem SyncLogMenuItem;
  private CheckMenuItem ViewServeriFoldersMenuItem;
  private Gtk.MenuItem MigrateMenuItem;
  private Gtk.MenuItem MigrateMenuSubItem;
  private DomainController domainController;
  private Manager simiasManager;
  private Hashtable PropDialogs;
  private Hashtable ConflictDialogs;
  private EventBox ContentEventBox;
  private Entry SearchEntry;
  private Button CancelSearchButton;
  private uint searchTimeoutID;
  private Button AddiFolderButton;
  private Button ShowHideAllFoldersButton;
  private Label ShowHideAllFoldersButtonText;
  private bool bAvailableFoldersShowing;
  private ScrolledWindow iFoldersScrolledWindow;
  private iFolderIconView iFoldersIconView;
  private static iFolderViewGroup localGroup;
  private TreeModelFilter myiFoldersFilter;
  private TreeModelFilter treeModelFilter;
  private Timer updateStatusTimer;
  private VBox SynchronizedFolderTasks;
  private Button OpenSynchronizedFolderButton;
  private Button SynchronizeNowButton;
  private Button ShareSynchronizedFolderButton;
  private Button ResolveConflictsButton;
  private Button RemoveiFolderButton;
  private Button ViewFolderPropertiesButton;
  private Button DownloadAvailableiFolderButton;
  private Button MergeAvailableiFolderButton;
  private Button DeleteFromServerButton;
  private Button RemoveMembershipButton;
  private Hashtable serverGroups;
  private Hashtable serverGroupFilters;
  private Timer RefreshAvailableiFolderTimer;
  private int lastXPos;
  private int lastYPos;
        public static IiFolderLog log;
  public int LastXPos
  {
   get
   {
    return lastXPos;
   }
  }
  public int LastYPos
  {
   get
   {
    return lastYPos;
   }
  }
        public enum DragTargetType
        {
         UriList,
         RootWindow,
         iFolderID
        };
  public iFolderWindow(iFolderWebService webService, SimiasWebService SimiasWS, Manager simiasManager)
   : base (Util.GS("iFolder"))
  {
   if(webService == null)
    throw new ApplicationException("iFolderWebServices was null");
   ifws = webService;
   simws = SimiasWS;
   this.simiasManager = simiasManager;
   ifdata = iFolderData.GetData();
   serverGroups = new Hashtable();
   serverGroupFilters = new Hashtable();
   PropDialogs = new Hashtable();
   ConflictDialogs = new Hashtable();
   lastXPos = -1;
   lastYPos = -1;
   searchTimeoutID = 0;
   domainController = DomainController.GetDomainController();
   bAvailableFoldersShowing = false;
   CreateWidgets();
   RefreshiFolders(true);
   if (domainController != null)
   {
    domainController.DomainAdded +=
     new DomainAddedEventHandler(OnDomainAddedEvent);
    domainController.DomainDeleted +=
     new DomainDeletedEventHandler(OnDomainDeletedEvent);
    domainController.DomainLoggedIn +=
     new DomainLoggedInEventHandler(OnDomainLoggedInEvent);
    domainController.DomainLoggedOut +=
     new DomainLoggedOutEventHandler(OnDomainLoggedOutEvent);
   }
   simiasEventBroker = SimiasEventBroker.GetSimiasEventBroker();
   if (simiasEventBroker != null)
   {
    simiasEventBroker.CollectionSyncEventFired +=
     new CollectionSyncEventHandler(OniFolderSyncEvent);
    simiasEventBroker.FileSyncEventFired +=
     new FileSyncEventHandler(OniFolderFileSyncEvent);
   }
  }
  ~iFolderWindow()
  {
   if (domainController != null)
   {
    domainController.DomainAdded -=
     new DomainAddedEventHandler(OnDomainAddedEvent);
    domainController.DomainDeleted -=
     new DomainDeletedEventHandler(OnDomainDeletedEvent);
    domainController.DomainLoggedIn -=
     new DomainLoggedInEventHandler(OnDomainLoggedInEvent);
    domainController.DomainLoggedOut -=
     new DomainLoggedOutEventHandler(OnDomainLoggedOutEvent);
   }
   if (simiasEventBroker != null)
   {
    simiasEventBroker.CollectionSyncEventFired -=
     new CollectionSyncEventHandler(OniFolderSyncEvent);
    simiasEventBroker.FileSyncEventFired -=
     new FileSyncEventHandler(OniFolderFileSyncEvent);
   }
  }
  private void CreateWidgets()
  {
   this.SetDefaultSize (600, 480);
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));
   this.WindowPosition = Gtk.WindowPosition.Center;
   this.Add(CreateContentArea());
   this.Realized += new EventHandler(OnRealizeWidget);
   this.RefreshAvailableiFolderTimer = new Timer(new TimerCallback(RefreshAvailableiFolderTimer_click), null , 300000, 300000 );
  }
  private Widget CreateContentArea()
  {
   VBox vbox = new VBox (false, 0);
   MenuBar menubar = CreateNormalMenu ();
   vbox.PackStart (menubar, false, false, 0);
   vbox.PackStart(CreateiFolderContentArea(), true, true, 0);
   MainStatusBar = new Statusbar ();
   UpdateStatus(Util.GS("Idle..."));
   vbox.PackStart (MainStatusBar, false, false, 0);
   NewMenuItem.Sensitive = true;
   DownloadMenuItem.Sensitive = false;
   MergeMenuItem.Sensitive = false;
   DeleteMenuItem.Sensitive = false;
   RemoveMenuItem.Sensitive = false;
   RemoveMenuItem.Visible = false;
   ShareMenuItem.Sensitive = false;
   OpenMenuItem.Sensitive = false;
   SyncNowMenuItem.Sensitive = false;
   ConflictMenuItem.Sensitive = false;
   RevertMenuItem.Sensitive = false;
   PropMenuItem.Sensitive = false;
   return vbox;
  }
  private MenuBar CreateNormalMenu ()
  {
   MenuBar menubar = new MenuBar ();
   AccelGroup agrp = new AccelGroup();
   this.AddAccelGroup(agrp);
   Menu iFolderMenu = new Menu();
   NewMenuItem = new ImageMenuItem (Util.GS("_Upload a Folder..."));
   NewMenuItem.Image = new Image(
     new Gdk.Pixbuf(Util.ImagesPath("ifolder-upload16.png")));
   iFolderMenu.Append(NewMenuItem);
   NewMenuItem.AddAccelerator("activate", agrp,
    new AccelKey(Gdk.Key.N, Gdk.ModifierType.ControlMask,
        AccelFlags.Visible));
   NewMenuItem.Activated += new EventHandler(AddiFolderHandler);
   DownloadMenuItem =
    new ImageMenuItem (Util.GS("_Download..."));
   DownloadMenuItem.Image = new Image(
    new Gdk.Pixbuf(Util.ImagesPath("ifolder-download16.png")));
   MergeMenuItem = new ImageMenuItem( Util.GS("Merge to Folder"));
   MergeMenuItem.Image = new Image( new Gdk.Pixbuf(Util.ImagesPath("ifolder-download16.png")));
   iFolderMenu.Append(DownloadMenuItem);
   iFolderMenu.Append(MergeMenuItem);
   DownloadMenuItem.Activated += new EventHandler(DownloadAvailableiFolderHandler);
   MergeMenuItem.Activated += new EventHandler(MergeAvailableiFolderHandler);
   DeleteMenuItem =
    new ImageMenuItem (Util.GS("Dele_te From Server"));
   DeleteMenuItem.Image = new Image(Stock.Delete, Gtk.IconSize.Menu);
   iFolderMenu.Append(DeleteMenuItem);
   DeleteMenuItem.Activated += new EventHandler(DeleteFromServerHandler);
   RemoveMenuItem =
    new ImageMenuItem (Util.GS("Re_move my membership"));
   RemoveMenuItem.Image = new Image(Stock.Delete, Gtk.IconSize.Menu);
   iFolderMenu.Append(RemoveMenuItem);
   RemoveMenuItem.Activated += new EventHandler(RemoveMembershipHandler);
   iFolderMenu.Append(new SeparatorMenuItem());
   OpenMenuItem = new ImageMenuItem ( Stock.Open, agrp );
   iFolderMenu.Append(OpenMenuItem);
   OpenMenuItem.Activated += new EventHandler(OnOpenSynchronizedFolder);
   ShareMenuItem = new MenuItem (Util.GS("Share _with..."));
   iFolderMenu.Append(ShareMenuItem);
   ShareMenuItem.Activated += new EventHandler(OnShareSynchronizedFolder);
   ConflictMenuItem = new MenuItem (Util.GS("Resolve conflic_ts"));
   iFolderMenu.Append(ConflictMenuItem);
   ConflictMenuItem.Activated +=
     new EventHandler(OnResolveConflicts);
   SyncNowMenuItem = new MenuItem(Util.GS("S_ynchronize now"));
   iFolderMenu.Append(SyncNowMenuItem);
   SyncNowMenuItem.Activated += new EventHandler(OnSynchronizeNow);
   RevertMenuItem =
    new ImageMenuItem (Util.GS("_Revert to a Normal Folder"));
   RevertMenuItem.Image = new Image(Stock.Undo, Gtk.IconSize.Menu);
   iFolderMenu.Append(RevertMenuItem);
   RevertMenuItem.Activated += new EventHandler(RemoveiFolderHandler);
   PropMenuItem = new ImageMenuItem (Stock.Properties, agrp);
   iFolderMenu.Append(PropMenuItem);
   PropMenuItem.Activated += new EventHandler(OnShowFolderProperties);
   iFolderMenu.Append(new SeparatorMenuItem());
   MigrateMenuItem = new MenuItem(Util.GS("_Migrate iFolder"));
   Menu MigrateMenu = new Menu();
   MigrateMenuSubItem = new MenuItem(Util.GS("Migrate from 2.x"));
   MigrateMenu.Append(MigrateMenuSubItem);
   MigrateMenuItem.Submenu = MigrateMenu;
   iFolderMenu.Append( MigrateMenuItem);
   MigrateMenuSubItem.Activated += new EventHandler(Migrate2xClickedHandler);
   iFolderMenu.Append(new SeparatorMenuItem());
   CloseMenuItem = new ImageMenuItem (Stock.Close, agrp);
   iFolderMenu.Append(CloseMenuItem);
   CloseMenuItem.Activated += new EventHandler(CloseEventHandler);
   QuitMenuItem = new ImageMenuItem(Stock.Quit, agrp);
   iFolderMenu.Append(QuitMenuItem);
   QuitMenuItem.Activated += new EventHandler(QuitEventHandler);
   MenuItem iFolderMenuItem = new MenuItem(Util.GS("i_Folder"));
   iFolderMenuItem.Submenu = iFolderMenu;
   menubar.Append (iFolderMenuItem);
   Menu EditMenu = new Menu();
   AccountsMenuItem =
    new MenuItem (Util.GS("_Account Settings..."));
   EditMenu.Append(AccountsMenuItem);
   AccountsMenuItem.Activated += new EventHandler(AccountsMenuItemHandler);
   PreferencesMenuItem = new ImageMenuItem(Util.GS("_Preferences"));
   PreferencesMenuItem.Image = new Image(Stock.Preferences, Gtk.IconSize.Menu);
   EditMenu.Append(PreferencesMenuItem);
   PreferencesMenuItem.Activated += new EventHandler(ShowPreferencesHandler);
   MenuItem EditMenuItem = new MenuItem(Util.GS("_Edit"));
   EditMenuItem.Submenu = EditMenu;
   menubar.Append(EditMenuItem);
   Menu ViewMenu = new Menu();
   RefreshMenuItem =
    new ImageMenuItem(Stock.Refresh, agrp);
   ViewMenu.Append(RefreshMenuItem);
   RefreshMenuItem.Activated +=
     new EventHandler(RefreshiFoldersHandler);
   ViewMenu.Append(new SeparatorMenuItem());
   SyncLogMenuItem =
    new MenuItem (Util.GS("Synchronization _Log"));
   ViewMenu.Append(SyncLogMenuItem);
   SyncLogMenuItem.Activated += new EventHandler(SyncLogMenuItemHandler);
   ViewMenu.Append(new SeparatorMenuItem());
   ViewServeriFoldersMenuItem =
    new CheckMenuItem(Util.GS("View _available iFolders"));
   ViewMenu.Append(ViewServeriFoldersMenuItem);
   ViewServeriFoldersMenuItem.Toggled +=
    new EventHandler(OnToggleViewServeriFoldersMenuItem);
   if((bool)ClientConfig.Get(ClientConfig.KEY_IFOLDER_DEBUG_COLOR_PALETTE))
   {
    MenuItem showColorPaletteMenuItem =
     new MenuItem("Show _Color Palette (FIXME: Remove this before shipping)...");
    ViewMenu.Append(showColorPaletteMenuItem);
    showColorPaletteMenuItem.Activated += ShowColorPalette;
   }
   MenuItem ViewMenuItem = new MenuItem(Util.GS("_View"));
   ViewMenuItem.Submenu = ViewMenu;
   menubar.Append(ViewMenuItem);
   Menu SecurityMenu = new Menu();
   RecoveryMenuItem = new MenuItem(Util.GS("_Key Recovery"));
   SecurityMenu.Append(RecoveryMenuItem);
   ImportMenuSubItem = new MenuItem(Util.GS("Import Decrypted Keys"));
   ExportMenuSubItem = new MenuItem(Util.GS("Export Encrypted Keys"));
   ImportMenuSubItem.Activated += new EventHandler(ImportClicked);
   ExportMenuSubItem.Activated += new EventHandler(ExportClicked);
   Menu recoverMenu = new Menu();
   recoverMenu.Append( ExportMenuSubItem);
   recoverMenu.Append( ImportMenuSubItem);
   RecoveryMenuItem.Submenu = recoverMenu;;
   ResetPassMenuItem = new MenuItem(Util.GS("Reset _Passphrase"));
   ResetPassMenuItem.Activated += new EventHandler(OnResetPassMenuItem);
   SecurityMenu.Append(ResetPassMenuItem);
   ResetPasswordMenuItem = new MenuItem(Util.GS("Change Password"));
   ResetPasswordMenuItem.Activated += new EventHandler(OnResetPasswordMenuItem);
   SecurityMenu.Append(ResetPasswordMenuItem);
   MenuItem MainSecurityMenuItem = new MenuItem (Util.GS ("_Security"));
   MainSecurityMenuItem.Submenu = SecurityMenu;
   menubar.Append (MainSecurityMenuItem);
   Menu HelpMenu = new Menu();
   HelpMenuItem =
    new ImageMenuItem(Stock.Help, agrp);
   HelpMenu.Append(HelpMenuItem);
   HelpMenuItem.Activated += new EventHandler(OnHelpMenuItem);
   AboutMenuItem = new ImageMenuItem(Util.GS("A_bout"));
   AboutMenuItem.Image = new Image(Gnome.Stock.About,
       Gtk.IconSize.Menu);
   HelpMenu.Append(AboutMenuItem);
   AboutMenuItem.Activated += new EventHandler(OnAbout);
   MenuItem MainHelpMenuItem = new MenuItem(Util.GS("_Help"));
   MainHelpMenuItem.Submenu = HelpMenu;
   menubar.Append(MainHelpMenuItem);
   return menubar;
  }
  private Widget CreateiFolderContentArea()
  {
   ContentEventBox = new EventBox();
   ContentEventBox.ModifyBg(StateType.Normal, this.Style.Background(StateType.Active));
   VBox vbox = new VBox(false, 0);
   ContentEventBox.Add(vbox);
   HBox hbox = new HBox(false, 0);
   hbox.PackStart(CreateActions(), false, false, 12);
   hbox.PackStart(CreateIconViewPane(), true, true, 0);
   vbox.PackStart(hbox, true, true, 0);
   return ContentEventBox;
  }
  private Widget CreateActions()
  {
   VBox actionsVBox = new VBox(false, 0);
   actionsVBox.WidthRequest = 175;
   Label l = new Label("<span size=\"small\"></span>");
   actionsVBox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l = new Label(
    string.Format(
     "<span size=\"large\">{0}</span>",
     Util.GS("Filter")));
   actionsVBox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l.ModifyFg(StateType.Normal, this.Style.Base(StateType.Selected));
   l.Xalign = 0.0F;
   HBox searchHBox = new HBox(false, 4);
   actionsVBox.PackStart(searchHBox, false, false, 0);
   SearchEntry = new Entry();
   searchHBox.PackStart(SearchEntry, true, true, 0);
   SearchEntry.SelectRegion(0, -1);
   SearchEntry.CanFocus = true;
   SearchEntry.Changed +=
    new EventHandler(OnSearchEntryChanged);
   Image stopImage = new Image(Stock.Stop, Gtk.IconSize.Menu);
   stopImage.SetAlignment(0.5F, 0F);
   CancelSearchButton = new Button(stopImage);
   CancelSearchButton.Relief = ReliefStyle.None;
   CancelSearchButton.Sensitive = false;
   CancelSearchButton.Clicked +=
    new EventHandler(OnCancelSearchButton);
   CancelSearchButton.Visible = false;
   l = new Label("<span size=\"small\"></span>");
   actionsVBox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l = new Label(
    string.Format(
     "<span size=\"large\">{0}</span>",
     Util.GS("General Actions")));
   actionsVBox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l.ModifyFg(StateType.Normal, this.Style.Base(StateType.Selected));
   l.Xalign = 0.0F;
   HBox spacerHBox = new HBox(false, 0);
   actionsVBox.PackStart(spacerHBox, false, false, 0);
   spacerHBox.PackStart(new Label(""), false, false, 4);
   VBox vbox = new VBox(false, 0);
   spacerHBox.PackStart(vbox, true, true, 0);
   HBox hbox = new HBox(false, 0);
   AddiFolderButton = new Button(hbox);
   vbox.PackStart(AddiFolderButton, false, false, 0);
   AddiFolderButton.Relief = ReliefStyle.None;
   Label buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Upload a folder...")));
   hbox.PackStart(buttonText, false, false, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   AddiFolderButton.Clicked +=
    new EventHandler(AddiFolderHandler);
   hbox = new HBox(false, 0);
   ShowHideAllFoldersButton = new Button(hbox);
   vbox.PackStart(ShowHideAllFoldersButton, false, false, 0);
   ShowHideAllFoldersButton.Relief = ReliefStyle.None;
   ShowHideAllFoldersButtonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("View available iFolders")));
   hbox.PackStart(ShowHideAllFoldersButtonText, false, false, 4);
   ShowHideAllFoldersButtonText.UseMarkup = true;
   ShowHideAllFoldersButtonText.UseUnderline = false;
   ShowHideAllFoldersButtonText.Xalign = 0;
   ShowHideAllFoldersButton.Clicked +=
    new EventHandler(ShowHideAllFoldersHandler);
   l = new Label("<span size=\"small\"></span>");
   actionsVBox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   SynchronizedFolderTasks = new VBox(false, 0);
   actionsVBox.PackStart(SynchronizedFolderTasks, false, false, 0);
   l = new Label(
    string.Format(
     "<span size=\"large\">{0}</span>",
     Util.GS("iFolder Actions")));
   SynchronizedFolderTasks.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l.ModifyFg(StateType.Normal, this.Style.Base(StateType.Selected));
   l.Xalign = 0.0F;
   spacerHBox = new HBox(false, 0);
   SynchronizedFolderTasks.PackStart(spacerHBox, false, false, 0);
   spacerHBox.PackStart(new Label(""), false, false, 4);
   vbox = new VBox(false, 0);
   spacerHBox.PackStart(vbox, true, true, 0);
   hbox = new HBox(false, 0);
   OpenSynchronizedFolderButton = new Button(hbox);
   vbox.PackStart(OpenSynchronizedFolderButton, false, false, 0);
   OpenSynchronizedFolderButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Open...")));
   hbox.PackStart(buttonText, false, false, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   OpenSynchronizedFolderButton.Clicked +=
    new EventHandler(OnOpenSynchronizedFolder);
   hbox = new HBox(false, 0);
   ResolveConflictsButton = new Button(hbox);
   vbox.PackStart(ResolveConflictsButton, false, false, 0);
   ResolveConflictsButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Resolve conflicts...")));
   hbox.PackStart(buttonText, false, false, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   ResolveConflictsButton.Clicked +=
    new EventHandler(OnResolveConflicts);
   hbox = new HBox(false, 0);
   SynchronizeNowButton = new Button(hbox);
   vbox.PackStart(SynchronizeNowButton, false, false, 0);
   SynchronizeNowButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Synchronize Now")));
   hbox.PackStart(buttonText, true, true, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   SynchronizeNowButton.Clicked +=
    new EventHandler(OnSynchronizeNow);
   hbox = new HBox(false, 0);
   ShareSynchronizedFolderButton = new Button(hbox);
   vbox.PackStart(ShareSynchronizedFolderButton, false, false, 0);
   ShareSynchronizedFolderButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Share with...")));
   hbox.PackStart(buttonText, true, true, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   ShareSynchronizedFolderButton.Clicked +=
    new EventHandler(OnShareSynchronizedFolder);
   hbox = new HBox(false, 0);
   RemoveiFolderButton = new Button(hbox);
   vbox.PackStart(RemoveiFolderButton, false, false, 0);
   RemoveiFolderButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Revert to a Normal Folder")));
   hbox.PackStart(buttonText, true, true, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   RemoveiFolderButton.Clicked +=
    new EventHandler(RemoveiFolderHandler);
   hbox = new HBox(false, 0);
   ViewFolderPropertiesButton = new Button(hbox);
   vbox.PackStart(ViewFolderPropertiesButton, false, false, 0);
   ViewFolderPropertiesButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Properties...")));
   hbox.PackStart(buttonText, true, true, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   ViewFolderPropertiesButton.Clicked +=
    new EventHandler(OnShowFolderProperties);
   hbox = new HBox(false, 0);
   DownloadAvailableiFolderButton = new Button(hbox);
   vbox.PackStart(DownloadAvailableiFolderButton, false, false, 0);
   DownloadAvailableiFolderButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Download...")));
   hbox.PackStart(buttonText, true, true, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   DownloadAvailableiFolderButton.Clicked +=
    new EventHandler(DownloadAvailableiFolderHandler);
   hbox = new HBox(false, 0);
   MergeAvailableiFolderButton = new Button(hbox);
   vbox.PackStart(MergeAvailableiFolderButton, false, false, 0);
   MergeAvailableiFolderButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Merge")));
   hbox.PackStart(buttonText, true, true, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   MergeAvailableiFolderButton.Clicked +=
    new EventHandler(MergeAvailableiFolderHandler);
   hbox = new HBox(false, 0);
   DeleteFromServerButton = new Button(hbox);
   vbox.PackStart(DeleteFromServerButton, false, false, 0);
   DeleteFromServerButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Delete from server")));
   hbox.PackStart(buttonText, true, true, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   DeleteFromServerButton.Clicked +=
    new EventHandler(DeleteFromServerHandler);
   hbox = new HBox(false, 0);
   RemoveMembershipButton = new Button(hbox);
   vbox.PackStart(RemoveMembershipButton, false, false, 0);
   RemoveMembershipButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span>{0}</span>",
         Util.GS("Remove My Membership")));
   hbox.PackStart(buttonText, true, true, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   RemoveMembershipButton.Clicked +=
    new EventHandler(RemoveMembershipHandler);
   return actionsVBox;
  }
  private Widget CreateIconViewPane()
  {
   iFoldersScrolledWindow = new ScrolledWindow();
   iFoldersIconView = new iFolderIconView(iFoldersScrolledWindow);
   myiFoldersFilter = new TreeModelFilter(ifdata.iFolders, null);
   myiFoldersFilter.VisibleFunc = SynchronizedFoldersFilterFunc;
   updateStatusTimer =
    new Timer(new TimerCallback(UpdateLocalViewItems),
        myiFoldersFilter,
        30000,
        30000);
   localGroup = new iFolderViewGroup(Util.GS("iFolders on This Computer"), myiFoldersFilter, SearchEntry);
   iFoldersIconView.AddGroup(localGroup);
   VBox emptyVBox = new VBox(false, 0);
   emptyVBox.BorderWidth = 12;
   Table table = new Table(3, 2, false);
   emptyVBox.PackStart(table, true, true, 0);
   table.RowSpacing = 12;
   table.ColumnSpacing = 12;
   Label l = new Label(
    string.Format("<span>{0}</span>",
          Util.GS("There are no iFolders on this computer.  To set up an iFolder, do one of the following:")));
   table.Attach(l,
       0, 2,
       0, 1,
       AttachOptions.Expand | AttachOptions.Fill,
       0, 0, 0);
   l.UseMarkup = true;
   l.LineWrap = true;
   l.Xalign = 0;
   Image uploadImg = new Image(Util.ImagesPath("ifolder-upload48.png"));
   table.Attach(uploadImg,
       0, 1,
       1, 2,
       AttachOptions.Shrink | AttachOptions.Fill,
       0, 0, 0);
   l = new Label(
    string.Format("<span>{0}</span>",
          Util.GS("Select an existing folder on this computer to upload to an iFolder Server")));
   table.Attach(l,
       1, 2,
       1, 2,
       AttachOptions.Expand | AttachOptions.Fill,
       0, 0, 0);
   l.UseMarkup = true;
   l.LineWrap = true;
   l.Xalign = 0;
   Image downloadImg = new Image(Util.ImagesPath("ifolder-download48.png"));
   table.Attach(downloadImg,
       0, 1,
       2, 3,
       AttachOptions.Shrink | AttachOptions.Fill,
       0, 0, 0);
   l = new Label(
    string.Format("<span>{0}</span>",
          Util.GS("Select an iFolder on the server to download to this computer")));
   table.Attach(l,
       1, 2,
       2, 3,
       AttachOptions.Expand | AttachOptions.Fill,
       0, 0, 0);
   l.UseMarkup = true;
   l.LineWrap = true;
   l.Xalign = 0;
   localGroup.EmptyWidget = emptyVBox;
   VBox emptySearchVBox = new VBox(false, 0);
   emptySearchVBox.BorderWidth = 12;
   l = new Label(
    string.Format("<span size=\"large\">{0}</span>",
          Util.GS("No matches found")));
   emptySearchVBox.PackStart(l, true, true, 0);
   l.UseMarkup = true;
   l.LineWrap = true;
   localGroup.EmptySearchWidget = emptySearchVBox;
   TargetEntry[] targets =
    new TargetEntry[]
    {
                 new TargetEntry ("text/uri-list", 0, (uint) DragTargetType.UriList),
                 new TargetEntry ("application/x-root-window-drop", 0, (uint) DragTargetType.RootWindow),
                 new TargetEntry ("text/ifolder-id", 0, (uint) DragTargetType.iFolderID)
    };
   Drag.DestSet(iFoldersIconView,
       DestDefaults.All,
       targets,
       Gdk.DragAction.Copy | Gdk.DragAction.Move);
   iFoldersIconView.DragMotion +=
    new DragMotionHandler(OnIconViewDragMotion);
   iFoldersIconView.DragDrop +=
    new DragDropHandler(OnIconViewDragDrop);
   iFoldersIconView.DragDataReceived +=
    new DragDataReceivedHandler(OnIconViewDragDataReceived);
   DomainInformation[] domains = domainController.GetDomains();
   foreach (DomainInformation domain in domains)
   {
    AddServerGroup(domain.ID);
   }
   iFoldersIconView.SelectionChanged +=
    new EventHandler(OniFolderIconViewSelectionChanged);
   iFoldersIconView.BackgroundClicked +=
    new iFolderClickedHandler(OniFolderIconViewBackgroundClicked);
   iFoldersIconView.iFolderClicked +=
    new iFolderClickedHandler(OniFolderClicked);
   iFoldersIconView.iFolderActivated +=
    new iFolderActivatedHandler(OniFolderActivated);
   iFoldersIconView.KeyPressEvent +=
    new KeyPressEventHandler(OniFolderIconViewKeyPress);
   iFoldersScrolledWindow.AddWithViewport(iFoldersIconView);
   return iFoldersScrolledWindow;
  }
  private void UpdateLocalViewItems(object state)
  {
   GLib.Idle.Add(UpdateLocalViewItemsMainThread);
  }
  private bool UpdateLocalViewItemsMainThread()
  {
   iFolderViewItem[] viewItems = localGroup.Items;
   foreach(iFolderViewItem item in viewItems)
   {
    item.Refresh();
   }
   return false;
  }
  private void OnOpenSynchronizedFolder(object o, EventArgs args)
  {
   OpenSelectedFolder();
  }
  private void OnResolveConflicts(object o, EventArgs args)
  {
   ResolveSelectedFolderConflicts();
  }
  private void OnSynchronizeNow(object o, EventArgs args)
  {
   SyncSelectedFolder();
  }
  private void OnShareSynchronizedFolder(object o, EventArgs args)
  {
   ShareSelectedFolder();
  }
  private void ShowFolderProperties(iFolderHolder ifHolder, int desiredPage)
  {
   if (ifHolder != null)
   {
    iFolderPropertiesDialog propsDialog =
     (iFolderPropertiesDialog) PropDialogs[ifHolder.iFolder.ID];
    if (propsDialog == null)
    {
     try
     {
      propsDialog =
       new iFolderPropertiesDialog(this, ifHolder.iFolder, ifws, simws, simiasManager);
      propsDialog.SetPosition(WindowPosition.Center);
      propsDialog.Response +=
        new ResponseHandler(OnPropertiesDialogResponse);
      propsDialog.CurrentPage = desiredPage;
      propsDialog.ShowAll();
      PropDialogs[ifHolder.iFolder.ID] = propsDialog;
     }
     catch(Exception e)
     {
      if(propsDialog != null)
      {
       propsDialog.Hide();
       propsDialog.Destroy();
       propsDialog = null;
      }
      iFolderExceptionDialog ied =
       new iFolderExceptionDialog(this, e);
      ied.Run();
      ied.Hide();
      ied.Destroy();
      ied = null;
     }
    }
    else
    {
     propsDialog.Present();
     propsDialog.CurrentPage = desiredPage;
    }
   }
  }
  private void AddiFolderHandler(object o, EventArgs args)
  {
   CreateNewiFolder();
  }
  private void Migrate2xClickedHandler(object o, EventArgs args)
  {
   bool if2Present = true;
                        string str = Mono.Unix.UnixEnvironment.EffectiveUser.HomeDirectory;
                        if(!System.IO.Directory.Exists(str+"/.novell/ifolder"))
   {
    if2Present = false;
                        }
   else
   {
                         string[] dirs;
                         dirs = System.IO.Directory.GetDirectories(str+"/.novell/ifolder");
                         str = str+"/.novell/ifolder";
    int i;
                         for(i=0;i<dirs.Length;i++)
                         {
                                 if(dirs[i] != str+"/reg" && dirs[i] != str+"/Save")
                                 {
      break;
                                 }
    }
    if( i == dirs.Length)
     if2Present = false;
                        }
   if( if2Present == false)
   {
    iFolderMsgDialog NoiF2Dialog = new iFolderMsgDialog(
    null,
    iFolderMsgDialog.DialogType.Info,
    iFolderMsgDialog.ButtonSet.Ok,
    Util.GS("iFolder Migration"),
    Util.GS("There is no iFolder 2.x data present on this computer."),Util.GS(" "));
    NoiF2Dialog.Run();
    NoiF2Dialog.Hide();
    NoiF2Dialog.Destroy();
    return;
   }
   MigrationWindow migrationWindow = new MigrationWindow(this, ifws, simws);
   migrationWindow.ShowAll();
   return;
  }
  private void ExportClicked( object o, EventArgs args)
  {
   ExportKeysDialog export = new ExportKeysDialog(ifws, simws);
   export.TransientFor = this;
   int res = export.Run();
   string fileName = export.FileName;
   string domainID = export.Domain;
   export.Hide();
   export.Destroy();
   if( res == (int)ResponseType.Ok)
   {
    try
    {
     this.simws.ExportiFoldersCryptoKeys(domainID, fileName);
     iFolderMsgDialog dialog = new iFolderMsgDialog(
             null,
             iFolderMsgDialog.DialogType.Info,
             iFolderMsgDialog.ButtonSet.None,
             Util.GS("Export Encrypted Keys"), Util.GS("Successfully exported the keys."),
             Util.GS("File name:")+fileName);
     dialog.Run();
     dialog.Hide();
     dialog.Destroy();
     dialog = null;
    }
    catch(Exception ex)
    {
     iFolderMsgDialog dialog = new iFolderMsgDialog(
      null,
      iFolderMsgDialog.DialogType.Error,
      iFolderMsgDialog.ButtonSet.None,
      Util.GS("Export Encrypted Keys"),
      Util.GS(ex.Message),
      Util.GS(""));
      dialog.Run();
      dialog.Hide();
      dialog.Destroy();
      dialog = null;
    }
   }
   else if( res == (int)ResponseType.Help)
   {
    Util.ShowHelp("managingpassphrse.html", this);
   }
  }
  private void ImportClicked( object o, EventArgs args)
  {
   DomainInformation[] domains = this.domainController.GetLoggedInDomains();
   if( domains == null)
   {
    iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                                                                                null,
                                                                                                                iFolderMsgDialog.DialogType.Error,
                                                                                                                iFolderMsgDialog.ButtonSet.None,
                                                                                                                Util.GS("No Logged-In domains"),
                                                                                                                Util.GS("There are no logged-in domains for importing keys."),
                                                                                                                Util.GS("For importing keys the domain should be connected. Log on to the domain and try."));
     dialog.Run();
                                 dialog.Hide();
                                 dialog.Destroy();
                                 dialog = null;
     return;
    }
   ImportKeysDialog import = new ImportKeysDialog(ifws, simws);
   import.TransientFor = this;
   int result = import.Run();
   string fileName = import.FileName;
   string domainID = import.Domain;
   string OneTimePassphrase = import.OneTimePP;
   string NewPassphrase = import.PassPhrase;
   import.Hide();
   import.Destroy();
   if( result == (int)ResponseType.Ok)
   {
    try
    {
     this.simws.ImportiFoldersCryptoKeys( domainID, NewPassphrase, OneTimePassphrase, fileName);
     bool rememberOption = this.simws.GetRememberOption(domainID);
     simws.StorePassPhrase(domainID, "", CredentialType.None, false);
     simws.StorePassPhrase(domainID, NewPassphrase, CredentialType.Basic, rememberOption);
     iFolderMsgDialog dialog = new iFolderMsgDialog(
             null,
             iFolderMsgDialog.DialogType.Info,
             iFolderMsgDialog.ButtonSet.None,
             Util.GS("Import Decrypted Keys"),
             Util.GS("Successfully imported the keys and changed the passphrase"),
             Util.GS("Use your new passphrase from now"));
     dialog.Run();
     dialog.Hide();
     dialog.Destroy();
     dialog = null;
    }
    catch(Exception ex)
    {
     iFolderMsgDialog dialog = new iFolderMsgDialog(
      null,
      iFolderMsgDialog.DialogType.Error,
      iFolderMsgDialog.ButtonSet.None,
      Util.GS("Import Decrypted Keys"),
      Util.GS("Error importing the keys."),
      Util.GS(""));
      dialog.Run();
      dialog.Hide();
      dialog.Destroy();
      dialog = null;
    }
   }
   else if( result == (int)ResponseType.Help)
   {
    Util.ShowHelp("managingpassphrse.html", this);
   }
  }
  private void OnToggleViewServeriFoldersMenuItem(object o, EventArgs args)
  {
   if (ViewServeriFoldersMenuItem.Active)
    ShowAvailableiFolders();
   else
    HideAvailableiFolders();
  }
  private void ShowHideAllFoldersHandler(object o, EventArgs args)
  {
   if (bAvailableFoldersShowing)
    HideAvailableiFolders();
   else
    ShowAvailableiFolders();
  }
  private void RemoveiFolderHandler(object o, EventArgs args)
  {
   RemoveSelectedFolderHandler();
  }
  private void DownloadAvailableiFolderHandler(object o, EventArgs args)
  {
   DownloadSelectedFolder();
   myiFoldersFilter.Refilter();
         treeModelFilter.Refilter();
  }
  private void MergeAvailableiFolderHandler(object o, EventArgs args)
  {
   MergeSelectedFolder();
   myiFoldersFilter.Refilter();
         treeModelFilter.Refilter();
  }
  private void DeleteFromServerHandler(object o, EventArgs args)
  {
   DeleteSelectedFolderFromServer();
  }
  private void RemoveMembershipHandler(object o, EventArgs args)
  {
   RemoveMembershipFromSelectedFolder();
  }
  private void OnCancelSearchButton(object o, EventArgs args)
  {
   SearchEntry.Text = "";
   SearchEntry.GrabFocus();
  }
  private void OnSearchEntryChanged(object o, EventArgs args)
  {
   if (searchTimeoutID != 0)
   {
    GLib.Source.Remove(searchTimeoutID);
    searchTimeoutID = 0;
   }
   if (SearchEntry.Text.Length > 0)
    CancelSearchButton.Sensitive = true;
   else
    CancelSearchButton.Sensitive = false;
   searchTimeoutID = GLib.Timeout.Add(
    500, new GLib.TimeoutHandler(SearchCallback));
   CancelSearchButton.Visible = false;
  }
  private void OniFolderIconViewBackgroundClicked(object o, iFolderClickedArgs args)
  {
   iFoldersIconView.UnselectAll();
   if (args.Button == 3)
   {
    Menu menu = new Menu();
    MenuItem item_refresh =
     new MenuItem(Util.GS("Refresh"));
    menu.Append(item_refresh);
    item_refresh.Activated += new EventHandler(
     RefreshiFoldersHandler);
    menu.ShowAll();
    menu.Popup(null, null, null,
     IntPtr.Zero, 3,
     Gtk.Global.CurrentEventTime);
   }
  }
  private void OniFolderActivated(object o, iFolderActivatedArgs args)
  {
   if (args.Holder == null || args.Holder.iFolder == null) return;
   if (args.Holder.iFolder.IsSubscription)
    DownloadSelectedFolder();
   else
    OpenSelectedFolder();
  }
  private void OniFolderIconViewKeyPress(object o, KeyPressEventArgs args)
  {
   switch(args.Event.Key)
   {
    case Gdk.Key.Delete:
     iFolderHolder holder = iFoldersIconView.SelectedFolder;
     if (holder != null)
     {
      if (holder.iFolder.IsSubscription)
      {
       DomainInformation domain =
        domainController.GetDomain(holder.iFolder.DomainID);
       if ( holder.iFolder.CurrentUserID== holder.iFolder.OwnerID)
       {
        DeleteSelectedFolderFromServer();
       }
       else
       {
        RemoveMembershipFromSelectedFolder();
       }
      }
      else
      {
       RemoveSelectedFolderHandler();
      }
     }
     break;
    default:
     break;
   }
  }
  private void OniFolderClicked(object o, iFolderClickedArgs args)
  {
   iFolderHolder holder = args.Holder;
   if (holder == null) return;
   switch(args.Button)
   {
    case 3:
     Menu menu = new Menu();
     if (holder.iFolder.IsSubscription)
     {
      MenuItem item_download =
       new MenuItem(Util.GS("Download..."));
      menu.Append(item_download);
      item_download.Activated += new EventHandler(
        DownloadAvailableiFolderHandler);
      menu.Append(new SeparatorMenuItem());
      MenuItem item_merge =
       new MenuItem(Util.GS("Merge"));
      menu.Append(item_merge);
      item_merge.Activated += new EventHandler(
        MergeAvailableiFolderHandler);
      menu.Append(new SeparatorMenuItem());
      DomainInformation domain =
       domainController.GetDomain(holder.iFolder.DomainID);
      if ( holder.iFolder.CurrentUserID== holder.iFolder.OwnerID)
      {
       MenuItem item_delete = new MenuItem (
         Util.GS("Delete from Server"));
       menu.Append (item_delete);
       item_delete.Activated += new EventHandler(
         DeleteFromServerHandler);
      }
      else
      {
       MenuItem item_remove_membership = new MenuItem (
         Util.GS("Remove My Membership"));
       menu.Append (item_remove_membership);
       item_remove_membership.Activated +=
        new EventHandler(
         RemoveMembershipHandler);
      }
     }
     else
     {
      MenuItem item_open =
       new MenuItem (Util.GS("Open..."));
      menu.Append (item_open);
      item_open.Activated += new EventHandler(
        OnOpenFolderMenu);
      menu.Append(new SeparatorMenuItem());
      if(holder.iFolder.HasConflicts)
      {
       MenuItem item_resolve = new MenuItem (
         Util.GS("Resolve conflicts..."));
       menu.Append (item_resolve);
       item_resolve.Activated += new EventHandler(
        OnResolveConflicts);
       menu.Append(new SeparatorMenuItem());
      }
      MenuItem item_sync =
       new MenuItem(Util.GS("Synchronize Now"));
      menu.Append (item_sync);
      item_sync.Activated += new EventHandler(
        OnSynchronizeNow);
      MenuItem item_share =
       new MenuItem (Util.GS("Share with..."));
      menu.Append (item_share);
      item_share.Activated += new EventHandler(
        OnShareSynchronizedFolder);
      if (!holder.iFolder.Role.Equals("Master"))
      {
       MenuItem item_revert = new MenuItem (
         Util.GS("Revert to a Normal Folder"));
          menu.Append (item_revert);
       if (false == this.RemoveiFolderButton.Sensitive)
       {
           item_revert.Sensitive = false;
       }
       else
       {
           item_revert.Activated += new EventHandler(
         RemoveiFolderHandler);
       }
      }
      else if (holder.iFolder.OwnerID !=
          holder.iFolder.CurrentUserID)
      {
       MenuItem item_delete = new MenuItem (
         Util.GS("Revert to a normal folder"));
       menu.Append (item_delete);
       if (false == this.RemoveiFolderButton.Sensitive)
                            {
                                 item_delete.Sensitive = false;
                            }
                            else
       {
            item_delete.Activated += new EventHandler(
         RemoveiFolderHandler);
       }
      }
      menu.Append(new SeparatorMenuItem());
      MenuItem item_properties =
       new MenuItem (Util.GS("Properties"));
      menu.Append (item_properties);
      item_properties.Activated +=
       new EventHandler(OnShowFolderProperties);
      if (holder.State == iFolderState.Initial && holder.iFolder.State == "Available")
      {
              item_share.Sensitive = false;
       item_properties.Sensitive = false;
      }
      else
      {
              item_share.Sensitive = true;
       item_properties.Sensitive = true;
      }
     }
     menu.ShowAll();
     menu.Popup(null, null, null,
      IntPtr.Zero, 3,
      Gtk.Global.CurrentEventTime);
     break;
    default:
     break;
   }
  }
  private void OniFolderIconViewSelectionChanged(object o, EventArgs args)
  {
   UpdateSensitivity();
  }
  private void OnIconViewDragMotion(object o, DragMotionArgs args)
  {
   Gdk.Drag.Status(args.Context, args.Context.SuggestedAction, args.Time);
   args.RetVal = true;
  }
  private void OnIconViewDragDrop(object o, DragDropArgs args)
  {
   args.RetVal = true;
  }
  private void OnIconViewDragDataReceived(object o, DragDataReceivedArgs args)
  {
   bool bFolderCreated = false;
   switch (args.Info)
   {
    case (uint) DragTargetType.iFolderID:
     string ifolderID =
      System.Text.Encoding.UTF8.GetString(args.SelectionData.Data);
     iFolderHolder holder = ifdata.GetAvailableiFolder(ifolderID);
     if (holder != null)
      DownloadiFolder(holder, true);
     break;
    case (uint) DragTargetType.UriList:
     UriList uriList = new UriList(args.SelectionData);
     if (uriList.Count == 1)
     {
      string path = null;
      try
      {
       path = uriList.ToLocalPaths()[0];
      }
      catch
      {
       return;
      }
      DomainInformation[] domains = domainController.GetDomains();
      if (domains.Length <= 0) return;
      string domainID = domains[0].ID;
      DomainInformation defaultDomain = domainController.GetDefaultDomain();
      if (defaultDomain != null)
       domainID = defaultDomain.ID;
      DragCreateDialog cd = new DragCreateDialog(this, domains, domainID, path, ifws);
      cd.TransientFor = this;
      int rc = 0;
      do
      {
       rc = cd.Run();
       cd.Hide();
       if (rc == (int)ResponseType.Ok)
       {
        try
        {
         string selectedFolder = cd.iFolderPath.Trim();
         string selectedDomain = cd.DomainID;
         bool SSL = cd.ssl;
         string algorithm = cd.EncryptionAlgorithm;
         string parentDir = System.IO.Path.GetDirectoryName( selectedFolder );
         if ( ( parentDir == null ) || ( parentDir == String.Empty ) )
         {
          iFolderMsgDialog dg = new iFolderMsgDialog(
           this,
           iFolderMsgDialog.DialogType.Warning,
           iFolderMsgDialog.ButtonSet.Ok,
           "",
           Util.GS("Invalid folder specified"),
           Util.GS("An invalid folder was specified"));
          dg.Run();
          dg.Hide();
          dg.Destroy();
          continue;
         }
         iFolderHolder ifHolder = null;
         try
         {
          if( algorithm != null)
          {
           bool passPhraseStatus = false;
           bool passphraseStatus = false;
           try
           {
                                                      passphraseStatus = simws.IsPassPhraseSet(selectedDomain);
           }
           catch(Exception ex)
           {
            DisplayLoginMesg();
            continue;
           }
           if(passphraseStatus == true)
           {
            string passphrasecheck = simws.GetPassPhrase(selectedDomain);
            if( passphrasecheck == null || passphrasecheck =="")
            {
             Debug.PrintLine(" passphrase not entered at login");
             passPhraseStatus = ShowVerifyDialog(selectedDomain, simws);
            }
            else
            {
             passPhraseStatus = true;
            }
           }
           else
           {
            passPhraseStatus = ShowEnterPassPhraseDialog(selectedDomain, simws);
           }
           if( passPhraseStatus == false)
           {
            continue;
           }
          }
          ifHolder = ifdata.CreateiFolder(selectedFolder, selectedDomain, SSL, algorithm);
         }
         catch(Exception e)
         {
          if (DisplayCreateOrSetupException(e))
          {
           continue;
          }
         }
         if(ifHolder == null)
          throw new Exception("Simias returned null");
         rc = 0;
         Util.LastCreatedPath = ifHolder.iFolder.UnManagedPath;
         if ((bool)ClientConfig.Get(ClientConfig.KEY_SHOW_CREATION))
         {
          iFolderCreationDialog dlg =
           new iFolderCreationDialog(ifHolder.iFolder);
          dlg.TransientFor = this;
          int createRC;
          do
          {
           createRC = dlg.Run();
           if(createRC == (int)Gtk.ResponseType.Help)
           {
            Util.ShowHelp("myifolders.html", this);
           }
          }while(createRC != (int)Gtk.ResponseType.Ok);
          dlg.Hide();
          if(dlg.HideDialog)
          {
           ClientConfig.Set(
            ClientConfig.KEY_SHOW_CREATION, false);
          }
          cd.Destroy();
          cd = null;
         }
        }
        catch (Exception e)
        {
         Debug.PrintLine(e.Message);
         continue;
        }
       }
      }
      while(rc == (int)ResponseType.Ok);
     }
     break;
    default:
     break;
   }
   Gtk.Drag.Finish (args.Context, bFolderCreated, false, args.Time);
  }
  private void OnRealizeWidget(object o, EventArgs args)
  {
    ViewServeriFoldersMenuItem.Active = true;
    ShowAvailableiFolders();
    UpdateLocalViewItems(null);
   OniFolderIconViewSelectionChanged(null, EventArgs.Empty);
  }
  private void RefreshiFoldersHandler(object o, EventArgs args)
  {
   RefreshiFolders(true);
  }
  private void AccountsMenuItemHandler(object o, EventArgs args)
  {
   Util.ShowPrefsPage(1, simiasManager);
  }
  private void SyncLogMenuItemHandler(object o, EventArgs args)
  {
   Util.ShowLogWindow(simiasManager);
  }
  private void ShowColorPalette(object o, EventArgs args)
  {
   ColorPaletteDialog palette = new ColorPaletteDialog();
   palette.Run();
   palette.Hide();
   palette.Destroy();
  }
  private void CloseEventHandler(object o, EventArgs args)
  {
   CloseWindow();
  }
  private void QuitEventHandler(object o, EventArgs args)
  {
   Util.QuitiFolder();
  }
  private void ShowPreferencesHandler(object o, EventArgs args)
  {
   Util.ShowPrefsPage(0, simiasManager);
  }
  private void OnOpenFolderMenu(object o, EventArgs args)
  {
   OpenSelectedFolder();
  }
  private void OnShowFolderProperties(object o, EventArgs args)
  {
   ShowSelectedFolderProperties();
  }
  private void OnPropertiesDialogResponse(object o, ResponseArgs args)
  {
   iFolderPropertiesDialog propsDialog = (iFolderPropertiesDialog) o;
   switch(args.ResponseId)
   {
    case Gtk.ResponseType.Help:
     if (propsDialog != null)
     {
      if (propsDialog.CurrentPage == 0)
      {
       Util.ShowHelp("propifolders.html", this);
      }
      else if (propsDialog.CurrentPage == 1)
      {
       Util.ShowHelp("sharewith.html", this);
      }
      else
      {
       Util.ShowHelp(Util.HelpMainPage, this);
      }
     }
     break;
    default:
    {
     if(propsDialog != null)
     {
      propsDialog.Hide();
      propsDialog.Destroy();
      if (PropDialogs.ContainsKey(propsDialog.iFolder.ID))
       PropDialogs.Remove(propsDialog.iFolder.ID);
      propsDialog = null;
     }
     break;
    }
   }
  }
  private void OnConflictDialogResponse(object o, ResponseArgs args)
  {
   iFolderConflictDialog conflictDialog = (iFolderConflictDialog) o;
   if (args.ResponseId == ResponseType.Help)
    Util.ShowHelp("conflicts.html", this);
   else
   {
    if (conflictDialog != null)
    {
     conflictDialog.Hide();
     conflictDialog.Destroy();
     if (ConflictDialogs.ContainsKey(conflictDialog.iFolder.ID))
      ConflictDialogs.Remove(conflictDialog.iFolder.ID);
     conflictDialog = null;
    }
   }
  }
  private void OnHelpMenuItem(object o, EventArgs args)
  {
   Util.ShowHelp(Util.HelpMainPage, this);
  }
  private void OnRecoveryMenuItem(object o, EventArgs args)
  {
   Util.ShowHelp(Util.HelpMainPage, this);
  }
  private void OnResetPasswordMenuItem(object o, EventArgs args)
  {
   string DomainID, oldPassword, newPassword;
   bool rememberOption;
   bool status = false;
   int result =0;
   do
   {
    DomainInformation[] domains = this.domainController.GetLoggedInDomains();
    if( domains == null)
    {
     iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                                                                                null,
                                                                                                                iFolderMsgDialog.DialogType.Error,
                                                                                                                iFolderMsgDialog.ButtonSet.None,
                                                                                                                Util.GS("No Logged-In domains"),
                                                                                                                Util.GS("There are no logged-in domains for changing the password."),
                                                                                                                Util.GS("For changing password the domain should be connected. Log on to the domain and try."));
     dialog.Run();
                                 dialog.Hide();
                                 dialog.Destroy();
                                 dialog = null;
     return;
    }
    ResetPasswordDialog resetDialog = new ResetPasswordDialog(simws,ifws);
    resetDialog.Domains = domains;
    resetDialog.TransientFor = this;
    result = resetDialog.Run();
    DomainID = resetDialog.Domain;
    newPassword = resetDialog.NewPassword;
    rememberOption = resetDialog.SavePassword;
    status = resetDialog.Status;
    resetDialog.Hide();
    resetDialog.Destroy();
    if(result == (int)ResponseType.Cancel || result == (int)ResponseType.DeleteEvent)
     return;
    if (result == (int)ResponseType.Help)
    {
    }
    else if( status == true)
    {
     if( rememberOption == true)
     {
      simws.SetDomainCredentials(DomainID, newPassword, CredentialType.Basic);
     }
     else
     {
      simws.SetDomainCredentials(DomainID, null, CredentialType.None);
     }
     iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                                                                                null,
                                                                                                                iFolderMsgDialog.DialogType.Info,
                                                                                                                iFolderMsgDialog.ButtonSet.None,
                                                                                                                Util.GS("Change password"),
                                                                                                                Util.GS("Successfully changed the password."), null);
                                        dialog.Run();
                                        dialog.Hide();
                                        dialog.Destroy();
                                        dialog = null;
    }
   }while(status == false);
  }
  private void OnResetPassMenuItem(object o, EventArgs args)
  {
   string DomainID, oldPassphrase, newPassphrase, RAName, publicKey;
   bool rememberOption;
   bool status = false;
   int result =0;
   do
   {
    DomainInformation[] domains = this.domainController.GetLoggedInDomains();
    if( domains == null)
    {
     iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                                                                                null,
                                                                                                                iFolderMsgDialog.DialogType.Error,
                                                                                                                iFolderMsgDialog.ButtonSet.None,
                                                                                                                Util.GS("No Logged-In domains"),
                                                                                                                Util.GS("There are no logged-in domains for changing the passphrase."),
                                                                                                                Util.GS("For changing passphrase the domain should be connected. Log on to the domain and try."));
     dialog.Run();
                                 dialog.Hide();
                                 dialog.Destroy();
                                 dialog = null;
     return;
    }
    ResetPassPhraseDialog resetDialog = new ResetPassPhraseDialog(simws,ifws);
    resetDialog.Domains = domains;
    resetDialog.TransientFor = this;
    result = resetDialog.Run();
    DomainID = resetDialog.Domain;
    oldPassphrase = resetDialog.OldPassphrase;
    newPassphrase = resetDialog.NewPassphrase;
    rememberOption = resetDialog.SavePassphrase;
    status = resetDialog.Status;
    resetDialog.Hide();
    resetDialog.Destroy();
    if(result == (int)ResponseType.Cancel || result == (int)ResponseType.DeleteEvent)
     return;
    if (result == (int)ResponseType.Help)
    {
                   Util.ShowHelp("managingpassphrse.html", this);
    }
    else if( status == true)
    {
     iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                                                                                null,
                                                                                                                iFolderMsgDialog.DialogType.Info,
                                                                                                                iFolderMsgDialog.ButtonSet.None,
                                                                                                                Util.GS("Reset Passphrase"),
                                                                                                                Util.GS("Successfully changed the passphrase"), null);
                                 dialog.Run();
                                 dialog.Hide();
                                 dialog.Destroy();
                                 dialog = null;
    }
   }while(status == false);
  }
  private void OnAbout(object o, EventArgs args)
  {
   Util.ShowAbout();
  }
  private void OnDomainAddedEvent(object sender, DomainEventArgs args)
  {
   RefreshiFolders(true);
    ShowAvailableiFolders();
   AddServerGroup(args.DomainID);
   RefilterServerGroups();
  }
  private void OnDomainDeletedEvent(object sender, DomainEventArgs args)
  {
   RefreshiFolders(true);
   if (serverGroups.ContainsKey(args.DomainID))
   {
    iFolderViewGroup group =
     (iFolderViewGroup)serverGroups[args.DomainID];
    iFoldersIconView.RemoveGroup(group);
    serverGroups.Remove(args.DomainID);
    group.Dispose();
    group = null;
   }
   if (serverGroupFilters.ContainsKey(args.DomainID))
   {
    serverGroupFilters.Remove(args.DomainID);
   }
   iFoldersIconView.UnselectAll();
   RefilterServerGroups();
  }
  private void OnDomainLoggedInEvent(object sender, DomainEventArgs args)
  {
   iFolderViewGroup group = (iFolderViewGroup)serverGroups[args.DomainID];
   if(null == group)
   {
    AddServerGroup( args.DomainID );
    group = (iFolderViewGroup)serverGroups[args.DomainID];
   }
   group.VisibleWhenEmpty = true;
   iFolderViewItem[] viewItems = localGroup.Items;
   foreach(iFolderViewItem item in viewItems)
   {
    iFolderHolder holder = item.Holder;
    if (args.DomainID == holder.iFolder.DomainID)
     holder.State = iFolderState.Initial;
   }
   GLib.Idle.Add(UpdateLocalViewItemsMainThread);
            RefreshiFolders(true);
  }
  private void OnDomainLoggedOutEvent(object sender, DomainEventArgs args)
  {
   iFolderViewGroup group = (iFolderViewGroup)serverGroups[args.DomainID];
            group.VisibleWhenEmpty = false;
   iFolderViewItem[] viewItems = localGroup.Items;
   foreach(iFolderViewItem item in viewItems)
   {
    iFolderHolder holder = item.Holder;
    if (args.DomainID == holder.iFolder.DomainID)
     holder.State = iFolderState.Disconnected;
   }
   GLib.Idle.Add(UpdateLocalViewItemsMainThread);
  }
  private void ShowAvailableiFolders()
  {
   foreach(iFolderViewGroup group in serverGroups.Values)
   {
                                if( ViewServeriFoldersMenuItem.Active == false && group.IsEmpty == true )
                                        iFoldersIconView.RemoveGroup(group);
                                else
                                        iFoldersIconView.AddGroup(group);
   }
   ShowHideAllFoldersButtonText.Markup =
    string.Format("<span>{0}</span>",
         Util.GS("Hide available iFolders"));
   bAvailableFoldersShowing = true;
   ViewServeriFoldersMenuItem.Toggled -= new EventHandler(OnToggleViewServeriFoldersMenuItem);
   ViewServeriFoldersMenuItem.Active = true;
   ViewServeriFoldersMenuItem.Toggled += new EventHandler(OnToggleViewServeriFoldersMenuItem);
  }
  private void HideAvailableiFolders()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   if (holder != null && holder.iFolder.IsSubscription)
    iFoldersIconView.UnselectAll();
   foreach(iFolderViewGroup group in serverGroups.Values)
   {
    iFoldersIconView.RemoveGroup(group);
   }
    ShowHideAllFoldersButtonText.Markup =
    string.Format("<span>{0}</span>",
         Util.GS("View available iFolders"));
    bAvailableFoldersShowing = false;
    ViewServeriFoldersMenuItem.Toggled -= new EventHandler(OnToggleViewServeriFoldersMenuItem);
    ViewServeriFoldersMenuItem.Active = false;
    ViewServeriFoldersMenuItem.Toggled += new EventHandler(OnToggleViewServeriFoldersMenuItem);
  }
  private void DownloadSelectedFolder()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   DownloadiFolder(holder, false);
  }
  private void MergeSelectedFolder()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   DownloadiFolder(holder, true);
  }
  private bool MergeWithLocaliFolder(iFolderHolder holder, out bool cancelled)
  {
   bool status = false;
   cancelled = false;
   try
   {
    MigrationDialog migrationWindow = new MigrationDialog(this, ifws, simws, true, holder.iFolder.Name);
    migrationWindow.ShowAll();
    int res = migrationWindow.Run();
    String path = migrationWindow.MergePath;
    string UName = migrationWindow.UserName;
    migrationWindow.Hide();
    migrationWindow.Dispose();
    if( res == (int)ResponseType.Cancel )
    {
     cancelled = true;
    }
    if( res == (int)ResponseType.Ok && !cancelled && path != null)
    {
     ifdata.AcceptiFolderInvitation(holder.iFolder.ID, holder.iFolder.DomainID, path, true );
     MigrationDialog.RemoveFromRegistry( UName );
     status = true;
    }
    else if( path != null)
     cancelled = true;
   }
   catch(Exception e)
   {
    DisplayCreateOrSetupException(e);
   }
   return status;
  }
  private void DownloadiFolder(iFolderHolder holder, bool merge)
  {
   if( holder.iFolder.encryptionAlgorithm != null && holder.iFolder.encryptionAlgorithm != "")
   {
    if( IsPassPhraseAvailable(holder.iFolder.DomainID) == false)
    {
     return;
    }
   }
   if (holder != null && holder.iFolder.IsSubscription)
   {
    string newPath = "";
    int rc = 0;
     if( holder.iFolder.MigratediFolder > 0 )
     {
      if( MigrationDialog.CanBeMigrated(holder.iFolder.Name) )
      {
       iFolderMsgDialog dlg = new iFolderMsgDialog( null, iFolderMsgDialog.DialogType.Info, iFolderMsgDialog.ButtonSet.OkCancel,
                                                                                                Util.GS("Migration Alert"), Util.GS("The iFolder you have selected for downloading is a migrated iFolder.") , Util.GS("There are 2.x iFolders on your local machine. Do you want to merge this iFolder with any of the 2.x iFolders?") );
       int res = dlg.Run();
       dlg.Hide();
       dlg.Destroy();
       if( res == (int)ResponseType.Ok)
       {
        bool cancelled = false;
        do
        {
         bool result = MergeWithLocaliFolder( holder, out cancelled);
         if( result == true || cancelled == true)
          return;
        }while(cancelled == false);
       }
      }
     }
    do
    {
     iFolderAcceptDialog iad =
             new iFolderAcceptDialog(holder.iFolder, Util.LastSetupPath, merge);
     iad.TransientFor = this;
     rc = iad.Run();
     newPath = ParseAndReplaceTildeInPath(iad.Path);
     iad.Hide();
     iad.Destroy();
     if(rc != -5)
         return;
     try
     {
      if(merge == true)
      {
       String folderPath = newPath;
       if( System.IO.Path.GetFileName(folderPath) != holder.iFolder.Name)
       {
        throw new Exception("FolderDoesNotExist");
       }
       if( ifws.IsiFolder(folderPath))
       {
        throw new Exception("AtOrInsideCollectionPath");
       }
      }
      if( merge )
      {
       ifdata.AcceptiFolderInvitation(
           holder.iFolder.ID,
           holder.iFolder.DomainID,
           newPath,
           merge);
      }
      else
      {
       string downloadpath = newPath;
       System.IO.DirectoryInfo dir = new System.IO.DirectoryInfo(downloadpath);
                                          if(dir.Name == holder.iFolder.Name)
       {
                                                  downloadpath = System.IO.Directory.GetParent(newPath).ToString();
        dir = new System.IO.DirectoryInfo(downloadpath);
       }
                                          if( System.IO.Directory.Exists( downloadpath+"/"+holder.iFolder.Name))
                                          {
                                                  iFolderMsgDialog DownloadMergeDialog = new iFolderMsgDialog(
                                                  null,
                                                  iFolderMsgDialog.DialogType.Info,
                                                  iFolderMsgDialog.ButtonSet.OkCancel,
                                                  Util.GS("A folder with the same name already exists."),
                                                  string.Format(Util.GS("Click Ok to merge the folder or Cancel to select a different location")),null);
                                                  int rc1 = DownloadMergeDialog.Run();
                                                  DownloadMergeDialog.Hide();
                                                  DownloadMergeDialog.Destroy();
                                                  if ((ResponseType)rc1 == ResponseType.Ok)
                                           {
                                                          ifdata.AcceptiFolderInvitation( holder.iFolder.ID, holder.iFolder.DomainID, System.IO.Path.Combine(downloadpath,holder.iFolder.Name),true);
                                                  }
        else
         continue;
       }
       else
                                                  ifdata.AcceptiFolderInvitation( holder.iFolder.ID, holder.iFolder.DomainID, downloadpath);
      }
      iFoldersIconView.UnselectAll();
      rc = 0;
      Util.LastSetupPath = newPath;
     }
     catch(Exception e)
     {
         DisplayCreateOrSetupException(e);
     }
    }while(rc == -5);
   }
  }
  private void DeleteSelectedFolderFromServer()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   if (holder != null && holder.iFolder.IsSubscription)
   {
    int rc = 0;
    rc = AskDeleteiFolder(holder);
    if(rc != -8)
     return;
    try
    {
     bool removeDefault = false;
     string domainID = holder.iFolder.DomainID;
     if( simws.GetDefaultiFolder( domainID ) == holder.iFolder.ID)
     {
      Debug.PrintLine("Removing default iFolder");
      removeDefault = true;
     }
     if( removeDefault == true )
     {
      Debug.PrintLine("Removing default ifolder");
      simws.DefaultAccount(domainID, null);
     }
     else
      Debug.PrintLine("Not a default account");
     ifdata.DeleteiFolder(holder.iFolder.ID);
     iFoldersIconView.UnselectAll();
    }
    catch(Exception e)
    {
     iFolderExceptionDialog ied =
      new iFolderExceptionDialog(
       this,
       e);
     ied.Run();
     ied.Hide();
     ied.Destroy();
     return;
    }
   }
  }
  private void RemoveMembershipFromSelectedFolder()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   if (holder != null && holder.iFolder.IsSubscription)
   {
    int rc = 0;
    rc = AskRemoveMembership(holder);
    if(rc != -8)
     return;
    try
    {
     ifdata.DeleteiFolder(holder.iFolder.ID);
     iFoldersIconView.UnselectAll();
    }
    catch(Exception e)
    {
     iFolderExceptionDialog ied =
      new iFolderExceptionDialog(
       this,
       e);
     ied.Run();
     ied.Hide();
     ied.Destroy();
     return;
    }
   }
  }
  private bool SearchCallback()
  {
   SearchFolders();
   return false;
  }
  private void SearchFolders()
  {
   RefilterServerGroups();
  }
  private void RefilterServerGroups()
  {
   myiFoldersFilter.Refilter();
   foreach(iFolderViewGroup group in serverGroups.Values)
   {
    group.Model.Refilter();
   }
  }
  private void AddServerGroup(string domainID)
  {
   if (serverGroups.ContainsKey(domainID)) return;
   DomainInformation domain = domainController.GetDomain(domainID);
   if (domain == null) return;
   iFolderServerFilter serverFilter =
    new iFolderServerFilter(domainID, SearchEntry);
   treeModelFilter = new TreeModelFilter(ifdata.iFolders, null);
   treeModelFilter.VisibleFunc = serverFilter.FilterFunc;
   iFolderViewGroup group =
    new iFolderViewGroup(
     string.Format(
      Util.GS("iFolders on {0}"),
      domain.Name + " - " + domain.Host),
     treeModelFilter,
     SearchEntry);
   serverGroups[domainID] = group;
   serverGroupFilters[domainID] = serverFilter;
   group.VisibleWhenEmpty = false;
   if (bAvailableFoldersShowing)
    iFoldersIconView.AddGroup(group);
  }
       private void UpdateQoutaData(DomainInformation domain)
    {
          string str,str1 = null;
    iFolderViewGroup ifGrp = null;
    if (domain == null) return;
       try
    {
     str = string.Format(Util.GS("Disk Space Available: {0}"), CalculateDiskQouta(domain.MemberUserID) );
     str1 = string.Format(Util.GS("Disk Space Used: {0}"), CalculateDiskUsed(domain.MemberUserID)) ;
     str = str + "            " + str1;
     ifGrp = (iFolderViewGroup)serverGroups[domain.ID];
     if(ifGrp != null)
     {
         ifGrp.QoutaLabel.Markup = string.Format("<span size=\"small\">{0}</span>", GLib.Markup.EscapeText(str));
     }
    }
    catch
    {
    }
    }
  public string CalculateDiskUsed (string domainMemberUserID)
  {
   string QuotaUsedLabel = null;
                        double tmpValue=0;
                        DiskSpace ds = null;
                        ds = ifws.GetUserDiskSpace(domainMemberUserID);
   if(ds.UsedSpace == 0)
                  {
                                QuotaUsedLabel = Util.GS("N/A");
                        }
                        else
                        {
                                tmpValue = (int)(ds.UsedSpace / (1024 * 1024)) + 1;
                                QuotaUsedLabel =
                                      string.Format("{0} {1}", tmpValue, Util.GS("MB"));
                        }
   return QuotaUsedLabel;
  }
  public string CalculateDiskQouta (string domainMemberUserID)
  {
   string QoutaAvailable = null;
   double temp=0;
   DiskSpace ds = null;
   ds = ifws.GetUserDiskSpace(domainMemberUserID);
   if(ds.AvailableSpace == 0)
   {
    QoutaAvailable = string.Format("{0} {1}",Util.GS("0"), Util.GS ("MB"));
   }
   else
   {
    temp = (double)Math.Round(((decimal)ds.AvailableSpace / (1024 * 1024)),2);
       QoutaAvailable = string.Format("{0} {1}", temp, Util.GS("MB"));
   }
   if (ds.Limit == 0)
   {
               QoutaAvailable = Util.GS("Unlimited");
   }
            return QoutaAvailable;
  }
  private bool SynchronizedFoldersFilterFunc(TreeModel model, TreeIter iter)
  {
   ListStore ifolderListStore = model as ListStore;
   if (!ifolderListStore.IterIsValid(iter))
    return false;
   iFolderHolder ifHolder = (iFolderHolder)ifolderListStore.GetValue(iter, 0);
   if (ifHolder != null && ifHolder.iFolder != null && !ifHolder.iFolder.IsSubscription)
   {
    string searchString = SearchEntry.Text;
    if (searchString != null)
    {
     searchString = searchString.Trim();
     if (searchString.Length > 0)
      searchString = searchString.ToLower();
    }
    if (searchString == null || searchString.Trim().Length == 0)
     return true;
    else
    {
     string name = ifHolder.iFolder.Name;
     if (name != null)
     {
      name = name.ToLower();
      if (name.IndexOf(searchString) >= 0)
       return true;
     }
    }
   }
   return false;
  }
  public void UpdateSensitivity()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   UpdateActionsSensitivity(holder);
   UpdateMenuSensitivity(holder);
  }
  private void UpdateActionsSensitivity(iFolderHolder holder)
  {
   if (holder == null)
   {
    SynchronizedFolderTasks.Visible = false;
   }
   else
   {
    if (holder.iFolder.IsSubscription)
    {
     OpenSynchronizedFolderButton.Visible = false;
     SynchronizeNowButton.Visible = false;
     ShareSynchronizedFolderButton.Visible = false;
     ViewFolderPropertiesButton.Visible = false;
     ResolveConflictsButton.Visible = false;
     RemoveiFolderButton.Visible = false;
     DownloadAvailableiFolderButton.Visible = true;
     MergeAvailableiFolderButton.Visible = true;
     DomainInformation domain =
      domainController.GetDomain(holder.iFolder.DomainID);
     if (domain == null ||
      domain.MemberUserID == holder.iFolder.OwnerID)
     {
      DeleteFromServerButton.Visible = true;
      RemoveMembershipButton.Visible = false;
     }
     else
     {
      DeleteFromServerButton.Visible = false;
      RemoveMembershipButton.Visible = true;
     }
    }
    else
    {
     DownloadAvailableiFolderButton.Visible = false;
     MergeAvailableiFolderButton.Visible = false;
     DeleteFromServerButton.Visible = false;
     RemoveMembershipButton.Visible = false;
     OpenSynchronizedFolderButton.Visible = true;
     SynchronizeNowButton.Visible = true;
     if (holder.State == iFolderState.Initial && holder.iFolder.State == "Available")
     {
             ShareSynchronizedFolderButton.Visible = false;
             ViewFolderPropertiesButton.Visible = false;
     }
     else
     {
             ShareSynchronizedFolderButton.Visible = true;
             ViewFolderPropertiesButton.Visible = true;
     }
     RemoveiFolderButton.Visible = true;
     if (holder.iFolder.HasConflicts)
      ResolveConflictsButton.Visible = true;
     else
      ResolveConflictsButton.Visible = false;
    }
    SynchronizedFolderTasks.Visible = true;
    if( ( holder.iFolder.State == "WaitSync" )
     && (holder.State == iFolderState.Synchronizing) )
    {
       RemoveiFolderButton.Sensitive = false;
    }
    else
    {
      RemoveiFolderButton.Sensitive = true;
    }
   }
  }
  private void UpdateMenuSensitivity(iFolderHolder holder)
  {
   if (holder != null)
   {
           if((holder.iFolder != null) && (holder.State == iFolderState.Initial))
     {
            ShareMenuItem.Sensitive = false;
     OpenMenuItem.Sensitive = false;
     SyncNowMenuItem.Sensitive = false;
     ConflictMenuItem.Sensitive = false;
     RevertMenuItem.Sensitive = false;
     DeleteMenuItem.Sensitive = false;
     RemoveMenuItem.Sensitive = false;
     RemoveMenuItem.Visible = false;
     PropMenuItem.Sensitive = false;
     DownloadMenuItem.Sensitive = false;
     MergeMenuItem.Sensitive = false;
     }
    if( (holder.iFolder != null) &&
         (holder.iFolder.HasConflicts) )
    {
     ConflictMenuItem.Sensitive = true;
    }
    else
    {
     ConflictMenuItem.Sensitive = false;
    }
    if(!holder.iFolder.IsSubscription)
    {
     DownloadMenuItem.Sensitive = false;
     MergeMenuItem.Sensitive = false;
     if (holder.State == iFolderState.Initial && holder.iFolder.State == "Available")
     {
             ShareMenuItem.Sensitive = false;
      PropMenuItem.Sensitive = false;
     }
     else
     {
             ShareMenuItem.Sensitive = true;
      PropMenuItem.Sensitive = true;
     }
     OpenMenuItem.Sensitive = true;
     SyncNowMenuItem.Sensitive = true;
     if ( (holder.iFolder.Role.Equals("Master"))
       || ( ( holder.iFolder.State == "WaitSync" )
       && (holder.State == iFolderState.Synchronizing) ))
     {
      RevertMenuItem.Sensitive = false;
     }
     else
     {
      RevertMenuItem.Sensitive = true;
     }
     PropMenuItem.Sensitive = true;
     DeleteMenuItem.Sensitive = false;
     RemoveMenuItem.Sensitive = false;
    }
    else
    {
     DownloadMenuItem.Sensitive = true;
     MergeMenuItem.Sensitive = true;
     ShareMenuItem.Sensitive = false;
     OpenMenuItem.Sensitive = false;
     SyncNowMenuItem.Sensitive = false;
     RevertMenuItem.Sensitive = false;
     PropMenuItem.Sensitive = false;
     DomainInformation domain =
      domainController.GetDomain(holder.iFolder.DomainID);
     if (domain == null ||
      domain.MemberUserID == holder.iFolder.OwnerID)
     {
      DeleteMenuItem.Sensitive = true;
      DeleteMenuItem.Visible = true;
      RemoveMenuItem.Sensitive = false;
      RemoveMenuItem.Visible = false;
     }
     else
     {
      DeleteMenuItem.Sensitive = false;
      DeleteMenuItem.Visible = false;
      RemoveMenuItem.Sensitive = true;
      RemoveMenuItem.Visible = true;
     }
    }
   }
   else
   {
    ShareMenuItem.Sensitive = false;
    OpenMenuItem.Sensitive = false;
    SyncNowMenuItem.Sensitive = false;
                                               SyncNowMenuItem.Sensitive = false;
    ConflictMenuItem.Sensitive = false;
    RevertMenuItem.Sensitive = false;
    DeleteMenuItem.Sensitive = false;
    RemoveMenuItem.Sensitive = false;
    RemoveMenuItem.Visible = false;
    PropMenuItem.Sensitive = false;
    DownloadMenuItem.Sensitive = false;
    MergeMenuItem.Sensitive = false;
   }
  }
  private void RefreshiFolders(bool bReadFromSimias)
  {
   ifdata.Refresh();
   this.RefreshAvailableiFolderTimer.Change(300000, 300000);
  }
  private void RefreshAvailableiFolderTimer_click(object sender)
  {
   RefreshiFolders(true);
  }
  public void CloseWindow()
  {
   int x;
   int y;
   this.GetPosition(out x, out y);
   lastXPos = x;
   lastYPos = y;
   this.Hide();
  }
  private void UpdateStatus(string message)
  {
   MainStatusBar.Pop (ctx);
   MainStatusBar.Push (ctx, message);
  }
  private void OpenSelectedFolder()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   if (holder != null)
   {
    try
    {
     Util.OpenInBrowser(holder.iFolder.UnManagedPath);
    }
    catch(Exception e)
    {
     iFolderMsgDialog dg = new iFolderMsgDialog(
      this,
      iFolderMsgDialog.DialogType.Error,
      iFolderMsgDialog.ButtonSet.Ok,
      "",
      string.Format(Util.GS("Unable to open folder \"{0}\""), holder.iFolder.Name),
      Util.GS("iFolder could not open the Nautilus File Manager or the Konquerer File Manager."));
     dg.Run();
     dg.Hide();
     dg.Destroy();
    }
   }
  }
  private void ResolveSelectedFolderConflicts()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   if (holder != null)
    ResolveConflicts(holder);
  }
        public static bool AdvancedConflictResolver( Gtk.Window parent, iFolderWeb ifolder,
                                                     iFolderWebService iFolderWS, SimiasWebService SimiasWS )
        {
            bool status = false;
            const string assemblyName = "plugins/EnhancedConflictResolution";
            const string conflictClass = "Novell.EnhancedConflictResolution.iFolderEnhancedConflictDialog";
            const string conflictShowAllMethod = "ShowAll";
            const string conflictSetPosMethod = "SetPosition";
            System.Object[] args = new System.Object[4];
            System.Object[] setPosArgs = new System.Object[1];
            if( assemblyName != null )
            {
                try
                {
                    Assembly idAssembly = Assembly.Load( assemblyName );
                    if ( idAssembly != null )
                    {
                        Type type = idAssembly.GetType( conflictClass );
                        if( null != type )
                        {
                            args[0] = parent;
                            args[1] = ifolder;
                            args[2] = iFolderWS;
                            args[3] = SimiasWS;
                            System.Object enhancedConflictObj = Activator.CreateInstance(type,args);
                            MethodInfo method = type.GetMethod( conflictSetPosMethod );
                            setPosArgs[0] = WindowPosition.Center;
                            method.Invoke( enhancedConflictObj, setPosArgs );
                            method = type.GetMethod( conflictShowAllMethod );
                            method.Invoke(enhancedConflictObj, null);
                            status = true;
                        }
                    }
                }
                catch( Exception e )
                {
                   iFolderWindow.log.Info("Exception type {0} Message {1} StackTrace {2}", e.GetType(), e.Message, e.StackTrace );
                }
            }
            else
            {
                iFolderWindow.log.Info("Enhanced Conflict Resolution functionality not available...");
            }
            return status;
        }
  private void ResolveConflicts(iFolderHolder holder)
  {
   if (holder == null) return;
   iFolderConflictDialog conflictDialog =
    (iFolderConflictDialog) ConflictDialogs[holder.iFolder.ID];
   if (conflictDialog == null)
   {
    try
    {
                    if(!AdvancedConflictResolver(this, holder.iFolder, ifws, simws))
                    {
            conflictDialog = new iFolderConflictDialog( this, holder.iFolder, ifws, simws );
         conflictDialog.SetPosition(WindowPosition.Center);
         conflictDialog.Response +=
          new ResponseHandler(OnConflictDialogResponse);
         conflictDialog.ShowAll();
         ConflictDialogs[holder.iFolder.ID] = conflictDialog;
                    }
    }
    catch(Exception e)
    {
     if(conflictDialog != null)
     {
      conflictDialog.Hide();
      conflictDialog.Destroy();
      conflictDialog = null;
     }
     iFolderExceptionDialog ied =
      new iFolderExceptionDialog(this, e);
     ied.Run();
     ied.Hide();
     ied.Destroy();
     ied = null;
    }
   }
   else
   {
    conflictDialog.Present();
   }
  }
  private void SyncSelectedFolder()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   if (holder != null)
   {
    try
    {
        ifws.SynciFolderNow(holder.iFolder.ID);
    }
    catch(Exception e)
    {
     iFolderExceptionDialog ied =
      new iFolderExceptionDialog(
       this,
       e);
     ied.Run();
     ied.Hide();
     ied.Destroy();
    }
   }
  }
  private void ShareSelectedFolder()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   if (holder != null)
   {
    iFolderWeb selectedFolder = holder.iFolder;
    if(selectedFolder.encryptionAlgorithm != null && selectedFolder.encryptionAlgorithm != "")
    {
     iFolderMsgDialog dg = new iFolderMsgDialog(
        this,
        iFolderMsgDialog.DialogType.Warning,
        iFolderMsgDialog.ButtonSet.Ok,
        "",
        Util.GS("Cannot share iFolder"),
        Util.GS("It is not possible to share an Encrypted iFolder. Only regular iFolders can be shared"));
     dg.Run();
     dg.Hide();
     dg.Destroy();
    }
    else
    {
     ShowFolderProperties(holder, 1);
    }
   }
  }
  private void ShowSelectedFolderProperties()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   if (holder != null)
   {
    ShowFolderProperties(holder, 0);
   }
  }
  private void RemoveSelectedFolderHandler()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   if (holder != null)
   {
    iFolderMsgDialog dialog = new iFolderMsgDialog(
     this,
     iFolderMsgDialog.DialogType.Question,
     iFolderMsgDialog.ButtonSet.YesNo,
     Util.GS("Revert to Normal Folder"),
     Util.GS("Revert this iFolder to a normal folder?"),
     Util.GS("The folder will still be on your computer, but it will no longer synchronize with the iFolder Server."));
    CheckButton deleteFromServerCB;
    DomainInformation domain =
     domainController.GetDomain(holder.iFolder.DomainID);
    if (domain == null || domain.MemberUserID == holder.iFolder.OwnerID)
     deleteFromServerCB = new CheckButton(Util.GS("_Delete this iFolder from the server"));
    else
     deleteFromServerCB = new CheckButton(Util.GS("_Remove my membership from this iFolder"));
    dialog.ExtraWidget = deleteFromServerCB;
    int rc = dialog.Run();
    dialog.Hide();
    dialog.Destroy();
    if(rc == -8)
    {
     try
     {
      bool removeDefault = false;
      string domainID = holder.iFolder.DomainID;
      if( simws.GetDefaultiFolder( domainID ) == holder.iFolder.ID)
      {
       Debug.PrintLine("Removing default iFolder");
       removeDefault = true;
      }
      iFolderHolder subHolder =
       ifdata.RevertiFolder(holder.iFolder.ID);
      if (deleteFromServerCB.Active)
      {
       string defaultiFolder = "";
       if (subHolder == null)
       {
        defaultiFolder = holder.iFolder.ID;
        ifdata.DeleteiFolder(holder.iFolder.ID);
       }
       else
       {
        defaultiFolder = subHolder.iFolder.ID;
        ifdata.DeleteiFolder(subHolder.iFolder.ID);
       }
       if( removeDefault )
        simws.DefaultAccount(domainID, null);
      }
      iFoldersIconView.UnselectAll();
     }
     catch(Exception e)
     {
      iFolderExceptionDialog ied =
       new iFolderExceptionDialog(
        this,
        e);
      ied.Run();
      ied.Hide();
      ied.Destroy();
     }
     UpdateSensitivity();
    }
   }
  }
  private int AskDeleteiFolder(iFolderHolder holder)
  {
   int rc = 0;
   iFolderMsgDialog dialog = new iFolderMsgDialog(
    this,
    iFolderMsgDialog.DialogType.Question,
    iFolderMsgDialog.ButtonSet.YesNo,
    "",
    string.Format(Util.GS("Delete \"{0}\" from the server?"),
         holder.iFolder.Name),
    Util.GS("This deletes the iFolder and its files from the server."));
   rc = dialog.Run();
   dialog.Hide();
   dialog.Destroy();
   return rc;
  }
  private int AskRemoveMembership(iFolderHolder holder)
  {
   int rc = 0;
   iFolderMsgDialog dialog = new iFolderMsgDialog(
    this,
    iFolderMsgDialog.DialogType.Question,
    iFolderMsgDialog.ButtonSet.YesNo,
    "",
    string.Format(Util.GS("Remove your membership from \"{0}\"?"),
         holder.iFolder.Name),
    Util.GS("This removes your membership from the iFolder and removes the iFolder from your list."));
   rc = dialog.Run();
   dialog.Hide();
   dialog.Destroy();
   return rc;
  }
  private void CreateNewiFolder()
  {
   DomainInformation[] domains = domainController.GetDomains();
   if (domains.Length <= 0) return;
   string domainID = domains[0].ID;
   DomainInformation defaultDomain = domainController.GetDefaultDomain();
   if (defaultDomain != null)
    domainID = defaultDomain.ID;
   CreateDialog cd = new CreateDialog(this, domains, domainID, Util.LastCreatedPath, ifws);
   cd.TransientFor = this;
   int rc = 0;
   do
   {
    rc = cd.Run();
    cd.Hide();
    if (rc == (int)ResponseType.Ok)
    {
     try
     {
      string selectedFolder = cd.iFolderPath.Trim();
      string selectedDomain = cd.DomainID;
      bool SSL = cd.ssl;
      string algorithm = cd.EncryptionAlgorithm;
      if (selectedFolder == String.Empty)
      {
       iFolderMsgDialog dg = new iFolderMsgDialog(
        this,
        iFolderMsgDialog.DialogType.Warning,
        iFolderMsgDialog.ButtonSet.Ok,
        "",
        Util.GS("Invalid folder specified"),
        Util.GS("An invalid folder was specified."));
       dg.Run();
       dg.Hide();
       dg.Destroy();
       continue;
      }
      string parentDir = System.IO.Path.GetDirectoryName( selectedFolder );
      if ( ( parentDir == null ) || ( parentDir == String.Empty ) )
      {
       iFolderMsgDialog dg = new iFolderMsgDialog(
        this,
        iFolderMsgDialog.DialogType.Warning,
        iFolderMsgDialog.ButtonSet.Ok,
        "",
        Util.GS("Invalid folder specified"),
        Util.GS("An invalid folder was specified"));
       dg.Run();
       dg.Hide();
       dg.Destroy();
       continue;
      }
      if(ifws.GetLimitPolicyStatus(selectedDomain) != 1 )
                                                {
                                                        iFolderMsgDialog dg = new iFolderMsgDialog(
                                                                this,
                                                                iFolderMsgDialog.DialogType.Warning,
                                                                iFolderMsgDialog.ButtonSet.Ok,
                                                                "",
                                                                Util.GS("Error"),
                                                                Util.GS("Folder could not be created as you are exceeding the limit of ifolders set by your Administrator."));
                                                        dg.Run();
                                                        dg.Hide();
                                                        dg.Destroy();
                                                        continue;
                                                }
      string name = selectedFolder.Substring(parentDir.Length + 1);
      if (name == null || name == String.Empty)
      {
       iFolderMsgDialog dg = new iFolderMsgDialog(
        this,
        iFolderMsgDialog.DialogType.Warning,
        iFolderMsgDialog.ButtonSet.Ok,
        "",
        Util.GS("Invalid folder specified"),
        Util.GS("The folder you've specified is invalid.  Please remove the trailing separator character (/) and try again."));
       dg.Run();
       dg.Hide();
       dg.Destroy();
       continue;
      }
      selectedFolder = ParseAndReplaceTildeInPath(selectedFolder);
      iFolderHolder ifHolder = null;
      try
      {
       if( algorithm != null)
       {
        bool passPhraseStatus = false;
        bool passphraseStatus = false;
        try
        {
                              passphraseStatus = simws.IsPassPhraseSet(selectedDomain);
        }
        catch(Exception ex)
        {
         DisplayLoginMesg();
         continue;
        }
        if(passphraseStatus == true)
        {
         string passphrasecheck = simws.GetPassPhrase(selectedDomain);
         if( passphrasecheck == null || passphrasecheck =="")
         {
          Debug.PrintLine(" passphrase not entered at login");
          passPhraseStatus = ShowVerifyDialog(selectedDomain, simws);
         }
         else
         {
          passPhraseStatus = true;
         }
        }
        else
        {
         passPhraseStatus = ShowEnterPassPhraseDialog(selectedDomain, simws);
        }
        if( passPhraseStatus == false)
        {
         continue;
        }
       }
       ifHolder = ifdata.CreateiFolder(selectedFolder, selectedDomain, SSL, algorithm);
      }
      catch(Exception e)
      {
       if (DisplayCreateOrSetupException(e))
       {
        cd.iFolderPath = selectedFolder;
        continue;
       }
      }
      if(ifHolder == null)
       throw new Exception("Simias returned null");
      rc = 0;
      Util.LastCreatedPath = ifHolder.iFolder.UnManagedPath;
      if((bool)ClientConfig.Get(ClientConfig.KEY_SHOW_CREATION))
      {
       iFolderCreationDialog dlg =
        new iFolderCreationDialog(ifHolder.iFolder);
       dlg.TransientFor = this;
       int createRC;
       do
       {
        createRC = dlg.Run();
        if(createRC == (int)Gtk.ResponseType.Help)
        {
         Util.ShowHelp("myifolders.html", this);
        }
       }while(createRC != (int)Gtk.ResponseType.Ok);
       dlg.Hide();
       if(dlg.HideDialog)
       {
        ClientConfig.Set(
         ClientConfig.KEY_SHOW_CREATION, false);
       }
       cd.Destroy();
       cd = null;
      }
     }
     catch (Exception e)
     {
      Debug.PrintLine(e.Message);
      continue;
     }
    }
   }
   while(rc == (int)ResponseType.Ok);
  }
  private bool DisplayCreateOrSetupException(Exception e)
  {
   string primaryText = null;
   string secondaryText = null;
   if (e.Message.IndexOf("Path did not exist") >= 0 || e.Message.IndexOf("URI scheme was not recognized") >= 0)
   {
    primaryText = Util.GS("Invalid folder specified");
    secondaryText = Util.GS("The folder you've specified does not exist.  Please select an existing folder and try again.");
   }
   else if (e.Message.IndexOf("PathExists") >= 0)
   {
    primaryText = Util.GS("A folder with the same name already exists.");
    secondaryText = Util.GS("The location you selected already contains a folder with the same name as this iFolder.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("RootOfDrivePath") >= 0)
   {
    primaryText = Util.GS("iFolders cannot exist at the drive level.");
    secondaryText = Util.GS("The location you selected is at the root of the drive.  Please select a location that is not at the root of a drive and try again.");
   }
   else if (e.Message.IndexOf("InvalidCharactersPath") >= 0)
   {
    primaryText = Util.GS("The selected location contains invalid characters.");
    secondaryText = Util.GS("The characters \\:*?\"<>| cannot be used in an iFolder. Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("AtOrInsideStorePath") >= 0)
   {
    primaryText = Util.GS("The selected location is inside the iFolder data folder.");
    secondaryText = Util.GS("The iFolder data folder is normally located in your home folder in the folder \".local/share\".  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("ContainsStorePath") >= 0)
   {
    primaryText = Util.GS("The selected location contains the iFolder data files.");
    secondaryText = Util.GS("The location you have selected contains the iFolder data files.  These are normally located in your home folder in the folder \".local/share\".  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("NotFixedDrivePath") >= 0)
   {
    primaryText = Util.GS("The selected location is on a network or non-physical drive.");
    secondaryText = Util.GS("iFolders must reside on a physical drive.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("SystemDirectoryPath") >= 0)
   {
    primaryText = Util.GS("The selected location contains a system folder.");
    secondaryText = Util.GS("System folders cannot be contained in an iFolder.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("SystemDrivePath") >= 0)
   {
    primaryText = Util.GS("The selected location is a system drive.");
    secondaryText = Util.GS("System drives cannot be contained in an iFolder.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("IncludesWinDirPath") >= 0)
   {
    primaryText = Util.GS("The selected location includes the Windows folder.");
    secondaryText = Util.GS("The Windows folder cannot be contained in an iFolder.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("IncludesProgFilesPath") >= 0)
   {
    primaryText = Util.GS("The selected location includes the Program Files folder.");
    secondaryText = Util.GS("The Program Files folder cannot be contained in an iFolder.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("DoesNotExistPath") >= 0)
   {
    primaryText = Util.GS("The selected location does not exist.");
    secondaryText = Util.GS("iFolders can only be created from folders that exist.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("NoReadRightsPath") >= 0)
   {
    primaryText = Util.GS("You do not have access to read files in the selected location.");
    secondaryText = Util.GS("iFolders can only be created from folders where you have access to read and write files.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("NoWriteRightsPath") >= 0)
   {
    primaryText = Util.GS("You do not have access to write files in the selected location.");
    secondaryText = Util.GS("iFolders can only be created from folders where you have access to read and write files.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("ContainsCollectionPath") >= 0)
   {
    primaryText = Util.GS("The selected location already contains an iFolder.");
    secondaryText = Util.GS("iFolders cannot exist inside other iFolders.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("AtOrInsideCollectionPath") >= 0)
   {
    primaryText = Util.GS("The selected location is inside another iFolder.");
    secondaryText = Util.GS("iFolders cannot exist inside other iFolders.  Please select a different location and try again.");
   }
   else if (e.Message.IndexOf("FolderDoesNotExist") >= 0)
   {
    primaryText = Util.GS("The name of folder and iFolder must be the same");
    secondaryText = Util.GS("You can merge an iFolder only with a folder with same name. Select a folder with the same name as the iFolder and try again.");
   }
   else if(e.Message.IndexOf("Invalid iFolderID") >=0)
   {
    primaryText = Util.GS("The iFolder does not exist.");
    secondaryText = Util.GS("The iFolder you have chosen could not be found on the server.");
    ifdata.Refresh();
   }
   if (primaryText != null)
   {
    iFolderMsgDialog dg = new iFolderMsgDialog(
     this,
     iFolderMsgDialog.DialogType.Warning,
     iFolderMsgDialog.ButtonSet.Ok,
     "",
     primaryText,
     secondaryText);
     dg.Run();
     dg.Hide();
     dg.Destroy();
     return true;
   }
   else
   {
    iFolderExceptionDialog ied =
     new iFolderExceptionDialog(
      this,
      e);
    ied.Run();
    ied.Hide();
    ied.Destroy();
   }
   return false;
  }
  private string ParseAndReplaceTildeInPath(string origPath)
  {
   string parsedString = origPath;
   if (origPath.IndexOf('~') >= 0)
   {
    string homeDirectory = Environment.GetFolderPath(Environment.SpecialFolder.Personal);
    parsedString = origPath.Replace("~", homeDirectory);
   }
   return parsedString;
  }
  public void DownloadiFolder(string ifolderID)
  {
   iFolderHolder holder = ifdata.GetiFolder(ifolderID);
   DownloadiFolder(holder, false);
  }
  public void ResolveConflicts(string ifolderID)
  {
   if (ifolderID == null) return;
   iFolderHolder holder = ifdata.GetiFolder(ifolderID);
   if (holder != null)
    ResolveConflicts(holder);
  }
  private void DisplayLoginMesg()
  {
   iFolderMsgDialog dlg = new iFolderMsgDialog(null, iFolderMsgDialog.DialogType.Error, iFolderMsgDialog.ButtonSet.Ok,
         Util.GS("iFolder Error"), Util.GS("Error creating iFolder"),
         Util.GS("You should be logged-in to the domain for creating encrypted iFolders."));
   dlg.Run();
   dlg.Hide();
   dlg.Destroy();
  }
  private void OniFolderSyncEvent(object o, CollectionSyncEventArgs args)
  {
   if (args == null || args.ID == null || args.Name == null)
    return;
   switch(args.Action)
   {
    case Simias.Client.Event.Action.StartLocalSync:
    {
                    this.RevertMenuItem.Sensitive = false;
        this.RemoveiFolderButton.Sensitive = false;
     if (args.Name != null && args.Name.StartsWith("POBox:"))
     {
      DomainInformation domain = domainController.GetPOBoxDomain(args.ID);
      if (domain != null)
       UpdateStatus(string.Format(Util.GS("Checking for new iFolders: {0}"), domain.Name));
      else
       UpdateStatus(Util.GS("Checking for new iFolders..."));
     }
     else
     {
      UpdateStatus(string.Format(Util.GS(
         "Checking for changes: {0}"), args.Name));
     }
     break;
    }
    case Simias.Client.Event.Action.StartSync:
    {
     if (args.Name != null && args.Name.StartsWith("POBox:"))
     {
      DomainInformation domain = domainController.GetPOBoxDomain(args.ID);
      if (domain != null)
       UpdateStatus(string.Format(Util.GS("Checking for new iFolders: {0}"), domain.Name));
      else
       UpdateStatus(Util.GS("Checking for new iFolders..."));
     }
     else
     {
      UpdateStatus(string.Format(Util.GS(
          "Synchronizing: {0}"), args.Name));
     }
     break;
    }
    case Simias.Client.Event.Action.StopSync:
    {
     if(SyncBar != null)
      SyncBar.Hide();
     UpdateStatus(Util.GS("Idle..."));
     if (args.Name != null )
     {
                         DomainInformation domain = domainController.GetDomain(args.ID);
       UpdateQoutaData(domain);
     }
        this.RevertMenuItem.Sensitive = true;
     this.RemoveiFolderButton.Sensitive = true;
     break;
    }
   }
  }
  private void OniFolderFileSyncEvent(object o, FileSyncEventArgs args)
  {
   if (args == null || args.CollectionID == null || args.Name == null)
    return;
   if (args.SizeRemaining == args.SizeToSync)
   {
    if (!args.Direction.Equals(Simias.Client.Event.Direction.Local))
    {
     if(SyncBar == null)
     {
      SyncBar = new ProgressBar();
      SyncBar.Orientation = Gtk.ProgressBarOrientation.LeftToRight;
      SyncBar.PulseStep = .01;
      MainStatusBar.PackEnd(SyncBar, false, true, 0);
     }
     SyncBar.Fraction = 0;
     SyncBar.Show();
    }
    switch (args.ObjectType)
    {
     case ObjectType.File:
      if (args.Delete)
       UpdateStatus(string.Format(
        Util.GS("Deleting file: {0}"),
        args.Name));
      else
      {
       switch (args.Direction)
       {
        case Simias.Client.Event.Direction.Uploading:
         UpdateStatus(string.Format(
          Util.GS("Uploading file: {0}"),
          args.Name));
         break;
        case Simias.Client.Event.Direction.Downloading:
         UpdateStatus(string.Format(
          Util.GS("Downloading file: {0}"),
          args.Name));
         break;
        case Simias.Client.Event.Direction.Local:
         UpdateStatus(string.Format(
          Util.GS("Found changes in file: {0}"),
          args.Name));
         break;
        default:
         UpdateStatus(string.Format(
          Util.GS("Synchronizing file: {0}"),
          args.Name));
         break;
       }
      }
      break;
     case ObjectType.Directory:
      if (args.Delete)
       UpdateStatus(string.Format(
        Util.GS("Deleting directory: {0}"),
        args.Name));
      else
      {
       switch (args.Direction)
       {
        case Simias.Client.Event.Direction.Uploading:
         UpdateStatus(string.Format(
          Util.GS("Uploading directory: {0}"),
          args.Name));
         break;
        case Simias.Client.Event.Direction.Downloading:
         UpdateStatus(string.Format(
          Util.GS("Downloading directory: {0}"),
          args.Name));
         break;
        case Simias.Client.Event.Direction.Local:
         UpdateStatus(string.Format(
          Util.GS("Found changes in directory: {0}"),
          args.Name));
         break;
        default:
         UpdateStatus(string.Format(
          Util.GS("Synchronizing directory: {0}"),
          args.Name));
         break;
       }
      }
      break;
     case ObjectType.Unknown:
      UpdateStatus(string.Format(
       Util.GS("Deleting on server: {0}"),
       args.Name));
      break;
    }
   }
   else
   {
    if (SyncBar != null)
    {
     SyncBar.Show();
     if (args.SizeToSync > 0)
     {
      SyncBar.Fraction =
       (((double)args.SizeToSync) - ((double)args.SizeRemaining)) /
       ((double)args.SizeToSync);
     }
     else
      SyncBar.Fraction = 1;
    }
   }
  }
 public static bool ShowEnterPassPhraseDialog(string DomainID, SimiasWebService simws)
 {
  bool status = false;
     EnterPassPhraseDialog epd = new EnterPassPhraseDialog(DomainID, simws);
  if (!Util.RegisterModalWindow(epd))
     {
      epd.Destroy();
   epd = null;
   return false;
  }
  try
  {
            status = PassphraseHelper( epd, DomainID, simws);
            if( !status )
                return status;
  }
  catch(Exception ex)
  {
            iFolderMsgDialog dialog = new iFolderMsgDialog(
                null,
                iFolderMsgDialog.DialogType.Error,
                iFolderMsgDialog.ButtonSet.None,
                Util.GS("Unable to set the passphrase"),
                Util.GS(ex.Message),
                Util.GS("Please enter the passphrase again"));
            dialog.Run();
            dialog.Hide();
            dialog.Destroy();
            dialog = null;
   return false;
  }
  return status;
 }
    private static bool SetPassPhrase( EnterPassPhraseDialog epd, string DomainID, string publicKey, SimiasWebService simws )
    {
        bool status;
        Status passPhraseStatus = simws.SetPassPhrase( DomainID, epd.PassPhrase, epd.RecoveryAgent, publicKey);
        if(passPhraseStatus.statusCode == StatusCodes.Success)
        {
            status = true;
            simws.StorePassPhrase( DomainID, epd.PassPhrase, CredentialType.Basic, epd.ShouldSavePassPhrase);
        }
        else
        {
            status = false;
            iFolderMsgDialog dialog = new iFolderMsgDialog(
                null,
                iFolderMsgDialog.DialogType.Error,
                iFolderMsgDialog.ButtonSet.None,
                Util.GS("Error setting the PassPhrase"),
                Util.GS("Unable to set the passphrase"),
                Util.GS("Please try again"));
            dialog.Run();
            dialog.Hide();
            dialog.Destroy();
            dialog = null;
        }
        return status;
    }
    private static bool PassphraseHelper( EnterPassPhraseDialog epd, string DomainID, SimiasWebService simws)
    {
     bool status = false;
  int result;
        do
        {
      result = epd.Run();
         if(result == (int)ResponseType.Cancel || result == (int) ResponseType.DeleteEvent)
         {
                epd.Hide();
          return false;
            }
      if( epd.PassPhrase != epd.RetypedPassPhrase )
      {
       iFolderMsgDialog dialog = new iFolderMsgDialog(
        null,
        iFolderMsgDialog.DialogType.Error,
        iFolderMsgDialog.ButtonSet.None,
        Util.GS("passphrase mismatch"),
        Util.GS("The passphrase and retyped passphrase are not same"),
           Util.GS("Please enter the passphrase again"));
       dialog.Run();
       dialog.Hide();
       dialog.Destroy();
       dialog = null;
      }
            else
            {
                break;
            }
        }while( result != (int)ResponseType.Cancel );
  if( result != (int)ResponseType.Cancel || result != (int) ResponseType.DeleteEvent)
  {
   string publicKey = null;
   if( epd.RecoveryAgent != null)
   {
    byte [] RACertificateObj = DomainController.GetDomainController().GetRACertificate(DomainID, epd.RecoveryAgent);
    if( RACertificateObj != null && RACertificateObj.Length != 0)
    {
     System.Security.Cryptography.X509Certificates.X509Certificate Cert = new System.Security.Cryptography.X509Certificates.X509Certificate(RACertificateObj);
     CertificateDialog dlg = new CertificateDialog(Cert.ToString(true));
     if (!Util.RegisterModalWindow(dlg))
     {
      dlg.Destroy();
      dlg = null;
      return false;
     }
     int res = dlg.Run();
     dlg.Hide();
     dlg.Destroy();
     dlg = null;
     if( res == (int)ResponseType.Ok)
     {
      publicKey = Convert.ToBase64String(Cert.GetPublicKey());
     }
     else
     {
                        status = false;
                  simws.StorePassPhrase(DomainID, "", CredentialType.None, false);
      return ShowEnterPassPhraseDialog(DomainID, simws);
     }
    }
                status = SetPassPhrase( epd, DomainID, publicKey, simws );
                epd.Hide();
                return status;
   }
            else
            {
                iFolderMsgDialog dg = new iFolderMsgDialog(
                    epd,
                    iFolderMsgDialog.DialogType.Warning,
                    iFolderMsgDialog.ButtonSet.YesNo,
                    "No Recovery Agent",
                    Util.GS("Recovery Agent Not Selected"),
                    Util.GS("There is no Recovery Agent selected. Encrypted data cannot be recovered later, if passphrase is lost. Do you want to continue?"));
          int rc = dg.Run();
       dg.Hide();
             dg.Destroy();
                if( (ResponseType)rc == ResponseType.Yes )
                {
                    status = SetPassPhrase( epd, DomainID, publicKey, simws );
                    epd.Hide();
                    return status;
                }
                else
                {
                    epd.Hide();
                    PassphraseHelper( epd, DomainID, simws );
                }
            }
  }
        else
        {
            epd.Hide();
        }
        return true;
    }
  public static bool ShowVerifyDialog(string DomainID, SimiasWebService simws)
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
    if( result == (int)ResponseType.Ok)
     passPhraseStatus = simws.ValidatePassPhrase(DomainID, vpd.PassPhrase);
    if( passPhraseStatus != null)
    {
     if( passPhraseStatus.statusCode == StatusCodes.PassPhraseInvalid)
     {
      Debug.PrintLine("Invalid Passphrase");
      iFolderMsgDialog dialog = new iFolderMsgDialog(
       null,
       iFolderMsgDialog.DialogType.Error,
       iFolderMsgDialog.ButtonSet.None,
       Util.GS("Invalid Passphrase"),
       Util.GS("The Passphrase entered is invalid"),
       Util.GS("Please enter the passphrase again"));
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
   if( result == (int)ResponseType.Cancel || result == (int)ResponseType.DeleteEvent)
   {
    try
    {
     simws.StorePassPhrase(DomainID, "", CredentialType.None, false);
     status = false;
    }
    catch(Exception e)
    {
     return false;
    }
   }
   else if( passPhraseStatus != null && passPhraseStatus.statusCode == StatusCodes.Success)
   {
    try
    {
     simws.StorePassPhrase( DomainID, vpd.PassPhrase, CredentialType.Basic, vpd.ShouldSavePassPhrase);
     status = true;
    }
    catch(Exception ex)
    {
     return false;
    }
   }
   }
   catch(Exception e)
   {
    return false;
   }
   return status;
  }
  private bool IsPassPhraseAvailable(string selectedDomain)
  {
   bool passPhraseStatus = false;
   bool passphraseStatus = false;
   try
   {
    passphraseStatus = simws.IsPassPhraseSet(selectedDomain);
   }
   catch(Exception ex)
   {
    DisplayLoginMesg();
    return false;
   }
   if(passphraseStatus == true)
   {
    string passphrasecheck = simws.GetPassPhrase(selectedDomain);
    if( passphrasecheck == null || passphrasecheck =="")
    {
     passPhraseStatus = ShowVerifyDialog(selectedDomain, simws);
    }
    else
    {
     passPhraseStatus = true;
    }
   }
   else
   {
    passPhraseStatus = ShowEnterPassPhraseDialog(selectedDomain, simws);
   }
   return passPhraseStatus;
  }
 }
public class UriList : ArrayList {
 private void LoadFromString (string data) {
  string [] items = data.Split ('\n');
  foreach (String i in items) {
   if (!i.StartsWith ("#")) {
    Uri uri;
    String s = i;
    if (i.EndsWith ("\r")) {
     s = i.Substring (0, i.Length - 1);
    }
    try {
     uri = new Uri (s);
    } catch {
     continue;
    }
    Add (uri);
   }
  }
 }
 static char[] CharsToQuote = { ';', '?', ':', '@', '&', '=', '$', ',', '#' };
 public static Uri PathToFileUri (string path)
 {
  path = Path.GetFullPath (path);
  StringBuilder builder = new StringBuilder ();
  builder.Append (Uri.UriSchemeFile);
  builder.Append (Uri.SchemeDelimiter);
  int i;
  while ((i = path.IndexOfAny (CharsToQuote)) != -1) {
   if (i > 0)
    builder.Append (path.Substring (0, i));
   builder.Append (Uri.HexEscape (path [i]));
   path = path.Substring (i+1);
  }
  builder.Append (path);
  return new Uri (builder.ToString (), true);
 }
 public UriList (string [] uris)
 {
  foreach (string str in uris) {
   Uri uri;
   if (File.Exists (str) || Directory.Exists (str))
    uri = PathToFileUri (str);
   else
    uri = new Uri (str);
   Add (uri);
  }
 }
 public UriList (string data) {
  LoadFromString (data);
 }
 public UriList (Gtk.SelectionData selection)
 {
  LoadFromString (System.Text.Encoding.UTF8.GetString (selection.Data));
 }
 public override string ToString () {
  StringBuilder list = new StringBuilder ();
  foreach (Uri uri in this) {
   if (uri == null)
    break;
   list.Append (uri.ToString () + "\r\n");
  }
  return list.ToString ();
 }
 public string [] ToLocalPaths () {
  int count = 0;
  foreach (Uri uri in this) {
   if (uri.IsFile)
    count++;
  }
  String [] paths = new String [count];
  count = 0;
  foreach (Uri uri in this) {
   if (uri.IsFile)
    paths[count++] = uri.LocalPath;
  }
  return paths;
 }
}
 public class ColorPaletteDialog : Dialog
 {
  public ColorPaletteDialog() : base()
  {
   this.VBox.Add(CreateWidgets());
   this.AddButton("_Close", ResponseType.Close);
  }
  private Widget CreateWidgets()
  {
   VBox vbox = new VBox(false, 0);
   Label l = new Label("<span size=\"large\">Base Colors</span>");
   vbox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l.Xalign = 0;
   vbox.PackStart(CreateColorBox("Normal", this.Style.Base(StateType.Normal)));
   vbox.PackStart(CreateColorBox("Active", this.Style.Base(StateType.Active)));
   vbox.PackStart(CreateColorBox("Prelight", this.Style.Base(StateType.Prelight)));
   vbox.PackStart(CreateColorBox("Selected", this.Style.Base(StateType.Selected)));
   vbox.PackStart(CreateColorBox("Insensitive", this.Style.Base(StateType.Insensitive)));
   l = new Label("<span size=\"large\">Background Colors</span>");
   vbox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l.Xalign = 0;
   vbox.PackStart(CreateColorBox("Normal", this.Style.Background(StateType.Normal)));
   vbox.PackStart(CreateColorBox("Active", this.Style.Background(StateType.Active)));
   vbox.PackStart(CreateColorBox("Prelight", this.Style.Background(StateType.Prelight)));
   vbox.PackStart(CreateColorBox("Selected", this.Style.Background(StateType.Selected)));
   vbox.PackStart(CreateColorBox("Insensitive", this.Style.Background(StateType.Insensitive)));
   l = new Label("<span size=\"large\">Foreground Colors</span>");
   vbox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l.Xalign = 0;
   vbox.PackStart(CreateColorBox("Normal", this.Style.Foreground(StateType.Normal)));
   vbox.PackStart(CreateColorBox("Active", this.Style.Foreground(StateType.Active)));
   vbox.PackStart(CreateColorBox("Prelight", this.Style.Foreground(StateType.Prelight)));
   vbox.PackStart(CreateColorBox("Selected", this.Style.Foreground(StateType.Selected)));
   vbox.PackStart(CreateColorBox("Insensitive", this.Style.Foreground(StateType.Insensitive)));
   vbox.ShowAll();
   return vbox;
  }
  private Widget CreateColorBox(string name, Gdk.Color color)
  {
   EventBox eb = new EventBox();
   eb.ModifyBg(StateType.Normal, color);
   Label l = new Label(name);
   eb.Add(l);
   l.Show();
   return eb;
  }
 }
 public class iFolderServerFilter
 {
  private string domainID;
  private Entry searchEntry;
  public iFolderServerFilter(string domainID, Entry searchEntry)
  {
   this.domainID = domainID;
   this.searchEntry = searchEntry;
  }
  public bool FilterFunc(TreeModel model, TreeIter iter)
  {
   iFolderHolder ifHolder = (iFolderHolder)model.GetValue(iter, 0);
   if (ifHolder == null || ifHolder.iFolder == null || ifHolder.iFolder.DomainID == null) return false;
   if (ifHolder.iFolder.IsSubscription
    && ifHolder.iFolder.DomainID == domainID)
   {
    string searchString = searchEntry.Text;
    if (searchString != null)
    {
     searchString = searchString.Trim();
     if (searchString.Length > 0)
      searchString = searchString.ToLower();
    }
    if (searchString == null || searchString.Trim().Length == 0)
     return true;
    else
    {
     string name = ifHolder.iFolder.Name;
     if (name != null)
     {
      name = name.ToLower();
      if (name.IndexOf(searchString) >= 0)
       return true;
     }
    }
   }
   return false;
  }
 }
}
