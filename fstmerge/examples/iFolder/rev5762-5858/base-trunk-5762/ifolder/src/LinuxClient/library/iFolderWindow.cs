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
 public class iFolderWindow : Window
 {
  const int ctx = 1;
  private iFolderWebService ifws;
  private SimiasWebService simws;
  private iFolderData ifdata;
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
  private Gtk.MenuItem DownloadMenuItem;
  private ImageMenuItem PropMenuItem;
  private ImageMenuItem CloseMenuItem;
  private ImageMenuItem QuitMenuItem;
  private ImageMenuItem RefreshMenuItem;
  private ImageMenuItem HelpMenuItem;
  private ImageMenuItem AboutMenuItem;
  private ImageMenuItem PreferencesMenuItem;
  private Gtk.MenuItem AccountsMenuItem;
  private Gtk.MenuItem SyncLogMenuItem;
  private CheckMenuItem ViewServeriFoldersMenuItem;
  private DomainController domainController;
  private Manager simiasManager;
  private Hashtable PropDialogs;
  private Hashtable ConflictDialogs;
  private Notebook WindowNotebook;
  private Button ConnectToServerButton;
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
  private iFolderViewGroup localGroup;
  private TreeModelFilter myiFoldersFilter;
  private VBox SynchronizedFolderTasks;
  private Button OpenSynchronizedFolderButton;
  private Button SynchronizeNowButton;
  private Button ShareSynchronizedFolderButton;
  private Button ResolveConflictsButton;
  private Button RemoveiFolderButton;
  private Button ViewFolderPropertiesButton;
  private Button DownloadAvailableiFolderButton;
  private Button DeleteFromServerButton;
  private Button RemoveMembershipButton;
  private Hashtable serverGroups;
  private Hashtable serverGroupFilters;
        enum TargetType
        {
         UriList,
         RootWindow
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
   }
  }
  private void CreateWidgets()
  {
   this.SetDefaultSize (600, 480);
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder24.png"));
   this.WindowPosition = Gtk.WindowPosition.Center;
   WindowNotebook = new Notebook();
   this.Add(WindowNotebook);
   WindowNotebook.ShowTabs = false;
   WindowNotebook.AppendPage(CreateWelcomePage(), null);
   WindowNotebook.AppendPage(CreateNormalPage(), null);
   this.Realized += new EventHandler(OnRealizeWidget);
  }
  private Widget CreateWelcomePage()
  {
   VBox vbox = new VBox(false, 0);
   MenuBar menubar = CreateWelcomeMenuBar();
   vbox.PackStart (menubar, false, false, 0);
   Frame frame = new Frame();
   vbox.PackStart(frame, true, true, 0);
   vbox.ModifyBase(StateType.Normal, new Gdk.Color(255, 255, 255));
   VBox welcomeVBox = new VBox(false, 0);
   frame.Add(welcomeVBox);
   Gdk.Pixbuf pixbuf = new Gdk.Pixbuf(Util.ImagesPath("ifolder128.png"));
   Image image = new Image(pixbuf);
   image.SetAlignment(0.5F, 0.5F);
   welcomeVBox.PackStart(image, false, false, 0);
   Label l = new Label(
    string.Format("<span size=\"x-large\" weight=\"bold\">{0}</span>",
    Util.GS("Welcome to iFolder")));
   welcomeVBox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l = new Label(
    string.Format("<span>{0}</span>",
    Util.GS("iFolder is a file sharing solution for workgroup and enterprise environments.")));
   welcomeVBox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   HBox hbox = new HBox(false, 0);
   ConnectToServerButton = new Button(hbox);
   ConnectToServerButton.Relief = ReliefStyle.None;
   vbox.PackStart(ConnectToServerButton, false, false, 0);
   Gdk.Pixbuf folderPixbuf = new Gdk.Pixbuf(Util.ImagesPath("add-account.png"));
   folderPixbuf = folderPixbuf.ScaleSimple(64, 64, Gdk.InterpType.Bilinear);
   Image folderImage = new Image(folderPixbuf);
   folderImage.SetAlignment(0.5F, 0F);
   hbox.PackStart(folderImage, false, false, 0);
   VBox buttonVBox = new VBox(false, 0);
   hbox.PackStart(buttonVBox, true, true, 4);
   Label buttonText = new Label(string.Format("<span size=\"large\" weight=\"bold\">{0}</span>", Util.GS("Connect to an iFolder Server")));
   buttonVBox.PackStart(buttonText, false, false, 0);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   Label buttonMessage = new Label(string.Format("<span size=\"small\">{0}</span>", Util.GS("Start synchronizing files by connecting to an iFolder server")));
   buttonVBox.PackStart(buttonMessage, false, false, 0);
   buttonMessage.UseMarkup = true;
   buttonMessage.UseUnderline = false;
   buttonMessage.LineWrap = true;
   buttonMessage.Justify = Justification.Left;
   buttonMessage.Xalign = 0;
   buttonMessage.Yalign = 0;
   ConnectToServerButton.Clicked +=
    new EventHandler(OnConnectToServerButton);
   return vbox;
  }
  private MenuBar CreateWelcomeMenuBar()
  {
   MenuBar menubar = new MenuBar ();
   AccelGroup agrp = new AccelGroup();
   this.AddAccelGroup(agrp);
   Menu menu = new Menu();
   ImageMenuItem imageMenuItem = new ImageMenuItem (Util.GS("Connect to a _server"));
   Gdk.Pixbuf pixbuf = new Gdk.Pixbuf(Util.ImagesPath("add-account.png"));
   pixbuf = pixbuf.ScaleSimple(24, 24, Gdk.InterpType.Bilinear);
   imageMenuItem.Image = new Image(pixbuf);
   menu.Append(imageMenuItem);
   imageMenuItem.AddAccelerator("activate", agrp,
    new AccelKey(Gdk.Key.S, Gdk.ModifierType.ControlMask,
        AccelFlags.Visible));
   imageMenuItem.Activated += new EventHandler(OnAddNewAccount);
   menu.Append(new SeparatorMenuItem());
   imageMenuItem = new ImageMenuItem (Stock.Close, agrp);
   menu.Append(imageMenuItem);
   imageMenuItem.Activated += new EventHandler(CloseEventHandler);
   imageMenuItem = new ImageMenuItem(Stock.Quit, agrp);
   menu.Append(imageMenuItem);
   imageMenuItem.Activated += new EventHandler(QuitEventHandler);
   MenuItem menuItem = new MenuItem(Util.GS("i_Folder"));
   menuItem.Submenu = menu;
   menubar.Append (menuItem);
   menu = new Menu();
   imageMenuItem = new ImageMenuItem(Util.GS("_Preferences"));
   imageMenuItem.Image = new Image(Stock.Preferences, Gtk.IconSize.Menu);
   menu.Append(imageMenuItem);
   imageMenuItem.Activated += new EventHandler(ShowPreferencesHandler);
   menuItem = new MenuItem(Util.GS("_Edit"));
   menuItem.Submenu = menu;
   menubar.Append(menuItem);
   menu = new Menu();
   imageMenuItem = new ImageMenuItem(Stock.Help, agrp);
   menu.Append(imageMenuItem);
   imageMenuItem.Activated += new EventHandler(OnHelpMenuItem);
   imageMenuItem = new ImageMenuItem(Util.GS("A_bout"));
   imageMenuItem.Image = new Image(Gnome.Stock.About,
       Gtk.IconSize.Menu);
   menu.Append(imageMenuItem);
   imageMenuItem.Activated += new EventHandler(OnAbout);
   menuItem = new MenuItem(Util.GS("_Help"));
   menuItem.Submenu = menu;
   menubar.Append(menuItem);
   return menubar;
  }
  private Widget CreateNormalPage()
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
   NewMenuItem = new ImageMenuItem (Util.GS("Create a _new iFolder..."));
   NewMenuItem.Image = new Image(
     new Gdk.Pixbuf(Util.ImagesPath("ifolder24.png")));
   iFolderMenu.Append(NewMenuItem);
   NewMenuItem.AddAccelerator("activate", agrp,
    new AccelKey(Gdk.Key.N, Gdk.ModifierType.ControlMask,
        AccelFlags.Visible));
   NewMenuItem.Activated += new EventHandler(AddiFolderHandler);
   DownloadMenuItem =
    new MenuItem (Util.GS("_Download and synchronize..."));
   iFolderMenu.Append(DownloadMenuItem);
   DownloadMenuItem.Activated += new EventHandler(DownloadAvailableiFolderHandler);
   DeleteMenuItem =
    new ImageMenuItem (Util.GS("Dele_te from server"));
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
    new ImageMenuItem (Util.GS("C_hange to a normal folder"));
   RevertMenuItem.Image = new Image(Stock.Undo, Gtk.IconSize.Menu);
   iFolderMenu.Append(RevertMenuItem);
   RevertMenuItem.Activated += new EventHandler(RemoveiFolderHandler);
   PropMenuItem = new ImageMenuItem (Stock.Properties, agrp);
   iFolderMenu.Append(PropMenuItem);
   PropMenuItem.Activated += new EventHandler(OnShowFolderProperties);
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
   MenuItem showColorPaletteMenuItem =
    new MenuItem(Util.GS("Show _Color Palette (FIXME: Remove this before shipping)..."));
   ViewMenu.Append(showColorPaletteMenuItem);
   showColorPaletteMenuItem.Activated += ShowColorPalette;
   MenuItem ViewMenuItem = new MenuItem(Util.GS("_View"));
   ViewMenuItem.Submenu = ViewMenu;
   menubar.Append(ViewMenuItem);
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
   actionsVBox.WidthRequest = 250;
   Label l = new Label("<span size=\"small\"></span>");
   actionsVBox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l = new Label(
    string.Format(
     "<span size=\"x-large\">{0}</span>",
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
   searchHBox.PackEnd(CancelSearchButton, false, false, 0);
   CancelSearchButton.Relief = ReliefStyle.None;
   CancelSearchButton.Sensitive = false;
   CancelSearchButton.Clicked +=
    new EventHandler(OnCancelSearchButton);
   l = new Label("<span size=\"small\"></span>");
   actionsVBox.PackStart(l, false, false, 0);
   l.UseMarkup = true;
   l = new Label(
    string.Format(
     "<span size=\"x-large\">{0}</span>",
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
    string.Format("<span size=\"large\">{0}</span>",
         Util.GS("Create a new iFolder")));
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
    string.Format("<span size=\"large\">{0}</span>",
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
     "<span size=\"x-large\">{0}</span>",
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
    string.Format("<span size=\"large\">{0}</span>",
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
    string.Format("<span size=\"large\">{0}</span>",
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
    string.Format("<span size=\"large\">{0}</span>",
         Util.GS("Synchronize now")));
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
    string.Format("<span size=\"large\">{0}</span>",
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
    string.Format("<span size=\"large\">{0}</span>",
         Util.GS("Change to a normal folder")));
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
    string.Format("<span size=\"large\">{0}</span>",
         Util.GS("View properties...")));
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
    string.Format("<span size=\"large\">{0}</span>",
         Util.GS("Download and synchronize")));
   hbox.PackStart(buttonText, true, true, 4);
   buttonText.UseMarkup = true;
   buttonText.UseUnderline = false;
   buttonText.Xalign = 0;
   DownloadAvailableiFolderButton.Clicked +=
    new EventHandler(DownloadAvailableiFolderHandler);
   hbox = new HBox(false, 0);
   DeleteFromServerButton = new Button(hbox);
   vbox.PackStart(DeleteFromServerButton, false, false, 0);
   DeleteFromServerButton.Relief = ReliefStyle.None;
   buttonText = new Label(
    string.Format("<span size=\"large\">{0}</span>",
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
    string.Format("<span size=\"large\">{0}</span>",
         Util.GS("Remove my membership")));
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
   localGroup = new iFolderViewGroup(Util.GS("iFolders on This Computer"), myiFoldersFilter);
   iFoldersIconView.AddGroup(localGroup);
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
   TargetEntry[] targets =
    new TargetEntry[]
    {
                 new TargetEntry ("text/uri-list", 0, (uint) TargetType.UriList),
                 new TargetEntry ("application/x-root-window-drop", 0, (uint) TargetType.RootWindow)
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
   iFoldersScrolledWindow.AddWithViewport(iFoldersIconView);
   return iFoldersScrolledWindow;
  }
  private void OnAddNewAccount(object o, EventArgs args)
  {
   Util.ShowPrefsPage(1, simiasManager);
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
  private void OnConnectToServerButton(object o, EventArgs args)
  {
   AddAccountWizard aaw = new AddAccountWizard(simws);
   aaw.TransientFor = this;
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
  private void AddiFolderHandler(object o, EventArgs args)
  {
   CreateNewiFolder();
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
       if (domain == null ||
        domain.MemberUserID == holder.iFolder.OwnerID)
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
       new MenuItem(Util.GS("Download and synchronize"));
      menu.Append(item_download);
      item_download.Activated += new EventHandler(
        DownloadAvailableiFolderHandler);
      menu.Append(new SeparatorMenuItem());
      DomainInformation domain =
       domainController.GetDomain(holder.iFolder.DomainID);
      if (domain == null ||
       domain.MemberUserID == holder.iFolder.OwnerID)
      {
       MenuItem item_delete = new MenuItem (
         Util.GS("Delete from server"));
       menu.Append (item_delete);
       item_delete.Activated += new EventHandler(
         DeleteFromServerHandler);
      }
      else
      {
       MenuItem item_remove_membership = new MenuItem (
         Util.GS("Remove my membership"));
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
       new MenuItem(Util.GS("Synchronize now"));
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
         Util.GS("Change to a normal folder"));
       menu.Append (item_revert);
       item_revert.Activated += new EventHandler(
         RemoveiFolderHandler);
      }
      else if(holder.iFolder.OwnerID !=
          holder.iFolder.CurrentUserID)
      {
       MenuItem item_delete = new MenuItem (
         Util.GS("Change to a normal folder"));
       menu.Append (item_delete);
       item_delete.Activated += new EventHandler(
         RemoveiFolderHandler);
      }
      menu.Append(new SeparatorMenuItem());
      MenuItem item_properties =
       new MenuItem (Util.GS("Properties"));
      menu.Append (item_properties);
      item_properties.Activated +=
       new EventHandler(OnShowFolderProperties);
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
Console.WriteLine("iFolderWindow.OniFolderIconViewSelectionChanged()");
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
   Console.WriteLine("OnIconViewDragDataReceived: {0}", (TargetType)args.Info);
   bool bFolderCreated = false;
   switch (args.Info)
   {
    case (uint) TargetType.UriList:
     DomainInformation defaultDomain = domainController.GetDefaultDomain();
     if (defaultDomain == null) return;
     UriList uriList = new UriList(args.SelectionData);
     foreach (string path in uriList.ToLocalPaths())
     {
      if (ifws.CanBeiFolder(path))
      {
       try
       {
        ifdata.CreateiFolder(path, defaultDomain.ID);
        bFolderCreated = true;
       }
       catch
       {
        Console.WriteLine("Error creating folder on drag-and-drop");
       }
      }
     }
     break;
    default:
     break;
   }
   Gtk.Drag.Finish (args.Context, bFolderCreated, false, args.Time);
  }
  private void OnRealizeWidget(object o, EventArgs args)
  {
   if (domainController.GetDomains().Length == 0)
    WindowNotebook.CurrentPage = 0;
   else
    WindowNotebook.CurrentPage = 1;
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
       Util.ShowHelp("front.html", this);
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
   Util.ShowHelp("front.html", this);
  }
  private void OnAbout(object o, EventArgs args)
  {
   Util.ShowAbout();
  }
  private void OnDomainAddedEvent(object sender, DomainEventArgs args)
  {
   if (domainController.GetDomains().Length == 0)
    WindowNotebook.CurrentPage = 0;
   else
    WindowNotebook.CurrentPage = 1;
   AddServerGroup(args.DomainID);
   RefilterServerGroups();
  }
  private void OnDomainDeletedEvent(object sender, DomainEventArgs args)
  {
Console.WriteLine("iFolderWindow.DomainDeletedEvent()");
   if (domainController.GetDomains().Length == 0)
    WindowNotebook.CurrentPage = 0;
   else
    WindowNotebook.CurrentPage = 1;
   if (serverGroups.ContainsKey(args.DomainID))
   {
Console.WriteLine("\tremoving the group from the iFolderIconView");
    iFolderViewGroup group =
     (iFolderViewGroup)serverGroups[args.DomainID];
    iFoldersIconView.RemoveGroup(group);
    serverGroups.Remove(args.DomainID);
    group.Destroy();
   }
   if (serverGroupFilters.ContainsKey(args.DomainID))
   {
Console.WriteLine("\tremoving the group from the serverGroupFilters Hashtable");
    serverGroupFilters.Remove(args.DomainID);
   }
   RefilterServerGroups();
  }
  private void ShowAvailableiFolders()
  {
   foreach(iFolderViewGroup group in serverGroups.Values)
   {
    iFoldersIconView.AddGroup(group);
   }
   ShowHideAllFoldersButtonText.Markup =
    string.Format("<span size=\"large\">{0}</span>",
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
    string.Format("<span size=\"large\">{0}</span>",
         Util.GS("View available iFolders"));
   bAvailableFoldersShowing = false;
   ViewServeriFoldersMenuItem.Toggled -= new EventHandler(OnToggleViewServeriFoldersMenuItem);
   ViewServeriFoldersMenuItem.Active = false;
   ViewServeriFoldersMenuItem.Toggled += new EventHandler(OnToggleViewServeriFoldersMenuItem);
  }
  private void DownloadSelectedFolder()
  {
   iFolderHolder holder = iFoldersIconView.SelectedFolder;
   DownloadiFolder(holder);
  }
  private void DownloadiFolder(iFolderHolder holder)
  {
   if (holder != null && holder.iFolder.IsSubscription)
   {
    string newPath = "";
                int rc = 0;
                do
                {
                    iFolderAcceptDialog iad =
                            new iFolderAcceptDialog(holder.iFolder, Util.LastSetupPath);
                    iad.TransientFor = this;
                    rc = iad.Run();
                    newPath = ParseAndReplaceTildeInPath(iad.Path);
                    iad.Hide();
                    iad.Destroy();
                    if(rc != -5)
                        return;
                    try
                    {
                        ifdata.AcceptiFolderInvitation(
                holder.iFolder.ID,
                holder.iFolder.DomainID,
                newPath);
                        rc = 0;
                        Util.LastSetupPath = newPath;
                    }
                    catch(Exception e)
                    {
                        DisplayCreateOrSetupException(e);
                    }
                }
                while(rc == -5);
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
     ifdata.DeleteiFolder(holder.iFolder.ID);
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
Console.WriteLine("iFolderWindow.AddServerGroup(DomainID: {0})", domainID);
   if (serverGroups.ContainsKey(domainID)) return;
   DomainInformation domain = domainController.GetDomain(domainID);
   if (domain == null) return;
   iFolderServerFilter serverFilter =
    new iFolderServerFilter(domainID, SearchEntry);
   TreeModelFilter treeModelFilter = new TreeModelFilter(ifdata.iFolders, null);
   treeModelFilter.VisibleFunc = serverFilter.FilterFunc;
   iFolderViewGroup group =
    new iFolderViewGroup(
     string.Format(
      Util.GS("iFolders on {0}"),
      domain.Name),
     treeModelFilter);
   serverGroups[domainID] = group;
   serverGroupFilters[domainID] = serverFilter;
   group.VisibleWhenEmpty = false;
   if (bAvailableFoldersShowing)
    iFoldersIconView.AddGroup(group);
  }
  private bool SynchronizedFoldersFilterFunc(TreeModel model, TreeIter iter)
  {
   iFolderHolder ifHolder = (iFolderHolder)model.GetValue(iter, 0);
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
  private void UpdateSensitivity()
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
     DeleteFromServerButton.Visible = false;
     RemoveMembershipButton.Visible = false;
     OpenSynchronizedFolderButton.Visible = true;
     SynchronizeNowButton.Visible = true;
     ShareSynchronizedFolderButton.Visible = true;
     ViewFolderPropertiesButton.Visible = true;
     RemoveiFolderButton.Visible = true;
     if (holder.iFolder.HasConflicts)
      ResolveConflictsButton.Visible = true;
     else
      ResolveConflictsButton.Visible = false;
    }
    SynchronizedFolderTasks.Visible = true;
    RemoveiFolderButton.Sensitive = true;
   }
  }
  private void UpdateMenuSensitivity(iFolderHolder holder)
  {
   if (holder != null)
   {
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
     ShareMenuItem.Sensitive = true;
     OpenMenuItem.Sensitive = true;
     SyncNowMenuItem.Sensitive = true;
     if (holder.iFolder.Role.Equals("Master"))
      RevertMenuItem.Sensitive = false;
     else
      RevertMenuItem.Sensitive = true;
     PropMenuItem.Sensitive = true;
     DeleteMenuItem.Sensitive = false;
     RemoveMenuItem.Sensitive = false;
    }
    else
    {
     DownloadMenuItem.Sensitive = true;
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
    ConflictMenuItem.Sensitive = false;
    RevertMenuItem.Sensitive = false;
    DeleteMenuItem.Sensitive = false;
    RemoveMenuItem.Sensitive = false;
    RemoveMenuItem.Visible = false;
    PropMenuItem.Sensitive = false;
    DownloadMenuItem.Sensitive = false;
   }
  }
  private void RefreshiFolders(bool bReadFromSimias)
  {
   ifdata.Refresh();
   domainController.CheckForNewiFolders();
  }
  private void CloseWindow()
  {
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
  private void ResolveConflicts(iFolderHolder holder)
  {
   if (holder == null) return;
   iFolderConflictDialog conflictDialog =
    (iFolderConflictDialog) ConflictDialogs[holder.iFolder.ID];
   if (conflictDialog == null)
   {
    try
    {
     conflictDialog =
      new iFolderConflictDialog(
       this, holder.iFolder, ifws, simws);
     conflictDialog.SetPosition(WindowPosition.Center);
     conflictDialog.Response +=
      new ResponseHandler(OnConflictDialogResponse);
     conflictDialog.ShowAll();
     ConflictDialogs[holder.iFolder.ID] = conflictDialog;
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
    ShowFolderProperties(holder, 1);
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
     "",
     Util.GS("Change this iFolder back to a normal folder?"),
     Util.GS("The folder will still be on your computer, but it will no longer synchronize with the iFolder Server."));
    int rc = dialog.Run();
    dialog.Hide();
    dialog.Destroy();
    if(rc == -8)
    {
     try
     {
      ifdata.RevertiFolder(holder.iFolder.ID);
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
       ifHolder =
        ifdata.CreateiFolder(selectedFolder,
              selectedDomain);
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
      if(ClientConfig.Get(ClientConfig.KEY_SHOW_CREATION,
          "true") == "true")
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
         ClientConfig.KEY_SHOW_CREATION, "false");
       }
       cd.Destroy();
       cd = null;
      }
     }
     catch (Exception e)
     {
      Console.WriteLine(e.Message);
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
   DownloadiFolder(holder);
  }
  public void ResolveConflicts(string ifolderID)
  {
   if (ifolderID == null) return;
   iFolderHolder holder = ifdata.GetiFolder(ifolderID);
   if (holder != null)
    ResolveConflicts(holder);
  }
  public void HandleSyncEvent(CollectionSyncEventArgs args)
  {
   switch(args.Action)
   {
    case Simias.Client.Event.Action.StartLocalSync:
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
     break;
    }
   }
  }
  public void HandleFileSyncEvent(FileSyncEventArgs args)
  {
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
     Console.WriteLine ("uri = {0}", s);
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
