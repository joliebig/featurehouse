

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;
using System.Diagnostics;
using System.Xml;
using Microsoft.Win32;
using Novell.iFolderCom;
using Novell.Win32Util;
using Simias.Client;
using Simias.Client.Event;

namespace Novell.FormsTrayApp
{



 public class GlobalProperties : System.Windows.Forms.Form
 {

  private const string myiFoldersX = "MyiFoldersX";
  private const string myiFoldersY = "MyiFoldersY";


  private delegate void SyncCollectionDelegate(CollectionSyncEventArgs syncEventArgs);
  private SyncCollectionDelegate syncCollectionDelegate;
  private delegate void SyncFileDelegate(FileSyncEventArgs syncEventArgs);
  private SyncFileDelegate syncFileDelegate;




  public delegate void CreateChangeEventDelegate(iFolderWeb ifolder, string eventData);



  public CreateChangeEventDelegate createChangeEventDelegate;




  public delegate void DeleteEventDelegate(NodeEventArgs args);



  public DeleteEventDelegate deleteEventDelegate;




  public delegate void RemoveDomainDelegate(object sender, DomainRemoveEventArgs e);



  public event RemoveDomainDelegate RemoveDomain;

  System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(GlobalProperties));
  private Preferences preferences;
  private SyncLog syncLog;
  private System.Timers.Timer updateEnterpriseTimer;
  private Hashtable ht;
  private uint objectsToSync;
  private bool startSync;
  private iFolderWebService ifWebService;
  private SimiasWebService simiasWebService;
  private IProcEventClient eventClient;
  private bool initialConnect = false;
  private bool shutdown = false;
  private bool initialPositionSet = false;
  private Domain defaultDomain = null;
  private string domainList;
  private System.Windows.Forms.ListView iFolderView;
  private System.Windows.Forms.ColumnHeader columnHeader1;
  private System.Windows.Forms.ContextMenu contextMenu1;
  private System.Windows.Forms.MenuItem menuOpen;
  private System.Windows.Forms.MenuItem menuCreate;
  private System.Windows.Forms.MenuItem menuShare;
  private System.Windows.Forms.MenuItem menuRevert;
  private System.Windows.Forms.MenuItem menuProperties;
  private System.Windows.Forms.MenuItem menuRefresh;
  private System.Windows.Forms.MenuItem menuSeparator1;
  private System.Windows.Forms.MenuItem menuSeparator2;
  private System.Windows.Forms.ColumnHeader columnHeader4;
  private System.Windows.Forms.ColumnHeader columnHeader5;
  private System.Windows.Forms.MenuItem menuSyncNow;
  private System.Windows.Forms.MainMenu mainMenu1;
  private System.Windows.Forms.MenuItem menuAction;
  private System.Windows.Forms.MenuItem menuView;
  private System.Windows.Forms.MenuItem menuViewRefresh;
  private System.Windows.Forms.MenuItem menuActionOpen;
  private System.Windows.Forms.MenuItem menuActionCreate;
  private System.Windows.Forms.MenuItem menuActionRevert;
  private System.Windows.Forms.MenuItem menuActionShare;
  private System.Windows.Forms.MenuItem menuActionSync;
  private System.Windows.Forms.MenuItem menuActionProperties;
  private System.Windows.Forms.MenuItem menuActionSeparator1;
  private System.Windows.Forms.MenuItem menuResolve;
  private System.Windows.Forms.MenuItem menuActionResolve;
  private System.Windows.Forms.MenuItem menuAccept;
  private System.Windows.Forms.MenuItem menuActionAccept;
  private System.Windows.Forms.MenuItem menuActionSeparator2;
  private System.Windows.Forms.MenuItem menuExit;
  private System.Windows.Forms.MenuItem menuItem4;
  private System.Windows.Forms.MenuItem menuHelp;
  private System.Windows.Forms.MenuItem menuHelpHelp;
  private System.Windows.Forms.MenuItem menuHelpAbout;
  private System.Windows.Forms.ProgressBar progressBar1;
  private System.Windows.Forms.MenuItem menuRemove;
  private System.Windows.Forms.MenuItem menuActionRemove;
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.ComboBox servers;
  private ToolBarEx toolBar1;
  private System.Windows.Forms.ToolBarButton toolBarCreate;
  private System.Windows.Forms.ToolBarButton toolBarSetup;
  private System.Windows.Forms.ToolBarButton toolBarResolve;
  private System.Windows.Forms.ToolBarButton toolBarSync;
  private System.Windows.Forms.ToolBarButton toolBarShare;
  private System.Windows.Forms.Panel panel1;
  private System.Windows.Forms.StatusBar statusBar1;
  private System.Windows.Forms.GroupBox groupBox1;
  private System.Windows.Forms.MenuItem menuItem1;
  private System.Windows.Forms.MenuItem menuViewAccounts;
  private System.Windows.Forms.MenuItem menuViewLog;
  private System.Windows.Forms.MenuItem menuEdit;
  private System.Windows.Forms.MenuItem menuEditPrefs;
  private System.ComponentModel.IContainer components;





  public GlobalProperties(iFolderWebService ifolderWebService, SimiasWebService simiasWebService, IProcEventClient eventClient)
  {
   syncCollectionDelegate = new SyncCollectionDelegate(syncCollection);
   syncFileDelegate = new SyncFileDelegate(syncFile);
   createChangeEventDelegate = new CreateChangeEventDelegate(createChangeEvent);
   deleteEventDelegate = new DeleteEventDelegate(deleteEvent);




   InitializeComponent();

   progressBar1.Visible = false;

   ifWebService = ifolderWebService;
   this.simiasWebService = simiasWebService;
   this.eventClient = eventClient;



   eventClient.SetEvent(IProcEventAction.AddCollectionSync, new IProcEventHandler(global_collectionSyncHandler));
   eventClient.SetEvent(IProcEventAction.AddFileSync, new IProcEventHandler(global_fileSyncHandler));

   updateEnterpriseTimer = new System.Timers.Timer(1000);
   updateEnterpriseTimer.AutoReset = false;
   updateEnterpriseTimer.Elapsed += new System.Timers.ElapsedEventHandler(updateEnterpriseTimer_Elapsed);
   ht = new Hashtable();

   progressBar1.Minimum = 0;

   this.StartPosition = FormStartPosition.CenterScreen;


   try
   {
    this.Icon = new Icon(Path.Combine(Application.StartupPath, @"ifolder_app.ico"));






    this.iFolderView.SmallImageList = new ImageList();
    iFolderView.SmallImageList.Images.Add(new Icon(Path.Combine(Application.StartupPath, @"res\ifolder_loaded.ico")));
    iFolderView.SmallImageList.Images.Add(new Icon(Path.Combine(Application.StartupPath, @"res\serverifolder.ico")));
    iFolderView.SmallImageList.Images.Add(new Icon(Path.Combine(Application.StartupPath, @"res\ifolderconflict.ico")));


    toolBar1.ImageList = new ImageList();
    toolBar1.ImageList.ImageSize = new Size(24, 24);
    toolBar1.ImageList.TransparentColor = Color.White;
    toolBar1.ImageList.Images.AddStrip(Image.FromFile(Path.Combine(Application.StartupPath, @"res\mtoolbar_nor.bmp")));


    toolBar1.DisabledImageList = new ImageList();
    toolBar1.DisabledImageList.ImageSize = new Size(24, 24);
    toolBar1.DisabledImageList.TransparentColor = Color.White;
    toolBar1.DisabledImageList.Images.AddStrip(Image.FromFile(Path.Combine(Application.StartupPath, @"res\mtoolbar_dis.bmp")));


    toolBar1.HotImageList = new ImageList();
    toolBar1.HotImageList.ImageSize = new Size(24, 24);
    toolBar1.HotImageList.TransparentColor = Color.White;
    toolBar1.HotImageList.Images.AddStrip(Image.FromFile(Path.Combine(Application.StartupPath, @"res\mtoolbar_hot.bmp")));

    toolBarCreate.ImageIndex = 0;
    toolBarSetup.ImageIndex = 1;
    toolBarShare.ImageIndex = 2;
    toolBarResolve.ImageIndex = 3;
    toolBarSync.ImageIndex = 4;
   }
   catch {}

   this.MinimumSize = this.Size;
  }




  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if(components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }






  private void InitializeComponent()
  {
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(GlobalProperties));
   this.servers = new System.Windows.Forms.ComboBox();
   this.label1 = new System.Windows.Forms.Label();
   this.iFolderView = new System.Windows.Forms.ListView();
   this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
   this.columnHeader4 = new System.Windows.Forms.ColumnHeader();
   this.columnHeader5 = new System.Windows.Forms.ColumnHeader();
   this.contextMenu1 = new System.Windows.Forms.ContextMenu();
   this.menuOpen = new System.Windows.Forms.MenuItem();
   this.menuCreate = new System.Windows.Forms.MenuItem();
   this.menuRefresh = new System.Windows.Forms.MenuItem();
   this.menuAccept = new System.Windows.Forms.MenuItem();
   this.menuSeparator1 = new System.Windows.Forms.MenuItem();
   this.menuShare = new System.Windows.Forms.MenuItem();
   this.menuResolve = new System.Windows.Forms.MenuItem();
   this.menuSyncNow = new System.Windows.Forms.MenuItem();
   this.menuRevert = new System.Windows.Forms.MenuItem();
   this.menuRemove = new System.Windows.Forms.MenuItem();
   this.menuSeparator2 = new System.Windows.Forms.MenuItem();
   this.menuProperties = new System.Windows.Forms.MenuItem();
   this.mainMenu1 = new System.Windows.Forms.MainMenu();
   this.menuAction = new System.Windows.Forms.MenuItem();
   this.menuActionCreate = new System.Windows.Forms.MenuItem();
   this.menuActionSeparator1 = new System.Windows.Forms.MenuItem();
   this.menuActionAccept = new System.Windows.Forms.MenuItem();
   this.menuActionRemove = new System.Windows.Forms.MenuItem();
   this.menuActionSeparator2 = new System.Windows.Forms.MenuItem();
   this.menuActionOpen = new System.Windows.Forms.MenuItem();
   this.menuActionShare = new System.Windows.Forms.MenuItem();
   this.menuActionResolve = new System.Windows.Forms.MenuItem();
   this.menuActionSync = new System.Windows.Forms.MenuItem();
   this.menuActionRevert = new System.Windows.Forms.MenuItem();
   this.menuActionProperties = new System.Windows.Forms.MenuItem();
   this.menuItem4 = new System.Windows.Forms.MenuItem();
   this.menuExit = new System.Windows.Forms.MenuItem();
   this.menuView = new System.Windows.Forms.MenuItem();
   this.menuViewRefresh = new System.Windows.Forms.MenuItem();
   this.menuHelp = new System.Windows.Forms.MenuItem();
   this.menuHelpHelp = new System.Windows.Forms.MenuItem();
   this.menuHelpAbout = new System.Windows.Forms.MenuItem();
   this.progressBar1 = new System.Windows.Forms.ProgressBar();
   this.toolBar1 = new Novell.FormsTrayApp.ToolBarEx();
   this.toolBarCreate = new System.Windows.Forms.ToolBarButton();
   this.toolBarSetup = new System.Windows.Forms.ToolBarButton();
   this.toolBarShare = new System.Windows.Forms.ToolBarButton();
   this.toolBarResolve = new System.Windows.Forms.ToolBarButton();
   this.toolBarSync = new System.Windows.Forms.ToolBarButton();
   this.panel1 = new System.Windows.Forms.Panel();
   this.groupBox1 = new System.Windows.Forms.GroupBox();
   this.statusBar1 = new System.Windows.Forms.StatusBar();
   this.menuItem1 = new System.Windows.Forms.MenuItem();
   this.menuViewAccounts = new System.Windows.Forms.MenuItem();
   this.menuViewLog = new System.Windows.Forms.MenuItem();
   this.menuEdit = new System.Windows.Forms.MenuItem();
   this.menuEditPrefs = new System.Windows.Forms.MenuItem();
   this.panel1.SuspendLayout();
   this.SuspendLayout();



   this.servers.AccessibleDescription = resources.GetString("servers.AccessibleDescription");
   this.servers.AccessibleName = resources.GetString("servers.AccessibleName");
   this.servers.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("servers.Anchor")));
   this.servers.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("servers.BackgroundImage")));
   this.servers.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("servers.Dock")));
   this.servers.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
   this.servers.Enabled = ((bool)(resources.GetObject("servers.Enabled")));
   this.servers.Font = ((System.Drawing.Font)(resources.GetObject("servers.Font")));
   this.servers.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("servers.ImeMode")));
   this.servers.IntegralHeight = ((bool)(resources.GetObject("servers.IntegralHeight")));
   this.servers.ItemHeight = ((int)(resources.GetObject("servers.ItemHeight")));
   this.servers.Location = ((System.Drawing.Point)(resources.GetObject("servers.Location")));
   this.servers.MaxDropDownItems = ((int)(resources.GetObject("servers.MaxDropDownItems")));
   this.servers.MaxLength = ((int)(resources.GetObject("servers.MaxLength")));
   this.servers.Name = "servers";
   this.servers.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("servers.RightToLeft")));
   this.servers.Size = ((System.Drawing.Size)(resources.GetObject("servers.Size")));
   this.servers.TabIndex = ((int)(resources.GetObject("servers.TabIndex")));
   this.servers.Text = resources.GetString("servers.Text");
   this.servers.Visible = ((bool)(resources.GetObject("servers.Visible")));
   this.servers.SelectedIndexChanged += new System.EventHandler(this.servers_SelectedIndexChanged);



   this.label1.AccessibleDescription = resources.GetString("label1.AccessibleDescription");
   this.label1.AccessibleName = resources.GetString("label1.AccessibleName");
   this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label1.Anchor")));
   this.label1.AutoSize = ((bool)(resources.GetObject("label1.AutoSize")));
   this.label1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label1.Dock")));
   this.label1.Enabled = ((bool)(resources.GetObject("label1.Enabled")));
   this.label1.Font = ((System.Drawing.Font)(resources.GetObject("label1.Font")));
   this.label1.Image = ((System.Drawing.Image)(resources.GetObject("label1.Image")));
   this.label1.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label1.ImageAlign")));
   this.label1.ImageIndex = ((int)(resources.GetObject("label1.ImageIndex")));
   this.label1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label1.ImeMode")));
   this.label1.Location = ((System.Drawing.Point)(resources.GetObject("label1.Location")));
   this.label1.Name = "label1";
   this.label1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label1.RightToLeft")));
   this.label1.Size = ((System.Drawing.Size)(resources.GetObject("label1.Size")));
   this.label1.TabIndex = ((int)(resources.GetObject("label1.TabIndex")));
   this.label1.Text = resources.GetString("label1.Text");
   this.label1.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label1.TextAlign")));
   this.label1.Visible = ((bool)(resources.GetObject("label1.Visible")));



   this.iFolderView.AccessibleDescription = resources.GetString("iFolderView.AccessibleDescription");
   this.iFolderView.AccessibleName = resources.GetString("iFolderView.AccessibleName");
   this.iFolderView.Alignment = ((System.Windows.Forms.ListViewAlignment)(resources.GetObject("iFolderView.Alignment")));
   this.iFolderView.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("iFolderView.Anchor")));
   this.iFolderView.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("iFolderView.BackgroundImage")));
   this.iFolderView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
                        this.columnHeader1,
                        this.columnHeader4,
                        this.columnHeader5});
   this.iFolderView.ContextMenu = this.contextMenu1;
   this.iFolderView.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("iFolderView.Dock")));
   this.iFolderView.Enabled = ((bool)(resources.GetObject("iFolderView.Enabled")));
   this.iFolderView.Font = ((System.Drawing.Font)(resources.GetObject("iFolderView.Font")));
   this.iFolderView.FullRowSelect = true;
   this.iFolderView.HideSelection = false;
   this.iFolderView.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("iFolderView.ImeMode")));
   this.iFolderView.LabelWrap = ((bool)(resources.GetObject("iFolderView.LabelWrap")));
   this.iFolderView.Location = ((System.Drawing.Point)(resources.GetObject("iFolderView.Location")));
   this.iFolderView.MultiSelect = false;
   this.iFolderView.Name = "iFolderView";
   this.iFolderView.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("iFolderView.RightToLeft")));
   this.iFolderView.Size = ((System.Drawing.Size)(resources.GetObject("iFolderView.Size")));
   this.iFolderView.TabIndex = ((int)(resources.GetObject("iFolderView.TabIndex")));
   this.iFolderView.Text = resources.GetString("iFolderView.Text");
   this.iFolderView.View = System.Windows.Forms.View.Details;
   this.iFolderView.Visible = ((bool)(resources.GetObject("iFolderView.Visible")));
   this.iFolderView.DoubleClick += new System.EventHandler(this.iFolderView_DoubleClick);
   this.iFolderView.SelectedIndexChanged += new System.EventHandler(this.iFolderView_SelectedIndexChanged);



   this.columnHeader1.Text = resources.GetString("columnHeader1.Text");
   this.columnHeader1.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader1.TextAlign")));
   this.columnHeader1.Width = ((int)(resources.GetObject("columnHeader1.Width")));



   this.columnHeader4.Text = resources.GetString("columnHeader4.Text");
   this.columnHeader4.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader4.TextAlign")));
   this.columnHeader4.Width = ((int)(resources.GetObject("columnHeader4.Width")));



   this.columnHeader5.Text = resources.GetString("columnHeader5.Text");
   this.columnHeader5.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader5.TextAlign")));
   this.columnHeader5.Width = ((int)(resources.GetObject("columnHeader5.Width")));



   this.contextMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                       this.menuOpen,
                       this.menuCreate,
                       this.menuRefresh,
                       this.menuAccept,
                       this.menuSeparator1,
                       this.menuShare,
                       this.menuResolve,
                       this.menuSyncNow,
                       this.menuRevert,
                       this.menuRemove,
                       this.menuSeparator2,
                       this.menuProperties});
   this.contextMenu1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("contextMenu1.RightToLeft")));



   this.menuOpen.DefaultItem = true;
   this.menuOpen.Enabled = ((bool)(resources.GetObject("menuOpen.Enabled")));
   this.menuOpen.Index = 0;
   this.menuOpen.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuOpen.Shortcut")));
   this.menuOpen.ShowShortcut = ((bool)(resources.GetObject("menuOpen.ShowShortcut")));
   this.menuOpen.Text = resources.GetString("menuOpen.Text");
   this.menuOpen.Visible = ((bool)(resources.GetObject("menuOpen.Visible")));
   this.menuOpen.Click += new System.EventHandler(this.menuOpen_Click);



   this.menuCreate.Enabled = ((bool)(resources.GetObject("menuCreate.Enabled")));
   this.menuCreate.Index = 1;
   this.menuCreate.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuCreate.Shortcut")));
   this.menuCreate.ShowShortcut = ((bool)(resources.GetObject("menuCreate.ShowShortcut")));
   this.menuCreate.Text = resources.GetString("menuCreate.Text");
   this.menuCreate.Visible = ((bool)(resources.GetObject("menuCreate.Visible")));
   this.menuCreate.Click += new System.EventHandler(this.menuCreate_Click);



   this.menuRefresh.Enabled = ((bool)(resources.GetObject("menuRefresh.Enabled")));
   this.menuRefresh.Index = 2;
   this.menuRefresh.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuRefresh.Shortcut")));
   this.menuRefresh.ShowShortcut = ((bool)(resources.GetObject("menuRefresh.ShowShortcut")));
   this.menuRefresh.Text = resources.GetString("menuRefresh.Text");
   this.menuRefresh.Visible = ((bool)(resources.GetObject("menuRefresh.Visible")));
   this.menuRefresh.Click += new System.EventHandler(this.menuRefresh_Click);



   this.menuAccept.DefaultItem = true;
   this.menuAccept.Enabled = ((bool)(resources.GetObject("menuAccept.Enabled")));
   this.menuAccept.Index = 3;
   this.menuAccept.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuAccept.Shortcut")));
   this.menuAccept.ShowShortcut = ((bool)(resources.GetObject("menuAccept.ShowShortcut")));
   this.menuAccept.Text = resources.GetString("menuAccept.Text");
   this.menuAccept.Visible = ((bool)(resources.GetObject("menuAccept.Visible")));
   this.menuAccept.Click += new System.EventHandler(this.menuAccept_Click);



   this.menuSeparator1.Enabled = ((bool)(resources.GetObject("menuSeparator1.Enabled")));
   this.menuSeparator1.Index = 4;
   this.menuSeparator1.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuSeparator1.Shortcut")));
   this.menuSeparator1.ShowShortcut = ((bool)(resources.GetObject("menuSeparator1.ShowShortcut")));
   this.menuSeparator1.Text = resources.GetString("menuSeparator1.Text");
   this.menuSeparator1.Visible = ((bool)(resources.GetObject("menuSeparator1.Visible")));



   this.menuShare.Enabled = ((bool)(resources.GetObject("menuShare.Enabled")));
   this.menuShare.Index = 5;
   this.menuShare.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuShare.Shortcut")));
   this.menuShare.ShowShortcut = ((bool)(resources.GetObject("menuShare.ShowShortcut")));
   this.menuShare.Text = resources.GetString("menuShare.Text");
   this.menuShare.Visible = ((bool)(resources.GetObject("menuShare.Visible")));
   this.menuShare.Click += new System.EventHandler(this.menuShare_Click);



   this.menuResolve.Enabled = ((bool)(resources.GetObject("menuResolve.Enabled")));
   this.menuResolve.Index = 6;
   this.menuResolve.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuResolve.Shortcut")));
   this.menuResolve.ShowShortcut = ((bool)(resources.GetObject("menuResolve.ShowShortcut")));
   this.menuResolve.Text = resources.GetString("menuResolve.Text");
   this.menuResolve.Visible = ((bool)(resources.GetObject("menuResolve.Visible")));
   this.menuResolve.Click += new System.EventHandler(this.menuResolve_Click);



   this.menuSyncNow.Enabled = ((bool)(resources.GetObject("menuSyncNow.Enabled")));
   this.menuSyncNow.Index = 7;
   this.menuSyncNow.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuSyncNow.Shortcut")));
   this.menuSyncNow.ShowShortcut = ((bool)(resources.GetObject("menuSyncNow.ShowShortcut")));
   this.menuSyncNow.Text = resources.GetString("menuSyncNow.Text");
   this.menuSyncNow.Visible = ((bool)(resources.GetObject("menuSyncNow.Visible")));
   this.menuSyncNow.Click += new System.EventHandler(this.menuSyncNow_Click);



   this.menuRevert.Enabled = ((bool)(resources.GetObject("menuRevert.Enabled")));
   this.menuRevert.Index = 8;
   this.menuRevert.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuRevert.Shortcut")));
   this.menuRevert.ShowShortcut = ((bool)(resources.GetObject("menuRevert.ShowShortcut")));
   this.menuRevert.Text = resources.GetString("menuRevert.Text");
   this.menuRevert.Visible = ((bool)(resources.GetObject("menuRevert.Visible")));
   this.menuRevert.Click += new System.EventHandler(this.menuRevert_Click);



   this.menuRemove.Enabled = ((bool)(resources.GetObject("menuRemove.Enabled")));
   this.menuRemove.Index = 9;
   this.menuRemove.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuRemove.Shortcut")));
   this.menuRemove.ShowShortcut = ((bool)(resources.GetObject("menuRemove.ShowShortcut")));
   this.menuRemove.Text = resources.GetString("menuRemove.Text");
   this.menuRemove.Visible = ((bool)(resources.GetObject("menuRemove.Visible")));
   this.menuRemove.Click += new System.EventHandler(this.menuRemove_Click);



   this.menuSeparator2.Enabled = ((bool)(resources.GetObject("menuSeparator2.Enabled")));
   this.menuSeparator2.Index = 10;
   this.menuSeparator2.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuSeparator2.Shortcut")));
   this.menuSeparator2.ShowShortcut = ((bool)(resources.GetObject("menuSeparator2.ShowShortcut")));
   this.menuSeparator2.Text = resources.GetString("menuSeparator2.Text");
   this.menuSeparator2.Visible = ((bool)(resources.GetObject("menuSeparator2.Visible")));



   this.menuProperties.Enabled = ((bool)(resources.GetObject("menuProperties.Enabled")));
   this.menuProperties.Index = 11;
   this.menuProperties.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuProperties.Shortcut")));
   this.menuProperties.ShowShortcut = ((bool)(resources.GetObject("menuProperties.ShowShortcut")));
   this.menuProperties.Text = resources.GetString("menuProperties.Text");
   this.menuProperties.Visible = ((bool)(resources.GetObject("menuProperties.Visible")));
   this.menuProperties.Click += new System.EventHandler(this.menuProperties_Click);



   this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                       this.menuAction,
                       this.menuEdit,
                       this.menuView,
                       this.menuHelp});
   this.mainMenu1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("mainMenu1.RightToLeft")));



   this.menuAction.Enabled = ((bool)(resources.GetObject("menuAction.Enabled")));
   this.menuAction.Index = 0;
   this.menuAction.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                        this.menuActionCreate,
                        this.menuActionSeparator1,
                        this.menuActionAccept,
                        this.menuActionRemove,
                        this.menuActionSeparator2,
                        this.menuActionOpen,
                        this.menuActionShare,
                        this.menuActionResolve,
                        this.menuActionSync,
                        this.menuActionRevert,
                        this.menuActionProperties,
                        this.menuItem4,
                        this.menuExit});
   this.menuAction.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuAction.Shortcut")));
   this.menuAction.ShowShortcut = ((bool)(resources.GetObject("menuAction.ShowShortcut")));
   this.menuAction.Text = resources.GetString("menuAction.Text");
   this.menuAction.Visible = ((bool)(resources.GetObject("menuAction.Visible")));



   this.menuActionCreate.Enabled = ((bool)(resources.GetObject("menuActionCreate.Enabled")));
   this.menuActionCreate.Index = 0;
   this.menuActionCreate.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionCreate.Shortcut")));
   this.menuActionCreate.ShowShortcut = ((bool)(resources.GetObject("menuActionCreate.ShowShortcut")));
   this.menuActionCreate.Text = resources.GetString("menuActionCreate.Text");
   this.menuActionCreate.Visible = ((bool)(resources.GetObject("menuActionCreate.Visible")));
   this.menuActionCreate.Click += new System.EventHandler(this.menuCreate_Click);



   this.menuActionSeparator1.Enabled = ((bool)(resources.GetObject("menuActionSeparator1.Enabled")));
   this.menuActionSeparator1.Index = 1;
   this.menuActionSeparator1.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionSeparator1.Shortcut")));
   this.menuActionSeparator1.ShowShortcut = ((bool)(resources.GetObject("menuActionSeparator1.ShowShortcut")));
   this.menuActionSeparator1.Text = resources.GetString("menuActionSeparator1.Text");
   this.menuActionSeparator1.Visible = ((bool)(resources.GetObject("menuActionSeparator1.Visible")));



   this.menuActionAccept.Enabled = ((bool)(resources.GetObject("menuActionAccept.Enabled")));
   this.menuActionAccept.Index = 2;
   this.menuActionAccept.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionAccept.Shortcut")));
   this.menuActionAccept.ShowShortcut = ((bool)(resources.GetObject("menuActionAccept.ShowShortcut")));
   this.menuActionAccept.Text = resources.GetString("menuActionAccept.Text");
   this.menuActionAccept.Visible = ((bool)(resources.GetObject("menuActionAccept.Visible")));
   this.menuActionAccept.Click += new System.EventHandler(this.menuAccept_Click);



   this.menuActionRemove.Enabled = ((bool)(resources.GetObject("menuActionRemove.Enabled")));
   this.menuActionRemove.Index = 3;
   this.menuActionRemove.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionRemove.Shortcut")));
   this.menuActionRemove.ShowShortcut = ((bool)(resources.GetObject("menuActionRemove.ShowShortcut")));
   this.menuActionRemove.Text = resources.GetString("menuActionRemove.Text");
   this.menuActionRemove.Visible = ((bool)(resources.GetObject("menuActionRemove.Visible")));
   this.menuActionRemove.Click += new System.EventHandler(this.menuRemove_Click);



   this.menuActionSeparator2.Enabled = ((bool)(resources.GetObject("menuActionSeparator2.Enabled")));
   this.menuActionSeparator2.Index = 4;
   this.menuActionSeparator2.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionSeparator2.Shortcut")));
   this.menuActionSeparator2.ShowShortcut = ((bool)(resources.GetObject("menuActionSeparator2.ShowShortcut")));
   this.menuActionSeparator2.Text = resources.GetString("menuActionSeparator2.Text");
   this.menuActionSeparator2.Visible = ((bool)(resources.GetObject("menuActionSeparator2.Visible")));



   this.menuActionOpen.Enabled = ((bool)(resources.GetObject("menuActionOpen.Enabled")));
   this.menuActionOpen.Index = 5;
   this.menuActionOpen.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionOpen.Shortcut")));
   this.menuActionOpen.ShowShortcut = ((bool)(resources.GetObject("menuActionOpen.ShowShortcut")));
   this.menuActionOpen.Text = resources.GetString("menuActionOpen.Text");
   this.menuActionOpen.Visible = ((bool)(resources.GetObject("menuActionOpen.Visible")));
   this.menuActionOpen.Click += new System.EventHandler(this.menuOpen_Click);



   this.menuActionShare.Enabled = ((bool)(resources.GetObject("menuActionShare.Enabled")));
   this.menuActionShare.Index = 6;
   this.menuActionShare.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionShare.Shortcut")));
   this.menuActionShare.ShowShortcut = ((bool)(resources.GetObject("menuActionShare.ShowShortcut")));
   this.menuActionShare.Text = resources.GetString("menuActionShare.Text");
   this.menuActionShare.Visible = ((bool)(resources.GetObject("menuActionShare.Visible")));
   this.menuActionShare.Click += new System.EventHandler(this.menuShare_Click);



   this.menuActionResolve.Enabled = ((bool)(resources.GetObject("menuActionResolve.Enabled")));
   this.menuActionResolve.Index = 7;
   this.menuActionResolve.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionResolve.Shortcut")));
   this.menuActionResolve.ShowShortcut = ((bool)(resources.GetObject("menuActionResolve.ShowShortcut")));
   this.menuActionResolve.Text = resources.GetString("menuActionResolve.Text");
   this.menuActionResolve.Visible = ((bool)(resources.GetObject("menuActionResolve.Visible")));
   this.menuActionResolve.Click += new System.EventHandler(this.menuResolve_Click);



   this.menuActionSync.Enabled = ((bool)(resources.GetObject("menuActionSync.Enabled")));
   this.menuActionSync.Index = 8;
   this.menuActionSync.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionSync.Shortcut")));
   this.menuActionSync.ShowShortcut = ((bool)(resources.GetObject("menuActionSync.ShowShortcut")));
   this.menuActionSync.Text = resources.GetString("menuActionSync.Text");
   this.menuActionSync.Visible = ((bool)(resources.GetObject("menuActionSync.Visible")));
   this.menuActionSync.Click += new System.EventHandler(this.menuSyncNow_Click);



   this.menuActionRevert.Enabled = ((bool)(resources.GetObject("menuActionRevert.Enabled")));
   this.menuActionRevert.Index = 9;
   this.menuActionRevert.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionRevert.Shortcut")));
   this.menuActionRevert.ShowShortcut = ((bool)(resources.GetObject("menuActionRevert.ShowShortcut")));
   this.menuActionRevert.Text = resources.GetString("menuActionRevert.Text");
   this.menuActionRevert.Visible = ((bool)(resources.GetObject("menuActionRevert.Visible")));
   this.menuActionRevert.Click += new System.EventHandler(this.menuRevert_Click);



   this.menuActionProperties.Enabled = ((bool)(resources.GetObject("menuActionProperties.Enabled")));
   this.menuActionProperties.Index = 10;
   this.menuActionProperties.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuActionProperties.Shortcut")));
   this.menuActionProperties.ShowShortcut = ((bool)(resources.GetObject("menuActionProperties.ShowShortcut")));
   this.menuActionProperties.Text = resources.GetString("menuActionProperties.Text");
   this.menuActionProperties.Visible = ((bool)(resources.GetObject("menuActionProperties.Visible")));
   this.menuActionProperties.Click += new System.EventHandler(this.menuProperties_Click);



   this.menuItem4.Enabled = ((bool)(resources.GetObject("menuItem4.Enabled")));
   this.menuItem4.Index = 11;
   this.menuItem4.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuItem4.Shortcut")));
   this.menuItem4.ShowShortcut = ((bool)(resources.GetObject("menuItem4.ShowShortcut")));
   this.menuItem4.Text = resources.GetString("menuItem4.Text");
   this.menuItem4.Visible = ((bool)(resources.GetObject("menuItem4.Visible")));



   this.menuExit.Enabled = ((bool)(resources.GetObject("menuExit.Enabled")));
   this.menuExit.Index = 12;
   this.menuExit.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuExit.Shortcut")));
   this.menuExit.ShowShortcut = ((bool)(resources.GetObject("menuExit.ShowShortcut")));
   this.menuExit.Text = resources.GetString("menuExit.Text");
   this.menuExit.Visible = ((bool)(resources.GetObject("menuExit.Visible")));
   this.menuExit.Click += new System.EventHandler(this.menuFileExit_Click);



   this.menuView.Enabled = ((bool)(resources.GetObject("menuView.Enabled")));
   this.menuView.Index = 2;
   this.menuView.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                      this.menuViewRefresh,
                      this.menuItem1,
                      this.menuViewAccounts,
                      this.menuViewLog});
   this.menuView.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuView.Shortcut")));
   this.menuView.ShowShortcut = ((bool)(resources.GetObject("menuView.ShowShortcut")));
   this.menuView.Text = resources.GetString("menuView.Text");
   this.menuView.Visible = ((bool)(resources.GetObject("menuView.Visible")));



   this.menuViewRefresh.Enabled = ((bool)(resources.GetObject("menuViewRefresh.Enabled")));
   this.menuViewRefresh.Index = 0;
   this.menuViewRefresh.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuViewRefresh.Shortcut")));
   this.menuViewRefresh.ShowShortcut = ((bool)(resources.GetObject("menuViewRefresh.ShowShortcut")));
   this.menuViewRefresh.Text = resources.GetString("menuViewRefresh.Text");
   this.menuViewRefresh.Visible = ((bool)(resources.GetObject("menuViewRefresh.Visible")));
   this.menuViewRefresh.Click += new System.EventHandler(this.menuRefresh_Click);



   this.menuHelp.Enabled = ((bool)(resources.GetObject("menuHelp.Enabled")));
   this.menuHelp.Index = 3;
   this.menuHelp.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                      this.menuHelpHelp,
                      this.menuHelpAbout});
   this.menuHelp.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuHelp.Shortcut")));
   this.menuHelp.ShowShortcut = ((bool)(resources.GetObject("menuHelp.ShowShortcut")));
   this.menuHelp.Text = resources.GetString("menuHelp.Text");
   this.menuHelp.Visible = ((bool)(resources.GetObject("menuHelp.Visible")));



   this.menuHelpHelp.Enabled = ((bool)(resources.GetObject("menuHelpHelp.Enabled")));
   this.menuHelpHelp.Index = 0;
   this.menuHelpHelp.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuHelpHelp.Shortcut")));
   this.menuHelpHelp.ShowShortcut = ((bool)(resources.GetObject("menuHelpHelp.ShowShortcut")));
   this.menuHelpHelp.Text = resources.GetString("menuHelpHelp.Text");
   this.menuHelpHelp.Visible = ((bool)(resources.GetObject("menuHelpHelp.Visible")));
   this.menuHelpHelp.Click += new System.EventHandler(this.menuHelpHelp_Click);



   this.menuHelpAbout.Enabled = ((bool)(resources.GetObject("menuHelpAbout.Enabled")));
   this.menuHelpAbout.Index = 1;
   this.menuHelpAbout.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuHelpAbout.Shortcut")));
   this.menuHelpAbout.ShowShortcut = ((bool)(resources.GetObject("menuHelpAbout.ShowShortcut")));
   this.menuHelpAbout.Text = resources.GetString("menuHelpAbout.Text");
   this.menuHelpAbout.Visible = ((bool)(resources.GetObject("menuHelpAbout.Visible")));
   this.menuHelpAbout.Click += new System.EventHandler(this.menuHelpAbout_Click);



   this.progressBar1.AccessibleDescription = resources.GetString("progressBar1.AccessibleDescription");
   this.progressBar1.AccessibleName = resources.GetString("progressBar1.AccessibleName");
   this.progressBar1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("progressBar1.Anchor")));
   this.progressBar1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("progressBar1.BackgroundImage")));
   this.progressBar1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("progressBar1.Dock")));
   this.progressBar1.Enabled = ((bool)(resources.GetObject("progressBar1.Enabled")));
   this.progressBar1.Font = ((System.Drawing.Font)(resources.GetObject("progressBar1.Font")));
   this.progressBar1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("progressBar1.ImeMode")));
   this.progressBar1.Location = ((System.Drawing.Point)(resources.GetObject("progressBar1.Location")));
   this.progressBar1.Name = "progressBar1";
   this.progressBar1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("progressBar1.RightToLeft")));
   this.progressBar1.Size = ((System.Drawing.Size)(resources.GetObject("progressBar1.Size")));
   this.progressBar1.TabIndex = ((int)(resources.GetObject("progressBar1.TabIndex")));
   this.progressBar1.Text = resources.GetString("progressBar1.Text");
   this.progressBar1.Visible = ((bool)(resources.GetObject("progressBar1.Visible")));



   this.toolBar1.AccessibleDescription = resources.GetString("toolBar1.AccessibleDescription");
   this.toolBar1.AccessibleName = resources.GetString("toolBar1.AccessibleName");
   this.toolBar1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("toolBar1.Anchor")));
   this.toolBar1.Appearance = ((System.Windows.Forms.ToolBarAppearance)(resources.GetObject("toolBar1.Appearance")));
   this.toolBar1.AutoSize = ((bool)(resources.GetObject("toolBar1.AutoSize")));
   this.toolBar1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("toolBar1.BackgroundImage")));
   this.toolBar1.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
                      this.toolBarCreate,
                      this.toolBarSetup,
                      this.toolBarShare,
                      this.toolBarResolve,
                      this.toolBarSync});
   this.toolBar1.ButtonSize = ((System.Drawing.Size)(resources.GetObject("toolBar1.ButtonSize")));
   this.toolBar1.DisabledImageList = null;
   this.toolBar1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("toolBar1.Dock")));
   this.toolBar1.DropDownArrows = ((bool)(resources.GetObject("toolBar1.DropDownArrows")));
   this.toolBar1.Enabled = ((bool)(resources.GetObject("toolBar1.Enabled")));
   this.toolBar1.Font = ((System.Drawing.Font)(resources.GetObject("toolBar1.Font")));
   this.toolBar1.HotImageList = null;
   this.toolBar1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("toolBar1.ImeMode")));
   this.toolBar1.Location = ((System.Drawing.Point)(resources.GetObject("toolBar1.Location")));
   this.toolBar1.Name = "toolBar1";
   this.toolBar1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("toolBar1.RightToLeft")));
   this.toolBar1.ShowToolTips = ((bool)(resources.GetObject("toolBar1.ShowToolTips")));
   this.toolBar1.Size = ((System.Drawing.Size)(resources.GetObject("toolBar1.Size")));
   this.toolBar1.TabIndex = ((int)(resources.GetObject("toolBar1.TabIndex")));
   this.toolBar1.TabStop = true;
   this.toolBar1.TextAlign = ((System.Windows.Forms.ToolBarTextAlign)(resources.GetObject("toolBar1.TextAlign")));
   this.toolBar1.Visible = ((bool)(resources.GetObject("toolBar1.Visible")));
   this.toolBar1.Wrappable = ((bool)(resources.GetObject("toolBar1.Wrappable")));
   this.toolBar1.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.toolBar1_ButtonClick);



   this.toolBarCreate.Enabled = ((bool)(resources.GetObject("toolBarCreate.Enabled")));
   this.toolBarCreate.ImageIndex = ((int)(resources.GetObject("toolBarCreate.ImageIndex")));
   this.toolBarCreate.Text = resources.GetString("toolBarCreate.Text");
   this.toolBarCreate.ToolTipText = resources.GetString("toolBarCreate.ToolTipText");
   this.toolBarCreate.Visible = ((bool)(resources.GetObject("toolBarCreate.Visible")));



   this.toolBarSetup.Enabled = ((bool)(resources.GetObject("toolBarSetup.Enabled")));
   this.toolBarSetup.ImageIndex = ((int)(resources.GetObject("toolBarSetup.ImageIndex")));
   this.toolBarSetup.Text = resources.GetString("toolBarSetup.Text");
   this.toolBarSetup.ToolTipText = resources.GetString("toolBarSetup.ToolTipText");
   this.toolBarSetup.Visible = ((bool)(resources.GetObject("toolBarSetup.Visible")));



   this.toolBarShare.Enabled = ((bool)(resources.GetObject("toolBarShare.Enabled")));
   this.toolBarShare.ImageIndex = ((int)(resources.GetObject("toolBarShare.ImageIndex")));
   this.toolBarShare.Text = resources.GetString("toolBarShare.Text");
   this.toolBarShare.ToolTipText = resources.GetString("toolBarShare.ToolTipText");
   this.toolBarShare.Visible = ((bool)(resources.GetObject("toolBarShare.Visible")));



   this.toolBarResolve.Enabled = ((bool)(resources.GetObject("toolBarResolve.Enabled")));
   this.toolBarResolve.ImageIndex = ((int)(resources.GetObject("toolBarResolve.ImageIndex")));
   this.toolBarResolve.Text = resources.GetString("toolBarResolve.Text");
   this.toolBarResolve.ToolTipText = resources.GetString("toolBarResolve.ToolTipText");
   this.toolBarResolve.Visible = ((bool)(resources.GetObject("toolBarResolve.Visible")));



   this.toolBarSync.Enabled = ((bool)(resources.GetObject("toolBarSync.Enabled")));
   this.toolBarSync.ImageIndex = ((int)(resources.GetObject("toolBarSync.ImageIndex")));
   this.toolBarSync.Text = resources.GetString("toolBarSync.Text");
   this.toolBarSync.ToolTipText = resources.GetString("toolBarSync.ToolTipText");
   this.toolBarSync.Visible = ((bool)(resources.GetObject("toolBarSync.Visible")));



   this.panel1.AccessibleDescription = resources.GetString("panel1.AccessibleDescription");
   this.panel1.AccessibleName = resources.GetString("panel1.AccessibleName");
   this.panel1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("panel1.Anchor")));
   this.panel1.AutoScroll = ((bool)(resources.GetObject("panel1.AutoScroll")));
   this.panel1.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("panel1.AutoScrollMargin")));
   this.panel1.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("panel1.AutoScrollMinSize")));
   this.panel1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("panel1.BackgroundImage")));
   this.panel1.Controls.Add(this.servers);
   this.panel1.Controls.Add(this.label1);
   this.panel1.Controls.Add(this.groupBox1);
   this.panel1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("panel1.Dock")));
   this.panel1.Enabled = ((bool)(resources.GetObject("panel1.Enabled")));
   this.panel1.Font = ((System.Drawing.Font)(resources.GetObject("panel1.Font")));
   this.panel1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("panel1.ImeMode")));
   this.panel1.Location = ((System.Drawing.Point)(resources.GetObject("panel1.Location")));
   this.panel1.Name = "panel1";
   this.panel1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("panel1.RightToLeft")));
   this.panel1.Size = ((System.Drawing.Size)(resources.GetObject("panel1.Size")));
   this.panel1.TabIndex = ((int)(resources.GetObject("panel1.TabIndex")));
   this.panel1.Text = resources.GetString("panel1.Text");
   this.panel1.Visible = ((bool)(resources.GetObject("panel1.Visible")));



   this.groupBox1.AccessibleDescription = resources.GetString("groupBox1.AccessibleDescription");
   this.groupBox1.AccessibleName = resources.GetString("groupBox1.AccessibleName");
   this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("groupBox1.Anchor")));
   this.groupBox1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("groupBox1.BackgroundImage")));
   this.groupBox1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("groupBox1.Dock")));
   this.groupBox1.Enabled = ((bool)(resources.GetObject("groupBox1.Enabled")));
   this.groupBox1.Font = ((System.Drawing.Font)(resources.GetObject("groupBox1.Font")));
   this.groupBox1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("groupBox1.ImeMode")));
   this.groupBox1.Location = ((System.Drawing.Point)(resources.GetObject("groupBox1.Location")));
   this.groupBox1.Name = "groupBox1";
   this.groupBox1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("groupBox1.RightToLeft")));
   this.groupBox1.Size = ((System.Drawing.Size)(resources.GetObject("groupBox1.Size")));
   this.groupBox1.TabIndex = ((int)(resources.GetObject("groupBox1.TabIndex")));
   this.groupBox1.TabStop = false;
   this.groupBox1.Text = resources.GetString("groupBox1.Text");
   this.groupBox1.Visible = ((bool)(resources.GetObject("groupBox1.Visible")));



   this.statusBar1.AccessibleDescription = resources.GetString("statusBar1.AccessibleDescription");
   this.statusBar1.AccessibleName = resources.GetString("statusBar1.AccessibleName");
   this.statusBar1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("statusBar1.Anchor")));
   this.statusBar1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("statusBar1.BackgroundImage")));
   this.statusBar1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("statusBar1.Dock")));
   this.statusBar1.Enabled = ((bool)(resources.GetObject("statusBar1.Enabled")));
   this.statusBar1.Font = ((System.Drawing.Font)(resources.GetObject("statusBar1.Font")));
   this.statusBar1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("statusBar1.ImeMode")));
   this.statusBar1.Location = ((System.Drawing.Point)(resources.GetObject("statusBar1.Location")));
   this.statusBar1.Name = "statusBar1";
   this.statusBar1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("statusBar1.RightToLeft")));
   this.statusBar1.Size = ((System.Drawing.Size)(resources.GetObject("statusBar1.Size")));
   this.statusBar1.TabIndex = ((int)(resources.GetObject("statusBar1.TabIndex")));
   this.statusBar1.Text = resources.GetString("statusBar1.Text");
   this.statusBar1.Visible = ((bool)(resources.GetObject("statusBar1.Visible")));



   this.menuItem1.Enabled = ((bool)(resources.GetObject("menuItem1.Enabled")));
   this.menuItem1.Index = 1;
   this.menuItem1.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuItem1.Shortcut")));
   this.menuItem1.ShowShortcut = ((bool)(resources.GetObject("menuItem1.ShowShortcut")));
   this.menuItem1.Text = resources.GetString("menuItem1.Text");
   this.menuItem1.Visible = ((bool)(resources.GetObject("menuItem1.Visible")));



   this.menuViewAccounts.Enabled = ((bool)(resources.GetObject("menuViewAccounts.Enabled")));
   this.menuViewAccounts.Index = 2;
   this.menuViewAccounts.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuViewAccounts.Shortcut")));
   this.menuViewAccounts.ShowShortcut = ((bool)(resources.GetObject("menuViewAccounts.ShowShortcut")));
   this.menuViewAccounts.Text = resources.GetString("menuViewAccounts.Text");
   this.menuViewAccounts.Visible = ((bool)(resources.GetObject("menuViewAccounts.Visible")));
   this.menuViewAccounts.Click += new System.EventHandler(this.menuViewAccounts_Click);



   this.menuViewLog.Enabled = ((bool)(resources.GetObject("menuViewLog.Enabled")));
   this.menuViewLog.Index = 3;
   this.menuViewLog.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuViewLog.Shortcut")));
   this.menuViewLog.ShowShortcut = ((bool)(resources.GetObject("menuViewLog.ShowShortcut")));
   this.menuViewLog.Text = resources.GetString("menuViewLog.Text");
   this.menuViewLog.Visible = ((bool)(resources.GetObject("menuViewLog.Visible")));
   this.menuViewLog.Click += new System.EventHandler(this.menuViewLog_Click);



   this.menuEdit.Enabled = ((bool)(resources.GetObject("menuEdit.Enabled")));
   this.menuEdit.Index = 1;
   this.menuEdit.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                      this.menuEditPrefs});
   this.menuEdit.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuEdit.Shortcut")));
   this.menuEdit.ShowShortcut = ((bool)(resources.GetObject("menuEdit.ShowShortcut")));
   this.menuEdit.Text = resources.GetString("menuEdit.Text");
   this.menuEdit.Visible = ((bool)(resources.GetObject("menuEdit.Visible")));



   this.menuEditPrefs.Enabled = ((bool)(resources.GetObject("menuEditPrefs.Enabled")));
   this.menuEditPrefs.Index = 0;
   this.menuEditPrefs.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuEditPrefs.Shortcut")));
   this.menuEditPrefs.ShowShortcut = ((bool)(resources.GetObject("menuEditPrefs.ShowShortcut")));
   this.menuEditPrefs.Text = resources.GetString("menuEditPrefs.Text");
   this.menuEditPrefs.Visible = ((bool)(resources.GetObject("menuEditPrefs.Visible")));
   this.menuEditPrefs.Click += new System.EventHandler(this.menuEditPrefs_Click);



   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.iFolderView);
   this.Controls.Add(this.panel1);
   this.Controls.Add(this.toolBar1);
   this.Controls.Add(this.progressBar1);
   this.Controls.Add(this.statusBar1);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.KeyPreview = true;
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.Menu = this.mainMenu1;
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "GlobalProperties";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.GlobalProperties_KeyDown);
   this.Closing += new System.ComponentModel.CancelEventHandler(this.GlobalProperties_Closing);
   this.Move += new System.EventHandler(this.GlobalProperties_Move);
   this.VisibleChanged += new System.EventHandler(this.GlobalProperties_VisibleChanged);
   this.panel1.ResumeLayout(false);
   this.ResumeLayout(false);

  }






  public bool InitialConnect
  {
   set { initialConnect = value; }
  }




  public Preferences PreferenceDialog
  {
   set { preferences = value; }
  }




  public SyncLog SyncLogDialog
  {
   set { syncLog = value; }
  }







  public void AddDomainToList(DomainInformation domainInfo)
  {
   Domain domain = null;
   foreach (Domain d in servers.Items)
   {
    if (d.ID.Equals(domainInfo.ID))
    {

     domain = d;
    }
   }

   if (domain == null)
   {

    domain = new Domain(domainInfo);
    servers.Items.Add(domain);
   }


   if (domainInfo.IsDefault)
   {
    if (defaultDomain != null)
    {
     defaultDomain.DomainInfo.IsDefault = false;
    }


    defaultDomain = domain;
   }


   addDomainToFile(domainInfo);
  }





  public void RemoveDomainFromList(DomainInformation domainInfo, string defaultDomainID)
  {
   Domain domain = null;
   Domain showAllDomain = null;

   foreach (Domain d in servers.Items)
   {
    if (d.ID.Equals(domainInfo.ID))
    {
     domain = d;
    }

    if (d.ShowAll)
    {
     showAllDomain = d;
    }


    if ((defaultDomainID != null) && d.ID.Equals(defaultDomainID))
    {
     d.DomainInfo.IsDefault = true;
    }
   }

   if (domain != null)
   {
    if (servers.SelectedItem.Equals(domain))
    {

     servers.SelectedItem = showAllDomain;
    }
    else if (((Domain)servers.SelectedItem).ShowAll)
    {

     refreshiFolders((Domain)servers.SelectedItem);
    }

    servers.Items.Remove(domain);
   }


   removeDomainFromFile(domainInfo, defaultDomainID);
  }





  public void RemoveDomainFromList(string domainID)
  {
   Domain domain = null;
   Domain showAllDomain = null;

   foreach (Domain d in servers.Items)
   {
    if (d.ID.Equals(domainID))
    {
     domain = d;
    }

    if (d.ShowAll)
    {
     showAllDomain = d;
    }
   }

   if (domain != null)
   {
    if (servers.SelectedItem.Equals(domain))
    {

     servers.SelectedItem = showAllDomain;
    }
    else if (((Domain)servers.SelectedItem).ShowAll)
    {

     refreshiFolders((Domain)servers.SelectedItem);
    }

    servers.Items.Remove(domain);


    string defaultDomainID = simiasWebService.GetDefaultDomainID();
    removeDomainFromFile(domain.DomainInfo, defaultDomainID);

    if (RemoveDomain != null)
    {
     RemoveDomain(this, new DomainRemoveEventArgs(domain.DomainInfo, defaultDomainID));
    }
   }
  }




  public void InitializeServerList()
  {
   servers.Items.Clear();


   Domain domain = new Domain(resourceManager.GetString("showAll"));
   servers.Items.Add(domain);
   servers.SelectedItem = domain;


   domainList = Path.Combine(
    Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
    "domain.list");

   XmlDocument domainsDoc = new XmlDocument();

   try
   {
    if (!File.Exists(domainList))
    {
     domainsDoc.LoadXml("<domains></domains>");
    }
    else
    {

     domainsDoc.Load(domainList);
     XmlNode node = domainsDoc.SelectSingleNode("/domains");
     if (node == null)
     {
      XmlElement element = domainsDoc.CreateElement("domains");
      domainsDoc.AppendChild(element);
     }
     else
     {
      node.RemoveAll();
     }
    }
   }
   catch
   {
    domainsDoc.LoadXml("<domains></domains>");
   }

   saveXmlFile(domainsDoc);
  }






  public bool IsCurrentUser(string userID)
  {
   bool result = false;

   foreach (Domain d in servers.Items)
   {
    if (!d.ShowAll && d.DomainInfo.MemberUserID.Equals(userID))
    {
     result = true;
     break;
    }
   }

   return result;
  }






  public bool IsPOBox(string poBoxID)
  {
   bool result = false;

   foreach (Domain d in servers.Items)
   {
    if (!d.ShowAll && d.DomainInfo.POBoxID.Equals(poBoxID))
    {
     result = true;
     break;
    }
   }

   return result;
  }







  public bool IsSelected(string domainID)
  {
   bool result = false;

   try
   {
    Domain domain = (Domain)servers.SelectedItem;
    if (domain.ShowAll || domain.ID.Equals(domainID))
    {
     result = true;
    }
   }
   catch {}

   return result;
  }





  public void UpdateDomain(DomainInformation domainInfo)
  {
   foreach (Domain d in servers.Items)
   {
    if (d.ID.Equals(domainInfo.ID))
    {
     d.DomainInfo = domainInfo;
     break;
    }
   }

   if ((iFolderView.SelectedItems.Count == 1) &&
    !((iFolderObject)iFolderView.SelectedItems[0].Tag).iFolderWeb.IsSubscription &&
    ((iFolderObject)iFolderView.SelectedItems[0].Tag).iFolderWeb.DomainID.Equals(domainInfo.ID))
   {
    menuSyncNow.Visible = menuActionSync.Enabled = toolBarSync.Enabled = domainInfo.Active;
   }
  }



  private void addDomainToFile(DomainInformation domainInfo)
  {
   XmlDocument domainsDoc;


   domainsDoc = new XmlDocument();
   domainsDoc.Load(domainList);

   XmlElement element = (XmlElement)domainsDoc.SelectSingleNode("/domains");

   bool found = false;


   XmlNodeList nodeList = element.GetElementsByTagName("domain");
   foreach (XmlNode node in nodeList)
   {
    string id = ((XmlElement)node).GetAttribute("ID");
    if (id.Equals(domainInfo.ID))
    {

     found = true;
     break;
    }
   }

   if (!found)
   {



    XmlElement domain = domainsDoc.CreateElement("domain");


    domain.SetAttribute("name", domainInfo.Name);
    domain.SetAttribute("ID", domainInfo.ID);


    element.AppendChild(domain);
   }


   if (domainInfo.IsDefault)
   {
    XmlElement defaultDomainElement = (XmlElement)domainsDoc.SelectSingleNode("/domains/defaultDomain");
    if (defaultDomainElement == null)
    {
     defaultDomainElement = domainsDoc.CreateElement("defaultDomain");
     defaultDomainElement.SetAttribute("ID", domainInfo.ID);
     element.AppendChild(defaultDomainElement);
    }
    else
    {
     string id = defaultDomainElement.GetAttribute("ID");
     if (!id.Equals(domainInfo.ID))
     {
      defaultDomainElement.SetAttribute("ID", domainInfo.ID);
     }
    }
   }

   saveXmlFile(domainsDoc);
  }

  private void removeDomainFromFile(DomainInformation domainInfo, string defaultDomainID)
  {
   XmlDocument domainsDoc;


   domainsDoc = new XmlDocument();
   domainsDoc.Load(domainList);

   XmlElement element = (XmlElement)domainsDoc.SelectSingleNode("/domains");



   XmlNode domainNode = null;
   XmlNodeList nodeList = element.GetElementsByTagName("domain");
   foreach (XmlNode node in nodeList)
   {
    string id = ((XmlElement)node).GetAttribute("ID");
    if (id.Equals(domainInfo.ID))
    {
     domainNode = node;
     break;
    }
   }

   if (domainNode != null)
   {

    element.RemoveChild(domainNode);
   }


   if (defaultDomainID != null)
   {
    element = (XmlElement)domainsDoc.SelectSingleNode("/domains/defaultDomain");
    if (!element.GetAttribute("ID").Equals(defaultDomainID))
    {
     element.SetAttribute("ID", defaultDomainID);
    }
   }

   saveXmlFile(domainsDoc);
  }

  private void saveXmlFile(XmlDocument doc)
  {

   XmlTextWriter xtw = new XmlTextWriter(domainList, System.Text.Encoding.UTF8);
   try
   {
    xtw.Formatting = Formatting.Indented;

    doc.WriteTo(xtw);
   }
   finally
   {
    xtw.Close();
   }
  }

  private void syncCollection(CollectionSyncEventArgs syncEventArgs)
  {
   try
   {
    progressBar1.Visible = false;

    switch (syncEventArgs.Action)
    {
     case Action.StartLocalSync:
     {
      if (!syncEventArgs.Name.StartsWith("POBox:"))
      {
       statusBar1.Text = string.Format(resourceManager.GetString("localSync"), syncEventArgs.Name);
       lock (ht)
       {
        ListViewItem lvi = (ListViewItem)ht[syncEventArgs.ID];
        if (lvi != null)
        {
         ((iFolderObject)lvi.Tag).iFolderState = iFolderState.SynchronizingLocal;
         lvi.SubItems[2].Text = resourceManager.GetString("preSync");
        }
       }
      }
      break;
     }
     case Action.StartSync:
     {
      if (syncEventArgs.Name.StartsWith("POBox:"))
      {
       statusBar1.Text = resourceManager.GetString("checkingForiFolders");
      }
      else
      {
       statusBar1.Text = string.Format(resourceManager.GetString("synciFolder"), syncEventArgs.Name);
       lock (ht)
       {
        ListViewItem lvi = (ListViewItem)ht[syncEventArgs.ID];
        if (lvi != null)
        {
         startSync = true;
         ((iFolderObject)lvi.Tag).iFolderState = iFolderState.Synchronizing;
         lvi.SubItems[2].Text = resourceManager.GetString("statusSyncing");
        }
       }
      }
      break;
     }
     case Action.StopSync:
     {
      lock(ht)
      {
       ListViewItem lvi = (ListViewItem)ht[syncEventArgs.ID];

       if (lvi != null)
       {
        try
        {
         SyncSize syncSize = ifWebService.CalculateSyncSize(syncEventArgs.ID);
         objectsToSync = syncSize.SyncNodeCount;
        }
        catch {}

        if (objectsToSync == 0)
        {
         iFolderObject ifolderObject = (iFolderObject)lvi.Tag;

         if (syncEventArgs.Connected)
         {
          ifolderObject.iFolderState = iFolderState.Normal;
         }
         else
         {
          ifolderObject.iFolderState = iFolderState.Disconnected;
         }

         int imageIndex;
         lvi.SubItems[2].Text = stateToString(ifolderObject, out imageIndex);
        }
        else
        {
         lvi.SubItems[2].Text = string.Format(resourceManager.GetString("statusSyncItemsFailed"), objectsToSync);
         ((iFolderObject)lvi.Tag).iFolderState = iFolderState.FailedSync;
        }
       }

       objectsToSync = 0;
      }

      statusBar1.Text = resourceManager.GetString("statusBar1.Text");
      if (initialConnect)
      {
       initialConnect = false;
       updateEnterpriseTimer.Start();
      }
      break;
     }
    }
   }
   catch {}
  }

  private void syncFile(FileSyncEventArgs syncEventArgs)
  {
   try
   {
    if (syncEventArgs.SizeRemaining == syncEventArgs.SizeToSync)
    {
     progressBar1.Visible = syncEventArgs.SizeToSync > 0;
     progressBar1.Value = 0;
     progressBar1.Maximum = 200;

     if (startSync || (objectsToSync <= 0))
     {
      startSync = false;
      SyncSize syncSize = ifWebService.CalculateSyncSize(syncEventArgs.CollectionID);
      objectsToSync = syncSize.SyncNodeCount;
     }

     if (!syncEventArgs.Direction.Equals(Direction.Local))
     {
      lock (ht)
      {
       ListViewItem lvi = (ListViewItem)ht[syncEventArgs.CollectionID];
       if (lvi != null)
       {
        lvi.SubItems[2].Text = string.Format(resourceManager.GetString("statusSyncingItems"), objectsToSync--);
       }
      }
     }

     switch (syncEventArgs.ObjectType)
     {
      case ObjectType.File:
       if (syncEventArgs.Delete)
       {
        statusBar1.Text = string.Format(resourceManager.GetString("deleteClientFile"), syncEventArgs.Name);
       }
       else
       {
        switch (syncEventArgs.Direction)
        {
         case Direction.Uploading:
          statusBar1.Text = string.Format(resourceManager.GetString("uploadFile"), syncEventArgs.Name);
          break;
         case Direction.Downloading:
          statusBar1.Text = string.Format(resourceManager.GetString("downloadFile"), syncEventArgs.Name);
          break;
         case Direction.Local:
          statusBar1.Text = string.Format(resourceManager.GetString("localFile"), syncEventArgs.Name);
          break;
         default:
          statusBar1.Text = string.Format(resourceManager.GetString("syncingFile"), syncEventArgs.Name);
          break;
        }
       }
       break;
      case ObjectType.Directory:
       if (syncEventArgs.Delete)
       {
        statusBar1.Text = string.Format(resourceManager.GetString("deleteClientDir"), syncEventArgs.Name);
       }
       else
       {
        switch (syncEventArgs.Direction)
        {
         case Direction.Uploading:
          statusBar1.Text = string.Format(resourceManager.GetString("uploadDir"), syncEventArgs.Name);
          break;
         case Direction.Downloading:
          statusBar1.Text = string.Format(resourceManager.GetString("downloadDir"), syncEventArgs.Name);
          break;
         case Direction.Local:
          statusBar1.Text = string.Format(resourceManager.GetString("localDir"), syncEventArgs.Name);
          break;
         default:
          statusBar1.Text = string.Format(resourceManager.GetString("syncingDir"), syncEventArgs.Name);
          break;
        }
       }
       break;
      case ObjectType.Unknown:
       statusBar1.Text = string.Format(resourceManager.GetString("deleteUnknown"), syncEventArgs.Name);
       break;
     }
    }
    else
    {
     statusBar1.Text = syncEventArgs.Name;
     progressBar1.Value = syncEventArgs.SizeToSync > 0 ? (int)(((syncEventArgs.SizeToSync - syncEventArgs.SizeRemaining) * 200) / syncEventArgs.SizeToSync) : progressBar1.Maximum;
    }
   }
   catch {}
  }

  private void deleteEvent(NodeEventArgs args)
  {
   switch (args.Type)
   {
    case "Collection":
    case "Subscription":
     lock (ht)
     {
      ListViewItem lvi = (ListViewItem)ht[args.Node];
      if (lvi != null)
      {

       Win32Window.ShChangeNotify(Win32Window.SHCNE_UPDATEITEM, Win32Window.SHCNF_PATHW, ((iFolderObject)lvi.Tag).iFolderWeb.UnManagedPath, IntPtr.Zero);

       lvi.Remove();
       ht.Remove(args.Node);
      }
     }
     break;
    case "Domain":
     RemoveDomainFromList(args.Node);
     break;
   }
  }

  private void createChangeEvent(iFolderWeb ifolder, string eventData)
  {
   if (ifolder != null)
   {
    if (eventData.Equals("NodeCreated"))
    {
     if (IsSelected(ifolder.DomainID))
     {
      addiFolderToListView(new iFolderObject(ifolder, iFolderState.Normal));

      if (ifolder.State.Equals("Local"))
      {

       Win32Window.ShChangeNotify(Win32Window.SHCNE_UPDATEITEM, Win32Window.SHCNF_PATHW, ifolder.UnManagedPath, IntPtr.Zero);
      }



      if (!ifolder.IsSubscription)
      {

       lock (ht)
       {
        ListViewItem[] lvia = new ListViewItem[ht.Count];
        ht.Values.CopyTo(lvia, 0);

        foreach(ListViewItem lvi in lvia)
        {
         iFolderObject ifo = lvi.Tag as iFolderObject;
         if (ifo.iFolderWeb.IsSubscription &&
          (ifo.iFolderWeb.CollectionID == ifolder.CollectionID))
         {
          ht.Remove(ifo.iFolderWeb.ID);
          lvi.Remove();
          break;
         }
        }
       }
      }
     }
    }
    else
    {
     ListViewItem lvi;
     lock (ht)
     {

      lvi = (ListViewItem)ht[ifolder.ID];
     }

     if (lvi != null)
     {

      ((iFolderObject)lvi.Tag).iFolderWeb = ifolder;
      updateListViewItem(lvi);
     }
    }
   }
  }

  private void addiFolderToListView(iFolderObject ifolderObject)
  {
   iFolderWeb ifolder = ifolderObject.iFolderWeb;

   lock (ht)
   {

    if (ht[ifolder.ID] == null)
    {
     string[] items = new string[3];
     int imageIndex;

     items[0] = ifolder.Name;
     items[1] = ifolder.IsSubscription ? ifolder.Owner : ifolder.UnManagedPath;
     items[2] = stateToString(ifolderObject, out imageIndex);

     ListViewItem lvi = new ListViewItem(items, imageIndex);
     lvi.Tag = ifolderObject;
     iFolderView.Items.Add(lvi);


     ht.Add(ifolder.ID, lvi);
    }
   }
  }

  private void updateListViewItem(ListViewItem lvi)
  {
   iFolderObject ifolderObject = (iFolderObject)lvi.Tag;
   iFolderWeb ifolder = ifolderObject.iFolderWeb;

   if (ifolder.State.Equals("Available") &&
    (ifWebService.GetiFolder(ifolder.CollectionID) != null))
   {

    lock (ht)
    {
     ht.Remove(ifolder.ID);
    }

    lvi.Remove();
   }
   else
   {
    int imageIndex;
    lvi.SubItems[0].Text = ifolder.Name;
    lvi.SubItems[1].Text = ifolder.IsSubscription ? "" : ifolder.UnManagedPath;
    lvi.SubItems[2].Text = stateToString(ifolderObject, out imageIndex);
    lvi.ImageIndex = imageIndex;


    if (lvi.Selected && (iFolderView.SelectedItems.Count == 1))
    {
     updateMenus(ifolder);
    }
   }
  }

  private string stateToString(iFolderObject ifolderObject, out int imageIndex)
  {
   string status;
   imageIndex = 0;

   if (ifolderObject.iFolderWeb.HasConflicts)
   {
    imageIndex = 2;
    status = resourceManager.GetString("statusConflicts");
   }
   else
   {
    switch (ifolderObject.iFolderState)
    {
     case iFolderState.Normal:
     {
      switch (ifolderObject.iFolderWeb.State)
      {
       case "Local":
        status = resourceManager.GetString("statusOK");
        break;
       case "Available":
       case "WaitConnect":
       case "WaitSync":
        imageIndex = ifolderObject.iFolderWeb.IsSubscription ? 1 : 0;
        status = resourceManager.GetString(ifolderObject.iFolderWeb.State);
        break;
       default:

        imageIndex = 1;
        status = resourceManager.GetString("statusUnknown");
        break;
      }
      break;
     }
     case iFolderState.Disconnected:
      status = resourceManager.GetString("disconnected");
      break;
     case iFolderState.FailedSync:
      status = objectsToSync == 0 ?
       status = resourceManager.GetString("statusSyncFailure") :
       string.Format(resourceManager.GetString("statusSyncItemsFailed"), objectsToSync);
      break;
     case iFolderState.Synchronizing:
      status = string.Format(resourceManager.GetString("statusSyncingItems"), objectsToSync);
      break;
     case iFolderState.SynchronizingLocal:
      status = resourceManager.GetString("preSync");
      break;
     default:
      status = resourceManager.GetString("statusUnknown");
      break;
    }
   }

   return status;
  }

  private void refreshAll(Domain domain)
  {
   refreshiFolders(domain);


   if (domain.ShowAll)
   {
    foreach (Domain d in servers.Items)
    {
     if (!d.ShowAll)
     {
      try
      {
       ifWebService.SynciFolderNow(d.DomainInfo.POBoxID);
      }
      catch {}
     }
    }
   }
   else
   {
    try
    {
     ifWebService.SynciFolderNow(domain.DomainInfo.POBoxID);
    }
    catch {}
   }
  }

  private void refreshiFolders(Domain domain)
  {
   Cursor.Current = Cursors.WaitCursor;

   iFolderView.Items.Clear();
   iFolderView.SelectedItems.Clear();


   menuShare.Visible = menuActionShare.Enabled = toolBarShare.Enabled =
    menuProperties.Visible = menuActionProperties.Enabled =
    menuRevert.Visible = menuActionRevert.Enabled =
    menuSyncNow.Visible = menuActionSync.Enabled = toolBarSync.Enabled =
    menuOpen.Visible = menuActionOpen.Enabled =
    menuSeparator1.Visible = menuSeparator2.Visible =
    menuResolve.Visible = menuActionResolve.Visible = toolBarResolve.Enabled =
    menuAccept.Visible = menuActionAccept.Visible = toolBarSetup.Enabled =
    menuActionSeparator2.Visible =
    menuRemove.Visible = menuActionRemove.Visible =
    menuActionSeparator2.Visible = false;


   menuRefresh.Visible = menuCreate.Visible = true;


   Array oldValues = Array.CreateInstance(typeof(ListViewItem), ht.Count);
   ht.Values.CopyTo(oldValues, 0);

   lock(ht)
   {
    ht.Clear();
   }

   iFolderView.BeginUpdate();

   try
   {
    iFolderWeb[] ifolderArray = domain.ShowAll ?
     ifWebService.GetAlliFolders() :
     ifWebService.GetiFoldersForDomain(domain.ID);
    foreach (iFolderWeb ifolder in ifolderArray)
    {
     iFolderState state = iFolderState.Normal;
     if (!ifolder.IsSubscription)
     {
      foreach (ListViewItem lvi in oldValues)
      {
       iFolderObject oldiFolder = (iFolderObject)lvi.Tag;
       if (oldiFolder.iFolderWeb.ID.Equals(ifolder.ID))
       {
        state = oldiFolder.iFolderState;
        break;
       }
      }
     }

     addiFolderToListView(new iFolderObject(ifolder, state));
    }
   }
   catch (Exception ex)
   {
    Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("iFolderError"), resourceManager.GetString("iFolderErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
    mmb.ShowDialog();
   }

   iFolderView.EndUpdate();
   Cursor.Current = Cursors.Default;
  }

  private void invokeiFolderProperties(ListViewItem lvi, int activeTab)
  {
   iFolderAdvanced ifolderAdvanced = new iFolderAdvanced();
   ifolderAdvanced.CurrentiFolder = ((iFolderObject)lvi.Tag).iFolderWeb;
   ifolderAdvanced.LoadPath = Application.StartupPath;
   ifolderAdvanced.ActiveTab = activeTab;
   ifolderAdvanced.EventClient = eventClient;
   ifolderAdvanced.ShowDialog();
   ifolderAdvanced.Dispose();
  }

  private void synciFolder(string iFolderID)
  {
   try
   {
    ifWebService.SynciFolderNow(iFolderID);
   }
   catch (Exception ex)
   {
    Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("syncError"), resourceManager.GetString("syncErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
  }

  private void updateMenus(iFolderWeb ifolderWeb)
  {
   if (ifolderWeb == null)
   {
    menuShare.Visible = menuActionShare.Enabled = toolBarShare.Enabled =
     menuProperties.Visible = menuActionProperties.Enabled =
     menuRevert.Visible = menuActionRevert.Enabled =
     menuOpen.Visible = menuActionOpen.Enabled =
     menuSyncNow.Visible = menuActionSync.Enabled = toolBarSync.Enabled =
     menuSeparator1.Visible = menuSeparator2.Visible =
     menuResolve.Visible = menuActionResolve.Visible = toolBarResolve.Enabled =
     menuAccept.Visible = menuActionAccept.Visible = toolBarSetup.Enabled =
     menuActionSeparator2.Visible =
     menuRemove.Visible = menuActionRemove.Visible =
     menuActionSeparator2.Visible = false;

   }
   else
   {
    menuShare.Visible = menuActionShare.Enabled = toolBarShare.Enabled =
     menuProperties.Visible = menuActionProperties.Enabled =
     menuOpen.Visible = menuActionOpen.Enabled =
     menuSeparator1.Visible = menuSeparator2.Visible =
     !ifolderWeb.IsSubscription;
    menuRevert.Visible = menuActionRevert.Enabled =
     !ifolderWeb.IsSubscription && !ifolderWeb.Role.Equals("Master");
    if (ifolderWeb.IsSubscription)
    {
     menuSyncNow.Visible = menuActionSync.Enabled = toolBarSync.Enabled = false;
    }
    else
    {
     Domain selectedDomain = (Domain)servers.SelectedItem;
     if (!selectedDomain.ShowAll)
     {
      menuSyncNow.Visible = menuActionSync.Enabled = toolBarSync.Enabled = selectedDomain.DomainInfo.Active;
     }
     else
     {
      foreach (Domain d in servers.Items)
      {
       if (d.ID.Equals(ifolderWeb.DomainID))
       {
        menuSyncNow.Visible = menuActionSync.Enabled = toolBarSync.Enabled = d.DomainInfo.Active;
        break;
       }
      }
     }
    }

    menuResolve.Visible = menuActionResolve.Visible = toolBarResolve.Enabled = ifolderWeb.HasConflicts;


    menuAccept.Visible = menuActionAccept.Visible = toolBarSetup.Enabled =
     menuActionSeparator2.Visible = ifolderWeb.IsSubscription && ifolderWeb.State.Equals("Available");


    menuRemove.Visible = menuActionRemove.Visible =
     menuActionSeparator2.Visible = (!ifolderWeb.IsSubscription || ifolderWeb.State.Equals("Available"));

    if (menuRemove.Visible)
    {
     if (IsCurrentUser(ifolderWeb.OwnerID))
     {
      menuRemove.Text = menuActionRemove.Text =
       resourceManager.GetString("deleteAction");
     }
     else
     {
      menuRemove.Text = resourceManager.GetString("menuRemove.Text");
      menuActionRemove.Text = resourceManager.GetString("menuActionRemove.Text");

     }
    }
   }

   menuRefresh.Visible = menuCreate.Visible = iFolderView.SelectedItems.Count == 0;
  }

  private void updateEnterpriseData()
  {
   servers.Items.Clear();
   DomainInformation[] domains;
   try
   {
    domains = simiasWebService.GetDomains(false);
    foreach (DomainInformation di in domains)
    {
     AddDomainToList(di);
    }
   }
   catch
   {

   }
  }



  private void GlobalProperties_Move(object sender, System.EventArgs e)
  {
   if (initialPositionSet)
   {
    try
    {

     RegistryKey regKey = Registry.CurrentUser.CreateSubKey(Preferences.iFolderKey);


     regKey.SetValue(myiFoldersX, Location.X);
     regKey.SetValue(myiFoldersY, Location.Y);
    }
    catch {}
   }
   else
   {
    try
    {

     RegistryKey regKey = Registry.CurrentUser.CreateSubKey(Preferences.iFolderKey);


     int x = (int)regKey.GetValue(myiFoldersX);
     int y = (int)regKey.GetValue(myiFoldersY);

     Point point = new Point(x, y);


     if (SystemInformation.VirtualScreen.Contains(point))
     {
      this.Location = point;
     }
    }
    catch {}

    initialPositionSet = true;
   }
  }

  private void GlobalProperties_VisibleChanged(object sender, System.EventArgs e)
  {
   if (this.Visible)
   {
    InitializeServerList();

    DomainInformation[] domains;
    try
    {
     domains = simiasWebService.GetDomains(false);
     foreach (DomainInformation di in domains)
     {
      AddDomainToList(di);
     }
    }
    catch{}

    Activate();
   }
  }

  private void GlobalProperties_Closing(object sender, System.ComponentModel.CancelEventArgs e)
  {

   if (!shutdown)
   {
    e.Cancel = true;
    Hide();
   }
  }

  private void updateEnterpriseTimer_Elapsed(object sender, System.Timers.ElapsedEventArgs e)
  {
   updateEnterpriseTimer.Stop();
   updateEnterpriseData();
  }

  private void menuFileExit_Click(object sender, System.EventArgs e)
  {
   this.Close();
  }

  private void menuHelpHelp_Click(object sender, System.EventArgs e)
  {
   new iFolderComponent().ShowHelp(Application.StartupPath, string.Empty);
  }

  private void menuHelpAbout_Click(object sender, System.EventArgs e)
  {
   About about = new About();
   about.ShowDialog();
  }

  private void GlobalProperties_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
  {

   if (e.KeyCode == Keys.F5)
   {
    refreshAll((Domain)servers.SelectedItem);
   }
  }

  private void servers_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   refreshiFolders((Domain)servers.SelectedItem);
  }

  private void menuOpen_Click(object sender, System.EventArgs e)
  {
   ListViewItem lvi = iFolderView.SelectedItems[0];
   iFolderWeb ifolder = ((iFolderObject)lvi.Tag).iFolderWeb;

   try
   {
    Process.Start(ifolder.UnManagedPath);
   }
   catch (Exception ex)
   {
    Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(string.Format(resourceManager.GetString("iFolderOpenError"), ifolder.Name), resourceManager.GetString("openErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
  }

  private void menuRevert_Click(object sender, System.EventArgs e)
  {
   ListViewItem lvi = iFolderView.SelectedItems[0];

   Cursor.Current = Cursors.WaitCursor;

   try
   {
    iFolderWeb ifolder = ((iFolderObject)lvi.Tag).iFolderWeb;

    MyMessageBox mmb = new Novell.iFolderCom.MyMessageBox(
     resourceManager.GetString("revertiFolder") + "\n\n" +
     resourceManager.GetString("revertPrompt"),
     resourceManager.GetString("revertTitle"),
     string.Empty,
     MyMessageBoxButtons.YesNo,
     MyMessageBoxIcon.Question,
     MyMessageBoxDefaultButton.Button2);
    if (mmb.ShowDialog() == DialogResult.Yes)
    {

     iFolderWeb newiFolder = ifWebService.RevertiFolder(ifolder.ID);


     Win32Window.ShChangeNotify(Win32Window.SHCNE_UPDATEITEM, Win32Window.SHCNF_PATHW, ifolder.UnManagedPath, IntPtr.Zero);

     if (newiFolder != null)
     {
      lvi.Tag = new iFolderObject(newiFolder, iFolderState.Normal);

      lock (ht)
      {
       ht.Add(newiFolder.ID, lvi);
      }

      updateListViewItem(lvi);
     }
     else
     {
      lvi.Remove();
     }

     lock (ht)
     {
      ht.Remove(ifolder.ID);
     }
    }
   }
   catch (Exception ex)
   {
    Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("iFolderRevertError"), resourceManager.GetString("revertErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }

   Cursor.Current = Cursors.Default;
  }

  private void menuResolve_Click(object sender, System.EventArgs e)
  {
   ConflictResolver conflictResolver = new ConflictResolver();
   conflictResolver.iFolder = ((iFolderObject)iFolderView.SelectedItems[0].Tag).iFolderWeb;
   conflictResolver.iFolderWebService = ifWebService;
   conflictResolver.LoadPath = Application.StartupPath;
   conflictResolver.Show();
  }

  private void menuShare_Click(object sender, System.EventArgs e)
  {
   invokeiFolderProperties(iFolderView.SelectedItems[0], 1);
  }

  private void menuSyncNow_Click(object sender, System.EventArgs e)
  {
   synciFolder(((iFolderObject)iFolderView.SelectedItems[0].Tag).iFolderWeb.ID);
  }

  private void menuProperties_Click(object sender, System.EventArgs e)
  {
   invokeiFolderProperties(iFolderView.SelectedItems[0], 0);
  }

  private void menuCreate_Click(object sender, System.EventArgs e)
  {

   ArrayList domains = new ArrayList();
   Domain selectedDomain = (Domain)servers.SelectedItem;
   selectedDomain = selectedDomain.ShowAll ? defaultDomain : selectedDomain;
   DomainItem selectedDomainItem = null;
   foreach (Domain d in servers.Items)
   {
    if (!d.ShowAll)
    {
     DomainItem domainItem = new DomainItem(d.Name, d.ID);
     if ((selectedDomain != null) && d.ID.Equals(selectedDomain.ID))
     {
      selectedDomainItem = domainItem;
     }

     domains.Add(domainItem);
    }
   }

   CreateiFolder createiFolder = new CreateiFolder();
   createiFolder.Servers = domains;
   createiFolder.SelectedDomain = selectedDomainItem;
   createiFolder.LoadPath = Application.StartupPath;
   createiFolder.iFolderWebService = ifWebService;

   if ((DialogResult.OK == createiFolder.ShowDialog()) && iFolderComponent.DisplayConfirmationEnabled)
   {
    new iFolderComponent().NewiFolderWizard(Application.StartupPath, createiFolder.iFolderPath);
   }
  }

  private void menuRefresh_Click(object sender, System.EventArgs e)
  {
   refreshAll((Domain)servers.SelectedItem);
  }

  private void menuViewAccounts_Click(object sender, System.EventArgs e)
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

  private void menuViewLog_Click(object sender, System.EventArgs e)
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

  private void menuEditPrefs_Click(object sender, System.EventArgs e)
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

  private void menuAccept_Click(object sender, System.EventArgs e)
  {
   ListViewItem lvi = iFolderView.SelectedItems[0];
   iFolderWeb ifolder = ((iFolderObject)lvi.Tag).iFolderWeb;

   AcceptInvitation acceptInvitation = new AcceptInvitation(ifWebService, ifolder);

   acceptInvitation.ShowDialog();
  }

  private void menuRemove_Click(object sender, System.EventArgs e)
  {
   ListViewItem lvi = iFolderView.SelectedItems[0];
   iFolderWeb ifolder = ((iFolderObject)lvi.Tag).iFolderWeb;
   try
   {
    string message;
    string caption = resourceManager.GetString("removeTitle");

    if (ifolder.IsSubscription)
    {
     message = resourceManager.GetString("removeiFolder2") + "\n\n" +
      resourceManager.GetString("removePrompt");

     MyMessageBox mmb = new Novell.iFolderCom.MyMessageBox(
      message,
      caption,
      string.Empty,
      MyMessageBoxButtons.YesNo,
      MyMessageBoxIcon.Question,
      MyMessageBoxDefaultButton.Button2);
     if (mmb.ShowDialog() == DialogResult.Yes)
     {
      ifWebService.DeclineiFolderInvitation(ifolder.DomainID, ifolder.ID);
     }
    }
    else
    {
     if (IsCurrentUser(ifolder.OwnerID))
     {
      message = resourceManager.GetString("deleteiFolder") + "\n\n" +
       resourceManager.GetString("removePrompt");
     }
     else
     {
      message = resourceManager.GetString("removeiFolder") + "\n\n" +
       resourceManager.GetString("removePrompt");
     }

     MyMessageBox mmb = new Novell.iFolderCom.MyMessageBox(
      message,
      caption,
      string.Empty,
      MyMessageBoxButtons.YesNo,
      MyMessageBoxIcon.Question,
      MyMessageBoxDefaultButton.Button2);
     if (mmb.ShowDialog() == DialogResult.Yes)
     {
      if (ifolder.Role.Equals("Master"))
      {
       ifWebService.DeleteiFolder(ifolder.ID);
       lvi.Remove();
      }
      else
      {

       iFolderWeb newiFolder = ifWebService.RevertiFolder(ifolder.ID);


       Win32Window.ShChangeNotify(Win32Window.SHCNE_UPDATEITEM, Win32Window.SHCNF_PATHW, ifolder.UnManagedPath, IntPtr.Zero);

       if (newiFolder != null)
       {

        lvi.Tag = new iFolderObject(newiFolder, iFolderState.Normal);

        lock (ht)
        {
         ht.Add(newiFolder.ID, lvi);
        }

        updateListViewItem(lvi);


        ifWebService.DeclineiFolderInvitation(newiFolder.DomainID, newiFolder.ID);
       }
       else
       {
        lvi.Remove();
       }
      }

      lock (ht)
      {
       ht.Remove(ifolder.ID);
      }
     }
    }
   }
   catch (Exception ex)
   {
    Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("declineError"), resourceManager.GetString("errorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
  }

  private void iFolderView_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   iFolderWeb ifolderWeb = null;

   if (iFolderView.SelectedItems.Count == 1)
   {
    ifolderWeb = ((iFolderObject)iFolderView.SelectedItems[0].Tag).iFolderWeb;
   }

   updateMenus(ifolderWeb);
  }

  private void iFolderView_DoubleClick(object sender, System.EventArgs e)
  {
   if (iFolderView.SelectedItems.Count == 1)
   {
    ListViewItem lvi = iFolderView.SelectedItems[0];
    iFolderWeb ifolder = ((iFolderObject)lvi.Tag).iFolderWeb;
    if (ifolder.IsSubscription)
    {
     if (ifolder.State.Equals("Available"))
     {
      menuAccept_Click(sender, e);
     }
    }
    else
    {
     menuOpen_Click(sender, e);
    }
   }
  }

  private void toolBar1_ButtonClick(object sender, System.Windows.Forms.ToolBarButtonClickEventArgs e)
  {
   switch (toolBar1.Buttons.IndexOf(e.Button))
   {
    case 0:
     menuCreate_Click(this, new EventArgs());
     break;
    case 1:
     menuAccept_Click(this, new EventArgs());
     break;
    case 2:
     invokeiFolderProperties(iFolderView.SelectedItems[0], 1);
     break;
    case 3:
     menuResolve_Click(this, new EventArgs());
     break;
    case 4:
     synciFolder(((iFolderObject)iFolderView.SelectedItems[0].Tag).iFolderWeb.ID);
     break;

   }
  }


  private void global_collectionSyncHandler(SimiasEventArgs args)
  {
   try
   {
    CollectionSyncEventArgs syncEventArgs = args as CollectionSyncEventArgs;
    BeginInvoke(syncCollectionDelegate, new object[] {syncEventArgs});
   }
   catch {}
  }

  private void global_fileSyncHandler(SimiasEventArgs args)
  {
   try
   {
    FileSyncEventArgs syncEventArgs = args as FileSyncEventArgs;
    BeginInvoke(syncFileDelegate, new object[] {syncEventArgs});
   }
   catch {}
  }




  private const int WM_QUERYENDSESSION = 0x0011;





  protected override void WndProc(ref Message m)
  {

   switch (m.Msg)
   {
    case WM_QUERYENDSESSION:
     this.shutdown = true;
     break;
   }

   base.WndProc (ref m);
  }
 }
}
