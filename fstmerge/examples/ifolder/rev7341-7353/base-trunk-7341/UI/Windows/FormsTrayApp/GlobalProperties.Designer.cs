using System;
using System.Windows.Forms;
using System.Drawing;
using System.ComponentModel;
using System.Collections;
using Novell.iFolderCom;
using System.Windows.Forms;

namespace Novell.FormsTrayApp
{
 public partial class GlobalProperties
 {


  private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(GlobalProperties));
            this.mainMenu1 = new System.Windows.Forms.MainMenu(this.components);
            this.menuAction = new System.Windows.Forms.MenuItem();
            this.menuActionCreate = new System.Windows.Forms.MenuItem();
            this.menuActionAccept = new System.Windows.Forms.MenuItem();
            this.menuActionMerge = new System.Windows.Forms.MenuItem();
            this.menuActionRemove = new System.Windows.Forms.MenuItem();
            this.menuItem7 = new System.Windows.Forms.MenuItem();
            this.menuActionOpen = new System.Windows.Forms.MenuItem();
            this.menuActionShare = new System.Windows.Forms.MenuItem();
            this.menuActionResolve = new System.Windows.Forms.MenuItem();
            this.menuActionSync = new System.Windows.Forms.MenuItem();
            this.menuActionRevert = new System.Windows.Forms.MenuItem();
            this.menuActionProperties = new System.Windows.Forms.MenuItem();
            this.menuItem4 = new System.Windows.Forms.MenuItem();
            this.migrationMenuItem = new System.Windows.Forms.MenuItem();
            this.migrationMenuSubItem = new System.Windows.Forms.MenuItem();
            this.menuSeparator = new System.Windows.Forms.MenuItem();
            this.menuActionClose = new System.Windows.Forms.MenuItem();
            this.menuActionExit = new System.Windows.Forms.MenuItem();
            this.menuEdit = new System.Windows.Forms.MenuItem();
            this.menuViewAccounts = new System.Windows.Forms.MenuItem();
            this.menuEditPrefs = new System.Windows.Forms.MenuItem();
            this.menuView = new System.Windows.Forms.MenuItem();
            this.menuViewRefresh = new System.Windows.Forms.MenuItem();
            this.menuItem1 = new System.Windows.Forms.MenuItem();
            this.menuViewLog = new System.Windows.Forms.MenuItem();
            this.menuItem3 = new System.Windows.Forms.MenuItem();
            this.menuViewAvailable = new System.Windows.Forms.MenuItem();
            this.menuSecurity = new System.Windows.Forms.MenuItem();
            this.menuRecoverKeys = new System.Windows.Forms.MenuItem();
            this.menuResetPassphrase = new System.Windows.Forms.MenuItem();
            this.menuResetPassword = new System.Windows.Forms.MenuItem();
            this.menuHelp = new System.Windows.Forms.MenuItem();
            this.menuHelpHelp = new System.Windows.Forms.MenuItem();
            this.menuHelpAbout = new System.Windows.Forms.MenuItem();
            this.menuHelpUpgrade = new System.Windows.Forms.MenuItem();
            this.progressBar1 = new System.Windows.Forms.ProgressBar();
            this.statusBar1 = new System.Windows.Forms.StatusBar();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.searchTimer = new System.Windows.Forms.Timer(this.components);
            this.refreshTimer = new System.Windows.Forms.Timer(this.components);
            this.toolStripiFolderActions = new System.Windows.Forms.ToolStrip();
            this.toolStripBtnCreate = new System.Windows.Forms.ToolStripButton();
            this.toolStripBtnSyncNow = new System.Windows.Forms.ToolStripButton();
            this.toolStripBtnShare = new System.Windows.Forms.ToolStripButton();
            this.toolStripBtnResolve = new System.Windows.Forms.ToolStripButton();
            this.toolStripBtnRevert = new System.Windows.Forms.ToolStripButton();
            this.toolStripBtnDownload = new System.Windows.Forms.ToolStripButton();
            this.toolStripBtnMerge = new System.Windows.Forms.ToolStripButton();
            this.toolStripBtnDelete = new System.Windows.Forms.ToolStripButton();
            this.toolStipBtnChangeView = new System.Windows.Forms.ToolStripDropDownButton();
            this.toolStripMenuThumbnails = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuLeftPane = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripBtnFilter = new System.Windows.Forms.ToolStripComboBox();
            this.iFolderContextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.CtxMenuOpen = new System.Windows.Forms.ToolStripMenuItem();
            this.MenuResolveSeperator = new System.Windows.Forms.ToolStripSeparator();
            this.MenuResolve = new System.Windows.Forms.ToolStripMenuItem();
            this.MenuAccept = new System.Windows.Forms.ToolStripMenuItem();
            this.MenuMerge = new System.Windows.Forms.ToolStripMenuItem();
            this.menuSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.MenuRemove = new System.Windows.Forms.ToolStripMenuItem();
            this.menuSyncNow = new System.Windows.Forms.ToolStripMenuItem();
            this.menuShare = new System.Windows.Forms.ToolStripMenuItem();
            this.MenuRevert = new System.Windows.Forms.ToolStripMenuItem();
            this.menuSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.menuProperties = new System.Windows.Forms.ToolStripMenuItem();
            this.MenuRefresh = new System.Windows.Forms.ToolStripMenuItem();
            this.panel3 = new System.Windows.Forms.Panel();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.LoginLogoff = new System.Windows.Forms.Button();
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.label1 = new System.Windows.Forms.Label();
            this.filter = new System.Windows.Forms.TextBox();
            this.titleAvailable = new System.Windows.Forms.Label();
            this.titleUsed = new System.Windows.Forms.Label();
            this.titleDiskQuota = new System.Windows.Forms.Label();
            this.titleNOFolders = new System.Windows.Forms.Label();
            this.titleServer = new System.Windows.Forms.Label();
            this.titleUser = new System.Windows.Forms.Label();
            this.domainListComboBox = new System.Windows.Forms.ComboBox();
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.titleName = new System.Windows.Forms.Label();
            this.titleServerorSize = new System.Windows.Forms.Label();
            this.titleAccess = new System.Windows.Forms.Label();
            this.titleEncrypted = new System.Windows.Forms.Label();
            this.titleOwner = new System.Windows.Forms.Label();
            this.titleLastSyncTime = new System.Windows.Forms.Label();
            this.directorySearcher1 = new System.DirectoryServices.DirectorySearcher();
            this.panel2 = new System.Windows.Forms.Panel();
            this.iFolderView = new Novell.FormsTrayApp.TileListView();
            this.localiFoldersHeading = new System.Windows.Forms.RichTextBox();
            this.listView1 = new System.Windows.Forms.ListView();
            this.iFolder = new System.Windows.Forms.ColumnHeader();
            this.Size = new System.Windows.Forms.ColumnHeader();
            this.Server = new System.Windows.Forms.ColumnHeader();
            this.Status = new System.Windows.Forms.ColumnHeader();
            this.panel1 = new System.Windows.Forms.Panel();
            this.toolStripiFolderActions.SuspendLayout();
            this.iFolderContextMenu.SuspendLayout();
            this.panel3.SuspendLayout();
            this.groupBox2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            this.tableLayoutPanel1.SuspendLayout();
            this.panel2.SuspendLayout();
            this.panel1.SuspendLayout();
            this.SuspendLayout();



            this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuAction,
            this.menuEdit,
            this.menuView,
            this.menuSecurity,
            this.menuHelp});



            this.menuAction.Index = 0;
            this.menuAction.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuActionCreate,
            this.menuActionAccept,
            this.menuActionMerge,
            this.menuActionRemove,
            this.menuItem7,
            this.menuActionOpen,
            this.menuActionShare,
            this.menuActionResolve,
            this.menuActionSync,
            this.menuActionRevert,
            this.menuActionProperties,
            this.menuItem4,
            this.migrationMenuItem,
            this.menuSeparator,
            this.menuActionClose,
            this.menuActionExit});
            resources.ApplyResources(this.menuAction, "menuAction");



            this.menuActionCreate.Index = 0;
            resources.ApplyResources(this.menuActionCreate, "menuActionCreate");
            this.menuActionCreate.Click += new System.EventHandler(this.menuCreate_Click);



            resources.ApplyResources(this.menuActionAccept, "menuActionAccept");
            this.menuActionAccept.Index = 1;
            this.menuActionAccept.Click += new System.EventHandler(this.menuAccept_Click);



            resources.ApplyResources(this.menuActionMerge, "menuActionMerge");
            this.menuActionMerge.Index = 2;
            this.menuActionMerge.Click += new System.EventHandler(this.menuMerge_Click);



            resources.ApplyResources(this.menuActionRemove, "menuActionRemove");
            this.menuActionRemove.Index = 3;
            this.menuActionRemove.Click += new System.EventHandler(this.menuRemove_Click);



            this.menuItem7.Index = 4;
            resources.ApplyResources(this.menuItem7, "menuItem7");



            resources.ApplyResources(this.menuActionOpen, "menuActionOpen");
            this.menuActionOpen.Index = 5;
            this.menuActionOpen.Click += new System.EventHandler(this.menuOpen_Click);



            resources.ApplyResources(this.menuActionShare, "menuActionShare");
            this.menuActionShare.Index = 6;
            this.menuActionShare.Click += new System.EventHandler(this.menuShare_Click);



            resources.ApplyResources(this.menuActionResolve, "menuActionResolve");
            this.menuActionResolve.Index = 7;
            this.menuActionResolve.Click += new System.EventHandler(this.menuResolve_Click);



            resources.ApplyResources(this.menuActionSync, "menuActionSync");
            this.menuActionSync.Index = 8;
            this.menuActionSync.Click += new System.EventHandler(this.menuSyncNow_Click);



            resources.ApplyResources(this.menuActionRevert, "menuActionRevert");
            this.menuActionRevert.Index = 9;
            this.menuActionRevert.Click += new System.EventHandler(this.menuRevert_Click);



            resources.ApplyResources(this.menuActionProperties, "menuActionProperties");
            this.menuActionProperties.Index = 10;
            this.menuActionProperties.Click += new System.EventHandler(this.menuProperties_Click);



            this.menuItem4.Index = 11;
            resources.ApplyResources(this.menuItem4, "menuItem4");



            this.migrationMenuItem.Index = 12;
            this.migrationMenuItem.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.migrationMenuSubItem});
            resources.ApplyResources(this.migrationMenuItem, "migrationMenuItem");



            this.migrationMenuSubItem.Index = 0;
            resources.ApplyResources(this.migrationMenuSubItem, "migrationMenuSubItem");
            this.migrationMenuSubItem.Click += new System.EventHandler(this.menuMigrateMigrate_Click);



            this.menuSeparator.Index = 13;
            resources.ApplyResources(this.menuSeparator, "menuSeparator");



            this.menuActionClose.Index = 14;
            resources.ApplyResources(this.menuActionClose, "menuActionClose");
            this.menuActionClose.Click += new System.EventHandler(this.menuClose_Click);



            this.menuActionExit.Index = 15;
            resources.ApplyResources(this.menuActionExit, "menuActionExit");
            this.menuActionExit.Click += new System.EventHandler(this.menuFileExit_Click);



            this.menuEdit.Index = 1;
            this.menuEdit.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuViewAccounts,
            this.menuEditPrefs});
            resources.ApplyResources(this.menuEdit, "menuEdit");



            this.menuViewAccounts.Index = 0;
            resources.ApplyResources(this.menuViewAccounts, "menuViewAccounts");
            this.menuViewAccounts.Click += new System.EventHandler(this.menuViewAccounts_Click);



            this.menuEditPrefs.Index = 1;
            resources.ApplyResources(this.menuEditPrefs, "menuEditPrefs");
            this.menuEditPrefs.Click += new System.EventHandler(this.menuEditPrefs_Click);



            this.menuView.Index = 2;
            this.menuView.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuViewRefresh,
            this.menuItem1,
            this.menuViewLog,
            this.menuItem3,
            this.menuViewAvailable});
            resources.ApplyResources(this.menuView, "menuView");



            this.menuViewRefresh.Index = 0;
            resources.ApplyResources(this.menuViewRefresh, "menuViewRefresh");
            this.menuViewRefresh.Click += new System.EventHandler(this.menuRefresh_Click);



            this.menuItem1.Index = 1;
            resources.ApplyResources(this.menuItem1, "menuItem1");



            this.menuViewLog.Index = 2;
            resources.ApplyResources(this.menuViewLog, "menuViewLog");
            this.menuViewLog.Click += new System.EventHandler(this.menuViewLog_Click);



            this.menuItem3.Index = 3;
            resources.ApplyResources(this.menuItem3, "menuItem3");



            this.menuViewAvailable.Checked = true;
            this.menuViewAvailable.Index = 4;
            resources.ApplyResources(this.menuViewAvailable, "menuViewAvailable");
            this.menuViewAvailable.Click += new System.EventHandler(this.showiFolders_Click);



            this.menuSecurity.Index = 3;
            this.menuSecurity.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuRecoverKeys,
            this.menuResetPassphrase,
            this.menuResetPassword});
            resources.ApplyResources(this.menuSecurity, "menuSecurity");



            this.menuRecoverKeys.Index = 0;
            resources.ApplyResources(this.menuRecoverKeys, "menuRecoverKeys");
            this.menuRecoverKeys.Click += new System.EventHandler(this.menuRecoverKeys_Click);



            this.menuResetPassphrase.Index = 1;
            resources.ApplyResources(this.menuResetPassphrase, "menuResetPassphrase");
            this.menuResetPassphrase.Click += new System.EventHandler(this.menuResetPassphrase_Select);



            this.menuResetPassword.Index = 2;
            resources.ApplyResources(this.menuResetPassword, "menuResetPassword");
            this.menuResetPassword.Click += new System.EventHandler(this.menuResetPassword_Click);



            this.menuHelp.Index = 4;
            this.menuHelp.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuHelpHelp,
            this.menuHelpUpgrade,
            this.menuHelpAbout});
            resources.ApplyResources(this.menuHelp, "menuHelp");



            this.menuHelpHelp.Index = 0;
            resources.ApplyResources(this.menuHelpHelp, "menuHelpHelp");
            this.menuHelpHelp.Click += new System.EventHandler(this.menuHelpHelp_Click);



            this.menuHelpAbout.Index = 2;
            resources.ApplyResources(this.menuHelpAbout, "menuHelpAbout");
            this.menuHelpAbout.Click += new System.EventHandler(this.menuHelpAbout_Click);



            this.menuHelpUpgrade.Index = 1;
            resources.ApplyResources(this.menuHelpUpgrade, "menuHelpUpgrade");
            this.menuHelpUpgrade.Click += new System.EventHandler(this.menuHelpUpgrade_Click);



            resources.ApplyResources(this.progressBar1, "progressBar1");
            this.progressBar1.Name = "progressBar1";



            resources.ApplyResources(this.statusBar1, "statusBar1");
            this.statusBar1.Name = "statusBar1";



            resources.ApplyResources(this.groupBox1, "groupBox1");
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.TabStop = false;



            this.searchTimer.Interval = 1000;
            this.searchTimer.Tick += new System.EventHandler(this.searchTimer_Tick);



            this.refreshTimer.Interval = 300000;
            this.refreshTimer.Tick += new System.EventHandler(this.refreshTimer_Tick);



            this.toolStripiFolderActions.BackColor = System.Drawing.Color.Gainsboro;
            this.toolStripiFolderActions.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.toolStripiFolderActions.ImageScalingSize = new System.Drawing.Size(48, 48);
            this.toolStripiFolderActions.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripBtnCreate,
            this.toolStripBtnSyncNow,
            this.toolStripBtnShare,
            this.toolStripBtnResolve,
            this.toolStripBtnRevert,
            this.toolStripBtnDownload,
            this.toolStripBtnMerge,
            this.toolStripBtnDelete,
            this.toolStipBtnChangeView,
            this.toolStripBtnFilter});
            resources.ApplyResources(this.toolStripiFolderActions, "toolStripiFolderActions");
            this.toolStripiFolderActions.Name = "toolStripiFolderActions";



            resources.ApplyResources(this.toolStripBtnCreate, "toolStripBtnCreate");
            this.toolStripBtnCreate.Name = "toolStripBtnCreate";
            this.toolStripBtnCreate.Tag = "Create";
            this.toolStripBtnCreate.Click += new System.EventHandler(this.menuCreate_Click);



            resources.ApplyResources(this.toolStripBtnSyncNow, "toolStripBtnSyncNow");
            this.toolStripBtnSyncNow.Name = "toolStripBtnSyncNow";
            this.toolStripBtnSyncNow.Click += new System.EventHandler(this.menuSyncNow_Click);



            resources.ApplyResources(this.toolStripBtnShare, "toolStripBtnShare");
            this.toolStripBtnShare.Name = "toolStripBtnShare";
            this.toolStripBtnShare.Click += new System.EventHandler(this.menuShare_Click);



            resources.ApplyResources(this.toolStripBtnResolve, "toolStripBtnResolve");
            this.toolStripBtnResolve.Name = "toolStripBtnResolve";
            this.toolStripBtnResolve.Click += new System.EventHandler(this.menuResolve_Click);



            resources.ApplyResources(this.toolStripBtnRevert, "toolStripBtnRevert");
            this.toolStripBtnRevert.Name = "toolStripBtnRevert";
            this.toolStripBtnRevert.Click += new System.EventHandler(this.menuRevert_Click);



            resources.ApplyResources(this.toolStripBtnDownload, "toolStripBtnDownload");
            this.toolStripBtnDownload.Name = "toolStripBtnDownload";
            this.toolStripBtnDownload.Click += new System.EventHandler(this.menuAccept_Click);



            resources.ApplyResources(this.toolStripBtnMerge, "toolStripBtnMerge");
            this.toolStripBtnMerge.Name = "toolStripBtnMerge";
            this.toolStripBtnMerge.Click += new System.EventHandler(this.menuMerge_Click);



            resources.ApplyResources(this.toolStripBtnDelete, "toolStripBtnDelete");
            this.toolStripBtnDelete.Name = "toolStripBtnDelete";
            this.toolStripBtnDelete.Click += new System.EventHandler(this.menuRemove_Click);



            this.toolStipBtnChangeView.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
            this.toolStipBtnChangeView.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuThumbnails,
            this.toolStripMenuLeftPane});
            resources.ApplyResources(this.toolStipBtnChangeView, "toolStipBtnChangeView");
            this.toolStipBtnChangeView.Name = "toolStipBtnChangeView";



            this.toolStripMenuThumbnails.Name = "toolStripMenuThumbnails";
            resources.ApplyResources(this.toolStripMenuThumbnails, "toolStripMenuThumbnails");
            this.toolStripMenuThumbnails.Click += new System.EventHandler(this.toolStripMenuThumbnails_Click);



            this.toolStripMenuLeftPane.Name = "toolStripMenuLeftPane";
            resources.ApplyResources(this.toolStripMenuLeftPane, "toolStripMenuLeftPane");
            this.toolStripMenuLeftPane.Click += new System.EventHandler(this.toolStripMenuLeftPane_Click);



            this.toolStripBtnFilter.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
            this.toolStripBtnFilter.Name = "toolStripBtnFilter";
            resources.ApplyResources(this.toolStripBtnFilter, "toolStripBtnFilter");
            this.toolStripBtnFilter.TextUpdate += new System.EventHandler(this.toolStripBtnFilter_TextUpdate);



            this.iFolderContextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.CtxMenuOpen,
            this.MenuResolveSeperator,
            this.MenuResolve,
            this.MenuAccept,
            this.MenuMerge,
            this.menuSeparator1,
            this.MenuRemove,
            this.menuSyncNow,
            this.menuShare,
            this.MenuRevert,
            this.menuSeparator2,
            this.menuProperties,
            this.MenuRefresh});
            this.iFolderContextMenu.Name = "iFolderContextMenu";
            resources.ApplyResources(this.iFolderContextMenu, "iFolderContextMenu");
            this.iFolderContextMenu.Opening += new System.ComponentModel.CancelEventHandler(this.iFolderContextMenu_Popup);



            this.CtxMenuOpen.Name = "CtxMenuOpen";
            resources.ApplyResources(this.CtxMenuOpen, "CtxMenuOpen");
            this.CtxMenuOpen.Click += new System.EventHandler(this.menuOpen_Click);



            this.MenuResolveSeperator.Name = "MenuResolveSeperator";
            resources.ApplyResources(this.MenuResolveSeperator, "MenuResolveSeperator");



            this.MenuResolve.Name = "MenuResolve";
            resources.ApplyResources(this.MenuResolve, "MenuResolve");
            this.MenuResolve.Click += new System.EventHandler(this.menuResolve_Click);



            this.MenuAccept.Name = "MenuAccept";
            resources.ApplyResources(this.MenuAccept, "MenuAccept");
            this.MenuAccept.Click += new System.EventHandler(this.menuAccept_Click);



            this.MenuMerge.Name = "MenuMerge";
            resources.ApplyResources(this.MenuMerge, "MenuMerge");
            this.MenuMerge.Click += new System.EventHandler(this.menuMerge_Click);



            this.menuSeparator1.Name = "menuSeparator1";
            resources.ApplyResources(this.menuSeparator1, "menuSeparator1");



            this.MenuRemove.Name = "MenuRemove";
            resources.ApplyResources(this.MenuRemove, "MenuRemove");
            this.MenuRemove.Click += new System.EventHandler(this.menuRemove_Click);



            this.menuSyncNow.Name = "menuSyncNow";
            resources.ApplyResources(this.menuSyncNow, "menuSyncNow");
            this.menuSyncNow.Click += new System.EventHandler(this.menuSyncNow_Click);



            this.menuShare.Name = "menuShare";
            resources.ApplyResources(this.menuShare, "menuShare");
            this.menuShare.Click += new System.EventHandler(this.menuShare_Click);



            this.MenuRevert.Name = "MenuRevert";
            resources.ApplyResources(this.MenuRevert, "MenuRevert");
            this.MenuRevert.Click += new System.EventHandler(this.menuRevert_Click);



            this.menuSeparator2.Name = "menuSeparator2";
            resources.ApplyResources(this.menuSeparator2, "menuSeparator2");



            this.menuProperties.Name = "menuProperties";
            resources.ApplyResources(this.menuProperties, "menuProperties");
            this.menuProperties.Click += new System.EventHandler(this.menuProperties_Click);



            this.MenuRefresh.Name = "MenuRefresh";
            resources.ApplyResources(this.MenuRefresh, "MenuRefresh");
            this.MenuRefresh.Click += new System.EventHandler(this.menuRefresh_Click);



            resources.ApplyResources(this.panel3, "panel3");
            this.panel3.BackColor = System.Drawing.Color.DarkGray;
            this.panel3.Controls.Add(this.groupBox2);
            this.panel3.Name = "panel3";



            resources.ApplyResources(this.groupBox2, "groupBox2");
            this.groupBox2.BackColor = System.Drawing.Color.Gainsboro;
            this.groupBox2.Controls.Add(this.LoginLogoff);
            this.groupBox2.Controls.Add(this.pictureBox1);
            this.groupBox2.Controls.Add(this.label1);
            this.groupBox2.Controls.Add(this.filter);
            this.groupBox2.Controls.Add(this.titleAvailable);
            this.groupBox2.Controls.Add(this.titleUsed);
            this.groupBox2.Controls.Add(this.titleDiskQuota);
            this.groupBox2.Controls.Add(this.titleNOFolders);
            this.groupBox2.Controls.Add(this.titleServer);
            this.groupBox2.Controls.Add(this.titleUser);
            this.groupBox2.Controls.Add(this.domainListComboBox);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.TabStop = false;



            resources.ApplyResources(this.LoginLogoff, "LoginLogoff");
            this.LoginLogoff.Name = "LoginLogoff";
            this.LoginLogoff.UseVisualStyleBackColor = true;
            this.LoginLogoff.Click += new System.EventHandler(this.LoginLogoff_Click);



            this.pictureBox1.ErrorImage = null;
            resources.ApplyResources(this.pictureBox1, "pictureBox1");
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.TabStop = false;



            resources.ApplyResources(this.label1, "label1");
            this.label1.Name = "label1";



            resources.ApplyResources(this.filter, "filter");
            this.filter.Name = "filter";



            resources.ApplyResources(this.titleAvailable, "titleAvailable");
            this.titleAvailable.Name = "titleAvailable";



            resources.ApplyResources(this.titleUsed, "titleUsed");
            this.titleUsed.Name = "titleUsed";



            resources.ApplyResources(this.titleDiskQuota, "titleDiskQuota");
            this.titleDiskQuota.Name = "titleDiskQuota";



            resources.ApplyResources(this.titleNOFolders, "titleNOFolders");
            this.titleNOFolders.Name = "titleNOFolders";



            resources.ApplyResources(this.titleServer, "titleServer");
            this.titleServer.Name = "titleServer";



            resources.ApplyResources(this.titleUser, "titleUser");
            this.titleUser.Name = "titleUser";



            this.domainListComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.domainListComboBox.FormattingEnabled = true;
            resources.ApplyResources(this.domainListComboBox, "domainListComboBox");
            this.domainListComboBox.Name = "domainListComboBox";
            this.domainListComboBox.SelectedIndexChanged += new System.EventHandler(this.serverListComboBox_SelectedIndexChanged);



            resources.ApplyResources(this.tableLayoutPanel1, "tableLayoutPanel1");
            this.tableLayoutPanel1.BackColor = System.Drawing.Color.Gainsboro;
            this.tableLayoutPanel1.Controls.Add(this.titleName, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.titleServerorSize, 1, 0);
            this.tableLayoutPanel1.Controls.Add(this.titleAccess, 0, 1);
            this.tableLayoutPanel1.Controls.Add(this.titleEncrypted, 1, 1);
            this.tableLayoutPanel1.Controls.Add(this.titleOwner, 0, 2);
            this.tableLayoutPanel1.Controls.Add(this.titleLastSyncTime, 1, 2);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";



            resources.ApplyResources(this.titleName, "titleName");
            this.titleName.Name = "titleName";



            resources.ApplyResources(this.titleServerorSize, "titleServerorSize");
            this.titleServerorSize.Name = "titleServerorSize";



            resources.ApplyResources(this.titleAccess, "titleAccess");
            this.titleAccess.Name = "titleAccess";



            resources.ApplyResources(this.titleEncrypted, "titleEncrypted");
            this.titleEncrypted.Name = "titleEncrypted";



            resources.ApplyResources(this.titleOwner, "titleOwner");
            this.titleOwner.Name = "titleOwner";



            resources.ApplyResources(this.titleLastSyncTime, "titleLastSyncTime");
            this.titleLastSyncTime.Name = "titleLastSyncTime";



            this.directorySearcher1.ClientTimeout = System.TimeSpan.Parse("-00:00:01");
            this.directorySearcher1.ServerPageTimeLimit = System.TimeSpan.Parse("-00:00:01");
            this.directorySearcher1.ServerTimeLimit = System.TimeSpan.Parse("-00:00:01");



            resources.ApplyResources(this.panel2, "panel2");
            this.panel2.BackColor = System.Drawing.Color.White;
            this.panel2.ContextMenuStrip = this.iFolderContextMenu;
            this.panel2.Controls.Add(this.iFolderView);
            this.panel2.Controls.Add(this.localiFoldersHeading);
            this.panel2.Name = "panel2";
            this.panel2.MouseDown += new System.Windows.Forms.MouseEventHandler(this.panel2_MouseDown);



            resources.ApplyResources(this.iFolderView, "iFolderView");
            this.iFolderView.BackColor = System.Drawing.Color.White;
            this.iFolderView.ContextMenuStrip = this.iFolderContextMenu;
            this.iFolderView.HorizontalSpacing = 5;
            this.iFolderView.ItemHeight = 72;
            this.iFolderView.ItemWidth = 280;
            this.iFolderView.LargeImageList = null;
            this.iFolderView.Name = "iFolderView";
            this.iFolderView.SelectedItem = null;
            this.iFolderView.VerticleSpacing = 5;
            this.iFolderView.DoubleClick += new System.EventHandler(this.iFolderView_DoubleClick);
            this.iFolderView.NavigateItem += new Novell.FormsTrayApp.TileListView.NavigateItemDelegate(this.iFolderView_NavigateItem);
            this.iFolderView.SelectedIndexChanged += new Novell.FormsTrayApp.TileListView.SelectedIndexChangedDelegate(this.ifListView_SelectedIndexChanged);



            this.localiFoldersHeading.BackColor = System.Drawing.Color.LightGray;
            this.localiFoldersHeading.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.localiFoldersHeading.Cursor = System.Windows.Forms.Cursors.Arrow;
            resources.ApplyResources(this.localiFoldersHeading, "localiFoldersHeading");
            this.localiFoldersHeading.ForeColor = System.Drawing.SystemColors.Desktop;
            this.localiFoldersHeading.Name = "localiFoldersHeading";
            this.localiFoldersHeading.ReadOnly = true;
            this.localiFoldersHeading.TabStop = false;



            resources.ApplyResources(this.listView1, "listView1");
            this.listView1.BackColor = System.Drawing.Color.White;
            this.listView1.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.iFolder,
            this.Size,
            this.Server,
            this.Status});
            this.listView1.ContextMenuStrip = this.iFolderContextMenu;
            this.listView1.ForeColor = System.Drawing.SystemColors.WindowText;
            this.listView1.FullRowSelect = true;
            this.listView1.HideSelection = false;
            this.listView1.MultiSelect = false;
            this.listView1.Name = "listView1";
            this.listView1.ShowItemToolTips = true;
            this.listView1.Sorting = System.Windows.Forms.SortOrder.Ascending;
            this.listView1.UseCompatibleStateImageBehavior = false;
            this.listView1.View = System.Windows.Forms.View.Details;
            this.listView1.DoubleClick += new System.EventHandler(this.iFolderView_DoubleClick);
            this.listView1.ColumnClick += new System.Windows.Forms.ColumnClickEventHandler(this.listView1_ColumnClick);
            this.listView1.ItemSelectionChanged += new System.Windows.Forms.ListViewItemSelectionChangedEventHandler(this.listView1_ItemSelectionChanged);



            resources.ApplyResources(this.iFolder, "iFolder");



            resources.ApplyResources(this.Size, "Size");



            resources.ApplyResources(this.Server, "Server");



            resources.ApplyResources(this.Status, "Status");



            resources.ApplyResources(this.panel1, "panel1");
            this.panel1.Controls.Add(this.listView1);
            this.panel1.Name = "panel1";



            resources.ApplyResources(this, "$this");
            this.BackColor = System.Drawing.Color.LightGray;
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.panel3);
            this.Controls.Add(this.panel2);
            this.Controls.Add(this.tableLayoutPanel1);
            this.Controls.Add(this.toolStripiFolderActions);
            this.Controls.Add(this.progressBar1);
            this.Controls.Add(this.statusBar1);
            this.Controls.Add(this.groupBox1);
            this.KeyPreview = true;
            this.Menu = this.mainMenu1;
            this.Name = "GlobalProperties";
            this.Load += new System.EventHandler(this.GlobalProperties_Load);
            this.SizeChanged += new System.EventHandler(this.GlobalProperties_SizeChanged);
            this.VisibleChanged += new System.EventHandler(this.GlobalProperties_VisibleChanged);
            this.Closing += new System.ComponentModel.CancelEventHandler(this.GlobalProperties_Closing);
            this.Move += new System.EventHandler(this.GlobalProperties_Move);
            this.Resize += new System.EventHandler(this.GlobalProperties_SizeChanged);
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.GlobalProperties_KeyDown);
            this.toolStripiFolderActions.ResumeLayout(false);
            this.toolStripiFolderActions.PerformLayout();
            this.iFolderContextMenu.ResumeLayout(false);
            this.panel3.ResumeLayout(false);
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            this.tableLayoutPanel1.ResumeLayout(false);
            this.tableLayoutPanel1.PerformLayout();
            this.panel2.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }


  protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        private MainMenu mainMenu1;
        private GroupBox groupBox1;
        private MenuItem menuActionAccept;
        private MenuItem menuAction;
        private MenuItem menuActionOpen;
        private MenuItem menuActionCreate;
        private MenuItem menuActionMerge;
        private MenuItem menuItem4;
        private MenuItem menuItem7;
        private MenuItem menuActionShare;
        private MenuItem menuActionResolve;
        private MenuItem menuActionSync;
        private MenuItem menuActionRevert;
        private MenuItem menuActionRemove;
        private MenuItem menuActionProperties;
        private MenuItem menuSeparator;
        private MenuItem menuEdit;
        private MenuItem menuViewAccounts;
        private MenuItem menuEditPrefs;
        private MenuItem menuView;
        private MenuItem menuViewRefresh;
        private MenuItem menuItem1;
        private MenuItem menuViewLog;
        private MenuItem menuItem3;
        private MenuItem menuViewAvailable;
        private MenuItem menuSecurity;
        private MenuItem menuRecoverKeys;
        private MenuItem menuResetPassphrase;
        private MenuItem menuResetPassword;
        private MenuItem menuHelp;
        private MenuItem menuHelpHelp;
        private MenuItem menuHelpAbout;
        private ToolStrip toolStripiFolderActions;
        private ToolStripButton toolStripBtnCreate;
        private ToolStripButton toolStripBtnDownload;
        private ToolStripButton toolStripBtnSyncNow;
        private ToolStripButton toolStripBtnShare;
        private ToolStripComboBox toolStripBtnFilter;
        private ToolStripDropDownButton toolStipBtnChangeView;
        private ToolStripButton toolStripBtnResolve;
        private ToolStripButton toolStripBtnMerge;


        private System.Windows.Forms.MenuItem migrationMenuItem;
        private System.Windows.Forms.MenuItem migrationMenuSubItem;
        private System.Windows.Forms.MenuItem menuActionClose;
        private System.Windows.Forms.MenuItem menuActionExit;
        private System.Windows.Forms.ProgressBar progressBar1;
        private System.Windows.Forms.StatusBar statusBar1;
        private System.Windows.Forms.MenuItem menuClose;


        private System.Windows.Forms.MenuItem MigrationMenuItem;
        private System.Windows.Forms.MenuItem MigrationMenuSubItem;
        private Panel panel3;
        private TextBox filter;
        private ContextMenuStrip iFolderContextMenu;
        private ToolStripMenuItem CtxMenuOpen;
        private ToolStripSeparator MenuResolveSeperator;
        private ToolStripMenuItem MenuResolve;
        private ToolStripMenuItem MenuAccept;
        private ToolStripMenuItem MenuMerge;
        private ToolStripSeparator menuSeparator1;
        private ToolStripMenuItem MenuRemove;
        private ToolStripMenuItem menuSyncNow;
        private ToolStripMenuItem menuShare;
        private ToolStripMenuItem MenuRevert;
        private ToolStripSeparator menuSeparator2;
        private ToolStripMenuItem menuProperties;
        private ToolStripMenuItem MenuRefresh;
        private System.DirectoryServices.DirectorySearcher directorySearcher1;
        private TableLayoutPanel tableLayoutPanel1;
        private Label titleName;
        private Label titleOwner;
        private Label titleEncrypted;
        private Label titleServerorSize;
        private Label titleLastSyncTime;
        private Label titleAccess;
        private Panel panel2;
        private TileListView iFolderView;
        private RichTextBox localiFoldersHeading;
        private ToolStripMenuItem toolStripMenuThumbnails;
        private ToolStripMenuItem toolStripMenuLeftPane;
        private ComboBox domainListComboBox;
        private GroupBox groupBox2;
        private Label titleDiskQuota;
        private Label titleNOFolders;
        private Label titleServer;
        private Label titleUser;
        private Label titleAvailable;
        private Label titleUsed;
        private Label label1;
        private PictureBox pictureBox1;
        private Button LoginLogoff;
        private ListView listView1;
        private Panel panel1;
        private ColumnHeader iFolder;
        private ColumnHeader Size;
        private ColumnHeader Server;
        private ColumnHeader Status;
        private ToolStripButton toolStripBtnRevert;
        private ToolStripButton toolStripBtnDelete;
        private MenuItem menuHelpUpgrade;

 }
}
