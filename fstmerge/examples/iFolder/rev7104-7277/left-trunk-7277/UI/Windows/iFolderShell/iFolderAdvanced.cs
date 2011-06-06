using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using System.Threading;
using System.IO;
using System.Net;
using System.Globalization;
using Simias.Client;
using Simias.Client.Event;
using Microsoft.Win32;
using Novell.iFolder.Web;
namespace Novell.iFolderCom
{
 [ComVisible(false)]
 public class iFolderAdvanced : System.Windows.Forms.Form
 {
  private const string member = "Member";
  private const string inviting = "Inviting";
  private const double megaByte = 1048576;
  private delegate void NodeDelegate(iFolderWeb ifolder, iFolderUser ifolderUser, string eventData);
  private NodeDelegate nodeDelegate;
  private delegate void FileSyncDelegate(FileSyncEventArgs fileSyncEventArgs);
  private FileSyncDelegate fileSyncDelegate;
  private delegate void CollectionSyncDelegate(CollectionSyncEventArgs collectionSyncEventArgs);
  private CollectionSyncDelegate collectionSyncDelegate;
  private Queue eventQueue;
  private Thread worker = null;
  protected AutoResetEvent workEvent = null;
  private System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(iFolderAdvanced));
  private System.Windows.Forms.TabControl tabControl1;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button cancel;
  private System.Windows.Forms.Button apply;
  private System.Windows.Forms.ListView shareWith;
  private System.Windows.Forms.ColumnHeader columnHeader1;
  private System.Windows.Forms.Button remove;
  private System.Windows.Forms.Button add;
  private string domainName;
        private string domainUrl;
  private uint objectsToSync;
  private bool startSync;
  private string longName = string.Empty;
  private Hashtable subscrHT;
  private Hashtable userIDHT;
  private IProcEventClient eventClient;
  private bool existingEventClient = true;
  private bool eventError = false;
  private int initTabTop;
  private Size initMinSize;
  private int leftDelta;
  private int bottomDelta;
  private int lvbDelta;
  private int bbDelta;
  private bool accessClick;
  private iFolderWeb currentiFolder;
  private iFolderUser currentUser;
  private ListViewItem ownerLvi;
  private ListViewItem newOwnerLvi;
  private iFolderWebService ifWebService;
  private ArrayList removedList;
  private string loadPath;
  private Control currentControl;
  private Control firstControl;
  private Control lastControl;
  private System.Windows.Forms.ToolTip toolTip1;
  private System.Windows.Forms.HelpProvider helpProvider1;
  private System.Windows.Forms.GroupBox groupBox1;
  private System.Windows.Forms.NumericUpDown syncInterval;
  private System.Windows.Forms.CheckBox autoSync;
  private System.Windows.Forms.Label objectCount;
  private System.Windows.Forms.Label label8;
  private System.Windows.Forms.LinkLabel conflicts;
  private System.Windows.Forms.TabPage tabSharing;
  private System.Windows.Forms.TabPage tabGeneral;
  private System.Windows.Forms.CheckBox setLimit;
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Label used;
  private System.Windows.Forms.Label usedUnits;
  private System.Windows.Forms.Label label9;
  private System.Windows.Forms.Label available;
  private System.Windows.Forms.Label availableUnits;
  private System.Windows.Forms.ComboBox ifolders;
  private System.Windows.Forms.Label label3;
  private System.Windows.Forms.Label label10;
  private System.Windows.Forms.Label ifolderLabel;
  private System.Windows.Forms.Button open;
  private System.Windows.Forms.GroupBox groupBox3;
  private System.Windows.Forms.ContextMenu contextMenu1;
  private System.Windows.Forms.MenuItem menuFullControl;
  private System.Windows.Forms.MenuItem menuReadWrite;
  private System.Windows.Forms.MenuItem menuReadOnly;
  private System.Windows.Forms.Button access;
  private System.Windows.Forms.Label label7;
  private System.Windows.Forms.ColumnHeader columnHeader2;
  private System.Windows.Forms.ColumnHeader columnHeader3;
  private System.Windows.Forms.TextBox limitEdit;
  private System.Windows.Forms.Label limitLabel;
  private System.Windows.Forms.Label limit;
  private Novell.iFolderCom.GaugeChart gaugeChart;
  private System.Windows.Forms.Label syncUnits;
  private System.Windows.Forms.Label label4;
  private System.Windows.Forms.Label lastSync;
  private System.Windows.Forms.Button syncNow;
  private System.Windows.Forms.PictureBox conflictIcon;
  private System.Windows.Forms.Label syncLabel;
  private System.Windows.Forms.GroupBox groupBox2;
  private System.Windows.Forms.PictureBox pictureBox1;
  private System.Windows.Forms.Label iFolderName;
  private System.Windows.Forms.Label lblOwner;
  private System.Windows.Forms.Label lblLocation;
  private System.Windows.Forms.Label lblAccount;
  private System.Windows.Forms.Label iFolderLocation;
  private System.Windows.Forms.Label iFolderAccount;
  private System.Windows.Forms.Label iFolderOwner;
  private System.Windows.Forms.Button btnHelp;
        private System.Windows.Forms.Label securesync;
        private System.Windows.Forms.CheckBox ssl;
  private System.ComponentModel.IContainer components;
  public iFolderAdvanced()
  {
   nodeDelegate = new NodeDelegate(nodeEvent);
   fileSyncDelegate = new FileSyncDelegate(fileSync);
   collectionSyncDelegate = new CollectionSyncDelegate(collectionSync);
   eventQueue = new Queue();
   workEvent = new AutoResetEvent(false);
   if (worker == null)
   {
    worker = new Thread(new ThreadStart(eventThreadProc));
                worker.Name = "iFolder Advanced Event Process";
    worker.IsBackground = true;
    worker.Priority = ThreadPriority.BelowNormal;
    worker.Start();
   }
   InitializeComponent();
   apply.Enabled = remove.Enabled = access.Enabled = false;
   syncInterval.TextChanged += new EventHandler(syncInterval_ValueChanged);
   initTabTop = tabControl1.Top;
   initMinSize = this.Size;
   currentControl = firstControl = this.ifolders;
   lastControl = this.apply;
   resizeButton(syncNow);
   int delta = calculateSize(ifolderLabel, 0);
   ifolderLabel.Width += delta;
   int temp = ifolders.Left;
   ifolders.Left = ifolderLabel.Left + ifolderLabel.Width;
   ifolders.Width -= ifolders.Left - temp;
   leftDelta = tabControl1.Width - shareWith.Right;
   bottomDelta = tabControl1.Height - shareWith.Bottom;
   lvbDelta = access.Top - (shareWith.Top + shareWith.Height);
   bbDelta = remove.Left - (add.Left + add.Width);
   this.StartPosition = FormStartPosition.CenterParent;
  }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if (eventClient != null)
    {
     if (existingEventClient)
     {
      eventClient.SetEvent(IProcEventAction.RemoveNodeChanged, new IProcEventHandler(nodeEventHandler));
      eventClient.SetEvent(IProcEventAction.RemoveNodeCreated, new IProcEventHandler(nodeEventHandler));
      eventClient.SetEvent(IProcEventAction.RemoveNodeDeleted, new IProcEventHandler(nodeEventHandler));
      eventClient.SetEvent(IProcEventAction.RemoveCollectionSync, new IProcEventHandler(collectionSyncHandler));
      eventClient.SetEvent(IProcEventAction.RemoveFileSync, new IProcEventHandler(fileSyncHandler));
     }
     else
     {
      eventClient.Deregister();
     }
    }
    if ((worker != null) && worker.IsAlive)
    {
     worker.Abort();
    }
    if(components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }
  private void InitializeComponent()
  {
   this.components = new System.ComponentModel.Container();
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(iFolderAdvanced));
   this.tabControl1 = new System.Windows.Forms.TabControl();
   this.tabGeneral = new System.Windows.Forms.TabPage();
   this.groupBox1 = new System.Windows.Forms.GroupBox();
   this.syncNow = new System.Windows.Forms.Button();
   this.lastSync = new System.Windows.Forms.Label();
   this.label4 = new System.Windows.Forms.Label();
   this.syncInterval = new System.Windows.Forms.NumericUpDown();
   this.syncUnits = new System.Windows.Forms.Label();
   this.autoSync = new System.Windows.Forms.CheckBox();
   this.objectCount = new System.Windows.Forms.Label();
   this.label8 = new System.Windows.Forms.Label();
   this.groupBox3 = new System.Windows.Forms.GroupBox();
   this.gaugeChart = new Novell.iFolderCom.GaugeChart();
   this.label7 = new System.Windows.Forms.Label();
   this.limitEdit = new System.Windows.Forms.TextBox();
   this.setLimit = new System.Windows.Forms.CheckBox();
   this.label10 = new System.Windows.Forms.Label();
   this.label3 = new System.Windows.Forms.Label();
   this.availableUnits = new System.Windows.Forms.Label();
   this.available = new System.Windows.Forms.Label();
   this.label9 = new System.Windows.Forms.Label();
   this.usedUnits = new System.Windows.Forms.Label();
   this.used = new System.Windows.Forms.Label();
   this.label1 = new System.Windows.Forms.Label();
   this.limitLabel = new System.Windows.Forms.Label();
   this.limit = new System.Windows.Forms.Label();
   this.tabSharing = new System.Windows.Forms.TabPage();
   this.access = new System.Windows.Forms.Button();
   this.add = new System.Windows.Forms.Button();
   this.remove = new System.Windows.Forms.Button();
   this.shareWith = new System.Windows.Forms.ListView();
   this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
   this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
   this.columnHeader3 = new System.Windows.Forms.ColumnHeader();
   this.contextMenu1 = new System.Windows.Forms.ContextMenu();
   this.menuFullControl = new System.Windows.Forms.MenuItem();
   this.menuReadWrite = new System.Windows.Forms.MenuItem();
   this.menuReadOnly = new System.Windows.Forms.MenuItem();
   this.conflicts = new System.Windows.Forms.LinkLabel();
   this.conflictIcon = new System.Windows.Forms.PictureBox();
   this.ok = new System.Windows.Forms.Button();
   this.cancel = new System.Windows.Forms.Button();
   this.apply = new System.Windows.Forms.Button();
   this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
   this.ifolders = new System.Windows.Forms.ComboBox();
   this.ifolderLabel = new System.Windows.Forms.Label();
   this.open = new System.Windows.Forms.Button();
   this.helpProvider1 = new System.Windows.Forms.HelpProvider();
   this.syncLabel = new System.Windows.Forms.Label();
   this.groupBox2 = new System.Windows.Forms.GroupBox();
   this.pictureBox1 = new System.Windows.Forms.PictureBox();
   this.iFolderName = new System.Windows.Forms.Label();
   this.lblOwner = new System.Windows.Forms.Label();
   this.lblLocation = new System.Windows.Forms.Label();
   this.lblAccount = new System.Windows.Forms.Label();
   this.iFolderOwner = new System.Windows.Forms.Label();
   this.iFolderLocation = new System.Windows.Forms.Label();
   this.iFolderAccount = new System.Windows.Forms.Label();
   this.btnHelp = new System.Windows.Forms.Button();
            this.securesync = new System.Windows.Forms.Label();
            this.ssl = new System.Windows.Forms.CheckBox();
   this.tabControl1.SuspendLayout();
   this.tabGeneral.SuspendLayout();
   this.groupBox1.SuspendLayout();
   this.groupBox2.SuspendLayout();
   ((System.ComponentModel.ISupportInitialize)(this.syncInterval)).BeginInit();
   this.groupBox3.SuspendLayout();
   this.tabSharing.SuspendLayout();
   this.SuspendLayout();
   this.tabControl1.AccessibleDescription = resources.GetString("tabControl1.AccessibleDescription");
   this.tabControl1.AccessibleName = resources.GetString("tabControl1.AccessibleName");
   this.tabControl1.Alignment = ((System.Windows.Forms.TabAlignment)(resources.GetObject("tabControl1.Alignment")));
   this.tabControl1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("tabControl1.Anchor")));
   this.tabControl1.Appearance = ((System.Windows.Forms.TabAppearance)(resources.GetObject("tabControl1.Appearance")));
   this.tabControl1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("tabControl1.BackgroundImage")));
   this.tabControl1.Controls.Add(this.tabGeneral);
   this.tabControl1.Controls.Add(this.tabSharing);
   this.tabControl1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("tabControl1.Dock")));
   this.tabControl1.Enabled = ((bool)(resources.GetObject("tabControl1.Enabled")));
   this.tabControl1.Font = ((System.Drawing.Font)(resources.GetObject("tabControl1.Font")));
   this.helpProvider1.SetHelpKeyword(this.tabControl1, resources.GetString("tabControl1.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.tabControl1, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("tabControl1.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.tabControl1, resources.GetString("tabControl1.HelpString"));
   this.tabControl1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("tabControl1.ImeMode")));
   this.tabControl1.ItemSize = ((System.Drawing.Size)(resources.GetObject("tabControl1.ItemSize")));
   this.tabControl1.Location = ((System.Drawing.Point)(resources.GetObject("tabControl1.Location")));
   this.tabControl1.Name = "tabControl1";
   this.tabControl1.Padding = ((System.Drawing.Point)(resources.GetObject("tabControl1.Padding")));
   this.tabControl1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("tabControl1.RightToLeft")));
   this.tabControl1.SelectedIndex = 0;
   this.helpProvider1.SetShowHelp(this.tabControl1, ((bool)(resources.GetObject("tabControl1.ShowHelp"))));
   this.tabControl1.ShowToolTips = ((bool)(resources.GetObject("tabControl1.ShowToolTips")));
   this.tabControl1.Size = ((System.Drawing.Size)(resources.GetObject("tabControl1.Size")));
   this.tabControl1.TabIndex = ((int)(resources.GetObject("tabControl1.TabIndex")));
   this.tabControl1.Text = resources.GetString("tabControl1.Text");
   this.toolTip1.SetToolTip(this.tabControl1, resources.GetString("tabControl1.ToolTip"));
   this.tabControl1.Visible = ((bool)(resources.GetObject("tabControl1.Visible")));
   this.tabGeneral.AccessibleDescription = resources.GetString("tabGeneral.AccessibleDescription");
   this.tabGeneral.AccessibleName = resources.GetString("tabGeneral.AccessibleName");
   this.tabGeneral.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("tabGeneral.Anchor")));
   this.tabGeneral.AutoScroll = ((bool)(resources.GetObject("tabGeneral.AutoScroll")));
   this.tabGeneral.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("tabGeneral.AutoScrollMargin")));
   this.tabGeneral.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("tabGeneral.AutoScrollMinSize")));
   this.tabGeneral.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("tabGeneral.BackgroundImage")));
   this.tabGeneral.Controls.Add(this.groupBox1);
   this.tabGeneral.Controls.Add(this.groupBox2);
   this.tabGeneral.Controls.Add(this.groupBox3);
   this.tabGeneral.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("tabGeneral.Dock")));
   this.tabGeneral.Enabled = ((bool)(resources.GetObject("tabGeneral.Enabled")));
   this.tabGeneral.Font = ((System.Drawing.Font)(resources.GetObject("tabGeneral.Font")));
   this.helpProvider1.SetHelpKeyword(this.tabGeneral, resources.GetString("tabGeneral.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.tabGeneral, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("tabGeneral.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.tabGeneral, resources.GetString("tabGeneral.HelpString"));
   this.tabGeneral.ImageIndex = ((int)(resources.GetObject("tabGeneral.ImageIndex")));
   this.tabGeneral.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("tabGeneral.ImeMode")));
   this.tabGeneral.Location = ((System.Drawing.Point)(resources.GetObject("tabGeneral.Location")));
   this.tabGeneral.Name = "tabGeneral";
   this.tabGeneral.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("tabGeneral.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.tabGeneral, ((bool)(resources.GetObject("tabGeneral.ShowHelp"))));
   this.tabGeneral.Size = ((System.Drawing.Size)(resources.GetObject("tabGeneral.Size")));
   this.tabGeneral.TabIndex = ((int)(resources.GetObject("tabGeneral.TabIndex")));
   this.tabGeneral.Text = resources.GetString("tabGeneral.Text");
   this.toolTip1.SetToolTip(this.tabGeneral, resources.GetString("tabGeneral.ToolTip"));
   this.tabGeneral.ToolTipText = resources.GetString("tabGeneral.ToolTipText");
   this.tabGeneral.Visible = ((bool)(resources.GetObject("tabGeneral.Visible")));
   this.groupBox1.AccessibleDescription = resources.GetString("groupBox1.AccessibleDescription");
   this.groupBox1.AccessibleName = resources.GetString("groupBox1.AccessibleName");
   this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("groupBox1.Anchor")));
   this.groupBox1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("groupBox1.BackgroundImage")));
   this.groupBox1.Controls.Add(this.syncNow);
   this.groupBox1.Controls.Add(this.lastSync);
   this.groupBox1.Controls.Add(this.label4);
   this.groupBox1.Controls.Add(this.syncInterval);
   this.groupBox1.Controls.Add(this.syncUnits);
   this.groupBox1.Controls.Add(this.autoSync);
            this.groupBox1.Controls.Add(this.securesync);
            this.groupBox1.Controls.Add(this.ssl);
   this.groupBox1.Controls.Add(this.objectCount);
   this.groupBox1.Controls.Add(this.label8);
   this.groupBox1.Controls.Add(this.syncLabel);
   this.groupBox1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("groupBox1.Dock")));
   this.groupBox1.Enabled = ((bool)(resources.GetObject("groupBox1.Enabled")));
   this.groupBox1.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.groupBox1.Font = ((System.Drawing.Font)(resources.GetObject("groupBox1.Font")));
   this.helpProvider1.SetHelpKeyword(this.groupBox1, resources.GetString("groupBox1.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.groupBox1, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("groupBox1.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.groupBox1, resources.GetString("groupBox1.HelpString"));
   this.groupBox1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("groupBox1.ImeMode")));
   this.groupBox1.Location = ((System.Drawing.Point)(resources.GetObject("groupBox1.Location")));
   this.groupBox1.Name = "groupBox1";
   this.groupBox1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("groupBox1.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.groupBox1, ((bool)(resources.GetObject("groupBox1.ShowHelp"))));
   this.groupBox1.Size = ((System.Drawing.Size)(resources.GetObject("groupBox1.Size")));
   this.groupBox1.TabIndex = ((int)(resources.GetObject("groupBox1.TabIndex")));
   this.groupBox1.TabStop = false;
   this.groupBox1.Text = resources.GetString("groupBox1.Text");
   this.toolTip1.SetToolTip(this.groupBox1, resources.GetString("groupBox1.ToolTip"));
   this.groupBox1.Visible = ((bool)(resources.GetObject("groupBox1.Visible")));
   this.syncNow.AccessibleDescription = resources.GetString("syncNow.AccessibleDescription");
   this.syncNow.AccessibleName = resources.GetString("syncNow.AccessibleName");
   this.syncNow.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("syncNow.Anchor")));
   this.syncNow.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("syncNow.BackgroundImage")));
   this.syncNow.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("syncNow.Dock")));
   this.syncNow.Enabled = ((bool)(resources.GetObject("syncNow.Enabled")));
   this.syncNow.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("syncNow.FlatStyle")));
   this.syncNow.Font = ((System.Drawing.Font)(resources.GetObject("syncNow.Font")));
   this.helpProvider1.SetHelpKeyword(this.syncNow, resources.GetString("syncNow.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.syncNow, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("syncNow.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.syncNow, resources.GetString("syncNow.HelpString"));
   this.syncNow.Image = ((System.Drawing.Image)(resources.GetObject("syncNow.Image")));
   this.syncNow.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("syncNow.ImageAlign")));
   this.syncNow.ImageIndex = ((int)(resources.GetObject("syncNow.ImageIndex")));
   this.syncNow.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("syncNow.ImeMode")));
   this.syncNow.Location = ((System.Drawing.Point)(resources.GetObject("syncNow.Location")));
   this.syncNow.Name = "syncNow";
   this.syncNow.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("syncNow.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.syncNow, ((bool)(resources.GetObject("syncNow.ShowHelp"))));
   this.syncNow.Size = ((System.Drawing.Size)(resources.GetObject("syncNow.Size")));
   this.syncNow.TabIndex = ((int)(resources.GetObject("syncNow.TabIndex")));
   this.syncNow.Text = resources.GetString("syncNow.Text");
   this.syncNow.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("syncNow.TextAlign")));
   this.toolTip1.SetToolTip(this.syncNow, resources.GetString("syncNow.ToolTip"));
   this.syncNow.Visible = ((bool)(resources.GetObject("syncNow.Visible")));
   this.syncNow.Click += new System.EventHandler(this.syncNow_Click);
   this.lastSync.AccessibleDescription = resources.GetString("lastSync.AccessibleDescription");
   this.lastSync.AccessibleName = resources.GetString("lastSync.AccessibleName");
   this.lastSync.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("lastSync.Anchor")));
   this.lastSync.AutoSize = ((bool)(resources.GetObject("lastSync.AutoSize")));
   this.lastSync.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("lastSync.Dock")));
   this.lastSync.Enabled = ((bool)(resources.GetObject("lastSync.Enabled")));
   this.lastSync.Font = ((System.Drawing.Font)(resources.GetObject("lastSync.Font")));
   this.helpProvider1.SetHelpKeyword(this.lastSync, resources.GetString("lastSync.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.lastSync, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("lastSync.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.lastSync, resources.GetString("lastSync.HelpString"));
   this.lastSync.Image = ((System.Drawing.Image)(resources.GetObject("lastSync.Image")));
   this.lastSync.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("lastSync.ImageAlign")));
   this.lastSync.ImageIndex = ((int)(resources.GetObject("lastSync.ImageIndex")));
   this.lastSync.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("lastSync.ImeMode")));
   this.lastSync.Location = ((System.Drawing.Point)(resources.GetObject("lastSync.Location")));
   this.lastSync.Name = "lastSync";
   this.lastSync.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("lastSync.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.lastSync, ((bool)(resources.GetObject("lastSync.ShowHelp"))));
   this.lastSync.Size = ((System.Drawing.Size)(resources.GetObject("lastSync.Size")));
   this.lastSync.TabIndex = ((int)(resources.GetObject("lastSync.TabIndex")));
   this.lastSync.Text = resources.GetString("lastSync.Text");
   this.lastSync.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("lastSync.TextAlign")));
   this.toolTip1.SetToolTip(this.lastSync, resources.GetString("lastSync.ToolTip"));
   this.lastSync.Visible = ((bool)(resources.GetObject("lastSync.Visible")));
   this.label4.AccessibleDescription = resources.GetString("label4.AccessibleDescription");
   this.label4.AccessibleName = resources.GetString("label4.AccessibleName");
   this.label4.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label4.Anchor")));
   this.label4.AutoSize = ((bool)(resources.GetObject("label4.AutoSize")));
   this.label4.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label4.Dock")));
   this.label4.Enabled = ((bool)(resources.GetObject("label4.Enabled")));
   this.label4.Font = ((System.Drawing.Font)(resources.GetObject("label4.Font")));
   this.helpProvider1.SetHelpKeyword(this.label4, resources.GetString("label4.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.label4, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label4.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.label4, resources.GetString("label4.HelpString"));
   this.label4.Image = ((System.Drawing.Image)(resources.GetObject("label4.Image")));
   this.label4.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label4.ImageAlign")));
   this.label4.ImageIndex = ((int)(resources.GetObject("label4.ImageIndex")));
   this.label4.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label4.ImeMode")));
   this.label4.Location = ((System.Drawing.Point)(resources.GetObject("label4.Location")));
   this.label4.Name = "label4";
   this.label4.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label4.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.label4, ((bool)(resources.GetObject("label4.ShowHelp"))));
   this.label4.Size = ((System.Drawing.Size)(resources.GetObject("label4.Size")));
   this.label4.TabIndex = ((int)(resources.GetObject("label4.TabIndex")));
   this.label4.Text = resources.GetString("label4.Text");
   this.label4.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label4.TextAlign")));
   this.toolTip1.SetToolTip(this.label4, resources.GetString("label4.ToolTip"));
   this.label4.Visible = ((bool)(resources.GetObject("label4.Visible")));
   this.syncInterval.AccessibleDescription = resources.GetString("syncInterval.AccessibleDescription");
   this.syncInterval.AccessibleName = resources.GetString("syncInterval.AccessibleName");
   this.syncInterval.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("syncInterval.Anchor")));
   this.syncInterval.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("syncInterval.Dock")));
   this.syncInterval.Enabled = ((bool)(resources.GetObject("syncInterval.Enabled")));
   this.syncInterval.Font = ((System.Drawing.Font)(resources.GetObject("syncInterval.Font")));
   this.helpProvider1.SetHelpKeyword(this.syncInterval, resources.GetString("syncInterval.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.syncInterval, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("syncInterval.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.syncInterval, resources.GetString("syncInterval.HelpString"));
   this.syncInterval.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("syncInterval.ImeMode")));
   this.syncInterval.Increment = new System.Decimal(new int[] {
                     5,
                     0,
                     0,
                     0});
   this.syncInterval.Location = ((System.Drawing.Point)(resources.GetObject("syncInterval.Location")));
   this.syncInterval.Maximum = new System.Decimal(new int[] {
                   86400,
                   0,
                   0,
                   0});
   this.syncInterval.Minimum = new System.Decimal(new int[] {
                   1,
                   0,
                   0,
                   -2147483648});
   this.syncInterval.Name = "syncInterval";
   this.syncInterval.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("syncInterval.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.syncInterval, ((bool)(resources.GetObject("syncInterval.ShowHelp"))));
   this.syncInterval.Size = ((System.Drawing.Size)(resources.GetObject("syncInterval.Size")));
   this.syncInterval.TabIndex = ((int)(resources.GetObject("syncInterval.TabIndex")));
   this.syncInterval.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("syncInterval.TextAlign")));
   this.syncInterval.ThousandsSeparator = ((bool)(resources.GetObject("syncInterval.ThousandsSeparator")));
   this.toolTip1.SetToolTip(this.syncInterval, resources.GetString("syncInterval.ToolTip"));
   this.syncInterval.UpDownAlign = ((System.Windows.Forms.LeftRightAlignment)(resources.GetObject("syncInterval.UpDownAlign")));
   this.syncInterval.Visible = ((bool)(resources.GetObject("syncInterval.Visible")));
   this.syncUnits.AccessibleDescription = resources.GetString("syncUnits.AccessibleDescription");
   this.syncUnits.AccessibleName = resources.GetString("syncUnits.AccessibleName");
   this.syncUnits.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("syncUnits.Anchor")));
   this.syncUnits.AutoSize = ((bool)(resources.GetObject("syncUnits.AutoSize")));
   this.syncUnits.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("syncUnits.Dock")));
   this.syncUnits.Enabled = ((bool)(resources.GetObject("syncUnits.Enabled")));
   this.syncUnits.Font = ((System.Drawing.Font)(resources.GetObject("syncUnits.Font")));
   this.helpProvider1.SetHelpKeyword(this.syncUnits, resources.GetString("syncUnits.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.syncUnits, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("syncUnits.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.syncUnits, resources.GetString("syncUnits.HelpString"));
   this.syncUnits.Image = ((System.Drawing.Image)(resources.GetObject("syncUnits.Image")));
   this.syncUnits.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("syncUnits.ImageAlign")));
   this.syncUnits.ImageIndex = ((int)(resources.GetObject("syncUnits.ImageIndex")));
   this.syncUnits.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("syncUnits.ImeMode")));
   this.syncUnits.Location = ((System.Drawing.Point)(resources.GetObject("syncUnits.Location")));
   this.syncUnits.Name = "syncUnits";
   this.syncUnits.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("syncUnits.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.syncUnits, ((bool)(resources.GetObject("syncUnits.ShowHelp"))));
   this.syncUnits.Size = ((System.Drawing.Size)(resources.GetObject("syncUnits.Size")));
   this.syncUnits.TabIndex = ((int)(resources.GetObject("syncUnits.TabIndex")));
   this.syncUnits.Text = resources.GetString("syncUnits.Text");
   this.syncUnits.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("syncUnits.TextAlign")));
   this.toolTip1.SetToolTip(this.syncUnits, resources.GetString("syncUnits.ToolTip"));
   this.syncUnits.Visible = ((bool)(resources.GetObject("syncUnits.Visible")));
   this.autoSync.AccessibleDescription = resources.GetString("autoSync.AccessibleDescription");
   this.autoSync.AccessibleName = resources.GetString("autoSync.AccessibleName");
   this.autoSync.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("autoSync.Anchor")));
   this.autoSync.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("autoSync.Appearance")));
   this.autoSync.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("autoSync.BackgroundImage")));
   this.autoSync.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("autoSync.CheckAlign")));
   this.autoSync.Checked = true;
   this.autoSync.CheckState = System.Windows.Forms.CheckState.Checked;
   this.autoSync.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("autoSync.Dock")));
   this.autoSync.Enabled = ((bool)(resources.GetObject("autoSync.Enabled")));
   this.autoSync.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("autoSync.FlatStyle")));
   this.autoSync.Font = ((System.Drawing.Font)(resources.GetObject("autoSync.Font")));
   this.helpProvider1.SetHelpKeyword(this.autoSync, resources.GetString("autoSync.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.autoSync, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("autoSync.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.autoSync, resources.GetString("autoSync.HelpString"));
   this.autoSync.Image = ((System.Drawing.Image)(resources.GetObject("autoSync.Image")));
   this.autoSync.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("autoSync.ImageAlign")));
   this.autoSync.ImageIndex = ((int)(resources.GetObject("autoSync.ImageIndex")));
   this.autoSync.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("autoSync.ImeMode")));
   this.autoSync.Location = ((System.Drawing.Point)(resources.GetObject("autoSync.Location")));
   this.autoSync.Name = "autoSync";
   this.autoSync.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("autoSync.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.autoSync, ((bool)(resources.GetObject("autoSync.ShowHelp"))));
   this.autoSync.Size = ((System.Drawing.Size)(resources.GetObject("autoSync.Size")));
   this.autoSync.TabIndex = ((int)(resources.GetObject("autoSync.TabIndex")));
   this.autoSync.Text = resources.GetString("autoSync.Text");
   this.autoSync.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("autoSync.TextAlign")));
   this.toolTip1.SetToolTip(this.autoSync, resources.GetString("autoSync.ToolTip"));
   this.autoSync.Visible = ((bool)(resources.GetObject("autoSync.Visible")));
   this.autoSync.CheckedChanged += new System.EventHandler(this.autoSync_CheckedChanged);
            this.securesync.AccessibleDescription = resources.GetString("securesync.AccessibleDescription");
            this.securesync.AccessibleName = resources.GetString("securesync.AccessibleName");
            this.securesync.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("securesync.Anchor")));
            this.securesync.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("securesync.BackgroundImage")));
            this.securesync.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("securesync.Dock")));
            this.securesync.Enabled = ((bool)(resources.GetObject("securesync.Enabled")));
            this.securesync.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("securesync.FlatStyle")));
            this.securesync.Font = ((System.Drawing.Font)(resources.GetObject("securesync.Font")));
            this.helpProvider1.SetHelpKeyword(this.securesync, resources.GetString("securesync.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.securesync, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("securesync.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.securesync, resources.GetString("securesync.HelpString"));
            this.securesync.Image = ((System.Drawing.Image)(resources.GetObject("securesync.Image")));
            this.securesync.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("securesync.ImageAlign")));
            this.securesync.ImageIndex = ((int)(resources.GetObject("securesync.ImageIndex")));
            this.securesync.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("securesync.ImeMode")));
            this.securesync.Location = ((System.Drawing.Point)(resources.GetObject("securesync.Location")));
            this.securesync.Name = "securesync";
            this.securesync.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("securesync.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.securesync, ((bool)(resources.GetObject("securesync.ShowHelp"))));
            this.securesync.Size = ((System.Drawing.Size)(resources.GetObject("securesync.Size")));
            this.securesync.TabIndex = ((int)(resources.GetObject("securesync.TabIndex")));
            this.securesync.Text = resources.GetString("securesync.Text");
            this.securesync.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("securesync.TextAlign")));
            this.toolTip1.SetToolTip(this.autoSync, resources.GetString("securesync.ToolTip"));
            this.securesync.Visible = ((bool)(resources.GetObject("securesync.Visible")));
   this.objectCount.AccessibleDescription = resources.GetString("objectCount.AccessibleDescription");
   this.objectCount.AccessibleName = resources.GetString("objectCount.AccessibleName");
   this.objectCount.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("objectCount.Anchor")));
   this.objectCount.AutoSize = ((bool)(resources.GetObject("objectCount.AutoSize")));
   this.objectCount.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("objectCount.Dock")));
   this.objectCount.Enabled = ((bool)(resources.GetObject("objectCount.Enabled")));
   this.objectCount.Font = ((System.Drawing.Font)(resources.GetObject("objectCount.Font")));
   this.helpProvider1.SetHelpKeyword(this.objectCount, resources.GetString("objectCount.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.objectCount, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("objectCount.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.objectCount, resources.GetString("objectCount.HelpString"));
   this.objectCount.Image = ((System.Drawing.Image)(resources.GetObject("objectCount.Image")));
   this.objectCount.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("objectCount.ImageAlign")));
   this.objectCount.ImageIndex = ((int)(resources.GetObject("objectCount.ImageIndex")));
   this.objectCount.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("objectCount.ImeMode")));
   this.objectCount.Location = ((System.Drawing.Point)(resources.GetObject("objectCount.Location")));
   this.objectCount.Name = "objectCount";
   this.objectCount.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("objectCount.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.objectCount, ((bool)(resources.GetObject("objectCount.ShowHelp"))));
   this.objectCount.Size = ((System.Drawing.Size)(resources.GetObject("objectCount.Size")));
   this.objectCount.TabIndex = ((int)(resources.GetObject("objectCount.TabIndex")));
   this.objectCount.Text = resources.GetString("objectCount.Text");
   this.objectCount.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("objectCount.TextAlign")));
   this.toolTip1.SetToolTip(this.objectCount, resources.GetString("objectCount.ToolTip"));
   this.objectCount.Visible = ((bool)(resources.GetObject("objectCount.Visible")));
   this.label8.AccessibleDescription = resources.GetString("label8.AccessibleDescription");
   this.label8.AccessibleName = resources.GetString("label8.AccessibleName");
   this.label8.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label8.Anchor")));
   this.label8.AutoSize = ((bool)(resources.GetObject("label8.AutoSize")));
   this.label8.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label8.Dock")));
   this.label8.Enabled = ((bool)(resources.GetObject("label8.Enabled")));
   this.label8.Font = ((System.Drawing.Font)(resources.GetObject("label8.Font")));
   this.helpProvider1.SetHelpKeyword(this.label8, resources.GetString("label8.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.label8, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label8.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.label8, resources.GetString("label8.HelpString"));
   this.label8.Image = ((System.Drawing.Image)(resources.GetObject("label8.Image")));
   this.label8.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label8.ImageAlign")));
   this.label8.ImageIndex = ((int)(resources.GetObject("label8.ImageIndex")));
   this.label8.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label8.ImeMode")));
   this.label8.Location = ((System.Drawing.Point)(resources.GetObject("label8.Location")));
   this.label8.Name = "label8";
   this.label8.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label8.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.label8, ((bool)(resources.GetObject("label8.ShowHelp"))));
   this.label8.Size = ((System.Drawing.Size)(resources.GetObject("label8.Size")));
   this.label8.TabIndex = ((int)(resources.GetObject("label8.TabIndex")));
   this.label8.Text = resources.GetString("label8.Text");
   this.label8.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label8.TextAlign")));
   this.toolTip1.SetToolTip(this.label8, resources.GetString("label8.ToolTip"));
   this.label8.Visible = ((bool)(resources.GetObject("label8.Visible")));
   this.groupBox3.AccessibleDescription = resources.GetString("groupBox3.AccessibleDescription");
   this.groupBox3.AccessibleName = resources.GetString("groupBox3.AccessibleName");
   this.groupBox3.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("groupBox3.Anchor")));
   this.groupBox3.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("groupBox3.BackgroundImage")));
   this.groupBox3.Controls.Add(this.gaugeChart);
   this.groupBox3.Controls.Add(this.label7);
   this.groupBox3.Controls.Add(this.limitEdit);
   this.groupBox3.Controls.Add(this.setLimit);
   this.groupBox3.Controls.Add(this.label10);
   this.groupBox3.Controls.Add(this.label3);
   this.groupBox3.Controls.Add(this.availableUnits);
   this.groupBox3.Controls.Add(this.available);
   this.groupBox3.Controls.Add(this.label9);
   this.groupBox3.Controls.Add(this.usedUnits);
   this.groupBox3.Controls.Add(this.used);
   this.groupBox3.Controls.Add(this.label1);
   this.groupBox3.Controls.Add(this.limitLabel);
   this.groupBox3.Controls.Add(this.limit);
   this.groupBox3.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("groupBox3.Dock")));
   this.groupBox3.Enabled = ((bool)(resources.GetObject("groupBox3.Enabled")));
   this.groupBox3.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.groupBox3.Font = ((System.Drawing.Font)(resources.GetObject("groupBox3.Font")));
   this.helpProvider1.SetHelpKeyword(this.groupBox3, resources.GetString("groupBox3.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.groupBox3, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("groupBox3.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.groupBox3, resources.GetString("groupBox3.HelpString"));
   this.groupBox3.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("groupBox3.ImeMode")));
   this.groupBox3.Location = ((System.Drawing.Point)(resources.GetObject("groupBox3.Location")));
   this.groupBox3.Name = "groupBox3";
   this.groupBox3.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("groupBox3.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.groupBox3, ((bool)(resources.GetObject("groupBox3.ShowHelp"))));
   this.groupBox3.Size = ((System.Drawing.Size)(resources.GetObject("groupBox3.Size")));
   this.groupBox3.TabIndex = ((int)(resources.GetObject("groupBox3.TabIndex")));
   this.groupBox3.TabStop = false;
   this.groupBox3.Text = resources.GetString("groupBox3.Text");
   this.toolTip1.SetToolTip(this.groupBox3, resources.GetString("groupBox3.ToolTip"));
   this.groupBox3.Visible = ((bool)(resources.GetObject("groupBox3.Visible")));
   this.gaugeChart.AccessibleDescription = resources.GetString("gaugeChart.AccessibleDescription");
   this.gaugeChart.AccessibleName = resources.GetString("gaugeChart.AccessibleName");
   this.gaugeChart.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("gaugeChart.Anchor")));
   this.gaugeChart.AutoScroll = ((bool)(resources.GetObject("gaugeChart.AutoScroll")));
   this.gaugeChart.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("gaugeChart.AutoScrollMargin")));
   this.gaugeChart.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("gaugeChart.AutoScrollMinSize")));
   this.gaugeChart.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("gaugeChart.BackgroundImage")));
   this.gaugeChart.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("gaugeChart.Dock")));
   this.gaugeChart.Enabled = ((bool)(resources.GetObject("gaugeChart.Enabled")));
   this.gaugeChart.Font = ((System.Drawing.Font)(resources.GetObject("gaugeChart.Font")));
   this.helpProvider1.SetHelpKeyword(this.gaugeChart, resources.GetString("gaugeChart.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.gaugeChart, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("gaugeChart.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.gaugeChart, resources.GetString("gaugeChart.HelpString"));
   this.gaugeChart.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("gaugeChart.ImeMode")));
   this.gaugeChart.Location = ((System.Drawing.Point)(resources.GetObject("gaugeChart.Location")));
   this.gaugeChart.Name = "gaugeChart";
   this.gaugeChart.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("gaugeChart.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.gaugeChart, ((bool)(resources.GetObject("gaugeChart.ShowHelp"))));
   this.gaugeChart.Size = ((System.Drawing.Size)(resources.GetObject("gaugeChart.Size")));
   this.gaugeChart.TabIndex = ((int)(resources.GetObject("gaugeChart.TabIndex")));
   this.toolTip1.SetToolTip(this.gaugeChart, resources.GetString("gaugeChart.ToolTip"));
   this.gaugeChart.Visible = ((bool)(resources.GetObject("gaugeChart.Visible")));
   this.label7.AccessibleDescription = resources.GetString("label7.AccessibleDescription");
   this.label7.AccessibleName = resources.GetString("label7.AccessibleName");
   this.label7.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label7.Anchor")));
   this.label7.AutoSize = ((bool)(resources.GetObject("label7.AutoSize")));
   this.label7.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label7.Dock")));
   this.label7.Enabled = ((bool)(resources.GetObject("label7.Enabled")));
   this.label7.Font = ((System.Drawing.Font)(resources.GetObject("label7.Font")));
   this.helpProvider1.SetHelpKeyword(this.label7, resources.GetString("label7.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.label7, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label7.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.label7, resources.GetString("label7.HelpString"));
   this.label7.Image = ((System.Drawing.Image)(resources.GetObject("label7.Image")));
   this.label7.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label7.ImageAlign")));
   this.label7.ImageIndex = ((int)(resources.GetObject("label7.ImageIndex")));
   this.label7.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label7.ImeMode")));
   this.label7.Location = ((System.Drawing.Point)(resources.GetObject("label7.Location")));
   this.label7.Name = "label7";
   this.label7.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label7.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.label7, ((bool)(resources.GetObject("label7.ShowHelp"))));
   this.label7.Size = ((System.Drawing.Size)(resources.GetObject("label7.Size")));
   this.label7.TabIndex = ((int)(resources.GetObject("label7.TabIndex")));
   this.label7.Text = resources.GetString("label7.Text");
   this.label7.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label7.TextAlign")));
   this.toolTip1.SetToolTip(this.label7, resources.GetString("label7.ToolTip"));
   this.label7.Visible = ((bool)(resources.GetObject("label7.Visible")));
   this.limitEdit.AccessibleDescription = resources.GetString("limitEdit.AccessibleDescription");
   this.limitEdit.AccessibleName = resources.GetString("limitEdit.AccessibleName");
   this.limitEdit.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("limitEdit.Anchor")));
   this.limitEdit.AutoSize = ((bool)(resources.GetObject("limitEdit.AutoSize")));
   this.limitEdit.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("limitEdit.BackgroundImage")));
   this.limitEdit.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("limitEdit.Dock")));
   this.limitEdit.Enabled = ((bool)(resources.GetObject("limitEdit.Enabled")));
   this.limitEdit.Font = ((System.Drawing.Font)(resources.GetObject("limitEdit.Font")));
   this.helpProvider1.SetHelpKeyword(this.limitEdit, resources.GetString("limitEdit.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.limitEdit, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("limitEdit.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.limitEdit, resources.GetString("limitEdit.HelpString"));
   this.limitEdit.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("limitEdit.ImeMode")));
   this.limitEdit.Location = ((System.Drawing.Point)(resources.GetObject("limitEdit.Location")));
   this.limitEdit.MaxLength = ((int)(resources.GetObject("limitEdit.MaxLength")));
   this.limitEdit.Multiline = ((bool)(resources.GetObject("limitEdit.Multiline")));
   this.limitEdit.Name = "limitEdit";
   this.limitEdit.PasswordChar = ((char)(resources.GetObject("limitEdit.PasswordChar")));
   this.limitEdit.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("limitEdit.RightToLeft")));
   this.limitEdit.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("limitEdit.ScrollBars")));
   this.helpProvider1.SetShowHelp(this.limitEdit, ((bool)(resources.GetObject("limitEdit.ShowHelp"))));
   this.limitEdit.Size = ((System.Drawing.Size)(resources.GetObject("limitEdit.Size")));
   this.limitEdit.TabIndex = ((int)(resources.GetObject("limitEdit.TabIndex")));
   this.limitEdit.Text = resources.GetString("limitEdit.Text");
   this.limitEdit.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("limitEdit.TextAlign")));
   this.toolTip1.SetToolTip(this.limitEdit, resources.GetString("limitEdit.ToolTip"));
   this.limitEdit.Visible = ((bool)(resources.GetObject("limitEdit.Visible")));
   this.limitEdit.WordWrap = ((bool)(resources.GetObject("limitEdit.WordWrap")));
   this.limitEdit.TextChanged += new System.EventHandler(this.limitEdit_TextChanged);
   this.setLimit.AccessibleDescription = resources.GetString("setLimit.AccessibleDescription");
   this.setLimit.AccessibleName = resources.GetString("setLimit.AccessibleName");
   this.setLimit.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("setLimit.Anchor")));
   this.setLimit.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("setLimit.Appearance")));
   this.setLimit.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("setLimit.BackgroundImage")));
   this.setLimit.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("setLimit.CheckAlign")));
   this.setLimit.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("setLimit.Dock")));
   this.setLimit.Enabled = ((bool)(resources.GetObject("setLimit.Enabled")));
   this.setLimit.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("setLimit.FlatStyle")));
   this.setLimit.Font = ((System.Drawing.Font)(resources.GetObject("setLimit.Font")));
   this.helpProvider1.SetHelpKeyword(this.setLimit, resources.GetString("setLimit.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.setLimit, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("setLimit.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.setLimit, resources.GetString("setLimit.HelpString"));
   this.setLimit.Image = ((System.Drawing.Image)(resources.GetObject("setLimit.Image")));
   this.setLimit.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("setLimit.ImageAlign")));
   this.setLimit.ImageIndex = ((int)(resources.GetObject("setLimit.ImageIndex")));
   this.setLimit.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("setLimit.ImeMode")));
   this.setLimit.Location = ((System.Drawing.Point)(resources.GetObject("setLimit.Location")));
   this.setLimit.Name = "setLimit";
   this.setLimit.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("setLimit.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.setLimit, ((bool)(resources.GetObject("setLimit.ShowHelp"))));
   this.setLimit.Size = ((System.Drawing.Size)(resources.GetObject("setLimit.Size")));
   this.setLimit.TabIndex = ((int)(resources.GetObject("setLimit.TabIndex")));
   this.setLimit.Text = resources.GetString("setLimit.Text");
   this.setLimit.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("setLimit.TextAlign")));
   this.toolTip1.SetToolTip(this.setLimit, resources.GetString("setLimit.ToolTip"));
   this.setLimit.Visible = ((bool)(resources.GetObject("setLimit.Visible")));
   this.setLimit.CheckedChanged += new System.EventHandler(this.setLimit_CheckedChanged);
            this.ssl.AccessibleDescription = resources.GetString("ssl.AccessibleDescription");
            this.ssl.AccessibleName = resources.GetString("ssl.AccessibleName");
            this.ssl.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("ssl.Anchor")));
            this.ssl.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("ssl.Appearance")));
            this.ssl.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("ssl.BackgroundImage")));
            this.ssl.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ssl.CheckAlign")));
            this.ssl.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("ssl.Dock")));
            this.ssl.Enabled = ((bool)(resources.GetObject("ssl.Enabled")));
            this.ssl.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("ssl.FlatStyle")));
            this.ssl.Font = ((System.Drawing.Font)(resources.GetObject("ssl.Font")));
            this.helpProvider1.SetHelpKeyword(this.setLimit, resources.GetString("ssl.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.setLimit, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("ssl.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.setLimit, resources.GetString("ssl.HelpString"));
            this.ssl.Image = ((System.Drawing.Image)(resources.GetObject("ssl.Image")));
            this.ssl.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ssl.ImageAlign")));
            this.ssl.ImageIndex = ((int)(resources.GetObject("ssl.ImageIndex")));
            this.ssl.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("ssl.ImeMode")));
            this.ssl.Location = ((System.Drawing.Point)(resources.GetObject("ssl.Location")));
            this.ssl.Name = "ssl";
            this.ssl.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("ssl.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.setLimit, ((bool)(resources.GetObject("ssl.ShowHelp"))));
            this.ssl.Size = ((System.Drawing.Size)(resources.GetObject("ssl.Size")));
            this.ssl.TabIndex = ((int)(resources.GetObject("ssl.TabIndex")));
            this.ssl.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ssl.TextAlign")));
            this.toolTip1.SetToolTip(this.setLimit, resources.GetString("ssl.ToolTip"));
            this.ssl.Visible = ((bool)(resources.GetObject("ssl.Visible")));
            this.ssl.CheckedChanged += new System.EventHandler(this.ssl_CheckedChanged);
   this.label10.AccessibleDescription = resources.GetString("label10.AccessibleDescription");
   this.label10.AccessibleName = resources.GetString("label10.AccessibleName");
   this.label10.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label10.Anchor")));
   this.label10.AutoSize = ((bool)(resources.GetObject("label10.AutoSize")));
   this.label10.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label10.Dock")));
   this.label10.Enabled = ((bool)(resources.GetObject("label10.Enabled")));
   this.label10.Font = ((System.Drawing.Font)(resources.GetObject("label10.Font")));
   this.helpProvider1.SetHelpKeyword(this.label10, resources.GetString("label10.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.label10, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label10.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.label10, resources.GetString("label10.HelpString"));
   this.label10.Image = ((System.Drawing.Image)(resources.GetObject("label10.Image")));
   this.label10.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label10.ImageAlign")));
   this.label10.ImageIndex = ((int)(resources.GetObject("label10.ImageIndex")));
   this.label10.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label10.ImeMode")));
   this.label10.Location = ((System.Drawing.Point)(resources.GetObject("label10.Location")));
   this.label10.Name = "label10";
   this.label10.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label10.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.label10, ((bool)(resources.GetObject("label10.ShowHelp"))));
   this.label10.Size = ((System.Drawing.Size)(resources.GetObject("label10.Size")));
   this.label10.TabIndex = ((int)(resources.GetObject("label10.TabIndex")));
   this.label10.Text = resources.GetString("label10.Text");
   this.label10.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label10.TextAlign")));
   this.toolTip1.SetToolTip(this.label10, resources.GetString("label10.ToolTip"));
   this.label10.Visible = ((bool)(resources.GetObject("label10.Visible")));
   this.label3.AccessibleDescription = resources.GetString("label3.AccessibleDescription");
   this.label3.AccessibleName = resources.GetString("label3.AccessibleName");
   this.label3.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label3.Anchor")));
   this.label3.AutoSize = ((bool)(resources.GetObject("label3.AutoSize")));
   this.label3.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label3.Dock")));
   this.label3.Enabled = ((bool)(resources.GetObject("label3.Enabled")));
   this.label3.Font = ((System.Drawing.Font)(resources.GetObject("label3.Font")));
   this.helpProvider1.SetHelpKeyword(this.label3, resources.GetString("label3.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.label3, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label3.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.label3, resources.GetString("label3.HelpString"));
   this.label3.Image = ((System.Drawing.Image)(resources.GetObject("label3.Image")));
   this.label3.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label3.ImageAlign")));
   this.label3.ImageIndex = ((int)(resources.GetObject("label3.ImageIndex")));
   this.label3.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label3.ImeMode")));
   this.label3.Location = ((System.Drawing.Point)(resources.GetObject("label3.Location")));
   this.label3.Name = "label3";
   this.label3.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label3.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.label3, ((bool)(resources.GetObject("label3.ShowHelp"))));
   this.label3.Size = ((System.Drawing.Size)(resources.GetObject("label3.Size")));
   this.label3.TabIndex = ((int)(resources.GetObject("label3.TabIndex")));
   this.label3.Text = resources.GetString("label3.Text");
   this.label3.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label3.TextAlign")));
   this.toolTip1.SetToolTip(this.label3, resources.GetString("label3.ToolTip"));
   this.label3.Visible = ((bool)(resources.GetObject("label3.Visible")));
   this.availableUnits.AccessibleDescription = resources.GetString("availableUnits.AccessibleDescription");
   this.availableUnits.AccessibleName = resources.GetString("availableUnits.AccessibleName");
   this.availableUnits.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("availableUnits.Anchor")));
   this.availableUnits.AutoSize = ((bool)(resources.GetObject("availableUnits.AutoSize")));
   this.availableUnits.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("availableUnits.Dock")));
   this.availableUnits.Enabled = ((bool)(resources.GetObject("availableUnits.Enabled")));
   this.availableUnits.Font = ((System.Drawing.Font)(resources.GetObject("availableUnits.Font")));
   this.helpProvider1.SetHelpKeyword(this.availableUnits, resources.GetString("availableUnits.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.availableUnits, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("availableUnits.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.availableUnits, resources.GetString("availableUnits.HelpString"));
   this.availableUnits.Image = ((System.Drawing.Image)(resources.GetObject("availableUnits.Image")));
   this.availableUnits.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("availableUnits.ImageAlign")));
   this.availableUnits.ImageIndex = ((int)(resources.GetObject("availableUnits.ImageIndex")));
   this.availableUnits.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("availableUnits.ImeMode")));
   this.availableUnits.Location = ((System.Drawing.Point)(resources.GetObject("availableUnits.Location")));
   this.availableUnits.Name = "availableUnits";
   this.availableUnits.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("availableUnits.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.availableUnits, ((bool)(resources.GetObject("availableUnits.ShowHelp"))));
   this.availableUnits.Size = ((System.Drawing.Size)(resources.GetObject("availableUnits.Size")));
   this.availableUnits.TabIndex = ((int)(resources.GetObject("availableUnits.TabIndex")));
   this.availableUnits.Text = resources.GetString("availableUnits.Text");
   this.availableUnits.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("availableUnits.TextAlign")));
   this.toolTip1.SetToolTip(this.availableUnits, resources.GetString("availableUnits.ToolTip"));
   this.availableUnits.Visible = ((bool)(resources.GetObject("availableUnits.Visible")));
   this.available.AccessibleDescription = resources.GetString("available.AccessibleDescription");
   this.available.AccessibleName = resources.GetString("available.AccessibleName");
   this.available.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("available.Anchor")));
   this.available.AutoSize = ((bool)(resources.GetObject("available.AutoSize")));
   this.available.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("available.Dock")));
   this.available.Enabled = ((bool)(resources.GetObject("available.Enabled")));
   this.available.Font = ((System.Drawing.Font)(resources.GetObject("available.Font")));
   this.helpProvider1.SetHelpKeyword(this.available, resources.GetString("available.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.available, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("available.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.available, resources.GetString("available.HelpString"));
   this.available.Image = ((System.Drawing.Image)(resources.GetObject("available.Image")));
   this.available.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("available.ImageAlign")));
   this.available.ImageIndex = ((int)(resources.GetObject("available.ImageIndex")));
   this.available.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("available.ImeMode")));
   this.available.Location = ((System.Drawing.Point)(resources.GetObject("available.Location")));
   this.available.Name = "available";
   this.available.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("available.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.available, ((bool)(resources.GetObject("available.ShowHelp"))));
   this.available.Size = ((System.Drawing.Size)(resources.GetObject("available.Size")));
   this.available.TabIndex = ((int)(resources.GetObject("available.TabIndex")));
   this.available.Text = resources.GetString("available.Text");
   this.available.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("available.TextAlign")));
   this.toolTip1.SetToolTip(this.available, resources.GetString("available.ToolTip"));
   this.available.Visible = ((bool)(resources.GetObject("available.Visible")));
   this.label9.AccessibleDescription = resources.GetString("label9.AccessibleDescription");
   this.label9.AccessibleName = resources.GetString("label9.AccessibleName");
   this.label9.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label9.Anchor")));
   this.label9.AutoSize = ((bool)(resources.GetObject("label9.AutoSize")));
   this.label9.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label9.Dock")));
   this.label9.Enabled = ((bool)(resources.GetObject("label9.Enabled")));
   this.label9.Font = ((System.Drawing.Font)(resources.GetObject("label9.Font")));
   this.helpProvider1.SetHelpKeyword(this.label9, resources.GetString("label9.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.label9, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label9.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.label9, resources.GetString("label9.HelpString"));
   this.label9.Image = ((System.Drawing.Image)(resources.GetObject("label9.Image")));
   this.label9.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label9.ImageAlign")));
   this.label9.ImageIndex = ((int)(resources.GetObject("label9.ImageIndex")));
   this.label9.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label9.ImeMode")));
   this.label9.Location = ((System.Drawing.Point)(resources.GetObject("label9.Location")));
   this.label9.Name = "label9";
   this.label9.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label9.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.label9, ((bool)(resources.GetObject("label9.ShowHelp"))));
   this.label9.Size = ((System.Drawing.Size)(resources.GetObject("label9.Size")));
   this.label9.TabIndex = ((int)(resources.GetObject("label9.TabIndex")));
   this.label9.Text = resources.GetString("label9.Text");
   this.label9.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label9.TextAlign")));
   this.toolTip1.SetToolTip(this.label9, resources.GetString("label9.ToolTip"));
   this.label9.Visible = ((bool)(resources.GetObject("label9.Visible")));
   this.usedUnits.AccessibleDescription = resources.GetString("usedUnits.AccessibleDescription");
   this.usedUnits.AccessibleName = resources.GetString("usedUnits.AccessibleName");
   this.usedUnits.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("usedUnits.Anchor")));
   this.usedUnits.AutoSize = ((bool)(resources.GetObject("usedUnits.AutoSize")));
   this.usedUnits.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("usedUnits.Dock")));
   this.usedUnits.Enabled = ((bool)(resources.GetObject("usedUnits.Enabled")));
   this.usedUnits.Font = ((System.Drawing.Font)(resources.GetObject("usedUnits.Font")));
   this.helpProvider1.SetHelpKeyword(this.usedUnits, resources.GetString("usedUnits.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.usedUnits, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("usedUnits.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.usedUnits, resources.GetString("usedUnits.HelpString"));
   this.usedUnits.Image = ((System.Drawing.Image)(resources.GetObject("usedUnits.Image")));
   this.usedUnits.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("usedUnits.ImageAlign")));
   this.usedUnits.ImageIndex = ((int)(resources.GetObject("usedUnits.ImageIndex")));
   this.usedUnits.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("usedUnits.ImeMode")));
   this.usedUnits.Location = ((System.Drawing.Point)(resources.GetObject("usedUnits.Location")));
   this.usedUnits.Name = "usedUnits";
   this.usedUnits.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("usedUnits.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.usedUnits, ((bool)(resources.GetObject("usedUnits.ShowHelp"))));
   this.usedUnits.Size = ((System.Drawing.Size)(resources.GetObject("usedUnits.Size")));
   this.usedUnits.TabIndex = ((int)(resources.GetObject("usedUnits.TabIndex")));
   this.usedUnits.Text = resources.GetString("usedUnits.Text");
   this.usedUnits.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("usedUnits.TextAlign")));
   this.toolTip1.SetToolTip(this.usedUnits, resources.GetString("usedUnits.ToolTip"));
   this.usedUnits.Visible = ((bool)(resources.GetObject("usedUnits.Visible")));
   this.used.AccessibleDescription = resources.GetString("used.AccessibleDescription");
   this.used.AccessibleName = resources.GetString("used.AccessibleName");
   this.used.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("used.Anchor")));
   this.used.AutoSize = ((bool)(resources.GetObject("used.AutoSize")));
   this.used.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("used.Dock")));
   this.used.Enabled = ((bool)(resources.GetObject("used.Enabled")));
   this.used.Font = ((System.Drawing.Font)(resources.GetObject("used.Font")));
   this.helpProvider1.SetHelpKeyword(this.used, resources.GetString("used.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.used, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("used.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.used, resources.GetString("used.HelpString"));
   this.used.Image = ((System.Drawing.Image)(resources.GetObject("used.Image")));
   this.used.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("used.ImageAlign")));
   this.used.ImageIndex = ((int)(resources.GetObject("used.ImageIndex")));
   this.used.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("used.ImeMode")));
   this.used.Location = ((System.Drawing.Point)(resources.GetObject("used.Location")));
   this.used.Name = "used";
   this.used.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("used.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.used, ((bool)(resources.GetObject("used.ShowHelp"))));
   this.used.Size = ((System.Drawing.Size)(resources.GetObject("used.Size")));
   this.used.TabIndex = ((int)(resources.GetObject("used.TabIndex")));
   this.used.Text = resources.GetString("used.Text");
   this.used.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("used.TextAlign")));
   this.toolTip1.SetToolTip(this.used, resources.GetString("used.ToolTip"));
   this.used.Visible = ((bool)(resources.GetObject("used.Visible")));
   this.label1.AccessibleDescription = resources.GetString("label1.AccessibleDescription");
   this.label1.AccessibleName = resources.GetString("label1.AccessibleName");
   this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label1.Anchor")));
   this.label1.AutoSize = ((bool)(resources.GetObject("label1.AutoSize")));
   this.label1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label1.Dock")));
   this.label1.Enabled = ((bool)(resources.GetObject("label1.Enabled")));
   this.label1.Font = ((System.Drawing.Font)(resources.GetObject("label1.Font")));
   this.helpProvider1.SetHelpKeyword(this.label1, resources.GetString("label1.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.label1, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label1.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.label1, resources.GetString("label1.HelpString"));
   this.label1.Image = ((System.Drawing.Image)(resources.GetObject("label1.Image")));
   this.label1.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label1.ImageAlign")));
   this.label1.ImageIndex = ((int)(resources.GetObject("label1.ImageIndex")));
   this.label1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label1.ImeMode")));
   this.label1.Location = ((System.Drawing.Point)(resources.GetObject("label1.Location")));
   this.label1.Name = "label1";
   this.label1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label1.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.label1, ((bool)(resources.GetObject("label1.ShowHelp"))));
   this.label1.Size = ((System.Drawing.Size)(resources.GetObject("label1.Size")));
   this.label1.TabIndex = ((int)(resources.GetObject("label1.TabIndex")));
   this.label1.Text = resources.GetString("label1.Text");
   this.label1.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label1.TextAlign")));
   this.toolTip1.SetToolTip(this.label1, resources.GetString("label1.ToolTip"));
   this.label1.Visible = ((bool)(resources.GetObject("label1.Visible")));
   this.limitLabel.AccessibleDescription = resources.GetString("limitLabel.AccessibleDescription");
   this.limitLabel.AccessibleName = resources.GetString("limitLabel.AccessibleName");
   this.limitLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("limitLabel.Anchor")));
   this.limitLabel.AutoSize = ((bool)(resources.GetObject("limitLabel.AutoSize")));
   this.limitLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("limitLabel.Dock")));
   this.limitLabel.Enabled = ((bool)(resources.GetObject("limitLabel.Enabled")));
   this.limitLabel.Font = ((System.Drawing.Font)(resources.GetObject("limitLabel.Font")));
   this.helpProvider1.SetHelpKeyword(this.limitLabel, resources.GetString("limitLabel.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.limitLabel, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("limitLabel.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.limitLabel, resources.GetString("limitLabel.HelpString"));
   this.limitLabel.Image = ((System.Drawing.Image)(resources.GetObject("limitLabel.Image")));
   this.limitLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("limitLabel.ImageAlign")));
   this.limitLabel.ImageIndex = ((int)(resources.GetObject("limitLabel.ImageIndex")));
   this.limitLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("limitLabel.ImeMode")));
   this.limitLabel.Location = ((System.Drawing.Point)(resources.GetObject("limitLabel.Location")));
   this.limitLabel.Name = "limitLabel";
   this.limitLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("limitLabel.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.limitLabel, ((bool)(resources.GetObject("limitLabel.ShowHelp"))));
   this.limitLabel.Size = ((System.Drawing.Size)(resources.GetObject("limitLabel.Size")));
   this.limitLabel.TabIndex = ((int)(resources.GetObject("limitLabel.TabIndex")));
   this.limitLabel.Text = resources.GetString("limitLabel.Text");
   this.limitLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("limitLabel.TextAlign")));
   this.toolTip1.SetToolTip(this.limitLabel, resources.GetString("limitLabel.ToolTip"));
   this.limitLabel.Visible = ((bool)(resources.GetObject("limitLabel.Visible")));
   this.limit.AccessibleDescription = resources.GetString("limit.AccessibleDescription");
   this.limit.AccessibleName = resources.GetString("limit.AccessibleName");
   this.limit.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("limit.Anchor")));
   this.limit.AutoSize = ((bool)(resources.GetObject("limit.AutoSize")));
   this.limit.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("limit.Dock")));
   this.limit.Enabled = ((bool)(resources.GetObject("limit.Enabled")));
   this.limit.Font = ((System.Drawing.Font)(resources.GetObject("limit.Font")));
   this.helpProvider1.SetHelpKeyword(this.limit, resources.GetString("limit.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.limit, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("limit.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.limit, resources.GetString("limit.HelpString"));
   this.limit.Image = ((System.Drawing.Image)(resources.GetObject("limit.Image")));
   this.limit.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("limit.ImageAlign")));
   this.limit.ImageIndex = ((int)(resources.GetObject("limit.ImageIndex")));
   this.limit.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("limit.ImeMode")));
   this.limit.Location = ((System.Drawing.Point)(resources.GetObject("limit.Location")));
   this.limit.Name = "limit";
   this.limit.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("limit.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.limit, ((bool)(resources.GetObject("limit.ShowHelp"))));
   this.limit.Size = ((System.Drawing.Size)(resources.GetObject("limit.Size")));
   this.limit.TabIndex = ((int)(resources.GetObject("limit.TabIndex")));
   this.limit.Text = resources.GetString("limit.Text");
   this.limit.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("limit.TextAlign")));
   this.toolTip1.SetToolTip(this.limit, resources.GetString("limit.ToolTip"));
   this.limit.Visible = ((bool)(resources.GetObject("limit.Visible")));
   this.tabSharing.AccessibleDescription = resources.GetString("tabSharing.AccessibleDescription");
   this.tabSharing.AccessibleName = resources.GetString("tabSharing.AccessibleName");
   this.tabSharing.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("tabSharing.Anchor")));
   this.tabSharing.AutoScroll = ((bool)(resources.GetObject("tabSharing.AutoScroll")));
   this.tabSharing.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("tabSharing.AutoScrollMargin")));
   this.tabSharing.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("tabSharing.AutoScrollMinSize")));
   this.tabSharing.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("tabSharing.BackgroundImage")));
   this.tabSharing.Controls.Add(this.access);
   this.tabSharing.Controls.Add(this.add);
   this.tabSharing.Controls.Add(this.remove);
   this.tabSharing.Controls.Add(this.shareWith);
   this.tabSharing.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("tabSharing.Dock")));
   this.tabSharing.Enabled = ((bool)(resources.GetObject("tabSharing.Enabled")));
   this.tabSharing.Font = ((System.Drawing.Font)(resources.GetObject("tabSharing.Font")));
   this.helpProvider1.SetHelpKeyword(this.tabSharing, resources.GetString("tabSharing.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.tabSharing, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("tabSharing.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.tabSharing, resources.GetString("tabSharing.HelpString"));
   this.tabSharing.ImageIndex = ((int)(resources.GetObject("tabSharing.ImageIndex")));
   this.tabSharing.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("tabSharing.ImeMode")));
   this.tabSharing.Location = ((System.Drawing.Point)(resources.GetObject("tabSharing.Location")));
   this.tabSharing.Name = "tabSharing";
   this.tabSharing.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("tabSharing.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.tabSharing, ((bool)(resources.GetObject("tabSharing.ShowHelp"))));
   this.tabSharing.Size = ((System.Drawing.Size)(resources.GetObject("tabSharing.Size")));
   this.tabSharing.TabIndex = ((int)(resources.GetObject("tabSharing.TabIndex")));
   this.tabSharing.Text = resources.GetString("tabSharing.Text");
   this.toolTip1.SetToolTip(this.tabSharing, resources.GetString("tabSharing.ToolTip"));
   this.tabSharing.ToolTipText = resources.GetString("tabSharing.ToolTipText");
   this.tabSharing.Visible = ((bool)(resources.GetObject("tabSharing.Visible")));
   this.access.AccessibleDescription = resources.GetString("access.AccessibleDescription");
   this.access.AccessibleName = resources.GetString("access.AccessibleName");
   this.access.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("access.Anchor")));
   this.access.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("access.BackgroundImage")));
   this.access.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("access.Dock")));
   this.access.Enabled = ((bool)(resources.GetObject("access.Enabled")));
   this.access.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("access.FlatStyle")));
   this.access.Font = ((System.Drawing.Font)(resources.GetObject("access.Font")));
   this.helpProvider1.SetHelpKeyword(this.access, resources.GetString("access.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.access, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("access.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.access, resources.GetString("access.HelpString"));
   this.access.Image = ((System.Drawing.Image)(resources.GetObject("access.Image")));
   this.access.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("access.ImageAlign")));
   this.access.ImageIndex = ((int)(resources.GetObject("access.ImageIndex")));
   this.access.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("access.ImeMode")));
   this.access.Location = ((System.Drawing.Point)(resources.GetObject("access.Location")));
   this.access.Name = "access";
   this.access.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("access.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.access, ((bool)(resources.GetObject("access.ShowHelp"))));
   this.access.Size = ((System.Drawing.Size)(resources.GetObject("access.Size")));
   this.access.TabIndex = ((int)(resources.GetObject("access.TabIndex")));
   this.access.Text = resources.GetString("access.Text");
   this.access.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("access.TextAlign")));
   this.toolTip1.SetToolTip(this.access, resources.GetString("access.ToolTip"));
   this.access.Visible = ((bool)(resources.GetObject("access.Visible")));
   this.access.Click += new System.EventHandler(this.access_Click);
   this.add.AccessibleDescription = resources.GetString("add.AccessibleDescription");
   this.add.AccessibleName = resources.GetString("add.AccessibleName");
   this.add.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("add.Anchor")));
   this.add.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("add.BackgroundImage")));
   this.add.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("add.Dock")));
   this.add.Enabled = ((bool)(resources.GetObject("add.Enabled")));
   this.add.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("add.FlatStyle")));
   this.add.Font = ((System.Drawing.Font)(resources.GetObject("add.Font")));
   this.helpProvider1.SetHelpKeyword(this.add, resources.GetString("add.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.add, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("add.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.add, resources.GetString("add.HelpString"));
   this.add.Image = ((System.Drawing.Image)(resources.GetObject("add.Image")));
   this.add.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("add.ImageAlign")));
   this.add.ImageIndex = ((int)(resources.GetObject("add.ImageIndex")));
   this.add.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("add.ImeMode")));
   this.add.Location = ((System.Drawing.Point)(resources.GetObject("add.Location")));
   this.add.Name = "add";
   this.add.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("add.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.add, ((bool)(resources.GetObject("add.ShowHelp"))));
   this.add.Size = ((System.Drawing.Size)(resources.GetObject("add.Size")));
   this.add.TabIndex = ((int)(resources.GetObject("add.TabIndex")));
   this.add.Text = resources.GetString("add.Text");
   this.add.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("add.TextAlign")));
   this.toolTip1.SetToolTip(this.add, resources.GetString("add.ToolTip"));
   this.add.Visible = ((bool)(resources.GetObject("add.Visible")));
   this.add.Click += new System.EventHandler(this.add_Click);
   this.remove.AccessibleDescription = resources.GetString("remove.AccessibleDescription");
   this.remove.AccessibleName = resources.GetString("remove.AccessibleName");
   this.remove.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("remove.Anchor")));
   this.remove.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("remove.BackgroundImage")));
   this.remove.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("remove.Dock")));
   this.remove.Enabled = ((bool)(resources.GetObject("remove.Enabled")));
   this.remove.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("remove.FlatStyle")));
   this.remove.Font = ((System.Drawing.Font)(resources.GetObject("remove.Font")));
   this.helpProvider1.SetHelpKeyword(this.remove, resources.GetString("remove.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.remove, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("remove.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.remove, resources.GetString("remove.HelpString"));
   this.remove.Image = ((System.Drawing.Image)(resources.GetObject("remove.Image")));
   this.remove.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("remove.ImageAlign")));
   this.remove.ImageIndex = ((int)(resources.GetObject("remove.ImageIndex")));
   this.remove.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("remove.ImeMode")));
   this.remove.Location = ((System.Drawing.Point)(resources.GetObject("remove.Location")));
   this.remove.Name = "remove";
   this.remove.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("remove.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.remove, ((bool)(resources.GetObject("remove.ShowHelp"))));
   this.remove.Size = ((System.Drawing.Size)(resources.GetObject("remove.Size")));
   this.remove.TabIndex = ((int)(resources.GetObject("remove.TabIndex")));
   this.remove.Text = resources.GetString("remove.Text");
   this.remove.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("remove.TextAlign")));
   this.toolTip1.SetToolTip(this.remove, resources.GetString("remove.ToolTip"));
   this.remove.Visible = ((bool)(resources.GetObject("remove.Visible")));
   this.remove.Click += new System.EventHandler(this.remove_Click);
   this.shareWith.AccessibleDescription = resources.GetString("shareWith.AccessibleDescription");
   this.shareWith.AccessibleName = resources.GetString("shareWith.AccessibleName");
   this.shareWith.Alignment = ((System.Windows.Forms.ListViewAlignment)(resources.GetObject("shareWith.Alignment")));
   this.shareWith.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("shareWith.Anchor")));
   this.shareWith.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("shareWith.BackgroundImage")));
   this.shareWith.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
                      this.columnHeader1,
                      this.columnHeader2,
                      this.columnHeader3});
   this.shareWith.ContextMenu = this.contextMenu1;
   this.shareWith.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("shareWith.Dock")));
   this.shareWith.Enabled = ((bool)(resources.GetObject("shareWith.Enabled")));
   this.shareWith.Font = ((System.Drawing.Font)(resources.GetObject("shareWith.Font")));
   this.shareWith.FullRowSelect = true;
   this.helpProvider1.SetHelpKeyword(this.shareWith, resources.GetString("shareWith.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.shareWith, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("shareWith.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.shareWith, resources.GetString("shareWith.HelpString"));
   this.shareWith.HideSelection = false;
   this.shareWith.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("shareWith.ImeMode")));
   this.shareWith.LabelWrap = ((bool)(resources.GetObject("shareWith.LabelWrap")));
   this.shareWith.Location = ((System.Drawing.Point)(resources.GetObject("shareWith.Location")));
   this.shareWith.Name = "shareWith";
   this.shareWith.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("shareWith.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.shareWith, ((bool)(resources.GetObject("shareWith.ShowHelp"))));
   this.shareWith.Size = ((System.Drawing.Size)(resources.GetObject("shareWith.Size")));
   this.shareWith.TabIndex = ((int)(resources.GetObject("shareWith.TabIndex")));
   this.shareWith.Text = resources.GetString("shareWith.Text");
   this.toolTip1.SetToolTip(this.shareWith, resources.GetString("shareWith.ToolTip"));
   this.shareWith.View = System.Windows.Forms.View.Details;
   this.shareWith.Visible = ((bool)(resources.GetObject("shareWith.Visible")));
   this.shareWith.KeyDown += new System.Windows.Forms.KeyEventHandler(this.shareWith_KeyDown);
   this.shareWith.MouseDown += new System.Windows.Forms.MouseEventHandler(this.shareWith_MouseDown);
   this.shareWith.SelectedIndexChanged += new System.EventHandler(this.shareWith_SelectedIndexChanged);
   this.columnHeader1.Text = resources.GetString("columnHeader1.Text");
   this.columnHeader1.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader1.TextAlign")));
   this.columnHeader1.Width = ((int)(resources.GetObject("columnHeader1.Width")));
   this.columnHeader2.Text = resources.GetString("columnHeader2.Text");
   this.columnHeader2.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader2.TextAlign")));
   this.columnHeader2.Width = ((int)(resources.GetObject("columnHeader2.Width")));
   this.columnHeader3.Text = resources.GetString("columnHeader3.Text");
   this.columnHeader3.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader3.TextAlign")));
   this.columnHeader3.Width = ((int)(resources.GetObject("columnHeader3.Width")));
   this.contextMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                       this.menuFullControl,
                       this.menuReadWrite,
                       this.menuReadOnly});
   this.contextMenu1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("contextMenu1.RightToLeft")));
   this.contextMenu1.Popup += new System.EventHandler(this.contextMenu1_Popup);
   this.menuFullControl.Enabled = ((bool)(resources.GetObject("menuFullControl.Enabled")));
   this.menuFullControl.Index = 0;
   this.menuFullControl.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuFullControl.Shortcut")));
   this.menuFullControl.ShowShortcut = ((bool)(resources.GetObject("menuFullControl.ShowShortcut")));
   this.menuFullControl.Text = resources.GetString("menuFullControl.Text");
   this.menuFullControl.Visible = ((bool)(resources.GetObject("menuFullControl.Visible")));
   this.menuFullControl.Click += new System.EventHandler(this.menuFullControl_Click);
   this.menuReadWrite.Enabled = ((bool)(resources.GetObject("menuReadWrite.Enabled")));
   this.menuReadWrite.Index = 1;
   this.menuReadWrite.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuReadWrite.Shortcut")));
   this.menuReadWrite.ShowShortcut = ((bool)(resources.GetObject("menuReadWrite.ShowShortcut")));
   this.menuReadWrite.Text = resources.GetString("menuReadWrite.Text");
   this.menuReadWrite.Visible = ((bool)(resources.GetObject("menuReadWrite.Visible")));
   this.menuReadWrite.Click += new System.EventHandler(this.menuReadWrite_Click);
   this.menuReadOnly.Enabled = ((bool)(resources.GetObject("menuReadOnly.Enabled")));
   this.menuReadOnly.Index = 2;
   this.menuReadOnly.Shortcut = ((System.Windows.Forms.Shortcut)(resources.GetObject("menuReadOnly.Shortcut")));
   this.menuReadOnly.ShowShortcut = ((bool)(resources.GetObject("menuReadOnly.ShowShortcut")));
   this.menuReadOnly.Text = resources.GetString("menuReadOnly.Text");
   this.menuReadOnly.Visible = ((bool)(resources.GetObject("menuReadOnly.Visible")));
   this.menuReadOnly.Click += new System.EventHandler(this.menuReadOnly_Click);
   this.ok.AccessibleDescription = resources.GetString("ok.AccessibleDescription");
   this.ok.AccessibleName = resources.GetString("ok.AccessibleName");
   this.ok.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("ok.Anchor")));
   this.ok.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("ok.BackgroundImage")));
   this.ok.DialogResult = System.Windows.Forms.DialogResult.OK;
   this.ok.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("ok.Dock")));
   this.ok.Enabled = ((bool)(resources.GetObject("ok.Enabled")));
   this.ok.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("ok.FlatStyle")));
   this.ok.Font = ((System.Drawing.Font)(resources.GetObject("ok.Font")));
   this.helpProvider1.SetHelpKeyword(this.ok, resources.GetString("ok.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.ok, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("ok.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.ok, resources.GetString("ok.HelpString"));
   this.ok.Image = ((System.Drawing.Image)(resources.GetObject("ok.Image")));
   this.ok.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ok.ImageAlign")));
   this.ok.ImageIndex = ((int)(resources.GetObject("ok.ImageIndex")));
   this.ok.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("ok.ImeMode")));
   this.ok.Location = ((System.Drawing.Point)(resources.GetObject("ok.Location")));
   this.ok.Name = "ok";
   this.ok.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("ok.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.ok, ((bool)(resources.GetObject("ok.ShowHelp"))));
   this.ok.Size = ((System.Drawing.Size)(resources.GetObject("ok.Size")));
   this.ok.TabIndex = ((int)(resources.GetObject("ok.TabIndex")));
   this.ok.Text = resources.GetString("ok.Text");
   this.ok.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ok.TextAlign")));
   this.toolTip1.SetToolTip(this.ok, resources.GetString("ok.ToolTip"));
   this.ok.Visible = ((bool)(resources.GetObject("ok.Visible")));
   this.ok.Click += new System.EventHandler(this.ok_Click);
   this.cancel.AccessibleDescription = resources.GetString("cancel.AccessibleDescription");
   this.cancel.AccessibleName = resources.GetString("cancel.AccessibleName");
   this.cancel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("cancel.Anchor")));
   this.cancel.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("cancel.BackgroundImage")));
   this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("cancel.Dock")));
   this.cancel.Enabled = ((bool)(resources.GetObject("cancel.Enabled")));
   this.cancel.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("cancel.FlatStyle")));
   this.cancel.Font = ((System.Drawing.Font)(resources.GetObject("cancel.Font")));
   this.helpProvider1.SetHelpKeyword(this.cancel, resources.GetString("cancel.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.cancel, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("cancel.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.cancel, resources.GetString("cancel.HelpString"));
   this.cancel.Image = ((System.Drawing.Image)(resources.GetObject("cancel.Image")));
   this.cancel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("cancel.ImageAlign")));
   this.cancel.ImageIndex = ((int)(resources.GetObject("cancel.ImageIndex")));
   this.cancel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("cancel.ImeMode")));
   this.cancel.Location = ((System.Drawing.Point)(resources.GetObject("cancel.Location")));
   this.cancel.Name = "cancel";
   this.cancel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("cancel.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.cancel, ((bool)(resources.GetObject("cancel.ShowHelp"))));
   this.cancel.Size = ((System.Drawing.Size)(resources.GetObject("cancel.Size")));
   this.cancel.TabIndex = ((int)(resources.GetObject("cancel.TabIndex")));
   this.cancel.Text = resources.GetString("cancel.Text");
   this.cancel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("cancel.TextAlign")));
   this.toolTip1.SetToolTip(this.cancel, resources.GetString("cancel.ToolTip"));
   this.cancel.Visible = ((bool)(resources.GetObject("cancel.Visible")));
   this.cancel.Click += new System.EventHandler(this.cancel_Click);
   this.apply.AccessibleDescription = resources.GetString("apply.AccessibleDescription");
   this.apply.AccessibleName = resources.GetString("apply.AccessibleName");
   this.apply.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("apply.Anchor")));
   this.apply.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("apply.BackgroundImage")));
   this.apply.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("apply.Dock")));
   this.apply.Enabled = ((bool)(resources.GetObject("apply.Enabled")));
   this.apply.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("apply.FlatStyle")));
   this.apply.Font = ((System.Drawing.Font)(resources.GetObject("apply.Font")));
   this.helpProvider1.SetHelpKeyword(this.apply, resources.GetString("apply.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.apply, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("apply.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.apply, resources.GetString("apply.HelpString"));
   this.apply.Image = ((System.Drawing.Image)(resources.GetObject("apply.Image")));
   this.apply.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("apply.ImageAlign")));
   this.apply.ImageIndex = ((int)(resources.GetObject("apply.ImageIndex")));
   this.apply.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("apply.ImeMode")));
   this.apply.Location = ((System.Drawing.Point)(resources.GetObject("apply.Location")));
   this.apply.Name = "apply";
   this.apply.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("apply.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.apply, ((bool)(resources.GetObject("apply.ShowHelp"))));
   this.apply.Size = ((System.Drawing.Size)(resources.GetObject("apply.Size")));
   this.apply.TabIndex = ((int)(resources.GetObject("apply.TabIndex")));
   this.apply.Text = resources.GetString("apply.Text");
   this.apply.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("apply.TextAlign")));
   this.toolTip1.SetToolTip(this.apply, resources.GetString("apply.ToolTip"));
   this.apply.Visible = ((bool)(resources.GetObject("apply.Visible")));
   this.apply.Click += new System.EventHandler(this.apply_Click);
   this.helpProvider1.HelpNamespace = resources.GetString("helpProvider1.HelpNamespace");
   this.groupBox2.AccessibleDescription = resources.GetString("groupBox2.AccessibleDescription");
   this.groupBox2.AccessibleName = resources.GetString("groupBox2.AccessibleName");
   this.groupBox2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("groupBox2.Anchor")));
   this.groupBox2.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("groupBox2.BackgroundImage")));
   this.groupBox2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("groupBox2.Dock")));
   this.groupBox2.Enabled = ((bool)(resources.GetObject("groupBox2.Enabled")));
   this.groupBox2.Font = ((System.Drawing.Font)(resources.GetObject("groupBox2.Font")));
   this.helpProvider1.SetHelpKeyword(this.groupBox2, resources.GetString("groupBox2.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.groupBox2, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("groupBox2.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.groupBox2, resources.GetString("groupBox2.HelpString"));
   this.groupBox2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("groupBox2.ImeMode")));
   this.groupBox2.Location = ((System.Drawing.Point)(resources.GetObject("groupBox2.Location")));
   this.groupBox2.Name = "groupBox2";
   this.groupBox2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("groupBox2.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.groupBox2, ((bool)(resources.GetObject("groupBox2.ShowHelp"))));
   this.groupBox2.Size = ((System.Drawing.Size)(resources.GetObject("groupBox2.Size")));
   this.groupBox2.TabIndex = ((int)(resources.GetObject("groupBox2.TabIndex")));
   this.groupBox2.TabStop = false;
   this.groupBox2.Text = resources.GetString("groupBox2.Text");
   this.toolTip1.SetToolTip(this.groupBox2, resources.GetString("groupBox2.ToolTip"));
   this.groupBox2.Visible = ((bool)(resources.GetObject("groupBox2.Visible")));
   this.groupBox2.Controls.Add(this.lblAccount);
   this.groupBox2.Controls.Add(this.lblLocation);
   this.groupBox2.Controls.Add(this.lblOwner);
   this.groupBox2.Controls.Add(this.iFolderAccount);
   this.groupBox2.Controls.Add(this.iFolderLocation);
   this.groupBox2.Controls.Add(this.iFolderOwner);
            this.groupBox3.ForeColor = this.groupBox2.ForeColor = this.groupBox3.ForeColor;
   this.pictureBox1.AccessibleDescription = resources.GetString("pictureBox1.AccessibleDescription");
   this.pictureBox1.AccessibleName = resources.GetString("pictureBox1.AccessibleName");
   this.pictureBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("pictureBox1.Anchor")));
   this.pictureBox1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("pictureBox1.BackgroundImage")));
   this.pictureBox1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("pictureBox1.Dock")));
   this.pictureBox1.Enabled = ((bool)(resources.GetObject("pictureBox1.Enabled")));
   this.pictureBox1.Font = ((System.Drawing.Font)(resources.GetObject("pictureBox1.Font")));
   this.helpProvider1.SetHelpKeyword(this.pictureBox1, resources.GetString("pictureBox1.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.pictureBox1, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("pictureBox1.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.pictureBox1, resources.GetString("pictureBox1.HelpString"));
   this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
   this.pictureBox1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("pictureBox1.ImeMode")));
   this.pictureBox1.Location = ((System.Drawing.Point)(resources.GetObject("pictureBox1.Location")));
   this.pictureBox1.Name = "pictureBox1";
   this.pictureBox1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("pictureBox1.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.pictureBox1, ((bool)(resources.GetObject("pictureBox1.ShowHelp"))));
   this.pictureBox1.Size = ((System.Drawing.Size)(resources.GetObject("pictureBox1.Size")));
   this.pictureBox1.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("pictureBox1.SizeMode")));
   this.pictureBox1.TabIndex = ((int)(resources.GetObject("pictureBox1.TabIndex")));
   this.pictureBox1.TabStop = false;
   this.pictureBox1.Text = resources.GetString("pictureBox1.Text");
   this.toolTip1.SetToolTip(this.pictureBox1, resources.GetString("pictureBox1.ToolTip"));
   this.pictureBox1.Visible = ((bool)(resources.GetObject("pictureBox1.Visible")));
   this.groupBox2.Controls.Add(this.pictureBox1);
   this.iFolderName.AccessibleDescription = resources.GetString("iFolderName.AccessibleDescription");
   this.iFolderName.AccessibleName = resources.GetString("iFolderName.AccessibleName");
   this.iFolderName.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("iFolderName.Anchor")));
   this.iFolderName.AutoSize = ((bool)(resources.GetObject("iFolderName.AutoSize")));
   this.iFolderName.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("iFolderName.Dock")));
   this.iFolderName.Enabled = ((bool)(resources.GetObject("iFolderName.Enabled")));
   this.iFolderName.Font = ((System.Drawing.Font)(resources.GetObject("iFolderName.Font")));
   this.helpProvider1.SetHelpKeyword(this.iFolderName, resources.GetString("iFolderName.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.iFolderName, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("iFolderName.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.iFolderName, resources.GetString("iFolderName.HelpString"));
   this.iFolderName.Image = ((System.Drawing.Image)(resources.GetObject("iFolderName.Image")));
   this.iFolderName.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("iFolderName.ImageAlign")));
   this.iFolderName.ImageIndex = ((int)(resources.GetObject("iFolderName.ImageIndex")));
   this.iFolderName.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("iFolderName.ImeMode")));
   this.iFolderName.Location = ((System.Drawing.Point)(resources.GetObject("iFolderName.Location")));
   this.iFolderName.Name = "iFolderName";
   this.iFolderName.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("iFolderName.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.iFolderName, ((bool)(resources.GetObject("iFolderName.ShowHelp"))));
   this.iFolderName.Size = ((System.Drawing.Size)(resources.GetObject("iFolderName.Size")));
   this.iFolderName.TabIndex = ((int)(resources.GetObject("iFolderName.TabIndex")));
   this.iFolderName.Text = resources.GetString("iFolderName.Text");
   this.iFolderName.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("iFolderName.TextAlign")));
   this.toolTip1.SetToolTip(this.iFolderName, resources.GetString("iFolderName.ToolTip"));
   this.iFolderName.Visible = ((bool)(resources.GetObject("iFolderName.Visible")));
   this.groupBox2.Controls.Add(this.iFolderName);
   this.lblOwner.AccessibleDescription = resources.GetString("lblOwner.AccessibleDescription");
   this.lblOwner.AccessibleName = resources.GetString("lblOwner.AccessibleName");
   this.lblOwner.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("lblOwner.Anchor")));
   this.lblOwner.AutoSize = ((bool)(resources.GetObject("lblOwner.AutoSize")));
   this.lblOwner.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("lblOwner.Dock")));
   this.lblOwner.Enabled = ((bool)(resources.GetObject("lblOwner.Enabled")));
   this.lblOwner.Font = ((System.Drawing.Font)(resources.GetObject("lblOwner.Font")));
   this.helpProvider1.SetHelpKeyword(this.lblOwner, resources.GetString("lblOwner.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.lblOwner, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("lblOwner.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.lblOwner, resources.GetString("lblOwner.HelpString"));
   this.lblOwner.Image = ((System.Drawing.Image)(resources.GetObject("lblOwner.Image")));
   this.lblOwner.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("lblOwner.ImageAlign")));
   this.lblOwner.ImageIndex = ((int)(resources.GetObject("lblOwner.ImageIndex")));
   this.lblOwner.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("lblOwner.ImeMode")));
   this.lblOwner.Location = ((System.Drawing.Point)(resources.GetObject("lblOwner.Location")));
   this.lblOwner.Name = "lblOwner";
   this.lblOwner.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("lblOwner.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.lblOwner, ((bool)(resources.GetObject("lblOwner.ShowHelp"))));
   this.lblOwner.Size = ((System.Drawing.Size)(resources.GetObject("lblOwner.Size")));
   this.lblOwner.TabIndex = ((int)(resources.GetObject("lblOwner.TabIndex")));
   this.lblOwner.Text = resources.GetString("lblOwner.Text");
   this.lblOwner.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("lblOwner.TextAlign")));
   this.toolTip1.SetToolTip(this.lblOwner, resources.GetString("lblOwner.ToolTip"));
   this.lblOwner.Visible = ((bool)(resources.GetObject("lblOwner.Visible")));
   this.lblLocation.AccessibleDescription = resources.GetString("lblLocation.AccessibleDescription");
   this.lblLocation.AccessibleName = resources.GetString("lblLocation.AccessibleName");
   this.lblLocation.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("lblLocation.Anchor")));
   this.lblLocation.AutoSize = ((bool)(resources.GetObject("lblLocation.AutoSize")));
   this.lblLocation.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("lblLocation.Dock")));
   this.lblLocation.Enabled = ((bool)(resources.GetObject("lblLocation.Enabled")));
   this.lblLocation.Font = ((System.Drawing.Font)(resources.GetObject("lblLocation.Font")));
   this.helpProvider1.SetHelpKeyword(this.lblLocation, resources.GetString("lblLocation.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.lblLocation, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("lblLocation.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.lblLocation, resources.GetString("lblLocation.HelpString"));
   this.lblLocation.Image = ((System.Drawing.Image)(resources.GetObject("lblLocation.Image")));
   this.lblLocation.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("lblLocation.ImageAlign")));
   this.lblLocation.ImageIndex = ((int)(resources.GetObject("lblLocation.ImageIndex")));
   this.lblLocation.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("lblLocation.ImeMode")));
   this.lblLocation.Location = ((System.Drawing.Point)(resources.GetObject("lblLocation.Location")));
   this.lblLocation.Name = "lblLocation";
   this.lblLocation.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("lblLocation.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.lblLocation, ((bool)(resources.GetObject("lblLocation.ShowHelp"))));
   this.lblLocation.Size = ((System.Drawing.Size)(resources.GetObject("lblLocation.Size")));
   this.lblLocation.TabIndex = ((int)(resources.GetObject("lblLocation.TabIndex")));
   this.lblLocation.Text = resources.GetString("lblLocation.Text");
   this.lblLocation.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("lblLocation.TextAlign")));
   this.toolTip1.SetToolTip(this.lblLocation, resources.GetString("lblLocation.ToolTip"));
   this.lblLocation.Visible = ((bool)(resources.GetObject("lblLocation.Visible")));
   this.lblAccount.AccessibleDescription = resources.GetString("lblAccount.AccessibleDescription");
   this.lblAccount.AccessibleName = resources.GetString("lblAccount.AccessibleName");
   this.lblAccount.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("lblAccount.Anchor")));
   this.lblAccount.AutoSize = ((bool)(resources.GetObject("lblAccount.AutoSize")));
   this.lblAccount.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("lblAccount.Dock")));
   this.lblAccount.Enabled = ((bool)(resources.GetObject("lblAccount.Enabled")));
   this.lblAccount.Font = ((System.Drawing.Font)(resources.GetObject("lblAccount.Font")));
   this.helpProvider1.SetHelpKeyword(this.lblAccount, resources.GetString("lblAccount.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.lblAccount, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("lblAccount.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.lblAccount, resources.GetString("lblAccount.HelpString"));
   this.lblAccount.Image = ((System.Drawing.Image)(resources.GetObject("lblAccount.Image")));
   this.lblAccount.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("lblAccount.ImageAlign")));
   this.lblAccount.ImageIndex = ((int)(resources.GetObject("lblAccount.ImageIndex")));
   this.lblAccount.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("lblAccount.ImeMode")));
   this.lblAccount.Location = ((System.Drawing.Point)(resources.GetObject("lblAccount.Location")));
   this.lblAccount.Name = "lblAccount";
   this.lblAccount.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("lblAccount.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.lblAccount, ((bool)(resources.GetObject("lblAccount.ShowHelp"))));
   this.lblAccount.Size = ((System.Drawing.Size)(resources.GetObject("lblAccount.Size")));
   this.lblAccount.TabIndex = ((int)(resources.GetObject("lblAccount.TabIndex")));
   this.lblAccount.Text = resources.GetString("lblAccount.Text");
   this.lblAccount.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("lblAccount.TextAlign")));
   this.toolTip1.SetToolTip(this.lblAccount, resources.GetString("lblAccount.ToolTip"));
   this.lblAccount.Visible = ((bool)(resources.GetObject("lblAccount.Visible")));
   this.iFolderOwner.AccessibleDescription = resources.GetString("iFolderOwner.AccessibleDescription");
   this.iFolderOwner.AccessibleName = resources.GetString("iFolderOwner.AccessibleName");
   this.iFolderOwner.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("iFolderOwner.Anchor")));
   this.iFolderOwner.AutoSize = ((bool)(resources.GetObject("iFolderOwner.AutoSize")));
   this.iFolderOwner.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("iFolderOwner.Dock")));
   this.iFolderOwner.Enabled = ((bool)(resources.GetObject("iFolderOwner.Enabled")));
   this.iFolderOwner.Font = ((System.Drawing.Font)(resources.GetObject("iFolderOwner.Font")));
   this.helpProvider1.SetHelpKeyword(this.iFolderOwner, resources.GetString("iFolderOwner.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.iFolderOwner, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("iFolderOwner.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.iFolderOwner, resources.GetString("iFolderOwner.HelpString"));
   this.iFolderOwner.Image = ((System.Drawing.Image)(resources.GetObject("iFolderOwner.Image")));
   this.iFolderOwner.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("iFolderOwner.ImageAlign")));
   this.iFolderOwner.ImageIndex = ((int)(resources.GetObject("iFolderOwner.ImageIndex")));
   this.iFolderOwner.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("iFolderOwner.ImeMode")));
   this.iFolderOwner.Location = ((System.Drawing.Point)(resources.GetObject("iFolderOwner.Location")));
   this.iFolderOwner.Name = "iFolderOwner";
   this.iFolderOwner.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("iFolderOwner.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.iFolderOwner, ((bool)(resources.GetObject("iFolderOwner.ShowHelp"))));
   this.iFolderOwner.Size = ((System.Drawing.Size)(resources.GetObject("iFolderOwner.Size")));
   this.iFolderOwner.TabIndex = ((int)(resources.GetObject("iFolderOwner.TabIndex")));
   this.iFolderOwner.Text = resources.GetString("iFolderOwner.Text");
   this.iFolderOwner.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("iFolderOwner.TextAlign")));
   this.toolTip1.SetToolTip(this.iFolderOwner, resources.GetString("iFolderOwner.ToolTip"));
   this.iFolderOwner.Visible = ((bool)(resources.GetObject("iFolderOwner.Visible")));
   this.iFolderLocation.AccessibleDescription = resources.GetString("iFolderLocation.AccessibleDescription");
   this.iFolderLocation.AccessibleName = resources.GetString("iFolderLocation.AccessibleName");
   this.iFolderLocation.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("iFolderLocation.Anchor")));
   this.iFolderLocation.AutoSize = ((bool)(resources.GetObject("iFolderLocation.AutoSize")));
   this.iFolderLocation.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("iFolderLocation.Dock")));
   this.iFolderLocation.Enabled = ((bool)(resources.GetObject("iFolderLocation.Enabled")));
   this.iFolderLocation.Font = ((System.Drawing.Font)(resources.GetObject("iFolderLocation.Font")));
   this.helpProvider1.SetHelpKeyword(this.iFolderLocation, resources.GetString("iFolderLocation.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.iFolderLocation, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("iFolderLocation.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.iFolderLocation, resources.GetString("iFolderLocation.HelpString"));
   this.iFolderLocation.Image = ((System.Drawing.Image)(resources.GetObject("iFolderLocation.Image")));
   this.iFolderLocation.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("iFolderLocation.ImageAlign")));
   this.iFolderLocation.ImageIndex = ((int)(resources.GetObject("iFolderLocation.ImageIndex")));
   this.iFolderLocation.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("iFolderLocation.ImeMode")));
   this.iFolderLocation.Location = ((System.Drawing.Point)(resources.GetObject("iFolderLocation.Location")));
   this.iFolderLocation.Name = "iFolderLocation";
   this.iFolderLocation.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("iFolderLocation.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.iFolderLocation, ((bool)(resources.GetObject("iFolderLocation.ShowHelp"))));
   this.iFolderLocation.Size = ((System.Drawing.Size)(resources.GetObject("iFolderLocation.Size")));
   this.iFolderLocation.TabIndex = ((int)(resources.GetObject("iFolderLocation.TabIndex")));
   this.iFolderLocation.Text = resources.GetString("iFolderLocation.Text");
   this.iFolderLocation.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("iFolderLocation.TextAlign")));
   this.toolTip1.SetToolTip(this.iFolderLocation, resources.GetString("iFolderLocation.ToolTip"));
   this.iFolderLocation.Visible = ((bool)(resources.GetObject("iFolderLocation.Visible")));
   this.iFolderAccount.AccessibleDescription = resources.GetString("iFolderAccount.AccessibleDescription");
   this.iFolderAccount.AccessibleName = resources.GetString("iFolderAccount.AccessibleName");
   this.iFolderAccount.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("iFolderAccount.Anchor")));
   this.iFolderAccount.AutoSize = ((bool)(resources.GetObject("iFolderAccount.AutoSize")));
   this.iFolderAccount.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("iFolderAccount.Dock")));
   this.iFolderAccount.Enabled = ((bool)(resources.GetObject("iFolderAccount.Enabled")));
   this.iFolderAccount.Font = ((System.Drawing.Font)(resources.GetObject("iFolderAccount.Font")));
   this.helpProvider1.SetHelpKeyword(this.iFolderAccount, resources.GetString("iFolderAccount.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.iFolderAccount, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("iFolderAccount.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.iFolderAccount, resources.GetString("iFolderAccount.HelpString"));
   this.iFolderAccount.Image = ((System.Drawing.Image)(resources.GetObject("iFolderAccount.Image")));
   this.iFolderAccount.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("iFolderAccount.ImageAlign")));
   this.iFolderAccount.ImageIndex = ((int)(resources.GetObject("iFolderAccount.ImageIndex")));
   this.iFolderAccount.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("iFolderAccount.ImeMode")));
   this.iFolderAccount.Location = ((System.Drawing.Point)(resources.GetObject("iFolderAccount.Location")));
   this.iFolderAccount.Name = "iFolderAccount";
   this.iFolderAccount.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("iFolderAccount.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.iFolderAccount, ((bool)(resources.GetObject("iFolderAccount.ShowHelp"))));
   this.iFolderAccount.Size = ((System.Drawing.Size)(resources.GetObject("iFolderAccount.Size")));
   this.iFolderAccount.TabIndex = ((int)(resources.GetObject("iFolderAccount.TabIndex")));
   this.iFolderAccount.Text = resources.GetString("iFolderAccount.Text");
   this.iFolderAccount.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("iFolderAccount.TextAlign")));
   this.toolTip1.SetToolTip(this.iFolderAccount, resources.GetString("iFolderAccount.ToolTip"));
   this.iFolderAccount.Visible = ((bool)(resources.GetObject("iFolderAccount.Visible")));
   this.syncLabel.AccessibleDescription = resources.GetString("syncLabel.AccessibleDescription");
   this.syncLabel.AccessibleName = resources.GetString("syncLabel.AccessibleName");
   this.syncLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("syncLabel.Anchor")));
   this.syncLabel.AutoSize = ((bool)(resources.GetObject("syncLabel.AutoSize")));
   this.syncLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("syncLabel.Dock")));
   this.syncLabel.Enabled = ((bool)(resources.GetObject("syncLabel.Enabled")));
   this.syncLabel.Font = ((System.Drawing.Font)(resources.GetObject("syncLabel.Font")));
   this.helpProvider1.SetHelpKeyword(this.syncLabel, resources.GetString("syncLabel.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.syncLabel, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("syncLabel.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.syncLabel, resources.GetString("syncLabel.HelpString"));
   this.syncLabel.Image = ((System.Drawing.Image)(resources.GetObject("syncLabel.Image")));
   this.syncLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("syncLabel.ImageAlign")));
   this.syncLabel.ImageIndex = ((int)(resources.GetObject("syncLabel.ImageIndex")));
   this.syncLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("syncLabel.ImeMode")));
   this.syncLabel.Location = ((System.Drawing.Point)(resources.GetObject("syncLabel.Location")));
   this.syncLabel.Name = "syncLabel";
   this.syncLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("syncLabel.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.syncLabel, ((bool)(resources.GetObject("syncLabel.ShowHelp"))));
   this.syncLabel.Size = ((System.Drawing.Size)(resources.GetObject("syncLabel.Size")));
   this.syncLabel.TabIndex = ((int)(resources.GetObject("syncLabel.TabIndex")));
   this.syncLabel.Text = resources.GetString("syncLabel.Text");
   this.syncLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("syncLabel.TextAlign")));
   this.toolTip1.SetToolTip(this.syncLabel, resources.GetString("syncLabel.ToolTip"));
   this.syncLabel.Visible = ((bool)(resources.GetObject("syncLabel.Visible")));
   this.btnHelp.AccessibleDescription = resources.GetString("btnHelp.AccessibleDescription");
   this.btnHelp.AccessibleName = resources.GetString("btnHelp.AccessibleName");
   this.btnHelp.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("btnHelp.Anchor")));
   this.btnHelp.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("btnHelp.BackgroundImage")));
   this.btnHelp.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("btnHelp.Dock")));
   this.btnHelp.Enabled = ((bool)(resources.GetObject("btnHelp.Enabled")));
   this.btnHelp.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("btnHelp.FlatStyle")));
   this.btnHelp.Font = ((System.Drawing.Font)(resources.GetObject("btnHelp.Font")));
   this.helpProvider1.SetHelpKeyword(this.btnHelp, resources.GetString("btnHelp.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this.btnHelp, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("btnHelp.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this.btnHelp, resources.GetString("btnHelp.HelpString"));
   this.btnHelp.Image = ((System.Drawing.Image)(resources.GetObject("btnHelp.Image")));
   this.btnHelp.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnHelp.ImageAlign")));
   this.btnHelp.ImageIndex = ((int)(resources.GetObject("btnHelp.ImageIndex")));
   this.btnHelp.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("btnHelp.ImeMode")));
   this.btnHelp.Location = ((System.Drawing.Point)(resources.GetObject("btnHelp.Location")));
   this.btnHelp.Name = "btnHelp";
   this.btnHelp.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("btnHelp.RightToLeft")));
   this.helpProvider1.SetShowHelp(this.btnHelp, ((bool)(resources.GetObject("btnHelp.ShowHelp"))));
   this.btnHelp.Size = ((System.Drawing.Size)(resources.GetObject("btnHelp.Size")));
   this.btnHelp.TabIndex = ((int)(resources.GetObject("btnHelp.TabIndex")));
   this.btnHelp.Text = resources.GetString("btnHelp.Text");
   this.btnHelp.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnHelp.TextAlign")));
   this.toolTip1.SetToolTip(this.btnHelp, resources.GetString("btnHelp.ToolTip"));
   this.btnHelp.Visible = ((bool)(resources.GetObject("btnHelp.Visible")));
   this.btnHelp.Click += new System.EventHandler(this.btnHelp_Click);
   this.AcceptButton = this.ok;
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.CancelButton = this.cancel;
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.apply);
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.ok);
   this.Controls.Add(this.tabControl1);
   this.Controls.Add(this.btnHelp);
   this.FormBorderStyle = FormBorderStyle.FixedDialog;
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.HelpButton = true;
   this.helpProvider1.SetHelpKeyword(this, resources.GetString("$this.HelpKeyword"));
   this.helpProvider1.SetHelpNavigator(this, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("$this.HelpNavigator"))));
   this.helpProvider1.SetHelpString(this, resources.GetString("$this.HelpString"));
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.KeyPreview = true;
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimizeBox = false;
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "iFolderAdvanced";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.helpProvider1.SetShowHelp(this, ((bool)(resources.GetObject("$this.ShowHelp"))));
   this.ShowInTaskbar = false;
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.toolTip1.SetToolTip(this, resources.GetString("$this.ToolTip"));
   this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.tabControl1_KeyDown);
   this.Resize += new System.EventHandler(this.iFolderAdvanced_Resize);
   this.Load += new System.EventHandler(this.iFolderAdvanced_Load);
   this.Paint += new System.Windows.Forms.PaintEventHandler(this.iFolderAdvanced_Paint);
   this.tabControl1.ResumeLayout(false);
   this.tabGeneral.ResumeLayout(false);
   this.groupBox1.ResumeLayout(false);
   this.groupBox2.ResumeLayout(false);
   ((System.ComponentModel.ISupportInitialize)(this.syncInterval)).EndInit();
   this.groupBox3.ResumeLayout(false);
   this.tabSharing.ResumeLayout(false);
   this.ResumeLayout(false);
  }
  static public string GetLanguageDirectory()
  {
   RegistryKey regKey = Registry.CurrentUser.CreateSubKey(@"SOFTWARE\Novell\iFolder");
   string languageDirectory = regKey.GetValue("language") as String;
   if (languageDirectory == null)
   {
    switch (CultureInfo.CurrentCulture.Name)
    {
     case "cs":
     case "cs-CZ":
      languageDirectory = "cs";
      break;
     case "de":
     case "de-AT":
     case "de-DE":
     case "de-LI":
     case "de-LU":
     case "de-CH":
      languageDirectory = "de";
      break;
     case "es":
     case "es-AR":
     case "es-BO":
     case "es-CL":
     case "es-CO":
     case "es-CR":
     case "es-DO":
     case "es-EC":
     case "es-SV":
     case "es-GT":
     case "es-HN":
     case "es-MX":
     case "es-NI":
     case "es-PA":
     case "es-PY":
     case "es-PE":
     case "es-PR":
     case "es-ES":
     case "es-UY":
     case "es-VE":
      languageDirectory = "es";
      break;
     case "fr":
     case "fr-BE":
     case "fr-CA":
     case "fr-FR":
     case "fr-LU":
     case "fr-MC":
     case "fr-CH":
      languageDirectory = "fr";
      break;
     case "hu":
     case "hu-HU":
      languageDirectory = "hu";
      break;
     case "it":
     case "it-IT":
     case "it-CH":
      languageDirectory = "it";
      break;
     case "ja":
     case "ja-JP":
      languageDirectory = "ja";
      break;
     case "pl":
     case "pl-PL":
      languageDirectory = "pl";
      break;
     case "pt":
     case "pt-BR":
     case "pt-PT":
      languageDirectory = "pt-BR";
      break;
     case "ru":
     case "ru-RU":
      languageDirectory = "ru";
      break;
     case "sk":
     case "sk-SK":
      languageDirectory = "sk";
      break;
     case "zh-CHT":
     case "zh-TW":
     case "zh-HK":
      languageDirectory = "zh-TW";
      break;
     case "zh-CHS":
     case "zh-CN":
     case "zh-MO":
     case "zh-SG":
      languageDirectory = "zh-CN";
      break;
     default:
     {
      languageDirectory = "en";
      break;
     }
    }
   }
   return languageDirectory;
  }
  static public decimal ConvertSecondsToTimeUnit(int seconds, out string units)
  {
   decimal time;
   TimeSpan timeSpan = new TimeSpan(0, 0, seconds);
   if (timeSpan.TotalDays.Equals(Math.Floor(timeSpan.TotalDays)))
   {
    time = (decimal)timeSpan.TotalDays;
    units = "days";
   }
   else if (timeSpan.TotalHours.Equals(Math.Floor(timeSpan.TotalHours)))
   {
    time = (decimal)timeSpan.TotalHours;
    units = "hours";
   }
   else if (timeSpan.TotalMinutes.Equals(Math.Floor(timeSpan.TotalMinutes)))
   {
    time = (decimal)timeSpan.TotalMinutes;
    units = "minutes";
   }
   else
   {
    time = (decimal)seconds;
    units = "seconds";
   }
   return time;
  }
  private int calculateSize(Control control, int delta)
  {
   int size;
   Graphics g = control.CreateGraphics();
   try
   {
    SizeF textSize = g.MeasureString(control.Text, control.Font);
    size = (int)Math.Ceiling(textSize.Width) - control.Width;
   }
   finally
   {
    g.Dispose();
   }
   return (int)Math.Max(delta, size);
  }
  private void nodeEvent(iFolderWeb ifolder, iFolderUser ifolderUser, string eventData)
  {
   try
   {
    if (ifolder != null)
    {
     currentiFolder = ifolder;
    }
    else if (ifolderUser != null)
    {
     ListViewItem lvi;
     lock (subscrHT)
     {
      lvi = (ListViewItem)subscrHT[ifolderUser.ID];
     }
     if (lvi != null)
     {
      ShareListMember slMember = (ShareListMember)lvi.Tag;
      slMember.iFolderUser = ifolderUser;
      lvi.Tag = slMember;
      updateListViewItem(lvi);
     }
     else if (eventData.Equals("NodeCreated"))
     {
      addiFolderUserToListView(ifolderUser);
     }
    }
    else
    {
     lock (subscrHT)
     {
      ListViewItem lvi = (ListViewItem)subscrHT[eventData];
      if (lvi != null)
      {
       lvi.Remove();
       subscrHT.Remove(eventData);
       lock (userIDHT)
       {
        ListViewItem lvitem = (ListViewItem)userIDHT[((ShareListMember)lvi.Tag).iFolderUser.UserID];
        if ((lvitem != null) && (subscrHT[((ShareListMember)lvitem.Tag).iFolderUser.ID] == null))
        {
         userIDHT.Remove(((ShareListMember)lvi.Tag).iFolderUser.UserID);
        }
       }
      }
     }
    }
   }
   catch
   {
   }
  }
  private void fileSync(FileSyncEventArgs fileSyncEventArgs)
  {
   try
   {
    if (fileSyncEventArgs.SizeRemaining == fileSyncEventArgs.SizeToSync)
    {
     if (startSync || (objectsToSync <= 0))
     {
      startSync = false;
      SyncSize syncSize = ifWebService.CalculateSyncSize(currentiFolder.ID);
      objectsToSync = syncSize.SyncNodeCount;
     }
     objectCount.Text = objectsToSync.ToString();
     objectsToSync--;
    }
   }
   catch {}
  }
  private void collectionSync(CollectionSyncEventArgs collectionSyncEventArgs)
  {
   try
   {
    SyncSize syncSize = ifWebService.CalculateSyncSize(currentiFolder.ID);
    objectCount.Text = syncSize.SyncNodeCount.ToString();
    if (collectionSyncEventArgs.Action.Equals(Action.StartSync))
    {
     startSync = true;
    }
    else if ((collectionSyncEventArgs.Action == Action.StopSync) && collectionSyncEventArgs.Connected)
    {
     lastSync.Text = ifWebService.GetMinimaliFolder(currentiFolder.ID,1).LastSyncTime;
    }
   }
   catch {}
  }
  private void connectToWebService()
  {
   if (ifWebService == null)
   {
    RegistryKey regKey = Registry.CurrentUser.OpenSubKey(@"SOFTWARE\Novell\iFolder");
    if (regKey != null)
    {
     string webServiceUri = regKey.GetValue("WebServiceUri") as string;
     string simiasDataPath = regKey.GetValue("SimiasDataPath") as string;
     if ((webServiceUri != null) && (simiasDataPath != null))
     {
      ifWebService = new iFolderWebService();
      ifWebService.Url = webServiceUri + "/iFolder.asmx";
      LocalService.Start(ifWebService, new Uri(webServiceUri), simiasDataPath);
     }
     regKey.Close();
    }
   }
  }
  private ListViewItem addiFolderUserToListView(iFolderUser ifolderUser)
  {
   ListViewItem lvitem;
   lock (subscrHT)
   {
    lvitem = (ListViewItem)subscrHT[ifolderUser.ID];
    if (lvitem == null)
    {
     bool addItem = true;
     ListViewItem lvi;
     lock (userIDHT)
     {
      lvi = (ListViewItem)userIDHT[ifolderUser.UserID];
      if (lvi != null)
      {
       if (!ifolderUser.State.Equals(member))
       {
        addItem = false;
        try
        {
         connectToWebService();
         ifWebService.RemoveiFolderUser(currentiFolder.ID, ifolderUser.UserID);
        }
        catch
        {
        }
       }
       else if (!((ShareListMember)lvi.Tag).iFolderUser.State.Equals(member))
       {
        lvi.Remove();
        subscrHT.Remove(((ShareListMember)lvi.Tag).iFolderUser.ID);
        userIDHT.Remove(ifolderUser.UserID);
       }
      }
     }
     if (addItem)
     {
      ShareListMember slMember = new ShareListMember();
      slMember.iFolderUser = ifolderUser;
      string[] items = new string[3];
      items[0] = (ifolderUser.FN != null) && !ifolderUser.FN.Equals(string.Empty) ? ifolderUser.FN : ifolderUser.Name;
      items[1] = stateToString(ifolderUser.State, ifolderUser.IsOwner);
      int imageIndex = 1;
      items[2] = rightsToString(ifolderUser.Rights);
      if ((currentUser != null) && currentUser.UserID.Equals(ifolderUser.UserID))
      {
       imageIndex = 0;
      }
      else if ((ifolderUser.State != null) && !ifolderUser.State.Equals(member))
      {
       imageIndex = 2;
      }
      lvitem = new ListViewItem(items, imageIndex);
      if (ifolderUser.State.Equals(inviting))
      {
       slMember.Added = true;
      }
      else
      {
       subscrHT.Add(slMember.iFolderUser.ID, lvitem);
      }
      lvitem.Tag = slMember;
      lock (userIDHT)
      {
       bool duplicateName = false;
       foreach (ListViewItem item in userIDHT.Values)
       {
        if (lvitem.SubItems[0].Text.ToLower().Equals(item.SubItems[0].Text.ToLower()))
        {
         duplicateName = true;
         ShareListMember slm = (ShareListMember)item.Tag;
         item.SubItems[0].Text = string.Format("{0} ({1})", slm.iFolderUser.FN, slm.iFolderUser.Name);
        }
       }
       if (duplicateName)
       {
        lvitem.SubItems[0].Text = string.Format("{0} ({1})", ifolderUser.FN, ifolderUser.Name);
       }
       shareWith.Items.Add(lvitem);
       userIDHT[slMember.iFolderUser.UserID] = lvitem;
      }
     }
    }
   }
   return lvitem;
  }
  private void showConflictMessage(bool show)
  {
   if (show)
   {
    if (!conflicts.Visible)
    {
     conflicts.Visible = conflictIcon.Visible = true;
     int delta = initTabTop - conflicts.Top;
     Height += delta;
     tabControl1.Height -= delta;
     tabControl1.Top = initTabTop;
     MinimumSize = initMinSize;
    }
   }
   else
   {
    if (conflicts.Visible)
    {
     conflicts.Visible = conflictIcon.Visible = false;
     int delta = tabControl1.Top - conflicts.Top;
     MinimumSize = new Size(initMinSize.Width, initMinSize.Height - delta);
     Height -= delta;
     tabControl1.Height += delta;
     tabControl1.Top = conflicts.Top;
    }
   }
   iFolderAdvanced_Resize(this, new EventArgs());
  }
  private void updateDiskQuotaDisplay()
  {
   try
   {
    connectToWebService();
    DiskSpace diskSpace = ifWebService.GetiFolderDiskSpace(currentiFolder.ID);
    double usedSpace = Math.Round(diskSpace.UsedSpace/megaByte, 2);
    used.Text = usedSpace.ToString();
    if (diskSpace.Limit != 0)
    {
     limitEdit.Text = limit.Text = ((double)Math.Round(diskSpace.Limit/megaByte, 2)).ToString();
     setLimit.Checked = true;
     available.Text = ((double)Math.Round(diskSpace.AvailableSpace/megaByte, 2)).ToString();
     gaugeChart.MaxValue = diskSpace.Limit / megaByte;
     gaugeChart.Used = usedSpace;
     gaugeChart.BarColor = SystemColors.ActiveCaption;
    }
    else
    {
     setLimit.Checked = false;
     available.Text = limit.Text = limitEdit.Text = "";
     gaugeChart.Used = 0;
    }
   }
   catch (Exception ex)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("diskQuotaReadError"), resourceManager.GetString("errorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
    setLimit.Checked = false;
    used.Text = available.Text = limit.Text = "";
    gaugeChart.Used = 0;
   }
   gaugeChart.Invalidate(true);
  }
  private void refreshData()
  {
   newOwnerLvi = null;
            int tmpSyncVal = 0;
   Cursor = Cursors.WaitCursor;
   updateDiskQuotaDisplay();
   lastSync.Text = currentiFolder.LastSyncTime;
   syncInterval.Value = (decimal)currentiFolder.SyncInterval;
   autoSync.Checked = currentiFolder.SyncInterval != Timeout.Infinite;
   switch (currentiFolder.Role)
   {
    case "Master":
     syncUnits.Visible = autoSync.Visible = syncInterval.Visible = true;
     syncNow.Enabled = false;
     syncLabel.Text = resourceManager.GetString("syncLabel.Text");
     break;
    case "Slave":
     syncUnits.Visible = autoSync.Visible = syncInterval.Visible = false;
     syncNow.Enabled = true;
     string units;
                    if (currentiFolder.EffectiveSyncInterval <= 0)
                    {
                        try
                        {
                            tmpSyncVal = ifWebService.GetDefaultSyncInterval();
                        }
                        catch { }
                    }
                    else
                    {
                        tmpSyncVal = currentiFolder.EffectiveSyncInterval;
                    }
                    decimal syncValue = ConvertSecondsToTimeUnit(tmpSyncVal, out units);
     syncLabel.Text = string.Format(resourceManager.GetString("slaveSyncInterval"), syncValue, resourceManager.GetString(units));
     break;
   }
   try
   {
    SyncSize syncSize = ifWebService.CalculateSyncSize(currentiFolder.ID);
    objectCount.Text = syncSize.SyncNodeCount.ToString();
   }
   catch
   {
    if (currentiFolder.Role.Equals("Master"))
    {
     objectCount.Text = "0";
    }
    else
    {
     objectCount.Text = resourceManager.GetString("unknown");
    }
   }
   shareWith.Items.Clear();
   shareWith.BeginUpdate();
   try
   {
    lock (subscrHT)
    {
     subscrHT.Clear();
    }
    lock (userIDHT)
    {
     userIDHT.Clear();
    }
    connectToWebService();
    iFolderUser[] ifolderUsers = ifWebService.GetiFolderUsers(currentiFolder.ID);
    foreach (iFolderUser ifolderUser in ifolderUsers)
    {
     if (ifolderUser.UserID.Equals(currentiFolder.CurrentUserID))
     {
      currentUser = ifolderUser;
     }
     ListViewItem lvitem = addiFolderUserToListView(ifolderUser);
     if (ifolderUser.UserID.Equals(currentiFolder.OwnerID))
     {
      ownerLvi = lvitem;
     }
    }
    shareWith.Items[0].Selected = true;
   }
   catch (WebException ex)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("memberReadError"), resourceManager.GetString("errorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
    if (ex.Status == WebExceptionStatus.ConnectFailure)
    {
     ifWebService = null;
    }
   }
   catch (Exception ex)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("memberReadError"), resourceManager.GetString("errorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
   shareWith.EndUpdate();
   add.Enabled = currentUser != null ? currentUser.Rights.Equals("Admin") : false;
   if( this.currentiFolder.encryptionAlgorithm != null && this.currentiFolder.encryptionAlgorithm != "")
   {
    this.add.Enabled = false;
   }
   setLimit.Visible = limitEdit.Visible = false;
            limitLabel.Visible = limit.Visible = !setLimit.Visible;
            this.ssl.Enabled = currentUser != null ? currentUser.UserID.Equals(currentiFolder.OwnerID) : false;
            if (this.DomainUrl.StartsWith(Uri.UriSchemeHttps))
            {
                this.ssl.Checked = true;
                this.ssl.Enabled = false;
            }
   Cursor = Cursors.Default;
  }
  private string rightsToString(string rights)
  {
   string rightsString = null;
   switch (rights)
   {
    case "Admin":
    case "ReadWrite":
    case "ReadOnly":
    case "Deny":
    {
     rightsString = resourceManager.GetString(rights);
     break;
    }
    default:
    {
     rightsString = resourceManager.GetString("unknown");
     break;
    }
   }
   return rightsString;
  }
  private void resizeButton(Button button)
  {
   Graphics g = button.CreateGraphics();
   try
   {
    Point p = button.Location;
    int width = button.Width;
    SizeF size = g.MeasureString(button.Text, button.Font);
    button.Width = (int)Math.Ceiling(size.Width) + 20;
    if ((button.Anchor & AnchorStyles.Right) == AnchorStyles.Right)
    {
     button.Left = p.X - (button.Width - width);
    }
   }
   finally
   {
    g.Dispose();
   }
  }
  private string stateToString(string state, bool isOwner)
  {
   string stateString;
   switch (state)
   {
    case "Invited":
    case "WaitSync":
    case "AccessRequest":
    case "Declined":
    case inviting:
     stateString = resourceManager.GetString(state);
     break;
    case member:
     stateString = resourceManager.GetString(isOwner ? "owner" : "user");
     break;
    default:
     stateString = resourceManager.GetString("unknown");
     break;
   }
   return stateString;
  }
  private void processChanges()
  {
   Cursor = Cursors.WaitCursor;
   if (newOwnerLvi != null)
   {
    try
    {
     connectToWebService();
     ShareListMember oldOwner = (ShareListMember)ownerLvi.Tag;
     ShareListMember newOwner = (ShareListMember)newOwnerLvi.Tag;
                    if(ifWebService.CanOwnerBeChanged(newOwner.iFolderUser.UserID,currentiFolder.DomainID))
                    {
         ifWebService.ChangeOwner(currentiFolder.ID, newOwner.iFolderUser.UserID, oldOwner.iFolderUser.Rights);
         oldOwner.Changed = newOwner.Changed = false;
                    }
                    else
                    {
                        MyMessageBox mmb = new MyMessageBox(String.Format(resourceManager.GetString("ifolderlimiterror"), currentiFolder.Name,newOwner.iFolderUser.Name), resourceManager.GetString("errorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                        mmb.ShowDialog();
                    }
    }
    catch (WebException e)
    {
     MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("changeOwnerError"), resourceManager.GetString("saveErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
     mmb.ShowDialog();
     if (e.Status == WebExceptionStatus.ConnectFailure)
     {
      ifWebService = null;
     }
    }
    catch (Exception e)
    {
     if (e.Message.IndexOf("Invalid iFolderID") == -1)
     {
      MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("changeOwnerError"), resourceManager.GetString("saveErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
      mmb.ShowDialog();
     }
    }
   }
            foreach (ListViewItem lvitem in shareWith.Items)
            {
                ShareListMember slMember = (ShareListMember)lvitem.Tag;
                try
                {
                    connectToWebService();
                    if (slMember.Added)
                    {
                        slMember.iFolderUser = ifWebService.AddAndInviteUser(
                            currentiFolder.ID,
                            slMember.iFolderUser.Name,
                            slMember.iFolderUser.FirstName,
                            slMember.iFolderUser.Surname,
                            slMember.iFolderUser.UserID,
                            null,
                            slMember.iFolderUser.Rights);
                        lvitem.Tag = slMember;
                        updateListViewItem(lvitem);
                        lock (subscrHT)
                        {
                            subscrHT.Add(slMember.iFolderUser.ID, lvitem);
                        }
                        slMember.Added = false;
                    }
                    else if (slMember.Changed)
                    {
                        ifWebService.SetUserRights(currentiFolder.ID, slMember.iFolderUser.UserID, slMember.iFolderUser.Rights);
                        slMember.Changed = false;
                    }
                }
                catch (WebException e)
                {
                    MyMessageBox mmb = new MyMessageBox(string.Format(resourceManager.GetString("memberCommitError"), slMember.Name), resourceManager.GetString("saveErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                    mmb.ShowDialog();
                    if (e.Status == WebExceptionStatus.ConnectFailure)
                    {
                        ifWebService = null;
                    }
                }
                catch (Exception e)
                {
                    if (e.Message.IndexOf("Invalid iFolderID") == -1)
                    {
                        MyMessageBox mmb = new MyMessageBox(string.Format(resourceManager.GetString("memberCommitError"), slMember.Name), resourceManager.GetString("saveErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                        mmb.ShowDialog();
                    }
                }
            }
   if (removedList != null)
   {
    foreach (ShareListMember slMember in removedList)
    {
     try
     {
      connectToWebService();
      ifWebService.RemoveiFolderUser(currentiFolder.ID, slMember.iFolderUser.UserID);
     }
     catch (WebException e)
     {
      MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("removeError"), resourceManager.GetString("saveErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
      mmb.ShowDialog();
      if (e.Status == WebExceptionStatus.ConnectFailure)
      {
       ifWebService = null;
      }
     }
     catch (Exception e)
     {
      if (e.Message.IndexOf("Invalid iFolderID") == -1)
      {
       MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("removeError"), resourceManager.GetString("saveErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
       mmb.ShowDialog();
      }
     }
    }
    removedList.Clear();
   }
   try
   {
    connectToWebService();
    if ((currentiFolder.SyncInterval != (int)syncInterval.Value) ||
     (autoSync.Checked != (currentiFolder.SyncInterval != System.Threading.Timeout.Infinite)))
    {
     ifWebService.SetiFolderSyncInterval(currentiFolder.ID, autoSync.Checked ? (int)syncInterval.Value : Timeout.Infinite);
    }
    updateDiskQuotaDisplay();
                if (currentiFolder.ssl != ssl.Checked)
                {
                    ifWebService.SetiFolderSecureSync(currentiFolder.ID, ssl.Checked);
                }
   }
   catch (WebException e)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("policyCommitError"), resourceManager.GetString("saveErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
    if (e.Status == WebExceptionStatus.ConnectFailure)
    {
     ifWebService = null;
    }
   }
   catch (Exception e)
   {
    if (e.Message.IndexOf("Invalid iFolderID") == -1)
    {
     MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("policyCommitError"), resourceManager.GetString("saveErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
     mmb.ShowDialog();
    }
   }
   apply.Enabled = false;
   Cursor = Cursors.Default;
  }
        private void updateSelectedListViewItems(string rights)
  {
   foreach (ListViewItem lvi in shareWith.SelectedItems)
   {
    updateListViewItem(lvi, rights);
   }
  }
  private void updateListViewItem(ListViewItem lvi, string rights)
  {
   ShareListMember slMember = (ShareListMember)lvi.Tag;
   string access = rightsToString(rights);
   try
   {
    if (slMember.iFolderUser.UserID.Equals(currentUser.UserID) ||
     slMember.iFolderUser.UserID.Equals(currentiFolder.OwnerID) ||
     ((newOwnerLvi != null) && lvi.Equals(this.newOwnerLvi)))
    {
    }
    else
    {
     if (!slMember.iFolderUser.Rights.Equals(rights))
     {
      slMember.Changed = true;
      slMember.iFolderUser.Rights = rights;
      lvi.SubItems[2].Text = access;
      apply.Enabled = true;
     }
     if (slMember.iFolderUser.State.Equals(member))
     {
      lvi.SubItems[1].Text = stateToString(slMember.iFolderUser.State, slMember.iFolderUser.IsOwner);
     }
    }
   }
   catch{}
  }
        private void updateListViewItem(ListViewItem lvi)
  {
   ShareListMember slMember = (ShareListMember)lvi.Tag;
   lvi.SubItems[0].Text = slMember.Name;
   lvi.SubItems[1].Text = stateToString(slMember.iFolderUser.State, slMember.iFolderUser.IsOwner);
   lvi.SubItems[2].Text = rightsToString(slMember.iFolderUser.Rights);
   if (slMember.iFolderUser.UserID.Equals(currentUser.UserID))
   {
    currentUser = slMember.iFolderUser;
    lvi.ImageIndex = 0;
    add.Enabled = currentUser.Rights.Equals("Admin");
    access.Enabled = access.Enabled ? add.Enabled : false;
    remove.Enabled = remove.Enabled ? add.Enabled : false;
    menuFullControl.Enabled = menuReadWrite.Enabled = menuReadOnly.Enabled =
     menuFullControl.Enabled ? add.Enabled : false;
    if (slMember.iFolderUser.IsOwner && !ownerLvi.Equals(lvi))
    {
     setLimit.Visible = limitEdit.Visible = true;
     limitLabel.Visible = limit.Visible = false;
     ownerLvi.SubItems[1].Text = resourceManager.GetString("user");
     ownerLvi = lvi;
    }
   }
   else
   {
    lvi.ImageIndex = slMember.iFolderUser.State.Equals(member) ? 1 : 2;
   }
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
                if (count <= 1)
                {
                    workEvent.WaitOne();
                    continue;
                }
    iFolderWeb ifolder = null;
    iFolderUser ifolderUser = null;
    try
    {
     switch (eventArgs.EventData)
     {
      case "NodeChanged":
      {
       if (eventArgs.Type.Equals(NodeTypes.CollectionType) && currentiFolder.ID.Equals(eventArgs.Collection))
       {
        ifolder = ifWebService.GetMinimaliFolder(eventArgs.Collection,1);
       }
       else if (eventArgs.Type.Equals(NodeTypes.MemberType) || eventArgs.Type.Equals(NodeTypes.SubscriptionType))
       {
        ifolderUser = ifWebService.GetiFolderUserFromNodeID(eventArgs.Collection, eventArgs.Node);
        if ((ifolderUser.iFolderID != null) && (!ifolderUser.iFolderID.Equals(currentiFolder.ID)))
        {
         ifolderUser = null;
        }
       }
       break;
      }
      case "NodeCreated":
      {
       if (currentiFolder.ID.Equals(eventArgs.Collection) && (eventArgs.Type.Equals(NodeTypes.MemberType) || eventArgs.Type.Equals(NodeTypes.SubscriptionType)))
       {
        ifolderUser = ifWebService.GetiFolderUserFromNodeID(eventArgs.Collection, eventArgs.Node);
       }
       break;
      }
      case "NodeDeleted":
      {
       BeginInvoke(nodeDelegate, new object[] {null, null, eventArgs.Node});
       break;
      }
     }
    }
    catch
    {
    }
    if ((ifolder != null) || (ifolderUser != null))
    {
     BeginInvoke(nodeDelegate, new object[] {ifolder, ifolderUser, eventArgs.EventData});
    }
    if (count <= 1)
    {
     workEvent.WaitOne();
    }
   }
  }
  public IProcEventClient EventClient
  {
   set { this.eventClient = value; }
  }
  public iFolderWebService iFolderWebService
  {
   set { ifWebService = value; }
  }
  public iFolderWeb CurrentiFolder
  {
   set
   {
    this.currentiFolder = value;
   }
  }
  public string DomainName
  {
   set
   {
    domainName = value;
   }
  }
        public string DomainUrl
        {
            set
            {
                domainUrl = value;
            }
            get
            {
                return domainUrl;
            }
        }
  public string LoadPath
  {
   get
   {
    return loadPath;
   }
   set
   {
    this.loadPath = value;
   }
  }
  public int ActiveTab
  {
   set
   {
    try
    {
     tabControl1.SelectedIndex = value;
    }
    catch{}
   }
  }
        private void iFolderAdvanced_Load(object sender, EventArgs e)
  {
   string helpFile = Path.Combine(Path.Combine(Path.Combine(loadPath, "help"), GetLanguageDirectory()), @"sharewith.html");
   if (!File.Exists(helpFile))
   {
    helpFile = Path.Combine(loadPath, @"help\en\sharewith.html");
   }
   if (File.Exists(helpFile))
   {
    helpProvider1.HelpNamespace = helpFile;
   }
   try
   {
    shareWith.SmallImageList = new ImageList();
    string basePath = loadPath != null ? Path.Combine(loadPath, "res") : Path.Combine(Application.StartupPath, "res");
    shareWith.SmallImageList.Images.Add(new Icon(Path.Combine(basePath, "ifolder_me_card.ico")));
    shareWith.SmallImageList.Images.Add(new Icon(Path.Combine(basePath, "ifolder_contact_card.ico")));
    shareWith.SmallImageList.Images.Add(new Icon(Path.Combine(basePath, "inviteduser.ico")));
                this.Text = this.currentiFolder.Name + " " + resourceManager.GetString("properties");
    this.Icon = new Icon(Path.Combine(basePath, @"ifolder_16.ico"));
    this.pictureBox1.Image = Image.FromFile(System.IO.Path.Combine(loadPath, @"res\ifolder48.png"));
                this.iFolderName.Text = this.currentiFolder.Name;
                if (this.currentiFolder.Name.Length > 20)
                    this.iFolderName.Text = this.currentiFolder.Name.Substring(0,20) + "...";
                if (this.currentiFolder.UnManagedPath.ToString().Length > 30)
        this.iFolderLocation.Text = ((string)this.currentiFolder.UnManagedPath).Substring(0,30);
                else
                    this.iFolderLocation.Text = ((string)this.currentiFolder.UnManagedPath).ToString();
    this.iFolderOwner.Text = this.currentiFolder.Owner;
    this.iFolderAccount.Text = this.domainName;
                this.ssl.Checked = this.currentiFolder.ssl;
   }
   catch {}
   subscrHT = new Hashtable();
   userIDHT = new Hashtable();
   if (eventClient == null)
   {
    eventClient = new IProcEventClient(new IProcEventError(errorHandler), null);
    existingEventClient = false;
    eventClient.Register();
   }
   if (!eventError)
   {
    eventClient.SetEvent(IProcEventAction.AddNodeChanged, new IProcEventHandler(nodeEventHandler));
    eventClient.SetEvent(IProcEventAction.AddNodeCreated, new IProcEventHandler(nodeEventHandler));
    eventClient.SetEvent(IProcEventAction.AddNodeDeleted, new IProcEventHandler(nodeEventHandler));
    eventClient.SetEvent(IProcEventAction.AddCollectionSync, new IProcEventHandler(collectionSyncHandler));
    eventClient.SetEvent(IProcEventAction.AddFileSync, new IProcEventHandler(fileSyncHandler));
   }
   try
   {
    connectToWebService();
    refreshData();
   }
   catch (WebException ex)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("iFolderReadError"), resourceManager.GetString("errorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
    if (ex.Status == WebExceptionStatus.ConnectFailure)
    {
     ifWebService = null;
    }
   }
   catch (Exception ex)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("iFolderReadError"), resourceManager.GetString("errorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
  }
        private void shareWith_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   if (shareWith.SelectedItems.Count == 1)
   {
    ListViewItem lvi = shareWith.SelectedItems[0];
   }
   else
   {
   }
   try
   {
    if ((shareWith.SelectedItems.Count == 1) &&
     (((ShareListMember)shareWith.SelectedItems[0].Tag).iFolderUser.UserID.Equals(currentUser.UserID) ||
     ((ShareListMember)shareWith.SelectedItems[0].Tag).iFolderUser.UserID.Equals(currentiFolder.OwnerID) ||
     ((newOwnerLvi != null) && shareWith.SelectedItems[0].Equals(newOwnerLvi))))
    {
     remove.Enabled = access.Enabled = menuFullControl.Enabled =
      menuReadWrite.Enabled = menuReadOnly.Enabled = false;
    }
    else
    {
     remove.Enabled = access.Enabled = menuFullControl.Enabled =
      menuReadWrite.Enabled = menuReadOnly.Enabled =
      (shareWith.SelectedItems.Count != 0 && currentUser.Rights.Equals("Admin"));
    }
   }
   catch {}
  }
        private void add_Click(object sender, System.EventArgs e)
  {
            bool SharingIsDisabled = ifWebService.GetDisableSharingPolicy(currentUser.UserID, currentiFolder.ID, currentiFolder.OwnerID, currentiFolder.DomainID);
   if( SharingIsDisabled == true)
   {
    Picker picker = new Picker();
    picker.LoadPath = loadPath;
    picker.iFolderWebService = ifWebService;
    picker.Ht = userIDHT;
    picker.CurrentUser = currentUser;
    picker.DomainID = currentiFolder.DomainID;
    if (ownerLvi != null)
    {
     picker.CurrentOwner = newOwnerLvi == null ? ((ShareListMember)ownerLvi.Tag).iFolderUser : ((ShareListMember)newOwnerLvi.Tag).iFolderUser;
    }
    picker.CreateControl();
    DialogResult result = picker.ShowDialog();
    if (result == DialogResult.OK)
    {
     Cursor.Current = Cursors.WaitCursor;
     shareWith.SelectedItems.Clear();
     apply.Enabled = true;
     foreach (ListViewItem lvi in picker.AddedUsers)
     {
      iFolderUser user = picker.GetiFolderUserFromListViewItem(lvi);
      ListViewItem lvitem;
      lock (userIDHT)
      {
       lvitem = (ListViewItem)userIDHT[user.UserID];
      }
      if (lvitem == null)
      {
                            user.Rights = "ReadOnly";
       user.State = inviting;
       addiFolderUserToListView(user);
      }
     }
     foreach (iFolderUser ifUser in picker.RemovedList)
     {
      lock (subscrHT)
      {
       ListViewItem lvi = (ListViewItem)subscrHT[ifUser.ID];
       if (lvi != null)
       {
        if (removedList == null)
        {
         removedList = new ArrayList();
        }
        removedList.Add(lvi.Tag);
        lvi.Remove();
        subscrHT.Remove(ifUser.ID);
       }
       else
       {
        lock (userIDHT)
        {
         lvi = (ListViewItem)userIDHT[ifUser.UserID];
         if (lvi != null)
         {
          lvi.Remove();
         }
        }
       }
      }
      lock (userIDHT)
      {
       userIDHT.Remove(ifUser.UserID);
      }
     }
     Cursor.Current = Cursors.Default;
    }
   }
   else
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("DisableSharingError"), resourceManager.GetString("PolicyViolation"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
    mmb.ShowDialog();
   }
  }
        private void remove_Click(object sender, System.EventArgs e)
  {
   foreach (ListViewItem lvi in shareWith.SelectedItems)
   {
    ShareListMember slMember = (ShareListMember)lvi.Tag;
    try
    {
     if (!((currentUser.UserID.Equals(slMember.iFolderUser.UserID) ||
      ((newOwnerLvi == null) && slMember.iFolderUser.UserID.Equals(currentiFolder.OwnerID)) ||
      ((newOwnerLvi != null) && lvi.Equals(newOwnerLvi))) &&
      slMember.iFolderUser.State.Equals(member)))
     {
      if (!slMember.Added)
      {
       if (removedList == null)
       {
        removedList = new ArrayList();
       }
       removedList.Add(slMember);
      }
      lvi.Remove();
      lock (subscrHT)
      {
       subscrHT.Remove(slMember.iFolderUser.ID);
      }
      lock (userIDHT)
      {
       userIDHT.Remove(slMember.iFolderUser.UserID);
      }
      apply.Enabled = true;
     }
    }
    catch
    {
    }
   }
  }
        private void ok_Click(object sender, System.EventArgs e)
  {
   processChanges();
   Close();
  }
        private void accept_Click(object sender, System.EventArgs e)
  {
  }
        private void decline_Click(object sender, System.EventArgs e)
  {
  }
        private void apply_Click(object sender, System.EventArgs e)
  {
   this.processChanges();
   try
   {
    connectToWebService();
    string id = currentiFolder.ID;
    currentiFolder = null;
    currentiFolder = ifWebService.GetiFolder(id);
   }
   catch (Exception ex)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("iFolderReadError"), resourceManager.GetString("errorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
  }
        private void cancel_Click(object sender, System.EventArgs e)
  {
   this.Close();
  }
        private void autoSync_CheckedChanged(object sender, System.EventArgs e)
  {
   syncInterval.Enabled = autoSync.Checked;
   if (autoSync.Focused)
    apply.Enabled = true;
  }
        private void syncInterval_ValueChanged(object sender, System.EventArgs e)
  {
   if (syncInterval.Focused)
   {
    if (!syncInterval.Text.Equals(string.Empty))
    {
     syncInterval.Value = decimal.Parse(syncInterval.Text);
    }
    apply.Enabled = true;
   }
  }
        private void conflicts_LinkClicked(object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e)
  {
            if( !iFolderComponent.AdvancedConflictResolver(ifWebService, currentiFolder))
            {
       ConflictResolver conflictResolver = new ConflictResolver();
       conflictResolver.iFolder = currentiFolder;
       conflictResolver.iFolderWebService = ifWebService;
       conflictResolver.LoadPath = loadPath;
       conflictResolver.ConflictsResolved += new Novell.iFolderCom.ConflictResolver.ConflictsResolvedDelegate(conflictResolver_ConflictsResolved);
       conflictResolver.ShowDialog();
            }
  }
  private void conflictResolver_ConflictsResolved(object sender, EventArgs e)
  {
   showConflictMessage(false);
  }
        private void errorHandler( ApplicationException e, object context )
  {
   eventError = true;
  }
  private void nodeEventHandler(SimiasEventArgs args)
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
  private void collectionSyncHandler(SimiasEventArgs args)
  {
   try
   {
    CollectionSyncEventArgs syncEventArgs = args as CollectionSyncEventArgs;
    if (currentiFolder.ID.Equals(syncEventArgs.ID))
    {
     BeginInvoke(collectionSyncDelegate, new object[] {syncEventArgs});
    }
   }
   catch {}
  }
  private void fileSyncHandler(SimiasEventArgs args)
  {
   try
   {
    FileSyncEventArgs syncEventArgs = args as FileSyncEventArgs;
    if (syncEventArgs.CollectionID.Equals(currentiFolder.ID))
    {
     BeginInvoke(fileSyncDelegate, new object[] {syncEventArgs});
    }
   }
   catch {}
  }
        private void setLimit_CheckedChanged(object sender, System.EventArgs e)
  {
   limitEdit.Enabled = setLimit.Checked;
   if (setLimit.Focused)
   {
    apply.Enabled = true;
   }
  }
        private void ssl_CheckedChanged(object sender, System.EventArgs e)
        {
            apply.Enabled = true;
        }
        private void limitEdit_TextChanged(object sender, System.EventArgs e)
  {
   if (limitEdit.Focused)
   {
    apply.Enabled = true;
   }
  }
        private void access_Click(object sender, System.EventArgs e)
  {
   UserProperties userProperties = new UserProperties();
   userProperties.OwnerCanBeSet = (currentUser.UserID.Equals(currentiFolder.OwnerID) && (shareWith.SelectedItems.Count == 1));
   if (shareWith.SelectedItems.Count == 1)
   {
    ListViewItem lvi = shareWith.SelectedItems[0];
    userProperties.Title = string.Format(resourceManager.GetString("userProperties"), lvi.Text);
    userProperties.Rights = ((ShareListMember)lvi.Tag).iFolderUser.Rights;
    userProperties.CanBeOwner = ((ShareListMember)lvi.Tag).iFolderUser.State.Equals(member);
    userProperties.IsOwner = newOwnerLvi != null ? lvi.Equals(newOwnerLvi) : lvi.Equals(ownerLvi);
   }
   if (DialogResult.OK == userProperties.ShowDialog())
   {
    updateSelectedListViewItems(userProperties.Rights);
    if (shareWith.SelectedItems.Count == 1)
    {
     if (userProperties.IsOwner)
     {
      ListViewItem lvi = shareWith.SelectedItems[0];
      lvi.SubItems[1].Text = resourceManager.GetString("owner");
      if (newOwnerLvi != null)
      {
       newOwnerLvi.SubItems[1].Text = resourceManager.GetString("user");
      }
      else
      {
       ownerLvi.SubItems[1].Text = resourceManager.GetString("user");
      }
      newOwnerLvi = lvi;
      access.Enabled = remove.Enabled = false;
      apply.Enabled = true;
     }
    }
   }
  }
        private void shareWith_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
  {
   if (e.Button.Equals(MouseButtons.Right))
   {
    accessClick = e.X > (columnHeader1.Width + columnHeader2.Width);
   }
  }
        private void contextMenu1_Popup(object sender, System.EventArgs e)
  {
   if (accessClick)
   {
    menuFullControl.Visible = menuReadWrite.Visible = menuReadOnly.Visible = shareWith.SelectedItems.Count != 0;
    if (shareWith.SelectedItems.Count == 1)
    {
     ShareListMember slMember = (ShareListMember)shareWith.SelectedItems[0].Tag;
     switch (slMember.iFolderUser.Rights)
     {
      case "Admin":
       menuFullControl.Checked = true;
       menuReadWrite.Checked = menuReadOnly.Checked = false;
       break;
      case "ReadWrite":
       menuReadWrite.Checked = true;
       menuFullControl.Checked = menuReadOnly.Checked = false;
       break;
      case "ReadOnly":
       menuReadOnly.Checked = true;
       menuFullControl.Checked = menuReadWrite.Checked = false;
       break;
     }
    }
    else if (shareWith.SelectedItems.Count > 1)
    {
     menuFullControl.Checked = menuReadWrite.Checked = menuReadOnly.Checked = false;
    }
   }
   else
   {
    menuFullControl.Visible = menuReadWrite.Visible = menuReadOnly.Visible = false;
   }
  }
        private void menuFullControl_Click(object sender, System.EventArgs e)
  {
   updateSelectedListViewItems("Admin");
  }
  private void menuReadWrite_Click(object sender, System.EventArgs e)
  {
   updateSelectedListViewItems("ReadWrite");
  }
        private void menuReadOnly_Click(object sender, System.EventArgs e)
  {
   updateSelectedListViewItems("ReadOnly");
  }
        private void shareWith_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
  {
   if (e.KeyCode == Keys.Delete)
   {
    remove_Click(this, new System.EventArgs());
   }
  }
        private void tabControl1_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
  {
   if (!this.Modal && e.KeyCode == Keys.Tab)
   {
    try
    {
     bool skip = currentControl.GetType().Equals(typeof(System.Windows.Forms.ComboBox));
     while (true)
     {
      currentControl = this.GetNextControl(currentControl, !e.Shift);
      if (currentControl == null)
      {
       currentControl = e.Shift ? lastControl : firstControl;
      }
      if (currentControl.CanFocus)
      {
       Type type = currentControl.GetType();
       if (!type.Equals(typeof(System.Windows.Forms.Label)) &&
        !type.Equals(typeof(System.Windows.Forms.TabPage)) &&
        !type.Equals(typeof(System.Windows.Forms.GroupBox)))
       {
        if (skip)
        {
         skip = false;
         continue;
        }
        else
        {
         break;
        }
       }
      }
     }
     currentControl.Focus();
    }
    catch
    {
    }
   }
  }
        private void iFolderAdvanced_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
  {
   if (!longName.Equals(string.Empty))
   {
    SizeF size = e.Graphics.MeasureString(longName, ifolders.Font);
    int maxWidth = ifolders.Width * 2;
    ifolders.DropDownWidth = (int)size.Width > maxWidth ? maxWidth : (int)size.Width;
   }
  }
        private void syncNow_Click(object sender, System.EventArgs e)
  {
   try
   {
    ifWebService.SynciFolderNow(currentiFolder.ID);
   }
   catch (Exception ex)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("syncError"), string.Empty, ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
  }
        private void iFolderAdvanced_Resize(object sender, System.EventArgs e)
  {
   shareWith.Width = tabControl1.Width - shareWith.Left - leftDelta;
   shareWith.Height = tabControl1.Height - shareWith.Top - bottomDelta;
   access.Top = add.Top = remove.Top = shareWith.Top + shareWith.Height + lvbDelta;
   remove.Left = shareWith.Left + shareWith.Width - remove.Width;
   add.Left = remove.Left - add.Width - bbDelta;
  }
        private void btnHelp_Click(object sender, System.EventArgs e)
  {
   string helpFile = "";
   if( this.tabControl1.SelectedIndex == 1)
   {
    helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"sharewith.html");
                helpFile = veryfiyPath(helpFile, "sharewith.html");
   }
   else if( this.tabControl1.SelectedIndex == 0)
   {
    helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"propifolders.html");
                helpFile = veryfiyPath(helpFile, "propifolders.html");
   }
   new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
  }
        static public string veryfiyPath(string exitingPath, string helpFile)
        {
            string filePath = helpFile;
            if (File.Exists(exitingPath) == false)
            {
                string dllName = "iFolderComponent.dll";
                System.Reflection.Assembly iFldll = System.Reflection.Assembly.Load("iFolderComponent");
                int len = dllName.Length;
                string loadpath = iFldll.Location;
                char[] trimlist = { '\\' };
                loadpath = loadpath.TrimEnd(trimlist);
                int len2 = loadpath.Length;
                loadpath = loadpath.Substring(0, len2 - len - 1);
                helpFile = Path.Combine(Path.Combine(Path.Combine(loadpath, "help"), iFolderAdvanced.GetLanguageDirectory()), @helpFile);
                filePath = helpFile;
            }
            return filePath;
        }
 }
}
