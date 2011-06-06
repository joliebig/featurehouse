using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Net;
using System.IO;
using System.Runtime.InteropServices;
using Novell.iFolder.Web;
namespace Novell.iFolderCom
{
 [ComVisible(false)]
 public class ConflictResolver : System.Windows.Forms.Form
 {
  System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(ConflictResolver));
  protected iFolderWebService ifWebService;
        protected iFolderWeb ifolder;
        protected string loadPath;
        protected bool fixPath = true;
        protected bool fixNames = true;
        protected Conflicts conflicts = null;
        protected bool readOnly;
        protected System.Windows.Forms.Label label1;
        protected System.Windows.Forms.Label label2;
        protected System.Windows.Forms.Label label3;
        protected System.Windows.Forms.Label ifolderName;
        protected System.Windows.Forms.Label ifolderPath;
        protected System.Windows.Forms.ColumnHeader columnHeader1;
        protected System.Windows.Forms.Label label4;
        protected System.Windows.Forms.Label label5;
        protected System.Windows.Forms.Label label6;
        protected System.Windows.Forms.Label localName;
        protected System.Windows.Forms.Label localDate;
        protected System.Windows.Forms.Label localSize;
        protected System.Windows.Forms.Button saveLocal;
        protected System.Windows.Forms.Button saveServer;
        protected System.Windows.Forms.Label serverSize;
        protected System.Windows.Forms.Label serverDate;
        protected System.Windows.Forms.Label serverName;
        protected System.Windows.Forms.Label label10;
        protected System.Windows.Forms.Label label11;
        protected System.Windows.Forms.Label label12;
        protected System.Windows.Forms.Button close;
        protected System.Windows.Forms.ListView conflictsView;
        protected System.Windows.Forms.ToolTip toolTip1;
        protected System.Windows.Forms.GroupBox localVersion;
        protected System.Windows.Forms.GroupBox serverVersion;
        protected System.Windows.Forms.Panel localPanel;
        protected System.Windows.Forms.Panel serverPanel;
        protected System.Windows.Forms.HelpProvider helpProvider1;
        protected System.Windows.Forms.ColumnHeader columnHeader2;
        protected System.Windows.Forms.ColumnHeader columnHeader3;
        protected System.Windows.Forms.Panel versionsPanel;
        protected System.Windows.Forms.GroupBox nameConflict;
        protected System.Windows.Forms.Label label8;
        protected System.Windows.Forms.TextBox newName;
        protected System.Windows.Forms.Button resolveName;
        protected System.Windows.Forms.Label resolveNameLabel;
        protected System.ComponentModel.IContainer components;
        protected string conflictErrorMsg, conflictErrorTitle;
  public ConflictResolver()
  {
   InitializeComponent();
   this.MinimumSize = this.Size;
   this.StartPosition = FormStartPosition.CenterParent;
  }
        public ConflictResolver(int dummy)
        {
            conflictErrorMsg = resourceManager.GetString("conflictResolveError");
            conflictErrorTitle = resourceManager.GetString("conflictErrorTitle");
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
        protected void AllocateMemForComponents()
        {
            this.components = new System.ComponentModel.Container();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.ifolderName = new System.Windows.Forms.Label();
            this.ifolderPath = new System.Windows.Forms.Label();
            this.conflictsView = new System.Windows.Forms.ListView();
            this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader3 = new System.Windows.Forms.ColumnHeader();
            this.localVersion = new System.Windows.Forms.GroupBox();
            this.saveLocal = new System.Windows.Forms.Button();
            this.localSize = new System.Windows.Forms.Label();
            this.localDate = new System.Windows.Forms.Label();
            this.localName = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.serverVersion = new System.Windows.Forms.GroupBox();
            this.saveServer = new System.Windows.Forms.Button();
            this.serverSize = new System.Windows.Forms.Label();
            this.serverDate = new System.Windows.Forms.Label();
            this.serverName = new System.Windows.Forms.Label();
            this.label10 = new System.Windows.Forms.Label();
            this.label11 = new System.Windows.Forms.Label();
            this.label12 = new System.Windows.Forms.Label();
            this.close = new System.Windows.Forms.Button();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.versionsPanel = new System.Windows.Forms.Panel();
            this.serverPanel = new System.Windows.Forms.Panel();
            this.localPanel = new System.Windows.Forms.Panel();
            this.nameConflict = new System.Windows.Forms.GroupBox();
            this.resolveName = new System.Windows.Forms.Button();
            this.newName = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.resolveNameLabel = new System.Windows.Forms.Label();
            this.helpProvider1 = new System.Windows.Forms.HelpProvider();
        }
        private void RegisterEventHandlers()
        {
            this.conflictsView.SelectedIndexChanged += new System.EventHandler(this.conflictsView_SelectedIndexChanged);
            this.saveLocal.Click += new System.EventHandler(this.saveLocal_Click);
            this.localName.DoubleClick += new System.EventHandler(this.localName_DoubleClick);
            this.saveServer.Click += new System.EventHandler(this.saveServer_Click);
            this.serverName.DoubleClick += new System.EventHandler(this.serverName_DoubleClick);
            this.close.Click += new System.EventHandler(this.close_Click);
            this.resolveName.Click += new System.EventHandler(this.resolveName_Click);
            this.newName.TextChanged += new System.EventHandler(this.newName_TextChanged);
            this.localName.Paint += new System.Windows.Forms.PaintEventHandler(this.localName_Paint);
            this.serverName.Paint += new System.Windows.Forms.PaintEventHandler(this.localName_Paint);
            this.ifolderPath.Paint += new System.Windows.Forms.PaintEventHandler(this.ifolderPath_Paint);
            this.SizeChanged += new System.EventHandler(this.ConflictResolver_SizeChanged);
            this.Load += new System.EventHandler(this.ConflictResolver_Load);
        }
        protected void PropInitForComponents(System.ComponentModel.ComponentResourceManager resources)
        {
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
            this.label2.AccessibleDescription = resources.GetString("label2.AccessibleDescription");
            this.label2.AccessibleName = resources.GetString("label2.AccessibleName");
            this.label2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label2.Anchor")));
            this.label2.AutoSize = ((bool)(resources.GetObject("label2.AutoSize")));
            this.label2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label2.Dock")));
            this.label2.Enabled = ((bool)(resources.GetObject("label2.Enabled")));
            this.label2.Font = ((System.Drawing.Font)(resources.GetObject("label2.Font")));
            this.helpProvider1.SetHelpKeyword(this.label2, resources.GetString("label2.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.label2, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label2.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.label2, resources.GetString("label2.HelpString"));
            this.label2.Image = ((System.Drawing.Image)(resources.GetObject("label2.Image")));
            this.label2.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label2.ImageAlign")));
            this.label2.ImageIndex = ((int)(resources.GetObject("label2.ImageIndex")));
            this.label2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label2.ImeMode")));
            this.label2.Location = ((System.Drawing.Point)(resources.GetObject("label2.Location")));
            this.label2.Name = "label2";
            this.label2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label2.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.label2, ((bool)(resources.GetObject("label2.ShowHelp"))));
            this.label2.Size = ((System.Drawing.Size)(resources.GetObject("label2.Size")));
            this.label2.TabIndex = ((int)(resources.GetObject("label2.TabIndex")));
            this.label2.Text = resources.GetString("label2.Text");
            this.label2.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label2.TextAlign")));
            this.toolTip1.SetToolTip(this.label2, resources.GetString("label2.ToolTip"));
            this.label2.Visible = ((bool)(resources.GetObject("label2.Visible")));
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
            this.ifolderName.AccessibleDescription = resources.GetString("ifolderName.AccessibleDescription");
            this.ifolderName.AccessibleName = resources.GetString("ifolderName.AccessibleName");
            this.ifolderName.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("ifolderName.Anchor")));
            this.ifolderName.AutoSize = ((bool)(resources.GetObject("ifolderName.AutoSize")));
            this.ifolderName.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("ifolderName.Dock")));
            this.ifolderName.Enabled = ((bool)(resources.GetObject("ifolderName.Enabled")));
            this.ifolderName.Font = ((System.Drawing.Font)(resources.GetObject("ifolderName.Font")));
            this.helpProvider1.SetHelpKeyword(this.ifolderName, resources.GetString("ifolderName.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.ifolderName, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("ifolderName.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.ifolderName, resources.GetString("ifolderName.HelpString"));
            this.ifolderName.Image = ((System.Drawing.Image)(resources.GetObject("ifolderName.Image")));
            this.ifolderName.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ifolderName.ImageAlign")));
            this.ifolderName.ImageIndex = ((int)(resources.GetObject("ifolderName.ImageIndex")));
            this.ifolderName.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("ifolderName.ImeMode")));
            this.ifolderName.Location = ((System.Drawing.Point)(resources.GetObject("ifolderName.Location")));
            this.ifolderName.Name = "ifolderName";
            this.ifolderName.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("ifolderName.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.ifolderName, ((bool)(resources.GetObject("ifolderName.ShowHelp"))));
            this.ifolderName.Size = ((System.Drawing.Size)(resources.GetObject("ifolderName.Size")));
            this.ifolderName.TabIndex = ((int)(resources.GetObject("ifolderName.TabIndex")));
            this.ifolderName.Text = resources.GetString("ifolderName.Text");
            this.ifolderName.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ifolderName.TextAlign")));
            this.toolTip1.SetToolTip(this.ifolderName, resources.GetString("ifolderName.ToolTip"));
            this.ifolderName.Visible = ((bool)(resources.GetObject("ifolderName.Visible")));
            this.ifolderPath.AccessibleDescription = resources.GetString("ifolderPath.AccessibleDescription");
            this.ifolderPath.AccessibleName = resources.GetString("ifolderPath.AccessibleName");
            this.ifolderPath.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("ifolderPath.Anchor")));
            this.ifolderPath.AutoSize = ((bool)(resources.GetObject("ifolderPath.AutoSize")));
            this.ifolderPath.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("ifolderPath.Dock")));
            this.ifolderPath.Enabled = ((bool)(resources.GetObject("ifolderPath.Enabled")));
            this.ifolderPath.Font = ((System.Drawing.Font)(resources.GetObject("ifolderPath.Font")));
            this.helpProvider1.SetHelpKeyword(this.ifolderPath, resources.GetString("ifolderPath.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.ifolderPath, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("ifolderPath.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.ifolderPath, resources.GetString("ifolderPath.HelpString"));
            this.ifolderPath.Image = ((System.Drawing.Image)(resources.GetObject("ifolderPath.Image")));
            this.ifolderPath.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ifolderPath.ImageAlign")));
            this.ifolderPath.ImageIndex = ((int)(resources.GetObject("ifolderPath.ImageIndex")));
            this.ifolderPath.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("ifolderPath.ImeMode")));
            this.ifolderPath.Location = ((System.Drawing.Point)(resources.GetObject("ifolderPath.Location")));
            this.ifolderPath.Name = "ifolderPath";
            this.ifolderPath.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("ifolderPath.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.ifolderPath, ((bool)(resources.GetObject("ifolderPath.ShowHelp"))));
            this.ifolderPath.Size = ((System.Drawing.Size)(resources.GetObject("ifolderPath.Size")));
            this.ifolderPath.TabIndex = ((int)(resources.GetObject("ifolderPath.TabIndex")));
            this.ifolderPath.Text = resources.GetString("ifolderPath.Text");
            this.ifolderPath.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ifolderPath.TextAlign")));
            this.toolTip1.SetToolTip(this.ifolderPath, resources.GetString("ifolderPath.ToolTip"));
            this.ifolderPath.Visible = ((bool)(resources.GetObject("ifolderPath.Visible")));
            this.conflictsView.AccessibleDescription = resources.GetString("conflictsView.AccessibleDescription");
            this.conflictsView.AccessibleName = resources.GetString("conflictsView.AccessibleName");
            this.conflictsView.Alignment = ((System.Windows.Forms.ListViewAlignment)(resources.GetObject("conflictsView.Alignment")));
            this.conflictsView.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("conflictsView.Anchor")));
            this.conflictsView.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("conflictsView.BackgroundImage")));
            this.conflictsView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
                       this.columnHeader1,
                       this.columnHeader2,
                       this.columnHeader3});
            this.conflictsView.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("conflictsView.Dock")));
            this.conflictsView.Enabled = ((bool)(resources.GetObject("conflictsView.Enabled")));
            this.conflictsView.Font = ((System.Drawing.Font)(resources.GetObject("conflictsView.Font")));
            this.conflictsView.FullRowSelect = true;
            this.helpProvider1.SetHelpKeyword(this.conflictsView, resources.GetString("conflictsView.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.conflictsView, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("conflictsView.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.conflictsView, resources.GetString("conflictsView.HelpString"));
            this.conflictsView.HideSelection = false;
            this.conflictsView.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("conflictsView.ImeMode")));
            this.conflictsView.LabelWrap = ((bool)(resources.GetObject("conflictsView.LabelWrap")));
            this.conflictsView.Location = ((System.Drawing.Point)(resources.GetObject("conflictsView.Location")));
            this.conflictsView.Name = "conflictsView";
            this.conflictsView.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("conflictsView.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.conflictsView, ((bool)(resources.GetObject("conflictsView.ShowHelp"))));
            this.conflictsView.Size = ((System.Drawing.Size)(resources.GetObject("conflictsView.Size")));
            this.conflictsView.TabIndex = ((int)(resources.GetObject("conflictsView.TabIndex")));
            this.conflictsView.Text = resources.GetString("conflictsView.Text");
            this.toolTip1.SetToolTip(this.conflictsView, resources.GetString("conflictsView.ToolTip"));
            this.conflictsView.View = System.Windows.Forms.View.Details;
            this.conflictsView.Visible = ((bool)(resources.GetObject("conflictsView.Visible")));
            this.columnHeader1.Text = resources.GetString("columnHeader1.Text");
            this.columnHeader1.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader1.TextAlign")));
            this.columnHeader1.Width = ((int)(resources.GetObject("columnHeader1.Width")));
            this.columnHeader2.Text = resources.GetString("columnHeader2.Text");
            this.columnHeader2.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader2.TextAlign")));
            this.columnHeader2.Width = ((int)(resources.GetObject("columnHeader2.Width")));
            this.columnHeader3.Text = resources.GetString("columnHeader3.Text");
            this.columnHeader3.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("columnHeader3.TextAlign")));
            this.columnHeader3.Width = ((int)(resources.GetObject("columnHeader3.Width")));
            this.localVersion.AccessibleDescription = resources.GetString("localVersion.AccessibleDescription");
            this.localVersion.AccessibleName = resources.GetString("localVersion.AccessibleName");
            this.localVersion.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("localVersion.Anchor")));
            this.localVersion.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("localVersion.BackgroundImage")));
            this.localVersion.Controls.Add(this.saveLocal);
            this.localVersion.Controls.Add(this.localSize);
            this.localVersion.Controls.Add(this.localDate);
            this.localVersion.Controls.Add(this.localName);
            this.localVersion.Controls.Add(this.label6);
            this.localVersion.Controls.Add(this.label5);
            this.localVersion.Controls.Add(this.label4);
            this.localVersion.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("localVersion.Dock")));
            this.localVersion.Enabled = ((bool)(resources.GetObject("localVersion.Enabled")));
            this.localVersion.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.localVersion.Font = ((System.Drawing.Font)(resources.GetObject("localVersion.Font")));
            this.helpProvider1.SetHelpKeyword(this.localVersion, resources.GetString("localVersion.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.localVersion, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("localVersion.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.localVersion, resources.GetString("localVersion.HelpString"));
            this.localVersion.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("localVersion.ImeMode")));
            this.localVersion.Location = ((System.Drawing.Point)(resources.GetObject("localVersion.Location")));
            this.localVersion.Name = "localVersion";
            this.localVersion.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("localVersion.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.localVersion, ((bool)(resources.GetObject("localVersion.ShowHelp"))));
            this.localVersion.Size = ((System.Drawing.Size)(resources.GetObject("localVersion.Size")));
            this.localVersion.TabIndex = ((int)(resources.GetObject("localVersion.TabIndex")));
            this.localVersion.TabStop = false;
            this.localVersion.Text = resources.GetString("localVersion.Text");
            this.toolTip1.SetToolTip(this.localVersion, resources.GetString("localVersion.ToolTip"));
            this.localVersion.Visible = ((bool)(resources.GetObject("localVersion.Visible")));
            this.saveLocal.AccessibleDescription = resources.GetString("saveLocal.AccessibleDescription");
            this.saveLocal.AccessibleName = resources.GetString("saveLocal.AccessibleName");
            this.saveLocal.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("saveLocal.Anchor")));
            this.saveLocal.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("saveLocal.BackgroundImage")));
            this.saveLocal.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("saveLocal.Dock")));
            this.saveLocal.Enabled = ((bool)(resources.GetObject("saveLocal.Enabled")));
            this.saveLocal.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("saveLocal.FlatStyle")));
            this.saveLocal.Font = ((System.Drawing.Font)(resources.GetObject("saveLocal.Font")));
            this.helpProvider1.SetHelpKeyword(this.saveLocal, resources.GetString("saveLocal.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.saveLocal, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("saveLocal.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.saveLocal, resources.GetString("saveLocal.HelpString"));
            this.saveLocal.Image = ((System.Drawing.Image)(resources.GetObject("saveLocal.Image")));
            this.saveLocal.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("saveLocal.ImageAlign")));
            this.saveLocal.ImageIndex = ((int)(resources.GetObject("saveLocal.ImageIndex")));
            this.saveLocal.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("saveLocal.ImeMode")));
            this.saveLocal.Location = ((System.Drawing.Point)(resources.GetObject("saveLocal.Location")));
            this.saveLocal.Name = "saveLocal";
            this.saveLocal.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("saveLocal.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.saveLocal, ((bool)(resources.GetObject("saveLocal.ShowHelp"))));
            this.saveLocal.Size = ((System.Drawing.Size)(resources.GetObject("saveLocal.Size")));
            this.saveLocal.TabIndex = ((int)(resources.GetObject("saveLocal.TabIndex")));
            this.saveLocal.Text = resources.GetString("saveLocal.Text");
            this.saveLocal.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("saveLocal.TextAlign")));
            this.toolTip1.SetToolTip(this.saveLocal, resources.GetString("saveLocal.ToolTip"));
            this.saveLocal.Visible = ((bool)(resources.GetObject("saveLocal.Visible")));
            this.localSize.AccessibleDescription = resources.GetString("localSize.AccessibleDescription");
            this.localSize.AccessibleName = resources.GetString("localSize.AccessibleName");
            this.localSize.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("localSize.Anchor")));
            this.localSize.AutoSize = ((bool)(resources.GetObject("localSize.AutoSize")));
            this.localSize.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("localSize.Dock")));
            this.localSize.Enabled = ((bool)(resources.GetObject("localSize.Enabled")));
            this.localSize.Font = ((System.Drawing.Font)(resources.GetObject("localSize.Font")));
            this.helpProvider1.SetHelpKeyword(this.localSize, resources.GetString("localSize.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.localSize, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("localSize.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.localSize, resources.GetString("localSize.HelpString"));
            this.localSize.Image = ((System.Drawing.Image)(resources.GetObject("localSize.Image")));
            this.localSize.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("localSize.ImageAlign")));
            this.localSize.ImageIndex = ((int)(resources.GetObject("localSize.ImageIndex")));
            this.localSize.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("localSize.ImeMode")));
            this.localSize.Location = ((System.Drawing.Point)(resources.GetObject("localSize.Location")));
            this.localSize.Name = "localSize";
            this.localSize.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("localSize.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.localSize, ((bool)(resources.GetObject("localSize.ShowHelp"))));
            this.localSize.Size = ((System.Drawing.Size)(resources.GetObject("localSize.Size")));
            this.localSize.TabIndex = ((int)(resources.GetObject("localSize.TabIndex")));
            this.localSize.Text = resources.GetString("localSize.Text");
            this.localSize.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("localSize.TextAlign")));
            this.toolTip1.SetToolTip(this.localSize, resources.GetString("localSize.ToolTip"));
            this.localSize.Visible = ((bool)(resources.GetObject("localSize.Visible")));
            this.localDate.AccessibleDescription = resources.GetString("localDate.AccessibleDescription");
            this.localDate.AccessibleName = resources.GetString("localDate.AccessibleName");
            this.localDate.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("localDate.Anchor")));
            this.localDate.AutoSize = ((bool)(resources.GetObject("localDate.AutoSize")));
            this.localDate.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("localDate.Dock")));
            this.localDate.Enabled = ((bool)(resources.GetObject("localDate.Enabled")));
            this.localDate.Font = ((System.Drawing.Font)(resources.GetObject("localDate.Font")));
            this.helpProvider1.SetHelpKeyword(this.localDate, resources.GetString("localDate.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.localDate, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("localDate.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.localDate, resources.GetString("localDate.HelpString"));
            this.localDate.Image = ((System.Drawing.Image)(resources.GetObject("localDate.Image")));
            this.localDate.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("localDate.ImageAlign")));
            this.localDate.ImageIndex = ((int)(resources.GetObject("localDate.ImageIndex")));
            this.localDate.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("localDate.ImeMode")));
            this.localDate.Location = ((System.Drawing.Point)(resources.GetObject("localDate.Location")));
            this.localDate.Name = "localDate";
            this.localDate.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("localDate.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.localDate, ((bool)(resources.GetObject("localDate.ShowHelp"))));
            this.localDate.Size = ((System.Drawing.Size)(resources.GetObject("localDate.Size")));
            this.localDate.TabIndex = ((int)(resources.GetObject("localDate.TabIndex")));
            this.localDate.Text = resources.GetString("localDate.Text");
            this.localDate.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("localDate.TextAlign")));
            this.toolTip1.SetToolTip(this.localDate, resources.GetString("localDate.ToolTip"));
            this.localDate.Visible = ((bool)(resources.GetObject("localDate.Visible")));
            this.localName.AccessibleDescription = resources.GetString("localName.AccessibleDescription");
            this.localName.AccessibleName = resources.GetString("localName.AccessibleName");
            this.localName.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("localName.Anchor")));
            this.localName.AutoSize = ((bool)(resources.GetObject("localName.AutoSize")));
            this.localName.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("localName.Dock")));
            this.localName.Enabled = ((bool)(resources.GetObject("localName.Enabled")));
            this.localName.Font = ((System.Drawing.Font)(resources.GetObject("localName.Font")));
            this.helpProvider1.SetHelpKeyword(this.localName, resources.GetString("localName.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.localName, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("localName.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.localName, resources.GetString("localName.HelpString"));
            this.localName.Image = ((System.Drawing.Image)(resources.GetObject("localName.Image")));
            this.localName.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("localName.ImageAlign")));
            this.localName.ImageIndex = ((int)(resources.GetObject("localName.ImageIndex")));
            this.localName.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("localName.ImeMode")));
            this.localName.Location = ((System.Drawing.Point)(resources.GetObject("localName.Location")));
            this.localName.Name = "localName";
            this.localName.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("localName.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.localName, ((bool)(resources.GetObject("localName.ShowHelp"))));
            this.localName.Size = ((System.Drawing.Size)(resources.GetObject("localName.Size")));
            this.localName.TabIndex = ((int)(resources.GetObject("localName.TabIndex")));
            this.localName.Text = resources.GetString("localName.Text");
            this.localName.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("localName.TextAlign")));
            this.toolTip1.SetToolTip(this.localName, resources.GetString("localName.ToolTip"));
            this.localName.Visible = ((bool)(resources.GetObject("localName.Visible")));
            this.label6.AccessibleDescription = resources.GetString("label6.AccessibleDescription");
            this.label6.AccessibleName = resources.GetString("label6.AccessibleName");
            this.label6.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label6.Anchor")));
            this.label6.AutoSize = ((bool)(resources.GetObject("label6.AutoSize")));
            this.label6.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label6.Dock")));
            this.label6.Enabled = ((bool)(resources.GetObject("label6.Enabled")));
            this.label6.Font = ((System.Drawing.Font)(resources.GetObject("label6.Font")));
            this.helpProvider1.SetHelpKeyword(this.label6, resources.GetString("label6.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.label6, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label6.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.label6, resources.GetString("label6.HelpString"));
            this.label6.Image = ((System.Drawing.Image)(resources.GetObject("label6.Image")));
            this.label6.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label6.ImageAlign")));
            this.label6.ImageIndex = ((int)(resources.GetObject("label6.ImageIndex")));
            this.label6.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label6.ImeMode")));
            this.label6.Location = ((System.Drawing.Point)(resources.GetObject("label6.Location")));
            this.label6.Name = "label6";
            this.label6.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label6.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.label6, ((bool)(resources.GetObject("label6.ShowHelp"))));
            this.label6.Size = ((System.Drawing.Size)(resources.GetObject("label6.Size")));
            this.label6.TabIndex = ((int)(resources.GetObject("label6.TabIndex")));
            this.label6.Text = resources.GetString("label6.Text");
            this.label6.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label6.TextAlign")));
            this.toolTip1.SetToolTip(this.label6, resources.GetString("label6.ToolTip"));
            this.label6.Visible = ((bool)(resources.GetObject("label6.Visible")));
            this.label5.AccessibleDescription = resources.GetString("label5.AccessibleDescription");
            this.label5.AccessibleName = resources.GetString("label5.AccessibleName");
            this.label5.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label5.Anchor")));
            this.label5.AutoSize = ((bool)(resources.GetObject("label5.AutoSize")));
            this.label5.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label5.Dock")));
            this.label5.Enabled = ((bool)(resources.GetObject("label5.Enabled")));
            this.label5.Font = ((System.Drawing.Font)(resources.GetObject("label5.Font")));
            this.helpProvider1.SetHelpKeyword(this.label5, resources.GetString("label5.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.label5, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label5.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.label5, resources.GetString("label5.HelpString"));
            this.label5.Image = ((System.Drawing.Image)(resources.GetObject("label5.Image")));
            this.label5.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label5.ImageAlign")));
            this.label5.ImageIndex = ((int)(resources.GetObject("label5.ImageIndex")));
            this.label5.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label5.ImeMode")));
            this.label5.Location = ((System.Drawing.Point)(resources.GetObject("label5.Location")));
            this.label5.Name = "label5";
            this.label5.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label5.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.label5, ((bool)(resources.GetObject("label5.ShowHelp"))));
            this.label5.Size = ((System.Drawing.Size)(resources.GetObject("label5.Size")));
            this.label5.TabIndex = ((int)(resources.GetObject("label5.TabIndex")));
            this.label5.Text = resources.GetString("label5.Text");
            this.label5.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label5.TextAlign")));
            this.toolTip1.SetToolTip(this.label5, resources.GetString("label5.ToolTip"));
            this.label5.Visible = ((bool)(resources.GetObject("label5.Visible")));
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
            this.serverVersion.AccessibleDescription = resources.GetString("serverVersion.AccessibleDescription");
            this.serverVersion.AccessibleName = resources.GetString("serverVersion.AccessibleName");
            this.serverVersion.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("serverVersion.Anchor")));
            this.serverVersion.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("serverVersion.BackgroundImage")));
            this.serverVersion.Controls.Add(this.saveServer);
            this.serverVersion.Controls.Add(this.serverSize);
            this.serverVersion.Controls.Add(this.serverDate);
            this.serverVersion.Controls.Add(this.serverName);
            this.serverVersion.Controls.Add(this.label10);
            this.serverVersion.Controls.Add(this.label11);
            this.serverVersion.Controls.Add(this.label12);
            this.serverVersion.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("serverVersion.Dock")));
            this.serverVersion.Enabled = ((bool)(resources.GetObject("serverVersion.Enabled")));
            this.serverVersion.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.serverVersion.Font = ((System.Drawing.Font)(resources.GetObject("serverVersion.Font")));
            this.helpProvider1.SetHelpKeyword(this.serverVersion, resources.GetString("serverVersion.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.serverVersion, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("serverVersion.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.serverVersion, resources.GetString("serverVersion.HelpString"));
            this.serverVersion.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("serverVersion.ImeMode")));
            this.serverVersion.Location = ((System.Drawing.Point)(resources.GetObject("serverVersion.Location")));
            this.serverVersion.Name = "serverVersion";
            this.serverVersion.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("serverVersion.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.serverVersion, ((bool)(resources.GetObject("serverVersion.ShowHelp"))));
            this.serverVersion.Size = ((System.Drawing.Size)(resources.GetObject("serverVersion.Size")));
            this.serverVersion.TabIndex = ((int)(resources.GetObject("serverVersion.TabIndex")));
            this.serverVersion.TabStop = false;
            this.serverVersion.Text = resources.GetString("serverVersion.Text");
            this.toolTip1.SetToolTip(this.serverVersion, resources.GetString("serverVersion.ToolTip"));
            this.serverVersion.Visible = ((bool)(resources.GetObject("serverVersion.Visible")));
            this.saveServer.AccessibleDescription = resources.GetString("saveServer.AccessibleDescription");
            this.saveServer.AccessibleName = resources.GetString("saveServer.AccessibleName");
            this.saveServer.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("saveServer.Anchor")));
            this.saveServer.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("saveServer.BackgroundImage")));
            this.saveServer.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("saveServer.Dock")));
            this.saveServer.Enabled = ((bool)(resources.GetObject("saveServer.Enabled")));
            this.saveServer.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("saveServer.FlatStyle")));
            this.saveServer.Font = ((System.Drawing.Font)(resources.GetObject("saveServer.Font")));
            this.helpProvider1.SetHelpKeyword(this.saveServer, resources.GetString("saveServer.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.saveServer, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("saveServer.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.saveServer, resources.GetString("saveServer.HelpString"));
            this.saveServer.Image = ((System.Drawing.Image)(resources.GetObject("saveServer.Image")));
            this.saveServer.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("saveServer.ImageAlign")));
            this.saveServer.ImageIndex = ((int)(resources.GetObject("saveServer.ImageIndex")));
            this.saveServer.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("saveServer.ImeMode")));
            this.saveServer.Location = ((System.Drawing.Point)(resources.GetObject("saveServer.Location")));
            this.saveServer.Name = "saveServer";
            this.saveServer.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("saveServer.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.saveServer, ((bool)(resources.GetObject("saveServer.ShowHelp"))));
            this.saveServer.Size = ((System.Drawing.Size)(resources.GetObject("saveServer.Size")));
            this.saveServer.TabIndex = ((int)(resources.GetObject("saveServer.TabIndex")));
            this.saveServer.Text = resources.GetString("saveServer.Text");
            this.saveServer.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("saveServer.TextAlign")));
            this.toolTip1.SetToolTip(this.saveServer, resources.GetString("saveServer.ToolTip"));
            this.saveServer.Visible = ((bool)(resources.GetObject("saveServer.Visible")));
            this.serverSize.AccessibleDescription = resources.GetString("serverSize.AccessibleDescription");
            this.serverSize.AccessibleName = resources.GetString("serverSize.AccessibleName");
            this.serverSize.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("serverSize.Anchor")));
            this.serverSize.AutoSize = ((bool)(resources.GetObject("serverSize.AutoSize")));
            this.serverSize.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("serverSize.Dock")));
            this.serverSize.Enabled = ((bool)(resources.GetObject("serverSize.Enabled")));
            this.serverSize.Font = ((System.Drawing.Font)(resources.GetObject("serverSize.Font")));
            this.helpProvider1.SetHelpKeyword(this.serverSize, resources.GetString("serverSize.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.serverSize, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("serverSize.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.serverSize, resources.GetString("serverSize.HelpString"));
            this.serverSize.Image = ((System.Drawing.Image)(resources.GetObject("serverSize.Image")));
            this.serverSize.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverSize.ImageAlign")));
            this.serverSize.ImageIndex = ((int)(resources.GetObject("serverSize.ImageIndex")));
            this.serverSize.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("serverSize.ImeMode")));
            this.serverSize.Location = ((System.Drawing.Point)(resources.GetObject("serverSize.Location")));
            this.serverSize.Name = "serverSize";
            this.serverSize.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("serverSize.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.serverSize, ((bool)(resources.GetObject("serverSize.ShowHelp"))));
            this.serverSize.Size = ((System.Drawing.Size)(resources.GetObject("serverSize.Size")));
            this.serverSize.TabIndex = ((int)(resources.GetObject("serverSize.TabIndex")));
            this.serverSize.Text = resources.GetString("serverSize.Text");
            this.serverSize.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverSize.TextAlign")));
            this.toolTip1.SetToolTip(this.serverSize, resources.GetString("serverSize.ToolTip"));
            this.serverSize.Visible = ((bool)(resources.GetObject("serverSize.Visible")));
            this.serverDate.AccessibleDescription = resources.GetString("serverDate.AccessibleDescription");
            this.serverDate.AccessibleName = resources.GetString("serverDate.AccessibleName");
            this.serverDate.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("serverDate.Anchor")));
            this.serverDate.AutoSize = ((bool)(resources.GetObject("serverDate.AutoSize")));
            this.serverDate.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("serverDate.Dock")));
            this.serverDate.Enabled = ((bool)(resources.GetObject("serverDate.Enabled")));
            this.serverDate.Font = ((System.Drawing.Font)(resources.GetObject("serverDate.Font")));
            this.helpProvider1.SetHelpKeyword(this.serverDate, resources.GetString("serverDate.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.serverDate, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("serverDate.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.serverDate, resources.GetString("serverDate.HelpString"));
            this.serverDate.Image = ((System.Drawing.Image)(resources.GetObject("serverDate.Image")));
            this.serverDate.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverDate.ImageAlign")));
            this.serverDate.ImageIndex = ((int)(resources.GetObject("serverDate.ImageIndex")));
            this.serverDate.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("serverDate.ImeMode")));
            this.serverDate.Location = ((System.Drawing.Point)(resources.GetObject("serverDate.Location")));
            this.serverDate.Name = "serverDate";
            this.serverDate.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("serverDate.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.serverDate, ((bool)(resources.GetObject("serverDate.ShowHelp"))));
            this.serverDate.Size = ((System.Drawing.Size)(resources.GetObject("serverDate.Size")));
            this.serverDate.TabIndex = ((int)(resources.GetObject("serverDate.TabIndex")));
            this.serverDate.Text = resources.GetString("serverDate.Text");
            this.serverDate.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverDate.TextAlign")));
            this.toolTip1.SetToolTip(this.serverDate, resources.GetString("serverDate.ToolTip"));
            this.serverDate.Visible = ((bool)(resources.GetObject("serverDate.Visible")));
            this.serverName.AccessibleDescription = resources.GetString("serverName.AccessibleDescription");
            this.serverName.AccessibleName = resources.GetString("serverName.AccessibleName");
            this.serverName.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("serverName.Anchor")));
            this.serverName.AutoSize = ((bool)(resources.GetObject("serverName.AutoSize")));
            this.serverName.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("serverName.Dock")));
            this.serverName.Enabled = ((bool)(resources.GetObject("serverName.Enabled")));
            this.serverName.Font = ((System.Drawing.Font)(resources.GetObject("serverName.Font")));
            this.helpProvider1.SetHelpKeyword(this.serverName, resources.GetString("serverName.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.serverName, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("serverName.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.serverName, resources.GetString("serverName.HelpString"));
            this.serverName.Image = ((System.Drawing.Image)(resources.GetObject("serverName.Image")));
            this.serverName.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverName.ImageAlign")));
            this.serverName.ImageIndex = ((int)(resources.GetObject("serverName.ImageIndex")));
            this.serverName.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("serverName.ImeMode")));
            this.serverName.Location = ((System.Drawing.Point)(resources.GetObject("serverName.Location")));
            this.serverName.Name = "serverName";
            this.serverName.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("serverName.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.serverName, ((bool)(resources.GetObject("serverName.ShowHelp"))));
            this.serverName.Size = ((System.Drawing.Size)(resources.GetObject("serverName.Size")));
            this.serverName.TabIndex = ((int)(resources.GetObject("serverName.TabIndex")));
            this.serverName.Text = resources.GetString("serverName.Text");
            this.serverName.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverName.TextAlign")));
            this.toolTip1.SetToolTip(this.serverName, resources.GetString("serverName.ToolTip"));
            this.serverName.Visible = ((bool)(resources.GetObject("serverName.Visible")));
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
            this.label11.AccessibleDescription = resources.GetString("label11.AccessibleDescription");
            this.label11.AccessibleName = resources.GetString("label11.AccessibleName");
            this.label11.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label11.Anchor")));
            this.label11.AutoSize = ((bool)(resources.GetObject("label11.AutoSize")));
            this.label11.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label11.Dock")));
            this.label11.Enabled = ((bool)(resources.GetObject("label11.Enabled")));
            this.label11.Font = ((System.Drawing.Font)(resources.GetObject("label11.Font")));
            this.helpProvider1.SetHelpKeyword(this.label11, resources.GetString("label11.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.label11, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label11.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.label11, resources.GetString("label11.HelpString"));
            this.label11.Image = ((System.Drawing.Image)(resources.GetObject("label11.Image")));
            this.label11.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label11.ImageAlign")));
            this.label11.ImageIndex = ((int)(resources.GetObject("label11.ImageIndex")));
            this.label11.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label11.ImeMode")));
            this.label11.Location = ((System.Drawing.Point)(resources.GetObject("label11.Location")));
            this.label11.Name = "label11";
            this.label11.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label11.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.label11, ((bool)(resources.GetObject("label11.ShowHelp"))));
            this.label11.Size = ((System.Drawing.Size)(resources.GetObject("label11.Size")));
            this.label11.TabIndex = ((int)(resources.GetObject("label11.TabIndex")));
            this.label11.Text = resources.GetString("label11.Text");
            this.label11.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label11.TextAlign")));
            this.toolTip1.SetToolTip(this.label11, resources.GetString("label11.ToolTip"));
            this.label11.Visible = ((bool)(resources.GetObject("label11.Visible")));
            this.label12.AccessibleDescription = resources.GetString("label12.AccessibleDescription");
            this.label12.AccessibleName = resources.GetString("label12.AccessibleName");
            this.label12.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label12.Anchor")));
            this.label12.AutoSize = ((bool)(resources.GetObject("label12.AutoSize")));
            this.label12.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label12.Dock")));
            this.label12.Enabled = ((bool)(resources.GetObject("label12.Enabled")));
            this.label12.Font = ((System.Drawing.Font)(resources.GetObject("label12.Font")));
            this.helpProvider1.SetHelpKeyword(this.label12, resources.GetString("label12.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.label12, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("label12.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.label12, resources.GetString("label12.HelpString"));
            this.label12.Image = ((System.Drawing.Image)(resources.GetObject("label12.Image")));
            this.label12.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label12.ImageAlign")));
            this.label12.ImageIndex = ((int)(resources.GetObject("label12.ImageIndex")));
            this.label12.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label12.ImeMode")));
            this.label12.Location = ((System.Drawing.Point)(resources.GetObject("label12.Location")));
            this.label12.Name = "label12";
            this.label12.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label12.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.label12, ((bool)(resources.GetObject("label12.ShowHelp"))));
            this.label12.Size = ((System.Drawing.Size)(resources.GetObject("label12.Size")));
            this.label12.TabIndex = ((int)(resources.GetObject("label12.TabIndex")));
            this.label12.Text = resources.GetString("label12.Text");
            this.label12.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label12.TextAlign")));
            this.toolTip1.SetToolTip(this.label12, resources.GetString("label12.ToolTip"));
            this.label12.Visible = ((bool)(resources.GetObject("label12.Visible")));
            this.close.AccessibleDescription = resources.GetString("close.AccessibleDescription");
            this.close.AccessibleName = resources.GetString("close.AccessibleName");
            this.close.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("close.Anchor")));
            this.close.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("close.BackgroundImage")));
            this.close.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("close.Dock")));
            this.close.Enabled = ((bool)(resources.GetObject("close.Enabled")));
            this.close.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("close.FlatStyle")));
            this.close.Font = ((System.Drawing.Font)(resources.GetObject("close.Font")));
            this.helpProvider1.SetHelpKeyword(this.close, resources.GetString("close.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.close, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("close.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.close, resources.GetString("close.HelpString"));
            this.close.Image = ((System.Drawing.Image)(resources.GetObject("close.Image")));
            this.close.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("close.ImageAlign")));
            this.close.ImageIndex = ((int)(resources.GetObject("close.ImageIndex")));
            this.close.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("close.ImeMode")));
            this.close.Location = ((System.Drawing.Point)(resources.GetObject("close.Location")));
            this.close.Name = "close";
            this.close.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("close.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.close, ((bool)(resources.GetObject("close.ShowHelp"))));
            this.close.Size = ((System.Drawing.Size)(resources.GetObject("close.Size")));
            this.close.TabIndex = ((int)(resources.GetObject("close.TabIndex")));
            this.close.Text = resources.GetString("close.Text");
            this.close.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("close.TextAlign")));
            this.toolTip1.SetToolTip(this.close, resources.GetString("close.ToolTip"));
            this.close.Visible = ((bool)(resources.GetObject("close.Visible")));
            this.versionsPanel.AccessibleDescription = resources.GetString("versionsPanel.AccessibleDescription");
            this.versionsPanel.AccessibleName = resources.GetString("versionsPanel.AccessibleName");
            this.versionsPanel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("versionsPanel.Anchor")));
            this.versionsPanel.AutoScroll = ((bool)(resources.GetObject("versionsPanel.AutoScroll")));
            this.versionsPanel.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("versionsPanel.AutoScrollMargin")));
            this.versionsPanel.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("versionsPanel.AutoScrollMinSize")));
            this.versionsPanel.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("versionsPanel.BackgroundImage")));
            this.versionsPanel.Controls.Add(this.serverPanel);
            this.versionsPanel.Controls.Add(this.localPanel);
            this.versionsPanel.Controls.Add(this.nameConflict);
            this.versionsPanel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("versionsPanel.Dock")));
            this.versionsPanel.Enabled = ((bool)(resources.GetObject("versionsPanel.Enabled")));
            this.versionsPanel.Font = ((System.Drawing.Font)(resources.GetObject("versionsPanel.Font")));
            this.helpProvider1.SetHelpKeyword(this.versionsPanel, resources.GetString("versionsPanel.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.versionsPanel, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("versionsPanel.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.versionsPanel, resources.GetString("versionsPanel.HelpString"));
            this.versionsPanel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("versionsPanel.ImeMode")));
            this.versionsPanel.Location = ((System.Drawing.Point)(resources.GetObject("versionsPanel.Location")));
            this.versionsPanel.Name = "versionsPanel";
            this.versionsPanel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("versionsPanel.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.versionsPanel, ((bool)(resources.GetObject("versionsPanel.ShowHelp"))));
            this.versionsPanel.Size = ((System.Drawing.Size)(resources.GetObject("versionsPanel.Size")));
            this.versionsPanel.TabIndex = ((int)(resources.GetObject("versionsPanel.TabIndex")));
            this.versionsPanel.Text = resources.GetString("versionsPanel.Text");
            this.toolTip1.SetToolTip(this.versionsPanel, resources.GetString("versionsPanel.ToolTip"));
            this.versionsPanel.Visible = ((bool)(resources.GetObject("versionsPanel.Visible")));
            this.serverPanel.AccessibleDescription = resources.GetString("serverPanel.AccessibleDescription");
            this.serverPanel.AccessibleName = resources.GetString("serverPanel.AccessibleName");
            this.serverPanel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("serverPanel.Anchor")));
            this.serverPanel.AutoScroll = ((bool)(resources.GetObject("serverPanel.AutoScroll")));
            this.serverPanel.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("serverPanel.AutoScrollMargin")));
            this.serverPanel.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("serverPanel.AutoScrollMinSize")));
            this.serverPanel.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("serverPanel.BackgroundImage")));
            this.serverPanel.Controls.Add(this.serverVersion);
            this.serverPanel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("serverPanel.Dock")));
            this.serverPanel.Enabled = ((bool)(resources.GetObject("serverPanel.Enabled")));
            this.serverPanel.Font = ((System.Drawing.Font)(resources.GetObject("serverPanel.Font")));
            this.helpProvider1.SetHelpKeyword(this.serverPanel, resources.GetString("serverPanel.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.serverPanel, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("serverPanel.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.serverPanel, resources.GetString("serverPanel.HelpString"));
            this.serverPanel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("serverPanel.ImeMode")));
            this.serverPanel.Location = ((System.Drawing.Point)(resources.GetObject("serverPanel.Location")));
            this.serverPanel.Name = "serverPanel";
            this.serverPanel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("serverPanel.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.serverPanel, ((bool)(resources.GetObject("serverPanel.ShowHelp"))));
            this.serverPanel.Size = ((System.Drawing.Size)(resources.GetObject("serverPanel.Size")));
            this.serverPanel.TabIndex = ((int)(resources.GetObject("serverPanel.TabIndex")));
            this.serverPanel.Text = resources.GetString("serverPanel.Text");
            this.toolTip1.SetToolTip(this.serverPanel, resources.GetString("serverPanel.ToolTip"));
            this.serverPanel.Visible = ((bool)(resources.GetObject("serverPanel.Visible")));
            this.localPanel.AccessibleDescription = resources.GetString("localPanel.AccessibleDescription");
            this.localPanel.AccessibleName = resources.GetString("localPanel.AccessibleName");
            this.localPanel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("localPanel.Anchor")));
            this.localPanel.AutoScroll = ((bool)(resources.GetObject("localPanel.AutoScroll")));
            this.localPanel.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("localPanel.AutoScrollMargin")));
            this.localPanel.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("localPanel.AutoScrollMinSize")));
            this.localPanel.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("localPanel.BackgroundImage")));
            this.localPanel.Controls.Add(this.localVersion);
            this.localPanel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("localPanel.Dock")));
            this.localPanel.Enabled = ((bool)(resources.GetObject("localPanel.Enabled")));
            this.localPanel.Font = ((System.Drawing.Font)(resources.GetObject("localPanel.Font")));
            this.helpProvider1.SetHelpKeyword(this.localPanel, resources.GetString("localPanel.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.localPanel, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("localPanel.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.localPanel, resources.GetString("localPanel.HelpString"));
            this.localPanel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("localPanel.ImeMode")));
            this.localPanel.Location = ((System.Drawing.Point)(resources.GetObject("localPanel.Location")));
            this.localPanel.Name = "localPanel";
            this.localPanel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("localPanel.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.localPanel, ((bool)(resources.GetObject("localPanel.ShowHelp"))));
            this.localPanel.Size = ((System.Drawing.Size)(resources.GetObject("localPanel.Size")));
            this.localPanel.TabIndex = ((int)(resources.GetObject("localPanel.TabIndex")));
            this.localPanel.Text = resources.GetString("localPanel.Text");
            this.toolTip1.SetToolTip(this.localPanel, resources.GetString("localPanel.ToolTip"));
            this.localPanel.Visible = ((bool)(resources.GetObject("localPanel.Visible")));
            this.nameConflict.AccessibleDescription = resources.GetString("nameConflict.AccessibleDescription");
            this.nameConflict.AccessibleName = resources.GetString("nameConflict.AccessibleName");
            this.nameConflict.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("nameConflict.Anchor")));
            this.nameConflict.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("nameConflict.BackgroundImage")));
            this.nameConflict.Controls.Add(this.resolveName);
            this.nameConflict.Controls.Add(this.newName);
            this.nameConflict.Controls.Add(this.label8);
            this.nameConflict.Controls.Add(this.resolveNameLabel);
            this.nameConflict.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("nameConflict.Dock")));
            this.nameConflict.Enabled = ((bool)(resources.GetObject("nameConflict.Enabled")));
            this.nameConflict.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.nameConflict.Font = ((System.Drawing.Font)(resources.GetObject("nameConflict.Font")));
            this.helpProvider1.SetHelpKeyword(this.nameConflict, resources.GetString("nameConflict.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.nameConflict, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("nameConflict.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.nameConflict, resources.GetString("nameConflict.HelpString"));
            this.nameConflict.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("nameConflict.ImeMode")));
            this.nameConflict.Location = ((System.Drawing.Point)(resources.GetObject("nameConflict.Location")));
            this.nameConflict.Name = "nameConflict";
            this.nameConflict.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("nameConflict.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.nameConflict, ((bool)(resources.GetObject("nameConflict.ShowHelp"))));
            this.nameConflict.Size = ((System.Drawing.Size)(resources.GetObject("nameConflict.Size")));
            this.nameConflict.TabIndex = ((int)(resources.GetObject("nameConflict.TabIndex")));
            this.nameConflict.TabStop = false;
            this.nameConflict.Text = resources.GetString("nameConflict.Text");
            this.toolTip1.SetToolTip(this.nameConflict, resources.GetString("nameConflict.ToolTip"));
            this.nameConflict.Visible = ((bool)(resources.GetObject("nameConflict.Visible")));
            this.resolveName.AccessibleDescription = resources.GetString("resolveName.AccessibleDescription");
            this.resolveName.AccessibleName = resources.GetString("resolveName.AccessibleName");
            this.resolveName.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("resolveName.Anchor")));
            this.resolveName.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("resolveName.BackgroundImage")));
            this.resolveName.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("resolveName.Dock")));
            this.resolveName.Enabled = ((bool)(resources.GetObject("resolveName.Enabled")));
            this.resolveName.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("resolveName.FlatStyle")));
            this.resolveName.Font = ((System.Drawing.Font)(resources.GetObject("resolveName.Font")));
            this.helpProvider1.SetHelpKeyword(this.resolveName, resources.GetString("resolveName.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.resolveName, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("resolveName.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.resolveName, resources.GetString("resolveName.HelpString"));
            this.resolveName.Image = ((System.Drawing.Image)(resources.GetObject("resolveName.Image")));
            this.resolveName.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("resolveName.ImageAlign")));
            this.resolveName.ImageIndex = ((int)(resources.GetObject("resolveName.ImageIndex")));
            this.resolveName.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("resolveName.ImeMode")));
            this.resolveName.Location = ((System.Drawing.Point)(resources.GetObject("resolveName.Location")));
            this.resolveName.Name = "resolveName";
            this.resolveName.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("resolveName.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.resolveName, ((bool)(resources.GetObject("resolveName.ShowHelp"))));
            this.resolveName.Size = ((System.Drawing.Size)(resources.GetObject("resolveName.Size")));
            this.resolveName.TabIndex = ((int)(resources.GetObject("resolveName.TabIndex")));
            this.resolveName.Text = resources.GetString("resolveName.Text");
            this.resolveName.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("resolveName.TextAlign")));
            this.toolTip1.SetToolTip(this.resolveName, resources.GetString("resolveName.ToolTip"));
            this.resolveName.Visible = ((bool)(resources.GetObject("resolveName.Visible")));
            this.newName.AccessibleDescription = resources.GetString("newName.AccessibleDescription");
            this.newName.AccessibleName = resources.GetString("newName.AccessibleName");
            this.newName.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("newName.Anchor")));
            this.newName.AutoSize = ((bool)(resources.GetObject("newName.AutoSize")));
            this.newName.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("newName.BackgroundImage")));
            this.newName.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("newName.Dock")));
            this.newName.Enabled = ((bool)(resources.GetObject("newName.Enabled")));
            this.newName.Font = ((System.Drawing.Font)(resources.GetObject("newName.Font")));
            this.helpProvider1.SetHelpKeyword(this.newName, resources.GetString("newName.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.newName, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("newName.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.newName, resources.GetString("newName.HelpString"));
            this.newName.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("newName.ImeMode")));
            this.newName.Location = ((System.Drawing.Point)(resources.GetObject("newName.Location")));
            this.newName.MaxLength = ((int)(resources.GetObject("newName.MaxLength")));
            this.newName.Multiline = ((bool)(resources.GetObject("newName.Multiline")));
            this.newName.Name = "newName";
            this.newName.PasswordChar = ((char)(resources.GetObject("newName.PasswordChar")));
            this.newName.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("newName.RightToLeft")));
            this.newName.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("newName.ScrollBars")));
            this.helpProvider1.SetShowHelp(this.newName, ((bool)(resources.GetObject("newName.ShowHelp"))));
            this.newName.Size = ((System.Drawing.Size)(resources.GetObject("newName.Size")));
            this.newName.TabIndex = ((int)(resources.GetObject("newName.TabIndex")));
            this.newName.Text = resources.GetString("newName.Text");
            this.newName.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("newName.TextAlign")));
            this.toolTip1.SetToolTip(this.newName, resources.GetString("newName.ToolTip"));
            this.newName.Visible = ((bool)(resources.GetObject("newName.Visible")));
            this.newName.WordWrap = ((bool)(resources.GetObject("newName.WordWrap")));
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
            this.resolveNameLabel.AccessibleDescription = resources.GetString("resolveNameLabel.AccessibleDescription");
            this.resolveNameLabel.AccessibleName = resources.GetString("resolveNameLabel.AccessibleName");
            this.resolveNameLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("resolveNameLabel.Anchor")));
            this.resolveNameLabel.AutoSize = ((bool)(resources.GetObject("resolveNameLabel.AutoSize")));
            this.resolveNameLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("resolveNameLabel.Dock")));
            this.resolveNameLabel.Enabled = ((bool)(resources.GetObject("resolveNameLabel.Enabled")));
            this.resolveNameLabel.Font = ((System.Drawing.Font)(resources.GetObject("resolveNameLabel.Font")));
            this.helpProvider1.SetHelpKeyword(this.resolveNameLabel, resources.GetString("resolveNameLabel.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this.resolveNameLabel, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("resolveNameLabel.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this.resolveNameLabel, resources.GetString("resolveNameLabel.HelpString"));
            this.resolveNameLabel.Image = ((System.Drawing.Image)(resources.GetObject("resolveNameLabel.Image")));
            this.resolveNameLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("resolveNameLabel.ImageAlign")));
            this.resolveNameLabel.ImageIndex = ((int)(resources.GetObject("resolveNameLabel.ImageIndex")));
            this.resolveNameLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("resolveNameLabel.ImeMode")));
            this.resolveNameLabel.Location = ((System.Drawing.Point)(resources.GetObject("resolveNameLabel.Location")));
            this.resolveNameLabel.Name = "resolveNameLabel";
            this.resolveNameLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("resolveNameLabel.RightToLeft")));
            this.helpProvider1.SetShowHelp(this.resolveNameLabel, ((bool)(resources.GetObject("resolveNameLabel.ShowHelp"))));
            this.resolveNameLabel.Size = ((System.Drawing.Size)(resources.GetObject("resolveNameLabel.Size")));
            this.resolveNameLabel.TabIndex = ((int)(resources.GetObject("resolveNameLabel.TabIndex")));
            this.resolveNameLabel.Text = resources.GetString("resolveNameLabel.Text");
            this.resolveNameLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("resolveNameLabel.TextAlign")));
            this.toolTip1.SetToolTip(this.resolveNameLabel, resources.GetString("resolveNameLabel.ToolTip"));
            this.resolveNameLabel.Visible = ((bool)(resources.GetObject("resolveNameLabel.Visible")));
            this.helpProvider1.HelpNamespace = resources.GetString("helpProvider1.HelpNamespace");
            this.helpProvider1.SetHelpKeyword(this, resources.GetString("$this.HelpKeyword"));
            this.helpProvider1.SetHelpNavigator(this, ((System.Windows.Forms.HelpNavigator)(resources.GetObject("$this.HelpNavigator"))));
            this.helpProvider1.SetHelpString(this, resources.GetString("$this.HelpString"));
            this.helpProvider1.SetShowHelp(this, ((bool)(resources.GetObject("$this.ShowHelp"))));
        }
        protected void SuspendLayoutForComponents()
        {
            this.localVersion.SuspendLayout();
            this.serverVersion.SuspendLayout();
            this.versionsPanel.SuspendLayout();
            this.serverPanel.SuspendLayout();
            this.localPanel.SuspendLayout();
            this.nameConflict.SuspendLayout();
        }
        protected void ResumeLayoutForComponents()
        {
            this.localVersion.ResumeLayout(false);
            this.serverVersion.ResumeLayout(false);
            this.versionsPanel.ResumeLayout(false);
            this.serverPanel.ResumeLayout(false);
            this.localPanel.ResumeLayout(false);
            this.nameConflict.ResumeLayout(false);
        }
  private void InitializeComponent()
  {
            AllocateMemForComponents();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ConflictResolver));
            SuspendLayoutForComponents();
   this.SuspendLayout();
            PropInitForComponents(resources);
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.ifolderPath);
   this.Controls.Add(this.ifolderName);
   this.Controls.Add(this.versionsPanel);
   this.Controls.Add(this.close);
   this.Controls.Add(this.conflictsView);
   this.Controls.Add(this.label3);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.label1);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.HelpButton = true;
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimizeBox = false;
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "ConflictResolver";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.toolTip1.SetToolTip(this, resources.GetString("$this.ToolTip"));
            RegisterEventHandlers();
            ResumeLayoutForComponents();
   this.ResumeLayout(false);
  }
  private void formatLabelString(Label label, string name)
  {
   Graphics g = label.CreateGraphics();
   try
   {
    SizeF stringSize = g.MeasureString(name, label.Font);
    if (stringSize.Width > label.Width)
    {
     int length = (int)(label.Width * name.Length / stringSize.Width);
     string tmp = String.Empty;
     while (stringSize.Width > label.Width)
     {
      tmp = name.Substring(0, length) + "...";
      stringSize = g.MeasureString(tmp, label.Font);
      length -= 1;
     }
     label.Text = tmp;
     toolTip1.SetToolTip(label, name);
    }
    else
    {
     label.Text = name;
     toolTip1.SetToolTip(label, string.Empty);
    }
   }
   finally
   {
    g.Dispose();
   }
  }
  protected void resolveConflicts(bool localWins)
  {
   foreach (ListViewItem lvi in conflictsView.SelectedItems)
   {
    Conflicts conflicts = (Conflicts)lvi.Tag;
    if (!conflicts.ServerConflict.IsNameConflict)
    {
     try
     {
      ifWebService.ResolveFileConflict(ifolder.ID, conflicts.ServerConflict.ConflictID, localWins);
      lvi.Remove();
     }
     catch (Exception ex)
     {
      MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("conflictResolveError"), resourceManager.GetString("conflictErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
      mmb.ShowDialog();
     }
    }
   }
   if (conflictsView.Items.Count == 0 && ConflictsResolved != null)
   {
    ConflictsResolved(this, new EventArgs());
   }
  }
  protected void resolveNameConflict()
  {
   if (conflictsView.SelectedItems.Count == 1)
   {
    ListViewItem lvi = conflictsView.SelectedItems[0];
    Conflicts conflicts = (Conflicts)lvi.Tag;
    bool fileValid = false;
    try
    {
     string path = Path.Combine(ifolder.UnManagedPath, newName.Text);
     FileInfo fi = new FileInfo(path);
     if (fi.Exists && conflicts.LocalConflict != null)
     {
      MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("fileExists"), resourceManager.GetString("errorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
      mmb.ShowDialog();
     }
     else
     {
      fileValid = true;
     }
                    if (conflicts.ServerConflict != null && newName.Text != conflicts.ServerConflict.ServerName)
     {
      fileValid = false;
      MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("fileExists"), resourceManager.GetString("errorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
      mmb.ShowDialog();
     }
    }
    catch (Exception ex)
    {
     MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("badFileName"), resourceManager.GetString("errorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
     mmb.ShowDialog();
    }
    if (fileValid)
    {
     if (conflicts.ServerConflict != null)
     {
     try
      {
       if (readOnly)
       {
        ifWebService.RenameAndResolveConflict(ifolder.ID, conflicts.ServerConflict.ConflictID, newName.Text);
       }
       else
       {
        ifWebService.ResolveNameConflict(ifolder.ID, conflicts.ServerConflict.ConflictID, newName.Text);
       }
       lvi.Remove();
      }
      catch (Exception ex)
      {
       MyMessageBox mmb;
       if (ex.Message.IndexOf("Malformed") != -1)
       {
        mmb = new MyMessageBox(resourceManager.GetString("badFileName"), resourceManager.GetString("errorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
        mmb.ShowDialog();
       }
       else
       {
        mmb = new MyMessageBox(resourceManager.GetString("conflictResolveError"), resourceManager.GetString("conflictErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
        mmb.ShowDialog();
       }
      }
     }
     else
     {
      try
      {
                            Conflict[] conflictArray = ifWebService.GetiFolderConflicts(ifolder.ID);
       ifWebService.ResolveNameConflict(ifolder.ID, conflicts.LocalConflict.ConflictID, newName.Text);
                            foreach (Conflict conflict in conflictArray)
                            {
                                if (conflict.IsNameConflict && conflict.ServerName != null)
                                {
                                    if (String.Compare(conflicts.LocalConflict.LocalFullPath, conflict.ServerFullPath, true) == 0)
                                    {
                                        if (readOnly)
                                        {
                                            ifWebService.RenameAndResolveConflict(ifolder.ID, conflict.ConflictID, conflict.ServerName);
                                            break;
                                        }
                                        else
                                        {
                                            ifWebService.ResolveNameConflict(ifolder.ID, conflict.ConflictID, conflict.ServerName);
                                            break;
                                        }
                                    }
                                }
                            }
       ifWebService.SynciFolderNow(ifolder.ID);
                            lvi.Remove();
      }
      catch (Exception ex)
      {
       MyMessageBox mmb;
       if (ex.Message.IndexOf("Malformed") != -1)
       {
        mmb = new MyMessageBox(resourceManager.GetString("badFileName"), resourceManager.GetString("errorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
        mmb.ShowDialog();
       }
       else
       {
        mmb = new MyMessageBox(resourceManager.GetString("conflictResolveError"), resourceManager.GetString("conflictErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
        mmb.ShowDialog();
       }
      }
     }
    }
   }
   if (conflictsView.Items.Count == 0 && ConflictsResolved != null)
   {
    ConflictsResolved(this, new EventArgs());
   }
  }
  public iFolderWebService iFolderWebService
  {
   set { ifWebService = value; }
  }
  public iFolderWeb iFolder
  {
   set { ifolder = value; }
  }
  public string LoadPath
  {
   set { loadPath = value; }
  }
  public delegate void ConflictsResolvedDelegate(object sender, EventArgs e);
  public event ConflictsResolvedDelegate ConflictsResolved;
        protected void InvokeConflictResolvedEvent(object sender, EventArgs e)
        {
            if( ConflictsResolved != null )
                ConflictsResolved(sender, e);
        }
  protected void ConflictResolver_Load(object sender, System.EventArgs e)
  {
   string helpFile = Path.Combine(Path.Combine(Path.Combine(loadPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"conflicts.html");
   if (!File.Exists(helpFile))
   {
    helpFile = Path.Combine(loadPath, @"help\en\conflicts.html");
   }
   if (File.Exists(helpFile))
   {
    helpProvider1.HelpNamespace = helpFile;
   }
   try
   {
    string basePath = loadPath != null ? Path.Combine(loadPath, "res") : Path.Combine(Application.StartupPath, "res");
                this.Icon = new Icon(Path.Combine(basePath, "ifolder-conflict22.ico"));
   }
   catch
   {
   }
   try
   {
    ifolderName.Text = ifolder.Name;
    Conflict[] conflictArray = ifWebService.GetiFolderConflicts(ifolder.ID);
    foreach (Conflict conflict in conflictArray)
    {
     Conflicts conflicts = new Conflicts();
     ListViewItem lvi = null;
     if (!conflict.IsNameConflict)
     {
      string[] items = new string[3];
      items[0] = conflict.LocalName;
      items[1] = Path.GetDirectoryName(conflict.LocalFullPath).Substring(ifolder.UnManagedPath.Length);
      items[2] = resourceManager.GetString("file");
      lvi = new ListViewItem(items);
      conflicts.ServerConflict = conflict;
     }
     else
     {
      if (conflict.LocalName != null)
      {
       string[] items = new string[3];
       items[0] = conflict.LocalName;
       items[1] = Path.GetDirectoryName(conflict.LocalFullPath).Substring(ifolder.UnManagedPath.Length);
       items[2] = resourceManager.GetString("name");
       conflicts.LocalConflict = conflict;
       lvi = new ListViewItem(items);
      }
      else
      {
       conflicts.ServerConflict = conflict;
      }
     }
     if (lvi != null)
     {
      lvi.Tag = conflicts;
      conflictsView.Items.Add(lvi);
     }
    }
    foreach (Conflict conflict in conflictArray)
    {
     if (conflict.IsNameConflict && (conflict.ServerFullPath != null))
     {
      foreach (ListViewItem lvi in conflictsView.Items)
      {
       Conflicts conflicts = (Conflicts)lvi.Tag;
                            if (conflicts.LocalConflict == null)
                                continue;
       if (lvi.Text.Equals(conflict.ServerName) && conflicts.LocalConflict.LocalFullPath.Equals(conflict.ServerFullPath))
       {
        ((Conflicts)lvi.Tag).ServerConflict = conflict;
        break;
       }
      }
     }
    }
   }
   catch (Exception ex)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("conflictReadError"), string.Empty, ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
   readOnly = ifolder.CurrentUserRights.Equals("ReadOnly");
   if (readOnly)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("readOnlyInfo"), resourceManager.GetString("readOnlyTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
    mmb.ShowDialog();
   }
  }
  protected void conflictsView_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   resolveNameLabel.Text = resourceManager.GetString("resolveNameLabel.Text");
   newName.Enabled = resolveName.Enabled = true;
   newName.Text = string.Empty;
   fixNames = true;
   if (conflictsView.SelectedItems.Count == 0)
   {
    serverPanel.Visible = localPanel.Visible = true;
    nameConflict.Visible = false;
    conflicts = null;
    localName.Text = serverName.Text =
     localDate.Text = serverDate.Text =
     localSize.Text = serverSize.Text = "";
    saveLocal.Enabled = saveServer.Enabled = false;
   }
   else if (conflictsView.SelectedItems.Count == 1)
   {
    conflicts = (Conflicts)conflictsView.SelectedItems[0].Tag;
    if ((conflicts.ServerConflict != null) && !conflicts.ServerConflict.IsNameConflict)
    {
     serverPanel.Visible = localPanel.Visible = true;
     nameConflict.Visible = false;
     localName.Text = conflicts.ServerConflict.LocalName;
     localDate.Text = conflicts.ServerConflict.LocalDate;
     localSize.Text = conflicts.ServerConflict.LocalSize;
     serverName.Text = conflicts.ServerConflict.ServerName;
     serverDate.Text = conflicts.ServerConflict.ServerDate;
     serverSize.Text = conflicts.ServerConflict.ServerSize;
     saveLocal.Enabled = !readOnly;
     saveServer.Enabled = true;
    }
    else
    {
     serverPanel.Visible = localPanel.Visible = false;
     nameConflict.Visible = true;
     if (conflicts.LocalConflict != null)
     {
      newName.Text = conflicts.LocalConflict.LocalName;
     }
     else if (readOnly)
     {
      string localFile = Path.Combine(ifolder.UnManagedPath, conflicts.ServerConflict.ServerName);
      if (File.Exists(localFile))
      {
       conflicts.LocalConflict = new Conflict();
       newName.Text = conflicts.LocalConflict.LocalName = conflicts.ServerConflict.ServerName;
       conflicts.LocalConflict.LocalFullPath = Path.Combine(ifolder.UnManagedPath, conflicts.LocalConflict.LocalName);
      }
      else
      {
       resolveName.Enabled = false;
      }
     }
     else
     {
      newName.Text = conflicts.ServerConflict.ServerName;
     }
    }
   }
   else
   {
    serverPanel.Visible = localPanel.Visible = false;
    nameConflict.Visible = true;
    resolveNameLabel.Text = resourceManager.GetString("multiNameConflict");
    newName.Enabled = resolveName.Enabled = false;
    foreach (ListViewItem lvi in conflictsView.SelectedItems)
    {
                    if(((Conflicts)lvi.Tag).ServerConflict != null)
     if (!((Conflicts)lvi.Tag).ServerConflict.IsNameConflict)
     {
      serverPanel.Visible = localPanel.Visible = true;
      nameConflict.Visible = false;
      break;
     }
    }
    conflicts = null;
    localName.Text = serverName.Text = resourceManager.GetString("multiSelect");
    localDate.Text = serverDate.Text =
     localSize.Text = serverSize.Text = "";
    saveLocal.Enabled = !readOnly;
    saveServer.Enabled = true;
   }
  }
        private void saveLocal_Click(object sender, System.EventArgs e)
  {
   resolveConflicts(true);
  }
        private void saveServer_Click(object sender, System.EventArgs e)
  {
   resolveConflicts(false);
  }
        protected void resolveName_Click(object sender, System.EventArgs e)
  {
   resolveNameConflict();
  }
        protected void newName_TextChanged(object sender, System.EventArgs e)
  {
   resolveName.Enabled = !newName.Text.Equals(string.Empty);
  }
        protected void localName_DoubleClick(object sender, System.EventArgs e)
  {
   if (conflicts != null)
   {
    try
    {
     if (!conflicts.ServerConflict.IsNameConflict)
     {
      System.Diagnostics.Process.Start(conflicts.ServerConflict.LocalFullPath);
     }
     else if (conflicts.LocalConflict != null)
     {
      System.Diagnostics.Process.Start(conflicts.LocalConflict.LocalFullPath);
     }
    }
    catch (Exception ex)
    {
     MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("openFileError"), string.Empty, ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
     mmb.ShowDialog();
    }
   }
  }
  protected void serverName_DoubleClick(object sender, System.EventArgs e)
  {
   if (conflicts != null)
   {
    try
    {
     System.Diagnostics.Process.Start(conflicts.ServerConflict.ServerFullPath);
    }
    catch (Exception ex)
    {
     MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("openFileError"), string.Empty, ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
     mmb.ShowDialog();
    }
   }
  }
        protected void ifolderPath_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
  {
   if (fixPath)
   {
    fixPath = false;
    formatLabelString(ifolderPath, ifolder.UnManagedPath);
   }
  }
        protected void localName_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
  {
   if (fixNames)
   {
    fixNames = false;
    if (conflictsView.SelectedItems.Count == 1)
    {
     Conflicts conflicts = (Conflicts)conflictsView.SelectedItems[0].Tag;
     if ((conflicts.ServerConflict != null) && !conflicts.ServerConflict.IsNameConflict)
     {
      formatLabelString(localName, conflicts.ServerConflict.LocalName);
      formatLabelString(serverName, conflicts.ServerConflict.ServerName);
     }
    }
   }
  }
  protected void close_Click(object sender, System.EventArgs e)
  {
   this.Close();
  }
        protected void ConflictResolver_SizeChanged(object sender, System.EventArgs e)
  {
   fixPath = fixNames = true;
   localPanel.Width = versionsPanel.Width / 2;
  }
 }
 [ComVisible(false)]
 public class Conflicts
 {
  private Conflict localConflict = null;
  private Conflict serverConflict = null;
  public Conflicts()
  {
  }
  public Conflict LocalConflict
  {
   get { return (this.localConflict); }
   set { this.localConflict = value; }
  }
  public Conflict ServerConflict
  {
   get { return (this.serverConflict); }
   set { this.serverConflict = value; }
  }
 }
}
