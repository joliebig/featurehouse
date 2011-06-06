using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;
using System.Xml;
using System.Runtime.InteropServices;
using Novell.Win32Util;
using Novell.iFolder.Web;
namespace Novell.iFolderCom
{
 [ComVisible(false)]
 public class CreateiFolder : System.Windows.Forms.Form
 {
  enum SecurityState
  {
   encryption = 1,
   enforceEncryption = 2,
   SSL = 4,
   enforceSSL = 8
  }
  System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(CreateiFolder));
  private iFolderWebService ifWebService;
  private SimiasWebService simws;
  private bool successful;
  private DomainItem selectedDomain;
  private string loadPath;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button cancel;
  private System.Windows.Forms.ComboBox servers;
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.Button browse;
        private System.Windows.Forms.Button help;
  private System.Windows.Forms.TextBox ifolderPath;
  private System.Windows.Forms.Label label3;
  private System.Windows.Forms.RadioButton encryption;
  private System.Windows.Forms.RadioButton regular;
        private System.Windows.Forms.CheckBox ssl;
  private System.ComponentModel.Container components = null;
  public CreateiFolder()
  {
   InitializeComponent();
   int delta = calculateSize(label1, 0);
   delta = calculateSize(label2, delta);
   label1.Width = label2.Width += delta;
   int temp = servers.Left;
   servers.Left = ifolderPath.Left = label1.Left + label1.Width;
   servers.Width = ifolderPath.Width -= servers.Left - temp;
  }
  public iFolderWebService iFolderWebService
  {
   set { ifWebService = value; }
  }
  public SimiasWebService simiasWebService
  {
   set { simws = value; }
  }
  public ArrayList Servers
  {
   set
   {
    foreach (DomainItem d in value)
    {
     servers.Items.Add(d);
    }
   }
  }
  public DomainItem SelectedDomain
  {
   set { selectedDomain = value; }
  }
  public string iFolderPath
  {
   get { return ifolderPath.Text; }
   set { ifolderPath.Text = value; }
  }
  public string LoadPath
  {
   set { loadPath = value; }
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(CreateiFolder));
   this.ok = new System.Windows.Forms.Button();
   this.cancel = new System.Windows.Forms.Button();
   this.servers = new System.Windows.Forms.ComboBox();
   this.label1 = new System.Windows.Forms.Label();
   this.ifolderPath = new System.Windows.Forms.TextBox();
   this.label2 = new System.Windows.Forms.Label();
   this.browse = new System.Windows.Forms.Button();
   this.label3 = new System.Windows.Forms.Label();
   this.encryption = new System.Windows.Forms.RadioButton();
            this.regular = new System.Windows.Forms.RadioButton();
   this.ssl = new System.Windows.Forms.CheckBox();
            Point sslloc = new System.Drawing.Point();
            this.help = new System.Windows.Forms.Button();
   this.SuspendLayout();
            sslloc.X = ((System.Drawing.Point)(resources.GetObject("encryption.Location"))).X;
            sslloc.Y = ((System.Drawing.Point)(resources.GetObject("ok.Location"))).Y;
   this.ok.AccessibleDescription = resources.GetString("ok.AccessibleDescription");
   this.ok.AccessibleName = resources.GetString("ok.AccessibleName");
   this.ok.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("ok.Anchor")));
   this.ok.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("ok.BackgroundImage")));
   this.ok.DialogResult = System.Windows.Forms.DialogResult.OK;
   this.ok.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("ok.Dock")));
   this.ok.Enabled = ((bool)(resources.GetObject("ok.Enabled")));
   this.ok.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("ok.FlatStyle")));
   this.ok.Font = ((System.Drawing.Font)(resources.GetObject("ok.Font")));
   this.ok.Image = ((System.Drawing.Image)(resources.GetObject("ok.Image")));
   this.ok.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ok.ImageAlign")));
   this.ok.ImageIndex = ((int)(resources.GetObject("ok.ImageIndex")));
   this.ok.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("ok.ImeMode")));
   this.ok.Location = ((System.Drawing.Point)(resources.GetObject("ok.Location")));
   this.ok.Name = "ok";
   this.ok.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("ok.RightToLeft")));
   this.ok.Size = ((System.Drawing.Size)(resources.GetObject("ok.Size")));
   this.ok.TabIndex = ((int)(resources.GetObject("ok.TabIndex")));
   this.ok.Text = resources.GetString("ok.Text");
   this.ok.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ok.TextAlign")));
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
   this.cancel.Image = ((System.Drawing.Image)(resources.GetObject("cancel.Image")));
   this.cancel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("cancel.ImageAlign")));
   this.cancel.ImageIndex = ((int)(resources.GetObject("cancel.ImageIndex")));
   this.cancel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("cancel.ImeMode")));
   this.cancel.Location = ((System.Drawing.Point)(resources.GetObject("cancel.Location")));
   this.cancel.Name = "cancel";
   this.cancel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("cancel.RightToLeft")));
   this.cancel.Size = ((System.Drawing.Size)(resources.GetObject("cancel.Size")));
   this.cancel.TabIndex = ((int)(resources.GetObject("cancel.TabIndex")));
   this.cancel.Text = resources.GetString("cancel.Text");
   this.cancel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("cancel.TextAlign")));
   this.cancel.Visible = ((bool)(resources.GetObject("cancel.Visible")));
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
   this.ifolderPath.AccessibleDescription = resources.GetString("ifolderPath.AccessibleDescription");
   this.ifolderPath.AccessibleName = resources.GetString("ifolderPath.AccessibleName");
   this.ifolderPath.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("ifolderPath.Anchor")));
   this.ifolderPath.AutoSize = ((bool)(resources.GetObject("ifolderPath.AutoSize")));
   this.ifolderPath.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("ifolderPath.BackgroundImage")));
   this.ifolderPath.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("ifolderPath.Dock")));
   this.ifolderPath.Enabled = ((bool)(resources.GetObject("ifolderPath.Enabled")));
   this.ifolderPath.Font = ((System.Drawing.Font)(resources.GetObject("ifolderPath.Font")));
   this.ifolderPath.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("ifolderPath.ImeMode")));
   this.ifolderPath.Location = ((System.Drawing.Point)(resources.GetObject("ifolderPath.Location")));
   this.ifolderPath.MaxLength = ((int)(resources.GetObject("ifolderPath.MaxLength")));
   this.ifolderPath.Multiline = ((bool)(resources.GetObject("ifolderPath.Multiline")));
   this.ifolderPath.Name = "ifolderPath";
   this.ifolderPath.PasswordChar = ((char)(resources.GetObject("ifolderPath.PasswordChar")));
   this.ifolderPath.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("ifolderPath.RightToLeft")));
   this.ifolderPath.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("ifolderPath.ScrollBars")));
   this.ifolderPath.Size = ((System.Drawing.Size)(resources.GetObject("ifolderPath.Size")));
   this.ifolderPath.TabIndex = ((int)(resources.GetObject("ifolderPath.TabIndex")));
   this.ifolderPath.Text = resources.GetString("ifolderPath.Text");
   this.ifolderPath.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("ifolderPath.TextAlign")));
   this.ifolderPath.Visible = ((bool)(resources.GetObject("ifolderPath.Visible")));
   this.ifolderPath.WordWrap = ((bool)(resources.GetObject("ifolderPath.WordWrap")));
   this.ifolderPath.TextChanged += new System.EventHandler(this.ifolderPath_TextChanged);
   this.label2.AccessibleDescription = resources.GetString("label2.AccessibleDescription");
   this.label2.AccessibleName = resources.GetString("label2.AccessibleName");
   this.label2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label2.Anchor")));
   this.label2.AutoSize = ((bool)(resources.GetObject("label2.AutoSize")));
   this.label2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label2.Dock")));
   this.label2.Enabled = ((bool)(resources.GetObject("label2.Enabled")));
   this.label2.Font = ((System.Drawing.Font)(resources.GetObject("label2.Font")));
   this.label2.Image = ((System.Drawing.Image)(resources.GetObject("label2.Image")));
   this.label2.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label2.ImageAlign")));
   this.label2.ImageIndex = ((int)(resources.GetObject("label2.ImageIndex")));
   this.label2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label2.ImeMode")));
   this.label2.Location = ((System.Drawing.Point)(resources.GetObject("label2.Location")));
   this.label2.Name = "label2";
   this.label2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label2.RightToLeft")));
   this.label2.Size = ((System.Drawing.Size)(resources.GetObject("label2.Size")));
   this.label2.TabIndex = ((int)(resources.GetObject("label2.TabIndex")));
   this.label2.Text = resources.GetString("label2.Text");
   this.label2.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label2.TextAlign")));
   this.label2.Visible = ((bool)(resources.GetObject("label2.Visible")));
   this.browse.AccessibleDescription = resources.GetString("browse.AccessibleDescription");
   this.browse.AccessibleName = resources.GetString("browse.AccessibleName");
   this.browse.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("browse.Anchor")));
   this.browse.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("browse.BackgroundImage")));
   this.browse.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("browse.Dock")));
   this.browse.Enabled = ((bool)(resources.GetObject("browse.Enabled")));
   this.browse.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("browse.FlatStyle")));
   this.browse.Font = ((System.Drawing.Font)(resources.GetObject("browse.Font")));
   this.browse.Image = ((System.Drawing.Image)(resources.GetObject("browse.Image")));
   this.browse.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("browse.ImageAlign")));
   this.browse.ImageIndex = ((int)(resources.GetObject("browse.ImageIndex")));
   this.browse.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("browse.ImeMode")));
   this.browse.Location = ((System.Drawing.Point)(resources.GetObject("browse.Location")));
   this.browse.Name = "browse";
   this.browse.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("browse.RightToLeft")));
   this.browse.Size = ((System.Drawing.Size)(resources.GetObject("browse.Size")));
   this.browse.TabIndex = ((int)(resources.GetObject("browse.TabIndex")));
   this.browse.Text = resources.GetString("browse.Text");
   this.browse.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("browse.TextAlign")));
   this.browse.Visible = ((bool)(resources.GetObject("browse.Visible")));
   this.browse.Click += new System.EventHandler(this.browse_Click);
   this.label3.AccessibleDescription = resources.GetString("label3.AccessibleDescription");
   this.label3.AccessibleName = resources.GetString("label3.AccessibleName");
   this.label3.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label3.Anchor")));
   this.label3.AutoSize = ((bool)(resources.GetObject("label3.AutoSize")));
   this.label3.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label3.Dock")));
   this.label3.Enabled = ((bool)(resources.GetObject("label3.Enabled")));
   this.label3.Font = ((System.Drawing.Font)(resources.GetObject("label3.Font")));
   this.label3.Image = ((System.Drawing.Image)(resources.GetObject("label3.Image")));
   this.label3.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label3.ImageAlign")));
   this.label3.ImageIndex = ((int)(resources.GetObject("label3.ImageIndex")));
   this.label3.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label3.ImeMode")));
   this.label3.Location = ((System.Drawing.Point)(resources.GetObject("label3.Location")));
   this.label3.Name = "label3";
   this.label3.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label3.RightToLeft")));
   this.label3.Size = ((System.Drawing.Size)(resources.GetObject("label3.Size")));
   this.label3.TabIndex = ((int)(resources.GetObject("label3.TabIndex")));
   this.label3.Text = resources.GetString("label3.Text");
   this.label3.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label3.TextAlign")));
   this.label3.Visible = ((bool)(resources.GetObject("label3.Visible")));
   this.encryption.AccessibleDescription = resources.GetString("encryption.AccessibleDescription");
   this.encryption.AccessibleName = resources.GetString("encryption.AccessibleName");
   this.encryption.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("encryption.Anchor")));
   this.encryption.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("encryption.Appearance")));
   this.encryption.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("encryption.BackgroundImage")));
   this.encryption.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("encryption.CheckAlign")));
   this.encryption.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("encryption.Dock")));
   this.encryption.Enabled = ((bool)(resources.GetObject("encryption.Enabled")));
   this.encryption.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("encryption.FlatStyle")));
   this.encryption.Font = ((System.Drawing.Font)(resources.GetObject("encryption.Font")));
   this.encryption.Image = ((System.Drawing.Image)(resources.GetObject("encryption.Image")));
   this.encryption.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("encryption.ImageAlign")));
   this.encryption.ImageIndex = ((int)(resources.GetObject("encryption.ImageIndex")));
   this.encryption.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("encryption.ImeMode")));
   this.encryption.Location = ((System.Drawing.Point)(resources.GetObject("encryption.Location")));
   this.encryption.Name = "encryption";
   this.encryption.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("encryption.RightToLeft")));
   this.encryption.Size = ((System.Drawing.Size)(resources.GetObject("encryption.Size")));
   this.encryption.TabIndex = ((int)(resources.GetObject("encryption.TabIndex")));
   this.encryption.Text = resources.GetString("encryption.Text");
   this.encryption.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("encryption.TextAlign")));
   this.encryption.Visible = ((bool)(resources.GetObject("encryption.Visible")));
   this.encryption.CheckedChanged += new System.EventHandler(this.checkBox1_CheckedChanged);
   this.regular.AccessibleDescription = resources.GetString("regular.AccessibleDescription");
   this.regular.AccessibleName = resources.GetString("regular.AccessibleName");
   this.regular.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("regular.Anchor")));
   this.regular.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("regular.Appearance")));
   this.regular.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("regular.BackgroundImage")));
   this.regular.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("regular.CheckAlign")));
   this.regular.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("regular.Dock")));
   this.regular.Enabled = ((bool)(resources.GetObject("regular.Enabled")));
   this.regular.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("regular.FlatStyle")));
   this.regular.Font = ((System.Drawing.Font)(resources.GetObject("regular.Font")));
   this.regular.Image = ((System.Drawing.Image)(resources.GetObject("regular.Image")));
   this.regular.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("regular.ImageAlign")));
   this.regular.ImageIndex = ((int)(resources.GetObject("regular.ImageIndex")));
   this.regular.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("regular.ImeMode")));
   this.regular.Location = ((System.Drawing.Point)(resources.GetObject("regular.Location")));
   this.regular.Name = "regular";
   this.regular.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("regular.RightToLeft")));
   this.regular.Size = ((System.Drawing.Size)(resources.GetObject("regular.Size")));
   this.regular.TabIndex = ((int)(resources.GetObject("regular.TabIndex")));
   this.regular.Text = resources.GetString("regular.Text");
   this.regular.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("regular.TextAlign")));
   this.regular.Visible = ((bool)(resources.GetObject("regular.Visible")));
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
            this.ssl.Image = ((System.Drawing.Image)(resources.GetObject("ssl.Image")));
            this.ssl.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ssl.ImageAlign")));
            this.ssl.ImageIndex = ((int)(resources.GetObject("ssl.ImageIndex")));
            this.ssl.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("ssl.ImeMode")));
            this.ssl.Location = ((System.Drawing.Point)(resources.GetObject("ssl.Location")));
            this.ssl.Name = "ssl";
            this.ssl.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("ssl.RightToLeft")));
            this.ssl.Size = ((System.Drawing.Size)(resources.GetObject("ssl.Size")));
            this.ssl.TabIndex = ((int)(resources.GetObject("ssl.TabIndex")));
            this.ssl.Text = resources.GetString("ssl.Text");
            this.ssl.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("ssl.TextAlign")));
            this.ssl.Visible = ((bool)(resources.GetObject("ssl.Visible")));
            this.help.AccessibleDescription = resources.GetString("help.AccessibleDescription");
            this.help.AccessibleName = resources.GetString("help.AccessibleName");
            this.help.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("help.Anchor")));
            this.help.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("help.BackgroundImage")));
            this.help.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("help.Dock")));
            this.help.Enabled = ((bool)(resources.GetObject("help.Enabled")));
            this.help.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("help.FlatStyle")));
            this.help.Font = ((System.Drawing.Font)(resources.GetObject("help.Font")));
            this.help.Image = ((System.Drawing.Image)(resources.GetObject("help.Image")));
            this.help.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("help.ImageAlign")));
            this.help.ImageIndex = ((int)(resources.GetObject("help.ImageIndex")));
            this.help.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("help.ImeMode")));
            this.help.Location = ((System.Drawing.Point)(resources.GetObject("help.Location")));
            this.help.Name = "help";
            this.help.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("help.RightToLeft")));
            this.help.Size = ((System.Drawing.Size)(resources.GetObject("help.Size")));
            this.help.TabIndex = ((int)(resources.GetObject("help.TabIndex")));
            this.help.Text = resources.GetString("help.Text");
            this.help.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("help.TextAlign")));
            this.help.Visible = ((bool)(resources.GetObject("help.Visible")));
            this.help.Click += new System.EventHandler(this.help_Click);
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
   this.Controls.Add(this.ssl);
            this.Controls.Add(this.regular);
   this.Controls.Add(this.encryption);
   this.Controls.Add(this.label3);
   this.Controls.Add(this.browse);
   this.Controls.Add(this.ifolderPath);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.servers);
   this.Controls.Add(this.label1);
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.ok);
            this.Controls.Add(this.help);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimizeBox = false;
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "CreateiFolder";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.ShowInTaskbar = false;
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.Closing += new System.ComponentModel.CancelEventHandler(this.CreateiFolder_Closing);
   this.Load += new System.EventHandler(this.CreateiFolder_Load);
   this.Activated += new System.EventHandler(this.CreateiFolder_Activated);
   this.ResumeLayout(false);
  }
  private void CreateiFolder_Load(object sender, System.EventArgs e)
  {
   this.Icon = new Icon(Path.Combine(loadPath, @"res\ifolder_16.ico"));
   this.ok.Enabled = false;
   this.encryption.Checked = false;
   this.encryption.Enabled = this.regular.Enabled = false;
   this.regular.Checked = true;
   if (servers.Items.Count == 0)
   {
    try
    {
     XmlDocument domainsDoc = new XmlDocument();
     domainsDoc.Load(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "domain.list"));
     XmlElement element = (XmlElement)domainsDoc.SelectSingleNode("/domains");
     XmlElement defaultDomainElement = (XmlElement)domainsDoc.SelectSingleNode("/domains/defaultDomain");
     string defaultID = defaultDomainElement.GetAttribute("ID");
     XmlNodeList nodeList = element.GetElementsByTagName("domain");
                    ArrayList domains = new ArrayList();
                    DomainInformation[] domainsInfo = simws.GetDomains(false);
                    if (null != domainsInfo)
                    {
                        for (int i = 0; i < domainsInfo.Length; i++)
                        {
                            DomainItem domainItem = new DomainItem(domainsInfo[i].Name, domainsInfo[i].ID, domainsInfo[i].Host, domainsInfo[i].HostUrl);
                            servers.Items.Add(domainItem);
                            if ( (domainsInfo[i].ID).Equals(defaultID))
                            {
                                selectedDomain = domainItem;
                            }
                        }
                    }
                    if (selectedDomain != null)
     {
      servers.SelectedItem = selectedDomain;
     }
     else
     {
      servers.SelectedIndex = 0;
     }
    }
    catch
    {
    }
   }
   else
   {
    if (selectedDomain != null)
    {
     servers.SelectedItem = selectedDomain;
    }
    else if (servers.Items.Count > 0)
    {
     servers.SelectedIndex = 0;
    }
   }
   if (!ifolderPath.Text.Equals(string.Empty))
   {
    ifolderPath.ReadOnly = true;
    browse.Enabled = false;
   }
  }
  private void CreateiFolder_Activated(object sender, System.EventArgs e)
  {
   if (ifolderPath.ReadOnly)
   {
    servers.Focus();
    if( servers.SelectedIndex < 0)
     servers.SelectedIndex = 0;
   }
   else
   {
    ifolderPath.Focus();
    if( servers.SelectedIndex < 0)
     servers.SelectedIndex = 0;
   }
  }
  private void browse_Click(object sender, System.EventArgs e)
  {
   FolderBrowserDialog folderBrowserDialog = new FolderBrowserDialog();
   folderBrowserDialog.Description = resourceManager.GetString("chooseFolder");
   folderBrowserDialog.SelectedPath = ifolderPath.Text;
   if(folderBrowserDialog.ShowDialog() == DialogResult.OK)
   {
    ifolderPath.Text = folderBrowserDialog.SelectedPath;
   }
  }
  private void ifolderPath_TextChanged(object sender, System.EventArgs e)
  {
   ok.Enabled = (ifolderPath.Text.Length > 0) && (servers.Items.Count != 0);
  }
        private void servers_SelectedIndexChanged(object sender, System.EventArgs e)
        {
            ok.Enabled = (ifolderPath.Text.Length > 0) && (servers.SelectedItem != null);
            DomainItem domain = (DomainItem)servers.SelectedItem;
            int SecurityPolicy = ifWebService.GetSecurityPolicy(domain.ID);
            this.encryption.Checked = true;
            this.encryption.Enabled = this.regular.Enabled = false;
            this.regular.Checked = false;
            if (SecurityPolicy != 0)
            {
                if ((SecurityPolicy & (int)SecurityState.encryption) == (int)SecurityState.encryption)
                {
                    if ((SecurityPolicy & (int)SecurityState.enforceEncryption) == (int)SecurityState.enforceEncryption)
                        encryption.Checked = true;
                    else
                    {
                        encryption.Enabled = true;
                        regular.Enabled = true;
                    }
                }
                else
                    regular.Checked = true;
            }
            else
                regular.Checked = true;
            if (domain.Url.StartsWith(Uri.UriSchemeHttps))
            {
                this.ssl.Checked = true;
                this.ssl.Enabled = false;
            }
            else
            {
                this.ssl.Checked = false;
                this.ssl.Enabled = true;
            }
        }
  private void ok_Click(object sender, System.EventArgs e)
  {
   successful = true;
   try
   {
    try
    {
     Uri uriPath = new Uri(
      ifolderPath.Text.EndsWith(Path.DirectorySeparatorChar.ToString()) ?
      ifolderPath.Text :
      ifolderPath.Text + Path.DirectorySeparatorChar.ToString());
     if (ifolderPath.Text.StartsWith(@"\\"))
     {
      throw new Exception("Invalid path");
     }
                       if ((ifolderPath.Text.EndsWith(@":\"))||(ifolderPath.Text.EndsWith(@":")))
                       {
                           MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("rootDriveError"), resourceManager.GetString("errorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                           mmb.ShowDialog();
                           successful = false;
                           return;
                       }
    }
    catch
    {
     MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("invalidFolder"), resourceManager.GetString("errorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
     mmb.ShowDialog();
     successful = false;
     return;
    }
                DomainItem domainItemInfo = (DomainItem)servers.SelectedItem;
                if (ifWebService.GetLimitPolicyStatus(domainItemInfo.ID) != 1)
                {
                    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("ifolderlimiterror"), resourceManager.GetString("errorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                    mmb.ShowDialog();
                    successful = false;
                    return;
                }
    if (!Directory.Exists(ifolderPath.Text))
    {
     MyMessageBox mmb =new MyMessageBox(resourceManager.GetString("createPrompt"), resourceManager.GetString("createPromptTitle"), string.Empty, MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question);
     if (mmb.ShowDialog() == DialogResult.Yes)
     {
      string parent = Path.GetDirectoryName(ifolderPath.Text);
      while ((parent != null) && !parent.Equals(string.Empty))
      {
       if (Directory.Exists(parent))
       {
        ifolderPath.Text = ifolderPath.Text.Replace(parent, FixPath(parent));
        break;
       }
       parent = Path.GetDirectoryName(parent);
      }
      Directory.CreateDirectory(ifolderPath.Text);
     }
     else
     {
      successful = false;
     }
    }
    else
    {
     ifolderPath.Text = FixPath( ifolderPath.Text );
    }
    if (successful)
    {
     if (Win32Security.AccessAllowed(ifolderPath.Text))
     {
      Cursor.Current = Cursors.WaitCursor;
      DomainItem domainItem = (DomainItem)servers.SelectedItem;
      int temp=ifWebService.GetSecurityPolicy(domainItem.ID);
      iFolderWeb ifolder;
                        bool sslChecked = this.ssl.Checked;
      if( this.regular.Checked)
      {
       ifolder = this.ifWebService.CreateiFolderInDomainEncr(ifolderPath.Text, domainItem.ID, sslChecked, null, null);
                        }
      else
      {
                            DomainInformation domainInfo = this.simws.GetDomainInformation(domainItem.ID);
                            if (domainInfo.Authenticated == false)
                            {
                                throw new Exception("LoginRequired");
                            }
       string algorithm = (this.encryption.Checked)? "BlowFish" : null;
       bool passPhraseStatus = false;
       bool passphraseStatus = false;
                            passphraseStatus = simws.IsPassPhraseSet(domainItem.ID);
                            if(passphraseStatus == true)
       {
        string passphrasecheck = null;
         passphrasecheck = simws.GetPassPhrase(domainItem.ID);
         if( passphrasecheck == null || passphrasecheck =="")
         {
          VerifyPassphraseDialog vpd = new VerifyPassphraseDialog(domainItem.ID, this.simws);
                                        vpd.LoadPath = loadPath;
          vpd.ShowDialog();
          passPhraseStatus = vpd.PassphraseStatus;
         }
        else
        {
         passPhraseStatus = true;
        }
       }
       else
       {
        EnterPassphraseDialog enterPassPhrase= new EnterPassphraseDialog(domainItem.ID, this.simws,this.ifWebService);
                                enterPassPhrase.LoadPath = loadPath;
        enterPassPhrase.ShowDialog();
        passPhraseStatus = enterPassPhrase.PassphraseStatus;
       }
       if( passPhraseStatus == false)
       {
        successful = false;
        MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("PPForEncryption") , resourceManager.GetString("$this.Text") , string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
        mmb.ShowDialog();
       }
       else
       {
        string Passphrase = simws.GetPassPhrase(domainItem.ID);
        ifolder = this.ifWebService.CreateiFolderInDomainEncr(ifolderPath.Text, domainItem.ID, sslChecked, algorithm, Passphrase);
       }
      }
      Win32Window.ShChangeNotify(Win32Window.SHCNE_UPDATEITEM, Win32Window.SHCNF_PATHW, ifolderPath.Text, IntPtr.Zero);
      Cursor.Current = Cursors.Default;
     }
     else
     {
      successful = false;
      MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("accessDenied"), resourceManager.GetString("accessErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
      mmb.ShowDialog();
     }
    }
   }
   catch (Exception ex)
   {
    successful = false;
    Cursor.Current = Cursors.Default;
    MyMessageBox mmb;
    string message;
    string caption = resourceManager.GetString("pathInvalidErrorTitle");
    if (ex.Message.IndexOf("InvalidCharactersPath") != -1)
    {
     message = resourceManager.GetString("invalidCharsError");
    }
    else if (ex.Message.IndexOf("AtOrInsideStorePath") != -1)
    {
     message = resourceManager.GetString("pathInStoreError");
    }
    else if (ex.Message.IndexOf("ContainsStorePath") != -1)
    {
     message = resourceManager.GetString("pathContainsStoreError");
    }
    else if (ex.Message.IndexOf("SystemDirectoryPath") != -1)
    {
     message = resourceManager.GetString("systemDirError");
    }
    else if (ex.Message.IndexOf("SystemDrivePath") != -1)
    {
     message = resourceManager.GetString("systemDriveError");
    }
    else if (ex.Message.IndexOf("IncludesWinDirPath") != -1)
    {
     message = resourceManager.GetString("winDirError");
    }
    else if (ex.Message.IndexOf("IncludesProgFilesPath") != -1)
    {
     message = resourceManager.GetString("progFilesDirError");
    }
    else if (ex.Message.IndexOf("ContainsCollectionPath") != -1)
    {
     message = resourceManager.GetString("containsiFolderError");
    }
    else if (ex.Message.IndexOf("AtOrInsideCollectionPath") != -1)
    {
     message = resourceManager.GetString("pathIniFolderError");
    }
    else if (ex.Message.IndexOf("RootOfDrivePath") != -1)
    {
     message = resourceManager.GetString("rootDriveError");
    }
    else if (ex.Message.IndexOf("NotFixedDrivePath") != -1)
    {
     message = resourceManager.GetString("networkPathError");
    }
                else if (ex.Message.IndexOf("LoginRequired") != -1)
                {
                    message = resourceManager.GetString("LoginToDomain");
                    caption = resourceManager.GetString("errorTitle");
                }
    else
    {
     message = resourceManager.GetString("iFolderCreateError");
     caption = resourceManager.GetString("errorTitle");
    }
    mmb = new MyMessageBox(message, caption, string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
  }
  private void CreateiFolder_Closing(object sender, System.ComponentModel.CancelEventArgs e)
  {
   if ((this.DialogResult == DialogResult.OK) && !successful)
   {
    e.Cancel = true;
   }
  }
  public static string FixPath(string path)
  {
   if (path[1].Equals(':'))
   {
    string root = path.Substring(0, 2);
    path = path.Replace(root, root.ToUpper());
   }
   try
   {
    string parent = path;
    string temp = string.Empty;
    while (true)
    {
     string file = Path.GetFileName(parent);
     parent = Path.GetDirectoryName(parent);
     if ((parent == null) || parent.Equals(string.Empty))
     {
      string psub = path.Substring(3);
      if (string.Compare(psub, temp, true) == 0)
       path = path.Replace(psub, temp);
      break;
     }
     string[] dirs = Directory.GetFileSystemEntries(parent, file);
     if (dirs.Length == 1)
     {
      temp = Path.Combine(Path.GetFileName(dirs[0]), temp);
     }
    }
   }
   catch {}
   return path;
  }
  private void SetSecurityState()
  {
   DomainItem domain = (DomainItem) servers.SelectedItem;
   this.encryption.Checked = this.regular.Checked = false;
   this.encryption.Enabled = this.regular.Enabled = false;
   if(domain == null)
    return;
   int securityPolicy = ifWebService.GetSecurityPolicy(domain.ID);
   if( (securityPolicy & 0x0001) == 0x01)
   {
    if( (securityPolicy & 0x0010) == 0x0010)
     this.encryption.Checked = true;
    else
     this.encryption.Enabled = true;
   }
   if( (securityPolicy & 0x0100) == 0x0100)
   {
   if( (securityPolicy & 0x01000) == 0x01000)
    this.regular.Checked = true;
   else
    this.regular.Enabled = true;
   }
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
  private void checkBox1_CheckedChanged(object sender, System.EventArgs e)
  {
  }
        private void help_Click(object sender, System.EventArgs e)
        {
            string helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"createifolder.html");
            helpFile = iFolderAdvanced.veryfiyPath(helpFile, "createifolder.html");
            new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
        }
 }
 internal class VerifyPassphraseDialog : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Panel panel1;
  private System.Windows.Forms.PictureBox waterMark;
  private System.Windows.Forms.PictureBox pictureBox1;
  private System.Windows.Forms.Label lblPassphrase;
  private System.Windows.Forms.TextBox Passphrase;
  private System.Windows.Forms.CheckBox savePassphrase;
  private System.Windows.Forms.Button btnCancel;
  private System.Windows.Forms.Button btnOk;
  private SimiasWebService simws;
  private string DomainID;
  private bool status;
  private System.ComponentModel.Container components = null;
        private string loadpath;
  public bool PassphraseStatus
  {
   get
   {
    return status;
   }
  }
        public string LoadPath
        {
            set
            {
                loadpath = value;
            }
        }
  public VerifyPassphraseDialog(string domainID, SimiasWebService simws)
  {
   this.DomainID = domainID;
   this.simws = simws;
   InitializeComponent();
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(CreateiFolder));
   this.panel1 = new System.Windows.Forms.Panel();
   this.pictureBox1 = new System.Windows.Forms.PictureBox();
   this.waterMark = new System.Windows.Forms.PictureBox();
   this.lblPassphrase = new System.Windows.Forms.Label();
   this.Passphrase = new System.Windows.Forms.TextBox();
   this.savePassphrase = new System.Windows.Forms.CheckBox();
   this.btnCancel = new System.Windows.Forms.Button();
   this.btnOk = new System.Windows.Forms.Button();
   this.panel1.SuspendLayout();
   this.SuspendLayout();
   this.panel1.BackColor = System.Drawing.Color.Transparent;
   this.panel1.Controls.Add(this.waterMark);
   this.panel1.Controls.Add(this.pictureBox1);
   this.panel1.Location = new System.Drawing.Point(0, 0);
   this.panel1.Name = "panel1";
   this.panel1.Size = new System.Drawing.Size(448, 65);
   this.panel1.TabIndex = 1;
   this.waterMark.Location = new System.Drawing.Point(0,0);
   this.waterMark.Name = "waterMark";
   this.waterMark.Size = new System.Drawing.Size(159, 65);
   this.waterMark.TabIndex = 0;
   this.waterMark.TabStop = false;
   this.pictureBox1.Location = new System.Drawing.Point(159, 0);
   this.pictureBox1.Size = new System.Drawing.Size(320, 65);
   this.lblPassphrase.Location = new System.Drawing.Point(16, 76);
   this.lblPassphrase.Name = "lblPassphrase";
   this.lblPassphrase.Size = new System.Drawing.Size(140, 20);
   this.lblPassphrase.TabIndex = 2;
   this.lblPassphrase.Text = resources.GetString("EnterPPText");
   this.lblPassphrase.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
   this.Passphrase.Location = new System.Drawing.Point(156, 76);
   this.Passphrase.Name = "Passphrase";
   this.Passphrase.Size = new System.Drawing.Size(240, 20);
   this.Passphrase.TabIndex = 3;
   this.Passphrase.Text = "";
   this.Passphrase.PasswordChar = '*';
   this.Passphrase.TextChanged +=new EventHandler(Passphrase_TextChanged);
   this.savePassphrase.Location = new System.Drawing.Point(156, 104);
   this.savePassphrase.Name = "savePassphrase";
   this.savePassphrase.Size = new System.Drawing.Size(240, 20);
   this.savePassphrase.TabIndex = 4;
   this.savePassphrase.Text = resources.GetString("RememberPPText");
   this.btnCancel.Location = new System.Drawing.Point(319, 134);
   this.btnCancel.Name = "btnCancel";
   this.btnCancel.Size = new System.Drawing.Size(72, 24);
   this.btnCancel.TabIndex = 6;
   this.btnCancel.Text = resources.GetString("cancel.Text");
   this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
   this.btnOk.Location = new System.Drawing.Point(239, 134);
   this.btnOk.Name = "btnOk";
   this.btnOk.TabIndex = 5;
   this.btnOk.Text = resources.GetString("ok.Text");
   this.btnOk.Click += new System.EventHandler(this.btnOk_Click);
   this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
   this.ClientSize = new System.Drawing.Size(420, 178);
   this.Controls.Add(this.btnOk);
   this.Controls.Add(this.btnCancel);
   this.Controls.Add(this.savePassphrase);
   this.Controls.Add(this.Passphrase);
   this.Controls.Add(this.lblPassphrase);
   this.Controls.Add(this.panel1);
   this.Name = "VerifyPassphraseDialog";
            this.AcceptButton = this.btnOk;
            this.MaximizeBox = false;
   this.Text = resources.GetString("VerifyPPTitle");
   this.Load += new System.EventHandler(this.VerifyPassphraseDialog_Load);
   this.panel1.ResumeLayout(false);
   this.ResumeLayout(false);
  }
  private void btnCancel_Click(object sender, System.EventArgs e)
  {
   simws.StorePassPhrase(DomainID, "", CredentialType.None, false);
   status = false;
   this.Dispose();
   this.Close();
  }
  private void btnOk_Click(object sender, System.EventArgs e)
  {
   Status passPhraseStatus = simws.ValidatePassPhrase(this.DomainID, this.Passphrase.Text);
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(CreateiFolder));
   if( passPhraseStatus != null)
   {
    if( passPhraseStatus.statusCode == StatusCodes.PassPhraseInvalid)
    {
     Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resources.GetString("ValidatePPError") , resources.GetString("VerifyPPTitle") , resources.GetString("TryAgain") , MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
     mmb.ShowDialog();
     mmb.Dispose();
    }
    else if(passPhraseStatus.statusCode == StatusCodes.Success)
    {
     try
     {
      simws.StorePassPhrase( DomainID, this.Passphrase.Text, CredentialType.Basic, this.savePassphrase.Checked);
      status = true;
      this.Dispose();
      this.Close();
     }
     catch
     {
      status = false;
     }
    }
   }
  }
  private void VerifyPassphraseDialog_Load(object sender, System.EventArgs e)
  {
   this.btnOk.Enabled = false;
   this.waterMark.Image = Image.FromFile(System.IO.Path.Combine(loadpath, @"res\ifolder-banner.png"));
            this.Icon = new Icon(System.IO.Path.Combine(loadpath, @"res\ifolder_16.ico"));
   this.pictureBox1.SizeMode = PictureBoxSizeMode.StretchImage;
   this.pictureBox1.Image = Image.FromFile(System.IO.Path.Combine(loadpath, @"res\ifolder-banner-scaler.png"));
  }
  private void Passphrase_TextChanged(object sender, EventArgs e)
  {
   if( this.Passphrase.Text.Length > 0)
    this.btnOk.Enabled = true;
   else
    this.btnOk.Enabled = false;
  }
 }
 internal class EnterPassphraseDialog : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Panel panel1;
  private System.Windows.Forms.PictureBox waterMark;
  private System.Windows.Forms.PictureBox pictureBox1;
  private System.Windows.Forms.ComboBox RecoveryAgentCombo;
  private System.Windows.Forms.Label lblRecoveryAgent;
  private System.Windows.Forms.TextBox Passphrase;
  private System.Windows.Forms.TextBox RetypePassphrase;
  private System.Windows.Forms.Label lblPassphrase;
  private System.Windows.Forms.Label lblRetypePassphrase;
  private System.Windows.Forms.CheckBox savePassphrase;
  private System.Windows.Forms.Button btnCancel;
  private System.Windows.Forms.Button btnOk;
  private System.ComponentModel.Container components = null;
  private SimiasWebService simws;
        private iFolderWebService ifws;
  private string DomainID;
  private bool status;
        private string loadpath;
  public bool PassphraseStatus
  {
   get
   {
    return status;
   }
  }
        public string LoadPath
        {
            set
            {
                loadpath = value;
            }
        }
        public EnterPassphraseDialog(string domainID, SimiasWebService simws, iFolderWebService ifws)
            :this(domainID, simws)
        {
            this.ifws = ifws;
        }
  public EnterPassphraseDialog(string domainID, SimiasWebService simws)
  {
   this.DomainID = domainID;
   this.simws = simws;
   InitializeComponent();
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(CreateiFolder));
   this.panel1 = new System.Windows.Forms.Panel();
   this.waterMark = new System.Windows.Forms.PictureBox();
   this.pictureBox1 = new PictureBox();
   this.RecoveryAgentCombo = new System.Windows.Forms.ComboBox();
   this.lblRecoveryAgent = new System.Windows.Forms.Label();
   this.Passphrase = new System.Windows.Forms.TextBox();
   this.RetypePassphrase = new System.Windows.Forms.TextBox();
   this.lblPassphrase = new System.Windows.Forms.Label();
   this.lblRetypePassphrase = new System.Windows.Forms.Label();
   this.savePassphrase = new System.Windows.Forms.CheckBox();
   this.btnCancel = new System.Windows.Forms.Button();
   this.btnOk = new System.Windows.Forms.Button();
   this.panel1.SuspendLayout();
   this.SuspendLayout();
   this.panel1.BackColor = System.Drawing.Color.Transparent;
   this.panel1.Controls.Add(this.waterMark);
   this.panel1.Controls.Add(this.pictureBox1);
   this.panel1.Location = new System.Drawing.Point(0, 0);
   this.panel1.Name = "panel1";
   this.panel1.Size = new System.Drawing.Size(448, 65);
   this.panel1.TabIndex = 1;
   this.waterMark.Location = new System.Drawing.Point(0, 0);
   this.waterMark.Name = "waterMark";
   this.waterMark.Size = new System.Drawing.Size(159, 65);
   this.waterMark.TabIndex = 0;
   this.waterMark.TabStop = false;
   this.pictureBox1.Location = new Point(159, 0);
   this.pictureBox1.Size = new Size(320, 65);
   this.pictureBox1.Name = "pictureBox1";
   this.RecoveryAgentCombo.Location = new System.Drawing.Point(156, 76);
   this.RecoveryAgentCombo.Name = "RecoveryAgentCombo";
   this.RecoveryAgentCombo.Size = new System.Drawing.Size(240, 21);
   this.RecoveryAgentCombo.TabIndex = 2;
   this.lblRecoveryAgent.Location = new System.Drawing.Point(16, 76);
   this.lblRecoveryAgent.Name = "lblRecoveryAgent";
   this.lblRecoveryAgent.Size = new System.Drawing.Size(140, 20);
   this.lblRecoveryAgent.TabIndex = 3;
   this.lblRecoveryAgent.Text = resources.GetString("RecoveryAgent");
   this.lblRecoveryAgent.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
   this.Passphrase.Location = new System.Drawing.Point(156, 104);
   this.Passphrase.Name = "Passphrase";
   this.Passphrase.Size = new System.Drawing.Size(240, 20);
   this.Passphrase.TabIndex = 4;
   this.Passphrase.Text = "";
   this.Passphrase.TextChanged += new System.EventHandler(this.Passphrase_TextChanged);
   this.Passphrase.PasswordChar = '*';
   this.RetypePassphrase.Location = new System.Drawing.Point(156, 132);
   this.RetypePassphrase.Name = "RetypePassphrase";
   this.RetypePassphrase.Size = new System.Drawing.Size(240, 20);
   this.RetypePassphrase.TabIndex = 5;
   this.RetypePassphrase.Text = "";
   this.RetypePassphrase.PasswordChar = '*';
   this.RetypePassphrase.TextChanged += new System.EventHandler(this.RetypePassphrase_TextChanged);
   this.lblPassphrase.Location = new System.Drawing.Point(16, 104);
   this.lblPassphrase.Name = "lblPassphrase";
   this.lblPassphrase.Size = new System.Drawing.Size(120, 20);
   this.lblPassphrase.TabIndex = 6;
   this.lblPassphrase.Text = resources.GetString("EnterPPText");
   this.lblRetypePassphrase.Location = new System.Drawing.Point(16, 132);
   this.lblRetypePassphrase.Name = "lblRetypePassphrase";
   this.lblRetypePassphrase.Size = new System.Drawing.Size(120, 20);
   this.lblRetypePassphrase.TabIndex = 7;
   this.lblRetypePassphrase.Text = resources.GetString("RetypePP");
   this.savePassphrase.Location = new System.Drawing.Point(156, 160);
   this.savePassphrase.Name = "savePassphrase";
   this.savePassphrase.Size = new System.Drawing.Size(240, 20);
   this.savePassphrase.TabIndex = 8;
   this.savePassphrase.Text = resources.GetString("RememberPPText");
   this.btnCancel.Location = new System.Drawing.Point(319, 200);
   this.btnCancel.Name = "btnCancel";
   this.btnCancel.TabIndex = 10;
   this.btnCancel.Text = resources.GetString("cancel.Text");
   this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
   this.btnOk.Location = new System.Drawing.Point(239, 200);
   this.btnOk.Name = "btnOk";
   this.btnOk.TabIndex = 9;
   this.btnOk.Text = resources.GetString("ok.Text");
   this.btnOk.Click += new System.EventHandler(this.btnOk_Click);
   this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
   this.ClientSize = new System.Drawing.Size(420, 242);
   this.Controls.Add(this.btnOk);
   this.Controls.Add(this.btnCancel);
   this.Controls.Add(this.savePassphrase);
   this.Controls.Add(this.lblRetypePassphrase);
   this.Controls.Add(this.lblPassphrase);
   this.Controls.Add(this.RetypePassphrase);
   this.Controls.Add(this.Passphrase);
   this.Controls.Add(this.lblRecoveryAgent);
   this.Controls.Add(this.RecoveryAgentCombo);
   this.Controls.Add(this.panel1);
            this.MaximizeBox = false;
            this.AcceptButton = this.btnOk;
   this.Name = "EnterPassphraseDialog";
   this.Text = resources.GetString("EnterPPTitle");
   this.Load += new System.EventHandler(this.EnterPassphraseDialog_Load);
   this.panel1.ResumeLayout(false);
   this.ResumeLayout(false);
  }
  private void btnCancel_Click(object sender, System.EventArgs e)
  {
   simws.StorePassPhrase(DomainID, "", CredentialType.None, false);
   status = false;
   this.Dispose();
   this.Close();
  }
  private void btnOk_Click(object sender, System.EventArgs e)
  {
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(CreateiFolder));
   if( this.Passphrase.Text == this.RetypePassphrase.Text)
   {
    string publicKey = null;
    string ragent = null;
                if (this.RecoveryAgentCombo.SelectedItem != null && (string)this.RecoveryAgentCombo.SelectedItem != resources.GetString("serverDefaultRA"))
                {
                    byte[] CertificateObj = this.simws.GetRACertificateOnClient(DomainID, (string)this.RecoveryAgentCombo.SelectedItem);
                    System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(CertificateObj);
                    MyMessageBox mmb = new MyMessageBox(string.Format(resources.GetString("verifyCert"), (string)this.RecoveryAgentCombo.SelectedItem), resources.GetString("verifyCertTitle"), cert.ToString(true), MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question, MyMessageBoxDefaultButton.Button2);
                    DialogResult messageDialogResult = mmb.ShowDialog();
                    mmb.Dispose();
                    mmb.Close();
                    if (messageDialogResult != DialogResult.Yes)
                        return;
                    else
                    {
                        ragent = this.RecoveryAgentCombo.SelectedText;
                        publicKey = Convert.ToBase64String(cert.GetPublicKey());
                    }
                }
                else
                {
                    ragent = "DEFAULT";
                    DomainInformation domainInfo = (DomainInformation)this.simws.GetDomainInformation(this.DomainID);
                    string memberUID = domainInfo.MemberUserID;
                    publicKey = this.ifws.GetDefaultServerPublicKey(this.DomainID, memberUID);
                }
    Status passPhraseStatus = null;
    try
    {
     passPhraseStatus = simws.SetPassPhrase( DomainID, this.Passphrase.Text, ragent, publicKey);
    }
    catch(Exception ex)
    {
     MessageBox.Show(resources.GetString("ErrorSetPP") + ex.Message);
    }
    if(passPhraseStatus.statusCode == StatusCodes.Success)
    {
     simws.StorePassPhrase( DomainID, this.Passphrase.Text, CredentialType.Basic, this.savePassphrase.Checked);
     string passphr = simws.GetPassPhrase(DomainID);
     this.status= simws.IsPassPhraseSet(DomainID);
     if( status == true)
     {
      Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resources.GetString("SetPPSuccess") , resources.GetString("EnterPPTitle") , "",MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
      mmb.ShowDialog();
      mmb.Dispose();
      this.Dispose();
      this.Close();
     }
    }
    else
    {
     status = false;
     Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resources.GetString("ErrorSetPP") , resources.GetString("EnterPPTitle") , resources.GetString("TryAgain") , MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
     mmb.ShowDialog();
     mmb.Dispose();
    }
   }
   else
   {
    MessageBox.Show(resources.GetString("PPSDontMatch") );
    status = false;
   }
  }
  private void EnterPassphraseDialog_Load(object sender, System.EventArgs e)
  {
   this.btnOk.Enabled = false;
            this.Icon = new Icon(System.IO.Path.Combine(loadpath, @"res\ifolder_16.ico"));
   this.waterMark.Image = Image.FromFile(System.IO.Path.Combine(loadpath, @"res\ifolder-banner.png"));
   this.pictureBox1.SizeMode = PictureBoxSizeMode.StretchImage;
            this.pictureBox1.Image = Image.FromFile(System.IO.Path.Combine( loadpath, @"res\ifolder-banner-scaler.png"));
   string[] rAgents= this.simws.GetRAListOnClient(DomainID);
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(CreateiFolder));
            this.RecoveryAgentCombo.Items.Add(resources.GetString("serverDefaultRA"));
            foreach( string rAgent in rAgents)
   {
    this.RecoveryAgentCombo.Items.Add( rAgent );
   }
            this.RecoveryAgentCombo.SelectedIndex = 0;
  }
  private void Passphrase_TextChanged(object sender, System.EventArgs e)
  {
   UpdateSensitivity();
  }
  private void RetypePassphrase_TextChanged(object sender, System.EventArgs e)
  {
   UpdateSensitivity();
  }
  private void UpdateSensitivity()
  {
   if( this.Passphrase.Text.Length > 0 && this.Passphrase.Text == this.RetypePassphrase.Text)
    this.btnOk.Enabled = true;
   else
    this.btnOk.Enabled = false;
  }
 }
}
