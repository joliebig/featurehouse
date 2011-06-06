using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;
using System.Net;
using Novell.iFolderCom;
using Simias.Client;
using Simias.Client.Authentication;
namespace Novell.FormsTrayApp
{
 public class ServerInfo : System.Windows.Forms.Form
 {
  System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(ServerInfo));
  bool cancelled = false;
  bool updateStarted = false;
  private DomainInformation domainInfo;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button cancel;
  private System.Windows.Forms.TextBox password;
  private System.Windows.Forms.PictureBox banner;
  private SimiasWebService simiasWebService;
  private System.Windows.Forms.Label serverLabel2;
  private System.Windows.Forms.Label userLabel2;
  private System.Windows.Forms.Label passwordLabel2;
  private System.Windows.Forms.CheckBox rememberPassword;
  private System.Windows.Forms.TextBox userName;
  private System.Windows.Forms.TextBox serverName;
  private System.Windows.Forms.PictureBox bannerFill;
  private System.Windows.Forms.Panel panel1;
  private Manager simiasManager;
  private System.ComponentModel.Container components = null;
  public ServerInfo(Manager simiasManager, DomainInformation domainInfo, string password)
  {
   InitializeComponent();
   this.domainInfo = domainInfo;
   this.simiasManager = simiasManager;
   if (password != null)
   {
    this.password.Text = password;
    rememberPassword.Checked = true;
   }
   int delta = calculateSize(serverLabel2, 0);
   delta = calculateSize(userLabel2, delta);
   delta = calculateSize(passwordLabel2, delta);
   if (delta > 0)
   {
    serverLabel2.Width = userLabel2.Width = passwordLabel2.Width += delta;
    int temp = serverName.Left;
    serverName.Left = userName.Left = this.password.Left = rememberPassword.Left = serverLabel2.Left + serverLabel2.Width;
    serverName.Width = userName.Width = this.password.Width = rememberPassword.Width -= serverName.Left - temp;
   }
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(ServerInfo));
   this.ok = new System.Windows.Forms.Button();
   this.cancel = new System.Windows.Forms.Button();
   this.password = new System.Windows.Forms.TextBox();
   this.banner = new System.Windows.Forms.PictureBox();
   this.serverLabel2 = new System.Windows.Forms.Label();
   this.userLabel2 = new System.Windows.Forms.Label();
   this.passwordLabel2 = new System.Windows.Forms.Label();
   this.rememberPassword = new System.Windows.Forms.CheckBox();
   this.userName = new System.Windows.Forms.TextBox();
   this.serverName = new System.Windows.Forms.TextBox();
   this.bannerFill = new System.Windows.Forms.PictureBox();
   this.panel1 = new System.Windows.Forms.Panel();
   this.panel1.SuspendLayout();
   this.SuspendLayout();
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
   this.cancel.Click += new System.EventHandler(this.cancel_Click);
   this.password.AccessibleDescription = resources.GetString("password.AccessibleDescription");
   this.password.AccessibleName = resources.GetString("password.AccessibleName");
   this.password.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("password.Anchor")));
   this.password.AutoSize = ((bool)(resources.GetObject("password.AutoSize")));
   this.password.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("password.BackgroundImage")));
   this.password.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("password.Dock")));
   this.password.Enabled = ((bool)(resources.GetObject("password.Enabled")));
   this.password.Font = ((System.Drawing.Font)(resources.GetObject("password.Font")));
   this.password.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("password.ImeMode")));
   this.password.Location = ((System.Drawing.Point)(resources.GetObject("password.Location")));
   this.password.MaxLength = ((int)(resources.GetObject("password.MaxLength")));
   this.password.Multiline = ((bool)(resources.GetObject("password.Multiline")));
   this.password.Name = "password";
   this.password.PasswordChar = ((char)(resources.GetObject("password.PasswordChar")));
   this.password.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("password.RightToLeft")));
   this.password.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("password.ScrollBars")));
   this.password.Size = ((System.Drawing.Size)(resources.GetObject("password.Size")));
   this.password.TabIndex = ((int)(resources.GetObject("password.TabIndex")));
   this.password.Text = resources.GetString("password.Text");
   this.password.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("password.TextAlign")));
   this.password.Visible = ((bool)(resources.GetObject("password.Visible")));
   this.password.WordWrap = ((bool)(resources.GetObject("password.WordWrap")));
   this.banner.AccessibleDescription = resources.GetString("banner.AccessibleDescription");
   this.banner.AccessibleName = resources.GetString("banner.AccessibleName");
   this.banner.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("banner.Anchor")));
   this.banner.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("banner.BackgroundImage")));
   this.banner.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("banner.Dock")));
   this.banner.Enabled = ((bool)(resources.GetObject("banner.Enabled")));
   this.banner.Font = ((System.Drawing.Font)(resources.GetObject("banner.Font")));
   this.banner.Image = ((System.Drawing.Image)(resources.GetObject("banner.Image")));
   this.banner.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("banner.ImeMode")));
   this.banner.Location = ((System.Drawing.Point)(resources.GetObject("banner.Location")));
   this.banner.Name = "banner";
   this.banner.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("banner.RightToLeft")));
   this.banner.Size = ((System.Drawing.Size)(resources.GetObject("banner.Size")));
   this.banner.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("banner.SizeMode")));
   this.banner.TabIndex = ((int)(resources.GetObject("banner.TabIndex")));
   this.banner.TabStop = false;
   this.banner.Text = resources.GetString("banner.Text");
   this.banner.Visible = ((bool)(resources.GetObject("banner.Visible")));
   this.serverLabel2.AccessibleDescription = resources.GetString("serverLabel2.AccessibleDescription");
   this.serverLabel2.AccessibleName = resources.GetString("serverLabel2.AccessibleName");
   this.serverLabel2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("serverLabel2.Anchor")));
   this.serverLabel2.AutoSize = ((bool)(resources.GetObject("serverLabel2.AutoSize")));
   this.serverLabel2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("serverLabel2.Dock")));
   this.serverLabel2.Enabled = ((bool)(resources.GetObject("serverLabel2.Enabled")));
   this.serverLabel2.Font = ((System.Drawing.Font)(resources.GetObject("serverLabel2.Font")));
   this.serverLabel2.Image = ((System.Drawing.Image)(resources.GetObject("serverLabel2.Image")));
   this.serverLabel2.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverLabel2.ImageAlign")));
   this.serverLabel2.ImageIndex = ((int)(resources.GetObject("serverLabel2.ImageIndex")));
   this.serverLabel2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("serverLabel2.ImeMode")));
   this.serverLabel2.Location = ((System.Drawing.Point)(resources.GetObject("serverLabel2.Location")));
   this.serverLabel2.Name = "serverLabel2";
   this.serverLabel2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("serverLabel2.RightToLeft")));
   this.serverLabel2.Size = ((System.Drawing.Size)(resources.GetObject("serverLabel2.Size")));
   this.serverLabel2.TabIndex = ((int)(resources.GetObject("serverLabel2.TabIndex")));
   this.serverLabel2.Text = resources.GetString("serverLabel2.Text");
   this.serverLabel2.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverLabel2.TextAlign")));
   this.serverLabel2.Visible = ((bool)(resources.GetObject("serverLabel2.Visible")));
   this.userLabel2.AccessibleDescription = resources.GetString("userLabel2.AccessibleDescription");
   this.userLabel2.AccessibleName = resources.GetString("userLabel2.AccessibleName");
   this.userLabel2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("userLabel2.Anchor")));
   this.userLabel2.AutoSize = ((bool)(resources.GetObject("userLabel2.AutoSize")));
   this.userLabel2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("userLabel2.Dock")));
   this.userLabel2.Enabled = ((bool)(resources.GetObject("userLabel2.Enabled")));
   this.userLabel2.Font = ((System.Drawing.Font)(resources.GetObject("userLabel2.Font")));
   this.userLabel2.Image = ((System.Drawing.Image)(resources.GetObject("userLabel2.Image")));
   this.userLabel2.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("userLabel2.ImageAlign")));
   this.userLabel2.ImageIndex = ((int)(resources.GetObject("userLabel2.ImageIndex")));
   this.userLabel2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("userLabel2.ImeMode")));
   this.userLabel2.Location = ((System.Drawing.Point)(resources.GetObject("userLabel2.Location")));
   this.userLabel2.Name = "userLabel2";
   this.userLabel2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("userLabel2.RightToLeft")));
   this.userLabel2.Size = ((System.Drawing.Size)(resources.GetObject("userLabel2.Size")));
   this.userLabel2.TabIndex = ((int)(resources.GetObject("userLabel2.TabIndex")));
   this.userLabel2.Text = resources.GetString("userLabel2.Text");
   this.userLabel2.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("userLabel2.TextAlign")));
   this.userLabel2.Visible = ((bool)(resources.GetObject("userLabel2.Visible")));
   this.passwordLabel2.AccessibleDescription = resources.GetString("passwordLabel2.AccessibleDescription");
   this.passwordLabel2.AccessibleName = resources.GetString("passwordLabel2.AccessibleName");
   this.passwordLabel2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("passwordLabel2.Anchor")));
   this.passwordLabel2.AutoSize = ((bool)(resources.GetObject("passwordLabel2.AutoSize")));
   this.passwordLabel2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("passwordLabel2.Dock")));
   this.passwordLabel2.Enabled = ((bool)(resources.GetObject("passwordLabel2.Enabled")));
   this.passwordLabel2.Font = ((System.Drawing.Font)(resources.GetObject("passwordLabel2.Font")));
   this.passwordLabel2.Image = ((System.Drawing.Image)(resources.GetObject("passwordLabel2.Image")));
   this.passwordLabel2.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("passwordLabel2.ImageAlign")));
   this.passwordLabel2.ImageIndex = ((int)(resources.GetObject("passwordLabel2.ImageIndex")));
   this.passwordLabel2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("passwordLabel2.ImeMode")));
   this.passwordLabel2.Location = ((System.Drawing.Point)(resources.GetObject("passwordLabel2.Location")));
   this.passwordLabel2.Name = "passwordLabel2";
   this.passwordLabel2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("passwordLabel2.RightToLeft")));
   this.passwordLabel2.Size = ((System.Drawing.Size)(resources.GetObject("passwordLabel2.Size")));
   this.passwordLabel2.TabIndex = ((int)(resources.GetObject("passwordLabel2.TabIndex")));
   this.passwordLabel2.Text = resources.GetString("passwordLabel2.Text");
   this.passwordLabel2.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("passwordLabel2.TextAlign")));
   this.passwordLabel2.Visible = ((bool)(resources.GetObject("passwordLabel2.Visible")));
   this.rememberPassword.AccessibleDescription = resources.GetString("rememberPassword.AccessibleDescription");
   this.rememberPassword.AccessibleName = resources.GetString("rememberPassword.AccessibleName");
   this.rememberPassword.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("rememberPassword.Anchor")));
   this.rememberPassword.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("rememberPassword.Appearance")));
   this.rememberPassword.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("rememberPassword.BackgroundImage")));
   this.rememberPassword.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("rememberPassword.CheckAlign")));
   this.rememberPassword.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("rememberPassword.Dock")));
   this.rememberPassword.Enabled = ((bool)(resources.GetObject("rememberPassword.Enabled")));
   this.rememberPassword.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("rememberPassword.FlatStyle")));
   this.rememberPassword.Font = ((System.Drawing.Font)(resources.GetObject("rememberPassword.Font")));
   this.rememberPassword.Image = ((System.Drawing.Image)(resources.GetObject("rememberPassword.Image")));
   this.rememberPassword.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("rememberPassword.ImageAlign")));
   this.rememberPassword.ImageIndex = ((int)(resources.GetObject("rememberPassword.ImageIndex")));
   this.rememberPassword.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("rememberPassword.ImeMode")));
   this.rememberPassword.Location = ((System.Drawing.Point)(resources.GetObject("rememberPassword.Location")));
   this.rememberPassword.Name = "rememberPassword";
   this.rememberPassword.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("rememberPassword.RightToLeft")));
   this.rememberPassword.Size = ((System.Drawing.Size)(resources.GetObject("rememberPassword.Size")));
   this.rememberPassword.TabIndex = ((int)(resources.GetObject("rememberPassword.TabIndex")));
   this.rememberPassword.Text = resources.GetString("rememberPassword.Text");
   this.rememberPassword.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("rememberPassword.TextAlign")));
   this.rememberPassword.Visible = ((bool)(resources.GetObject("rememberPassword.Visible")));
   this.userName.AccessibleDescription = resources.GetString("userName.AccessibleDescription");
   this.userName.AccessibleName = resources.GetString("userName.AccessibleName");
   this.userName.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("userName.Anchor")));
   this.userName.AutoSize = ((bool)(resources.GetObject("userName.AutoSize")));
   this.userName.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("userName.BackgroundImage")));
   this.userName.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("userName.Dock")));
   this.userName.Enabled = ((bool)(resources.GetObject("userName.Enabled")));
   this.userName.Font = ((System.Drawing.Font)(resources.GetObject("userName.Font")));
   this.userName.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("userName.ImeMode")));
   this.userName.Location = ((System.Drawing.Point)(resources.GetObject("userName.Location")));
   this.userName.MaxLength = ((int)(resources.GetObject("userName.MaxLength")));
   this.userName.Multiline = ((bool)(resources.GetObject("userName.Multiline")));
   this.userName.Name = "userName";
   this.userName.PasswordChar = ((char)(resources.GetObject("userName.PasswordChar")));
   this.userName.ReadOnly = true;
   this.userName.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("userName.RightToLeft")));
   this.userName.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("userName.ScrollBars")));
   this.userName.Size = ((System.Drawing.Size)(resources.GetObject("userName.Size")));
   this.userName.TabIndex = ((int)(resources.GetObject("userName.TabIndex")));
   this.userName.Text = resources.GetString("userName.Text");
   this.userName.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("userName.TextAlign")));
   this.userName.Visible = ((bool)(resources.GetObject("userName.Visible")));
   this.userName.WordWrap = ((bool)(resources.GetObject("userName.WordWrap")));
   this.serverName.AccessibleDescription = resources.GetString("serverName.AccessibleDescription");
   this.serverName.AccessibleName = resources.GetString("serverName.AccessibleName");
   this.serverName.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("serverName.Anchor")));
   this.serverName.AutoSize = ((bool)(resources.GetObject("serverName.AutoSize")));
   this.serverName.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("serverName.BackgroundImage")));
   this.serverName.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("serverName.Dock")));
   this.serverName.Enabled = ((bool)(resources.GetObject("serverName.Enabled")));
   this.serverName.Font = ((System.Drawing.Font)(resources.GetObject("serverName.Font")));
   this.serverName.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("serverName.ImeMode")));
   this.serverName.Location = ((System.Drawing.Point)(resources.GetObject("serverName.Location")));
   this.serverName.MaxLength = ((int)(resources.GetObject("serverName.MaxLength")));
   this.serverName.Multiline = ((bool)(resources.GetObject("serverName.Multiline")));
   this.serverName.Name = "serverName";
   this.serverName.PasswordChar = ((char)(resources.GetObject("serverName.PasswordChar")));
   this.serverName.ReadOnly = true;
   this.serverName.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("serverName.RightToLeft")));
   this.serverName.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("serverName.ScrollBars")));
   this.serverName.Size = ((System.Drawing.Size)(resources.GetObject("serverName.Size")));
   this.serverName.TabIndex = ((int)(resources.GetObject("serverName.TabIndex")));
   this.serverName.Text = resources.GetString("serverName.Text");
   this.serverName.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("serverName.TextAlign")));
   this.serverName.Visible = ((bool)(resources.GetObject("serverName.Visible")));
   this.serverName.WordWrap = ((bool)(resources.GetObject("serverName.WordWrap")));
   this.bannerFill.AccessibleDescription = resources.GetString("bannerFill.AccessibleDescription");
   this.bannerFill.AccessibleName = resources.GetString("bannerFill.AccessibleName");
   this.bannerFill.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("bannerFill.Anchor")));
   this.bannerFill.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("bannerFill.BackgroundImage")));
   this.bannerFill.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("bannerFill.Dock")));
   this.bannerFill.Enabled = ((bool)(resources.GetObject("bannerFill.Enabled")));
   this.bannerFill.Font = ((System.Drawing.Font)(resources.GetObject("bannerFill.Font")));
   this.bannerFill.Image = ((System.Drawing.Image)(resources.GetObject("bannerFill.Image")));
   this.bannerFill.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("bannerFill.ImeMode")));
   this.bannerFill.Location = ((System.Drawing.Point)(resources.GetObject("bannerFill.Location")));
   this.bannerFill.Name = "bannerFill";
   this.bannerFill.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("bannerFill.RightToLeft")));
   this.bannerFill.Size = ((System.Drawing.Size)(resources.GetObject("bannerFill.Size")));
   this.bannerFill.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("bannerFill.SizeMode")));
   this.bannerFill.TabIndex = ((int)(resources.GetObject("bannerFill.TabIndex")));
   this.bannerFill.TabStop = false;
   this.bannerFill.Text = resources.GetString("bannerFill.Text");
   this.bannerFill.Visible = ((bool)(resources.GetObject("bannerFill.Visible")));
   this.panel1.AccessibleDescription = resources.GetString("panel1.AccessibleDescription");
   this.panel1.AccessibleName = resources.GetString("panel1.AccessibleName");
   this.panel1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("panel1.Anchor")));
   this.panel1.AutoScroll = ((bool)(resources.GetObject("panel1.AutoScroll")));
   this.panel1.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("panel1.AutoScrollMargin")));
   this.panel1.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("panel1.AutoScrollMinSize")));
   this.panel1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("panel1.BackgroundImage")));
   this.panel1.Controls.Add(this.banner);
   this.panel1.Controls.Add(this.bannerFill);
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
   this.Controls.Add(this.panel1);
   this.Controls.Add(this.serverName);
   this.Controls.Add(this.userName);
   this.Controls.Add(this.password);
   this.Controls.Add(this.rememberPassword);
   this.Controls.Add(this.passwordLabel2);
   this.Controls.Add(this.userLabel2);
   this.Controls.Add(this.serverLabel2);
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.ok);
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
   this.Name = "ServerInfo";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.Load += new System.EventHandler(this.ServerInfo_Load);
   this.Activated += new System.EventHandler(this.ServerInfo_Activated);
   this.panel1.ResumeLayout(false);
   this.ResumeLayout(false);
  }
  public bool Cancelled
  {
   get { return cancelled; }
  }
  public bool UpdateStarted
  {
   get { return updateStarted; }
  }
  public DomainInformation DomainInfo
  {
   get { return domainInfo; }
  }
  private bool authenticate()
  {
   bool result = false;
   Cursor.Current = Cursors.WaitCursor;
   try
   {
    DomainAuthentication domainAuth = new DomainAuthentication("iFolder", domainInfo.ID, password.Text);
    Status authStatus = domainAuth.Authenticate(simiasManager.WebServiceUri, simiasManager.DataPath);
    MyMessageBox mmb;
    switch (authStatus.statusCode)
    {
     case StatusCodes.InvalidCertificate:
      byte[] byteArray = simiasWebService.GetCertificate(domainInfo.Host);
      System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(byteArray);
      mmb = new MyMessageBox(string.Format(resourceManager.GetString("verifyCert"), domainInfo.Host), resourceManager.GetString("verifyCertTitle"), cert.ToString(true), MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question, MyMessageBoxDefaultButton.Button2);
      if (mmb.ShowDialog() == DialogResult.Yes)
      {
       simiasWebService.StoreCertificate(byteArray, domainInfo.Host);
       result = authenticate();
      }
      break;
     case StatusCodes.Success:
     case StatusCodes.SuccessInGrace:
      if (authStatus.statusCode.Equals(StatusCodes.SuccessInGrace))
      {
       mmb = new MyMessageBox(
        string.Format(resourceManager.GetString("graceLogin"), authStatus.RemainingGraceLogins),
        resourceManager.GetString("graceLoginTitle"),
        string.Empty,
        MyMessageBoxButtons.OK,
        MyMessageBoxIcon.Information);
       mmb.ShowDialog();
      }
      try
      {
       updateStarted = FormsTrayApp.CheckForClientUpdate(domainInfo.ID);
      }
      catch
      {
      }
      if (rememberPassword.Checked)
      {
       try
       {
        simiasWebService.SetDomainCredentials(domainInfo.ID, password.Text, CredentialType.Basic);
       }
       catch (Exception ex)
       {
        mmb = new MyMessageBox(resourceManager.GetString("savePasswordError"), string.Empty, ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
        mmb.ShowDialog();
       }
      }
      result = true;
      break;
     case StatusCodes.InvalidCredentials:
     case StatusCodes.InvalidPassword:
     case StatusCodes.UnknownUser:
      mmb = new MyMessageBox(resourceManager.GetString("badPassword"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
      mmb.ShowDialog();
      break;
     case StatusCodes.AccountDisabled:
      mmb = new MyMessageBox(resourceManager.GetString("accountDisabled"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
      mmb.ShowDialog();
      break;
     case StatusCodes.AccountLockout:
      mmb = new MyMessageBox(resourceManager.GetString("accountLockout"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
      mmb.ShowDialog();
      break;
     case StatusCodes.SimiasLoginDisabled:
      mmb = new MyMessageBox(resourceManager.GetString("iFolderAccountDisabled"), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
      mmb.ShowDialog();
      break;
     default:
      mmb = new MyMessageBox(string.Format(resourceManager.GetString("serverReconnectError"), authStatus.statusCode.ToString()), resourceManager.GetString("serverConnectErrorTitle"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
      mmb.ShowDialog();
      break;
    }
   }
   catch (Exception ex)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("serverConnectError"), resourceManager.GetString("serverConnectErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
   Cursor.Current = Cursors.Default;
   return result;
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
  private void cancel_Click(object sender, System.EventArgs e)
  {
   cancelled = true;
   Close();
  }
  private void ok_Click(object sender, System.EventArgs e)
  {
   if (authenticate())
   {
    domainInfo.Authenticated = true;
    password.Clear();
    Close();
   }
   else
   {
    password.Focus();
    if (!password.Text.Equals(string.Empty))
    {
     password.SelectAll();
    }
   }
  }
  private void ServerInfo_Load(object sender, System.EventArgs e)
  {
   try
   {
    this.Icon = new Icon(Path.Combine(Application.StartupPath, @"ifolder_app.ico"));
    banner.Image = Image.FromFile(Path.Combine(Application.StartupPath, @"res\ifolder-banner.png"));
    bannerFill.SizeMode = PictureBoxSizeMode.StretchImage;
    bannerFill.Image = Image.FromFile(Path.Combine(Application.StartupPath, @"res\ifolder-banner-scaler.png"));
   }
   catch
   {
   }
   try
   {
    simiasWebService = new SimiasWebService();
    simiasWebService.Url = simiasManager.WebServiceUri.ToString() + "/Simias.asmx";
    LocalService.Start(simiasWebService, simiasManager.WebServiceUri, simiasManager.DataPath);
    if (domainInfo != null)
    {
     serverName.Text = domainInfo.Name;
     userName.Text = domainInfo.MemberName;
    }
   }
   catch (Exception ex)
   {
    Novell.iFolderCom.MyMessageBox mmb = new Novell.iFolderCom.MyMessageBox(resourceManager.GetString("domainInfoReadError"), string.Empty, ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
  }
  private void ServerInfo_Activated(object sender, System.EventArgs e)
  {
   password.Focus();
  }
 }
}
