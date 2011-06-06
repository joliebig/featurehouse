using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Xml;
using Novell.iFolderCom;
using System.IO;
using Novell.iFolder.Web;
namespace Novell.FormsTrayApp
{
 public class ResetPassword : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Panel panel;
  private System.Windows.Forms.PictureBox waterMark;
  private System.Windows.Forms.Label accountLabel;
  private System.Windows.Forms.Label passwordLabel;
  private System.Windows.Forms.Label newPasswordLabel;
  private System.Windows.Forms.Label confirmPasswordLabel;
  private System.Windows.Forms.ComboBox DomainComboBox;
  private System.Windows.Forms.TextBox oldpassword;
  private System.Windows.Forms.TextBox newPassword;
  private System.Windows.Forms.TextBox confirmPassword;
  private System.Windows.Forms.CheckBox rememberPassword;
  private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button buttonHelp;
        private System.Windows.Forms.Button btnReset;
  private SimiasWebService simws;
        private iFolderWebService ifws;
  private string domainID;
  private bool success;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(FormsTrayApp));
  private System.Windows.Forms.PictureBox pictureBox;
  private System.ComponentModel.Container components = null;
  public SimiasWebService simiasWebservice
  {
   get
   {
    return this.simws;
   }
   set
   {
    this.simws = value;
   }
  }
  public string DomainID
  {
   get
   {
    DomainItem domainItem = (DomainItem)this.DomainComboBox.SelectedItem;
    this.domainID = domainItem.ID;
    return this.domainID;
   }
  }
  public int DomainCount
  {
   get
   {
    return this.DomainComboBox.Items.Count;
   }
  }
        public ResetPassword(SimiasWebService simws, iFolderWebService ifws)
        {
            InitializeComponent();
            this.simws = simws;
            this.ifws = ifws;
            GetLoggedInDomains();
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(ResetPassword));
   this.panel = new System.Windows.Forms.Panel();
   this.pictureBox = new System.Windows.Forms.PictureBox();
   this.waterMark = new System.Windows.Forms.PictureBox();
   this.accountLabel = new System.Windows.Forms.Label();
   this.passwordLabel = new System.Windows.Forms.Label();
   this.newPasswordLabel = new System.Windows.Forms.Label();
   this.confirmPasswordLabel = new System.Windows.Forms.Label();
   this.DomainComboBox = new System.Windows.Forms.ComboBox();
   this.oldpassword = new System.Windows.Forms.TextBox();
   this.newPassword = new System.Windows.Forms.TextBox();
   this.confirmPassword = new System.Windows.Forms.TextBox();
   this.rememberPassword = new System.Windows.Forms.CheckBox();
   this.btnCancel = new System.Windows.Forms.Button();
   this.btnReset = new System.Windows.Forms.Button();
            this.buttonHelp = new System.Windows.Forms.Button();
            this.panel.SuspendLayout();
   this.SuspendLayout();
   this.panel.AccessibleDescription = resources.GetString("panel1.AccessibleDescription");
   this.panel.AccessibleName = resources.GetString("panel1.AccessibleName");
   this.panel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("panel1.Anchor")));
   this.panel.AutoScroll = ((bool)(resources.GetObject("panel1.AutoScroll")));
   this.panel.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("panel1.AutoScrollMargin")));
   this.panel.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("panel1.AutoScrollMinSize")));
   this.panel.BackColor = System.Drawing.Color.Transparent;
   this.panel.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("panel1.BackgroundImage")));
   this.panel.Controls.Add(this.pictureBox);
   this.panel.Controls.Add(this.waterMark);
   this.panel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("panel1.Dock")));
   this.panel.Enabled = ((bool)(resources.GetObject("panel1.Enabled")));
   this.panel.Font = ((System.Drawing.Font)(resources.GetObject("panel1.Font")));
   this.panel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("panel1.ImeMode")));
   this.panel.Location = ((System.Drawing.Point)(resources.GetObject("panel1.Location")));
   this.panel.Name = "panel1";
   this.panel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("panel1.RightToLeft")));
   this.panel.Size = ((System.Drawing.Size)(resources.GetObject("panel1.Size")));
   this.panel.TabIndex = ((int)(resources.GetObject("panel1.TabIndex")));
   this.panel.Text = resources.GetString("panel1.Text");
   this.panel.Visible = ((bool)(resources.GetObject("panel1.Visible")));
   this.pictureBox.AccessibleDescription = resources.GetString("pictureBox1.AccessibleDescription");
   this.pictureBox.AccessibleName = resources.GetString("pictureBox1.AccessibleName");
   this.pictureBox.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("pictureBox1.Anchor")));
   this.pictureBox.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("pictureBox1.BackgroundImage")));
   this.pictureBox.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("pictureBox1.Dock")));
   this.pictureBox.Enabled = ((bool)(resources.GetObject("pictureBox1.Enabled")));
   this.pictureBox.Font = ((System.Drawing.Font)(resources.GetObject("pictureBox1.Font")));
   this.pictureBox.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
   this.pictureBox.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("pictureBox1.ImeMode")));
   this.pictureBox.Location = ((System.Drawing.Point)(resources.GetObject("pictureBox1.Location")));
   this.pictureBox.Name = "pictureBox1";
   this.pictureBox.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("pictureBox1.RightToLeft")));
   this.pictureBox.Size = ((System.Drawing.Size)(resources.GetObject("pictureBox1.Size")));
   this.pictureBox.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("pictureBox1.SizeMode")));
   this.pictureBox.TabIndex = ((int)(resources.GetObject("pictureBox1.TabIndex")));
   this.pictureBox.TabStop = false;
   this.pictureBox.Text = resources.GetString("pictureBox1.Text");
   this.pictureBox.Visible = ((bool)(resources.GetObject("pictureBox1.Visible")));
   this.waterMark.AccessibleDescription = resources.GetString("waterMark.AccessibleDescription");
   this.waterMark.AccessibleName = resources.GetString("waterMark.AccessibleName");
   this.waterMark.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("waterMark.Anchor")));
   this.waterMark.BackColor = System.Drawing.Color.Transparent;
   this.waterMark.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("waterMark.BackgroundImage")));
   this.waterMark.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("waterMark.Dock")));
   this.waterMark.Enabled = ((bool)(resources.GetObject("waterMark.Enabled")));
   this.waterMark.Font = ((System.Drawing.Font)(resources.GetObject("waterMark.Font")));
   this.waterMark.Image = ((System.Drawing.Image)(resources.GetObject("waterMark.Image")));
   this.waterMark.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("waterMark.ImeMode")));
   this.waterMark.Location = ((System.Drawing.Point)(resources.GetObject("waterMark.Location")));
   this.waterMark.Name = "waterMark";
   this.waterMark.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("waterMark.RightToLeft")));
   this.waterMark.Size = ((System.Drawing.Size)(resources.GetObject("waterMark.Size")));
   this.waterMark.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("waterMark.SizeMode")));
   this.waterMark.TabIndex = ((int)(resources.GetObject("waterMark.TabIndex")));
   this.waterMark.TabStop = false;
   this.waterMark.Text = resources.GetString("waterMark.Text");
   this.waterMark.Visible = ((bool)(resources.GetObject("waterMark.Visible")));
   this.accountLabel.AccessibleDescription = resources.GetString("accountLabel.AccessibleDescription");
   this.accountLabel.AccessibleName = resources.GetString("accountLabel.AccessibleName");
   this.accountLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("accountLabel.Anchor")));
   this.accountLabel.AutoSize = ((bool)(resources.GetObject("accountLabel.AutoSize")));
   this.accountLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("accountLabel.Dock")));
   this.accountLabel.Enabled = ((bool)(resources.GetObject("accountLabel.Enabled")));
   this.accountLabel.Font = ((System.Drawing.Font)(resources.GetObject("accountLabel.Font")));
   this.accountLabel.Image = ((System.Drawing.Image)(resources.GetObject("accountLabel.Image")));
   this.accountLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("accountLabel.ImageAlign")));
   this.accountLabel.ImageIndex = ((int)(resources.GetObject("accountLabel.ImageIndex")));
   this.accountLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("accountLabel.ImeMode")));
   this.accountLabel.Location = ((System.Drawing.Point)(resources.GetObject("accountLabel.Location")));
   this.accountLabel.Name = "accountLabel";
   this.accountLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("accountLabel.RightToLeft")));
   this.accountLabel.Size = ((System.Drawing.Size)(resources.GetObject("accountLabel.Size")));
   this.accountLabel.TabIndex = ((int)(resources.GetObject("accountLabel.TabIndex")));
   this.accountLabel.Text = resources.GetString("accountLabel.Text");
   this.accountLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("accountLabel.TextAlign")));
   this.accountLabel.Visible = ((bool)(resources.GetObject("accountLabel.Visible")));
   this.passwordLabel.AccessibleDescription = resources.GetString("passphraseLabel.AccessibleDescription");
   this.passwordLabel.AccessibleName = resources.GetString("passphraseLabel.AccessibleName");
   this.passwordLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("passphraseLabel.Anchor")));
   this.passwordLabel.AutoSize = ((bool)(resources.GetObject("passphraseLabel.AutoSize")));
   this.passwordLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("passphraseLabel.Dock")));
   this.passwordLabel.Enabled = ((bool)(resources.GetObject("passphraseLabel.Enabled")));
   this.passwordLabel.Font = ((System.Drawing.Font)(resources.GetObject("passphraseLabel.Font")));
   this.passwordLabel.Image = ((System.Drawing.Image)(resources.GetObject("passphraseLabel.Image")));
   this.passwordLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("passphraseLabel.ImageAlign")));
   this.passwordLabel.ImageIndex = ((int)(resources.GetObject("passphraseLabel.ImageIndex")));
   this.passwordLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("passphraseLabel.ImeMode")));
   this.passwordLabel.Location = ((System.Drawing.Point)(resources.GetObject("passphraseLabel.Location")));
   this.passwordLabel.Name = "passphraseLabel";
   this.passwordLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("passphraseLabel.RightToLeft")));
   this.passwordLabel.Size = ((System.Drawing.Size)(resources.GetObject("passphraseLabel.Size")));
   this.passwordLabel.TabIndex = ((int)(resources.GetObject("passphraseLabel.TabIndex")));
            this.passwordLabel.Text = resources.GetString("CurrentPasswordLabelText");
   this.passwordLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("passphraseLabel.TextAlign")));
   this.passwordLabel.Visible = ((bool)(resources.GetObject("passphraseLabel.Visible")));
   this.newPasswordLabel.AccessibleDescription = resources.GetString("newPassphraseLabel.AccessibleDescription");
   this.newPasswordLabel.AccessibleName = resources.GetString("newPassphraseLabel.AccessibleName");
   this.newPasswordLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("newPassphraseLabel.Anchor")));
   this.newPasswordLabel.AutoSize = ((bool)(resources.GetObject("newPassphraseLabel.AutoSize")));
   this.newPasswordLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("newPassphraseLabel.Dock")));
   this.newPasswordLabel.Enabled = ((bool)(resources.GetObject("newPassphraseLabel.Enabled")));
   this.newPasswordLabel.Font = ((System.Drawing.Font)(resources.GetObject("newPassphraseLabel.Font")));
   this.newPasswordLabel.Image = ((System.Drawing.Image)(resources.GetObject("newPassphraseLabel.Image")));
   this.newPasswordLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("newPassphraseLabel.ImageAlign")));
   this.newPasswordLabel.ImageIndex = ((int)(resources.GetObject("newPassphraseLabel.ImageIndex")));
   this.newPasswordLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("newPassphraseLabel.ImeMode")));
   this.newPasswordLabel.Location = ((System.Drawing.Point)(resources.GetObject("newPassphraseLabel.Location")));
   this.newPasswordLabel.Name = "newPassphraseLabel";
   this.newPasswordLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("newPassphraseLabel.RightToLeft")));
   this.newPasswordLabel.Size = ((System.Drawing.Size)(resources.GetObject("newPassphraseLabel.Size")));
   this.newPasswordLabel.TabIndex = ((int)(resources.GetObject("newPassphraseLabel.TabIndex")));
            this.newPasswordLabel.Text = resources.GetString("NewPasswordLabelText");
   this.newPasswordLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("newPassphraseLabel.TextAlign")));
   this.newPasswordLabel.Visible = ((bool)(resources.GetObject("newPassphraseLabel.Visible")));
   this.confirmPasswordLabel.AccessibleDescription = resources.GetString("retypePassphraseLabel.AccessibleDescription");
   this.confirmPasswordLabel.AccessibleName = resources.GetString("retypePassphraseLabel.AccessibleName");
   this.confirmPasswordLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("retypePassphraseLabel.Anchor")));
   this.confirmPasswordLabel.AutoSize = ((bool)(resources.GetObject("retypePassphraseLabel.AutoSize")));
   this.confirmPasswordLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("retypePassphraseLabel.Dock")));
   this.confirmPasswordLabel.Enabled = ((bool)(resources.GetObject("retypePassphraseLabel.Enabled")));
   this.confirmPasswordLabel.Font = ((System.Drawing.Font)(resources.GetObject("retypePassphraseLabel.Font")));
   this.confirmPasswordLabel.Image = ((System.Drawing.Image)(resources.GetObject("retypePassphraseLabel.Image")));
   this.confirmPasswordLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("retypePassphraseLabel.ImageAlign")));
   this.confirmPasswordLabel.ImageIndex = ((int)(resources.GetObject("retypePassphraseLabel.ImageIndex")));
   this.confirmPasswordLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("retypePassphraseLabel.ImeMode")));
   this.confirmPasswordLabel.Location = ((System.Drawing.Point)(resources.GetObject("retypePassphraseLabel.Location")));
   this.confirmPasswordLabel.Name = "retypePassphraseLabel";
   this.confirmPasswordLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("retypePassphraseLabel.RightToLeft")));
   this.confirmPasswordLabel.Size = ((System.Drawing.Size)(resources.GetObject("retypePassphraseLabel.Size")));
   this.confirmPasswordLabel.TabIndex = ((int)(resources.GetObject("retypePassphraseLabel.TabIndex")));
            this.confirmPasswordLabel.Text = resources.GetString("ConfirmPasswordLabelText");
   this.confirmPasswordLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("retypePassphraseLabel.TextAlign")));
   this.confirmPasswordLabel.Visible = ((bool)(resources.GetObject("retypePassphraseLabel.Visible")));
   this.DomainComboBox.AccessibleDescription = resources.GetString("DomainComboBox.AccessibleDescription");
   this.DomainComboBox.AccessibleName = resources.GetString("DomainComboBox.AccessibleName");
   this.DomainComboBox.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("DomainComboBox.Anchor")));
   this.DomainComboBox.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("DomainComboBox.BackgroundImage")));
   this.DomainComboBox.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("DomainComboBox.Dock")));
   this.DomainComboBox.Enabled = ((bool)(resources.GetObject("DomainComboBox.Enabled")));
   this.DomainComboBox.Font = ((System.Drawing.Font)(resources.GetObject("DomainComboBox.Font")));
   this.DomainComboBox.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("DomainComboBox.ImeMode")));
   this.DomainComboBox.IntegralHeight = ((bool)(resources.GetObject("DomainComboBox.IntegralHeight")));
   this.DomainComboBox.ItemHeight = ((int)(resources.GetObject("DomainComboBox.ItemHeight")));
   this.DomainComboBox.Location = ((System.Drawing.Point)(resources.GetObject("DomainComboBox.Location")));
   this.DomainComboBox.MaxDropDownItems = ((int)(resources.GetObject("DomainComboBox.MaxDropDownItems")));
   this.DomainComboBox.MaxLength = ((int)(resources.GetObject("DomainComboBox.MaxLength")));
   this.DomainComboBox.Name = "DomainComboBox";
   this.DomainComboBox.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("DomainComboBox.RightToLeft")));
   this.DomainComboBox.Size = ((System.Drawing.Size)(resources.GetObject("DomainComboBox.Size")));
   this.DomainComboBox.TabIndex = ((int)(resources.GetObject("DomainComboBox.TabIndex")));
   this.DomainComboBox.Text = resources.GetString("DomainComboBox.Text");
   this.DomainComboBox.Visible = ((bool)(resources.GetObject("DomainComboBox.Visible")));
   this.oldpassword.AccessibleDescription = resources.GetString("passPhrase.AccessibleDescription");
   this.oldpassword.AccessibleName = resources.GetString("passPhrase.AccessibleName");
   this.oldpassword.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("passPhrase.Anchor")));
   this.oldpassword.AutoSize = ((bool)(resources.GetObject("passPhrase.AutoSize")));
   this.oldpassword.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("passPhrase.BackgroundImage")));
   this.oldpassword.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("passPhrase.Dock")));
   this.oldpassword.Enabled = ((bool)(resources.GetObject("passPhrase.Enabled")));
   this.oldpassword.Font = ((System.Drawing.Font)(resources.GetObject("passPhrase.Font")));
   this.oldpassword.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("passPhrase.ImeMode")));
   this.oldpassword.Location = ((System.Drawing.Point)(resources.GetObject("passPhrase.Location")));
   this.oldpassword.MaxLength = ((int)(resources.GetObject("passPhrase.MaxLength")));
   this.oldpassword.Multiline = ((bool)(resources.GetObject("passPhrase.Multiline")));
   this.oldpassword.Name = "passPhrase";
   this.oldpassword.PasswordChar = ((char)(resources.GetObject("passPhrase.PasswordChar")));
   this.oldpassword.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("passPhrase.RightToLeft")));
   this.oldpassword.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("passPhrase.ScrollBars")));
   this.oldpassword.Size = ((System.Drawing.Size)(resources.GetObject("passPhrase.Size")));
   this.oldpassword.TabIndex = ((int)(resources.GetObject("passPhrase.TabIndex")));
            this.oldpassword.Text = "";
   this.oldpassword.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("passPhrase.TextAlign")));
   this.oldpassword.Visible = ((bool)(resources.GetObject("passPhrase.Visible")));
   this.oldpassword.WordWrap = ((bool)(resources.GetObject("passPhrase.WordWrap")));
   this.oldpassword.TextChanged += new System.EventHandler(this.password_TextChanged);
   this.newPassword.AccessibleDescription = resources.GetString("newPassphrase.AccessibleDescription");
   this.newPassword.AccessibleName = resources.GetString("newPassphrase.AccessibleName");
   this.newPassword.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("newPassphrase.Anchor")));
   this.newPassword.AutoSize = ((bool)(resources.GetObject("newPassphrase.AutoSize")));
   this.newPassword.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("newPassphrase.BackgroundImage")));
   this.newPassword.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("newPassphrase.Dock")));
   this.newPassword.Enabled = ((bool)(resources.GetObject("newPassphrase.Enabled")));
   this.newPassword.Font = ((System.Drawing.Font)(resources.GetObject("newPassphrase.Font")));
   this.newPassword.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("newPassphrase.ImeMode")));
   this.newPassword.Location = ((System.Drawing.Point)(resources.GetObject("newPassphrase.Location")));
   this.newPassword.MaxLength = ((int)(resources.GetObject("newPassphrase.MaxLength")));
   this.newPassword.Multiline = ((bool)(resources.GetObject("newPassphrase.Multiline")));
   this.newPassword.Name = "newPassphrase";
   this.newPassword.PasswordChar = ((char)(resources.GetObject("newPassphrase.PasswordChar")));
   this.newPassword.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("newPassphrase.RightToLeft")));
   this.newPassword.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("newPassphrase.ScrollBars")));
   this.newPassword.Size = ((System.Drawing.Size)(resources.GetObject("newPassphrase.Size")));
   this.newPassword.TabIndex = ((int)(resources.GetObject("newPassphrase.TabIndex")));
            this.newPassword.Text = "";
   this.newPassword.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("newPassphrase.TextAlign")));
   this.newPassword.Visible = ((bool)(resources.GetObject("newPassphrase.Visible")));
   this.newPassword.WordWrap = ((bool)(resources.GetObject("newPassphrase.WordWrap")));
   this.newPassword.TextChanged += new System.EventHandler(this.newPassword_TextChanged);
   this.confirmPassword.AccessibleDescription = resources.GetString("retypePassphrase.AccessibleDescription");
   this.confirmPassword.AccessibleName = resources.GetString("retypePassphrase.AccessibleName");
   this.confirmPassword.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("retypePassphrase.Anchor")));
   this.confirmPassword.AutoSize = ((bool)(resources.GetObject("retypePassphrase.AutoSize")));
   this.confirmPassword.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("retypePassphrase.BackgroundImage")));
   this.confirmPassword.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("retypePassphrase.Dock")));
   this.confirmPassword.Enabled = ((bool)(resources.GetObject("retypePassphrase.Enabled")));
   this.confirmPassword.Font = ((System.Drawing.Font)(resources.GetObject("retypePassphrase.Font")));
   this.confirmPassword.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("retypePassphrase.ImeMode")));
   this.confirmPassword.Location = ((System.Drawing.Point)(resources.GetObject("retypePassphrase.Location")));
   this.confirmPassword.MaxLength = ((int)(resources.GetObject("retypePassphrase.MaxLength")));
   this.confirmPassword.Multiline = ((bool)(resources.GetObject("retypePassphrase.Multiline")));
   this.confirmPassword.Name = "retypePassphrase";
   this.confirmPassword.PasswordChar = ((char)(resources.GetObject("retypePassphrase.PasswordChar")));
   this.confirmPassword.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("retypePassphrase.RightToLeft")));
   this.confirmPassword.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("retypePassphrase.ScrollBars")));
   this.confirmPassword.Size = ((System.Drawing.Size)(resources.GetObject("retypePassphrase.Size")));
   this.confirmPassword.TabIndex = ((int)(resources.GetObject("retypePassphrase.TabIndex")));
            this.confirmPassword.Text = "";
   this.confirmPassword.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("retypePassphrase.TextAlign")));
   this.confirmPassword.Visible = ((bool)(resources.GetObject("retypePassphrase.Visible")));
   this.confirmPassword.WordWrap = ((bool)(resources.GetObject("retypePassphrase.WordWrap")));
   this.confirmPassword.TextChanged += new System.EventHandler(this.retypePassword_TextChanged);
   this.rememberPassword.AccessibleDescription = resources.GetString("rememberPassphrase.AccessibleDescription");
   this.rememberPassword.AccessibleName = resources.GetString("rememberPassphrase.AccessibleName");
   this.rememberPassword.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("rememberPassphrase.Anchor")));
   this.rememberPassword.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("rememberPassphrase.Appearance")));
   this.rememberPassword.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("rememberPassphrase.BackgroundImage")));
   this.rememberPassword.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("rememberPassphrase.CheckAlign")));
   this.rememberPassword.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("rememberPassphrase.Dock")));
   this.rememberPassword.Enabled = ((bool)(resources.GetObject("rememberPassphrase.Enabled")));
   this.rememberPassword.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("rememberPassphrase.FlatStyle")));
   this.rememberPassword.Font = ((System.Drawing.Font)(resources.GetObject("rememberPassphrase.Font")));
   this.rememberPassword.Image = ((System.Drawing.Image)(resources.GetObject("rememberPassphrase.Image")));
   this.rememberPassword.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("rememberPassphrase.ImageAlign")));
   this.rememberPassword.ImageIndex = ((int)(resources.GetObject("rememberPassphrase.ImageIndex")));
   this.rememberPassword.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("rememberPassphrase.ImeMode")));
   this.rememberPassword.Location = ((System.Drawing.Point)(resources.GetObject("rememberPassphrase.Location")));
   this.rememberPassword.Name = "rememberPassphrase";
   this.rememberPassword.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("rememberPassphrase.RightToLeft")));
   this.rememberPassword.Size = ((System.Drawing.Size)(resources.GetObject("rememberPassphrase.Size")));
   this.rememberPassword.TabIndex = ((int)(resources.GetObject("rememberPassphrase.TabIndex")));
            this.rememberPassword.Text = resources.GetString("RememberPasswordLabelText");
   this.rememberPassword.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("rememberPassphrase.TextAlign")));
   this.rememberPassword.Visible = ((bool)(resources.GetObject("rememberPassphrase.Visible")));
   this.btnCancel.AccessibleDescription = resources.GetString("btnCancel.AccessibleDescription");
   this.btnCancel.AccessibleName = resources.GetString("btnCancel.AccessibleName");
   this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("btnCancel.Anchor")));
   this.btnCancel.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("btnCancel.BackgroundImage")));
   this.btnCancel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("btnCancel.Dock")));
   this.btnCancel.Enabled = ((bool)(resources.GetObject("btnCancel.Enabled")));
   this.btnCancel.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("btnCancel.FlatStyle")));
   this.btnCancel.Font = ((System.Drawing.Font)(resources.GetObject("btnCancel.Font")));
   this.btnCancel.Image = ((System.Drawing.Image)(resources.GetObject("btnCancel.Image")));
   this.btnCancel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnCancel.ImageAlign")));
   this.btnCancel.ImageIndex = ((int)(resources.GetObject("btnCancel.ImageIndex")));
   this.btnCancel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("btnCancel.ImeMode")));
   this.btnCancel.Location = ((System.Drawing.Point)(resources.GetObject("btnCancel.Location")));
   this.btnCancel.Name = "btnCancel";
   this.btnCancel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("btnCancel.RightToLeft")));
   this.btnCancel.Size = ((System.Drawing.Size)(resources.GetObject("btnCancel.Size")));
   this.btnCancel.TabIndex = ((int)(resources.GetObject("btnCancel.TabIndex")));
   this.btnCancel.Text = resources.GetString("btnCancel.Text");
   this.btnCancel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnCancel.TextAlign")));
   this.btnCancel.Visible = ((bool)(resources.GetObject("btnCancel.Visible")));
   this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
            this.buttonHelp.AccessibleDescription = resources.GetString("btnHelp.AccessibleDescription");
            this.buttonHelp.AccessibleName = resources.GetString("btnHelp.AccessibleName");
            this.buttonHelp.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("btnHelp.Anchor")));
            this.buttonHelp.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("btnHelp.BackgroundImage")));
            this.buttonHelp.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("btnHelp.Dock")));
            this.buttonHelp.Enabled = ((bool)(resources.GetObject("btnHelp.Enabled")));
            this.buttonHelp.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("btnHelp.FlatStyle")));
            this.buttonHelp.Font = ((System.Drawing.Font)(resources.GetObject("btnHelp.Font")));
            this.buttonHelp.Image = ((System.Drawing.Image)(resources.GetObject("btnHelp.Image")));
            this.buttonHelp.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnHelp.ImageAlign")));
            this.buttonHelp.ImageIndex = ((int)(resources.GetObject("btnHelp.ImageIndex")));
            this.buttonHelp.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("btnHelp.ImeMode")));
            this.buttonHelp.Location = ((System.Drawing.Point)(resources.GetObject("btnHelp.Location")));
            this.buttonHelp.Name = "btnHelp";
            this.buttonHelp.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("btnHelp.RightToLeft")));
            this.buttonHelp.Size = ((System.Drawing.Size)(resources.GetObject("btnHelp.Size")));
            this.buttonHelp.TabIndex = ((int)(resources.GetObject("btnHelp.TabIndex")));
            this.buttonHelp.Text = resources.GetString("btnHelp.Text");
            this.buttonHelp.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnHelp.TextAlign")));
            this.buttonHelp.Visible = ((bool)(resources.GetObject("btnHelp.Visible")));
            this.buttonHelp.Click += new System.EventHandler(this.buttonHelp_Click);
   this.btnReset.AccessibleDescription = resources.GetString("btnReset.AccessibleDescription");
   this.btnReset.AccessibleName = resources.GetString("btnReset.AccessibleName");
   this.btnReset.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("btnReset.Anchor")));
   this.btnReset.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("btnReset.BackgroundImage")));
   this.btnReset.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("btnReset.Dock")));
   this.btnReset.Enabled = ((bool)(resources.GetObject("btnReset.Enabled")));
   this.btnReset.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("btnReset.FlatStyle")));
   this.btnReset.Font = ((System.Drawing.Font)(resources.GetObject("btnReset.Font")));
   this.btnReset.Image = ((System.Drawing.Image)(resources.GetObject("btnReset.Image")));
   this.btnReset.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnReset.ImageAlign")));
   this.btnReset.ImageIndex = ((int)(resources.GetObject("btnReset.ImageIndex")));
   this.btnReset.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("btnReset.ImeMode")));
   this.btnReset.Location = ((System.Drawing.Point)(resources.GetObject("btnReset.Location")));
   this.btnReset.Name = "btnReset";
   this.btnReset.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("btnReset.RightToLeft")));
   this.btnReset.Size = ((System.Drawing.Size)(resources.GetObject("btnReset.Size")));
   this.btnReset.TabIndex = ((int)(resources.GetObject("btnReset.TabIndex")));
   this.btnReset.Text = resources.GetString("btnReset.Text");
   this.btnReset.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("btnReset.TextAlign")));
   this.btnReset.Visible = ((bool)(resources.GetObject("btnReset.Visible")));
   this.btnReset.Click += new System.EventHandler(this.btnReset_Click);
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.btnReset);
   this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.buttonHelp);
   this.Controls.Add(this.rememberPassword);
   this.Controls.Add(this.confirmPassword);
   this.Controls.Add(this.newPassword);
   this.Controls.Add(this.oldpassword);
   this.Controls.Add(this.DomainComboBox);
   this.Controls.Add(this.confirmPasswordLabel);
   this.Controls.Add(this.newPasswordLabel);
   this.Controls.Add(this.passwordLabel);
   this.Controls.Add(this.accountLabel);
   this.Controls.Add(this.panel);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
            this.Size = ((System.Drawing.Size)(resources.GetObject("$this.Size")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "ResetPassword";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.Load += new System.EventHandler(this.ResetPassword_Load);
   this.panel.ResumeLayout(false);
   this.ResumeLayout(false);
  }
  private void newPassword_TextChanged(object sender, System.EventArgs e)
  {
   UpdateSensitivity();
  }
  private void ResetPassword_Load(object sender, System.EventArgs e)
  {
   this.Icon = new Icon(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder_16.ico"));
   this.waterMark.Image = Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner.png"));
   this.pictureBox.SizeMode = PictureBoxSizeMode.StretchImage;
   this.pictureBox.Image = Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner-scaler.png"));
   this.btnReset.Enabled = false;
            this.btnCancel.Select();
   try
   {
    if( this.DomainComboBox.Items.Count > 0)
    {
                    this.DomainComboBox.SelectedIndex = 0;
                    this.oldpassword.Enabled = this.newPassword.Enabled = this.confirmPassword.Enabled = rememberPassword.Enabled = true;
                }
   }
   catch
   {
   }
  }
        private void PerformResetPassword(string domainid, string oldpassword, string newpassword)
        {
            string title = null;
            string message = null;
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(ResetPassword));
            int result = this.ifws.ChangePassword(domainid, oldpassword, newpassword);
            if (result == 0)
            {
                try
                {
                    if (this.rememberPassword.Checked == true)
                    {
                        this.simiasWebservice.SetDomainCredentials(domainid, newpassword, CredentialType.Basic);
                    }
                    else
                    {
                        this.simiasWebservice.SetDomainCredentials(domainid, null, CredentialType.None);
                    }
      this.simiasWebservice.LogoutFromRemoteDomain(domainid);
                }
                catch { }
                title = resources.GetString("ResetPasswordTitle");
                message = resources.GetString("Resetpasswordsuccess");
                MyMessageBox mb = new MyMessageBox(message, title, "", MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
                mb.ShowDialog();
                mb.Dispose();
                this.success = true;
                this.Dispose();
                this.Close();
                return;
            }
            title = resources.GetString("Errorchangingpassword");
            switch (result)
            {
                case 1:
                    message = resources.GetString("IncorrectOldPassword");
                    break;
                case 2:
                    message = resources.GetString("Failedtoresetpassword");
                    break;
                case 3:
                    message = resources.GetString("LoginDisabled");
                    break;
                case 4:
                    message = resources.GetString("Useraccountexpired");
                    break;
                case 5:
                    message = resources.GetString("Usercannotchangepassword");
                    break;
                case 6:
                    message = resources.GetString("Userpasswordexpired");
                    break;
                case 7:
                    message = resources.GetString("Minimumpasswordlengthrestrictionnotmet");
                    break;
                case 8:
                    message = resources.GetString("Usernotfoundinsimias");
                    break;
                default:
                    message = resources.GetString("Errorchangingpassword");
                    break;
            }
            message = resources.GetString("Couldnotchangepassword") + message;
            MyMessageBox mb1 = new MyMessageBox(message, title, "", MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
            mb1.ShowDialog();
            mb1.Dispose();
            this.success = false;
        }
  private void btnReset_Click(object sender, System.EventArgs e)
  {
            string title;
            string message;
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(ResetPassword));
            if( this.newPassword.Text != this.confirmPassword.Text )
            {
                title = resources.GetString("Errorchangingpassword");
                message = resources.GetString("PasswordsDoesNotMatch");
                MyMessageBox mb1 = new MyMessageBox(message, title, "", MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                mb1.ShowDialog();
                mb1.Dispose();
                this.success = false;
                return;
            }
            else if (this.newPassword.Text == this.oldpassword.Text)
            {
                title = resources.GetString("Errorchangingpassword");
                message = resources.GetString("PasswordsSame");
                MyMessageBox mb1 = new MyMessageBox(message, title, "", MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                mb1.ShowDialog();
                mb1.Dispose();
                this.success = false;
                return;
            }
   try
   {
    DomainItem domainItem = (DomainItem)this.DomainComboBox.SelectedItem;
    this.domainID = domainItem.ID;
    System.Resources.ResourceManager resManager = new System.Resources.ResourceManager(typeof(Connecting));
                this.PerformResetPassword(this.domainID, this.oldpassword.Text, this.newPassword.Text);
                (Novell.FormsTrayApp.FormsTrayApp.globalProp()).refreshAll();
   }
   catch(Exception)
   {
    MessageBox.Show(Resource.GetString("ResetError") , Resource.GetString("ResetTitle") );
    this.success = false;
   }
  }
  private void password_TextChanged(object sender, System.EventArgs e)
  {
   UpdateSensitivity();
  }
  private void retypePassword_TextChanged(object sender, System.EventArgs e)
  {
   UpdateSensitivity();
  }
  private void btnCancel_Click(object sender, System.EventArgs e)
  {
   this.success = false;
   this.Dispose();
   this.Close();
  }
        private void buttonHelp_Click(object sender, EventArgs e)
        {
            string helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"bkmgmdj.html");
            new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
        }
  private void UpdateSensitivity()
  {
   if( this.oldpassword.Text.Length > 0 &&
    this.newPassword.Text.Length > 0 &&
                this.confirmPassword.Text.Length > 0)
        this.btnReset.Enabled = true;
   else
    this.btnReset.Enabled = false;
  }
  private void GetLoggedInDomains()
  {
   try
   {
    DomainInformation[] domains;
    domains = this.simiasWebservice.GetDomains(true);
    foreach (DomainInformation di in domains)
    {
     if( di.Authenticated)
     {
      DomainItem domainItem = new DomainItem(di.Name, di.ID, di.Host);
      this.DomainComboBox.Items.Add(domainItem);
     }
    }
   }
   catch(Exception)
   {
   }
  }
 }
}
