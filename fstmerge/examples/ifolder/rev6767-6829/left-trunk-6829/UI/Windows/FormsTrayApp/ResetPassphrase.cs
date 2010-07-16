

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



 public class ResetPassphrase : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Panel panel;
  private System.Windows.Forms.PictureBox waterMark;
  private System.Windows.Forms.Label accountLabel;
  private System.Windows.Forms.Label passphraseLabel;
  private System.Windows.Forms.Label newPassphraseLabel;
  private System.Windows.Forms.Label retypePassphraseLabel;
  private System.Windows.Forms.Label recoveryAgentLabel;
  private System.Windows.Forms.ComboBox DomainComboBox;
  private System.Windows.Forms.TextBox passPhrase;
  private System.Windows.Forms.TextBox newPassphrase;
  private System.Windows.Forms.TextBox retypePassphrase;
  private System.Windows.Forms.ComboBox recoveryAgentCombo;
  private System.Windows.Forms.CheckBox rememberPassphrase;
  private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button buttonHelp;
        private System.Windows.Forms.Button btnReset;
  private SimiasWebService simws;
        private iFolderWebService ifws;
  private string domainID;
  private DomainItem selectedDomain;
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




  public bool Success
  {
   get
   {
    return this.success;
   }
  }




  public int DomainCount
  {
   get
   {
    return this.DomainComboBox.Items.Count;
   }
  }






  public ResetPassphrase(SimiasWebService simws, iFolderWebService ifws)
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(ResetPassphrase));
   this.panel = new System.Windows.Forms.Panel();
   this.pictureBox = new System.Windows.Forms.PictureBox();
   this.waterMark = new System.Windows.Forms.PictureBox();
   this.accountLabel = new System.Windows.Forms.Label();
   this.passphraseLabel = new System.Windows.Forms.Label();
   this.newPassphraseLabel = new System.Windows.Forms.Label();
   this.retypePassphraseLabel = new System.Windows.Forms.Label();
   this.recoveryAgentLabel = new System.Windows.Forms.Label();
   this.DomainComboBox = new System.Windows.Forms.ComboBox();
   this.passPhrase = new System.Windows.Forms.TextBox();
   this.newPassphrase = new System.Windows.Forms.TextBox();
   this.retypePassphrase = new System.Windows.Forms.TextBox();
   this.recoveryAgentCombo = new System.Windows.Forms.ComboBox();
   this.rememberPassphrase = new System.Windows.Forms.CheckBox();
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



   this.passphraseLabel.AccessibleDescription = resources.GetString("passphraseLabel.AccessibleDescription");
   this.passphraseLabel.AccessibleName = resources.GetString("passphraseLabel.AccessibleName");
   this.passphraseLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("passphraseLabel.Anchor")));
   this.passphraseLabel.AutoSize = ((bool)(resources.GetObject("passphraseLabel.AutoSize")));
   this.passphraseLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("passphraseLabel.Dock")));
   this.passphraseLabel.Enabled = ((bool)(resources.GetObject("passphraseLabel.Enabled")));
   this.passphraseLabel.Font = ((System.Drawing.Font)(resources.GetObject("passphraseLabel.Font")));
   this.passphraseLabel.Image = ((System.Drawing.Image)(resources.GetObject("passphraseLabel.Image")));
   this.passphraseLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("passphraseLabel.ImageAlign")));
   this.passphraseLabel.ImageIndex = ((int)(resources.GetObject("passphraseLabel.ImageIndex")));
   this.passphraseLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("passphraseLabel.ImeMode")));
   this.passphraseLabel.Location = ((System.Drawing.Point)(resources.GetObject("passphraseLabel.Location")));
   this.passphraseLabel.Name = "passphraseLabel";
   this.passphraseLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("passphraseLabel.RightToLeft")));
   this.passphraseLabel.Size = ((System.Drawing.Size)(resources.GetObject("passphraseLabel.Size")));
   this.passphraseLabel.TabIndex = ((int)(resources.GetObject("passphraseLabel.TabIndex")));
   this.passphraseLabel.Text = resources.GetString("passphraseLabel.Text");
   this.passphraseLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("passphraseLabel.TextAlign")));
   this.passphraseLabel.Visible = ((bool)(resources.GetObject("passphraseLabel.Visible")));



   this.newPassphraseLabel.AccessibleDescription = resources.GetString("newPassphraseLabel.AccessibleDescription");
   this.newPassphraseLabel.AccessibleName = resources.GetString("newPassphraseLabel.AccessibleName");
   this.newPassphraseLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("newPassphraseLabel.Anchor")));
   this.newPassphraseLabel.AutoSize = ((bool)(resources.GetObject("newPassphraseLabel.AutoSize")));
   this.newPassphraseLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("newPassphraseLabel.Dock")));
   this.newPassphraseLabel.Enabled = ((bool)(resources.GetObject("newPassphraseLabel.Enabled")));
   this.newPassphraseLabel.Font = ((System.Drawing.Font)(resources.GetObject("newPassphraseLabel.Font")));
   this.newPassphraseLabel.Image = ((System.Drawing.Image)(resources.GetObject("newPassphraseLabel.Image")));
   this.newPassphraseLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("newPassphraseLabel.ImageAlign")));
   this.newPassphraseLabel.ImageIndex = ((int)(resources.GetObject("newPassphraseLabel.ImageIndex")));
   this.newPassphraseLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("newPassphraseLabel.ImeMode")));
   this.newPassphraseLabel.Location = ((System.Drawing.Point)(resources.GetObject("newPassphraseLabel.Location")));
   this.newPassphraseLabel.Name = "newPassphraseLabel";
   this.newPassphraseLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("newPassphraseLabel.RightToLeft")));
   this.newPassphraseLabel.Size = ((System.Drawing.Size)(resources.GetObject("newPassphraseLabel.Size")));
   this.newPassphraseLabel.TabIndex = ((int)(resources.GetObject("newPassphraseLabel.TabIndex")));
   this.newPassphraseLabel.Text = resources.GetString("newPassphraseLabel.Text");
   this.newPassphraseLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("newPassphraseLabel.TextAlign")));
   this.newPassphraseLabel.Visible = ((bool)(resources.GetObject("newPassphraseLabel.Visible")));
   this.newPassphraseLabel.Click += new System.EventHandler(this.newPassphraseLabel_Click);



   this.retypePassphraseLabel.AccessibleDescription = resources.GetString("retypePassphraseLabel.AccessibleDescription");
   this.retypePassphraseLabel.AccessibleName = resources.GetString("retypePassphraseLabel.AccessibleName");
   this.retypePassphraseLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("retypePassphraseLabel.Anchor")));
   this.retypePassphraseLabel.AutoSize = ((bool)(resources.GetObject("retypePassphraseLabel.AutoSize")));
   this.retypePassphraseLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("retypePassphraseLabel.Dock")));
   this.retypePassphraseLabel.Enabled = ((bool)(resources.GetObject("retypePassphraseLabel.Enabled")));
   this.retypePassphraseLabel.Font = ((System.Drawing.Font)(resources.GetObject("retypePassphraseLabel.Font")));
   this.retypePassphraseLabel.Image = ((System.Drawing.Image)(resources.GetObject("retypePassphraseLabel.Image")));
   this.retypePassphraseLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("retypePassphraseLabel.ImageAlign")));
   this.retypePassphraseLabel.ImageIndex = ((int)(resources.GetObject("retypePassphraseLabel.ImageIndex")));
   this.retypePassphraseLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("retypePassphraseLabel.ImeMode")));
   this.retypePassphraseLabel.Location = ((System.Drawing.Point)(resources.GetObject("retypePassphraseLabel.Location")));
   this.retypePassphraseLabel.Name = "retypePassphraseLabel";
   this.retypePassphraseLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("retypePassphraseLabel.RightToLeft")));
   this.retypePassphraseLabel.Size = ((System.Drawing.Size)(resources.GetObject("retypePassphraseLabel.Size")));
   this.retypePassphraseLabel.TabIndex = ((int)(resources.GetObject("retypePassphraseLabel.TabIndex")));
   this.retypePassphraseLabel.Text = resources.GetString("retypePassphraseLabel.Text");
   this.retypePassphraseLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("retypePassphraseLabel.TextAlign")));
   this.retypePassphraseLabel.Visible = ((bool)(resources.GetObject("retypePassphraseLabel.Visible")));



   this.recoveryAgentLabel.AccessibleDescription = resources.GetString("recoveryAgentLabel.AccessibleDescription");
   this.recoveryAgentLabel.AccessibleName = resources.GetString("recoveryAgentLabel.AccessibleName");
   this.recoveryAgentLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("recoveryAgentLabel.Anchor")));
   this.recoveryAgentLabel.AutoSize = ((bool)(resources.GetObject("recoveryAgentLabel.AutoSize")));
   this.recoveryAgentLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("recoveryAgentLabel.Dock")));
   this.recoveryAgentLabel.Enabled = ((bool)(resources.GetObject("recoveryAgentLabel.Enabled")));
   this.recoveryAgentLabel.Font = ((System.Drawing.Font)(resources.GetObject("recoveryAgentLabel.Font")));
   this.recoveryAgentLabel.Image = ((System.Drawing.Image)(resources.GetObject("recoveryAgentLabel.Image")));
   this.recoveryAgentLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("recoveryAgentLabel.ImageAlign")));
   this.recoveryAgentLabel.ImageIndex = ((int)(resources.GetObject("recoveryAgentLabel.ImageIndex")));
   this.recoveryAgentLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("recoveryAgentLabel.ImeMode")));
   this.recoveryAgentLabel.Location = ((System.Drawing.Point)(resources.GetObject("recoveryAgentLabel.Location")));
   this.recoveryAgentLabel.Name = "recoveryAgentLabel";
   this.recoveryAgentLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("recoveryAgentLabel.RightToLeft")));
   this.recoveryAgentLabel.Size = ((System.Drawing.Size)(resources.GetObject("recoveryAgentLabel.Size")));
   this.recoveryAgentLabel.TabIndex = ((int)(resources.GetObject("recoveryAgentLabel.TabIndex")));
   this.recoveryAgentLabel.Text = resources.GetString("recoveryAgentLabel.Text");
   this.recoveryAgentLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("recoveryAgentLabel.TextAlign")));
   this.recoveryAgentLabel.Visible = ((bool)(resources.GetObject("recoveryAgentLabel.Visible")));



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
            this.DomainComboBox.SelectedIndexChanged += new System.EventHandler(this.DomainComboBox_SelectedIndexChanged);



   this.passPhrase.AccessibleDescription = resources.GetString("passPhrase.AccessibleDescription");
   this.passPhrase.AccessibleName = resources.GetString("passPhrase.AccessibleName");
   this.passPhrase.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("passPhrase.Anchor")));
   this.passPhrase.AutoSize = ((bool)(resources.GetObject("passPhrase.AutoSize")));
   this.passPhrase.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("passPhrase.BackgroundImage")));
   this.passPhrase.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("passPhrase.Dock")));
   this.passPhrase.Enabled = ((bool)(resources.GetObject("passPhrase.Enabled")));
   this.passPhrase.Font = ((System.Drawing.Font)(resources.GetObject("passPhrase.Font")));
   this.passPhrase.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("passPhrase.ImeMode")));
   this.passPhrase.Location = ((System.Drawing.Point)(resources.GetObject("passPhrase.Location")));
   this.passPhrase.MaxLength = ((int)(resources.GetObject("passPhrase.MaxLength")));
   this.passPhrase.Multiline = ((bool)(resources.GetObject("passPhrase.Multiline")));
   this.passPhrase.Name = "passPhrase";
   this.passPhrase.PasswordChar = ((char)(resources.GetObject("passPhrase.PasswordChar")));
   this.passPhrase.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("passPhrase.RightToLeft")));
   this.passPhrase.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("passPhrase.ScrollBars")));
   this.passPhrase.Size = ((System.Drawing.Size)(resources.GetObject("passPhrase.Size")));
   this.passPhrase.TabIndex = ((int)(resources.GetObject("passPhrase.TabIndex")));
   this.passPhrase.Text = resources.GetString("passPhrase.Text");
   this.passPhrase.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("passPhrase.TextAlign")));
   this.passPhrase.Visible = ((bool)(resources.GetObject("passPhrase.Visible")));
   this.passPhrase.WordWrap = ((bool)(resources.GetObject("passPhrase.WordWrap")));
   this.passPhrase.TextChanged += new System.EventHandler(this.passPhrase_TextChanged);



   this.newPassphrase.AccessibleDescription = resources.GetString("newPassphrase.AccessibleDescription");
   this.newPassphrase.AccessibleName = resources.GetString("newPassphrase.AccessibleName");
   this.newPassphrase.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("newPassphrase.Anchor")));
   this.newPassphrase.AutoSize = ((bool)(resources.GetObject("newPassphrase.AutoSize")));
   this.newPassphrase.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("newPassphrase.BackgroundImage")));
   this.newPassphrase.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("newPassphrase.Dock")));
   this.newPassphrase.Enabled = ((bool)(resources.GetObject("newPassphrase.Enabled")));
   this.newPassphrase.Font = ((System.Drawing.Font)(resources.GetObject("newPassphrase.Font")));
   this.newPassphrase.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("newPassphrase.ImeMode")));
   this.newPassphrase.Location = ((System.Drawing.Point)(resources.GetObject("newPassphrase.Location")));
   this.newPassphrase.MaxLength = ((int)(resources.GetObject("newPassphrase.MaxLength")));
   this.newPassphrase.Multiline = ((bool)(resources.GetObject("newPassphrase.Multiline")));
   this.newPassphrase.Name = "newPassphrase";
   this.newPassphrase.PasswordChar = ((char)(resources.GetObject("newPassphrase.PasswordChar")));
   this.newPassphrase.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("newPassphrase.RightToLeft")));
   this.newPassphrase.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("newPassphrase.ScrollBars")));
   this.newPassphrase.Size = ((System.Drawing.Size)(resources.GetObject("newPassphrase.Size")));
   this.newPassphrase.TabIndex = ((int)(resources.GetObject("newPassphrase.TabIndex")));
   this.newPassphrase.Text = resources.GetString("newPassphrase.Text");
   this.newPassphrase.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("newPassphrase.TextAlign")));
   this.newPassphrase.Visible = ((bool)(resources.GetObject("newPassphrase.Visible")));
   this.newPassphrase.WordWrap = ((bool)(resources.GetObject("newPassphrase.WordWrap")));
   this.newPassphrase.TextChanged += new System.EventHandler(this.newPassphrase_TextChanged);



   this.retypePassphrase.AccessibleDescription = resources.GetString("retypePassphrase.AccessibleDescription");
   this.retypePassphrase.AccessibleName = resources.GetString("retypePassphrase.AccessibleName");
   this.retypePassphrase.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("retypePassphrase.Anchor")));
   this.retypePassphrase.AutoSize = ((bool)(resources.GetObject("retypePassphrase.AutoSize")));
   this.retypePassphrase.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("retypePassphrase.BackgroundImage")));
   this.retypePassphrase.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("retypePassphrase.Dock")));
   this.retypePassphrase.Enabled = ((bool)(resources.GetObject("retypePassphrase.Enabled")));
   this.retypePassphrase.Font = ((System.Drawing.Font)(resources.GetObject("retypePassphrase.Font")));
   this.retypePassphrase.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("retypePassphrase.ImeMode")));
   this.retypePassphrase.Location = ((System.Drawing.Point)(resources.GetObject("retypePassphrase.Location")));
   this.retypePassphrase.MaxLength = ((int)(resources.GetObject("retypePassphrase.MaxLength")));
   this.retypePassphrase.Multiline = ((bool)(resources.GetObject("retypePassphrase.Multiline")));
   this.retypePassphrase.Name = "retypePassphrase";
   this.retypePassphrase.PasswordChar = ((char)(resources.GetObject("retypePassphrase.PasswordChar")));
   this.retypePassphrase.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("retypePassphrase.RightToLeft")));
   this.retypePassphrase.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("retypePassphrase.ScrollBars")));
   this.retypePassphrase.Size = ((System.Drawing.Size)(resources.GetObject("retypePassphrase.Size")));
   this.retypePassphrase.TabIndex = ((int)(resources.GetObject("retypePassphrase.TabIndex")));
   this.retypePassphrase.Text = resources.GetString("retypePassphrase.Text");
   this.retypePassphrase.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("retypePassphrase.TextAlign")));
   this.retypePassphrase.Visible = ((bool)(resources.GetObject("retypePassphrase.Visible")));
   this.retypePassphrase.WordWrap = ((bool)(resources.GetObject("retypePassphrase.WordWrap")));
   this.retypePassphrase.TextChanged += new System.EventHandler(this.retypePassphrase_TextChanged);



   this.recoveryAgentCombo.AccessibleDescription = resources.GetString("recoveryAgentCombo.AccessibleDescription");
   this.recoveryAgentCombo.AccessibleName = resources.GetString("recoveryAgentCombo.AccessibleName");
   this.recoveryAgentCombo.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("recoveryAgentCombo.Anchor")));
   this.recoveryAgentCombo.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("recoveryAgentCombo.BackgroundImage")));
   this.recoveryAgentCombo.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("recoveryAgentCombo.Dock")));
   this.recoveryAgentCombo.Enabled = ((bool)(resources.GetObject("recoveryAgentCombo.Enabled")));
   this.recoveryAgentCombo.Font = ((System.Drawing.Font)(resources.GetObject("recoveryAgentCombo.Font")));
   this.recoveryAgentCombo.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("recoveryAgentCombo.ImeMode")));
   this.recoveryAgentCombo.IntegralHeight = ((bool)(resources.GetObject("recoveryAgentCombo.IntegralHeight")));
   this.recoveryAgentCombo.ItemHeight = ((int)(resources.GetObject("recoveryAgentCombo.ItemHeight")));
   this.recoveryAgentCombo.Location = ((System.Drawing.Point)(resources.GetObject("recoveryAgentCombo.Location")));
   this.recoveryAgentCombo.MaxDropDownItems = ((int)(resources.GetObject("recoveryAgentCombo.MaxDropDownItems")));
   this.recoveryAgentCombo.MaxLength = ((int)(resources.GetObject("recoveryAgentCombo.MaxLength")));
   this.recoveryAgentCombo.Name = "recoveryAgentCombo";
   this.recoveryAgentCombo.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("recoveryAgentCombo.RightToLeft")));
   this.recoveryAgentCombo.Size = ((System.Drawing.Size)(resources.GetObject("recoveryAgentCombo.Size")));
   this.recoveryAgentCombo.TabIndex = ((int)(resources.GetObject("recoveryAgentCombo.TabIndex")));
   this.recoveryAgentCombo.Text = resources.GetString("recoveryAgentCombo.Text");
   this.recoveryAgentCombo.Visible = ((bool)(resources.GetObject("recoveryAgentCombo.Visible")));



   this.rememberPassphrase.AccessibleDescription = resources.GetString("rememberPassphrase.AccessibleDescription");
   this.rememberPassphrase.AccessibleName = resources.GetString("rememberPassphrase.AccessibleName");
   this.rememberPassphrase.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("rememberPassphrase.Anchor")));
   this.rememberPassphrase.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("rememberPassphrase.Appearance")));
   this.rememberPassphrase.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("rememberPassphrase.BackgroundImage")));
   this.rememberPassphrase.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("rememberPassphrase.CheckAlign")));
   this.rememberPassphrase.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("rememberPassphrase.Dock")));
   this.rememberPassphrase.Enabled = ((bool)(resources.GetObject("rememberPassphrase.Enabled")));
   this.rememberPassphrase.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("rememberPassphrase.FlatStyle")));
   this.rememberPassphrase.Font = ((System.Drawing.Font)(resources.GetObject("rememberPassphrase.Font")));
   this.rememberPassphrase.Image = ((System.Drawing.Image)(resources.GetObject("rememberPassphrase.Image")));
   this.rememberPassphrase.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("rememberPassphrase.ImageAlign")));
   this.rememberPassphrase.ImageIndex = ((int)(resources.GetObject("rememberPassphrase.ImageIndex")));
   this.rememberPassphrase.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("rememberPassphrase.ImeMode")));
   this.rememberPassphrase.Location = ((System.Drawing.Point)(resources.GetObject("rememberPassphrase.Location")));
   this.rememberPassphrase.Name = "rememberPassphrase";
   this.rememberPassphrase.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("rememberPassphrase.RightToLeft")));
   this.rememberPassphrase.Size = ((System.Drawing.Size)(resources.GetObject("rememberPassphrase.Size")));
   this.rememberPassphrase.TabIndex = ((int)(resources.GetObject("rememberPassphrase.TabIndex")));
   this.rememberPassphrase.Text = resources.GetString("rememberPassphrase.Text");
   this.rememberPassphrase.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("rememberPassphrase.TextAlign")));
   this.rememberPassphrase.Visible = ((bool)(resources.GetObject("rememberPassphrase.Visible")));



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
   this.Controls.Add(this.rememberPassphrase);
   this.Controls.Add(this.recoveryAgentCombo);
   this.Controls.Add(this.retypePassphrase);
   this.Controls.Add(this.newPassphrase);
   this.Controls.Add(this.passPhrase);
   this.Controls.Add(this.DomainComboBox);
   this.Controls.Add(this.recoveryAgentLabel);
   this.Controls.Add(this.retypePassphraseLabel);
   this.Controls.Add(this.newPassphraseLabel);
   this.Controls.Add(this.passphraseLabel);
   this.Controls.Add(this.accountLabel);
   this.Controls.Add(this.panel);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "ResetPassphrase";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.Load += new System.EventHandler(this.ResetPassphrase_Load);
   this.panel.ResumeLayout(false);
   this.ResumeLayout(false);

  }





        private void newPassphrase_TextChanged(object sender, System.EventArgs e)
  {
   UpdateSensitivity();
  }




        private void newPassphraseLabel_Click(object sender, System.EventArgs e)
  {

  }




        private void ResetPassphrase_Load(object sender, System.EventArgs e)
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
     if (selectedDomain != null)
     {
      this.DomainComboBox.SelectedItem = selectedDomain;
     }
     else
      this.DomainComboBox.SelectedIndex = 0;
     PopulateRecoveryAgentList();
                    UpdateUI();
    }
   }
   catch
   {
   }
  }




        private void UpdateUI()
        {
            if ( (ifws.GetSecurityPolicy(DomainID) != 0 && simiasWebservice.IsPassPhraseSet(DomainID) ))
            {
                this.passPhrase.Enabled = this.newPassphrase.Enabled = this.retypePassphrase.Enabled = recoveryAgentCombo.Enabled = rememberPassphrase.Enabled = true;
            }
            else
            {
                this.passPhrase.Enabled = this.newPassphrase.Enabled = this.retypePassphrase.Enabled = recoveryAgentCombo.Enabled = rememberPassphrase.Enabled = false;
            }
        }




        private void btnReset_Click(object sender, System.EventArgs e)
  {
   try
   {
    DomainItem domainItem = (DomainItem)this.DomainComboBox.SelectedItem;
    this.domainID = domainItem.ID;
    System.Resources.ResourceManager resManager = new System.Resources.ResourceManager(typeof(Connecting));
    string publicKey = null;
    string ragent = null;


                Status status = null;
                try
                {
                    status = simws.ValidatePassPhrase(this.domainID, this.passPhrase.Text);
                }
                catch (Exception ex)
                {
                    System.Resources.ResourceManager resMgr = new System.Resources.ResourceManager(typeof(VerifyPassphraseDialog));
                    MessageBox.Show(resMgr.GetString("ValidatePPError"), ex.Message);
                }
                if (status.statusCode == StatusCodes.PassPhraseInvalid)
                {
                    MessageBox.Show(Resource.GetString("InvalidCurrentPPText") , Resource.GetString("ResetTitle") );
                    this.success = false;
                    return;
                }

    if( this.recoveryAgentCombo.SelectedItem != null && (string)this.recoveryAgentCombo.SelectedItem != Resource.GetString("NoneText"))
    {

     byte[] CertificateObj = this.simws.GetRACertificateOnClient(this.DomainID, (string)this.recoveryAgentCombo.SelectedItem);
     System.Security.Cryptography.X509Certificates.X509Certificate cert = new System.Security.Cryptography.X509Certificates.X509Certificate(CertificateObj);
     MyMessageBox mmb = new MyMessageBox( string.Format(resManager.GetString("verifyCert"), (string)this.recoveryAgentCombo.SelectedItem), resManager.GetString("verifyCertTitle"), cert.ToString(true), MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question, MyMessageBoxDefaultButton.Button2);
     DialogResult messageDialogResult = mmb.ShowDialog();
     mmb.Dispose();
     mmb.Close();
     if( messageDialogResult != DialogResult.Yes )
      return;
     else
     {
      ragent = (string)this.recoveryAgentCombo.SelectedItem;
      publicKey = Convert.ToBase64String(cert.GetPublicKey());
     }
    }
    else
    {
     MyMessageBox mmb = new MyMessageBox( resManager.GetString("NoCertWarning"), resManager.GetString("NoCertTitle"), "", MyMessageBoxButtons.YesNo, MyMessageBoxIcon.Question, MyMessageBoxDefaultButton.Button2);
     DialogResult messageDialogResult = mmb.ShowDialog();
     mmb.Dispose();
     mmb.Close();
     if( messageDialogResult != DialogResult.Yes )
      return;
    }

    status = this.simws.ReSetPassPhrase(this.domainID, this.passPhrase.Text , this.newPassphrase.Text, ragent, publicKey);
    if( status.statusCode == StatusCodes.Success)
    {

     simws.StorePassPhrase(this.domainID, "", CredentialType.None, false);

     simws.StorePassPhrase(this.domainID, this.newPassphrase.Text, CredentialType.Basic, this.rememberPassphrase.Checked);

     MyMessageBox mb = new MyMessageBox(string.Format(Resource.GetString("ResetSuccess")), Resource.GetString("ResetTitle"), "", MyMessageBoxButtons.OK, MyMessageBoxIcon.Information);
     mb.ShowDialog();
     mb.Dispose();
     this.success = true;
     this.Dispose();
     this.Close();
    }
    else
    {
     MessageBox.Show(Resource.GetString("ResetError") , Resource.GetString("ResetTitle") );
     this.success = false;
    }
   }
   catch(Exception ex)
   {
    MessageBox.Show(Resource.GetString("ResetError") , Resource.GetString("ResetTitle") );
    this.success = false;
   }
  }




        private void passPhrase_TextChanged(object sender, System.EventArgs e)
  {
   UpdateSensitivity();
  }




        private void retypePassphrase_TextChanged(object sender, System.EventArgs e)
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
            string helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"managingpassphrse.html");
            new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
        }




  private void UpdateSensitivity()
  {
   if( this.passPhrase.Text.Length > 0 &&
    this.newPassphrase.Text.Length > 0 &&
    this.newPassphrase.Text == this.retypePassphrase.Text)
    this.btnReset.Enabled = true;
   else
    this.btnReset.Enabled = false;

  }




        private void pictureBox1_Click(object sender, System.EventArgs e)
  {

  }

  private void DomainComboBox_SelectedIndexChanged(object sender, System.EventArgs e)
  {
   PopulateRecoveryAgentList();
            UpdateUI();
  }




  private void PopulateRecoveryAgentList()
  {
   this.recoveryAgentCombo.Items.Clear();
   try
   {
    string[] rAgents= this.simws.GetRAListOnClient(this.DomainID);
    foreach( string rAgent in rAgents)
    {
     this.recoveryAgentCombo.Items.Add( rAgent );
    }
   }
   catch(Exception ex)
   {
   }
   this.recoveryAgentCombo.Items.Add(Resource.GetString("NoneText"));
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
      DomainItem domainItem = new DomainItem(di.Name, di.ID);
      this.DomainComboBox.Items.Add(domainItem);
     }
    }
   }
   catch(Exception ex)
   {
   }
  }
 }
}
