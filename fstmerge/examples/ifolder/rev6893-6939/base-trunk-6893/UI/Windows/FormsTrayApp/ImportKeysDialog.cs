

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Xml;
using Novell.iFolderCom;
using Novell.iFolder.Web;
using System.IO;

namespace Novell.FormsTrayApp
{



 public class ImportKeysDialog : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Panel panel;
  private System.Windows.Forms.PictureBox waterMark;
  private System.Windows.Forms.Label filePathLabel;
  private System.Windows.Forms.Label oneTimePassphraseLabel;
  private System.Windows.Forms.Label passPhraseLabel;
  private System.Windows.Forms.Label retypePassphraseLabel;
  private System.Windows.Forms.TextBox oneTimePassphrase;
  private System.Windows.Forms.TextBox Passphrase;
  private System.Windows.Forms.TextBox retypePassphrase;
  private System.Windows.Forms.TextBox LocationEntry;
  private System.Windows.Forms.Button BrowseButton;
  private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button btnHelp;
  private System.Windows.Forms.Button btnImport;
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.ComboBox DomainComboBox;
  private DomainItem selectedDomain;
  private System.Windows.Forms.PictureBox pictureBox;
  private SimiasWebService simiasWebService;
        private iFolderWebService ifws = null;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(ImportKeysDialog));



  private System.ComponentModel.Container components = null;




  public int DomainCount
  {
   get
   {
    return this.DomainComboBox.Items.Count;
   }
  }






  public ImportKeysDialog(SimiasWebService simws, iFolderWebService ifws)
  {



   InitializeComponent();
   this.simiasWebService = simws;
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
   this.panel = new System.Windows.Forms.Panel();
   this.pictureBox = new System.Windows.Forms.PictureBox();
   this.waterMark = new System.Windows.Forms.PictureBox();
   this.filePathLabel = new System.Windows.Forms.Label();
   this.oneTimePassphraseLabel = new System.Windows.Forms.Label();
   this.passPhraseLabel = new System.Windows.Forms.Label();
   this.retypePassphraseLabel = new System.Windows.Forms.Label();
   this.oneTimePassphrase = new System.Windows.Forms.TextBox();
   this.Passphrase = new System.Windows.Forms.TextBox();
   this.retypePassphrase = new System.Windows.Forms.TextBox();
   this.LocationEntry = new System.Windows.Forms.TextBox();
   this.BrowseButton = new System.Windows.Forms.Button();
   this.btnCancel = new System.Windows.Forms.Button();
            this.btnHelp = new System.Windows.Forms.Button();
   this.btnImport = new System.Windows.Forms.Button();
   this.label1 = new System.Windows.Forms.Label();
   this.DomainComboBox = new System.Windows.Forms.ComboBox();
   this.panel.SuspendLayout();
   this.SuspendLayout();



   this.panel.AccessibleDescription = Resource.GetString("panel1.AccessibleDescription");
   this.panel.AccessibleName = Resource.GetString("panel1.AccessibleName");
   this.panel.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("panel1.Anchor")));
   this.panel.AutoScroll = ((bool)(Resource.GetObject("panel1.AutoScroll")));
   this.panel.AutoScrollMargin = ((System.Drawing.Size)(Resource.GetObject("panel1.AutoScrollMargin")));
   this.panel.AutoScrollMinSize = ((System.Drawing.Size)(Resource.GetObject("panel1.AutoScrollMinSize")));
   this.panel.BackColor = System.Drawing.Color.Transparent;
   this.panel.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("panel1.BackgroundImage")));
   this.panel.Controls.Add(this.pictureBox);
   this.panel.Controls.Add(this.waterMark);
   this.panel.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("panel1.Dock")));
   this.panel.Enabled = ((bool)(Resource.GetObject("panel1.Enabled")));
   this.panel.Font = ((System.Drawing.Font)(Resource.GetObject("panel1.Font")));
   this.panel.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("panel1.ImeMode")));
   this.panel.Location = ((System.Drawing.Point)(Resource.GetObject("panel1.Location")));
   this.panel.Name = "panel1";
   this.panel.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("panel1.RightToLeft")));
   this.panel.Size = ((System.Drawing.Size)(Resource.GetObject("panel1.Size")));
   this.panel.TabIndex = ((int)(Resource.GetObject("panel1.TabIndex")));
   this.panel.Text = Resource.GetString("panel1.Text");
   this.panel.Visible = ((bool)(Resource.GetObject("panel1.Visible")));



   this.pictureBox.AccessibleDescription = Resource.GetString("pictureBox1.AccessibleDescription");
   this.pictureBox.AccessibleName = Resource.GetString("pictureBox1.AccessibleName");
   this.pictureBox.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("pictureBox1.Anchor")));
   this.pictureBox.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("pictureBox1.BackgroundImage")));
   this.pictureBox.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("pictureBox1.Dock")));
   this.pictureBox.Enabled = ((bool)(Resource.GetObject("pictureBox1.Enabled")));
   this.pictureBox.Font = ((System.Drawing.Font)(Resource.GetObject("pictureBox1.Font")));
   this.pictureBox.Image = ((System.Drawing.Image)(Resource.GetObject("pictureBox1.Image")));
   this.pictureBox.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("pictureBox1.ImeMode")));
   this.pictureBox.Location = ((System.Drawing.Point)(Resource.GetObject("pictureBox1.Location")));
   this.pictureBox.Name = "pictureBox1";
   this.pictureBox.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("pictureBox1.RightToLeft")));
   this.pictureBox.Size = ((System.Drawing.Size)(Resource.GetObject("pictureBox1.Size")));
   this.pictureBox.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(Resource.GetObject("pictureBox1.SizeMode")));
   this.pictureBox.TabIndex = ((int)(Resource.GetObject("pictureBox1.TabIndex")));
   this.pictureBox.TabStop = false;
   this.pictureBox.Text = Resource.GetString("pictureBox1.Text");
   this.pictureBox.Visible = ((bool)(Resource.GetObject("pictureBox1.Visible")));



   this.waterMark.AccessibleDescription = Resource.GetString("waterMark.AccessibleDescription");
   this.waterMark.AccessibleName = Resource.GetString("waterMark.AccessibleName");
   this.waterMark.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("waterMark.Anchor")));
   this.waterMark.BackColor = System.Drawing.Color.Transparent;
   this.waterMark.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("waterMark.BackgroundImage")));
   this.waterMark.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("waterMark.Dock")));
   this.waterMark.Enabled = ((bool)(Resource.GetObject("waterMark.Enabled")));
   this.waterMark.Font = ((System.Drawing.Font)(Resource.GetObject("waterMark.Font")));
   this.waterMark.Image = ((System.Drawing.Image)(Resource.GetObject("waterMark.Image")));
   this.waterMark.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("waterMark.ImeMode")));
   this.waterMark.Location = ((System.Drawing.Point)(Resource.GetObject("waterMark.Location")));
   this.waterMark.Name = "waterMark";
   this.waterMark.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("waterMark.RightToLeft")));
   this.waterMark.Size = ((System.Drawing.Size)(Resource.GetObject("waterMark.Size")));
   this.waterMark.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(Resource.GetObject("waterMark.SizeMode")));
   this.waterMark.TabIndex = ((int)(Resource.GetObject("waterMark.TabIndex")));
   this.waterMark.TabStop = false;
   this.waterMark.Text = Resource.GetString("waterMark.Text");
   this.waterMark.Visible = ((bool)(Resource.GetObject("waterMark.Visible")));



   this.filePathLabel.AccessibleDescription = Resource.GetString("filePathLabel.AccessibleDescription");
   this.filePathLabel.AccessibleName = Resource.GetString("filePathLabel.AccessibleName");
   this.filePathLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("filePathLabel.Anchor")));
   this.filePathLabel.AutoSize = ((bool)(Resource.GetObject("filePathLabel.AutoSize")));
   this.filePathLabel.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("filePathLabel.Dock")));
   this.filePathLabel.Enabled = ((bool)(Resource.GetObject("filePathLabel.Enabled")));
   this.filePathLabel.Font = ((System.Drawing.Font)(Resource.GetObject("filePathLabel.Font")));
   this.filePathLabel.Image = ((System.Drawing.Image)(Resource.GetObject("filePathLabel.Image")));
   this.filePathLabel.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("filePathLabel.ImageAlign")));
   this.filePathLabel.ImageIndex = ((int)(Resource.GetObject("filePathLabel.ImageIndex")));
   this.filePathLabel.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("filePathLabel.ImeMode")));
   this.filePathLabel.Location = ((System.Drawing.Point)(Resource.GetObject("filePathLabel.Location")));
   this.filePathLabel.Name = "filePathLabel";
   this.filePathLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("filePathLabel.RightToLeft")));
   this.filePathLabel.Size = ((System.Drawing.Size)(Resource.GetObject("filePathLabel.Size")));
   this.filePathLabel.TabIndex = ((int)(Resource.GetObject("filePathLabel.TabIndex")));
   this.filePathLabel.Text = Resource.GetString("filePathLabel.Text");
   this.filePathLabel.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("filePathLabel.TextAlign")));
   this.filePathLabel.Visible = ((bool)(Resource.GetObject("filePathLabel.Visible")));



   this.oneTimePassphraseLabel.AccessibleDescription = Resource.GetString("oneTimePassphraseLabel.AccessibleDescription");
   this.oneTimePassphraseLabel.AccessibleName = Resource.GetString("oneTimePassphraseLabel.AccessibleName");
   this.oneTimePassphraseLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("oneTimePassphraseLabel.Anchor")));
   this.oneTimePassphraseLabel.AutoSize = ((bool)(Resource.GetObject("oneTimePassphraseLabel.AutoSize")));
   this.oneTimePassphraseLabel.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("oneTimePassphraseLabel.Dock")));
   this.oneTimePassphraseLabel.Enabled = ((bool)(Resource.GetObject("oneTimePassphraseLabel.Enabled")));
   this.oneTimePassphraseLabel.Font = ((System.Drawing.Font)(Resource.GetObject("oneTimePassphraseLabel.Font")));
   this.oneTimePassphraseLabel.Image = ((System.Drawing.Image)(Resource.GetObject("oneTimePassphraseLabel.Image")));
   this.oneTimePassphraseLabel.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("oneTimePassphraseLabel.ImageAlign")));
   this.oneTimePassphraseLabel.ImageIndex = ((int)(Resource.GetObject("oneTimePassphraseLabel.ImageIndex")));
   this.oneTimePassphraseLabel.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("oneTimePassphraseLabel.ImeMode")));
   this.oneTimePassphraseLabel.Location = ((System.Drawing.Point)(Resource.GetObject("oneTimePassphraseLabel.Location")));
   this.oneTimePassphraseLabel.Name = "oneTimePassphraseLabel";
   this.oneTimePassphraseLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("oneTimePassphraseLabel.RightToLeft")));
   this.oneTimePassphraseLabel.Size = ((System.Drawing.Size)(Resource.GetObject("oneTimePassphraseLabel.Size")));
   this.oneTimePassphraseLabel.TabIndex = ((int)(Resource.GetObject("oneTimePassphraseLabel.TabIndex")));
   this.oneTimePassphraseLabel.Text = Resource.GetString("oneTimePassphraseLabel.Text");
   this.oneTimePassphraseLabel.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("oneTimePassphraseLabel.TextAlign")));
   this.oneTimePassphraseLabel.Visible = ((bool)(Resource.GetObject("oneTimePassphraseLabel.Visible")));



   this.passPhraseLabel.AccessibleDescription = Resource.GetString("passPhraseLabel.AccessibleDescription");
   this.passPhraseLabel.AccessibleName = Resource.GetString("passPhraseLabel.AccessibleName");
   this.passPhraseLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("passPhraseLabel.Anchor")));
   this.passPhraseLabel.AutoSize = ((bool)(Resource.GetObject("passPhraseLabel.AutoSize")));
   this.passPhraseLabel.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("passPhraseLabel.Dock")));
   this.passPhraseLabel.Enabled = ((bool)(Resource.GetObject("passPhraseLabel.Enabled")));
   this.passPhraseLabel.Font = ((System.Drawing.Font)(Resource.GetObject("passPhraseLabel.Font")));
   this.passPhraseLabel.Image = ((System.Drawing.Image)(Resource.GetObject("passPhraseLabel.Image")));
   this.passPhraseLabel.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("passPhraseLabel.ImageAlign")));
   this.passPhraseLabel.ImageIndex = ((int)(Resource.GetObject("passPhraseLabel.ImageIndex")));
   this.passPhraseLabel.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("passPhraseLabel.ImeMode")));
   this.passPhraseLabel.Location = ((System.Drawing.Point)(Resource.GetObject("passPhraseLabel.Location")));
   this.passPhraseLabel.Name = "passPhraseLabel";
   this.passPhraseLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("passPhraseLabel.RightToLeft")));
   this.passPhraseLabel.Size = ((System.Drawing.Size)(Resource.GetObject("passPhraseLabel.Size")));
   this.passPhraseLabel.TabIndex = ((int)(Resource.GetObject("passPhraseLabel.TabIndex")));
   this.passPhraseLabel.Text = Resource.GetString("passPhraseLabel.Text");
   this.passPhraseLabel.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("passPhraseLabel.TextAlign")));
   this.passPhraseLabel.Visible = ((bool)(Resource.GetObject("passPhraseLabel.Visible")));



   this.retypePassphraseLabel.AccessibleDescription = Resource.GetString("retypePassphraseLabel.AccessibleDescription");
   this.retypePassphraseLabel.AccessibleName = Resource.GetString("retypePassphraseLabel.AccessibleName");
   this.retypePassphraseLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("retypePassphraseLabel.Anchor")));
   this.retypePassphraseLabel.AutoSize = ((bool)(Resource.GetObject("retypePassphraseLabel.AutoSize")));
   this.retypePassphraseLabel.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("retypePassphraseLabel.Dock")));
   this.retypePassphraseLabel.Enabled = ((bool)(Resource.GetObject("retypePassphraseLabel.Enabled")));
   this.retypePassphraseLabel.Font = ((System.Drawing.Font)(Resource.GetObject("retypePassphraseLabel.Font")));
   this.retypePassphraseLabel.Image = ((System.Drawing.Image)(Resource.GetObject("retypePassphraseLabel.Image")));
   this.retypePassphraseLabel.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("retypePassphraseLabel.ImageAlign")));
   this.retypePassphraseLabel.ImageIndex = ((int)(Resource.GetObject("retypePassphraseLabel.ImageIndex")));
   this.retypePassphraseLabel.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("retypePassphraseLabel.ImeMode")));
   this.retypePassphraseLabel.Location = ((System.Drawing.Point)(Resource.GetObject("retypePassphraseLabel.Location")));
   this.retypePassphraseLabel.Name = "retypePassphraseLabel";
   this.retypePassphraseLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("retypePassphraseLabel.RightToLeft")));
   this.retypePassphraseLabel.Size = ((System.Drawing.Size)(Resource.GetObject("retypePassphraseLabel.Size")));
   this.retypePassphraseLabel.TabIndex = ((int)(Resource.GetObject("retypePassphraseLabel.TabIndex")));
   this.retypePassphraseLabel.Text = Resource.GetString("retypePassphraseLabel.Text");
   this.retypePassphraseLabel.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("retypePassphraseLabel.TextAlign")));
   this.retypePassphraseLabel.Visible = ((bool)(Resource.GetObject("retypePassphraseLabel.Visible")));



   this.oneTimePassphrase.AccessibleDescription = Resource.GetString("oneTimePassphrase.AccessibleDescription");
   this.oneTimePassphrase.AccessibleName = Resource.GetString("oneTimePassphrase.AccessibleName");
   this.oneTimePassphrase.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("oneTimePassphrase.Anchor")));
   this.oneTimePassphrase.AutoSize = ((bool)(Resource.GetObject("oneTimePassphrase.AutoSize")));
   this.oneTimePassphrase.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("oneTimePassphrase.BackgroundImage")));
   this.oneTimePassphrase.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("oneTimePassphrase.Dock")));
   this.oneTimePassphrase.Enabled = ((bool)(Resource.GetObject("oneTimePassphrase.Enabled")));
   this.oneTimePassphrase.Font = ((System.Drawing.Font)(Resource.GetObject("oneTimePassphrase.Font")));
   this.oneTimePassphrase.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("oneTimePassphrase.ImeMode")));
   this.oneTimePassphrase.Location = ((System.Drawing.Point)(Resource.GetObject("oneTimePassphrase.Location")));
   this.oneTimePassphrase.MaxLength = ((int)(Resource.GetObject("oneTimePassphrase.MaxLength")));
   this.oneTimePassphrase.Multiline = ((bool)(Resource.GetObject("oneTimePassphrase.Multiline")));
   this.oneTimePassphrase.Name = "oneTimePassphrase";
   this.oneTimePassphrase.PasswordChar = ((char)(Resource.GetObject("oneTimePassphrase.PasswordChar")));
   this.oneTimePassphrase.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("oneTimePassphrase.RightToLeft")));
   this.oneTimePassphrase.ScrollBars = ((System.Windows.Forms.ScrollBars)(Resource.GetObject("oneTimePassphrase.ScrollBars")));
   this.oneTimePassphrase.Size = ((System.Drawing.Size)(Resource.GetObject("oneTimePassphrase.Size")));
   this.oneTimePassphrase.TabIndex = ((int)(Resource.GetObject("oneTimePassphrase.TabIndex")));
   this.oneTimePassphrase.Text = Resource.GetString("oneTimePassphrase.Text");
   this.oneTimePassphrase.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(Resource.GetObject("oneTimePassphrase.TextAlign")));
   this.oneTimePassphrase.Visible = ((bool)(Resource.GetObject("oneTimePassphrase.Visible")));
   this.oneTimePassphrase.WordWrap = ((bool)(Resource.GetObject("oneTimePassphrase.WordWrap")));
   this.oneTimePassphrase.TextChanged += new System.EventHandler(this.oneTimePassphrase_TextChanged);



   this.Passphrase.AccessibleDescription = Resource.GetString("Passphrase.AccessibleDescription");
   this.Passphrase.AccessibleName = Resource.GetString("Passphrase.AccessibleName");
   this.Passphrase.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("Passphrase.Anchor")));
   this.Passphrase.AutoSize = ((bool)(Resource.GetObject("Passphrase.AutoSize")));
   this.Passphrase.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("Passphrase.BackgroundImage")));
   this.Passphrase.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("Passphrase.Dock")));
   this.Passphrase.Enabled = ((bool)(Resource.GetObject("Passphrase.Enabled")));
   this.Passphrase.Font = ((System.Drawing.Font)(Resource.GetObject("Passphrase.Font")));
   this.Passphrase.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("Passphrase.ImeMode")));
   this.Passphrase.Location = ((System.Drawing.Point)(Resource.GetObject("Passphrase.Location")));
   this.Passphrase.MaxLength = ((int)(Resource.GetObject("Passphrase.MaxLength")));
   this.Passphrase.Multiline = ((bool)(Resource.GetObject("Passphrase.Multiline")));
   this.Passphrase.Name = "Passphrase";
   this.Passphrase.PasswordChar = ((char)(Resource.GetObject("Passphrase.PasswordChar")));
   this.Passphrase.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("Passphrase.RightToLeft")));
   this.Passphrase.ScrollBars = ((System.Windows.Forms.ScrollBars)(Resource.GetObject("Passphrase.ScrollBars")));
   this.Passphrase.Size = ((System.Drawing.Size)(Resource.GetObject("Passphrase.Size")));
   this.Passphrase.TabIndex = ((int)(Resource.GetObject("Passphrase.TabIndex")));
   this.Passphrase.Text = Resource.GetString("Passphrase.Text");
   this.Passphrase.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(Resource.GetObject("Passphrase.TextAlign")));
   this.Passphrase.Visible = ((bool)(Resource.GetObject("Passphrase.Visible")));
   this.Passphrase.WordWrap = ((bool)(Resource.GetObject("Passphrase.WordWrap")));
   this.Passphrase.TextChanged += new System.EventHandler(this.Passphrase_TextChanged);



   this.retypePassphrase.AccessibleDescription = Resource.GetString("retypePassphrase.AccessibleDescription");
   this.retypePassphrase.AccessibleName = Resource.GetString("retypePassphrase.AccessibleName");
   this.retypePassphrase.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("retypePassphrase.Anchor")));
   this.retypePassphrase.AutoSize = ((bool)(Resource.GetObject("retypePassphrase.AutoSize")));
   this.retypePassphrase.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("retypePassphrase.BackgroundImage")));
   this.retypePassphrase.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("retypePassphrase.Dock")));
   this.retypePassphrase.Enabled = ((bool)(Resource.GetObject("retypePassphrase.Enabled")));
   this.retypePassphrase.Font = ((System.Drawing.Font)(Resource.GetObject("retypePassphrase.Font")));
   this.retypePassphrase.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("retypePassphrase.ImeMode")));
   this.retypePassphrase.Location = ((System.Drawing.Point)(Resource.GetObject("retypePassphrase.Location")));
   this.retypePassphrase.MaxLength = ((int)(Resource.GetObject("retypePassphrase.MaxLength")));
   this.retypePassphrase.Multiline = ((bool)(Resource.GetObject("retypePassphrase.Multiline")));
   this.retypePassphrase.Name = "retypePassphrase";
   this.retypePassphrase.PasswordChar = ((char)(Resource.GetObject("retypePassphrase.PasswordChar")));
   this.retypePassphrase.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("retypePassphrase.RightToLeft")));
   this.retypePassphrase.ScrollBars = ((System.Windows.Forms.ScrollBars)(Resource.GetObject("retypePassphrase.ScrollBars")));
   this.retypePassphrase.Size = ((System.Drawing.Size)(Resource.GetObject("retypePassphrase.Size")));
   this.retypePassphrase.TabIndex = ((int)(Resource.GetObject("retypePassphrase.TabIndex")));
   this.retypePassphrase.Text = Resource.GetString("retypePassphrase.Text");
   this.retypePassphrase.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(Resource.GetObject("retypePassphrase.TextAlign")));
   this.retypePassphrase.Visible = ((bool)(Resource.GetObject("retypePassphrase.Visible")));
   this.retypePassphrase.WordWrap = ((bool)(Resource.GetObject("retypePassphrase.WordWrap")));
   this.retypePassphrase.TextChanged += new System.EventHandler(this.retypePassphrase_TextChanged);



   this.LocationEntry.AccessibleDescription = Resource.GetString("LocationEntry.AccessibleDescription");
   this.LocationEntry.AccessibleName = Resource.GetString("LocationEntry.AccessibleName");
   this.LocationEntry.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("LocationEntry.Anchor")));
   this.LocationEntry.AutoSize = ((bool)(Resource.GetObject("LocationEntry.AutoSize")));
   this.LocationEntry.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("LocationEntry.BackgroundImage")));
   this.LocationEntry.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("LocationEntry.Dock")));
   this.LocationEntry.Enabled = ((bool)(Resource.GetObject("LocationEntry.Enabled")));
   this.LocationEntry.Font = ((System.Drawing.Font)(Resource.GetObject("LocationEntry.Font")));
   this.LocationEntry.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("LocationEntry.ImeMode")));
   this.LocationEntry.Location = ((System.Drawing.Point)(Resource.GetObject("LocationEntry.Location")));
   this.LocationEntry.MaxLength = ((int)(Resource.GetObject("LocationEntry.MaxLength")));
   this.LocationEntry.Multiline = ((bool)(Resource.GetObject("LocationEntry.Multiline")));
   this.LocationEntry.Name = "LocationEntry";
   this.LocationEntry.PasswordChar = ((char)(Resource.GetObject("LocationEntry.PasswordChar")));
   this.LocationEntry.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("LocationEntry.RightToLeft")));
   this.LocationEntry.ScrollBars = ((System.Windows.Forms.ScrollBars)(Resource.GetObject("LocationEntry.ScrollBars")));
   this.LocationEntry.Size = ((System.Drawing.Size)(Resource.GetObject("LocationEntry.Size")));
   this.LocationEntry.TabIndex = ((int)(Resource.GetObject("LocationEntry.TabIndex")));
   this.LocationEntry.Text = Resource.GetString("LocationEntry.Text");
   this.LocationEntry.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(Resource.GetObject("LocationEntry.TextAlign")));
   this.LocationEntry.Visible = ((bool)(Resource.GetObject("LocationEntry.Visible")));
   this.LocationEntry.WordWrap = ((bool)(Resource.GetObject("LocationEntry.WordWrap")));
   this.LocationEntry.TextChanged += new System.EventHandler(this.LocationEntry_TextChanged);



   this.BrowseButton.AccessibleDescription = Resource.GetString("BrowseButton.AccessibleDescription");
   this.BrowseButton.AccessibleName = Resource.GetString("BrowseButton.AccessibleName");
   this.BrowseButton.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("BrowseButton.Anchor")));
   this.BrowseButton.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("BrowseButton.BackgroundImage")));
   this.BrowseButton.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("BrowseButton.Dock")));
   this.BrowseButton.Enabled = ((bool)(Resource.GetObject("BrowseButton.Enabled")));
   this.BrowseButton.FlatStyle = ((System.Windows.Forms.FlatStyle)(Resource.GetObject("BrowseButton.FlatStyle")));
   this.BrowseButton.Font = ((System.Drawing.Font)(Resource.GetObject("BrowseButton.Font")));
   this.BrowseButton.Image = ((System.Drawing.Image)(Resource.GetObject("BrowseButton.Image")));
   this.BrowseButton.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("BrowseButton.ImageAlign")));
   this.BrowseButton.ImageIndex = ((int)(Resource.GetObject("BrowseButton.ImageIndex")));
   this.BrowseButton.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("BrowseButton.ImeMode")));
   this.BrowseButton.Location = ((System.Drawing.Point)(Resource.GetObject("BrowseButton.Location")));
   this.BrowseButton.Name = "BrowseButton";
   this.BrowseButton.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("BrowseButton.RightToLeft")));
   this.BrowseButton.Size = ((System.Drawing.Size)(Resource.GetObject("BrowseButton.Size")));
   this.BrowseButton.TabIndex = ((int)(Resource.GetObject("BrowseButton.TabIndex")));
   this.BrowseButton.Text = Resource.GetString("BrowseButton.Text");
   this.BrowseButton.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("BrowseButton.TextAlign")));
   this.BrowseButton.Visible = ((bool)(Resource.GetObject("BrowseButton.Visible")));
   this.BrowseButton.Click += new System.EventHandler(this.BrowseButton_Click);



   this.btnCancel.AccessibleDescription = Resource.GetString("btnCancel.AccessibleDescription");
   this.btnCancel.AccessibleName = Resource.GetString("btnCancel.AccessibleName");
   this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("btnCancel.Anchor")));
   this.btnCancel.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("btnCancel.BackgroundImage")));
   this.btnCancel.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("btnCancel.Dock")));
   this.btnCancel.Enabled = ((bool)(Resource.GetObject("btnCancel.Enabled")));
   this.btnCancel.FlatStyle = ((System.Windows.Forms.FlatStyle)(Resource.GetObject("btnCancel.FlatStyle")));
   this.btnCancel.Font = ((System.Drawing.Font)(Resource.GetObject("btnCancel.Font")));
   this.btnCancel.Image = ((System.Drawing.Image)(Resource.GetObject("btnCancel.Image")));
   this.btnCancel.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("btnCancel.ImageAlign")));
   this.btnCancel.ImageIndex = ((int)(Resource.GetObject("btnCancel.ImageIndex")));
   this.btnCancel.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("btnCancel.ImeMode")));
   this.btnCancel.Location = ((System.Drawing.Point)(Resource.GetObject("btnCancel.Location")));
   this.btnCancel.Name = "btnCancel";
   this.btnCancel.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("btnCancel.RightToLeft")));
   this.btnCancel.Size = ((System.Drawing.Size)(Resource.GetObject("btnCancel.Size")));
   this.btnCancel.TabIndex = ((int)(Resource.GetObject("btnCancel.TabIndex")));
   this.btnCancel.Text = Resource.GetString("btnCancel.Text");
   this.btnCancel.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("btnCancel.TextAlign")));
   this.btnCancel.Visible = ((bool)(Resource.GetObject("btnCancel.Visible")));
   this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);



            this.btnHelp.AccessibleDescription = Resource.GetString("btnHelp.AccessibleDescription");
            this.btnHelp.AccessibleName = Resource.GetString("btnHelp.AccessibleName");
            this.btnHelp.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("btnHelp.Anchor")));
            this.btnHelp.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("btnHelp.BackgroundImage")));
            this.btnHelp.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("btnHelp.Dock")));
            this.btnHelp.Enabled = ((bool)(Resource.GetObject("btnHelp.Enabled")));
            this.btnHelp.FlatStyle = ((System.Windows.Forms.FlatStyle)(Resource.GetObject("btnHelp.FlatStyle")));
            this.btnHelp.Font = ((System.Drawing.Font)(Resource.GetObject("btnHelp.Font")));
            this.btnHelp.Image = ((System.Drawing.Image)(Resource.GetObject("btnHelp.Image")));
            this.btnHelp.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("btnHelp.ImageAlign")));
            this.btnHelp.ImageIndex = ((int)(Resource.GetObject("btnHelp.ImageIndex")));
            this.btnHelp.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("btnHelp.ImeMode")));
            this.btnHelp.Location = ((System.Drawing.Point)(Resource.GetObject("btnHelp.Location")));
            this.btnHelp.Name = "btnHelp";
            this.btnHelp.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("btnHelp.RightToLeft")));
            this.btnHelp.Size = ((System.Drawing.Size)(Resource.GetObject("btnHelp.Size")));
            this.btnHelp.TabIndex = ((int)(Resource.GetObject("btnHelp.TabIndex")));
            this.btnHelp.Text = Resource.GetString("btnHelp.Text");
            this.btnHelp.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("btnHelp.TextAlign")));
            this.btnHelp.Visible = ((bool)(Resource.GetObject("btnHelp.Visible")));
            this.btnHelp.Click += new System.EventHandler(this.btnHelp_Click);



   this.btnImport.AccessibleDescription = Resource.GetString("btnImport.AccessibleDescription");
   this.btnImport.AccessibleName = Resource.GetString("btnImport.AccessibleName");
   this.btnImport.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("btnImport.Anchor")));
   this.btnImport.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("btnImport.BackgroundImage")));
   this.btnImport.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("btnImport.Dock")));
   this.btnImport.Enabled = ((bool)(Resource.GetObject("btnImport.Enabled")));
   this.btnImport.FlatStyle = ((System.Windows.Forms.FlatStyle)(Resource.GetObject("btnImport.FlatStyle")));
   this.btnImport.Font = ((System.Drawing.Font)(Resource.GetObject("btnImport.Font")));
   this.btnImport.Image = ((System.Drawing.Image)(Resource.GetObject("btnImport.Image")));
   this.btnImport.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("btnImport.ImageAlign")));
   this.btnImport.ImageIndex = ((int)(Resource.GetObject("btnImport.ImageIndex")));
   this.btnImport.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("btnImport.ImeMode")));
   this.btnImport.Location = ((System.Drawing.Point)(Resource.GetObject("btnImport.Location")));
   this.btnImport.Name = "btnImport";
   this.btnImport.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("btnImport.RightToLeft")));
   this.btnImport.Size = ((System.Drawing.Size)(Resource.GetObject("btnImport.Size")));
   this.btnImport.TabIndex = ((int)(Resource.GetObject("btnImport.TabIndex")));
   this.btnImport.Text = Resource.GetString("btnImport.Text");
   this.btnImport.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("btnImport.TextAlign")));
   this.btnImport.Visible = ((bool)(Resource.GetObject("btnImport.Visible")));
   this.btnImport.Click += new System.EventHandler(this.btnImport_Click);



   this.label1.AccessibleDescription = Resource.GetString("label1.AccessibleDescription");
   this.label1.AccessibleName = Resource.GetString("label1.AccessibleName");
   this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("label1.Anchor")));
   this.label1.AutoSize = ((bool)(Resource.GetObject("label1.AutoSize")));
   this.label1.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("label1.Dock")));
   this.label1.Enabled = ((bool)(Resource.GetObject("label1.Enabled")));
   this.label1.Font = ((System.Drawing.Font)(Resource.GetObject("label1.Font")));
   this.label1.Image = ((System.Drawing.Image)(Resource.GetObject("label1.Image")));
   this.label1.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("label1.ImageAlign")));
   this.label1.ImageIndex = ((int)(Resource.GetObject("label1.ImageIndex")));
   this.label1.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("label1.ImeMode")));
   this.label1.Location = ((System.Drawing.Point)(Resource.GetObject("label1.Location")));
   this.label1.Name = "label1";
   this.label1.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("label1.RightToLeft")));
   this.label1.Size = ((System.Drawing.Size)(Resource.GetObject("label1.Size")));
   this.label1.TabIndex = ((int)(Resource.GetObject("label1.TabIndex")));
   this.label1.Text = Resource.GetString("label1.Text");
   this.label1.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("label1.TextAlign")));
   this.label1.Visible = ((bool)(Resource.GetObject("label1.Visible")));



   this.DomainComboBox.AccessibleDescription = Resource.GetString("DomainComboBox.AccessibleDescription");
   this.DomainComboBox.AccessibleName = Resource.GetString("DomainComboBox.AccessibleName");
   this.DomainComboBox.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("DomainComboBox.Anchor")));
   this.DomainComboBox.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("DomainComboBox.BackgroundImage")));
   this.DomainComboBox.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("DomainComboBox.Dock")));
   this.DomainComboBox.Enabled = ((bool)(Resource.GetObject("DomainComboBox.Enabled")));
   this.DomainComboBox.Font = ((System.Drawing.Font)(Resource.GetObject("DomainComboBox.Font")));
   this.DomainComboBox.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("DomainComboBox.ImeMode")));
   this.DomainComboBox.IntegralHeight = ((bool)(Resource.GetObject("DomainComboBox.IntegralHeight")));
   this.DomainComboBox.ItemHeight = ((int)(Resource.GetObject("DomainComboBox.ItemHeight")));
   this.DomainComboBox.Location = ((System.Drawing.Point)(Resource.GetObject("DomainComboBox.Location")));
   this.DomainComboBox.MaxDropDownItems = ((int)(Resource.GetObject("DomainComboBox.MaxDropDownItems")));
   this.DomainComboBox.MaxLength = ((int)(Resource.GetObject("DomainComboBox.MaxLength")));
   this.DomainComboBox.Name = "DomainComboBox";
   this.DomainComboBox.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("DomainComboBox.RightToLeft")));
   this.DomainComboBox.Size = ((System.Drawing.Size)(Resource.GetObject("DomainComboBox.Size")));
   this.DomainComboBox.TabIndex = ((int)(Resource.GetObject("DomainComboBox.TabIndex")));
   this.DomainComboBox.Text = Resource.GetString("DomainComboBox.Text");
   this.DomainComboBox.Visible = ((bool)(Resource.GetObject("DomainComboBox.Visible")));
            this.DomainComboBox.SelectedIndexChanged += new System.EventHandler(this.domainComboBox_SelectedIndexChanged);




   this.AccessibleDescription = Resource.GetString("$this.AccessibleDescription");
   this.AccessibleName = Resource.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(Resource.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(Resource.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(Resource.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(Resource.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(Resource.GetObject("$this.ClientSize")));
   this.Controls.Add(this.DomainComboBox);
   this.Controls.Add(this.label1);
   this.Controls.Add(this.btnImport);
   this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.btnHelp);
   this.Controls.Add(this.BrowseButton);
   this.Controls.Add(this.LocationEntry);
   this.Controls.Add(this.retypePassphrase);
   this.Controls.Add(this.Passphrase);
   this.Controls.Add(this.oneTimePassphrase);
   this.Controls.Add(this.retypePassphraseLabel);
   this.Controls.Add(this.passPhraseLabel);
   this.Controls.Add(this.oneTimePassphraseLabel);
   this.Controls.Add(this.filePathLabel);
   this.Controls.Add(this.panel);
   this.Enabled = ((bool)(Resource.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(Resource.GetObject("$this.Font")));
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.Icon = ((System.Drawing.Icon)(Resource.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(Resource.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(Resource.GetObject("$this.MaximumSize")));
   this.MinimumSize = ((System.Drawing.Size)(Resource.GetObject("$this.MinimumSize")));
   this.Name = "ImportKeysDialog";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(Resource.GetObject("$this.StartPosition")));
   this.Text = Resource.GetString("$this.Text");
   this.Load += new System.EventHandler(this.ImportKeysDialog_Load);
   this.panel.ResumeLayout(false);
   this.ResumeLayout(false);

  }





        private void btnCancel_Click(object sender, System.EventArgs e)
  {
   this.Dispose();
   this.Close();
  }




        private void btnHelp_Click(object sender, EventArgs e)
        {
            string helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"managingpassphrse.html");
            new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
        }




        private void ImportKeysDialog_Load(object sender, System.EventArgs e)
  {
   this.Icon = new Icon(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder_16.ico"));

   this.waterMark.Image = Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner.png"));
   this.pictureBox.SizeMode = PictureBoxSizeMode.StretchImage;
   this.pictureBox.Image = Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner-scaler.png"));
   this.btnImport.Enabled = false;
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
    }
    else
    {
     this.Hide();
     this.Close();
    }
                UpdateUI();
   }
   catch
   {
   }
  }




  private void UpdateSensitivity()
  {
   if( this.Passphrase.Text.Length >0 && this.retypePassphrase.Text.Length > 0 && this.Passphrase.Text == this.retypePassphrase.Text )

     if( this.LocationEntry.Text.Length > 0 && this.DomainComboBox.SelectedIndex >= 0)
     {
      this.btnImport.Enabled = true;
      return;
     }
   this.btnImport.Enabled = false;
  }




        private void LocationEntry_TextChanged(object sender, EventArgs e)
  {
   UpdateSensitivity();
  }




        private void retypePassphrase_TextChanged(object sender, EventArgs e)
  {
   UpdateSensitivity();
  }




        private void Passphrase_TextChanged(object sender, EventArgs e)
  {
   UpdateSensitivity();
  }




        private void domainComboBox_SelectedIndexChanged(object sender, EventArgs e)
        {


            UpdateUI();
        }




        private void UpdateUI()
        {
            DomainItem domainItem = (DomainItem)this.DomainComboBox.SelectedItem;
            if ( (ifws.GetSecurityPolicy(domainItem.ID) != 0) && simiasWebService.IsPassPhraseSet(domainItem.ID) )
            {
                this.oneTimePassphrase.Enabled = this.LocationEntry.Enabled = this.BrowseButton.Enabled = this.Passphrase.Enabled = this.retypePassphrase.Enabled = true;
            }
            else
            {
                this.oneTimePassphrase.Enabled = this.LocationEntry.Enabled = this.BrowseButton.Enabled = this.Passphrase.Enabled = this.retypePassphrase.Enabled = false;
            }
        }




        private void oneTimePassphrase_TextChanged(object sender, EventArgs e)
  {
   UpdateSensitivity();
  }




        private void BrowseButton_Click(object sender, System.EventArgs e)
  {
   OpenFileDialog fileDlg = new OpenFileDialog();
   fileDlg.ReadOnlyChecked = true;
   fileDlg.ShowDialog();
   this.LocationEntry.Text = fileDlg.FileName;
  }




        private void btnImport_Click(object sender, EventArgs e)
  {
   DomainItem domainItem = (DomainItem)this.DomainComboBox.SelectedItem;
   try
   {
    string onetimepp;
    if( this.oneTimePassphrase != null)
     onetimepp = this.oneTimePassphrase.Text;
    else
     onetimepp = null;
    this.simiasWebService.ImportiFoldersCryptoKeys(domainItem.ID, this.Passphrase.Text, onetimepp, this.LocationEntry.Text);

    bool rememberOption = this.simiasWebService.GetRememberOption(domainItem.ID);

    this.simiasWebService.StorePassPhrase(domainItem.ID, "", CredentialType.None, false);

    this.simiasWebService.StorePassPhrase(domainItem.ID, this.Passphrase.Text, CredentialType.Basic, rememberOption);

    MessageBox.Show(Resource.GetString("ImportKeysSuccess"));
    this.Dispose();
    this.Close();
   }
   catch(Exception ex)
   {
    MessageBox.Show(string.Format(Resource.GetString("ImportErrorMesg"), ex.Message));
    this.Dispose();
    this.Close();
   }
  }




        private void GetLoggedInDomains()
  {
   try
   {
    DomainInformation[] domains;
    domains = this.simiasWebService.GetDomains(true);
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
