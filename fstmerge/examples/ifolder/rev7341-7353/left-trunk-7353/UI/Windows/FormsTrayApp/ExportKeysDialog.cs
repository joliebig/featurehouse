

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



 public class ExportKeysDialog : System.Windows.Forms.Form
 {
  private System.Windows.Forms.Panel panel;
  private System.Windows.Forms.PictureBox waterMark;
  private System.Windows.Forms.Label domainLabel;
  private System.Windows.Forms.Label filePathLabel;
  private System.Windows.Forms.Label recoveryAgentLabel;
  private System.Windows.Forms.Label emailLabel;
  private System.Windows.Forms.ComboBox domainComboBox;
  private System.Windows.Forms.TextBox filePath;
  private System.Windows.Forms.TextBox recoveryAgent;
  private System.Windows.Forms.TextBox emailID;
  private System.Windows.Forms.Button btnCancel;
  private System.Windows.Forms.Button btnExport;
        private System.Windows.Forms.Button btnHelp;
  private System.Windows.Forms.Button BrowseButton;
  private DomainItem selectedDomain;
  private System.Windows.Forms.PictureBox pictureBox;
  private SimiasWebService simiasWebService = null;
  private iFolderWebService ifWebService = null;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(ExportKeysDialog));



  private System.ComponentModel.Container components = null;




  public System.Windows.Forms.ComboBox DomainComboBox
  {
   get
   {
    return this.domainComboBox;
   }
  }






  public ExportKeysDialog(iFolderWebService ifws, SimiasWebService simws)
  {



   InitializeComponent();
   this.simiasWebService = simws;
   this.ifWebService = ifws;



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
   this.domainLabel = new System.Windows.Forms.Label();
   this.filePathLabel = new System.Windows.Forms.Label();
   this.recoveryAgentLabel = new System.Windows.Forms.Label();
   this.emailLabel = new System.Windows.Forms.Label();
   this.domainComboBox = new System.Windows.Forms.ComboBox();
   this.filePath = new System.Windows.Forms.TextBox();
   this.recoveryAgent = new System.Windows.Forms.TextBox();
   this.emailID = new System.Windows.Forms.TextBox();
   this.btnCancel = new System.Windows.Forms.Button();
   this.btnExport = new System.Windows.Forms.Button();
            this.btnHelp = new System.Windows.Forms.Button();
   this.BrowseButton = new System.Windows.Forms.Button();
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



   this.domainLabel.AccessibleDescription = Resource.GetString("domainLabel.AccessibleDescription");
   this.domainLabel.AccessibleName = Resource.GetString("domainLabel.AccessibleName");
   this.domainLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("domainLabel.Anchor")));
   this.domainLabel.AutoSize = ((bool)(Resource.GetObject("domainLabel.AutoSize")));
   this.domainLabel.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("domainLabel.Dock")));
   this.domainLabel.Enabled = ((bool)(Resource.GetObject("domainLabel.Enabled")));
   this.domainLabel.Font = ((System.Drawing.Font)(Resource.GetObject("domainLabel.Font")));
   this.domainLabel.Image = ((System.Drawing.Image)(Resource.GetObject("domainLabel.Image")));
   this.domainLabel.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("domainLabel.ImageAlign")));
   this.domainLabel.ImageIndex = ((int)(Resource.GetObject("domainLabel.ImageIndex")));
   this.domainLabel.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("domainLabel.ImeMode")));
   this.domainLabel.Location = ((System.Drawing.Point)(Resource.GetObject("domainLabel.Location")));
   this.domainLabel.Name = "domainLabel";
   this.domainLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("domainLabel.RightToLeft")));
   this.domainLabel.Size = ((System.Drawing.Size)(Resource.GetObject("domainLabel.Size")));
   this.domainLabel.TabIndex = ((int)(Resource.GetObject("domainLabel.TabIndex")));
   this.domainLabel.Text = Resource.GetString("domainLabel.Text");
   this.domainLabel.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("domainLabel.TextAlign")));
   this.domainLabel.Visible = ((bool)(Resource.GetObject("domainLabel.Visible")));



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



   this.recoveryAgentLabel.AccessibleDescription = Resource.GetString("recoveryAgentLabel.AccessibleDescription");
   this.recoveryAgentLabel.AccessibleName = Resource.GetString("recoveryAgentLabel.AccessibleName");
   this.recoveryAgentLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("recoveryAgentLabel.Anchor")));
   this.recoveryAgentLabel.AutoSize = ((bool)(Resource.GetObject("recoveryAgentLabel.AutoSize")));
   this.recoveryAgentLabel.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("recoveryAgentLabel.Dock")));
   this.recoveryAgentLabel.Enabled = ((bool)(Resource.GetObject("recoveryAgentLabel.Enabled")));
   this.recoveryAgentLabel.Font = ((System.Drawing.Font)(Resource.GetObject("recoveryAgentLabel.Font")));
   this.recoveryAgentLabel.Image = ((System.Drawing.Image)(Resource.GetObject("recoveryAgentLabel.Image")));
   this.recoveryAgentLabel.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("recoveryAgentLabel.ImageAlign")));
   this.recoveryAgentLabel.ImageIndex = ((int)(Resource.GetObject("recoveryAgentLabel.ImageIndex")));
   this.recoveryAgentLabel.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("recoveryAgentLabel.ImeMode")));
   this.recoveryAgentLabel.Location = ((System.Drawing.Point)(Resource.GetObject("recoveryAgentLabel.Location")));
   this.recoveryAgentLabel.Name = "recoveryAgentLabel";
   this.recoveryAgentLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("recoveryAgentLabel.RightToLeft")));
   this.recoveryAgentLabel.Size = ((System.Drawing.Size)(Resource.GetObject("recoveryAgentLabel.Size")));
   this.recoveryAgentLabel.TabIndex = ((int)(Resource.GetObject("recoveryAgentLabel.TabIndex")));
   this.recoveryAgentLabel.Text = Resource.GetString("recoveryAgentLabel.Text");
   this.recoveryAgentLabel.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("recoveryAgentLabel.TextAlign")));
   this.recoveryAgentLabel.Visible = ((bool)(Resource.GetObject("recoveryAgentLabel.Visible")));



   this.emailLabel.AccessibleDescription = Resource.GetString("emailLabel.AccessibleDescription");
   this.emailLabel.AccessibleName = Resource.GetString("emailLabel.AccessibleName");
   this.emailLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("emailLabel.Anchor")));
   this.emailLabel.AutoSize = ((bool)(Resource.GetObject("emailLabel.AutoSize")));
   this.emailLabel.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("emailLabel.Dock")));
   this.emailLabel.Enabled = ((bool)(Resource.GetObject("emailLabel.Enabled")));
   this.emailLabel.Font = ((System.Drawing.Font)(Resource.GetObject("emailLabel.Font")));
   this.emailLabel.Image = ((System.Drawing.Image)(Resource.GetObject("emailLabel.Image")));
   this.emailLabel.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("emailLabel.ImageAlign")));
   this.emailLabel.ImageIndex = ((int)(Resource.GetObject("emailLabel.ImageIndex")));
   this.emailLabel.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("emailLabel.ImeMode")));
   this.emailLabel.Location = ((System.Drawing.Point)(Resource.GetObject("emailLabel.Location")));
   this.emailLabel.Name = "emailLabel";
   this.emailLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("emailLabel.RightToLeft")));
   this.emailLabel.Size = ((System.Drawing.Size)(Resource.GetObject("emailLabel.Size")));
   this.emailLabel.TabIndex = ((int)(Resource.GetObject("emailLabel.TabIndex")));
   this.emailLabel.Text = Resource.GetString("emailLabel.Text");
   this.emailLabel.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("emailLabel.TextAlign")));
   this.emailLabel.Visible = ((bool)(Resource.GetObject("emailLabel.Visible")));



   this.domainComboBox.AccessibleDescription = Resource.GetString("domainComboBox.AccessibleDescription");
   this.domainComboBox.AccessibleName = Resource.GetString("domainComboBox.AccessibleName");
   this.domainComboBox.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("domainComboBox.Anchor")));
   this.domainComboBox.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("domainComboBox.BackgroundImage")));
   this.domainComboBox.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("domainComboBox.Dock")));
   this.domainComboBox.Enabled = ((bool)(Resource.GetObject("domainComboBox.Enabled")));
   this.domainComboBox.Font = ((System.Drawing.Font)(Resource.GetObject("domainComboBox.Font")));
   this.domainComboBox.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("domainComboBox.ImeMode")));
   this.domainComboBox.IntegralHeight = ((bool)(Resource.GetObject("domainComboBox.IntegralHeight")));
   this.domainComboBox.ItemHeight = ((int)(Resource.GetObject("domainComboBox.ItemHeight")));
   this.domainComboBox.Location = ((System.Drawing.Point)(Resource.GetObject("domainComboBox.Location")));
   this.domainComboBox.MaxDropDownItems = ((int)(Resource.GetObject("domainComboBox.MaxDropDownItems")));
   this.domainComboBox.MaxLength = ((int)(Resource.GetObject("domainComboBox.MaxLength")));
   this.domainComboBox.Name = "domainComboBox";
   this.domainComboBox.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("domainComboBox.RightToLeft")));
   this.domainComboBox.Size = ((System.Drawing.Size)(Resource.GetObject("domainComboBox.Size")));
   this.domainComboBox.TabIndex = ((int)(Resource.GetObject("domainComboBox.TabIndex")));
   this.domainComboBox.Text = Resource.GetString("domainComboBox.Text");
   this.domainComboBox.Visible = ((bool)(Resource.GetObject("domainComboBox.Visible")));
   this.domainComboBox.SelectedIndexChanged += new System.EventHandler(this.domainComboBox_SelectedIndexChanged);



   this.filePath.AccessibleDescription = Resource.GetString("filePath.AccessibleDescription");
   this.filePath.AccessibleName = Resource.GetString("filePath.AccessibleName");
   this.filePath.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("filePath.Anchor")));
   this.filePath.AutoSize = ((bool)(Resource.GetObject("filePath.AutoSize")));
   this.filePath.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("filePath.BackgroundImage")));
   this.filePath.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("filePath.Dock")));
   this.filePath.Enabled = ((bool)(Resource.GetObject("filePath.Enabled")));
   this.filePath.Font = ((System.Drawing.Font)(Resource.GetObject("filePath.Font")));
   this.filePath.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("filePath.ImeMode")));
   this.filePath.Location = ((System.Drawing.Point)(Resource.GetObject("filePath.Location")));
   this.filePath.MaxLength = ((int)(Resource.GetObject("filePath.MaxLength")));
   this.filePath.Multiline = ((bool)(Resource.GetObject("filePath.Multiline")));
   this.filePath.Name = "filePath";
   this.filePath.PasswordChar = ((char)(Resource.GetObject("filePath.PasswordChar")));
   this.filePath.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("filePath.RightToLeft")));
   this.filePath.ScrollBars = ((System.Windows.Forms.ScrollBars)(Resource.GetObject("filePath.ScrollBars")));
   this.filePath.Size = ((System.Drawing.Size)(Resource.GetObject("filePath.Size")));
   this.filePath.TabIndex = ((int)(Resource.GetObject("filePath.TabIndex")));
   this.filePath.Text = Resource.GetString("filePath.Text");
   this.filePath.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(Resource.GetObject("filePath.TextAlign")));
   this.filePath.Visible = ((bool)(Resource.GetObject("filePath.Visible")));
   this.filePath.WordWrap = ((bool)(Resource.GetObject("filePath.WordWrap")));
   this.filePath.TextChanged += new System.EventHandler(this.filePath_TextChanged);



   this.recoveryAgent.AccessibleDescription = Resource.GetString("recoveryAgent.AccessibleDescription");
   this.recoveryAgent.AccessibleName = Resource.GetString("recoveryAgent.AccessibleName");
   this.recoveryAgent.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("recoveryAgent.Anchor")));
   this.recoveryAgent.AutoSize = ((bool)(Resource.GetObject("recoveryAgent.AutoSize")));
   this.recoveryAgent.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("recoveryAgent.BackgroundImage")));
   this.recoveryAgent.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("recoveryAgent.Dock")));
   this.recoveryAgent.Enabled = ((bool)(Resource.GetObject("recoveryAgent.Enabled")));
   this.recoveryAgent.Font = ((System.Drawing.Font)(Resource.GetObject("recoveryAgent.Font")));
   this.recoveryAgent.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("recoveryAgent.ImeMode")));
   this.recoveryAgent.Location = ((System.Drawing.Point)(Resource.GetObject("recoveryAgent.Location")));
   this.recoveryAgent.MaxLength = ((int)(Resource.GetObject("recoveryAgent.MaxLength")));
   this.recoveryAgent.Multiline = ((bool)(Resource.GetObject("recoveryAgent.Multiline")));
   this.recoveryAgent.Name = "recoveryAgent";
   this.recoveryAgent.PasswordChar = ((char)(Resource.GetObject("recoveryAgent.PasswordChar")));
   this.recoveryAgent.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("recoveryAgent.RightToLeft")));
   this.recoveryAgent.ScrollBars = ((System.Windows.Forms.ScrollBars)(Resource.GetObject("recoveryAgent.ScrollBars")));
   this.recoveryAgent.Size = ((System.Drawing.Size)(Resource.GetObject("recoveryAgent.Size")));
   this.recoveryAgent.TabIndex = ((int)(Resource.GetObject("recoveryAgent.TabIndex")));
   this.recoveryAgent.Text = Resource.GetString("recoveryAgent.Text");
   this.recoveryAgent.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(Resource.GetObject("recoveryAgent.TextAlign")));
   this.recoveryAgent.Visible = ((bool)(Resource.GetObject("recoveryAgent.Visible")));
   this.recoveryAgent.WordWrap = ((bool)(Resource.GetObject("recoveryAgent.WordWrap")));



   this.emailID.AccessibleDescription = Resource.GetString("emailID.AccessibleDescription");
   this.emailID.AccessibleName = Resource.GetString("emailID.AccessibleName");
   this.emailID.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("emailID.Anchor")));
   this.emailID.AutoSize = ((bool)(Resource.GetObject("emailID.AutoSize")));
   this.emailID.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("emailID.BackgroundImage")));
   this.emailID.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("emailID.Dock")));
   this.emailID.Enabled = ((bool)(Resource.GetObject("emailID.Enabled")));
   this.emailID.Font = ((System.Drawing.Font)(Resource.GetObject("emailID.Font")));
   this.emailID.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("emailID.ImeMode")));
   this.emailID.Location = ((System.Drawing.Point)(Resource.GetObject("emailID.Location")));
   this.emailID.MaxLength = ((int)(Resource.GetObject("emailID.MaxLength")));
   this.emailID.Multiline = ((bool)(Resource.GetObject("emailID.Multiline")));
   this.emailID.Name = "emailID";
   this.emailID.PasswordChar = ((char)(Resource.GetObject("emailID.PasswordChar")));
   this.emailID.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("emailID.RightToLeft")));
   this.emailID.ScrollBars = ((System.Windows.Forms.ScrollBars)(Resource.GetObject("emailID.ScrollBars")));
   this.emailID.Size = ((System.Drawing.Size)(Resource.GetObject("emailID.Size")));
   this.emailID.TabIndex = ((int)(Resource.GetObject("emailID.TabIndex")));
   this.emailID.Text = Resource.GetString("emailID.Text");
   this.emailID.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(Resource.GetObject("emailID.TextAlign")));
   this.emailID.Visible = ((bool)(Resource.GetObject("emailID.Visible")));
   this.emailID.WordWrap = ((bool)(Resource.GetObject("emailID.WordWrap")));



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



   this.btnExport.AccessibleDescription = Resource.GetString("btnExport.AccessibleDescription");
   this.btnExport.AccessibleName = Resource.GetString("btnExport.AccessibleName");
   this.btnExport.Anchor = ((System.Windows.Forms.AnchorStyles)(Resource.GetObject("btnExport.Anchor")));
   this.btnExport.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("btnExport.BackgroundImage")));
   this.btnExport.Dock = ((System.Windows.Forms.DockStyle)(Resource.GetObject("btnExport.Dock")));
   this.btnExport.Enabled = ((bool)(Resource.GetObject("btnExport.Enabled")));
   this.btnExport.FlatStyle = ((System.Windows.Forms.FlatStyle)(Resource.GetObject("btnExport.FlatStyle")));
   this.btnExport.Font = ((System.Drawing.Font)(Resource.GetObject("btnExport.Font")));
   this.btnExport.Image = ((System.Drawing.Image)(Resource.GetObject("btnExport.Image")));
   this.btnExport.ImageAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("btnExport.ImageAlign")));
   this.btnExport.ImageIndex = ((int)(Resource.GetObject("btnExport.ImageIndex")));
   this.btnExport.ImeMode = ((System.Windows.Forms.ImeMode)(Resource.GetObject("btnExport.ImeMode")));
   this.btnExport.Location = ((System.Drawing.Point)(Resource.GetObject("btnExport.Location")));
   this.btnExport.Name = "btnExport";
   this.btnExport.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("btnExport.RightToLeft")));
   this.btnExport.Size = ((System.Drawing.Size)(Resource.GetObject("btnExport.Size")));
   this.btnExport.TabIndex = ((int)(Resource.GetObject("btnExport.TabIndex")));
   this.btnExport.Text = Resource.GetString("btnExport.Text");
   this.btnExport.TextAlign = ((System.Drawing.ContentAlignment)(Resource.GetObject("btnExport.TextAlign")));
   this.btnExport.Visible = ((bool)(Resource.GetObject("btnExport.Visible")));
   this.btnExport.Click += new System.EventHandler(this.btnExport_Click);



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



   this.AccessibleDescription = Resource.GetString("$this.AccessibleDescription");
   this.AccessibleName = Resource.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(Resource.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(Resource.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(Resource.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(Resource.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(Resource.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(Resource.GetObject("$this.ClientSize")));
   this.Controls.Add(this.BrowseButton);
   this.Controls.Add(this.btnExport);
            this.Controls.Add(this.btnHelp);
   this.Controls.Add(this.btnCancel);
   this.Controls.Add(this.emailID);
   this.Controls.Add(this.recoveryAgent);
   this.Controls.Add(this.filePath);
   this.Controls.Add(this.domainComboBox);
   this.Controls.Add(this.emailLabel);
   this.Controls.Add(this.recoveryAgentLabel);
   this.Controls.Add(this.filePathLabel);
   this.Controls.Add(this.domainLabel);
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
   this.Name = "ExportKeysDialog";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(Resource.GetObject("$this.RightToLeft")));
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(Resource.GetObject("$this.StartPosition")));
   this.Text = Resource.GetString("$this.Text");
   this.Load += new System.EventHandler(this.ExportKeysDialog_Load);
   this.panel.ResumeLayout(false);
   this.ResumeLayout(false);

  }





        private void filePath_TextChanged(object sender, System.EventArgs e)
  {
   if( this.filePath.Text.Length > 0 && this.domainComboBox.SelectedIndex >=0)
    this.btnExport.Enabled = true;
   else
    this.btnExport.Enabled = false;
  }




        private void BrowseButton_Click(object sender, System.EventArgs e)
  {
   SaveFileDialog fileDlg = new SaveFileDialog();
   fileDlg.ShowDialog();
   this.filePath.Text = fileDlg.FileName;
  }




        private void btnCancel_Click(object sender, System.EventArgs e)
  {
   this.Dispose();
   this.Close();
  }




        private void ExportKeysDialog_Load(object sender, System.EventArgs e)
  {
   this.Icon = new Icon(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder_16.ico"));

   this.waterMark.Image = Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner.png"));
   this.pictureBox.SizeMode = PictureBoxSizeMode.StretchImage;
   this.pictureBox.Image = Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner-scaler.png"));
   this.btnCancel.Select();

   try
   {
    XmlDocument domainsDoc = new XmlDocument();
    domainsDoc.Load(System.IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "domain.list"));
    XmlElement element = (XmlElement)domainsDoc.SelectSingleNode("/domains");


    XmlElement defaultDomainElement = (XmlElement)domainsDoc.SelectSingleNode("/domains/defaultDomain");
    string defaultID = defaultDomainElement.GetAttribute("ID");



    XmlNodeList nodeList = element.GetElementsByTagName("domain");
    foreach (XmlNode node in nodeList)
    {
     string name = ((XmlElement)node).GetAttribute("name");
     string id = ((XmlElement)node).GetAttribute("ID");

     DomainItem domainItem = new DomainItem(name, id);
     this.DomainComboBox.Items.Add(domainItem);
     if (id.Equals(defaultID))
     {
      selectedDomain = domainItem;
     }
    }

    if (selectedDomain != null)
    {
     this.DomainComboBox.SelectedItem = selectedDomain;
     DisplayRAName(selectedDomain);
    }
    else
     this.DomainComboBox.SelectedIndex = 0;
                UpdateUI();
   }
   catch (Exception ex)
   {
    MessageBox.Show("ExportKeysDialog_Load {0}", ex.Message);
   }
  }




        private void domainComboBox_SelectedIndexChanged(object sender, EventArgs e)
  {


   DomainItem domainItem = (DomainItem)this.domainComboBox.SelectedItem;
   DisplayRAName(selectedDomain);
            UpdateUI();
  }




        private void UpdateUI()
        {
            DomainItem domainItem = (DomainItem)this.DomainComboBox.SelectedItem;
            if ( (ifWebService.GetSecurityPolicy(domainItem.ID) != 0) && ( simiasWebService.IsPassPhraseSet(domainItem.ID)) )
            {
                this.filePath.Enabled = this.BrowseButton.Enabled = true;
            }
            else
            {
                this.filePath.Enabled = this.BrowseButton.Enabled = false;
            }
        }





  private void DisplayRAName(DomainItem selectedDomain)
  {
   try
   {
    string RAName = this.ifWebService.GetRAName(selectedDomain.ID);
    if(RAName == null || RAName == "")
    {
     return;
    }
    else
    {

     this.recoveryAgent.Text = RAName;
     char [] EmailParser = {'='};
     string [] ParsedString = RAName.Split(EmailParser);
     string emailID = "";
     if (ParsedString.Length > 1)
     {
      for(int x = 0; x < ParsedString.Length; x++)
      {
       char [] FinalEmailParser = {'@'};
       string [] FinalParsedString = ParsedString[x].Split(FinalEmailParser);
       if(FinalParsedString.Length > 1)
       {
        emailID = ParsedString[x];
        this.emailID.Text = emailID;
       }
      }
     }
    }
   }
   catch(Exception ex)
   {
    MessageBox.Show("DisplayRAName : {0}", ex.Message);
   }
  }




        private void btnExport_Click(object sender, EventArgs e)
  {
   DomainItem domainItem = (DomainItem)this.domainComboBox.SelectedItem;
   try
   {
    this.simiasWebService.ExportiFoldersCryptoKeys(domainItem.ID, this.filePath.Text);
   }
   catch(Exception ex)
   {
    MessageBox.Show(string.Format(Resource.GetString("UnableToExportMesg"), ex.Message));
    return;
   }
   MessageBox.Show(Resource.GetString("ExportKeysSuccess"));
   this.Dispose();
   this.Close();
  }




        private void btnHelp_Click(object sender, EventArgs e)
        {
            string helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"managingpassphrse.html");
            new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
        }
 }
}
