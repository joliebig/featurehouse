
using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;

using Novell.iFolder.Web;
using Novell.iFolderCom;

namespace Novell.FormsTrayApp
{



 public class ServerDetails : System.Windows.Forms.Form
 {
  private const double megaByte = 1048576;
  private System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(ServerDetails));
  private iFolderWebService ifWebService;
  private SimiasWebService simiasWebService;
  private Domain domain;
  private bool updatePassword = false;
  private bool enableChanged = false;
  private bool defaultChanged = false;
  private bool addressChanged = false;
  private System.Windows.Forms.Label label18;
  private System.Windows.Forms.Label label17;
  private Novell.iFolderCom.GaugeChart gaugeChart1;
  private System.Windows.Forms.Label totalSpaceUnits;
  private System.Windows.Forms.Label usedSpaceUnits;
  private System.Windows.Forms.Label freeSpaceUnits;
  private System.Windows.Forms.Label totalSpace;
  private System.Windows.Forms.Label usedSpace;
  private System.Windows.Forms.Label freeSpace;
  private System.Windows.Forms.Label label13;
  private System.Windows.Forms.Label label12;
  private System.Windows.Forms.Label label11;
  private System.Windows.Forms.TextBox enterpriseDescription;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Label server;
  private System.Windows.Forms.TabControl tabControl1;
  private System.Windows.Forms.TabPage tabPage1;
  private System.Windows.Forms.TabPage tabPage2;
  private System.Windows.Forms.TabPage tabPage3;
  private System.Windows.Forms.GroupBox groupBox1;
  private System.Windows.Forms.Label serverNameLabel;
  private System.Windows.Forms.Label serverAddressLabel;
  private System.Windows.Forms.TextBox address;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.CheckBox accountEnabled;
  private System.Windows.Forms.CheckBox defaultServer;
  private System.Windows.Forms.Label usernameLabel;
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Label username;
  private System.Windows.Forms.TextBox password;
  private System.Windows.Forms.CheckBox rememberPassword;
  private System.Windows.Forms.Button cancel;



  private System.ComponentModel.Container components = null;






  public ServerDetails(SimiasWebService simiasWebService, iFolderWebService ifolderWebService, Domain domain)
  {



   InitializeComponent();

   this.simiasWebService = simiasWebService;
   ifWebService = ifolderWebService;
   this.domain = domain;
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(ServerDetails));
   this.label18 = new System.Windows.Forms.Label();
   this.label17 = new System.Windows.Forms.Label();
   this.gaugeChart1 = new Novell.iFolderCom.GaugeChart();
   this.totalSpaceUnits = new System.Windows.Forms.Label();
   this.usedSpaceUnits = new System.Windows.Forms.Label();
   this.freeSpaceUnits = new System.Windows.Forms.Label();
   this.totalSpace = new System.Windows.Forms.Label();
   this.usedSpace = new System.Windows.Forms.Label();
   this.freeSpace = new System.Windows.Forms.Label();
   this.label13 = new System.Windows.Forms.Label();
   this.label12 = new System.Windows.Forms.Label();
   this.label11 = new System.Windows.Forms.Label();
   this.enterpriseDescription = new System.Windows.Forms.TextBox();
   this.ok = new System.Windows.Forms.Button();
   this.server = new System.Windows.Forms.Label();
   this.tabControl1 = new System.Windows.Forms.TabControl();
   this.tabPage1 = new System.Windows.Forms.TabPage();
   this.label2 = new System.Windows.Forms.Label();
   this.address = new System.Windows.Forms.TextBox();
   this.serverAddressLabel = new System.Windows.Forms.Label();
   this.serverNameLabel = new System.Windows.Forms.Label();
   this.tabPage2 = new System.Windows.Forms.TabPage();
   this.rememberPassword = new System.Windows.Forms.CheckBox();
   this.password = new System.Windows.Forms.TextBox();
   this.username = new System.Windows.Forms.Label();
   this.label1 = new System.Windows.Forms.Label();
   this.usernameLabel = new System.Windows.Forms.Label();
   this.tabPage3 = new System.Windows.Forms.TabPage();
   this.groupBox1 = new System.Windows.Forms.GroupBox();
   this.accountEnabled = new System.Windows.Forms.CheckBox();
   this.defaultServer = new System.Windows.Forms.CheckBox();
   this.cancel = new System.Windows.Forms.Button();
   this.tabControl1.SuspendLayout();
   this.tabPage1.SuspendLayout();
   this.tabPage2.SuspendLayout();
   this.tabPage3.SuspendLayout();
   this.groupBox1.SuspendLayout();
   this.SuspendLayout();



   this.label18.AccessibleDescription = resources.GetString("label18.AccessibleDescription");
   this.label18.AccessibleName = resources.GetString("label18.AccessibleName");
   this.label18.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label18.Anchor")));
   this.label18.AutoSize = ((bool)(resources.GetObject("label18.AutoSize")));
   this.label18.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label18.Dock")));
   this.label18.Enabled = ((bool)(resources.GetObject("label18.Enabled")));
   this.label18.Font = ((System.Drawing.Font)(resources.GetObject("label18.Font")));
   this.label18.Image = ((System.Drawing.Image)(resources.GetObject("label18.Image")));
   this.label18.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label18.ImageAlign")));
   this.label18.ImageIndex = ((int)(resources.GetObject("label18.ImageIndex")));
   this.label18.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label18.ImeMode")));
   this.label18.Location = ((System.Drawing.Point)(resources.GetObject("label18.Location")));
   this.label18.Name = "label18";
   this.label18.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label18.RightToLeft")));
   this.label18.Size = ((System.Drawing.Size)(resources.GetObject("label18.Size")));
   this.label18.TabIndex = ((int)(resources.GetObject("label18.TabIndex")));
   this.label18.Text = resources.GetString("label18.Text");
   this.label18.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label18.TextAlign")));
   this.label18.Visible = ((bool)(resources.GetObject("label18.Visible")));



   this.label17.AccessibleDescription = resources.GetString("label17.AccessibleDescription");
   this.label17.AccessibleName = resources.GetString("label17.AccessibleName");
   this.label17.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label17.Anchor")));
   this.label17.AutoSize = ((bool)(resources.GetObject("label17.AutoSize")));
   this.label17.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label17.Dock")));
   this.label17.Enabled = ((bool)(resources.GetObject("label17.Enabled")));
   this.label17.Font = ((System.Drawing.Font)(resources.GetObject("label17.Font")));
   this.label17.Image = ((System.Drawing.Image)(resources.GetObject("label17.Image")));
   this.label17.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label17.ImageAlign")));
   this.label17.ImageIndex = ((int)(resources.GetObject("label17.ImageIndex")));
   this.label17.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label17.ImeMode")));
   this.label17.Location = ((System.Drawing.Point)(resources.GetObject("label17.Location")));
   this.label17.Name = "label17";
   this.label17.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label17.RightToLeft")));
   this.label17.Size = ((System.Drawing.Size)(resources.GetObject("label17.Size")));
   this.label17.TabIndex = ((int)(resources.GetObject("label17.TabIndex")));
   this.label17.Text = resources.GetString("label17.Text");
   this.label17.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label17.TextAlign")));
   this.label17.Visible = ((bool)(resources.GetObject("label17.Visible")));



   this.gaugeChart1.AccessibleDescription = resources.GetString("gaugeChart1.AccessibleDescription");
   this.gaugeChart1.AccessibleName = resources.GetString("gaugeChart1.AccessibleName");
   this.gaugeChart1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("gaugeChart1.Anchor")));
   this.gaugeChart1.AutoScroll = ((bool)(resources.GetObject("gaugeChart1.AutoScroll")));
   this.gaugeChart1.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("gaugeChart1.AutoScrollMargin")));
   this.gaugeChart1.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("gaugeChart1.AutoScrollMinSize")));
   this.gaugeChart1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("gaugeChart1.BackgroundImage")));
   this.gaugeChart1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("gaugeChart1.Dock")));
   this.gaugeChart1.Enabled = ((bool)(resources.GetObject("gaugeChart1.Enabled")));
   this.gaugeChart1.Font = ((System.Drawing.Font)(resources.GetObject("gaugeChart1.Font")));
   this.gaugeChart1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("gaugeChart1.ImeMode")));
   this.gaugeChart1.Location = ((System.Drawing.Point)(resources.GetObject("gaugeChart1.Location")));
   this.gaugeChart1.Name = "gaugeChart1";
   this.gaugeChart1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("gaugeChart1.RightToLeft")));
   this.gaugeChart1.Size = ((System.Drawing.Size)(resources.GetObject("gaugeChart1.Size")));
   this.gaugeChart1.TabIndex = ((int)(resources.GetObject("gaugeChart1.TabIndex")));
   this.gaugeChart1.Visible = ((bool)(resources.GetObject("gaugeChart1.Visible")));



   this.totalSpaceUnits.AccessibleDescription = resources.GetString("totalSpaceUnits.AccessibleDescription");
   this.totalSpaceUnits.AccessibleName = resources.GetString("totalSpaceUnits.AccessibleName");
   this.totalSpaceUnits.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("totalSpaceUnits.Anchor")));
   this.totalSpaceUnits.AutoSize = ((bool)(resources.GetObject("totalSpaceUnits.AutoSize")));
   this.totalSpaceUnits.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("totalSpaceUnits.Dock")));
   this.totalSpaceUnits.Enabled = ((bool)(resources.GetObject("totalSpaceUnits.Enabled")));
   this.totalSpaceUnits.Font = ((System.Drawing.Font)(resources.GetObject("totalSpaceUnits.Font")));
   this.totalSpaceUnits.Image = ((System.Drawing.Image)(resources.GetObject("totalSpaceUnits.Image")));
   this.totalSpaceUnits.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("totalSpaceUnits.ImageAlign")));
   this.totalSpaceUnits.ImageIndex = ((int)(resources.GetObject("totalSpaceUnits.ImageIndex")));
   this.totalSpaceUnits.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("totalSpaceUnits.ImeMode")));
   this.totalSpaceUnits.Location = ((System.Drawing.Point)(resources.GetObject("totalSpaceUnits.Location")));
   this.totalSpaceUnits.Name = "totalSpaceUnits";
   this.totalSpaceUnits.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("totalSpaceUnits.RightToLeft")));
   this.totalSpaceUnits.Size = ((System.Drawing.Size)(resources.GetObject("totalSpaceUnits.Size")));
   this.totalSpaceUnits.TabIndex = ((int)(resources.GetObject("totalSpaceUnits.TabIndex")));
   this.totalSpaceUnits.Text = resources.GetString("totalSpaceUnits.Text");
   this.totalSpaceUnits.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("totalSpaceUnits.TextAlign")));
   this.totalSpaceUnits.Visible = ((bool)(resources.GetObject("totalSpaceUnits.Visible")));



   this.usedSpaceUnits.AccessibleDescription = resources.GetString("usedSpaceUnits.AccessibleDescription");
   this.usedSpaceUnits.AccessibleName = resources.GetString("usedSpaceUnits.AccessibleName");
   this.usedSpaceUnits.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("usedSpaceUnits.Anchor")));
   this.usedSpaceUnits.AutoSize = ((bool)(resources.GetObject("usedSpaceUnits.AutoSize")));
   this.usedSpaceUnits.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("usedSpaceUnits.Dock")));
   this.usedSpaceUnits.Enabled = ((bool)(resources.GetObject("usedSpaceUnits.Enabled")));
   this.usedSpaceUnits.Font = ((System.Drawing.Font)(resources.GetObject("usedSpaceUnits.Font")));
   this.usedSpaceUnits.Image = ((System.Drawing.Image)(resources.GetObject("usedSpaceUnits.Image")));
   this.usedSpaceUnits.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("usedSpaceUnits.ImageAlign")));
   this.usedSpaceUnits.ImageIndex = ((int)(resources.GetObject("usedSpaceUnits.ImageIndex")));
   this.usedSpaceUnits.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("usedSpaceUnits.ImeMode")));
   this.usedSpaceUnits.Location = ((System.Drawing.Point)(resources.GetObject("usedSpaceUnits.Location")));
   this.usedSpaceUnits.Name = "usedSpaceUnits";
   this.usedSpaceUnits.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("usedSpaceUnits.RightToLeft")));
   this.usedSpaceUnits.Size = ((System.Drawing.Size)(resources.GetObject("usedSpaceUnits.Size")));
   this.usedSpaceUnits.TabIndex = ((int)(resources.GetObject("usedSpaceUnits.TabIndex")));
   this.usedSpaceUnits.Text = resources.GetString("usedSpaceUnits.Text");
   this.usedSpaceUnits.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("usedSpaceUnits.TextAlign")));
   this.usedSpaceUnits.Visible = ((bool)(resources.GetObject("usedSpaceUnits.Visible")));



   this.freeSpaceUnits.AccessibleDescription = resources.GetString("freeSpaceUnits.AccessibleDescription");
   this.freeSpaceUnits.AccessibleName = resources.GetString("freeSpaceUnits.AccessibleName");
   this.freeSpaceUnits.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("freeSpaceUnits.Anchor")));
   this.freeSpaceUnits.AutoSize = ((bool)(resources.GetObject("freeSpaceUnits.AutoSize")));
   this.freeSpaceUnits.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("freeSpaceUnits.Dock")));
   this.freeSpaceUnits.Enabled = ((bool)(resources.GetObject("freeSpaceUnits.Enabled")));
   this.freeSpaceUnits.Font = ((System.Drawing.Font)(resources.GetObject("freeSpaceUnits.Font")));
   this.freeSpaceUnits.Image = ((System.Drawing.Image)(resources.GetObject("freeSpaceUnits.Image")));
   this.freeSpaceUnits.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("freeSpaceUnits.ImageAlign")));
   this.freeSpaceUnits.ImageIndex = ((int)(resources.GetObject("freeSpaceUnits.ImageIndex")));
   this.freeSpaceUnits.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("freeSpaceUnits.ImeMode")));
   this.freeSpaceUnits.Location = ((System.Drawing.Point)(resources.GetObject("freeSpaceUnits.Location")));
   this.freeSpaceUnits.Name = "freeSpaceUnits";
   this.freeSpaceUnits.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("freeSpaceUnits.RightToLeft")));
   this.freeSpaceUnits.Size = ((System.Drawing.Size)(resources.GetObject("freeSpaceUnits.Size")));
   this.freeSpaceUnits.TabIndex = ((int)(resources.GetObject("freeSpaceUnits.TabIndex")));
   this.freeSpaceUnits.Text = resources.GetString("freeSpaceUnits.Text");
   this.freeSpaceUnits.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("freeSpaceUnits.TextAlign")));
   this.freeSpaceUnits.Visible = ((bool)(resources.GetObject("freeSpaceUnits.Visible")));



   this.totalSpace.AccessibleDescription = resources.GetString("totalSpace.AccessibleDescription");
   this.totalSpace.AccessibleName = resources.GetString("totalSpace.AccessibleName");
   this.totalSpace.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("totalSpace.Anchor")));
   this.totalSpace.AutoSize = ((bool)(resources.GetObject("totalSpace.AutoSize")));
   this.totalSpace.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("totalSpace.Dock")));
   this.totalSpace.Enabled = ((bool)(resources.GetObject("totalSpace.Enabled")));
   this.totalSpace.Font = ((System.Drawing.Font)(resources.GetObject("totalSpace.Font")));
   this.totalSpace.Image = ((System.Drawing.Image)(resources.GetObject("totalSpace.Image")));
   this.totalSpace.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("totalSpace.ImageAlign")));
   this.totalSpace.ImageIndex = ((int)(resources.GetObject("totalSpace.ImageIndex")));
   this.totalSpace.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("totalSpace.ImeMode")));
   this.totalSpace.Location = ((System.Drawing.Point)(resources.GetObject("totalSpace.Location")));
   this.totalSpace.Name = "totalSpace";
   this.totalSpace.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("totalSpace.RightToLeft")));
   this.totalSpace.Size = ((System.Drawing.Size)(resources.GetObject("totalSpace.Size")));
   this.totalSpace.TabIndex = ((int)(resources.GetObject("totalSpace.TabIndex")));
   this.totalSpace.Text = resources.GetString("totalSpace.Text");
   this.totalSpace.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("totalSpace.TextAlign")));
   this.totalSpace.Visible = ((bool)(resources.GetObject("totalSpace.Visible")));



   this.usedSpace.AccessibleDescription = resources.GetString("usedSpace.AccessibleDescription");
   this.usedSpace.AccessibleName = resources.GetString("usedSpace.AccessibleName");
   this.usedSpace.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("usedSpace.Anchor")));
   this.usedSpace.AutoSize = ((bool)(resources.GetObject("usedSpace.AutoSize")));
   this.usedSpace.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("usedSpace.Dock")));
   this.usedSpace.Enabled = ((bool)(resources.GetObject("usedSpace.Enabled")));
   this.usedSpace.Font = ((System.Drawing.Font)(resources.GetObject("usedSpace.Font")));
   this.usedSpace.Image = ((System.Drawing.Image)(resources.GetObject("usedSpace.Image")));
   this.usedSpace.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("usedSpace.ImageAlign")));
   this.usedSpace.ImageIndex = ((int)(resources.GetObject("usedSpace.ImageIndex")));
   this.usedSpace.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("usedSpace.ImeMode")));
   this.usedSpace.Location = ((System.Drawing.Point)(resources.GetObject("usedSpace.Location")));
   this.usedSpace.Name = "usedSpace";
   this.usedSpace.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("usedSpace.RightToLeft")));
   this.usedSpace.Size = ((System.Drawing.Size)(resources.GetObject("usedSpace.Size")));
   this.usedSpace.TabIndex = ((int)(resources.GetObject("usedSpace.TabIndex")));
   this.usedSpace.Text = resources.GetString("usedSpace.Text");
   this.usedSpace.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("usedSpace.TextAlign")));
   this.usedSpace.Visible = ((bool)(resources.GetObject("usedSpace.Visible")));



   this.freeSpace.AccessibleDescription = resources.GetString("freeSpace.AccessibleDescription");
   this.freeSpace.AccessibleName = resources.GetString("freeSpace.AccessibleName");
   this.freeSpace.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("freeSpace.Anchor")));
   this.freeSpace.AutoSize = ((bool)(resources.GetObject("freeSpace.AutoSize")));
   this.freeSpace.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("freeSpace.Dock")));
   this.freeSpace.Enabled = ((bool)(resources.GetObject("freeSpace.Enabled")));
   this.freeSpace.Font = ((System.Drawing.Font)(resources.GetObject("freeSpace.Font")));
   this.freeSpace.Image = ((System.Drawing.Image)(resources.GetObject("freeSpace.Image")));
   this.freeSpace.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("freeSpace.ImageAlign")));
   this.freeSpace.ImageIndex = ((int)(resources.GetObject("freeSpace.ImageIndex")));
   this.freeSpace.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("freeSpace.ImeMode")));
   this.freeSpace.Location = ((System.Drawing.Point)(resources.GetObject("freeSpace.Location")));
   this.freeSpace.Name = "freeSpace";
   this.freeSpace.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("freeSpace.RightToLeft")));
   this.freeSpace.Size = ((System.Drawing.Size)(resources.GetObject("freeSpace.Size")));
   this.freeSpace.TabIndex = ((int)(resources.GetObject("freeSpace.TabIndex")));
   this.freeSpace.Text = resources.GetString("freeSpace.Text");
   this.freeSpace.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("freeSpace.TextAlign")));
   this.freeSpace.Visible = ((bool)(resources.GetObject("freeSpace.Visible")));



   this.label13.AccessibleDescription = resources.GetString("label13.AccessibleDescription");
   this.label13.AccessibleName = resources.GetString("label13.AccessibleName");
   this.label13.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label13.Anchor")));
   this.label13.AutoSize = ((bool)(resources.GetObject("label13.AutoSize")));
   this.label13.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label13.Dock")));
   this.label13.Enabled = ((bool)(resources.GetObject("label13.Enabled")));
   this.label13.Font = ((System.Drawing.Font)(resources.GetObject("label13.Font")));
   this.label13.Image = ((System.Drawing.Image)(resources.GetObject("label13.Image")));
   this.label13.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label13.ImageAlign")));
   this.label13.ImageIndex = ((int)(resources.GetObject("label13.ImageIndex")));
   this.label13.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label13.ImeMode")));
   this.label13.Location = ((System.Drawing.Point)(resources.GetObject("label13.Location")));
   this.label13.Name = "label13";
   this.label13.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label13.RightToLeft")));
   this.label13.Size = ((System.Drawing.Size)(resources.GetObject("label13.Size")));
   this.label13.TabIndex = ((int)(resources.GetObject("label13.TabIndex")));
   this.label13.Text = resources.GetString("label13.Text");
   this.label13.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label13.TextAlign")));
   this.label13.Visible = ((bool)(resources.GetObject("label13.Visible")));



   this.label12.AccessibleDescription = resources.GetString("label12.AccessibleDescription");
   this.label12.AccessibleName = resources.GetString("label12.AccessibleName");
   this.label12.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label12.Anchor")));
   this.label12.AutoSize = ((bool)(resources.GetObject("label12.AutoSize")));
   this.label12.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label12.Dock")));
   this.label12.Enabled = ((bool)(resources.GetObject("label12.Enabled")));
   this.label12.Font = ((System.Drawing.Font)(resources.GetObject("label12.Font")));
   this.label12.Image = ((System.Drawing.Image)(resources.GetObject("label12.Image")));
   this.label12.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label12.ImageAlign")));
   this.label12.ImageIndex = ((int)(resources.GetObject("label12.ImageIndex")));
   this.label12.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label12.ImeMode")));
   this.label12.Location = ((System.Drawing.Point)(resources.GetObject("label12.Location")));
   this.label12.Name = "label12";
   this.label12.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label12.RightToLeft")));
   this.label12.Size = ((System.Drawing.Size)(resources.GetObject("label12.Size")));
   this.label12.TabIndex = ((int)(resources.GetObject("label12.TabIndex")));
   this.label12.Text = resources.GetString("label12.Text");
   this.label12.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label12.TextAlign")));
   this.label12.Visible = ((bool)(resources.GetObject("label12.Visible")));



   this.label11.AccessibleDescription = resources.GetString("label11.AccessibleDescription");
   this.label11.AccessibleName = resources.GetString("label11.AccessibleName");
   this.label11.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label11.Anchor")));
   this.label11.AutoSize = ((bool)(resources.GetObject("label11.AutoSize")));
   this.label11.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label11.Dock")));
   this.label11.Enabled = ((bool)(resources.GetObject("label11.Enabled")));
   this.label11.Font = ((System.Drawing.Font)(resources.GetObject("label11.Font")));
   this.label11.Image = ((System.Drawing.Image)(resources.GetObject("label11.Image")));
   this.label11.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label11.ImageAlign")));
   this.label11.ImageIndex = ((int)(resources.GetObject("label11.ImageIndex")));
   this.label11.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label11.ImeMode")));
   this.label11.Location = ((System.Drawing.Point)(resources.GetObject("label11.Location")));
   this.label11.Name = "label11";
   this.label11.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label11.RightToLeft")));
   this.label11.Size = ((System.Drawing.Size)(resources.GetObject("label11.Size")));
   this.label11.TabIndex = ((int)(resources.GetObject("label11.TabIndex")));
   this.label11.Text = resources.GetString("label11.Text");
   this.label11.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label11.TextAlign")));
   this.label11.Visible = ((bool)(resources.GetObject("label11.Visible")));



   this.enterpriseDescription.AccessibleDescription = resources.GetString("enterpriseDescription.AccessibleDescription");
   this.enterpriseDescription.AccessibleName = resources.GetString("enterpriseDescription.AccessibleName");
   this.enterpriseDescription.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("enterpriseDescription.Anchor")));
   this.enterpriseDescription.AutoSize = ((bool)(resources.GetObject("enterpriseDescription.AutoSize")));
   this.enterpriseDescription.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("enterpriseDescription.BackgroundImage")));
   this.enterpriseDescription.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("enterpriseDescription.Dock")));
   this.enterpriseDescription.Enabled = ((bool)(resources.GetObject("enterpriseDescription.Enabled")));
   this.enterpriseDescription.Font = ((System.Drawing.Font)(resources.GetObject("enterpriseDescription.Font")));
   this.enterpriseDescription.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("enterpriseDescription.ImeMode")));
   this.enterpriseDescription.Location = ((System.Drawing.Point)(resources.GetObject("enterpriseDescription.Location")));
   this.enterpriseDescription.MaxLength = ((int)(resources.GetObject("enterpriseDescription.MaxLength")));
   this.enterpriseDescription.Multiline = ((bool)(resources.GetObject("enterpriseDescription.Multiline")));
   this.enterpriseDescription.Name = "enterpriseDescription";
   this.enterpriseDescription.PasswordChar = ((char)(resources.GetObject("enterpriseDescription.PasswordChar")));
   this.enterpriseDescription.ReadOnly = true;
   this.enterpriseDescription.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("enterpriseDescription.RightToLeft")));
   this.enterpriseDescription.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("enterpriseDescription.ScrollBars")));
   this.enterpriseDescription.Size = ((System.Drawing.Size)(resources.GetObject("enterpriseDescription.Size")));
   this.enterpriseDescription.TabIndex = ((int)(resources.GetObject("enterpriseDescription.TabIndex")));
   this.enterpriseDescription.Text = resources.GetString("enterpriseDescription.Text");
   this.enterpriseDescription.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("enterpriseDescription.TextAlign")));
   this.enterpriseDescription.Visible = ((bool)(resources.GetObject("enterpriseDescription.Visible")));
   this.enterpriseDescription.WordWrap = ((bool)(resources.GetObject("enterpriseDescription.WordWrap")));



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



   this.server.AccessibleDescription = resources.GetString("server.AccessibleDescription");
   this.server.AccessibleName = resources.GetString("server.AccessibleName");
   this.server.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("server.Anchor")));
   this.server.AutoSize = ((bool)(resources.GetObject("server.AutoSize")));
   this.server.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("server.Dock")));
   this.server.Enabled = ((bool)(resources.GetObject("server.Enabled")));
   this.server.Font = ((System.Drawing.Font)(resources.GetObject("server.Font")));
   this.server.Image = ((System.Drawing.Image)(resources.GetObject("server.Image")));
   this.server.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("server.ImageAlign")));
   this.server.ImageIndex = ((int)(resources.GetObject("server.ImageIndex")));
   this.server.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("server.ImeMode")));
   this.server.Location = ((System.Drawing.Point)(resources.GetObject("server.Location")));
   this.server.Name = "server";
   this.server.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("server.RightToLeft")));
   this.server.Size = ((System.Drawing.Size)(resources.GetObject("server.Size")));
   this.server.TabIndex = ((int)(resources.GetObject("server.TabIndex")));
   this.server.Text = resources.GetString("server.Text");
   this.server.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("server.TextAlign")));
   this.server.Visible = ((bool)(resources.GetObject("server.Visible")));



   this.tabControl1.AccessibleDescription = resources.GetString("tabControl1.AccessibleDescription");
   this.tabControl1.AccessibleName = resources.GetString("tabControl1.AccessibleName");
   this.tabControl1.Alignment = ((System.Windows.Forms.TabAlignment)(resources.GetObject("tabControl1.Alignment")));
   this.tabControl1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("tabControl1.Anchor")));
   this.tabControl1.Appearance = ((System.Windows.Forms.TabAppearance)(resources.GetObject("tabControl1.Appearance")));
   this.tabControl1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("tabControl1.BackgroundImage")));
   this.tabControl1.Controls.Add(this.tabPage1);
   this.tabControl1.Controls.Add(this.tabPage2);
   this.tabControl1.Controls.Add(this.tabPage3);
   this.tabControl1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("tabControl1.Dock")));
   this.tabControl1.Enabled = ((bool)(resources.GetObject("tabControl1.Enabled")));
   this.tabControl1.Font = ((System.Drawing.Font)(resources.GetObject("tabControl1.Font")));
   this.tabControl1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("tabControl1.ImeMode")));
   this.tabControl1.ItemSize = ((System.Drawing.Size)(resources.GetObject("tabControl1.ItemSize")));
   this.tabControl1.Location = ((System.Drawing.Point)(resources.GetObject("tabControl1.Location")));
   this.tabControl1.Name = "tabControl1";
   this.tabControl1.Padding = ((System.Drawing.Point)(resources.GetObject("tabControl1.Padding")));
   this.tabControl1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("tabControl1.RightToLeft")));
   this.tabControl1.SelectedIndex = 0;
   this.tabControl1.ShowToolTips = ((bool)(resources.GetObject("tabControl1.ShowToolTips")));
   this.tabControl1.Size = ((System.Drawing.Size)(resources.GetObject("tabControl1.Size")));
   this.tabControl1.TabIndex = ((int)(resources.GetObject("tabControl1.TabIndex")));
   this.tabControl1.Text = resources.GetString("tabControl1.Text");
   this.tabControl1.Visible = ((bool)(resources.GetObject("tabControl1.Visible")));



   this.tabPage1.AccessibleDescription = resources.GetString("tabPage1.AccessibleDescription");
   this.tabPage1.AccessibleName = resources.GetString("tabPage1.AccessibleName");
   this.tabPage1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("tabPage1.Anchor")));
   this.tabPage1.AutoScroll = ((bool)(resources.GetObject("tabPage1.AutoScroll")));
   this.tabPage1.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("tabPage1.AutoScrollMargin")));
   this.tabPage1.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("tabPage1.AutoScrollMinSize")));
   this.tabPage1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("tabPage1.BackgroundImage")));
   this.tabPage1.Controls.Add(this.label2);
   this.tabPage1.Controls.Add(this.address);
   this.tabPage1.Controls.Add(this.serverAddressLabel);
   this.tabPage1.Controls.Add(this.serverNameLabel);
   this.tabPage1.Controls.Add(this.server);
   this.tabPage1.Controls.Add(this.enterpriseDescription);
   this.tabPage1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("tabPage1.Dock")));
   this.tabPage1.Enabled = ((bool)(resources.GetObject("tabPage1.Enabled")));
   this.tabPage1.Font = ((System.Drawing.Font)(resources.GetObject("tabPage1.Font")));
   this.tabPage1.ImageIndex = ((int)(resources.GetObject("tabPage1.ImageIndex")));
   this.tabPage1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("tabPage1.ImeMode")));
   this.tabPage1.Location = ((System.Drawing.Point)(resources.GetObject("tabPage1.Location")));
   this.tabPage1.Name = "tabPage1";
   this.tabPage1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("tabPage1.RightToLeft")));
   this.tabPage1.Size = ((System.Drawing.Size)(resources.GetObject("tabPage1.Size")));
   this.tabPage1.TabIndex = ((int)(resources.GetObject("tabPage1.TabIndex")));
   this.tabPage1.Text = resources.GetString("tabPage1.Text");
   this.tabPage1.ToolTipText = resources.GetString("tabPage1.ToolTipText");
   this.tabPage1.Visible = ((bool)(resources.GetObject("tabPage1.Visible")));



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



   this.address.AccessibleDescription = resources.GetString("address.AccessibleDescription");
   this.address.AccessibleName = resources.GetString("address.AccessibleName");
   this.address.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("address.Anchor")));
   this.address.AutoSize = ((bool)(resources.GetObject("address.AutoSize")));
   this.address.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("address.BackgroundImage")));
   this.address.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("address.Dock")));
   this.address.Enabled = ((bool)(resources.GetObject("address.Enabled")));
   this.address.Font = ((System.Drawing.Font)(resources.GetObject("address.Font")));
   this.address.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("address.ImeMode")));
   this.address.Location = ((System.Drawing.Point)(resources.GetObject("address.Location")));
   this.address.MaxLength = ((int)(resources.GetObject("address.MaxLength")));
   this.address.Multiline = ((bool)(resources.GetObject("address.Multiline")));
   this.address.Name = "address";
   this.address.PasswordChar = ((char)(resources.GetObject("address.PasswordChar")));
   this.address.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("address.RightToLeft")));
   this.address.ScrollBars = ((System.Windows.Forms.ScrollBars)(resources.GetObject("address.ScrollBars")));
   this.address.Size = ((System.Drawing.Size)(resources.GetObject("address.Size")));
   this.address.TabIndex = ((int)(resources.GetObject("address.TabIndex")));
   this.address.Text = resources.GetString("address.Text");
   this.address.TextAlign = ((System.Windows.Forms.HorizontalAlignment)(resources.GetObject("address.TextAlign")));
   this.address.Visible = ((bool)(resources.GetObject("address.Visible")));
   this.address.WordWrap = ((bool)(resources.GetObject("address.WordWrap")));



   this.serverAddressLabel.AccessibleDescription = resources.GetString("serverAddressLabel.AccessibleDescription");
   this.serverAddressLabel.AccessibleName = resources.GetString("serverAddressLabel.AccessibleName");
   this.serverAddressLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("serverAddressLabel.Anchor")));
   this.serverAddressLabel.AutoSize = ((bool)(resources.GetObject("serverAddressLabel.AutoSize")));
   this.serverAddressLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("serverAddressLabel.Dock")));
   this.serverAddressLabel.Enabled = ((bool)(resources.GetObject("serverAddressLabel.Enabled")));
   this.serverAddressLabel.Font = ((System.Drawing.Font)(resources.GetObject("serverAddressLabel.Font")));
   this.serverAddressLabel.Image = ((System.Drawing.Image)(resources.GetObject("serverAddressLabel.Image")));
   this.serverAddressLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverAddressLabel.ImageAlign")));
   this.serverAddressLabel.ImageIndex = ((int)(resources.GetObject("serverAddressLabel.ImageIndex")));
   this.serverAddressLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("serverAddressLabel.ImeMode")));
   this.serverAddressLabel.Location = ((System.Drawing.Point)(resources.GetObject("serverAddressLabel.Location")));
   this.serverAddressLabel.Name = "serverAddressLabel";
   this.serverAddressLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("serverAddressLabel.RightToLeft")));
   this.serverAddressLabel.Size = ((System.Drawing.Size)(resources.GetObject("serverAddressLabel.Size")));
   this.serverAddressLabel.TabIndex = ((int)(resources.GetObject("serverAddressLabel.TabIndex")));
   this.serverAddressLabel.Text = resources.GetString("serverAddressLabel.Text");
   this.serverAddressLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverAddressLabel.TextAlign")));
   this.serverAddressLabel.Visible = ((bool)(resources.GetObject("serverAddressLabel.Visible")));



   this.serverNameLabel.AccessibleDescription = resources.GetString("serverNameLabel.AccessibleDescription");
   this.serverNameLabel.AccessibleName = resources.GetString("serverNameLabel.AccessibleName");
   this.serverNameLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("serverNameLabel.Anchor")));
   this.serverNameLabel.AutoSize = ((bool)(resources.GetObject("serverNameLabel.AutoSize")));
   this.serverNameLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("serverNameLabel.Dock")));
   this.serverNameLabel.Enabled = ((bool)(resources.GetObject("serverNameLabel.Enabled")));
   this.serverNameLabel.Font = ((System.Drawing.Font)(resources.GetObject("serverNameLabel.Font")));
   this.serverNameLabel.Image = ((System.Drawing.Image)(resources.GetObject("serverNameLabel.Image")));
   this.serverNameLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverNameLabel.ImageAlign")));
   this.serverNameLabel.ImageIndex = ((int)(resources.GetObject("serverNameLabel.ImageIndex")));
   this.serverNameLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("serverNameLabel.ImeMode")));
   this.serverNameLabel.Location = ((System.Drawing.Point)(resources.GetObject("serverNameLabel.Location")));
   this.serverNameLabel.Name = "serverNameLabel";
   this.serverNameLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("serverNameLabel.RightToLeft")));
   this.serverNameLabel.Size = ((System.Drawing.Size)(resources.GetObject("serverNameLabel.Size")));
   this.serverNameLabel.TabIndex = ((int)(resources.GetObject("serverNameLabel.TabIndex")));
   this.serverNameLabel.Text = resources.GetString("serverNameLabel.Text");
   this.serverNameLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("serverNameLabel.TextAlign")));
   this.serverNameLabel.Visible = ((bool)(resources.GetObject("serverNameLabel.Visible")));



   this.tabPage2.AccessibleDescription = resources.GetString("tabPage2.AccessibleDescription");
   this.tabPage2.AccessibleName = resources.GetString("tabPage2.AccessibleName");
   this.tabPage2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("tabPage2.Anchor")));
   this.tabPage2.AutoScroll = ((bool)(resources.GetObject("tabPage2.AutoScroll")));
   this.tabPage2.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("tabPage2.AutoScrollMargin")));
   this.tabPage2.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("tabPage2.AutoScrollMinSize")));
   this.tabPage2.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("tabPage2.BackgroundImage")));
   this.tabPage2.Controls.Add(this.rememberPassword);
   this.tabPage2.Controls.Add(this.password);
   this.tabPage2.Controls.Add(this.username);
   this.tabPage2.Controls.Add(this.label1);
   this.tabPage2.Controls.Add(this.usernameLabel);
   this.tabPage2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("tabPage2.Dock")));
   this.tabPage2.Enabled = ((bool)(resources.GetObject("tabPage2.Enabled")));
   this.tabPage2.Font = ((System.Drawing.Font)(resources.GetObject("tabPage2.Font")));
   this.tabPage2.ImageIndex = ((int)(resources.GetObject("tabPage2.ImageIndex")));
   this.tabPage2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("tabPage2.ImeMode")));
   this.tabPage2.Location = ((System.Drawing.Point)(resources.GetObject("tabPage2.Location")));
   this.tabPage2.Name = "tabPage2";
   this.tabPage2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("tabPage2.RightToLeft")));
   this.tabPage2.Size = ((System.Drawing.Size)(resources.GetObject("tabPage2.Size")));
   this.tabPage2.TabIndex = ((int)(resources.GetObject("tabPage2.TabIndex")));
   this.tabPage2.Text = resources.GetString("tabPage2.Text");
   this.tabPage2.ToolTipText = resources.GetString("tabPage2.ToolTipText");
   this.tabPage2.Visible = ((bool)(resources.GetObject("tabPage2.Visible")));



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
   this.rememberPassword.CheckedChanged += new System.EventHandler(this.rememberPassword_CheckedChanged);



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
   this.password.TextChanged += new System.EventHandler(this.password_TextChanged);



   this.username.AccessibleDescription = resources.GetString("username.AccessibleDescription");
   this.username.AccessibleName = resources.GetString("username.AccessibleName");
   this.username.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("username.Anchor")));
   this.username.AutoSize = ((bool)(resources.GetObject("username.AutoSize")));
   this.username.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("username.Dock")));
   this.username.Enabled = ((bool)(resources.GetObject("username.Enabled")));
   this.username.Font = ((System.Drawing.Font)(resources.GetObject("username.Font")));
   this.username.Image = ((System.Drawing.Image)(resources.GetObject("username.Image")));
   this.username.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("username.ImageAlign")));
   this.username.ImageIndex = ((int)(resources.GetObject("username.ImageIndex")));
   this.username.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("username.ImeMode")));
   this.username.Location = ((System.Drawing.Point)(resources.GetObject("username.Location")));
   this.username.Name = "username";
   this.username.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("username.RightToLeft")));
   this.username.Size = ((System.Drawing.Size)(resources.GetObject("username.Size")));
   this.username.TabIndex = ((int)(resources.GetObject("username.TabIndex")));
   this.username.Text = resources.GetString("username.Text");
   this.username.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("username.TextAlign")));
   this.username.Visible = ((bool)(resources.GetObject("username.Visible")));



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



   this.usernameLabel.AccessibleDescription = resources.GetString("usernameLabel.AccessibleDescription");
   this.usernameLabel.AccessibleName = resources.GetString("usernameLabel.AccessibleName");
   this.usernameLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("usernameLabel.Anchor")));
   this.usernameLabel.AutoSize = ((bool)(resources.GetObject("usernameLabel.AutoSize")));
   this.usernameLabel.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("usernameLabel.Dock")));
   this.usernameLabel.Enabled = ((bool)(resources.GetObject("usernameLabel.Enabled")));
   this.usernameLabel.Font = ((System.Drawing.Font)(resources.GetObject("usernameLabel.Font")));
   this.usernameLabel.Image = ((System.Drawing.Image)(resources.GetObject("usernameLabel.Image")));
   this.usernameLabel.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("usernameLabel.ImageAlign")));
   this.usernameLabel.ImageIndex = ((int)(resources.GetObject("usernameLabel.ImageIndex")));
   this.usernameLabel.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("usernameLabel.ImeMode")));
   this.usernameLabel.Location = ((System.Drawing.Point)(resources.GetObject("usernameLabel.Location")));
   this.usernameLabel.Name = "usernameLabel";
   this.usernameLabel.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("usernameLabel.RightToLeft")));
   this.usernameLabel.Size = ((System.Drawing.Size)(resources.GetObject("usernameLabel.Size")));
   this.usernameLabel.TabIndex = ((int)(resources.GetObject("usernameLabel.TabIndex")));
   this.usernameLabel.Text = resources.GetString("usernameLabel.Text");
   this.usernameLabel.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("usernameLabel.TextAlign")));
   this.usernameLabel.Visible = ((bool)(resources.GetObject("usernameLabel.Visible")));



   this.tabPage3.AccessibleDescription = resources.GetString("tabPage3.AccessibleDescription");
   this.tabPage3.AccessibleName = resources.GetString("tabPage3.AccessibleName");
   this.tabPage3.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("tabPage3.Anchor")));
   this.tabPage3.AutoScroll = ((bool)(resources.GetObject("tabPage3.AutoScroll")));
   this.tabPage3.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("tabPage3.AutoScrollMargin")));
   this.tabPage3.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("tabPage3.AutoScrollMinSize")));
   this.tabPage3.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("tabPage3.BackgroundImage")));
   this.tabPage3.Controls.Add(this.groupBox1);
   this.tabPage3.Controls.Add(this.totalSpaceUnits);
   this.tabPage3.Controls.Add(this.usedSpaceUnits);
   this.tabPage3.Controls.Add(this.freeSpaceUnits);
   this.tabPage3.Controls.Add(this.totalSpace);
   this.tabPage3.Controls.Add(this.usedSpace);
   this.tabPage3.Controls.Add(this.freeSpace);
   this.tabPage3.Controls.Add(this.label13);
   this.tabPage3.Controls.Add(this.label12);
   this.tabPage3.Controls.Add(this.label11);
   this.tabPage3.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("tabPage3.Dock")));
   this.tabPage3.Enabled = ((bool)(resources.GetObject("tabPage3.Enabled")));
   this.tabPage3.Font = ((System.Drawing.Font)(resources.GetObject("tabPage3.Font")));
   this.tabPage3.ImageIndex = ((int)(resources.GetObject("tabPage3.ImageIndex")));
   this.tabPage3.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("tabPage3.ImeMode")));
   this.tabPage3.Location = ((System.Drawing.Point)(resources.GetObject("tabPage3.Location")));
   this.tabPage3.Name = "tabPage3";
   this.tabPage3.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("tabPage3.RightToLeft")));
   this.tabPage3.Size = ((System.Drawing.Size)(resources.GetObject("tabPage3.Size")));
   this.tabPage3.TabIndex = ((int)(resources.GetObject("tabPage3.TabIndex")));
   this.tabPage3.Text = resources.GetString("tabPage3.Text");
   this.tabPage3.ToolTipText = resources.GetString("tabPage3.ToolTipText");
   this.tabPage3.Visible = ((bool)(resources.GetObject("tabPage3.Visible")));



   this.groupBox1.AccessibleDescription = resources.GetString("groupBox1.AccessibleDescription");
   this.groupBox1.AccessibleName = resources.GetString("groupBox1.AccessibleName");
   this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("groupBox1.Anchor")));
   this.groupBox1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("groupBox1.BackgroundImage")));
   this.groupBox1.Controls.Add(this.label18);
   this.groupBox1.Controls.Add(this.gaugeChart1);
   this.groupBox1.Controls.Add(this.label17);
   this.groupBox1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("groupBox1.Dock")));
   this.groupBox1.Enabled = ((bool)(resources.GetObject("groupBox1.Enabled")));
   this.groupBox1.FlatStyle = System.Windows.Forms.FlatStyle.System;
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



   this.accountEnabled.AccessibleDescription = resources.GetString("accountEnabled.AccessibleDescription");
   this.accountEnabled.AccessibleName = resources.GetString("accountEnabled.AccessibleName");
   this.accountEnabled.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("accountEnabled.Anchor")));
   this.accountEnabled.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("accountEnabled.Appearance")));
   this.accountEnabled.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("accountEnabled.BackgroundImage")));
   this.accountEnabled.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("accountEnabled.CheckAlign")));
   this.accountEnabled.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("accountEnabled.Dock")));
   this.accountEnabled.Enabled = ((bool)(resources.GetObject("accountEnabled.Enabled")));
   this.accountEnabled.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("accountEnabled.FlatStyle")));
   this.accountEnabled.Font = ((System.Drawing.Font)(resources.GetObject("accountEnabled.Font")));
   this.accountEnabled.Image = ((System.Drawing.Image)(resources.GetObject("accountEnabled.Image")));
   this.accountEnabled.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("accountEnabled.ImageAlign")));
   this.accountEnabled.ImageIndex = ((int)(resources.GetObject("accountEnabled.ImageIndex")));
   this.accountEnabled.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("accountEnabled.ImeMode")));
   this.accountEnabled.Location = ((System.Drawing.Point)(resources.GetObject("accountEnabled.Location")));
   this.accountEnabled.Name = "accountEnabled";
   this.accountEnabled.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("accountEnabled.RightToLeft")));
   this.accountEnabled.Size = ((System.Drawing.Size)(resources.GetObject("accountEnabled.Size")));
   this.accountEnabled.TabIndex = ((int)(resources.GetObject("accountEnabled.TabIndex")));
   this.accountEnabled.Text = resources.GetString("accountEnabled.Text");
   this.accountEnabled.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("accountEnabled.TextAlign")));
   this.accountEnabled.Visible = ((bool)(resources.GetObject("accountEnabled.Visible")));



   this.defaultServer.AccessibleDescription = resources.GetString("defaultServer.AccessibleDescription");
   this.defaultServer.AccessibleName = resources.GetString("defaultServer.AccessibleName");
   this.defaultServer.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("defaultServer.Anchor")));
   this.defaultServer.Appearance = ((System.Windows.Forms.Appearance)(resources.GetObject("defaultServer.Appearance")));
   this.defaultServer.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("defaultServer.BackgroundImage")));
   this.defaultServer.CheckAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("defaultServer.CheckAlign")));
   this.defaultServer.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("defaultServer.Dock")));
   this.defaultServer.Enabled = ((bool)(resources.GetObject("defaultServer.Enabled")));
   this.defaultServer.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("defaultServer.FlatStyle")));
   this.defaultServer.Font = ((System.Drawing.Font)(resources.GetObject("defaultServer.Font")));
   this.defaultServer.Image = ((System.Drawing.Image)(resources.GetObject("defaultServer.Image")));
   this.defaultServer.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("defaultServer.ImageAlign")));
   this.defaultServer.ImageIndex = ((int)(resources.GetObject("defaultServer.ImageIndex")));
   this.defaultServer.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("defaultServer.ImeMode")));
   this.defaultServer.Location = ((System.Drawing.Point)(resources.GetObject("defaultServer.Location")));
   this.defaultServer.Name = "defaultServer";
   this.defaultServer.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("defaultServer.RightToLeft")));
   this.defaultServer.Size = ((System.Drawing.Size)(resources.GetObject("defaultServer.Size")));
   this.defaultServer.TabIndex = ((int)(resources.GetObject("defaultServer.TabIndex")));
   this.defaultServer.Text = resources.GetString("defaultServer.Text");
   this.defaultServer.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("defaultServer.TextAlign")));
   this.defaultServer.Visible = ((bool)(resources.GetObject("defaultServer.Visible")));



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
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.defaultServer);
   this.Controls.Add(this.accountEnabled);
   this.Controls.Add(this.tabControl1);
   this.Controls.Add(this.ok);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimizeBox = false;
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "ServerDetails";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.ShowInTaskbar = false;
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.Load += new System.EventHandler(this.ServerDetails_Load);
   this.tabControl1.ResumeLayout(false);
   this.tabPage1.ResumeLayout(false);
   this.tabPage2.ResumeLayout(false);
   this.tabPage3.ResumeLayout(false);
   this.groupBox1.ResumeLayout(false);
   this.ResumeLayout(false);

  }






        private void ok_Click(object sender, System.EventArgs e)
  {

   if (updatePassword)
   {
    try
    {
     if (rememberPassword.Checked)
     {
      simiasWebService.SetDomainCredentials(domain.ID, password.Text, CredentialType.Basic);
     }
     else
     {
      simiasWebService.SetDomainCredentials(domain.ID, null, CredentialType.None);
     }
    }
    catch (Exception ex)
    {
     Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("savePasswordError"), resourceManager.GetString("passwordErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                    mmb.ShowDialog();

    }
   }


   if ( accountEnabled.Checked != domain.DomainInfo.Active )
   {
    enableChanged = true;

    try
    {
     if (accountEnabled.Checked)
     {
      simiasWebService.SetDomainActive(domain.ID);
      domain.DomainInfo.Active = true;
     }
     else
     {
      simiasWebService.SetDomainInactive(domain.ID);
      domain.DomainInfo.Active = false;
     }
    }
    catch
    {


    }
   }


   if ( defaultServer.Enabled && defaultServer.Checked )
   {
    defaultChanged = true;

    try
    {
     simiasWebService.SetDefaultDomain(domain.DomainInfo.ID);

     domain.DomainInfo.IsDefault = true;
    }
    catch (Exception ex)
    {


    }
   }


            UriBuilder NewAddressUri = new UriBuilder(address.Text );
            UriBuilder OldAddressUri = new UriBuilder(domain.DomainInfo.HostUrl);
            bool schemeDiffers = (NewAddressUri.Scheme != OldAddressUri.Scheme);
            if ( schemeDiffers || !(NewAddressUri.Host == OldAddressUri.Host) )
   {
    addressChanged = true;

    try
                {
                    if (!simiasWebService.SetDomainHostAddress(domain.ID, NewAddressUri.Uri.ToString(), domain.DomainInfo.MemberName, password.Text ))
     {
                        Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("updateHostErrorDetail"), resourceManager.GetString("updateHostError"), string.Empty, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
      mmb.ShowDialog();
     }

    }
    catch (Exception ex)
    {
     Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("updateHostErrorDetail"), resourceManager.GetString("updateHostError"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
           mmb.ShowDialog();
    }
   }
  }




        private void password_TextChanged(object sender, System.EventArgs e)
  {
   if ( password.Focused )
   {
    updatePassword = true;
   }
  }




        private void rememberPassword_CheckedChanged(object sender, System.EventArgs e)
  {
   if ( rememberPassword.Focused )
   {
    updatePassword = true;
   }
  }




        private void ServerDetails_Load(object sender, System.EventArgs e)
  {

   try
   {
    this.Icon = new Icon(Path.Combine(Application.StartupPath, @"res\ifolder_16.ico"));
   }
   catch {}

   Text = string.Format( Text, domain.Name );
   server.Text = domain.Name;

            UriBuilder serverUri = new UriBuilder(domain.DomainInfo.HostUrl);
            UriBuilder serverUrl = new UriBuilder(serverUri.Scheme, serverUri.Host, serverUri.Port);
            address.Text = serverUrl.ToString() ;

   address.Enabled = !domain.DomainInfo.Authenticated;
   enterpriseDescription.Text = domain.DomainInfo.Description;
   defaultServer.Checked = domain.DomainInfo.IsDefault;

   defaultServer.Enabled = !domain.DomainInfo.IsDefault;
   accountEnabled.Checked = domain.DomainInfo.Active;

   username.Text = domain.DomainInfo.MemberName;

   try
   {
    string userID;
    string credentials;
    CredentialType credType = simiasWebService.GetDomainCredentials(domain.ID, out userID, out credentials);
    if ((credType == CredentialType.Basic) && (credentials != null))
    {

     rememberPassword.Checked = true;
     password.Text = credentials;
    }
   }
   catch (Exception ex)
   {

    MessageBox.Show(ex.Message);
   }

   try
   {

    DiskSpace diskSpace = ifWebService.GetUserDiskSpace(domain.DomainInfo.MemberUserID);
    double used = 0;
    if (diskSpace.UsedSpace != 0)
    {
     usedSpaceUnits.Text = resourceManager.GetString("freeSpaceUnits.Text");
     used = Math.Round(diskSpace.UsedSpace/megaByte, 2);
     usedSpace.Text = used.ToString();
    }
    else
    {
     usedSpaceUnits.Text = resourceManager.GetString("notApplicable");
     usedSpace.Text = "";
    }

    if (diskSpace.Limit != -1)
    {
     usedSpaceUnits.Text = freeSpaceUnits.Text = totalSpaceUnits.Text =
      resourceManager.GetString("freeSpaceUnits.Text");
     totalSpace.Text = ((double)Math.Round(diskSpace.Limit/megaByte, 2)).ToString();

     freeSpace.Text = ((double)Math.Round(diskSpace.AvailableSpace/megaByte, 2)).ToString();


     gaugeChart1.MaxValue = diskSpace.Limit / megaByte;
     gaugeChart1.Used = used;
     gaugeChart1.BarColor = SystemColors.ActiveCaption;
    }
    else
    {
     freeSpaceUnits.Text = totalSpaceUnits.Text =
      resourceManager.GetString("notApplicable");
     freeSpace.Text = totalSpace.Text = "";
     gaugeChart1.Used = 0;
    }
   }
   catch
   {
    usedSpace.Text = freeSpace.Text = totalSpace.Text = resourceManager.GetString("statusUnknown");
   }


   gaugeChart1.Invalidate(true);
  }
  public bool AddressChanged
  {
   get { return addressChanged; }
  }
  public bool DefaultChanged
  {
   get { return defaultChanged; }
  }
  public Domain Domain
  {
   get { return domain; }
  }
  public bool EnableChanged
  {
   get { return enableChanged; }
  }
 }
}
