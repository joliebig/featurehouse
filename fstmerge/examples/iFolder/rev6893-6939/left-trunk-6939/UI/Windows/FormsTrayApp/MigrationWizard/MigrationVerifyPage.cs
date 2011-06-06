using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.IO;
using Novell.iFolderCom;
using Novell.FormsTrayApp;
using Simias.Client;
using Simias.Client.Authentication;
using Simias.Client.Event;
using Novell.FormsTrayApp;
using System.Threading;
namespace Novell.Wizard
{
 public class MigrationVerifyPage : Novell.Wizard.MigrationInteriorPageTemplate
 {
  private System.Windows.Forms.Label label1;
  private System.ComponentModel.IContainer components = null;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.Label label3;
  private System.Windows.Forms.Label label4;
  private System.Windows.Forms.Label serverAddress;
  private System.Windows.Forms.Label location;
  private System.Windows.Forms.Label migrationOption;
  private System.Windows.Forms.Label security;
  private System.Windows.Forms.Label label6;
  private System.Windows.Forms.Label defaultDescription;
  private MigrationWizard wizard;
  private Connecting waitWindow;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));
  public MigrationVerifyPage( )
  {
   InitializeComponent();
   waitWindow = null;
  }
  private void InitializeComponent()
  {
   this.label1 = new System.Windows.Forms.Label();
   this.label2 = new System.Windows.Forms.Label();
   this.label3 = new System.Windows.Forms.Label();
   this.label4 = new System.Windows.Forms.Label();
   this.defaultDescription = new System.Windows.Forms.Label();
   this.serverAddress = new System.Windows.Forms.Label();
   this.location = new System.Windows.Forms.Label();
   this.migrationOption = new System.Windows.Forms.Label();
   this.security = new System.Windows.Forms.Label();
   this.label6 = new System.Windows.Forms.Label();
   this.SuspendLayout();
   this.label1.Location = new System.Drawing.Point(50, 196);
   this.label1.Name = "label1";
   this.label1.Size = new System.Drawing.Size(94, 16);
   this.label1.TabIndex = 3;
   this.label1.Text = Resource.GetString("LocationText");
   this.label2.Location = new System.Drawing.Point(40, 100);
   this.label2.Name = "label2";
   this.label2.Size = new System.Drawing.Size(440, 16);
   this.label2.TabIndex = 0;
   this.label2.Text = Resource.GetString("AccountVerify");
   this.label3.Location = new System.Drawing.Point(50, 124);
   this.label3.Name = "label3";
   this.label3.Size = new System.Drawing.Size(88, 16);
   this.label3.TabIndex = 1;
   this.label3.Text = Resource.GetString("DomainName")+":";
   this.label4.Location = new System.Drawing.Point(50, 172);
   this.label4.Name = "label4";
   this.label4.Size = new System.Drawing.Size(118, 16);
   this.label4.TabIndex = 5;
   this.label4.Text = Resource.GetString("MigrationOptions")+":";
   this.defaultDescription.Location = new System.Drawing.Point(50, 148);
   this.defaultDescription.Name = "defaultDescription";
   this.defaultDescription.Size = new System.Drawing.Size(118, 16);
   this.defaultDescription.TabIndex = 7;
   this.defaultDescription.Text = Resource.GetString("Security") + ":";
   this.serverAddress.Location = new System.Drawing.Point(176, 124);
   this.serverAddress.Name = "serverAddress";
   this.serverAddress.Size = new System.Drawing.Size(304, 16);
   this.serverAddress.TabIndex = 2;
   this.location.Location = new System.Drawing.Point(176, 196);
   this.location.Name = "location";
   this.location.Size = new System.Drawing.Size(304, 48);
   this.location.TabIndex = 4;
   this.migrationOption.Location = new System.Drawing.Point(176, 172);
   this.migrationOption.Name = "migrationOption";
   this.migrationOption.Size = new System.Drawing.Size(304, 16);
   this.migrationOption.TabIndex = 6;
   this.security.Location = new System.Drawing.Point(176, 148);
   this.security.Name = "security";
   this.security.Size = new System.Drawing.Size(304, 16);
   this.security.TabIndex = 8;
   this.security.MouseHover += new EventHandler(security_MouseHover);
   this.label6.Location = new System.Drawing.Point(40, 256);
   this.label6.Name = "label6";
   this.label6.Size = new System.Drawing.Size(432, 16);
   this.label6.TabIndex = 9;
   this.label6.Text = Resource.GetString("ClickMigrate");
   this.Controls.Add(this.label6);
   this.Controls.Add(this.security);
   this.Controls.Add(this.migrationOption);
   this.Controls.Add(this.location);
   this.Controls.Add(this.serverAddress);
   this.Controls.Add(this.defaultDescription);
   this.Controls.Add(this.label4);
   this.Controls.Add(this.label3);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.label1);
   this.HeaderSubTitle = "HeaderSubTitle";
   this.HeaderTitle = "HeaderTitle";
   this.Name = "VerifyPage";
   this.Controls.SetChildIndex(this.label1, 0);
   this.Controls.SetChildIndex(this.label2, 0);
   this.Controls.SetChildIndex(this.label3, 0);
   this.Controls.SetChildIndex(this.label4, 0);
   this.Controls.SetChildIndex(this.defaultDescription, 0);
   this.Controls.SetChildIndex(this.serverAddress, 0);
   this.Controls.SetChildIndex(this.location, 0);
   this.Controls.SetChildIndex(this.migrationOption, 0);
   this.Controls.SetChildIndex(this.security, 0);
   this.Controls.SetChildIndex(this.label6, 0);
   this.Load += new EventHandler(MigrationVerifyPage_Load);
   this.ResumeLayout(false);
  }
  internal override void ActivatePage(int previousIndex)
  {
   base.ActivatePage (previousIndex);
  }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if (components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }
  internal override int ValidatePage(int currentIndex)
  {
   Thread thrd = new Thread(new ThreadStart(ShowWaitDialog));
            thrd.Name = "Migration Wz Wait Dialog";
   thrd.Start();
   wizard = (MigrationWizard)this.Parent;
   bool result = wizard.MigrateFolder();
   CloseWaitDialog();
   if ( !result )
   {
    return currentIndex;
   }
   return base.ValidatePage (currentIndex);
  }
  private void ShowWaitDialog()
  {
   waitWindow = new Connecting();
   waitWindow.ShowDialog();
  }
  public void CloseWaitDialog()
  {
   if( waitWindow!=null)
   {
    waitWindow.Close();
    waitWindow = null;
   }
  }
  public void UpdateDetails()
  {
   MigrationWizard wizard = (MigrationWizard)this.Parent;
   this.serverAddress.Text = wizard.MigrationIdentityPage.DomainName;
   this.location.Text = wizard.MigrationServerPage.HomeLocation;
   if(wizard.MigrationServerPage.MigrationOption)
    this.migrationOption.Text = Resource.GetString("MigrateNRemove");
   else
    this.migrationOption.Text = Resource.GetString("MigrateNCopy");
   this.security.Text = "";
   if( wizard.MigrationIdentityPage.Encrypion)
   {
    this.security.Text = Resource.GetString("EncryptedText");
    if( wizard.MigrationIdentityPage.SSL)
     this.security.Text += " and use secure channel for data transfer";
   }
   else if(wizard.MigrationIdentityPage.SSL)
   {
    this.security.Text = Resource.GetString("SharableText");
   }
   else
   {
    this.security.Text = "None";
   }
  }
        private void MigrationVerifyPage_Load(object sender, EventArgs e)
  {
   UpdateDetails();
  }
        private void security_MouseHover(object sender, EventArgs e)
  {
  }
 }
}
