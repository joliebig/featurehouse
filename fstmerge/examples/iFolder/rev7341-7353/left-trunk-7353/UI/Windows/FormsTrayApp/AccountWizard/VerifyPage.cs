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
namespace Novell.Wizard
{
 public class VerifyPage : Novell.Wizard.InteriorPageTemplate
 {
  private System.Windows.Forms.Label label1;
  private System.ComponentModel.IContainer components = null;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.Label label3;
  private System.Windows.Forms.Label label4;
  private System.Windows.Forms.Label serverAddress;
  private System.Windows.Forms.Label username;
  private System.Windows.Forms.Label rememberPassword;
  private System.Windows.Forms.Label defaultAccount;
  private System.Windows.Forms.Label label6;
  private System.Windows.Forms.Label defaultDescription;
  private AccountWizard wizard;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));
  private int defaultTextWidth = 135;
  private int defaultTextXPos = 50;
  private int defaultTextYPos = 70;
  private int defaultValuePos = 238;
  private int maxTextWidth = 440;
  private int defaultSpacing = 16;
        private SizeF strSize;
  public VerifyPage( bool hideDefaultAccount )
  {
   InitializeComponent();
   defaultDescription.Visible = defaultAccount.Visible = !hideDefaultAccount;
  }
  private void InitializeComponent()
  {
   this.label1 = new System.Windows.Forms.Label();
   this.label2 = new System.Windows.Forms.Label();
   this.label3 = new System.Windows.Forms.Label();
   this.label4 = new System.Windows.Forms.Label();
   this.defaultDescription = new System.Windows.Forms.Label();
   this.serverAddress = new System.Windows.Forms.Label();
   this.username = new System.Windows.Forms.Label();
   this.rememberPassword = new System.Windows.Forms.Label();
   this.defaultAccount = new System.Windows.Forms.Label();
   this.label6 = new System.Windows.Forms.Label();
            this.strSize = new SizeF();
   this.SuspendLayout();
   Graphics graphics = CreateGraphics();
   this.label2.Location = new System.Drawing.Point( this.defaultTextXPos - 10, this.defaultTextYPos );
   this.label2.Name = "label2";
   this.label2.TabIndex = 0;
   this.label2.Text = Resource.GetString( "AccountVerify" );
            this.strSize = graphics.MeasureString(this.label2.Text, this.label2.Font);
   this.label2.Size = new System.Drawing.Size( this.maxTextWidth , ((int)this.strSize.Width /this.maxTextWidth+1)*16 );
   this.label3.Location = new System.Drawing.Point( this.defaultTextXPos, this.label2.Location.Y+this.label2.Size.Height+this.defaultSpacing );
   this.label3.Name = "label3";
   this.label3.TabIndex = 1;
   this.label3.Text = Resource.GetString("ServerNameText")+":";
            this.strSize = graphics.MeasureString(this.label3.Text, this.label3.Font);
            this.label3.Size = new System.Drawing.Size(this.defaultTextWidth, ((int)this.strSize.Width / this.defaultTextWidth + 1) * 16);
   this.label1.Location = new System.Drawing.Point( this.defaultTextXPos , this.label3.Location.Y+this.label3.Size.Height+this.defaultSpacing );
   this.label1.Name = "label1";
   this.label1.TabIndex = 3;
   this.label1.Text = Resource.GetString("UName")+":";
            this.strSize = graphics.MeasureString(this.label1.Text, this.label1.Font);
   this.label1.Size = new System.Drawing.Size( this.defaultTextWidth, ((int)this.strSize.Width/this.defaultTextWidth+1)*16 );
            this.label4.Location = new System.Drawing.Point( this.defaultTextXPos , this.label1.Location.Y+this.label1.Size.Height+this.defaultSpacing );
   this.label4.Name = "label4";
   this.label4.TabIndex = 5;
   this.label4.Text = Resource.GetString("RememberPasswd")+":";
            strSize = graphics.MeasureString(this.label4.Text, this.label4.Font);
            this.label4.Size = new System.Drawing.Size(this.defaultTextWidth, ((int)this.strSize.Width / this.defaultTextWidth + 1) * 16);
   this.defaultDescription.Location = new System.Drawing.Point( this.defaultTextXPos , this.label4.Location.Y+this.label4.Size.Height+this.defaultSpacing );
   this.defaultDescription.Name = "defaultDescription";
   this.defaultDescription.TabIndex = 7;
   this.defaultDescription.Text = Resource.GetString("DefaultServer");
            this.strSize = graphics.MeasureString(this.defaultDescription.Text, this.defaultDescription.Font);
   this.defaultDescription.Size = new System.Drawing.Size( this.defaultTextWidth, ((int)this.strSize.Width/this.defaultTextWidth+1)*16 );
   this.serverAddress.Location = new System.Drawing.Point( this.defaultValuePos, this.label2.Location.Y+this.label2.Size.Height+this.defaultSpacing );
   this.serverAddress.Name = "serverAddress";
   this.serverAddress.Size = new System.Drawing.Size( this.maxTextWidth, 16 );
   this.serverAddress.TabIndex = 2;
   this.username.Location = new System.Drawing.Point( this.defaultValuePos, this.label3.Location.Y+this.label3.Size.Height+this.defaultSpacing );
   this.username.Name = "username";
   this.username.Size = new System.Drawing.Size( this.maxTextWidth, 16);
   this.username.TabIndex = 4;
   this.rememberPassword.Location = new System.Drawing.Point( this.defaultValuePos, this.label1.Location.Y+this.label1.Size.Height+this.defaultSpacing );
   this.rememberPassword.Name = "rememberPassword";
   this.rememberPassword.Size = new System.Drawing.Size( this.maxTextWidth, 16);
   this.rememberPassword.TabIndex = 6;
   this.defaultAccount.Location = new System.Drawing.Point( this.defaultValuePos, this.label4.Location.Y+this.label4.Size.Height+this.defaultSpacing );
   this.defaultAccount.Name = "defaultAccount";
   this.defaultAccount.Size = new System.Drawing.Size( this.maxTextWidth, 16);
   this.defaultAccount.TabIndex = 8;
   this.label6.Location = new System.Drawing.Point( this.defaultTextXPos - 10, this.defaultDescription.Location.Y+this.defaultDescription.Size.Height+this.defaultSpacing );
   this.label6.Name = "label6";
   this.label6.TabIndex = 9;
   this.label6.Text = Resource.GetString("ClickConnect");
            this.strSize = graphics.MeasureString(this.label6.Text, this.label6.Font);
   this.label6.Size = new System.Drawing.Size( this.maxTextWidth, ((int)this.strSize.Width/this.maxTextWidth+1)*16 );
            graphics.Dispose();
   this.Controls.Add(this.label2);
   this.Controls.Add(this.label3);
   this.Controls.Add(this.label1);
   this.Controls.Add(this.label4);
   this.Controls.Add(this.defaultDescription);
   this.Controls.Add(this.serverAddress);
   this.Controls.Add(this.username);
   this.Controls.Add(this.rememberPassword);
   this.Controls.Add(this.defaultAccount);
   this.Controls.Add(this.label6);
   this.HeaderTitle = "";
   this.Name = "VerifyPage";
   this.Controls.SetChildIndex(this.label1, 0);
   this.Controls.SetChildIndex(this.label2, 0);
   this.Controls.SetChildIndex(this.label3, 0);
   this.Controls.SetChildIndex(this.label4, 0);
   this.Controls.SetChildIndex(this.defaultDescription, 0);
   this.Controls.SetChildIndex(this.serverAddress, 0);
   this.Controls.SetChildIndex(this.username, 0);
   this.Controls.SetChildIndex(this.rememberPassword, 0);
   this.Controls.SetChildIndex(this.defaultAccount, 0);
   this.Controls.SetChildIndex(this.label6, 0);
   this.ResumeLayout(false);
            this.PerformLayout();
  }
  internal override void ActivatePage(int previousIndex)
  {
   base.ActivatePage (previousIndex);
   wizard = (AccountWizard)this.Parent;
   serverAddress.Text = wizard.ServerPage.ServerAddress;
   username.Text = wizard.IdentityPage.Username;
   rememberPassword.Text = wizard.IdentityPage.RememberPassword ? Resource.GetString("YesText") : Resource.GetString("NoText");
   defaultAccount.Text = wizard.ServerPage.DefaultServer ? Resource.GetString("YesText") : Resource.GetString("NoText");
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
   if ( !wizard.ConnectToServer() )
   {
    return currentIndex;
   }
   return base.ValidatePage (currentIndex);
  }
 }
}
