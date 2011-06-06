using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.IO;
using Novell.FormsTrayApp;
namespace Novell.Wizard
{
 public class ServerPage : Novell.Wizard.InteriorPageTemplate
 {
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.TextBox serverAddress;
  private System.Windows.Forms.CheckBox defaultServer;
  private System.Windows.Forms.Label defaultDescription;
  private System.ComponentModel.IContainer components = null;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));
  private int defaultTextXPos =50;
  private int defaultTextYPos = 110;
  private int defaultSpacing = 16;
  private int maxTextWidth = 420;
  private int defaultTextWidth = 100;
        private SizeF strSize;
  public ServerPage( bool makeDefaultAccount )
  {
   InitializeComponent();
   defaultDescription.Visible = defaultServer.Visible = !makeDefaultAccount;
   defaultServer.Checked = makeDefaultAccount;
  }
        public void DisableServerEntry()
        {
            this.serverAddress.ReadOnly = true;
        }
  private void InitializeComponent()
  {
   this.serverAddress = new System.Windows.Forms.TextBox();
   this.label1 = new System.Windows.Forms.Label();
   this.label2 = new System.Windows.Forms.Label();
   this.defaultDescription = new System.Windows.Forms.Label();
   this.defaultServer = new System.Windows.Forms.CheckBox();
            this.strSize = new SizeF();
   this.SuspendLayout();
            Graphics graphics = CreateGraphics();
   this.label2.Location = new System.Drawing.Point( this.defaultTextXPos-10, this.defaultTextYPos );
   this.label2.Name = "label2";
   this.label2.TabIndex = 0;
   this.label2.Text = Resource.GetString("ServerPagelbl2txt");
            this.strSize = graphics.MeasureString(this.label2.Text, this.label2.Font);
   this.label2.Size = new System.Drawing.Size( this.maxTextWidth , ((int)this.strSize.Width/this.maxTextWidth+1)*16 );
   this.label1.Location = new System.Drawing.Point( this.defaultTextXPos, this.label2.Location.Y+this.label2.Size.Height+this.defaultSpacing );
   this.label1.Name = "label1";
   this.label1.TabIndex = 1;
            this.label1.Text = Resource.GetString("ServerNameTextbox") + ":";
            this.strSize = graphics.MeasureString(this.label1.Text, this.label1.Font);
   this.label1.Size = new System.Drawing.Size( this.defaultTextWidth, ((int)this.strSize.Width/this.defaultTextWidth+1)*16 );
   this.serverAddress.Location = new System.Drawing.Point( 145, this.label1.Location.Y-2);
   this.serverAddress.Name = "serverAddress";
   this.serverAddress.Size = new System.Drawing.Size(320, 20);
   this.serverAddress.TabIndex = 2;
   this.serverAddress.Text = "";
   this.serverAddress.TextChanged += new System.EventHandler(this.serverAddress_TextChanged);
   this.defaultDescription.Location = new System.Drawing.Point( this.defaultTextXPos-10, this.label1.Location.Y+this.label1.Size.Height+this.defaultSpacing );
   this.defaultDescription.Name = "defaultDescription";
   this.defaultDescription.TabIndex = 3;
   this.defaultDescription.Text = Resource.GetString("ServerPageDesc");
            this.strSize = graphics.MeasureString(this.defaultDescription.Text, this.defaultDescription.Font);
   this.defaultDescription.Size = new System.Drawing.Size( this.maxTextWidth , ((int)this.strSize.Width/this.maxTextWidth+1)*16 );
   this.defaultServer.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.defaultServer.Location = new System.Drawing.Point( this.defaultTextXPos, this.defaultDescription.Location.Y+this.defaultDescription.Size.Height+this.defaultSpacing );
   this.defaultServer.Name = "defaultServer";
   this.defaultServer.Size = new System.Drawing.Size(406, 20);
   this.defaultServer.TabIndex = 4;
   this.defaultServer.Text = Resource.GetString("DefaultServerCheckbox");
            graphics.Dispose();
   this.Controls.Add(this.defaultServer);
   this.Controls.Add(this.serverAddress);
   this.Controls.Add(this.defaultDescription);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.label1);
   this.HeaderTitle = "";
   this.Name = "ServerPage";
   this.Controls.SetChildIndex(this.label1, 0);
   this.Controls.SetChildIndex(this.label2, 0);
   this.Controls.SetChildIndex(this.defaultDescription, 0);
   this.Controls.SetChildIndex(this.serverAddress, 0);
   this.Controls.SetChildIndex(this.defaultServer, 0);
   this.ResumeLayout(false);
            this.PerformLayout();
  }
        private void serverAddress_TextChanged(object sender, System.EventArgs e)
  {
   if (serverAddress.Text != "")
   {
    ((AccountWizard)this.Parent).WizardButtons = WizardButtons.Next | WizardButtons.Back | WizardButtons.Cancel;
   }
   else
   {
    ((AccountWizard)this.Parent).WizardButtons = WizardButtons.Back | WizardButtons.Cancel;
   }
  }
  internal override void ActivatePage(int previousIndex)
  {
   base.ActivatePage (previousIndex);
   serverAddress_TextChanged(this, null);
   serverAddress.Focus();
  }
        internal override int DeactivatePage()
  {
   return base.DeactivatePage ();
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
   return base.ValidatePage (currentIndex);
  }
  public string ServerAddress
  {
   get { return serverAddress.Text; }
            set { serverAddress.Text = value; }
  }
  public bool DefaultServer
  {
   get { return defaultServer.Checked; }
  }
 }
}
