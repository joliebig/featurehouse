

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.IO;

namespace Novell.Wizard
{



 public class MigrationPassphraseVerifyPage : Novell.Wizard.MigrationInteriorPageTemplate
 {


  private System.Windows.Forms.Label EnterPPLabel;
  private System.Windows.Forms.Label RememberPPLabel;
  private System.Windows.Forms.TextBox EnterPPText;
  private System.Windows.Forms.CheckBox RememberPPCheck;

  private System.Windows.Forms.RadioButton removeFromServer;
  private System.Windows.Forms.RadioButton copyToServer;

  private System.Windows.Forms.CheckBox copyOption;

  private System.Windows.Forms.TextBox iFolderLocation;
  private System.Windows.Forms.Button browseButton;
  private System.Windows.Forms.CheckBox defaultServer;
  private System.Windows.Forms.Label defaultDescription;
  private System.ComponentModel.IContainer components = null;

  private System.Windows.Forms.Label label1, label2;

  private string homeLocation;
  private string prevLoc;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));
  public MigrationPassphraseVerifyPage()
  {
   this.homeLocation = "Home Location";
   InitializeComponent();
  }
  private void InitializeComponent()
  {
   EnterPPLabel = new Label();
   this.EnterPPText = new TextBox();
   RememberPPLabel = new Label();
   RememberPPCheck = new CheckBox();
   EnterPPLabel.Text = Resource.GetString("EnterPassPhrase");
   EnterPPLabel.Location = new System.Drawing.Point(60, 122);
   EnterPPLabel.Size = new System.Drawing.Size(120, 16);
   this.EnterPPText.Text = "";
   this.EnterPPText.Location = new System.Drawing.Point(180, 122);
   this.EnterPPText.Size = new System.Drawing.Size(260, 16);
   RememberPPCheck.Text = Resource.GetString("RememberPassPhrase");
   RememberPPCheck.Location = new System.Drawing.Point(180, 156);
   RememberPPCheck.Size = new System.Drawing.Size(350, 16);
   this.SuspendLayout();
   this.Controls.Add(this.EnterPPLabel);
   this.Controls.Add(this.RememberPPCheck);
   this.Controls.Add(this.EnterPPText);
   this.ResumeLayout(false);
  }
  internal override void ActivatePage(int previousIndex)
  {
   ((MigrationWizard)this.Parent).MigrationWizardButtons = MigrationWizardButtons.Next | MigrationWizardButtons.Back | MigrationWizardButtons.Cancel;
   base.ActivatePage (previousIndex);
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
  public string Passphrase
  {
   get
   {
    return this.EnterPPText.Text;
   }
  }
  public bool RememberPassphrase
  {
   get
   {
    return this.RememberPPCheck.Checked;
   }
  }
  private void copyToServer_CheckedChanged(object sender, EventArgs e)
  {
  }
 }
}
