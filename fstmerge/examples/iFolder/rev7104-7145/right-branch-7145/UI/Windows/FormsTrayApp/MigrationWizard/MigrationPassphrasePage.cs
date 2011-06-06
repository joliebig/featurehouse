using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.IO;
namespace Novell.Wizard
{
 public class MigrationPassphrasePage : Novell.Wizard.MigrationInteriorPageTemplate
 {
  private System.Windows.Forms.Label EnterPPTitle;
  private System.Windows.Forms.Label EnterPPLabel;
  private System.Windows.Forms.Label RetypePPLabel;
  private System.Windows.Forms.Label RememberPPLabel;
  private System.Windows.Forms.TextBox EnterPPText;
  private System.Windows.Forms.TextBox RetypePPText;
  private System.Windows.Forms.CheckBox RememberPPCheck;
  private System.Windows.Forms.Label RecoveryAgentTitle;
  private System.Windows.Forms.Label RecoveryAgentLabel;
  private System.Windows.Forms.ComboBox RecoveryAgentCombo;
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
  public MigrationPassphrasePage()
  {
   this.homeLocation = "Home Location";
   InitializeComponent();
  }
  private void InitializeComponent()
  {
   EnterPPTitle = new Label();
   EnterPPLabel = new Label();
   RetypePPLabel = new Label();
   this.EnterPPText = new TextBox();
   this.RetypePPText = new TextBox();
   this.RecoveryAgentTitle = new Label();
   this.RecoveryAgentLabel = new Label();
   this.RecoveryAgentCombo = new ComboBox();
   RememberPPLabel = new Label();
   RememberPPCheck = new CheckBox();
   EnterPPTitle.Text = Resource.GetString("PassPhraseTitle");
   EnterPPTitle.Location = new System.Drawing.Point(40, 96);
   EnterPPTitle.Size = new System.Drawing.Size(416, 16);
   EnterPPLabel.Text = Resource.GetString("EnterPassPhrase");
   EnterPPLabel.Location = new System.Drawing.Point(80, 122);
   EnterPPLabel.Size = new System.Drawing.Size(120, 16);
   this.EnterPPText.Text = "";
   this.EnterPPText.Location = new System.Drawing.Point(200, 122);
   this.EnterPPText.Size = new System.Drawing.Size(260, 16);
   RetypePPLabel.Text = Resource.GetString("ReTypePassPhrase");
   RetypePPLabel.Location = new System.Drawing.Point(80, 146);
   RetypePPLabel.Size = new System.Drawing.Size(120, 16);
   this.RetypePPText.Text = "";
   this.RetypePPText.Location = new System.Drawing.Point(200, 146);
   this.RetypePPText.Size = new System.Drawing.Size(260, 16);
   RememberPPLabel.Text = Resource.GetString("RememberPassPhrase");
   RememberPPLabel.Location = new System.Drawing.Point(200, 170);
   RememberPPLabel.Size = new System.Drawing.Size(350, 16);
   RememberPPCheck.Text = Resource.GetString("RememberPassPhrase");
   RememberPPCheck.Location = new System.Drawing.Point(200, 170);
   RememberPPCheck.Size = new System.Drawing.Size(350, 16);
   this.RecoveryAgentTitle.Text = Resource.GetString("SelectRecoveryAgent");
   this.RecoveryAgentTitle.Location = new System.Drawing.Point(40, 196);
   this.RecoveryAgentTitle.Size = new System.Drawing.Size(416, 16);
   this.RecoveryAgentLabel.Text = Resource.GetString("RecoveryAgent");
   this.RecoveryAgentLabel.Location = new System.Drawing.Point(80, 220);
   this.RecoveryAgentLabel.Size = new System.Drawing.Size(120, 16);
   this.RecoveryAgentCombo.Location = new System.Drawing.Point(200, 220);
   this.RecoveryAgentCombo.Size = new System.Drawing.Size(260, 16);
   this.SuspendLayout();
   this.Controls.Add(this.EnterPPTitle);
   this.Controls.Add(this.EnterPPLabel);
   this.Controls.Add(this.RememberPPCheck);
   this.Controls.Add(this.RetypePPLabel);
   this.Controls.Add(this.RetypePPText);
   this.Controls.Add(this.EnterPPText);
   this.Controls.Add(this.RecoveryAgentLabel);
   this.Controls.Add(this.RecoveryAgentTitle);
   this.Controls.Add(this.RecoveryAgentCombo);
   this.Load+= new EventHandler(MigrationPassphrasePage_Load);
   this.ResumeLayout(false);
  }
  private void browseButton_Click(object sender, System.EventArgs args)
  {
  }
  private void removeClicked(object sender, System.EventArgs args)
  {
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
  public string RetypePassphrase
  {
   get
   {
    return this.RetypePPText.Text;
   }
  }
  public string RecoveryAgent
  {
   get
   {
    if( this.RecoveryAgentCombo.SelectedIndex >= 0)
     return (string)(this.RecoveryAgentCombo.SelectedText);
    return null;
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
        private void MigrationPassphrasePage_Load(object sender, EventArgs e)
  {
   string[] rAgents= ((MigrationWizard)this.Parent).simws.GetRAListOnClient(((MigrationWizard)this.Parent).MigrationIdentityPage.domain.ID);
   foreach( string rAgent in rAgents)
   {
    this.RecoveryAgentCombo.Items.Add( rAgent );
   }
   this.RecoveryAgentCombo.Items.Add("None");
  }
 }
}
