using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.IO;
using Novell.FormsTrayApp;
using Novell.iFolderCom;
namespace Novell.Wizard
{
 public class MigrationServerPage : Novell.Wizard.MigrationInteriorPageTemplate
 {
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.RadioButton removeFromServer;
  private System.Windows.Forms.RadioButton copyToServer;
  private System.Windows.Forms.CheckBox copyOption;
  private System.Windows.Forms.TextBox iFolderLocation;
  private System.Windows.Forms.Button browseButton;
  private System.Windows.Forms.CheckBox defaultServer;
  private System.Windows.Forms.Label defaultDescription;
  private System.ComponentModel.IContainer components = null;
  private static System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(CreateiFolder));
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));
  private string homeLocation;
  private string prevLoc;
  public MigrationServerPage(string loc)
  {
   this.homeLocation = loc;
   InitializeComponent();
  }
  private void InitializeComponent()
  {
   this.label1 = new System.Windows.Forms.Label();
   this.label2 = new System.Windows.Forms.Label();
   this.removeFromServer = new RadioButton();
   this.copyToServer = new RadioButton();
   this.iFolderLocation = new TextBox();
   this.copyOption = new CheckBox();
   this.browseButton = new Button();
   this.defaultDescription = new System.Windows.Forms.Label();
   this.defaultServer = new System.Windows.Forms.CheckBox();
   this.SuspendLayout();
   this.label1.Location = new System.Drawing.Point(40, 215);
   this.label1.Name = "label1";
   this.label1.Size = new System.Drawing.Size(50, 24);
   this.label1.TabIndex = 1;
   this.label1.Text = Resource.GetString("LocationText");
   this.iFolderLocation.Location = new System.Drawing.Point(96, 213);
   this.iFolderLocation.Name = "iFolderLocation";
   this.iFolderLocation.Size = new System.Drawing.Size(280, 20);
   this.iFolderLocation.TabIndex = 2;
   this.iFolderLocation.Text = this.homeLocation;
   this.iFolderLocation.Enabled = false;
            this.iFolderLocation.TextChanged += new EventHandler(iFolderLocation_TextChanged);
   this.browseButton.Location = new Point(390, 213);
   this.browseButton.Size = new Size(75, 20);
   this.browseButton.Name = "browseButton";
   this.browseButton.Text = resourceManager.GetString("browse.Text");
   this.browseButton.Enabled = false;
   this.browseButton.Click += new EventHandler(browseButton_Click);
   this.removeFromServer.Location = new Point(96, 125);
   this.removeFromServer.Name = "removeFromServer";
   this.removeFromServer.Size = new Size(320, 20);
   this.removeFromServer.TabIndex = 2;
   this.removeFromServer.Text = Resource.GetString("MigrateNRemove");
   this.removeFromServer.Checked = true;
   this.removeFromServer.CheckedChanged += new EventHandler(removeClicked);
   this.copyToServer.Location = new Point(96, 150);
   this.copyToServer.Name = "copyToServer";
   this.copyToServer.Size = new Size(320, 20);
   this.copyToServer.Text = Resource.GetString("MigrateNCopy");
   this.copyToServer.Checked = false;
   this.copyToServer.CheckedChanged +=new EventHandler(copyToServer_CheckedChanged);
   this.copyOption.Location = new Point(112, 175);
   this.copyOption.Name = "copyOption";
   this.copyOption.Size = new Size(320,20);
   this.copyOption.Text = Resource.GetString("CopyParent");
   this.copyOption.Enabled = false;
   this.copyOption.Visible = true;
   this.copyOption.Checked = false;
   this.label2.Location = new System.Drawing.Point(40, 100);
   this.label2.Name = "label2";
   this.label2.Size = new System.Drawing.Size(416, 24);
   this.label2.TabIndex = 1;
   this.label2.Text = Resource.GetString("SelectAnOption");
   this.Controls.Add(this.removeFromServer);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.label1);
   this.Controls.Add(this.copyToServer);
   this.Controls.Add(this.iFolderLocation);
   this.Controls.Add(this.browseButton);
   this.Controls.Add(this.copyOption);
   this.ResumeLayout(false);
  }
        void iFolderLocation_TextChanged(object sender, EventArgs e)
        {
            if (this.copyToServer.Checked == true)
            {
                if (this.iFolderLocation.Text == null || this.iFolderLocation.Text == string.Empty)
                {
                    ((MigrationWizard)this.Parent).MigrationWizardButtons = MigrationWizardButtons.Back | MigrationWizardButtons.Cancel;
                }
                else
                    ((MigrationWizard)this.Parent).MigrationWizardButtons = MigrationWizardButtons.Next | MigrationWizardButtons.Back | MigrationWizardButtons.Cancel;
            }
            else
                ((MigrationWizard)this.Parent).MigrationWizardButtons = MigrationWizardButtons.Next | MigrationWizardButtons.Back | MigrationWizardButtons.Cancel;
        }
  private void browseButton_Click(object sender, System.EventArgs args)
  {
   FolderBrowserDialog folderBrowserDialog = new FolderBrowserDialog();
   folderBrowserDialog.Description = "chooseFolder";
   folderBrowserDialog.SelectedPath = this.iFolderLocation.Text;
   if(folderBrowserDialog.ShowDialog() == DialogResult.OK)
   {
    this.iFolderLocation.Text = folderBrowserDialog.SelectedPath;
   }
  }
  private void removeClicked(object sender, System.EventArgs args)
  {
   if(removeFromServer.Checked == true)
   {
    this.browseButton.Enabled = false;
    this.iFolderLocation.Enabled = false;
    this.prevLoc = this.iFolderLocation.Text;
    this.iFolderLocation.Text = homeLocation;
   }
   else
   {
    this.browseButton.Enabled = true;
    this.iFolderLocation.Enabled = true;
    this.iFolderLocation.Text = this.prevLoc;
   }
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
  public bool MigrationOption
  {
   get
   {
    return this.removeFromServer.Checked;
   }
  }
  public bool CopyParentDirectory
  {
   get
   {
    return this.copyOption.Checked;
   }
  }
  public string HomeLocation
  {
   get
   {
    return this.iFolderLocation.Text;
   }
  }
        private void copyToServer_CheckedChanged(object sender, EventArgs e)
  {
   if(this.copyToServer.Checked == true)
    this.copyOption.Enabled = true;
   else
    this.copyOption.Enabled = false;
  }
 }
}
