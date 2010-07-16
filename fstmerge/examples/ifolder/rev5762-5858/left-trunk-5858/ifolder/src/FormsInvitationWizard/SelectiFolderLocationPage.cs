

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.IO;
using Simias;
using Simias.Sync;
using Simias.POBox;

namespace Novell.InvitationWizard
{




 public class SelectiFolderLocationPage : Novell.InvitationWizard.InteriorPageTemplate
 {

  private static readonly ISimiasLog logger = SimiasLogManager.GetLogger(typeof(SelectiFolderLocationPage));
  private System.Windows.Forms.Label label1;
  private System.Windows.Forms.Button folderBrowser;
  private System.Windows.Forms.TextBox iFolderLocation;
  private System.ComponentModel.IContainer components = null;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.Label label3;
  private bool init = false;







  public SelectiFolderLocationPage()
  {
   init = true;


   InitializeComponent();

   iFolderLocation.Text = Subscription.DefaultRootPath;
   init = false;
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






  private void InitializeComponent()
  {
   this.label1 = new System.Windows.Forms.Label();
   this.iFolderLocation = new System.Windows.Forms.TextBox();
   this.folderBrowser = new System.Windows.Forms.Button();
   this.label2 = new System.Windows.Forms.Label();
   this.label3 = new System.Windows.Forms.Label();
   this.SuspendLayout();



   this.label1.Location = new System.Drawing.Point(40, 184);
   this.label1.Name = "label1";
   this.label1.Size = new System.Drawing.Size(128, 16);
   this.label1.TabIndex = 2;
   this.label1.Text = "Shared iFolder Location:";



   this.iFolderLocation.Location = new System.Drawing.Point(40, 200);
   this.iFolderLocation.Name = "iFolderLocation";
   this.iFolderLocation.Size = new System.Drawing.Size(360, 20);
   this.iFolderLocation.TabIndex = 3;
   this.iFolderLocation.Text = "";
   this.iFolderLocation.TextChanged += new System.EventHandler(this.iFolderLocation_TextChanged);



   this.folderBrowser.FlatStyle = System.Windows.Forms.FlatStyle.System;
   this.folderBrowser.Location = new System.Drawing.Point(408, 200);
   this.folderBrowser.Name = "folderBrowser";
   this.folderBrowser.Size = new System.Drawing.Size(72, 24);
   this.folderBrowser.TabIndex = 4;
   this.folderBrowser.Text = "Browse...";
   this.folderBrowser.Click += new System.EventHandler(this.folderBrowser_Click);



   this.label2.Location = new System.Drawing.Point(40, 80);
   this.label2.Name = "label2";
   this.label2.Size = new System.Drawing.Size(440, 32);
   this.label2.TabIndex = 5;
   this.label2.Text = "The destination location of the shared iFolder cannot be the root directory of a " +
    "local drive and cannot be in an existing iFolder.";



   this.label3.Location = new System.Drawing.Point(40, 128);
   this.label3.Name = "label3";
   this.label3.Size = new System.Drawing.Size(440, 32);
   this.label3.TabIndex = 6;
   this.label3.Text = "To place the shared iFolder in a different location, type the path, or click Brow" +
    "se and select another folder. ";



   this.Controls.Add(this.label3);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.folderBrowser);
   this.Controls.Add(this.iFolderLocation);
   this.Controls.Add(this.label1);
   this.HeaderSubTitle = "HeaderSubTitle";
   this.HeaderTitle = "HeaderTitle";
   this.Name = "SelectiFolderLocationPage";
   this.Controls.SetChildIndex(this.label1, 0);
   this.Controls.SetChildIndex(this.iFolderLocation, 0);
   this.Controls.SetChildIndex(this.folderBrowser, 0);
   this.Controls.SetChildIndex(this.label2, 0);
   this.Controls.SetChildIndex(this.label3, 0);
   this.ResumeLayout(false);

  }



  private void folderBrowser_Click(object sender, System.EventArgs e)
  {
   FolderBrowserDialog folderBrowserDialog = new FolderBrowserDialog();


   if ((iFolderLocation.Text != "") && Directory.Exists(iFolderLocation.Text))
   {
    folderBrowserDialog.SelectedPath = iFolderLocation.Text;
   }

   if(folderBrowserDialog.ShowDialog() == DialogResult.OK)
   {
    iFolderLocation.Text = folderBrowserDialog.SelectedPath;
   }
  }

  private void iFolderLocation_TextChanged(object sender, System.EventArgs e)
  {
   if (!init)
   {

    if (iFolderLocation.Text != "")
    {
     ((InvitationWizard)this.Parent).WizardButtons = WizardButtons.Next | WizardButtons.Back | WizardButtons.Cancel;
    }
    else
    {
     ((InvitationWizard)this.Parent).WizardButtons = WizardButtons.Back | WizardButtons.Cancel;
    }
   }
  }



  internal override int ValidatePage(int currentIndex)
  {
   if (!Directory.Exists(iFolderLocation.Text))
   {


    bool parentExists = false;
    string parent = Path.GetDirectoryName(iFolderLocation.Text);
    while (parent != "")
    {
     if (Directory.Exists(parent))
     {
      parentExists = true;
      break;
     }

     parent = Path.GetDirectoryName(parent);
    }

    if (!parentExists)
    {
     iFolderLocation.Text = Path.Combine(Environment.CurrentDirectory, iFolderLocation.Text);
    }


    if (MessageBox.Show("The directory specified does not exist.  Do you want the wizard to create the directory?", "Create Directory", MessageBoxButtons.YesNo, MessageBoxIcon.Question, MessageBoxDefaultButton.Button1) == DialogResult.Yes)
    {

     try
     {
      Directory.CreateDirectory(iFolderLocation.Text);
     }
     catch (Exception e)
     {
      MessageBox.Show("Unable to create directory:\n\n" + e.Message, "Create Failed");
      iFolderLocation.Focus();
      return currentIndex;
     }
    }
    else
    {
     iFolderLocation.Focus();
     return currentIndex;
    }
   }
   else
   {

    Cursor = Cursors.WaitCursor;

    bool isPathInvalid = true;


    try
    {


    }
    catch (SimiasException e)
    {
     e.LogError();
     MessageBox.Show("An exception occurred while validating the path.  Please view the log file for additional information.", "Path Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
    }
    catch (Exception e)
    {
     logger.Debug(e, "Validating path");
     MessageBox.Show("An exception occurred while validating the path.  Please view the log file for additional information.", "Path Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
    }


    Cursor = Cursors.Default;

    if (isPathInvalid)
    {

     MessageBox.Show("The location selected for the new iFolder is below an existing iFolder and cannot be used.  Please select a new location.", "Invalid Directory", MessageBoxButtons.OK, MessageBoxIcon.Error);
     iFolderLocation.Focus();

     return currentIndex;
    }
   }


   ((InvitationWizard)(this.Parent)).Subscription.CollectionRoot = iFolderLocation.Text;

   return base.ValidatePage (currentIndex);
  }

  internal override void ActivatePage(int previousIndex)
  {

   iFolderLocation_TextChanged(null, null);

   base.ActivatePage (previousIndex);
  }

 }
}
