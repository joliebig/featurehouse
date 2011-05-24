

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using Novell.FormsTrayApp;

namespace Novell.Wizard
{



 public class WelcomePageTemplate : Novell.Wizard.BaseWizardPage
 {

  private System.Windows.Forms.Panel panel1;
  private System.Windows.Forms.Label welcomeTitle;
  private System.Windows.Forms.Label descriptionText;
  private System.Windows.Forms.PictureBox waterMark;
  private System.Windows.Forms.Label actionText;

  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));



  private System.ComponentModel.Container components = null;





  public WelcomePageTemplate()
  {

   InitializeComponent();
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
   this.panel1 = new System.Windows.Forms.Panel();
   this.actionText = new System.Windows.Forms.Label();
   this.descriptionText = new System.Windows.Forms.Label();
   this.welcomeTitle = new System.Windows.Forms.Label();
   this.waterMark = new System.Windows.Forms.PictureBox();
   this.panel1.SuspendLayout();
   this.SuspendLayout();



   this.panel1.BackColor = System.Drawing.Color.White;
   this.panel1.Controls.Add(this.actionText);
   this.panel1.Controls.Add(this.descriptionText);
   this.panel1.Controls.Add(this.welcomeTitle);
   this.panel1.Location = new System.Drawing.Point(168, 0);
   this.panel1.Name = "panel1";
   this.panel1.Size = new System.Drawing.Size(328, 304);
   this.panel1.TabIndex = 1;



   this.actionText.Location = new System.Drawing.Point(16, 256);
   this.actionText.Name = "actionText";
   this.actionText.Size = new System.Drawing.Size(296, 40);
   this.actionText.TabIndex = 2;
   this.actionText.Text = "";



   this.descriptionText.Location = new System.Drawing.Point(16, 72);
   this.descriptionText.Name = "descriptionText";
   this.descriptionText.Size = new System.Drawing.Size(296, 184);
   this.descriptionText.TabIndex = 1;
   this.descriptionText.Text = Resource.GetString("CompletionPageDT");



   this.welcomeTitle.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.welcomeTitle.Location = new System.Drawing.Point(16, 16);
   this.welcomeTitle.Name = "welcomeTitle";
   this.welcomeTitle.Size = new System.Drawing.Size(296, 40);
   this.welcomeTitle.TabIndex = 0;
   this.welcomeTitle.Text = Resource.GetString("WelcomePageTitle");



   this.waterMark.Location = new System.Drawing.Point(0, 0);
   this.waterMark.Name = "waterMark";
   this.waterMark.Size = new System.Drawing.Size(168, 304);
   this.waterMark.TabIndex = 0;
   this.waterMark.TabStop = false;



   this.Controls.Add(this.panel1);
   this.Controls.Add(this.waterMark);
   this.Name = "WelcomePageTemplate";
   this.panel1.ResumeLayout(false);
   this.ResumeLayout(false);

  }






  public string ActionText
  {
   get
   {
    return actionText.Text;
   }

   set
   {
    actionText.Text = value;
   }
  }




  public string WelcomeTitle
  {
   get
   {
    return welcomeTitle.Text;
   }

   set
   {
    welcomeTitle.Text = value;
   }
  }




  public string DescriptionText
  {
   get
   {
    return descriptionText.Text;
   }

   set
   {
    descriptionText.Text = value;
   }
  }




  public Image Watermark
  {
   get
   {
    return waterMark.Image;
   }
   set
   {
    waterMark.Image = value;
   }
  }

 }
}
