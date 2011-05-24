
using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using Novell.FormsTrayApp;

namespace Novell.Wizard
{



 public class MigrationWelcomePageTemplate : Novell.Wizard.MigrationBaseWizardPage
 {

  private System.Windows.Forms.Panel panel1;
  private System.Windows.Forms.Label welcomeTitle;
  private System.Windows.Forms.Label descriptionText;
  private System.Windows.Forms.PictureBox waterMark;
  private System.Windows.Forms.Label actionText;
  private System.Windows.Forms.PictureBox waterMark2;
  private System.Windows.Forms.Label headerTitle;
  private static System.Resources.ResourceManager Resource = new System.Resources.ResourceManager(typeof(Novell.FormsTrayApp.FormsTrayApp));



  private System.ComponentModel.Container components = null;






  public MigrationWelcomePageTemplate()
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






  protected void InitializeComponent()
  {
   this.panel1 = new System.Windows.Forms.Panel();
   this.actionText = new System.Windows.Forms.Label();
   this.descriptionText = new System.Windows.Forms.Label();
   this.welcomeTitle = new System.Windows.Forms.Label();
   this.waterMark = new System.Windows.Forms.PictureBox();
   this.waterMark2 = new System.Windows.Forms.PictureBox();
   this.headerTitle = new Label();
   this.panel1.SuspendLayout();
   this.SuspendLayout();



   this.headerTitle.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.headerTitle.Location = new System.Drawing.Point(16,16);
   this.headerTitle.Name = "headerTitle";
   this.headerTitle.Size = new System.Drawing.Size(350, 16);



   this.panel1.Controls.Add(this.headerTitle);
   this.panel1.Controls.Add(this.actionText);
   this.panel1.Controls.Add(this.descriptionText);
   this.panel1.Controls.Add(this.welcomeTitle);
   this.panel1.Location = new System.Drawing.Point(0, 65);
   this.panel1.Name = "panel1";
   this.panel1.Size = new System.Drawing.Size(500,235);
   this.panel1.TabIndex = 1;



   this.actionText.Location = new System.Drawing.Point(16, 270);
   this.actionText.Name = "actionText";
   this.actionText.Size = new System.Drawing.Size(296, 40);
   this.actionText.TabIndex = 2;
   this.actionText.Text = "";



   this.descriptionText.Location = new System.Drawing.Point(100, 100);
   this.descriptionText.Name = "descriptionText";
   this.descriptionText.Size = new System.Drawing.Size(296, 150);
   this.descriptionText.TabIndex = 1;
   this.descriptionText.Text = "";



   this.welcomeTitle.Font = new System.Drawing.Font("TimesNewRoman", 16F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.welcomeTitle.Location = new System.Drawing.Point(100, 30);
   this.welcomeTitle.Name = "welcomeTitle";
   this.welcomeTitle.Size = new System.Drawing.Size(296, 64);
   this.welcomeTitle.TabIndex = 0;
   this.welcomeTitle.Text = "Welcome to the <WIZARD> Wizard";




   this.waterMark.Location = new System.Drawing.Point(0,0);
   this.waterMark.Name = "waterMark";
   this.waterMark.Size = new System.Drawing.Size(159,65);
   this.waterMark.TabIndex = 0;
   this.waterMark.TabStop = false;
   this.waterMark.Image = System.Drawing.Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner.png"));




   this.waterMark2.Location = new System.Drawing.Point(157,0);
   this.waterMark2.Name = "waterMark";
   this.waterMark2.Size = new System.Drawing.Size(342,65);
   this.waterMark2.TabIndex = 0;
   this.waterMark2.TabStop = false;
   this.waterMark2.SizeMode = PictureBoxSizeMode.StretchImage;
   this.waterMark2.Image = System.Drawing.Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner-scaler.png"));




   this.Controls.Add(this.panel1);
            this.Controls.Add(this.waterMark);
   this.Controls.Add(this.waterMark2);
   this.Name = "MigrationWelcomePageTemplate";
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
