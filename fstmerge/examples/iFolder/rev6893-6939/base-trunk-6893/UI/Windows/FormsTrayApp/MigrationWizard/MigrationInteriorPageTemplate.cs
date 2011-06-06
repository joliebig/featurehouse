using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
namespace Novell.Wizard
{
 public class MigrationInteriorPageTemplate : Novell.Wizard.MigrationBaseWizardPage
 {
  private System.Windows.Forms.Panel headerPanel;
  private System.Windows.Forms.PictureBox left_image;
  private System.Windows.Forms.PictureBox right_image;
  private System.Windows.Forms.Label headerTitle;
  private System.Windows.Forms.Label headerSubTitle;
  private System.ComponentModel.Container components = null;
  public MigrationInteriorPageTemplate()
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
   this.headerPanel = new System.Windows.Forms.Panel();
   this.left_image = new System.Windows.Forms.PictureBox();
   this.right_image = new System.Windows.Forms.PictureBox();
   this.headerSubTitle = new System.Windows.Forms.Label();
   this.headerTitle = new System.Windows.Forms.Label();
   this.headerPanel.SuspendLayout();
   this.SuspendLayout();
   this.headerPanel.Controls.Add(this.left_image);
   this.headerPanel.Controls.Add(this.right_image);
   this.headerPanel.Location = new System.Drawing.Point(0, 0);
   this.headerPanel.Name = "headerPanel";
   this.headerPanel.Size = new System.Drawing.Size(496, 65);
   this.headerPanel.TabIndex = 0;
   this.left_image.Location = new System.Drawing.Point(0, 0);
   this.left_image.Name = "left_image";
   this.left_image.Size = new System.Drawing.Size(159,65);
   this.left_image.TabIndex = 2;
   this.left_image.TabStop = false;
   this.left_image.Image = System.Drawing.Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner.png"));
   this.right_image.Location = new System.Drawing.Point(157,0);
   this.right_image.Name = "right_image";
   this.right_image.Size = new System.Drawing.Size(342,65);
   this.right_image.TabIndex = 3;
   this.right_image.TabStop = false;
   this.right_image.SizeMode = PictureBoxSizeMode.StretchImage;
   this.right_image.Image = System.Drawing.Image.FromFile(System.IO.Path.Combine(Application.StartupPath, @"res\ifolder-banner-scaler.png"));
   this.headerTitle.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.headerTitle.Location = new System.Drawing.Point(16,70);
   this.headerTitle.Name = "headerTitle";
   this.headerTitle.Size = new System.Drawing.Size(424, 16);
   this.headerTitle.TabIndex = 0;
   this.headerTitle.Text = "";
   this.Controls.Add(this.headerPanel);
   this.Controls.Add(this.headerTitle);
   this.Name = "InteriorPageTemplate";
   this.Size = new System.Drawing.Size(496, 314);
   this.headerPanel.ResumeLayout(false);
   this.ResumeLayout(false);
  }
  public string HeaderTitle
  {
   get { return headerTitle.Text; }
   set { headerTitle.Text = value; }
  }
  public string HeaderSubTitle
  {
   get { return headerSubTitle.Text; }
   set { headerSubTitle.Text = value; }
  }
  public Image Thumbnail
  {
   get { return left_image.Image; }
   set { left_image.Image = value; }
  }
 }
}
