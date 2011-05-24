

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;

namespace Novell.InvitationWizard
{



 public class InteriorPageTemplate : Novell.InvitationWizard.BaseWizardPage
 {

  private System.Windows.Forms.Panel headerPanel;
  private System.Windows.Forms.GroupBox groupBox1;
  private System.Windows.Forms.PictureBox pictureBox1;
  private System.Windows.Forms.Label headerTitle;
  private System.Windows.Forms.Label headerSubTitle;



  private System.ComponentModel.Container components = null;





  public InteriorPageTemplate()
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
   this.pictureBox1 = new System.Windows.Forms.PictureBox();
   this.headerSubTitle = new System.Windows.Forms.Label();
   this.headerTitle = new System.Windows.Forms.Label();
   this.groupBox1 = new System.Windows.Forms.GroupBox();
   this.headerPanel.SuspendLayout();
   this.SuspendLayout();



   this.headerPanel.BackColor = System.Drawing.Color.White;
   this.headerPanel.Controls.Add(this.pictureBox1);
   this.headerPanel.Controls.Add(this.headerSubTitle);
   this.headerPanel.Controls.Add(this.headerTitle);
   this.headerPanel.Location = new System.Drawing.Point(0, 0);
   this.headerPanel.Name = "headerPanel";
   this.headerPanel.Size = new System.Drawing.Size(496, 56);
   this.headerPanel.TabIndex = 0;



   this.pictureBox1.Location = new System.Drawing.Point(452, 12);
   this.pictureBox1.Name = "pictureBox1";
   this.pictureBox1.Size = new System.Drawing.Size(32, 32);
   this.pictureBox1.TabIndex = 2;
   this.pictureBox1.TabStop = false;



   this.headerSubTitle.Location = new System.Drawing.Point(40, 32);
   this.headerSubTitle.Name = "headerSubTitle";
   this.headerSubTitle.Size = new System.Drawing.Size(408, 16);
   this.headerSubTitle.TabIndex = 1;
   this.headerSubTitle.Text = "Header subtitle";



   this.headerTitle.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.headerTitle.Location = new System.Drawing.Point(24, 16);
   this.headerTitle.Name = "headerTitle";
   this.headerTitle.Size = new System.Drawing.Size(424, 16);
   this.headerTitle.TabIndex = 0;
   this.headerTitle.Text = "Header Title";



   this.groupBox1.Location = new System.Drawing.Point(0, 56);
   this.groupBox1.Name = "groupBox1";
   this.groupBox1.Size = new System.Drawing.Size(496, 4);
   this.groupBox1.TabIndex = 1;
   this.groupBox1.TabStop = false;
   this.groupBox1.Text = "groupBox1";



   this.Controls.Add(this.groupBox1);
   this.Controls.Add(this.headerPanel);
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
   get { return pictureBox1.Image; }
   set { pictureBox1.Image = value; }
  }

 }
}
