using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
namespace Novell.Wizard
{
 public class InteriorPageTemplate : Novell.Wizard.BaseWizardPage
 {
  private System.Windows.Forms.Panel headerPanel;
  private System.Windows.Forms.GroupBox groupBox1;
  private System.Windows.Forms.PictureBox pictureBox1;
  private System.Windows.Forms.Label headerTitle;
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
   this.headerTitle = new System.Windows.Forms.Label();
   this.groupBox1 = new System.Windows.Forms.GroupBox();
   this.headerPanel.SuspendLayout();
   this.SuspendLayout();
   this.headerPanel.BackColor = System.Drawing.Color.White;
   this.headerPanel.Controls.Add(this.pictureBox1);
   this.headerPanel.Controls.Add(this.headerTitle);
   this.headerPanel.Location = new System.Drawing.Point(0, 0);
   this.headerPanel.Name = "headerPanel";
   this.headerPanel.Size = new System.Drawing.Size(496, 56);
   this.headerPanel.TabIndex = 0;
   this.pictureBox1.Location = new System.Drawing.Point(436, 4);
   this.pictureBox1.Name = "pictureBox1";
   this.pictureBox1.Size = new System.Drawing.Size(48, 48);
   this.pictureBox1.TabIndex = 2;
   this.pictureBox1.TabStop = false;
   this.headerTitle.Font = new System.Drawing.Font("Microsoft Sans Serif", 14F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
   this.headerTitle.Location = new System.Drawing.Point(24, 16);
   this.headerTitle.Name = "headerTitle";
   this.headerTitle.Size = new System.Drawing.Size(424, 25);
   this.headerTitle.TabIndex = 0;
   this.headerTitle.Text = "";
   this.groupBox1.Location = new System.Drawing.Point(0, 56);
   this.groupBox1.Name = "groupBox1";
   this.groupBox1.Size = new System.Drawing.Size(496, 4);
   this.groupBox1.TabIndex = 1;
   this.groupBox1.TabStop = false;
   this.groupBox1.Text = "";
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
  public Image Thumbnail
  {
   get { return pictureBox1.Image; }
   set { pictureBox1.Image = value; }
  }
 }
}
