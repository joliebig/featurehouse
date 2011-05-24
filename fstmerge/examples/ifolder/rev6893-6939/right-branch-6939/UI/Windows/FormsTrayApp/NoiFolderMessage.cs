

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;

namespace Novell.FormsTrayApp
{



 public class NoiFolderMessage : System.Windows.Forms.UserControl
 {
  private System.Windows.Forms.PictureBox pictureBox2;
  private System.Windows.Forms.PictureBox pictureBox1;
  private System.Windows.Forms.RichTextBox richTextBox4;
  private System.Windows.Forms.RichTextBox richTextBox3;
  private System.Windows.Forms.RichTextBox richTextBox2;



  private System.ComponentModel.Container components = null;




  public NoiFolderMessage()
  {

   InitializeComponent();



  }




  public void DisplayNoMatches()
  {
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(NoiFolderMessage));
   this.richTextBox2.Text = resources.GetString("richTextBox2.NoMatchesText");
   this.richTextBox3.Visible = this.richTextBox4.Visible = false;
   this.pictureBox1.Visible = this.pictureBox2.Visible = false;
  }




  public void DisplayNoiFolders()
  {
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(NoiFolderMessage));
   this.richTextBox2.Text = resources.GetString("richTextBox2.Text");
   this.richTextBox3.Visible = this.richTextBox4.Visible = true;
   this.pictureBox1.Visible = this.pictureBox2.Visible = true;
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(NoiFolderMessage));
   this.pictureBox2 = new System.Windows.Forms.PictureBox();
   this.pictureBox1 = new System.Windows.Forms.PictureBox();
   this.richTextBox4 = new System.Windows.Forms.RichTextBox();
   this.richTextBox3 = new System.Windows.Forms.RichTextBox();
   this.richTextBox2 = new System.Windows.Forms.RichTextBox();
   this.SuspendLayout();



   this.pictureBox2.AccessibleDescription = resources.GetString("pictureBox2.AccessibleDescription");
   this.pictureBox2.AccessibleName = resources.GetString("pictureBox2.AccessibleName");
   this.pictureBox2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("pictureBox2.Anchor")));
   this.pictureBox2.BackColor = System.Drawing.SystemColors.Window;
   this.pictureBox2.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("pictureBox2.BackgroundImage")));
   this.pictureBox2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("pictureBox2.Dock")));
   this.pictureBox2.Enabled = ((bool)(resources.GetObject("pictureBox2.Enabled")));
   this.pictureBox2.Font = ((System.Drawing.Font)(resources.GetObject("pictureBox2.Font")));
   this.pictureBox2.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox2.Image")));
   this.pictureBox2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("pictureBox2.ImeMode")));
   this.pictureBox2.Location = ((System.Drawing.Point)(resources.GetObject("pictureBox2.Location")));
   this.pictureBox2.Name = "pictureBox2";
   this.pictureBox2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("pictureBox2.RightToLeft")));
   this.pictureBox2.Size = ((System.Drawing.Size)(resources.GetObject("pictureBox2.Size")));
   this.pictureBox2.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("pictureBox2.SizeMode")));
   this.pictureBox2.TabIndex = ((int)(resources.GetObject("pictureBox2.TabIndex")));
   this.pictureBox2.TabStop = false;
   this.pictureBox2.Text = resources.GetString("pictureBox2.Text");
   this.pictureBox2.Visible = ((bool)(resources.GetObject("pictureBox2.Visible")));



   this.pictureBox1.AccessibleDescription = resources.GetString("pictureBox1.AccessibleDescription");
   this.pictureBox1.AccessibleName = resources.GetString("pictureBox1.AccessibleName");
   this.pictureBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("pictureBox1.Anchor")));
   this.pictureBox1.BackColor = System.Drawing.SystemColors.Window;
   this.pictureBox1.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("pictureBox1.BackgroundImage")));
   this.pictureBox1.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("pictureBox1.Dock")));
   this.pictureBox1.Enabled = ((bool)(resources.GetObject("pictureBox1.Enabled")));
   this.pictureBox1.Font = ((System.Drawing.Font)(resources.GetObject("pictureBox1.Font")));
   this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
   this.pictureBox1.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("pictureBox1.ImeMode")));
   this.pictureBox1.Location = ((System.Drawing.Point)(resources.GetObject("pictureBox1.Location")));
   this.pictureBox1.Name = "pictureBox1";
   this.pictureBox1.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("pictureBox1.RightToLeft")));
   this.pictureBox1.Size = ((System.Drawing.Size)(resources.GetObject("pictureBox1.Size")));
   this.pictureBox1.SizeMode = ((System.Windows.Forms.PictureBoxSizeMode)(resources.GetObject("pictureBox1.SizeMode")));
   this.pictureBox1.TabIndex = ((int)(resources.GetObject("pictureBox1.TabIndex")));
   this.pictureBox1.TabStop = false;
   this.pictureBox1.Text = resources.GetString("pictureBox1.Text");
   this.pictureBox1.Visible = ((bool)(resources.GetObject("pictureBox1.Visible")));



   this.richTextBox4.AccessibleDescription = resources.GetString("richTextBox4.AccessibleDescription");
   this.richTextBox4.AccessibleName = resources.GetString("richTextBox4.AccessibleName");
   this.richTextBox4.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("richTextBox4.Anchor")));
   this.richTextBox4.AutoSize = ((bool)(resources.GetObject("richTextBox4.AutoSize")));
   this.richTextBox4.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("richTextBox4.BackgroundImage")));
   this.richTextBox4.BorderStyle = System.Windows.Forms.BorderStyle.None;
   this.richTextBox4.BackColor = System.Drawing.SystemColors.Window;
   this.richTextBox4.BulletIndent = ((int)(resources.GetObject("richTextBox4.BulletIndent")));
   this.richTextBox4.Cursor = System.Windows.Forms.Cursors.Arrow;
   this.richTextBox4.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("richTextBox4.Dock")));
   this.richTextBox4.Enabled = ((bool)(resources.GetObject("richTextBox4.Enabled")));
   this.richTextBox4.Font = ((System.Drawing.Font)(resources.GetObject("richTextBox4.Font")));
   this.richTextBox4.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("richTextBox4.ImeMode")));
   this.richTextBox4.Location = ((System.Drawing.Point)(resources.GetObject("richTextBox4.Location")));
   this.richTextBox4.MaxLength = ((int)(resources.GetObject("richTextBox4.MaxLength")));
   this.richTextBox4.Multiline = ((bool)(resources.GetObject("richTextBox4.Multiline")));
   this.richTextBox4.Name = "richTextBox4";
   this.richTextBox4.ReadOnly = true;
   this.richTextBox4.RightMargin = ((int)(resources.GetObject("richTextBox4.RightMargin")));
   this.richTextBox4.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("richTextBox4.RightToLeft")));
   this.richTextBox4.ScrollBars = ((System.Windows.Forms.RichTextBoxScrollBars)(resources.GetObject("richTextBox4.ScrollBars")));
   this.richTextBox4.Size = ((System.Drawing.Size)(resources.GetObject("richTextBox4.Size")));
   this.richTextBox4.TabIndex = ((int)(resources.GetObject("richTextBox4.TabIndex")));
   this.richTextBox4.TabStop = false;
   this.richTextBox4.Text = resources.GetString("richTextBox4.Text");
   this.richTextBox4.Visible = ((bool)(resources.GetObject("richTextBox4.Visible")));
   this.richTextBox4.WordWrap = ((bool)(resources.GetObject("richTextBox4.WordWrap")));
   this.richTextBox4.ZoomFactor = ((System.Single)(resources.GetObject("richTextBox4.ZoomFactor")));



   this.richTextBox3.AccessibleDescription = resources.GetString("richTextBox3.AccessibleDescription");
   this.richTextBox3.AccessibleName = resources.GetString("richTextBox3.AccessibleName");
   this.richTextBox3.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("richTextBox3.Anchor")));
   this.richTextBox3.AutoSize = ((bool)(resources.GetObject("richTextBox3.AutoSize")));
   this.richTextBox3.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("richTextBox3.BackgroundImage")));
   this.richTextBox3.BackColor = System.Drawing.SystemColors.Window;
   this.richTextBox3.BorderStyle = System.Windows.Forms.BorderStyle.None;
   this.richTextBox3.BulletIndent = ((int)(resources.GetObject("richTextBox3.BulletIndent")));
   this.richTextBox3.Cursor = System.Windows.Forms.Cursors.Default;
   this.richTextBox3.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("richTextBox3.Dock")));
   this.richTextBox3.Enabled = ((bool)(resources.GetObject("richTextBox3.Enabled")));
   this.richTextBox3.Font = ((System.Drawing.Font)(resources.GetObject("richTextBox3.Font")));
   this.richTextBox3.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("richTextBox3.ImeMode")));
   this.richTextBox3.Location = ((System.Drawing.Point)(resources.GetObject("richTextBox3.Location")));
   this.richTextBox3.MaxLength = ((int)(resources.GetObject("richTextBox3.MaxLength")));
   this.richTextBox3.Multiline = ((bool)(resources.GetObject("richTextBox3.Multiline")));
   this.richTextBox3.Name = "richTextBox3";
   this.richTextBox3.ReadOnly = true;
   this.richTextBox3.RightMargin = ((int)(resources.GetObject("richTextBox3.RightMargin")));
   this.richTextBox3.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("richTextBox3.RightToLeft")));
   this.richTextBox3.ScrollBars = ((System.Windows.Forms.RichTextBoxScrollBars)(resources.GetObject("richTextBox3.ScrollBars")));
   this.richTextBox3.Size = ((System.Drawing.Size)(resources.GetObject("richTextBox3.Size")));
   this.richTextBox3.TabIndex = ((int)(resources.GetObject("richTextBox3.TabIndex")));
   this.richTextBox3.TabStop = false;
   this.richTextBox3.Text = resources.GetString("richTextBox3.Text");
   this.richTextBox3.Visible = ((bool)(resources.GetObject("richTextBox3.Visible")));
   this.richTextBox3.WordWrap = ((bool)(resources.GetObject("richTextBox3.WordWrap")));
   this.richTextBox3.ZoomFactor = ((System.Single)(resources.GetObject("richTextBox3.ZoomFactor")));



   this.richTextBox2.AccessibleDescription = resources.GetString("richTextBox2.AccessibleDescription");
   this.richTextBox2.AccessibleName = resources.GetString("richTextBox2.AccessibleName");
   this.richTextBox2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("richTextBox2.Anchor")));
   this.richTextBox2.AutoSize = ((bool)(resources.GetObject("richTextBox2.AutoSize")));
   this.richTextBox2.BackColor = System.Drawing.SystemColors.Window;
   this.richTextBox2.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("richTextBox2.BackgroundImage")));
   this.richTextBox2.BorderStyle = System.Windows.Forms.BorderStyle.None;
   this.richTextBox2.BulletIndent = ((int)(resources.GetObject("richTextBox2.BulletIndent")));
   this.richTextBox2.Cursor = System.Windows.Forms.Cursors.Default;
   this.richTextBox2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("richTextBox2.Dock")));
   this.richTextBox2.Enabled = ((bool)(resources.GetObject("richTextBox2.Enabled")));
   this.richTextBox2.Font = ((System.Drawing.Font)(resources.GetObject("richTextBox2.Font")));
   this.richTextBox2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("richTextBox2.ImeMode")));
   this.richTextBox2.Location = ((System.Drawing.Point)(resources.GetObject("richTextBox2.Location")));
   this.richTextBox2.MaxLength = ((int)(resources.GetObject("richTextBox2.MaxLength")));
   this.richTextBox2.Multiline = ((bool)(resources.GetObject("richTextBox2.Multiline")));
   this.richTextBox2.Name = "richTextBox2";
   this.richTextBox2.ReadOnly = true;
   this.richTextBox2.RightMargin = ((int)(resources.GetObject("richTextBox2.RightMargin")));
   this.richTextBox2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("richTextBox2.RightToLeft")));
   this.richTextBox2.ScrollBars = ((System.Windows.Forms.RichTextBoxScrollBars)(resources.GetObject("richTextBox2.ScrollBars")));
   this.richTextBox2.Size = ((System.Drawing.Size)(resources.GetObject("richTextBox2.Size")));
   this.richTextBox2.TabIndex = ((int)(resources.GetObject("richTextBox2.TabIndex")));
   this.richTextBox2.TabStop = false;
   this.richTextBox2.Text = resources.GetString("richTextBox2.Text");
   this.richTextBox2.Visible = ((bool)(resources.GetObject("richTextBox2.Visible")));
   this.richTextBox2.WordWrap = ((bool)(resources.GetObject("richTextBox2.WordWrap")));
   this.richTextBox2.ZoomFactor = ((System.Single)(resources.GetObject("richTextBox2.ZoomFactor")));



   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackColor = System.Drawing.SystemColors.Window;
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.Controls.Add(this.pictureBox2);
   this.Controls.Add(this.pictureBox1);
   this.Controls.Add(this.richTextBox4);
   this.Controls.Add(this.richTextBox3);
   this.Controls.Add(this.richTextBox2);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.Name = "NoiFolderMessage";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.Size = ((System.Drawing.Size)(resources.GetObject("$this.Size")));
   this.ResumeLayout(false);

  }


 }
}
