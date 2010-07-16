

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;

using Novell.Win32Util;

namespace Novell.FormsTrayApp
{



 public class About : System.Windows.Forms.Form
 {
  private System.Windows.Forms.PictureBox pictureBox1;
  private System.Windows.Forms.Label label2;
  private System.Windows.Forms.Button credits;
  private System.Windows.Forms.Button close;
  private System.Windows.Forms.Label label3;
  private System.Windows.Forms.Label title;



  private System.ComponentModel.Container components = null;




  public About()
  {



   InitializeComponent();


   System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(About));
   title.Text = string.Format(resourceManager.GetString("title.Text"), Application.ProductVersion);


   centerString(title);


   centerString(label3);
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
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(About));
   this.pictureBox1 = new System.Windows.Forms.PictureBox();
   this.title = new System.Windows.Forms.Label();
   this.label2 = new System.Windows.Forms.Label();
   this.credits = new System.Windows.Forms.Button();
   this.close = new System.Windows.Forms.Button();
   this.label3 = new System.Windows.Forms.Label();
   this.SuspendLayout();



   this.pictureBox1.AccessibleDescription = resources.GetString("pictureBox1.AccessibleDescription");
   this.pictureBox1.AccessibleName = resources.GetString("pictureBox1.AccessibleName");
   this.pictureBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("pictureBox1.Anchor")));
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
   this.pictureBox1.Paint += new System.Windows.Forms.PaintEventHandler(this.pictureBox1_Paint);



   this.title.AccessibleDescription = resources.GetString("title.AccessibleDescription");
   this.title.AccessibleName = resources.GetString("title.AccessibleName");
   this.title.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("title.Anchor")));
   this.title.AutoSize = ((bool)(resources.GetObject("title.AutoSize")));
   this.title.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("title.Dock")));
   this.title.Enabled = ((bool)(resources.GetObject("title.Enabled")));
   this.title.Font = ((System.Drawing.Font)(resources.GetObject("title.Font")));
   this.title.Image = ((System.Drawing.Image)(resources.GetObject("title.Image")));
   this.title.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("title.ImageAlign")));
   this.title.ImageIndex = ((int)(resources.GetObject("title.ImageIndex")));
   this.title.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("title.ImeMode")));
   this.title.Location = ((System.Drawing.Point)(resources.GetObject("title.Location")));
   this.title.Name = "title";
   this.title.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("title.RightToLeft")));
   this.title.Size = ((System.Drawing.Size)(resources.GetObject("title.Size")));
   this.title.TabIndex = ((int)(resources.GetObject("title.TabIndex")));
   this.title.Text = resources.GetString("title.Text");
   this.title.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("title.TextAlign")));
   this.title.Visible = ((bool)(resources.GetObject("title.Visible")));



   this.label2.AccessibleDescription = resources.GetString("label2.AccessibleDescription");
   this.label2.AccessibleName = resources.GetString("label2.AccessibleName");
   this.label2.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label2.Anchor")));
   this.label2.AutoSize = ((bool)(resources.GetObject("label2.AutoSize")));
   this.label2.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label2.Dock")));
   this.label2.Enabled = ((bool)(resources.GetObject("label2.Enabled")));
   this.label2.Font = ((System.Drawing.Font)(resources.GetObject("label2.Font")));
   this.label2.Image = ((System.Drawing.Image)(resources.GetObject("label2.Image")));
   this.label2.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label2.ImageAlign")));
   this.label2.ImageIndex = ((int)(resources.GetObject("label2.ImageIndex")));
   this.label2.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label2.ImeMode")));
   this.label2.Location = ((System.Drawing.Point)(resources.GetObject("label2.Location")));
   this.label2.Name = "label2";
   this.label2.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label2.RightToLeft")));
   this.label2.Size = ((System.Drawing.Size)(resources.GetObject("label2.Size")));
   this.label2.TabIndex = ((int)(resources.GetObject("label2.TabIndex")));
   this.label2.Text = resources.GetString("label2.Text");
   this.label2.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label2.TextAlign")));
   this.label2.Visible = ((bool)(resources.GetObject("label2.Visible")));



   this.credits.AccessibleDescription = resources.GetString("credits.AccessibleDescription");
   this.credits.AccessibleName = resources.GetString("credits.AccessibleName");
   this.credits.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("credits.Anchor")));
   this.credits.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("credits.BackgroundImage")));
   this.credits.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("credits.Dock")));
   this.credits.Enabled = ((bool)(resources.GetObject("credits.Enabled")));
   this.credits.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("credits.FlatStyle")));
   this.credits.Font = ((System.Drawing.Font)(resources.GetObject("credits.Font")));
   this.credits.Image = ((System.Drawing.Image)(resources.GetObject("credits.Image")));
   this.credits.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("credits.ImageAlign")));
   this.credits.ImageIndex = ((int)(resources.GetObject("credits.ImageIndex")));
   this.credits.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("credits.ImeMode")));
   this.credits.Location = ((System.Drawing.Point)(resources.GetObject("credits.Location")));
   this.credits.Name = "credits";
   this.credits.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("credits.RightToLeft")));
   this.credits.Size = ((System.Drawing.Size)(resources.GetObject("credits.Size")));
   this.credits.TabIndex = ((int)(resources.GetObject("credits.TabIndex")));
   this.credits.Text = resources.GetString("credits.Text");
   this.credits.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("credits.TextAlign")));
   this.credits.Visible = ((bool)(resources.GetObject("credits.Visible")));
   this.credits.Click += new System.EventHandler(this.credits_Click);



   this.close.AccessibleDescription = resources.GetString("close.AccessibleDescription");
   this.close.AccessibleName = resources.GetString("close.AccessibleName");
   this.close.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("close.Anchor")));
   this.close.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("close.BackgroundImage")));
   this.close.DialogResult = System.Windows.Forms.DialogResult.OK;
   this.close.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("close.Dock")));
   this.close.Enabled = ((bool)(resources.GetObject("close.Enabled")));
   this.close.FlatStyle = ((System.Windows.Forms.FlatStyle)(resources.GetObject("close.FlatStyle")));
   this.close.Font = ((System.Drawing.Font)(resources.GetObject("close.Font")));
   this.close.Image = ((System.Drawing.Image)(resources.GetObject("close.Image")));
   this.close.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("close.ImageAlign")));
   this.close.ImageIndex = ((int)(resources.GetObject("close.ImageIndex")));
   this.close.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("close.ImeMode")));
   this.close.Location = ((System.Drawing.Point)(resources.GetObject("close.Location")));
   this.close.Name = "close";
   this.close.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("close.RightToLeft")));
   this.close.Size = ((System.Drawing.Size)(resources.GetObject("close.Size")));
   this.close.TabIndex = ((int)(resources.GetObject("close.TabIndex")));
   this.close.Text = resources.GetString("close.Text");
   this.close.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("close.TextAlign")));
   this.close.Visible = ((bool)(resources.GetObject("close.Visible")));



   this.label3.AccessibleDescription = resources.GetString("label3.AccessibleDescription");
   this.label3.AccessibleName = resources.GetString("label3.AccessibleName");
   this.label3.Anchor = ((System.Windows.Forms.AnchorStyles)(resources.GetObject("label3.Anchor")));
   this.label3.AutoSize = ((bool)(resources.GetObject("label3.AutoSize")));
   this.label3.Dock = ((System.Windows.Forms.DockStyle)(resources.GetObject("label3.Dock")));
   this.label3.Enabled = ((bool)(resources.GetObject("label3.Enabled")));
   this.label3.Font = ((System.Drawing.Font)(resources.GetObject("label3.Font")));
   this.label3.Image = ((System.Drawing.Image)(resources.GetObject("label3.Image")));
   this.label3.ImageAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label3.ImageAlign")));
   this.label3.ImageIndex = ((int)(resources.GetObject("label3.ImageIndex")));
   this.label3.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("label3.ImeMode")));
   this.label3.Location = ((System.Drawing.Point)(resources.GetObject("label3.Location")));
   this.label3.Name = "label3";
   this.label3.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("label3.RightToLeft")));
   this.label3.Size = ((System.Drawing.Size)(resources.GetObject("label3.Size")));
   this.label3.TabIndex = ((int)(resources.GetObject("label3.TabIndex")));
   this.label3.Text = resources.GetString("label3.Text");
   this.label3.TextAlign = ((System.Drawing.ContentAlignment)(resources.GetObject("label3.TextAlign")));
   this.label3.Visible = ((bool)(resources.GetObject("label3.Visible")));



   this.AcceptButton = this.close;
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScaleBaseSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScaleBaseSize")));
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.ClientSize = ((System.Drawing.Size)(resources.GetObject("$this.ClientSize")));
   this.Controls.Add(this.label3);
   this.Controls.Add(this.close);
   this.Controls.Add(this.credits);
   this.Controls.Add(this.label2);
   this.Controls.Add(this.title);
   this.Controls.Add(this.pictureBox1);
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.MaximizeBox = false;
   this.MaximumSize = ((System.Drawing.Size)(resources.GetObject("$this.MaximumSize")));
   this.MinimizeBox = false;
   this.MinimumSize = ((System.Drawing.Size)(resources.GetObject("$this.MinimumSize")));
   this.Name = "About";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.ShowInTaskbar = false;
   this.StartPosition = ((System.Windows.Forms.FormStartPosition)(resources.GetObject("$this.StartPosition")));
   this.Text = resources.GetString("$this.Text");
   this.ResumeLayout(false);

  }







  private void centerString(Control label)
  {
   Graphics g = label.CreateGraphics();
   try
   {
    SizeF size = g.MeasureString(label.Text, label.Font);
    label.Width = (int)Math.Ceiling(size.Width);
    label.Left = (ClientRectangle.Width - (int)size.Width) / 2;
   }
   finally
   {
    g.Dispose();
   }
  }






        private void credits_Click(object sender, System.EventArgs e)
  {
   Credits creds = new Credits();
   creds.ShowDialog();
  }

  private void pictureBox1_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
  {
   IntPtr hIcon = Win32Window.LoadImageFromFile(
    0,
    Path.Combine(Application.StartupPath, @"res\ifolder_16.ico"),
    Win32Window.IMAGE_ICON,
    48,
    48,
    Win32Window.LR_LOADFROMFILE);
   Bitmap bmap = Win32Window.IconToAlphaBitmap(Icon.FromHandle(hIcon));
   e.Graphics.DrawImage(bmap, 0, 0);
  }

 }
}
