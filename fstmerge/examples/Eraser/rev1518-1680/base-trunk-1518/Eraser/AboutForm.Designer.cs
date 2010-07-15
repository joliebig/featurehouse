

namespace Eraser
{
 partial class AboutForm
 {



  private System.ComponentModel.IContainer components = null;





  protected override void Dispose(bool disposing)
  {
   if (disposing && (components != null))
   {
    components.Dispose();
   }

   ParentBitmap.Dispose();
   AboutBitmap.Dispose();
   AboutTextBitmap.Dispose();
   DoubleBufferBitmap.Dispose();
   base.Dispose(disposing);
  }







  private void InitializeComponent()
  {
   this.components = new System.ComponentModel.Container();
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AboutForm));
   this.animationTimer = new System.Windows.Forms.Timer(this.components);
   this.SuspendLayout();



   this.animationTimer.Enabled = true;
   this.animationTimer.Interval = 50;
   this.animationTimer.Tick += new System.EventHandler(this.animationTimer_Tick);



   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Inherit;
   resources.ApplyResources(this, "$this");
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None;
   this.Name = "AboutForm";
   this.ShowInTaskbar = false;
   this.Click += new System.EventHandler(this.AboutForm_Click);
   this.Paint += new System.Windows.Forms.PaintEventHandler(this.AboutForm_Paint);
   this.MouseDown += new System.Windows.Forms.MouseEventHandler(this.AboutForm_MouseDown);
   this.MouseLeave += new System.EventHandler(this.AboutForm_MouseLeave);
   this.MouseMove += new System.Windows.Forms.MouseEventHandler(this.AboutForm_MouseMove);
   this.MouseUp += new System.Windows.Forms.MouseEventHandler(this.AboutForm_MouseUp);
   this.ResumeLayout(false);

  }



  private System.Windows.Forms.Timer animationTimer;
 }
}
