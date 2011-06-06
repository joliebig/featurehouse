namespace Eraser
{
 partial class BasePanel
 {
  private System.ComponentModel.IContainer components = null;
  protected override void Dispose(bool disposing)
  {
   if (disposing && (components != null))
   {
    components.Dispose();
   }
   base.Dispose(disposing);
  }
  private void InitializeComponent()
  {
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(BasePanel));
   this.titleLabel = new System.Windows.Forms.Label();
   this.content = new System.Windows.Forms.Panel();
   this.titleIcon = new System.Windows.Forms.PictureBox();
   ((System.ComponentModel.ISupportInitialize)(this.titleIcon)).BeginInit();
   this.SuspendLayout();
   resources.ApplyResources(this.titleLabel, "titleLabel");
   this.titleLabel.Name = "titleLabel";
   resources.ApplyResources(this.content, "content");
   this.content.Name = "content";
   resources.ApplyResources(this.titleIcon, "titleIcon");
   this.titleIcon.Name = "titleIcon";
   this.titleIcon.TabStop = false;
   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Inherit;
   this.Controls.Add(this.content);
   this.Controls.Add(this.titleIcon);
   this.Controls.Add(this.titleLabel);
   this.Name = "BasePanel";
   resources.ApplyResources(this, "$this");
   ((System.ComponentModel.ISupportInitialize)(this.titleIcon)).EndInit();
   this.ResumeLayout(false);
   this.PerformLayout();
  }
  protected System.Windows.Forms.Label titleLabel;
  protected System.Windows.Forms.PictureBox titleIcon;
  protected System.Windows.Forms.Panel content;
 }
}
