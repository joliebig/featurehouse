namespace Eraser
{
 partial class LightGroup
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
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(LightGroup));
   this.container = new System.Windows.Forms.TableLayoutPanel();
   this.separator = new Trustbridge.Windows.Controls.BevelLine();
   this.label = new System.Windows.Forms.Label();
   this.container.SuspendLayout();
   this.SuspendLayout();
   resources.ApplyResources(this.container, "container");
   this.container.Controls.Add(this.separator, 1, 0);
   this.container.Controls.Add(this.label, 0, 0);
   this.container.Name = "container";
   resources.ApplyResources(this.separator, "separator");
   this.separator.Angle = 90;
   this.separator.Name = "separator";
   resources.ApplyResources(this.label, "label");
   this.label.Name = "label";
   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Inherit;
   resources.ApplyResources(this, "$this");
   this.Controls.Add(this.container);
   this.Name = "LightGroup";
   this.container.ResumeLayout(false);
   this.container.PerformLayout();
   this.ResumeLayout(false);
   this.PerformLayout();
  }
  private System.Windows.Forms.TableLayoutPanel container;
        private Trustbridge.Windows.Controls.BevelLine separator;
  private System.Windows.Forms.Label label;
 }
}
