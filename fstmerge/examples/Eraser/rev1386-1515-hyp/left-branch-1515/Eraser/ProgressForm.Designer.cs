namespace Eraser
{
 partial class ProgressForm
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
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ProgressForm));
   this.overallProgressLbl = new System.Windows.Forms.Label();
   this.overallProgress = new System.Windows.Forms.ProgressBar();
   this.jobTitle = new System.Windows.Forms.Label();
   this.status = new System.Windows.Forms.Label();
   this.statusLbl = new System.Windows.Forms.Label();
   this.itemLbl = new System.Windows.Forms.Label();
   this.item = new System.Windows.Forms.Label();
   this.passLbl = new System.Windows.Forms.Label();
   this.pass = new System.Windows.Forms.Label();
   this.title = new System.Windows.Forms.PictureBox();
   this.titleLbl = new System.Windows.Forms.Label();
   this.itemProgressLbl = new System.Windows.Forms.Label();
   this.itemProgress = new System.Windows.Forms.ProgressBar();
   this.stop = new System.Windows.Forms.Button();
   this.bevelLine1 = new Trustbridge.Windows.Controls.BevelLine();
   this.bevelLine2 = new Trustbridge.Windows.Controls.BevelLine();
   this.timeLeftLbl = new System.Windows.Forms.Label();
   this.timeLeft = new System.Windows.Forms.Label();
   this.hide = new System.Windows.Forms.Button();
   ((System.ComponentModel.ISupportInitialize)(this.title)).BeginInit();
   this.SuspendLayout();
   resources.ApplyResources(this.overallProgressLbl, "overallProgressLbl");
   this.overallProgressLbl.Name = "overallProgressLbl";
   resources.ApplyResources(this.overallProgress, "overallProgress");
   this.overallProgress.Maximum = 1000;
   this.overallProgress.Name = "overallProgress";
   resources.ApplyResources(this.jobTitle, "jobTitle");
   this.jobTitle.Name = "jobTitle";
   resources.ApplyResources(this.status, "status");
   this.status.Name = "status";
   resources.ApplyResources(this.statusLbl, "statusLbl");
   this.statusLbl.Name = "statusLbl";
   resources.ApplyResources(this.itemLbl, "itemLbl");
   this.itemLbl.Name = "itemLbl";
   resources.ApplyResources(this.item, "item");
   this.item.Name = "item";
   resources.ApplyResources(this.passLbl, "passLbl");
   this.passLbl.Name = "passLbl";
   resources.ApplyResources(this.pass, "pass");
   this.pass.Name = "pass";
   resources.ApplyResources(this.title, "title");
   this.title.Name = "title";
   this.title.TabStop = false;
   resources.ApplyResources(this.titleLbl, "titleLbl");
   this.titleLbl.Name = "titleLbl";
   resources.ApplyResources(this.itemProgressLbl, "itemProgressLbl");
   this.itemProgressLbl.Name = "itemProgressLbl";
   resources.ApplyResources(this.itemProgress, "itemProgress");
   this.itemProgress.MarqueeAnimationSpeed = 75;
   this.itemProgress.Maximum = 1000;
   this.itemProgress.Name = "itemProgress";
   this.itemProgress.Style = System.Windows.Forms.ProgressBarStyle.Marquee;
   resources.ApplyResources(this.stop, "stop");
   this.stop.Name = "stop";
   this.stop.UseVisualStyleBackColor = true;
   this.stop.Click += new System.EventHandler(this.stop_Click);
   this.bevelLine1.Angle = 90;
   resources.ApplyResources(this.bevelLine1, "bevelLine1");
   this.bevelLine1.Name = "bevelLine1";
   this.bevelLine2.Angle = 0;
   resources.ApplyResources(this.bevelLine2, "bevelLine2");
   this.bevelLine2.Name = "bevelLine2";
   this.bevelLine2.Orientation = System.Windows.Forms.Orientation.Vertical;
   resources.ApplyResources(this.timeLeftLbl, "timeLeftLbl");
   this.timeLeftLbl.Name = "timeLeftLbl";
   resources.ApplyResources(this.timeLeft, "timeLeft");
   this.timeLeft.Name = "timeLeft";
   resources.ApplyResources(this.hide, "hide");
   this.hide.Name = "hide";
   this.hide.UseVisualStyleBackColor = true;
   this.hide.Click += new System.EventHandler(this.hide_Click);
   resources.ApplyResources(this, "$this");
   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
   this.Controls.Add(this.hide);
   this.Controls.Add(this.timeLeft);
   this.Controls.Add(this.timeLeftLbl);
   this.Controls.Add(this.bevelLine2);
   this.Controls.Add(this.bevelLine1);
   this.Controls.Add(this.stop);
   this.Controls.Add(this.itemProgress);
   this.Controls.Add(this.itemProgressLbl);
   this.Controls.Add(this.titleLbl);
   this.Controls.Add(this.title);
   this.Controls.Add(this.pass);
   this.Controls.Add(this.passLbl);
   this.Controls.Add(this.item);
   this.Controls.Add(this.itemLbl);
   this.Controls.Add(this.statusLbl);
   this.Controls.Add(this.status);
   this.Controls.Add(this.jobTitle);
   this.Controls.Add(this.overallProgress);
   this.Controls.Add(this.overallProgressLbl);
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "ProgressForm";
   this.ShowInTaskbar = false;
   this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.ProgressForm_FormClosed);
   ((System.ComponentModel.ISupportInitialize)(this.title)).EndInit();
   this.ResumeLayout(false);
   this.PerformLayout();
  }
  private System.Windows.Forms.Label overallProgressLbl;
  private System.Windows.Forms.ProgressBar overallProgress;
  private System.Windows.Forms.Label jobTitle;
  private System.Windows.Forms.Label status;
  private System.Windows.Forms.Label statusLbl;
  private System.Windows.Forms.Label itemLbl;
  private System.Windows.Forms.Label item;
  private System.Windows.Forms.Label passLbl;
  private System.Windows.Forms.Label pass;
  private System.Windows.Forms.PictureBox title;
  private System.Windows.Forms.Label titleLbl;
  private System.Windows.Forms.Label itemProgressLbl;
  private System.Windows.Forms.ProgressBar itemProgress;
  private System.Windows.Forms.Button stop;
  private Trustbridge.Windows.Controls.BevelLine bevelLine1;
  private Trustbridge.Windows.Controls.BevelLine bevelLine2;
  private System.Windows.Forms.Label timeLeftLbl;
  private System.Windows.Forms.Label timeLeft;
  private System.Windows.Forms.Button hide;
 }
}
