namespace Eraser
{
 partial class BlackBoxMainForm
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
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(BlackBoxMainForm));
   this.MainLbl = new System.Windows.Forms.Label();
   this.SubmitBtn = new System.Windows.Forms.Button();
   this.PostponeBtn = new System.Windows.Forms.Button();
   this.BlackBoxPic = new System.Windows.Forms.PictureBox();
   this.ReportsLv = new System.Windows.Forms.ListView();
   this.ReportsLvTimestampColumn = new System.Windows.Forms.ColumnHeader();
   this.ReportsLvErrorColumn = new System.Windows.Forms.ColumnHeader();
   ((System.ComponentModel.ISupportInitialize)(this.BlackBoxPic)).BeginInit();
   this.SuspendLayout();
   resources.ApplyResources(this.MainLbl, "MainLbl");
   this.MainLbl.Name = "MainLbl";
   resources.ApplyResources(this.SubmitBtn, "SubmitBtn");
   this.SubmitBtn.Name = "SubmitBtn";
   this.SubmitBtn.UseVisualStyleBackColor = true;
   this.SubmitBtn.Click += new System.EventHandler(this.SubmitBtn_Click);
   resources.ApplyResources(this.PostponeBtn, "PostponeBtn");
   this.PostponeBtn.Name = "PostponeBtn";
   this.PostponeBtn.UseVisualStyleBackColor = true;
   this.PostponeBtn.Click += new System.EventHandler(this.PostponeBtn_Click);
   this.BlackBoxPic.Image = global::Eraser.Properties.Resources.BlackBox;
   resources.ApplyResources(this.BlackBoxPic, "BlackBoxPic");
   this.BlackBoxPic.Name = "BlackBoxPic";
   this.BlackBoxPic.TabStop = false;
   resources.ApplyResources(this.ReportsLv, "ReportsLv");
   this.ReportsLv.CheckBoxes = true;
   this.ReportsLv.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.ReportsLvTimestampColumn,
            this.ReportsLvErrorColumn});
   this.ReportsLv.FullRowSelect = true;
   this.ReportsLv.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
   this.ReportsLv.Name = "ReportsLv";
   this.ReportsLv.UseCompatibleStateImageBehavior = false;
   this.ReportsLv.View = System.Windows.Forms.View.Details;
   this.ReportsLv.ItemActivate += new System.EventHandler(this.ReportsLv_ItemActivate);
   resources.ApplyResources(this.ReportsLvTimestampColumn, "ReportsLvTimestampColumn");
   resources.ApplyResources(this.ReportsLvErrorColumn, "ReportsLvErrorColumn");
   resources.ApplyResources(this, "$this");
   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
   this.Controls.Add(this.ReportsLv);
   this.Controls.Add(this.BlackBoxPic);
   this.Controls.Add(this.PostponeBtn);
   this.Controls.Add(this.SubmitBtn);
   this.Controls.Add(this.MainLbl);
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "BlackBoxMainForm";
   this.ShowInTaskbar = false;
   this.TopMost = true;
   ((System.ComponentModel.ISupportInitialize)(this.BlackBoxPic)).EndInit();
   this.ResumeLayout(false);
  }
  private System.Windows.Forms.Label MainLbl;
  private System.Windows.Forms.Button SubmitBtn;
  private System.Windows.Forms.Button PostponeBtn;
  private System.Windows.Forms.PictureBox BlackBoxPic;
  private System.Windows.Forms.ListView ReportsLv;
  private System.Windows.Forms.ColumnHeader ReportsLvTimestampColumn;
  private System.Windows.Forms.ColumnHeader ReportsLvErrorColumn;
 }
}
