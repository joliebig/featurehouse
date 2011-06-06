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
            this.MainLbl.AccessibleDescription = null;
            this.MainLbl.AccessibleName = null;
            resources.ApplyResources(this.MainLbl, "MainLbl");
            this.MainLbl.Font = null;
            this.MainLbl.Name = "MainLbl";
            this.SubmitBtn.AccessibleDescription = null;
            this.SubmitBtn.AccessibleName = null;
            resources.ApplyResources(this.SubmitBtn, "SubmitBtn");
            this.SubmitBtn.BackgroundImage = null;
            this.SubmitBtn.Font = null;
            this.SubmitBtn.Name = "SubmitBtn";
            this.SubmitBtn.UseVisualStyleBackColor = true;
            this.SubmitBtn.Click += new System.EventHandler(this.SubmitBtn_Click);
            this.PostponeBtn.AccessibleDescription = null;
            this.PostponeBtn.AccessibleName = null;
            resources.ApplyResources(this.PostponeBtn, "PostponeBtn");
            this.PostponeBtn.BackgroundImage = null;
            this.PostponeBtn.Font = null;
            this.PostponeBtn.Name = "PostponeBtn";
            this.PostponeBtn.UseVisualStyleBackColor = true;
            this.PostponeBtn.Click += new System.EventHandler(this.PostponeBtn_Click);
            this.BlackBoxPic.AccessibleDescription = null;
            this.BlackBoxPic.AccessibleName = null;
            resources.ApplyResources(this.BlackBoxPic, "BlackBoxPic");
            this.BlackBoxPic.BackgroundImage = null;
            this.BlackBoxPic.Font = null;
            this.BlackBoxPic.Image = global::Eraser.Properties.Resources.BlackBox;
            this.BlackBoxPic.ImageLocation = null;
            this.BlackBoxPic.Name = "BlackBoxPic";
            this.BlackBoxPic.TabStop = false;
            this.ReportsLv.AccessibleDescription = null;
            this.ReportsLv.AccessibleName = null;
            resources.ApplyResources(this.ReportsLv, "ReportsLv");
            this.ReportsLv.BackgroundImage = null;
            this.ReportsLv.CheckBoxes = true;
            this.ReportsLv.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.ReportsLvTimestampColumn,
            this.ReportsLvErrorColumn});
            this.ReportsLv.Font = null;
            this.ReportsLv.FullRowSelect = true;
            this.ReportsLv.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
            this.ReportsLv.Name = "ReportsLv";
            this.ReportsLv.UseCompatibleStateImageBehavior = false;
            this.ReportsLv.View = System.Windows.Forms.View.Details;
            this.ReportsLv.ItemActivate += new System.EventHandler(this.ReportsLv_ItemActivate);
            resources.ApplyResources(this.ReportsLvTimestampColumn, "ReportsLvTimestampColumn");
            resources.ApplyResources(this.ReportsLvErrorColumn, "ReportsLvErrorColumn");
            this.AccessibleDescription = null;
            this.AccessibleName = null;
            resources.ApplyResources(this, "$this");
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.BackgroundImage = null;
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
