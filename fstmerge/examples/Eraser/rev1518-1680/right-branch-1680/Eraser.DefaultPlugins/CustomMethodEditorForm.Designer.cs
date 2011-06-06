namespace Eraser.DefaultPlugins
{
 partial class CustomMethodEditorForm
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
   this.components = new System.ComponentModel.Container();
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CustomMethodEditorForm));
   this.nameLbl = new System.Windows.Forms.Label();
   this.nameTxt = new System.Windows.Forms.TextBox();
   this.passesLv = new System.Windows.Forms.ListView();
   this.passesColNumber = new System.Windows.Forms.ColumnHeader();
   this.passesColType = new System.Windows.Forms.ColumnHeader();
   this.passesAddBtn = new System.Windows.Forms.Button();
   this.passesRemoveBtn = new System.Windows.Forms.Button();
   this.passesDuplicateBtn = new System.Windows.Forms.Button();
   this.passGrp = new System.Windows.Forms.GroupBox();
   this.randomizeChk = new System.Windows.Forms.CheckBox();
   this.okBtn = new System.Windows.Forms.Button();
   this.cancelBtn = new System.Windows.Forms.Button();
   this.errorProvider = new System.Windows.Forms.ErrorProvider(this.components);
   this.passEditor = new Eraser.DefaultPlugins.CustomMethodPassEditor();
   this.passGrp.SuspendLayout();
   ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).BeginInit();
   this.SuspendLayout();
   resources.ApplyResources(this.nameLbl, "nameLbl");
   this.nameLbl.Name = "nameLbl";
   resources.ApplyResources(this.nameTxt, "nameTxt");
   this.nameTxt.Name = "nameTxt";
   this.passesLv.AllowDrop = true;
   resources.ApplyResources(this.passesLv, "passesLv");
   this.passesLv.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.passesColNumber,
            this.passesColType});
   this.passesLv.FullRowSelect = true;
   this.passesLv.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
   this.passesLv.HideSelection = false;
   this.passesLv.Name = "passesLv";
   this.passesLv.UseCompatibleStateImageBehavior = false;
   this.passesLv.View = System.Windows.Forms.View.Details;
   this.passesLv.DragDrop += new System.Windows.Forms.DragEventHandler(this.passesLv_DragDrop);
   this.passesLv.ItemSelectionChanged += new System.Windows.Forms.ListViewItemSelectionChangedEventHandler(this.passesLv_ItemSelectionChanged);
   this.passesLv.DragEnter += new System.Windows.Forms.DragEventHandler(this.passesLv_DragEnter);
   this.passesLv.GiveFeedback += new System.Windows.Forms.GiveFeedbackEventHandler(this.passesLv_GiveFeedback);
   this.passesLv.ItemDrag += new System.Windows.Forms.ItemDragEventHandler(this.passesLv_ItemDrag);
   resources.ApplyResources(this.passesColNumber, "passesColNumber");
   resources.ApplyResources(this.passesColType, "passesColType");
   resources.ApplyResources(this.passesAddBtn, "passesAddBtn");
   this.passesAddBtn.Name = "passesAddBtn";
   this.passesAddBtn.UseVisualStyleBackColor = true;
   this.passesAddBtn.Click += new System.EventHandler(this.passesAddBtn_Click);
   resources.ApplyResources(this.passesRemoveBtn, "passesRemoveBtn");
   this.passesRemoveBtn.Name = "passesRemoveBtn";
   this.passesRemoveBtn.UseVisualStyleBackColor = true;
   this.passesRemoveBtn.Click += new System.EventHandler(this.passesRemoveBtn_Click);
   resources.ApplyResources(this.passesDuplicateBtn, "passesDuplicateBtn");
   this.passesDuplicateBtn.Name = "passesDuplicateBtn";
   this.passesDuplicateBtn.UseVisualStyleBackColor = true;
   this.passesDuplicateBtn.Click += new System.EventHandler(this.passesDuplicateBtn_Click);
   resources.ApplyResources(this.passGrp, "passGrp");
   this.passGrp.Controls.Add(this.passEditor);
   this.passGrp.Name = "passGrp";
   this.passGrp.TabStop = false;
   resources.ApplyResources(this.randomizeChk, "randomizeChk");
   this.randomizeChk.Name = "randomizeChk";
   this.randomizeChk.UseVisualStyleBackColor = true;
   resources.ApplyResources(this.okBtn, "okBtn");
   this.okBtn.Name = "okBtn";
   this.okBtn.UseVisualStyleBackColor = true;
   this.okBtn.Click += new System.EventHandler(this.okBtn_Click);
   resources.ApplyResources(this.cancelBtn, "cancelBtn");
   this.cancelBtn.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancelBtn.Name = "cancelBtn";
   this.cancelBtn.UseVisualStyleBackColor = true;
   this.errorProvider.ContainerControl = this;
   resources.ApplyResources(this.passEditor, "passEditor");
   this.passEditor.Name = "passEditor";
   this.passEditor.PassData = null;
   this.passEditor.PassType = Eraser.DefaultPlugins.CustomMethodPassEditorPassType.Text;
   this.AcceptButton = this.okBtn;
   resources.ApplyResources(this, "$this");
   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
   this.CancelButton = this.cancelBtn;
   this.Controls.Add(this.okBtn);
   this.Controls.Add(this.cancelBtn);
   this.Controls.Add(this.randomizeChk);
   this.Controls.Add(this.passGrp);
   this.Controls.Add(this.passesDuplicateBtn);
   this.Controls.Add(this.passesRemoveBtn);
   this.Controls.Add(this.passesAddBtn);
   this.Controls.Add(this.passesLv);
   this.Controls.Add(this.nameTxt);
   this.Controls.Add(this.nameLbl);
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "CustomMethodEditorForm";
   this.ShowInTaskbar = false;
   this.passGrp.ResumeLayout(false);
   ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).EndInit();
   this.ResumeLayout(false);
   this.PerformLayout();
  }
  private System.Windows.Forms.Label nameLbl;
  private System.Windows.Forms.TextBox nameTxt;
  private System.Windows.Forms.ListView passesLv;
  private System.Windows.Forms.Button passesAddBtn;
  private System.Windows.Forms.Button passesRemoveBtn;
  private System.Windows.Forms.Button passesDuplicateBtn;
  private System.Windows.Forms.GroupBox passGrp;
  private System.Windows.Forms.CheckBox randomizeChk;
  private System.Windows.Forms.ColumnHeader passesColNumber;
  private System.Windows.Forms.ColumnHeader passesColType;
  private System.Windows.Forms.Button okBtn;
  private System.Windows.Forms.Button cancelBtn;
  private System.Windows.Forms.ErrorProvider errorProvider;
  private CustomMethodPassEditor passEditor;
 }
}
