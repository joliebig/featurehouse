namespace Eraser.DefaultPlugins
{
 partial class SettingsForm
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
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SettingsForm));
   this.customMethodContextMenuStrip = new System.Windows.Forms.ContextMenuStrip(this.components);
   this.deleteMethodToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.okBtn = new System.Windows.Forms.Button();
   this.cancelBtn = new System.Windows.Forms.Button();
   this.errorProvider = new System.Windows.Forms.ErrorProvider(this.components);
   this.customPassName = new System.Windows.Forms.ColumnHeader();
   this.customPassPassCount = new System.Windows.Forms.ColumnHeader();
   this.containerTab = new System.Windows.Forms.TabControl();
   this.containerTabGeneralPnl = new System.Windows.Forms.TabPage();
   this.fl16MethodCmb = new System.Windows.Forms.ComboBox();
   this.fl16MethodLbl = new System.Windows.Forms.Label();
   this.containerTabEraseMethodsPnl = new System.Windows.Forms.TabPage();
   this.customMethodAdd = new System.Windows.Forms.Button();
   this.customMethod = new System.Windows.Forms.ListView();
   this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
   this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
   this.customMethodContextMenuStrip.SuspendLayout();
   ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).BeginInit();
   this.containerTab.SuspendLayout();
   this.containerTabGeneralPnl.SuspendLayout();
   this.containerTabEraseMethodsPnl.SuspendLayout();
   this.SuspendLayout();
   this.customMethodContextMenuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.deleteMethodToolStripMenuItem});
   this.customMethodContextMenuStrip.Name = "customMethodContextMenuStrip";
   resources.ApplyResources(this.customMethodContextMenuStrip, "customMethodContextMenuStrip");
   this.customMethodContextMenuStrip.Opening += new System.ComponentModel.CancelEventHandler(this.customMethodContextMenuStrip_Opening);
   this.deleteMethodToolStripMenuItem.Name = "deleteMethodToolStripMenuItem";
   resources.ApplyResources(this.deleteMethodToolStripMenuItem, "deleteMethodToolStripMenuItem");
   this.deleteMethodToolStripMenuItem.Click += new System.EventHandler(this.deleteMethodToolStripMenuItem_Click);
   resources.ApplyResources(this.okBtn, "okBtn");
   this.okBtn.Name = "okBtn";
   this.okBtn.UseVisualStyleBackColor = true;
   this.okBtn.Click += new System.EventHandler(this.okBtn_Click);
   resources.ApplyResources(this.cancelBtn, "cancelBtn");
   this.cancelBtn.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancelBtn.Name = "cancelBtn";
   this.cancelBtn.UseVisualStyleBackColor = true;
   this.errorProvider.ContainerControl = this;
   resources.ApplyResources(this.customPassName, "customPassName");
   resources.ApplyResources(this.customPassPassCount, "customPassPassCount");
   this.containerTab.Controls.Add(this.containerTabGeneralPnl);
   this.containerTab.Controls.Add(this.containerTabEraseMethodsPnl);
   resources.ApplyResources(this.containerTab, "containerTab");
   this.containerTab.Name = "containerTab";
   this.containerTab.SelectedIndex = 0;
   this.containerTabGeneralPnl.Controls.Add(this.fl16MethodCmb);
   this.containerTabGeneralPnl.Controls.Add(this.fl16MethodLbl);
   resources.ApplyResources(this.containerTabGeneralPnl, "containerTabGeneralPnl");
   this.containerTabGeneralPnl.Name = "containerTabGeneralPnl";
   this.containerTabGeneralPnl.UseVisualStyleBackColor = true;
   resources.ApplyResources(this.fl16MethodCmb, "fl16MethodCmb");
   this.fl16MethodCmb.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
   this.fl16MethodCmb.FormattingEnabled = true;
   this.fl16MethodCmb.Name = "fl16MethodCmb";
   resources.ApplyResources(this.fl16MethodLbl, "fl16MethodLbl");
   this.fl16MethodLbl.Name = "fl16MethodLbl";
   this.containerTabEraseMethodsPnl.Controls.Add(this.customMethodAdd);
   this.containerTabEraseMethodsPnl.Controls.Add(this.customMethod);
   resources.ApplyResources(this.containerTabEraseMethodsPnl, "containerTabEraseMethodsPnl");
   this.containerTabEraseMethodsPnl.Name = "containerTabEraseMethodsPnl";
   this.containerTabEraseMethodsPnl.UseVisualStyleBackColor = true;
   resources.ApplyResources(this.customMethodAdd, "customMethodAdd");
   this.customMethodAdd.Name = "customMethodAdd";
   this.customMethodAdd.UseVisualStyleBackColor = true;
   this.customMethodAdd.Click += new System.EventHandler(this.customMethodAdd_Click);
   resources.ApplyResources(this.customMethod, "customMethod");
   this.customMethod.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader1,
            this.columnHeader2});
   this.customMethod.ContextMenuStrip = this.customMethodContextMenuStrip;
   this.customMethod.FullRowSelect = true;
   this.customMethod.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
   this.customMethod.MultiSelect = false;
   this.customMethod.Name = "customMethod";
   this.customMethod.UseCompatibleStateImageBehavior = false;
   this.customMethod.View = System.Windows.Forms.View.Details;
   this.customMethod.ItemActivate += new System.EventHandler(this.customMethod_ItemActivate);
   resources.ApplyResources(this.columnHeader1, "columnHeader1");
   resources.ApplyResources(this.columnHeader2, "columnHeader2");
   this.AcceptButton = this.okBtn;
   resources.ApplyResources(this, "$this");
   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
   this.CancelButton = this.cancelBtn;
   this.Controls.Add(this.containerTab);
   this.Controls.Add(this.okBtn);
   this.Controls.Add(this.cancelBtn);
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "SettingsForm";
   this.ShowInTaskbar = false;
   this.customMethodContextMenuStrip.ResumeLayout(false);
   ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).EndInit();
   this.containerTab.ResumeLayout(false);
   this.containerTabGeneralPnl.ResumeLayout(false);
   this.containerTabGeneralPnl.PerformLayout();
   this.containerTabEraseMethodsPnl.ResumeLayout(false);
   this.ResumeLayout(false);
  }
  private System.Windows.Forms.Button okBtn;
  private System.Windows.Forms.Button cancelBtn;
  private System.Windows.Forms.ErrorProvider errorProvider;
  private System.Windows.Forms.ContextMenuStrip customMethodContextMenuStrip;
  private System.Windows.Forms.ToolStripMenuItem deleteMethodToolStripMenuItem;
  private System.Windows.Forms.TabControl containerTab;
  private System.Windows.Forms.TabPage containerTabGeneralPnl;
  private System.Windows.Forms.ComboBox fl16MethodCmb;
  private System.Windows.Forms.Label fl16MethodLbl;
  private System.Windows.Forms.TabPage containerTabEraseMethodsPnl;
  private System.Windows.Forms.Button customMethodAdd;
  private System.Windows.Forms.ListView customMethod;
  private System.Windows.Forms.ColumnHeader columnHeader1;
  private System.Windows.Forms.ColumnHeader columnHeader2;
  private System.Windows.Forms.ColumnHeader customPassName;
  private System.Windows.Forms.ColumnHeader customPassPassCount;
 }
}
