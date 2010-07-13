

namespace Eraser
{
 partial class TaskDataSelectionForm
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
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(TaskDataSelectionForm));
   this.file = new System.Windows.Forms.RadioButton();
   this.folder = new System.Windows.Forms.RadioButton();
   this.unused = new System.Windows.Forms.RadioButton();
   this.filePath = new System.Windows.Forms.TextBox();
   this.fileBrowse = new System.Windows.Forms.Button();
   this.folderPath = new System.Windows.Forms.TextBox();
   this.folderBrowse = new System.Windows.Forms.Button();
   this.folderIncludeLbl = new System.Windows.Forms.Label();
   this.folderInclude = new System.Windows.Forms.TextBox();
   this.folderExcludeLbl = new System.Windows.Forms.Label();
   this.folderExclude = new System.Windows.Forms.TextBox();
   this.folderDelete = new System.Windows.Forms.CheckBox();
   this.unusedDisk = new System.Windows.Forms.ComboBox();
   this.methodLbl = new System.Windows.Forms.Label();
   this.method = new System.Windows.Forms.ComboBox();
   this.ok = new System.Windows.Forms.Button();
   this.cancel = new System.Windows.Forms.Button();
   this.fileDialog = new System.Windows.Forms.OpenFileDialog();
   this.folderDialog = new System.Windows.Forms.FolderBrowserDialog();
   this.errorProvider = new System.Windows.Forms.ErrorProvider(this.components);
   this.unusedClusterTips = new System.Windows.Forms.CheckBox();
   this.recycleBin = new System.Windows.Forms.RadioButton();
   ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).BeginInit();
   this.SuspendLayout();



   resources.ApplyResources(this.file, "file");
   this.file.Name = "file";
   this.file.TabStop = true;
   this.file.UseVisualStyleBackColor = true;
   this.file.CheckedChanged += new System.EventHandler(this.data_CheckedChanged);



   resources.ApplyResources(this.folder, "folder");
   this.folder.Name = "folder";
   this.folder.TabStop = true;
   this.folder.UseVisualStyleBackColor = true;
   this.folder.CheckedChanged += new System.EventHandler(this.data_CheckedChanged);



   resources.ApplyResources(this.unused, "unused");
   this.unused.Name = "unused";
   this.unused.TabStop = true;
   this.unused.UseVisualStyleBackColor = true;
   this.unused.CheckedChanged += new System.EventHandler(this.data_CheckedChanged);



   resources.ApplyResources(this.filePath, "filePath");
   this.filePath.Name = "filePath";



   resources.ApplyResources(this.fileBrowse, "fileBrowse");
   this.fileBrowse.Name = "fileBrowse";
   this.fileBrowse.UseVisualStyleBackColor = true;
   this.fileBrowse.Click += new System.EventHandler(this.fileBrowse_Click);



   resources.ApplyResources(this.folderPath, "folderPath");
   this.folderPath.Name = "folderPath";



   resources.ApplyResources(this.folderBrowse, "folderBrowse");
   this.folderBrowse.Name = "folderBrowse";
   this.folderBrowse.UseVisualStyleBackColor = true;
   this.folderBrowse.Click += new System.EventHandler(this.folderBrowse_Click);



   resources.ApplyResources(this.folderIncludeLbl, "folderIncludeLbl");
   this.folderIncludeLbl.Name = "folderIncludeLbl";



   resources.ApplyResources(this.folderInclude, "folderInclude");
   this.folderInclude.Name = "folderInclude";



   resources.ApplyResources(this.folderExcludeLbl, "folderExcludeLbl");
   this.folderExcludeLbl.Name = "folderExcludeLbl";



   resources.ApplyResources(this.folderExclude, "folderExclude");
   this.folderExclude.Name = "folderExclude";



   resources.ApplyResources(this.folderDelete, "folderDelete");
   this.folderDelete.Checked = true;
   this.folderDelete.CheckState = System.Windows.Forms.CheckState.Checked;
   this.folderDelete.Name = "folderDelete";
   this.folderDelete.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.unusedDisk, "unusedDisk");
   this.unusedDisk.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawFixed;
   this.unusedDisk.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
   this.unusedDisk.FormattingEnabled = true;
   this.unusedDisk.Name = "unusedDisk";
   this.unusedDisk.DrawItem += new System.Windows.Forms.DrawItemEventHandler(this.unusedDisk_DrawItem);



   resources.ApplyResources(this.methodLbl, "methodLbl");
   this.methodLbl.Name = "methodLbl";



   resources.ApplyResources(this.method, "method");
   this.method.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
   this.method.FormattingEnabled = true;
   this.method.Name = "method";
   this.method.SelectedIndexChanged += new System.EventHandler(this.method_SelectedIndexChanged);



   resources.ApplyResources(this.ok, "ok");
   this.ok.Name = "ok";
   this.ok.UseVisualStyleBackColor = true;
   this.ok.Click += new System.EventHandler(this.ok_Click);



   resources.ApplyResources(this.cancel, "cancel");
   this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancel.Name = "cancel";
   this.cancel.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.fileDialog, "fileDialog");



   resources.ApplyResources(this.folderDialog, "folderDialog");
   this.folderDialog.ShowNewFolderButton = false;



   this.errorProvider.ContainerControl = this;



   resources.ApplyResources(this.unusedClusterTips, "unusedClusterTips");
   this.unusedClusterTips.Checked = true;
   this.unusedClusterTips.CheckState = System.Windows.Forms.CheckState.Checked;
   this.unusedClusterTips.Name = "unusedClusterTips";
   this.unusedClusterTips.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.recycleBin, "recycleBin");
   this.recycleBin.Name = "recycleBin";
   this.recycleBin.TabStop = true;
   this.recycleBin.UseVisualStyleBackColor = true;
   this.recycleBin.CheckedChanged += new System.EventHandler(this.data_CheckedChanged);



   this.AcceptButton = this.ok;
   resources.ApplyResources(this, "$this");
   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
   this.CancelButton = this.cancel;
   this.Controls.Add(this.recycleBin);
   this.Controls.Add(this.unusedClusterTips);
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.ok);
   this.Controls.Add(this.method);
   this.Controls.Add(this.methodLbl);
   this.Controls.Add(this.unusedDisk);
   this.Controls.Add(this.folderDelete);
   this.Controls.Add(this.folderExclude);
   this.Controls.Add(this.folderExcludeLbl);
   this.Controls.Add(this.folderInclude);
   this.Controls.Add(this.folderIncludeLbl);
   this.Controls.Add(this.folderBrowse);
   this.Controls.Add(this.folderPath);
   this.Controls.Add(this.fileBrowse);
   this.Controls.Add(this.filePath);
   this.Controls.Add(this.unused);
   this.Controls.Add(this.folder);
   this.Controls.Add(this.file);
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "TaskDataSelectionForm";
   this.ShowInTaskbar = false;
   ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).EndInit();
   this.ResumeLayout(false);
   this.PerformLayout();

  }



  private System.Windows.Forms.RadioButton file;
  private System.Windows.Forms.RadioButton folder;
  private System.Windows.Forms.RadioButton unused;
  private System.Windows.Forms.TextBox filePath;
  private System.Windows.Forms.Button fileBrowse;
  private System.Windows.Forms.TextBox folderPath;
  private System.Windows.Forms.Button folderBrowse;
  private System.Windows.Forms.Label folderIncludeLbl;
  private System.Windows.Forms.TextBox folderInclude;
  private System.Windows.Forms.Label folderExcludeLbl;
  private System.Windows.Forms.TextBox folderExclude;
  private System.Windows.Forms.CheckBox folderDelete;
  private System.Windows.Forms.ComboBox unusedDisk;
  private System.Windows.Forms.Label methodLbl;
  private System.Windows.Forms.ComboBox method;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button cancel;
  private System.Windows.Forms.OpenFileDialog fileDialog;
  private System.Windows.Forms.FolderBrowserDialog folderDialog;
  private System.Windows.Forms.ErrorProvider errorProvider;
  private System.Windows.Forms.CheckBox unusedClusterTips;
  private System.Windows.Forms.RadioButton recycleBin;
 }
}
