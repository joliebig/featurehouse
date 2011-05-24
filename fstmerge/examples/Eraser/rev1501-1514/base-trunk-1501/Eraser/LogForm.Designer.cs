

namespace Eraser
{
 partial class LogForm
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
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(LogForm));
   this.log = new System.Windows.Forms.ListView();
   this.logTimestampColumn = new System.Windows.Forms.ColumnHeader();
   this.logSeverityColumn = new System.Windows.Forms.ColumnHeader();
   this.logMessageColumn = new System.Windows.Forms.ColumnHeader();
   this.logContextMenuStrip = new System.Windows.Forms.ContextMenuStrip(this.components);
   this.copySelectedEntriesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.clear = new System.Windows.Forms.Button();
   this.close = new System.Windows.Forms.Button();
   this.filterSeverityLabel = new System.Windows.Forms.Label();
   this.filterSeverityCombobox = new System.Windows.Forms.ComboBox();
   this.filterFilterTypeCombobox = new System.Windows.Forms.ComboBox();
   this.filterSessionLabel = new System.Windows.Forms.Label();
   this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
   this.filterSessionCombobox = new System.Windows.Forms.ComboBox();
   this.logContextMenuStrip.SuspendLayout();
   this.tableLayoutPanel1.SuspendLayout();
   this.SuspendLayout();



   resources.ApplyResources(this.log, "log");
   this.log.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.logTimestampColumn,
            this.logSeverityColumn,
            this.logMessageColumn});
   this.log.ContextMenuStrip = this.logContextMenuStrip;
   this.log.FullRowSelect = true;
   this.log.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
   this.log.Name = "log";
   this.log.UseCompatibleStateImageBehavior = false;
   this.log.View = System.Windows.Forms.View.Details;
   this.log.VirtualMode = true;
   this.log.ItemActivate += new System.EventHandler(this.log_ItemActivate);
   this.log.VirtualItemsSelectionRangeChanged += new System.Windows.Forms.ListViewVirtualItemsSelectionRangeChangedEventHandler(this.log_VirtualItemsSelectionRangeChanged);
   this.log.RetrieveVirtualItem += new System.Windows.Forms.RetrieveVirtualItemEventHandler(this.log_RetrieveVirtualItem);
   this.log.ItemSelectionChanged += new System.Windows.Forms.ListViewItemSelectionChangedEventHandler(this.log_ItemSelectionChanged);



   resources.ApplyResources(this.logTimestampColumn, "logTimestampColumn");



   resources.ApplyResources(this.logSeverityColumn, "logSeverityColumn");



   resources.ApplyResources(this.logMessageColumn, "logMessageColumn");



   this.logContextMenuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.copySelectedEntriesToolStripMenuItem});
   this.logContextMenuStrip.Name = "logContextMenuStrip";
   resources.ApplyResources(this.logContextMenuStrip, "logContextMenuStrip");
   this.logContextMenuStrip.Opening += new System.ComponentModel.CancelEventHandler(this.logContextMenuStrip_Opening);



   this.copySelectedEntriesToolStripMenuItem.Name = "copySelectedEntriesToolStripMenuItem";
   resources.ApplyResources(this.copySelectedEntriesToolStripMenuItem, "copySelectedEntriesToolStripMenuItem");
   this.copySelectedEntriesToolStripMenuItem.Click += new System.EventHandler(this.copySelectedEntriesToolStripMenuItem_Click);



   resources.ApplyResources(this.clear, "clear");
   this.clear.Name = "clear";
   this.clear.UseVisualStyleBackColor = true;
   this.clear.Click += new System.EventHandler(this.clear_Click);



   resources.ApplyResources(this.close, "close");
   this.close.Name = "close";
   this.close.UseVisualStyleBackColor = true;
   this.close.Click += new System.EventHandler(this.close_Click);



   resources.ApplyResources(this.filterSeverityLabel, "filterSeverityLabel");
   this.filterSeverityLabel.Name = "filterSeverityLabel";



   resources.ApplyResources(this.filterSeverityCombobox, "filterSeverityCombobox");
   this.filterSeverityCombobox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
   this.filterSeverityCombobox.Items.AddRange(new object[] {
            resources.GetString("filterSeverityCombobox.Items"),
            resources.GetString("filterSeverityCombobox.Items1"),
            resources.GetString("filterSeverityCombobox.Items2"),
            resources.GetString("filterSeverityCombobox.Items3"),
            resources.GetString("filterSeverityCombobox.Items4")});
   this.filterSeverityCombobox.Name = "filterSeverityCombobox";
   this.filterSeverityCombobox.SelectedIndexChanged += new System.EventHandler(this.filter_Changed);



   resources.ApplyResources(this.filterFilterTypeCombobox, "filterFilterTypeCombobox");
   this.filterFilterTypeCombobox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
   this.filterFilterTypeCombobox.FormattingEnabled = true;
   this.filterFilterTypeCombobox.Items.AddRange(new object[] {
            resources.GetString("filterFilterTypeCombobox.Items"),
            resources.GetString("filterFilterTypeCombobox.Items1"),
            resources.GetString("filterFilterTypeCombobox.Items2")});
   this.filterFilterTypeCombobox.Name = "filterFilterTypeCombobox";
   this.filterFilterTypeCombobox.SelectedIndexChanged += new System.EventHandler(this.filter_Changed);



   resources.ApplyResources(this.filterSessionLabel, "filterSessionLabel");
   this.filterSessionLabel.Name = "filterSessionLabel";



   resources.ApplyResources(this.tableLayoutPanel1, "tableLayoutPanel1");
   this.tableLayoutPanel1.Controls.Add(this.filterSessionLabel, 0, 0);
   this.tableLayoutPanel1.Controls.Add(this.filterSeverityLabel, 0, 1);
   this.tableLayoutPanel1.Controls.Add(this.filterFilterTypeCombobox, 2, 1);
   this.tableLayoutPanel1.Controls.Add(this.filterSeverityCombobox, 1, 1);
   this.tableLayoutPanel1.Controls.Add(this.filterSessionCombobox, 1, 0);
   this.tableLayoutPanel1.Name = "tableLayoutPanel1";



   this.tableLayoutPanel1.SetColumnSpan(this.filterSessionCombobox, 3);
   resources.ApplyResources(this.filterSessionCombobox, "filterSessionCombobox");
   this.filterSessionCombobox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
   this.filterSessionCombobox.FormattingEnabled = true;
   this.filterSessionCombobox.Name = "filterSessionCombobox";
   this.filterSessionCombobox.SelectedIndexChanged += new System.EventHandler(this.filter_Changed);



   resources.ApplyResources(this, "$this");
   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
   this.Controls.Add(this.tableLayoutPanel1);
   this.Controls.Add(this.close);
   this.Controls.Add(this.log);
   this.Controls.Add(this.clear);
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "LogForm";
   this.ShowIcon = false;
   this.ShowInTaskbar = false;
   this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.LogForm_FormClosed);
   this.logContextMenuStrip.ResumeLayout(false);
   this.tableLayoutPanel1.ResumeLayout(false);
   this.tableLayoutPanel1.PerformLayout();
   this.ResumeLayout(false);

  }



  private System.Windows.Forms.ListView log;
  private System.Windows.Forms.Button clear;
  private System.Windows.Forms.Button close;
  private System.Windows.Forms.ColumnHeader logTimestampColumn;
  private System.Windows.Forms.ColumnHeader logSeverityColumn;
  private System.Windows.Forms.ColumnHeader logMessageColumn;
  private System.Windows.Forms.Label filterSeverityLabel;
  private System.Windows.Forms.ComboBox filterSeverityCombobox;
  private System.Windows.Forms.ComboBox filterFilterTypeCombobox;
  private System.Windows.Forms.Label filterSessionLabel;
  private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
  private System.Windows.Forms.ComboBox filterSessionCombobox;
  private System.Windows.Forms.ContextMenuStrip logContextMenuStrip;
  private System.Windows.Forms.ToolStripMenuItem copySelectedEntriesToolStripMenuItem;
 }
}
