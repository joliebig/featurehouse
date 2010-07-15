

namespace Eraser
{
 partial class TaskPropertiesForm
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
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(TaskPropertiesForm));
   this.nameLbl = new System.Windows.Forms.Label();
   this.name = new System.Windows.Forms.TextBox();
   this.eraseLbl = new System.Windows.Forms.Label();
   this.typeLbl = new System.Windows.Forms.Label();
   this.typeImmediate = new System.Windows.Forms.RadioButton();
   this.typeRecurring = new System.Windows.Forms.RadioButton();
   this.data = new System.Windows.Forms.ListView();
   this.dataColData = new System.Windows.Forms.ColumnHeader();
   this.dataColMethod = new System.Windows.Forms.ColumnHeader();
   this.dataContextMenuStrip = new System.Windows.Forms.ContextMenuStrip(this.components);
   this.deleteDataToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.dataAdd = new System.Windows.Forms.Button();
   this.ok = new System.Windows.Forms.Button();
   this.cancel = new System.Windows.Forms.Button();
   this.container = new System.Windows.Forms.TabControl();
   this.containerTask = new System.Windows.Forms.TabPage();
   this.typeManual = new System.Windows.Forms.RadioButton();
   this.typeRestart = new System.Windows.Forms.RadioButton();
   this.containerSchedule = new System.Windows.Forms.TabPage();
   this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
   this.schedulePattern = new System.Windows.Forms.GroupBox();
   this.scheduleMonthlyLbl = new System.Windows.Forms.Label();
   this.scheduleMonthlyDayNumber = new System.Windows.Forms.NumericUpDown();
   this.scheduleMonthlyFreq = new System.Windows.Forms.NumericUpDown();
   this.scheduleMonthlyMonthLbl = new System.Windows.Forms.Label();
   this.scheduleMonthlyEveryLbl = new System.Windows.Forms.Label();
   this.scheduleWeeklyFreq = new System.Windows.Forms.NumericUpDown();
   this.scheduleDaily = new System.Windows.Forms.RadioButton();
   this.scheduleDailyPanel = new System.Windows.Forms.Panel();
   this.scheduleDailyByDayFreq = new System.Windows.Forms.NumericUpDown();
   this.scheduleDailyByDay = new System.Windows.Forms.RadioButton();
   this.scheduleDailyByDayLbl = new System.Windows.Forms.Label();
   this.scheduleDailyByWeekday = new System.Windows.Forms.RadioButton();
   this.scheduleWeeklyDays = new System.Windows.Forms.FlowLayoutPanel();
   this.scheduleWeeklyMonday = new System.Windows.Forms.CheckBox();
   this.scheduleWeeklyTuesday = new System.Windows.Forms.CheckBox();
   this.scheduleWeeklyWednesday = new System.Windows.Forms.CheckBox();
   this.scheduleWeeklyThursday = new System.Windows.Forms.CheckBox();
   this.scheduleWeeklyFriday = new System.Windows.Forms.CheckBox();
   this.scheduleWeeklySaturday = new System.Windows.Forms.CheckBox();
   this.scheduleWeeklySunday = new System.Windows.Forms.CheckBox();
   this.scheduleWeeklyFreqLbl = new System.Windows.Forms.Label();
   this.scheduleWeeklyLbl = new System.Windows.Forms.Label();
   this.scheduleWeekly = new System.Windows.Forms.RadioButton();
   this.scheduleMonthly = new System.Windows.Forms.RadioButton();
   this.nonRecurringPanel = new System.Windows.Forms.Panel();
   this.nonRecurringLbl = new System.Windows.Forms.Label();
   this.nonRecurringBitmap = new System.Windows.Forms.PictureBox();
   this.scheduleTimePanel = new System.Windows.Forms.Panel();
   this.scheduleTime = new System.Windows.Forms.DateTimePicker();
   this.scheduleTimeLbl = new System.Windows.Forms.Label();
   this.errorProvider = new System.Windows.Forms.ErrorProvider(this.components);
   this.dataContextMenuStrip.SuspendLayout();
   this.container.SuspendLayout();
   this.containerTask.SuspendLayout();
   this.containerSchedule.SuspendLayout();
   this.tableLayoutPanel1.SuspendLayout();
   this.schedulePattern.SuspendLayout();
   ((System.ComponentModel.ISupportInitialize)(this.scheduleMonthlyDayNumber)).BeginInit();
   ((System.ComponentModel.ISupportInitialize)(this.scheduleMonthlyFreq)).BeginInit();
   ((System.ComponentModel.ISupportInitialize)(this.scheduleWeeklyFreq)).BeginInit();
   this.scheduleDailyPanel.SuspendLayout();
   ((System.ComponentModel.ISupportInitialize)(this.scheduleDailyByDayFreq)).BeginInit();
   this.scheduleWeeklyDays.SuspendLayout();
   this.nonRecurringPanel.SuspendLayout();
   ((System.ComponentModel.ISupportInitialize)(this.nonRecurringBitmap)).BeginInit();
   this.scheduleTimePanel.SuspendLayout();
   ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).BeginInit();
   this.SuspendLayout();



   resources.ApplyResources(this.nameLbl, "nameLbl");
   this.nameLbl.Name = "nameLbl";



   resources.ApplyResources(this.name, "name");
   this.name.Name = "name";



   resources.ApplyResources(this.eraseLbl, "eraseLbl");
   this.eraseLbl.Name = "eraseLbl";



   resources.ApplyResources(this.typeLbl, "typeLbl");
   this.typeLbl.Name = "typeLbl";



   resources.ApplyResources(this.typeImmediate, "typeImmediate");
   this.typeImmediate.Name = "typeImmediate";
   this.typeImmediate.UseVisualStyleBackColor = true;
   this.typeImmediate.CheckedChanged += new System.EventHandler(this.taskType_CheckedChanged);



   resources.ApplyResources(this.typeRecurring, "typeRecurring");
   this.typeRecurring.Name = "typeRecurring";
   this.typeRecurring.UseVisualStyleBackColor = true;
   this.typeRecurring.CheckedChanged += new System.EventHandler(this.taskType_CheckedChanged);



   resources.ApplyResources(this.data, "data");
   this.data.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.dataColData,
            this.dataColMethod});
   this.data.ContextMenuStrip = this.dataContextMenuStrip;
   this.data.FullRowSelect = true;
   this.data.MultiSelect = false;
   this.data.Name = "data";
   this.data.UseCompatibleStateImageBehavior = false;
   this.data.View = System.Windows.Forms.View.Details;
   this.data.ItemActivate += new System.EventHandler(this.data_ItemActivate);



   resources.ApplyResources(this.dataColData, "dataColData");



   resources.ApplyResources(this.dataColMethod, "dataColMethod");



   this.dataContextMenuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.deleteDataToolStripMenuItem});
   this.dataContextMenuStrip.Name = "dataContextMenuStrip";
   resources.ApplyResources(this.dataContextMenuStrip, "dataContextMenuStrip");
   this.dataContextMenuStrip.Opening += new System.ComponentModel.CancelEventHandler(this.dataContextMenuStrip_Opening);



   this.deleteDataToolStripMenuItem.Name = "deleteDataToolStripMenuItem";
   resources.ApplyResources(this.deleteDataToolStripMenuItem, "deleteDataToolStripMenuItem");
   this.deleteDataToolStripMenuItem.Click += new System.EventHandler(this.deleteDataToolStripMenuItem_Click);



   resources.ApplyResources(this.dataAdd, "dataAdd");
   this.dataAdd.Name = "dataAdd";
   this.dataAdd.UseVisualStyleBackColor = true;
   this.dataAdd.Click += new System.EventHandler(this.dataAdd_Click);



   resources.ApplyResources(this.ok, "ok");
   this.ok.Name = "ok";
   this.ok.UseVisualStyleBackColor = true;
   this.ok.Click += new System.EventHandler(this.ok_Click);



   resources.ApplyResources(this.cancel, "cancel");
   this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
   this.cancel.Name = "cancel";
   this.cancel.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.container, "container");
   this.container.Controls.Add(this.containerTask);
   this.container.Controls.Add(this.containerSchedule);
   this.container.Name = "container";
   this.container.SelectedIndex = 0;



   this.containerTask.Controls.Add(this.typeManual);
   this.containerTask.Controls.Add(this.typeRestart);
   this.containerTask.Controls.Add(this.nameLbl);
   this.containerTask.Controls.Add(this.name);
   this.containerTask.Controls.Add(this.typeLbl);
   this.containerTask.Controls.Add(this.typeImmediate);
   this.containerTask.Controls.Add(this.typeRecurring);
   this.containerTask.Controls.Add(this.eraseLbl);
   this.containerTask.Controls.Add(this.data);
   this.containerTask.Controls.Add(this.dataAdd);
   resources.ApplyResources(this.containerTask, "containerTask");
   this.containerTask.Name = "containerTask";
   this.containerTask.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.typeManual, "typeManual");
   this.typeManual.Name = "typeManual";
   this.typeManual.TabStop = true;
   this.typeManual.UseVisualStyleBackColor = true;
   this.typeManual.CheckedChanged += new System.EventHandler(this.taskType_CheckedChanged);



   resources.ApplyResources(this.typeRestart, "typeRestart");
   this.typeRestart.Name = "typeRestart";
   this.typeRestart.TabStop = true;
   this.typeRestart.UseVisualStyleBackColor = true;



   this.containerSchedule.Controls.Add(this.tableLayoutPanel1);
   resources.ApplyResources(this.containerSchedule, "containerSchedule");
   this.containerSchedule.Name = "containerSchedule";
   this.containerSchedule.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.tableLayoutPanel1, "tableLayoutPanel1");
   this.tableLayoutPanel1.Controls.Add(this.schedulePattern, 0, 2);
   this.tableLayoutPanel1.Controls.Add(this.nonRecurringPanel, 0, 0);
   this.tableLayoutPanel1.Controls.Add(this.scheduleTimePanel, 0, 1);
   this.tableLayoutPanel1.Name = "tableLayoutPanel1";



   resources.ApplyResources(this.schedulePattern, "schedulePattern");
   this.schedulePattern.Controls.Add(this.scheduleMonthlyLbl);
   this.schedulePattern.Controls.Add(this.scheduleMonthlyDayNumber);
   this.schedulePattern.Controls.Add(this.scheduleMonthlyFreq);
   this.schedulePattern.Controls.Add(this.scheduleMonthlyMonthLbl);
   this.schedulePattern.Controls.Add(this.scheduleMonthlyEveryLbl);
   this.schedulePattern.Controls.Add(this.scheduleWeeklyFreq);
   this.schedulePattern.Controls.Add(this.scheduleDaily);
   this.schedulePattern.Controls.Add(this.scheduleDailyPanel);
   this.schedulePattern.Controls.Add(this.scheduleWeeklyDays);
   this.schedulePattern.Controls.Add(this.scheduleWeeklyFreqLbl);
   this.schedulePattern.Controls.Add(this.scheduleWeeklyLbl);
   this.schedulePattern.Controls.Add(this.scheduleWeekly);
   this.schedulePattern.Controls.Add(this.scheduleMonthly);
   this.schedulePattern.Name = "schedulePattern";
   this.schedulePattern.TabStop = false;



   resources.ApplyResources(this.scheduleMonthlyLbl, "scheduleMonthlyLbl");
   this.scheduleMonthlyLbl.Name = "scheduleMonthlyLbl";



   resources.ApplyResources(this.scheduleMonthlyDayNumber, "scheduleMonthlyDayNumber");
   this.scheduleMonthlyDayNumber.Maximum = new decimal(new int[] {
            31,
            0,
            0,
            0});
   this.scheduleMonthlyDayNumber.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
   this.scheduleMonthlyDayNumber.Name = "scheduleMonthlyDayNumber";
   this.scheduleMonthlyDayNumber.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});



   resources.ApplyResources(this.scheduleMonthlyFreq, "scheduleMonthlyFreq");
   this.scheduleMonthlyFreq.Maximum = new decimal(new int[] {
            120,
            0,
            0,
            0});
   this.scheduleMonthlyFreq.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
   this.scheduleMonthlyFreq.Name = "scheduleMonthlyFreq";
   this.scheduleMonthlyFreq.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});



   resources.ApplyResources(this.scheduleMonthlyMonthLbl, "scheduleMonthlyMonthLbl");
   this.scheduleMonthlyMonthLbl.Name = "scheduleMonthlyMonthLbl";



   resources.ApplyResources(this.scheduleMonthlyEveryLbl, "scheduleMonthlyEveryLbl");
   this.scheduleMonthlyEveryLbl.Name = "scheduleMonthlyEveryLbl";



   resources.ApplyResources(this.scheduleWeeklyFreq, "scheduleWeeklyFreq");
   this.scheduleWeeklyFreq.Maximum = new decimal(new int[] {
            104,
            0,
            0,
            0});
   this.scheduleWeeklyFreq.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
   this.scheduleWeeklyFreq.Name = "scheduleWeeklyFreq";
   this.scheduleWeeklyFreq.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});



   resources.ApplyResources(this.scheduleDaily, "scheduleDaily");
   this.scheduleDaily.Name = "scheduleDaily";
   this.scheduleDaily.UseVisualStyleBackColor = true;
   this.scheduleDaily.CheckedChanged += new System.EventHandler(this.scheduleSpan_CheckedChanged);



   resources.ApplyResources(this.scheduleDailyPanel, "scheduleDailyPanel");
   this.scheduleDailyPanel.Controls.Add(this.scheduleDailyByDayFreq);
   this.scheduleDailyPanel.Controls.Add(this.scheduleDailyByDay);
   this.scheduleDailyPanel.Controls.Add(this.scheduleDailyByDayLbl);
   this.scheduleDailyPanel.Controls.Add(this.scheduleDailyByWeekday);
   this.scheduleDailyPanel.Name = "scheduleDailyPanel";



   resources.ApplyResources(this.scheduleDailyByDayFreq, "scheduleDailyByDayFreq");
   this.scheduleDailyByDayFreq.Maximum = new decimal(new int[] {
            366,
            0,
            0,
            0});
   this.scheduleDailyByDayFreq.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
   this.scheduleDailyByDayFreq.Name = "scheduleDailyByDayFreq";
   this.scheduleDailyByDayFreq.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});



   resources.ApplyResources(this.scheduleDailyByDay, "scheduleDailyByDay");
   this.scheduleDailyByDay.Checked = true;
   this.scheduleDailyByDay.Name = "scheduleDailyByDay";
   this.scheduleDailyByDay.TabStop = true;
   this.scheduleDailyByDay.UseVisualStyleBackColor = true;
   this.scheduleDailyByDay.CheckedChanged += new System.EventHandler(this.scheduleDailySpan_CheckedChanged);



   resources.ApplyResources(this.scheduleDailyByDayLbl, "scheduleDailyByDayLbl");
   this.scheduleDailyByDayLbl.Name = "scheduleDailyByDayLbl";



   resources.ApplyResources(this.scheduleDailyByWeekday, "scheduleDailyByWeekday");
   this.scheduleDailyByWeekday.Name = "scheduleDailyByWeekday";
   this.scheduleDailyByWeekday.UseVisualStyleBackColor = true;
   this.scheduleDailyByWeekday.CheckedChanged += new System.EventHandler(this.scheduleDailySpan_CheckedChanged);



   resources.ApplyResources(this.scheduleWeeklyDays, "scheduleWeeklyDays");
   this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklyMonday);
   this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklyTuesday);
   this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklyWednesday);
   this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklyThursday);
   this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklyFriday);
   this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklySaturday);
   this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklySunday);
   this.scheduleWeeklyDays.Name = "scheduleWeeklyDays";



   resources.ApplyResources(this.scheduleWeeklyMonday, "scheduleWeeklyMonday");
   this.scheduleWeeklyMonday.Name = "scheduleWeeklyMonday";
   this.scheduleWeeklyMonday.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.scheduleWeeklyTuesday, "scheduleWeeklyTuesday");
   this.scheduleWeeklyTuesday.Name = "scheduleWeeklyTuesday";
   this.scheduleWeeklyTuesday.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.scheduleWeeklyWednesday, "scheduleWeeklyWednesday");
   this.scheduleWeeklyWednesday.Name = "scheduleWeeklyWednesday";
   this.scheduleWeeklyWednesday.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.scheduleWeeklyThursday, "scheduleWeeklyThursday");
   this.scheduleWeeklyThursday.Name = "scheduleWeeklyThursday";
   this.scheduleWeeklyThursday.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.scheduleWeeklyFriday, "scheduleWeeklyFriday");
   this.scheduleWeeklyFriday.Name = "scheduleWeeklyFriday";
   this.scheduleWeeklyFriday.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.scheduleWeeklySaturday, "scheduleWeeklySaturday");
   this.scheduleWeeklySaturday.Name = "scheduleWeeklySaturday";
   this.scheduleWeeklySaturday.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.scheduleWeeklySunday, "scheduleWeeklySunday");
   this.scheduleWeeklySunday.Name = "scheduleWeeklySunday";
   this.scheduleWeeklySunday.UseVisualStyleBackColor = true;



   resources.ApplyResources(this.scheduleWeeklyFreqLbl, "scheduleWeeklyFreqLbl");
   this.scheduleWeeklyFreqLbl.Name = "scheduleWeeklyFreqLbl";



   resources.ApplyResources(this.scheduleWeeklyLbl, "scheduleWeeklyLbl");
   this.scheduleWeeklyLbl.Name = "scheduleWeeklyLbl";



   resources.ApplyResources(this.scheduleWeekly, "scheduleWeekly");
   this.scheduleWeekly.Name = "scheduleWeekly";
   this.scheduleWeekly.UseVisualStyleBackColor = true;
   this.scheduleWeekly.CheckedChanged += new System.EventHandler(this.scheduleSpan_CheckedChanged);



   resources.ApplyResources(this.scheduleMonthly, "scheduleMonthly");
   this.scheduleMonthly.Name = "scheduleMonthly";
   this.scheduleMonthly.TabStop = true;
   this.scheduleMonthly.UseVisualStyleBackColor = true;
   this.scheduleMonthly.CheckedChanged += new System.EventHandler(this.scheduleSpan_CheckedChanged);



   this.nonRecurringPanel.Controls.Add(this.nonRecurringLbl);
   this.nonRecurringPanel.Controls.Add(this.nonRecurringBitmap);
   resources.ApplyResources(this.nonRecurringPanel, "nonRecurringPanel");
   this.nonRecurringPanel.Name = "nonRecurringPanel";



   resources.ApplyResources(this.nonRecurringLbl, "nonRecurringLbl");
   this.nonRecurringLbl.Name = "nonRecurringLbl";



   this.nonRecurringBitmap.Image = global::Eraser.Properties.Resources.Information;
   resources.ApplyResources(this.nonRecurringBitmap, "nonRecurringBitmap");
   this.nonRecurringBitmap.Name = "nonRecurringBitmap";
   this.nonRecurringBitmap.TabStop = false;



   resources.ApplyResources(this.scheduleTimePanel, "scheduleTimePanel");
   this.scheduleTimePanel.Controls.Add(this.scheduleTime);
   this.scheduleTimePanel.Controls.Add(this.scheduleTimeLbl);
   this.scheduleTimePanel.Name = "scheduleTimePanel";



   this.scheduleTime.Format = System.Windows.Forms.DateTimePickerFormat.Custom;
   resources.ApplyResources(this.scheduleTime, "scheduleTime");
   this.scheduleTime.Name = "scheduleTime";
   this.scheduleTime.ShowUpDown = true;



   resources.ApplyResources(this.scheduleTimeLbl, "scheduleTimeLbl");
   this.scheduleTimeLbl.Name = "scheduleTimeLbl";



   this.errorProvider.ContainerControl = this;



   this.AcceptButton = this.ok;
   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Inherit;
   this.CancelButton = this.cancel;
   resources.ApplyResources(this, "$this");
   this.Controls.Add(this.container);
   this.Controls.Add(this.cancel);
   this.Controls.Add(this.ok);
   this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
   this.MaximizeBox = false;
   this.MinimizeBox = false;
   this.Name = "TaskPropertiesForm";
   this.ShowInTaskbar = false;
   this.dataContextMenuStrip.ResumeLayout(false);
   this.container.ResumeLayout(false);
   this.containerTask.ResumeLayout(false);
   this.containerTask.PerformLayout();
   this.containerSchedule.ResumeLayout(false);
   this.tableLayoutPanel1.ResumeLayout(false);
   this.tableLayoutPanel1.PerformLayout();
   this.schedulePattern.ResumeLayout(false);
   this.schedulePattern.PerformLayout();
   ((System.ComponentModel.ISupportInitialize)(this.scheduleMonthlyDayNumber)).EndInit();
   ((System.ComponentModel.ISupportInitialize)(this.scheduleMonthlyFreq)).EndInit();
   ((System.ComponentModel.ISupportInitialize)(this.scheduleWeeklyFreq)).EndInit();
   this.scheduleDailyPanel.ResumeLayout(false);
   this.scheduleDailyPanel.PerformLayout();
   ((System.ComponentModel.ISupportInitialize)(this.scheduleDailyByDayFreq)).EndInit();
   this.scheduleWeeklyDays.ResumeLayout(false);
   this.scheduleWeeklyDays.PerformLayout();
   this.nonRecurringPanel.ResumeLayout(false);
   ((System.ComponentModel.ISupportInitialize)(this.nonRecurringBitmap)).EndInit();
   this.scheduleTimePanel.ResumeLayout(false);
   this.scheduleTimePanel.PerformLayout();
   ((System.ComponentModel.ISupportInitialize)(this.errorProvider)).EndInit();
   this.ResumeLayout(false);

  }



  private System.Windows.Forms.Label nameLbl;
  private System.Windows.Forms.TextBox name;
  private System.Windows.Forms.Label eraseLbl;
  private System.Windows.Forms.Label typeLbl;
  private System.Windows.Forms.RadioButton typeImmediate;
  private System.Windows.Forms.RadioButton typeRecurring;
  private System.Windows.Forms.ListView data;
  private System.Windows.Forms.ColumnHeader dataColData;
  private System.Windows.Forms.ColumnHeader dataColMethod;
  private System.Windows.Forms.Button dataAdd;
  private System.Windows.Forms.Button ok;
  private System.Windows.Forms.Button cancel;
  private System.Windows.Forms.TabControl container;
  private System.Windows.Forms.TabPage containerTask;
  private System.Windows.Forms.TabPage containerSchedule;
  private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
  private System.Windows.Forms.GroupBox schedulePattern;
  private System.Windows.Forms.NumericUpDown scheduleWeeklyFreq;
  private System.Windows.Forms.RadioButton scheduleDaily;
  private System.Windows.Forms.Panel scheduleDailyPanel;
  private System.Windows.Forms.NumericUpDown scheduleDailyByDayFreq;
  private System.Windows.Forms.RadioButton scheduleDailyByDay;
  private System.Windows.Forms.Label scheduleDailyByDayLbl;
  private System.Windows.Forms.RadioButton scheduleDailyByWeekday;
  private System.Windows.Forms.FlowLayoutPanel scheduleWeeklyDays;
  private System.Windows.Forms.CheckBox scheduleWeeklyMonday;
  private System.Windows.Forms.CheckBox scheduleWeeklyTuesday;
  private System.Windows.Forms.CheckBox scheduleWeeklyWednesday;
  private System.Windows.Forms.CheckBox scheduleWeeklyThursday;
  private System.Windows.Forms.CheckBox scheduleWeeklyFriday;
  private System.Windows.Forms.CheckBox scheduleWeeklySaturday;
  private System.Windows.Forms.CheckBox scheduleWeeklySunday;
  private System.Windows.Forms.Label scheduleWeeklyFreqLbl;
  private System.Windows.Forms.Label scheduleWeeklyLbl;
  private System.Windows.Forms.RadioButton scheduleWeekly;
  private System.Windows.Forms.RadioButton scheduleMonthly;
  private System.Windows.Forms.Panel nonRecurringPanel;
  private System.Windows.Forms.Label nonRecurringLbl;
  private System.Windows.Forms.PictureBox nonRecurringBitmap;
  private System.Windows.Forms.Panel scheduleTimePanel;
  private System.Windows.Forms.Label scheduleTimeLbl;
  private System.Windows.Forms.ErrorProvider errorProvider;
  private System.Windows.Forms.RadioButton typeRestart;
  private System.Windows.Forms.NumericUpDown scheduleMonthlyDayNumber;
  private System.Windows.Forms.NumericUpDown scheduleMonthlyFreq;
  private System.Windows.Forms.Label scheduleMonthlyMonthLbl;
  private System.Windows.Forms.Label scheduleMonthlyEveryLbl;
  private System.Windows.Forms.Label scheduleMonthlyLbl;
  private System.Windows.Forms.DateTimePicker scheduleTime;
  private System.Windows.Forms.ContextMenuStrip dataContextMenuStrip;
  private System.Windows.Forms.ToolStripMenuItem deleteDataToolStripMenuItem;
  private System.Windows.Forms.RadioButton typeManual;
 }
}
