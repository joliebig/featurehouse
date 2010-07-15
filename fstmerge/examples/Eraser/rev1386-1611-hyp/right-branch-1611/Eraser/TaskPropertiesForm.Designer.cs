

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



            this.nameLbl.AccessibleDescription = null;
            this.nameLbl.AccessibleName = null;
            resources.ApplyResources(this.nameLbl, "nameLbl");
            this.errorProvider.SetError(this.nameLbl, resources.GetString("nameLbl.Error"));
            this.nameLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.nameLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("nameLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.nameLbl, ((int)(resources.GetObject("nameLbl.IconPadding"))));
            this.nameLbl.Name = "nameLbl";



            this.name.AccessibleDescription = null;
            this.name.AccessibleName = null;
            resources.ApplyResources(this.name, "name");
            this.name.BackgroundImage = null;
            this.errorProvider.SetError(this.name, resources.GetString("name.Error"));
            this.name.Font = null;
            this.errorProvider.SetIconAlignment(this.name, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("name.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.name, ((int)(resources.GetObject("name.IconPadding"))));
            this.name.Name = "name";



            this.eraseLbl.AccessibleDescription = null;
            this.eraseLbl.AccessibleName = null;
            resources.ApplyResources(this.eraseLbl, "eraseLbl");
            this.errorProvider.SetError(this.eraseLbl, resources.GetString("eraseLbl.Error"));
            this.eraseLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.eraseLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("eraseLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.eraseLbl, ((int)(resources.GetObject("eraseLbl.IconPadding"))));
            this.eraseLbl.Name = "eraseLbl";



            this.typeLbl.AccessibleDescription = null;
            this.typeLbl.AccessibleName = null;
            resources.ApplyResources(this.typeLbl, "typeLbl");
            this.errorProvider.SetError(this.typeLbl, resources.GetString("typeLbl.Error"));
            this.typeLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.typeLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("typeLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.typeLbl, ((int)(resources.GetObject("typeLbl.IconPadding"))));
            this.typeLbl.Name = "typeLbl";



            this.typeImmediate.AccessibleDescription = null;
            this.typeImmediate.AccessibleName = null;
            resources.ApplyResources(this.typeImmediate, "typeImmediate");
            this.typeImmediate.BackgroundImage = null;
            this.errorProvider.SetError(this.typeImmediate, resources.GetString("typeImmediate.Error"));
            this.typeImmediate.Font = null;
            this.errorProvider.SetIconAlignment(this.typeImmediate, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("typeImmediate.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.typeImmediate, ((int)(resources.GetObject("typeImmediate.IconPadding"))));
            this.typeImmediate.Name = "typeImmediate";
            this.typeImmediate.UseVisualStyleBackColor = true;
            this.typeImmediate.CheckedChanged += new System.EventHandler(this.taskType_CheckedChanged);



            this.typeRecurring.AccessibleDescription = null;
            this.typeRecurring.AccessibleName = null;
            resources.ApplyResources(this.typeRecurring, "typeRecurring");
            this.typeRecurring.BackgroundImage = null;
            this.errorProvider.SetError(this.typeRecurring, resources.GetString("typeRecurring.Error"));
            this.typeRecurring.Font = null;
            this.errorProvider.SetIconAlignment(this.typeRecurring, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("typeRecurring.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.typeRecurring, ((int)(resources.GetObject("typeRecurring.IconPadding"))));
            this.typeRecurring.Name = "typeRecurring";
            this.typeRecurring.UseVisualStyleBackColor = true;
            this.typeRecurring.CheckedChanged += new System.EventHandler(this.taskType_CheckedChanged);



            this.data.AccessibleDescription = null;
            this.data.AccessibleName = null;
            resources.ApplyResources(this.data, "data");
            this.data.BackgroundImage = null;
            this.data.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.dataColData,
            this.dataColMethod});
            this.data.ContextMenuStrip = this.dataContextMenuStrip;
            this.errorProvider.SetError(this.data, resources.GetString("data.Error"));
            this.data.Font = null;
            this.data.FullRowSelect = true;
            this.errorProvider.SetIconAlignment(this.data, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("data.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.data, ((int)(resources.GetObject("data.IconPadding"))));
            this.data.MultiSelect = false;
            this.data.Name = "data";
            this.data.UseCompatibleStateImageBehavior = false;
            this.data.View = System.Windows.Forms.View.Details;
            this.data.ItemActivate += new System.EventHandler(this.data_ItemActivate);



            resources.ApplyResources(this.dataColData, "dataColData");



            resources.ApplyResources(this.dataColMethod, "dataColMethod");



            this.dataContextMenuStrip.AccessibleDescription = null;
            this.dataContextMenuStrip.AccessibleName = null;
            resources.ApplyResources(this.dataContextMenuStrip, "dataContextMenuStrip");
            this.dataContextMenuStrip.BackgroundImage = null;
            this.errorProvider.SetError(this.dataContextMenuStrip, resources.GetString("dataContextMenuStrip.Error"));
            this.dataContextMenuStrip.Font = null;
            this.errorProvider.SetIconAlignment(this.dataContextMenuStrip, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("dataContextMenuStrip.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.dataContextMenuStrip, ((int)(resources.GetObject("dataContextMenuStrip.IconPadding"))));
            this.dataContextMenuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.deleteDataToolStripMenuItem});
            this.dataContextMenuStrip.Name = "dataContextMenuStrip";
            this.dataContextMenuStrip.Opening += new System.ComponentModel.CancelEventHandler(this.dataContextMenuStrip_Opening);



            this.deleteDataToolStripMenuItem.AccessibleDescription = null;
            this.deleteDataToolStripMenuItem.AccessibleName = null;
            resources.ApplyResources(this.deleteDataToolStripMenuItem, "deleteDataToolStripMenuItem");
            this.deleteDataToolStripMenuItem.BackgroundImage = null;
            this.deleteDataToolStripMenuItem.Name = "deleteDataToolStripMenuItem";
            this.deleteDataToolStripMenuItem.ShortcutKeyDisplayString = null;
            this.deleteDataToolStripMenuItem.Click += new System.EventHandler(this.deleteDataToolStripMenuItem_Click);



            this.dataAdd.AccessibleDescription = null;
            this.dataAdd.AccessibleName = null;
            resources.ApplyResources(this.dataAdd, "dataAdd");
            this.dataAdd.BackgroundImage = null;
            this.errorProvider.SetError(this.dataAdd, resources.GetString("dataAdd.Error"));
            this.dataAdd.Font = null;
            this.errorProvider.SetIconAlignment(this.dataAdd, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("dataAdd.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.dataAdd, ((int)(resources.GetObject("dataAdd.IconPadding"))));
            this.dataAdd.Name = "dataAdd";
            this.dataAdd.UseVisualStyleBackColor = true;
            this.dataAdd.Click += new System.EventHandler(this.dataAdd_Click);



            this.ok.AccessibleDescription = null;
            this.ok.AccessibleName = null;
            resources.ApplyResources(this.ok, "ok");
            this.ok.BackgroundImage = null;
            this.errorProvider.SetError(this.ok, resources.GetString("ok.Error"));
            this.ok.Font = null;
            this.errorProvider.SetIconAlignment(this.ok, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("ok.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.ok, ((int)(resources.GetObject("ok.IconPadding"))));
            this.ok.Name = "ok";
            this.ok.UseVisualStyleBackColor = true;
            this.ok.Click += new System.EventHandler(this.ok_Click);



            this.cancel.AccessibleDescription = null;
            this.cancel.AccessibleName = null;
            resources.ApplyResources(this.cancel, "cancel");
            this.cancel.BackgroundImage = null;
            this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.errorProvider.SetError(this.cancel, resources.GetString("cancel.Error"));
            this.cancel.Font = null;
            this.errorProvider.SetIconAlignment(this.cancel, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("cancel.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.cancel, ((int)(resources.GetObject("cancel.IconPadding"))));
            this.cancel.Name = "cancel";
            this.cancel.UseVisualStyleBackColor = true;



            this.container.AccessibleDescription = null;
            this.container.AccessibleName = null;
            resources.ApplyResources(this.container, "container");
            this.container.BackgroundImage = null;
            this.container.Controls.Add(this.containerTask);
            this.container.Controls.Add(this.containerSchedule);
            this.errorProvider.SetError(this.container, resources.GetString("container.Error"));
            this.container.Font = null;
            this.errorProvider.SetIconAlignment(this.container, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("container.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.container, ((int)(resources.GetObject("container.IconPadding"))));
            this.container.Name = "container";
            this.container.SelectedIndex = 0;



            this.containerTask.AccessibleDescription = null;
            this.containerTask.AccessibleName = null;
            resources.ApplyResources(this.containerTask, "containerTask");
            this.containerTask.BackgroundImage = null;
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
            this.errorProvider.SetError(this.containerTask, resources.GetString("containerTask.Error"));
            this.containerTask.Font = null;
            this.errorProvider.SetIconAlignment(this.containerTask, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("containerTask.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.containerTask, ((int)(resources.GetObject("containerTask.IconPadding"))));
            this.containerTask.Name = "containerTask";
            this.containerTask.UseVisualStyleBackColor = true;



            this.typeManual.AccessibleDescription = null;
            this.typeManual.AccessibleName = null;
            resources.ApplyResources(this.typeManual, "typeManual");
            this.typeManual.BackgroundImage = null;
            this.errorProvider.SetError(this.typeManual, resources.GetString("typeManual.Error"));
            this.typeManual.Font = null;
            this.errorProvider.SetIconAlignment(this.typeManual, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("typeManual.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.typeManual, ((int)(resources.GetObject("typeManual.IconPadding"))));
            this.typeManual.Name = "typeManual";
            this.typeManual.TabStop = true;
            this.typeManual.UseVisualStyleBackColor = true;
            this.typeManual.CheckedChanged += new System.EventHandler(this.taskType_CheckedChanged);



            this.typeRestart.AccessibleDescription = null;
            this.typeRestart.AccessibleName = null;
            resources.ApplyResources(this.typeRestart, "typeRestart");
            this.typeRestart.BackgroundImage = null;
            this.errorProvider.SetError(this.typeRestart, resources.GetString("typeRestart.Error"));
            this.typeRestart.Font = null;
            this.errorProvider.SetIconAlignment(this.typeRestart, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("typeRestart.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.typeRestart, ((int)(resources.GetObject("typeRestart.IconPadding"))));
            this.typeRestart.Name = "typeRestart";
            this.typeRestart.TabStop = true;
            this.typeRestart.UseVisualStyleBackColor = true;



            this.containerSchedule.AccessibleDescription = null;
            this.containerSchedule.AccessibleName = null;
            resources.ApplyResources(this.containerSchedule, "containerSchedule");
            this.containerSchedule.BackgroundImage = null;
            this.containerSchedule.Controls.Add(this.tableLayoutPanel1);
            this.errorProvider.SetError(this.containerSchedule, resources.GetString("containerSchedule.Error"));
            this.containerSchedule.Font = null;
            this.errorProvider.SetIconAlignment(this.containerSchedule, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("containerSchedule.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.containerSchedule, ((int)(resources.GetObject("containerSchedule.IconPadding"))));
            this.containerSchedule.Name = "containerSchedule";
            this.containerSchedule.UseVisualStyleBackColor = true;



            this.tableLayoutPanel1.AccessibleDescription = null;
            this.tableLayoutPanel1.AccessibleName = null;
            resources.ApplyResources(this.tableLayoutPanel1, "tableLayoutPanel1");
            this.tableLayoutPanel1.BackgroundImage = null;
            this.tableLayoutPanel1.Controls.Add(this.schedulePattern, 0, 2);
            this.tableLayoutPanel1.Controls.Add(this.nonRecurringPanel, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.scheduleTimePanel, 0, 1);
            this.errorProvider.SetError(this.tableLayoutPanel1, resources.GetString("tableLayoutPanel1.Error"));
            this.tableLayoutPanel1.Font = null;
            this.errorProvider.SetIconAlignment(this.tableLayoutPanel1, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("tableLayoutPanel1.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.tableLayoutPanel1, ((int)(resources.GetObject("tableLayoutPanel1.IconPadding"))));
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";



            this.schedulePattern.AccessibleDescription = null;
            this.schedulePattern.AccessibleName = null;
            resources.ApplyResources(this.schedulePattern, "schedulePattern");
            this.schedulePattern.BackgroundImage = null;
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
            this.errorProvider.SetError(this.schedulePattern, resources.GetString("schedulePattern.Error"));
            this.schedulePattern.Font = null;
            this.errorProvider.SetIconAlignment(this.schedulePattern, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("schedulePattern.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.schedulePattern, ((int)(resources.GetObject("schedulePattern.IconPadding"))));
            this.schedulePattern.Name = "schedulePattern";
            this.schedulePattern.TabStop = false;



            this.scheduleMonthlyLbl.AccessibleDescription = null;
            this.scheduleMonthlyLbl.AccessibleName = null;
            resources.ApplyResources(this.scheduleMonthlyLbl, "scheduleMonthlyLbl");
            this.errorProvider.SetError(this.scheduleMonthlyLbl, resources.GetString("scheduleMonthlyLbl.Error"));
            this.scheduleMonthlyLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleMonthlyLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleMonthlyLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleMonthlyLbl, ((int)(resources.GetObject("scheduleMonthlyLbl.IconPadding"))));
            this.scheduleMonthlyLbl.Name = "scheduleMonthlyLbl";



            this.scheduleMonthlyDayNumber.AccessibleDescription = null;
            this.scheduleMonthlyDayNumber.AccessibleName = null;
            resources.ApplyResources(this.scheduleMonthlyDayNumber, "scheduleMonthlyDayNumber");
            this.errorProvider.SetError(this.scheduleMonthlyDayNumber, resources.GetString("scheduleMonthlyDayNumber.Error"));
            this.scheduleMonthlyDayNumber.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleMonthlyDayNumber, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleMonthlyDayNumber.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleMonthlyDayNumber, ((int)(resources.GetObject("scheduleMonthlyDayNumber.IconPadding"))));
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



            this.scheduleMonthlyFreq.AccessibleDescription = null;
            this.scheduleMonthlyFreq.AccessibleName = null;
            resources.ApplyResources(this.scheduleMonthlyFreq, "scheduleMonthlyFreq");
            this.errorProvider.SetError(this.scheduleMonthlyFreq, resources.GetString("scheduleMonthlyFreq.Error"));
            this.scheduleMonthlyFreq.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleMonthlyFreq, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleMonthlyFreq.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleMonthlyFreq, ((int)(resources.GetObject("scheduleMonthlyFreq.IconPadding"))));
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



            this.scheduleMonthlyMonthLbl.AccessibleDescription = null;
            this.scheduleMonthlyMonthLbl.AccessibleName = null;
            resources.ApplyResources(this.scheduleMonthlyMonthLbl, "scheduleMonthlyMonthLbl");
            this.errorProvider.SetError(this.scheduleMonthlyMonthLbl, resources.GetString("scheduleMonthlyMonthLbl.Error"));
            this.scheduleMonthlyMonthLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleMonthlyMonthLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleMonthlyMonthLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleMonthlyMonthLbl, ((int)(resources.GetObject("scheduleMonthlyMonthLbl.IconPadding"))));
            this.scheduleMonthlyMonthLbl.Name = "scheduleMonthlyMonthLbl";



            this.scheduleMonthlyEveryLbl.AccessibleDescription = null;
            this.scheduleMonthlyEveryLbl.AccessibleName = null;
            resources.ApplyResources(this.scheduleMonthlyEveryLbl, "scheduleMonthlyEveryLbl");
            this.errorProvider.SetError(this.scheduleMonthlyEveryLbl, resources.GetString("scheduleMonthlyEveryLbl.Error"));
            this.scheduleMonthlyEveryLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleMonthlyEveryLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleMonthlyEveryLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleMonthlyEveryLbl, ((int)(resources.GetObject("scheduleMonthlyEveryLbl.IconPadding"))));
            this.scheduleMonthlyEveryLbl.Name = "scheduleMonthlyEveryLbl";



            this.scheduleWeeklyFreq.AccessibleDescription = null;
            this.scheduleWeeklyFreq.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklyFreq, "scheduleWeeklyFreq");
            this.errorProvider.SetError(this.scheduleWeeklyFreq, resources.GetString("scheduleWeeklyFreq.Error"));
            this.scheduleWeeklyFreq.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklyFreq, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklyFreq.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklyFreq, ((int)(resources.GetObject("scheduleWeeklyFreq.IconPadding"))));
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



            this.scheduleDaily.AccessibleDescription = null;
            this.scheduleDaily.AccessibleName = null;
            resources.ApplyResources(this.scheduleDaily, "scheduleDaily");
            this.scheduleDaily.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleDaily, resources.GetString("scheduleDaily.Error"));
            this.scheduleDaily.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleDaily, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleDaily.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleDaily, ((int)(resources.GetObject("scheduleDaily.IconPadding"))));
            this.scheduleDaily.Name = "scheduleDaily";
            this.scheduleDaily.UseVisualStyleBackColor = true;
            this.scheduleDaily.CheckedChanged += new System.EventHandler(this.scheduleSpan_CheckedChanged);



            this.scheduleDailyPanel.AccessibleDescription = null;
            this.scheduleDailyPanel.AccessibleName = null;
            resources.ApplyResources(this.scheduleDailyPanel, "scheduleDailyPanel");
            this.scheduleDailyPanel.BackgroundImage = null;
            this.scheduleDailyPanel.Controls.Add(this.scheduleDailyByDayFreq);
            this.scheduleDailyPanel.Controls.Add(this.scheduleDailyByDay);
            this.scheduleDailyPanel.Controls.Add(this.scheduleDailyByDayLbl);
            this.scheduleDailyPanel.Controls.Add(this.scheduleDailyByWeekday);
            this.errorProvider.SetError(this.scheduleDailyPanel, resources.GetString("scheduleDailyPanel.Error"));
            this.scheduleDailyPanel.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleDailyPanel, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleDailyPanel.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleDailyPanel, ((int)(resources.GetObject("scheduleDailyPanel.IconPadding"))));
            this.scheduleDailyPanel.Name = "scheduleDailyPanel";



            this.scheduleDailyByDayFreq.AccessibleDescription = null;
            this.scheduleDailyByDayFreq.AccessibleName = null;
            resources.ApplyResources(this.scheduleDailyByDayFreq, "scheduleDailyByDayFreq");
            this.errorProvider.SetError(this.scheduleDailyByDayFreq, resources.GetString("scheduleDailyByDayFreq.Error"));
            this.scheduleDailyByDayFreq.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleDailyByDayFreq, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleDailyByDayFreq.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleDailyByDayFreq, ((int)(resources.GetObject("scheduleDailyByDayFreq.IconPadding"))));
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



            this.scheduleDailyByDay.AccessibleDescription = null;
            this.scheduleDailyByDay.AccessibleName = null;
            resources.ApplyResources(this.scheduleDailyByDay, "scheduleDailyByDay");
            this.scheduleDailyByDay.BackgroundImage = null;
            this.scheduleDailyByDay.Checked = true;
            this.errorProvider.SetError(this.scheduleDailyByDay, resources.GetString("scheduleDailyByDay.Error"));
            this.scheduleDailyByDay.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleDailyByDay, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleDailyByDay.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleDailyByDay, ((int)(resources.GetObject("scheduleDailyByDay.IconPadding"))));
            this.scheduleDailyByDay.Name = "scheduleDailyByDay";
            this.scheduleDailyByDay.TabStop = true;
            this.scheduleDailyByDay.UseVisualStyleBackColor = true;
            this.scheduleDailyByDay.CheckedChanged += new System.EventHandler(this.scheduleDailySpan_CheckedChanged);



            this.scheduleDailyByDayLbl.AccessibleDescription = null;
            this.scheduleDailyByDayLbl.AccessibleName = null;
            resources.ApplyResources(this.scheduleDailyByDayLbl, "scheduleDailyByDayLbl");
            this.errorProvider.SetError(this.scheduleDailyByDayLbl, resources.GetString("scheduleDailyByDayLbl.Error"));
            this.scheduleDailyByDayLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleDailyByDayLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleDailyByDayLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleDailyByDayLbl, ((int)(resources.GetObject("scheduleDailyByDayLbl.IconPadding"))));
            this.scheduleDailyByDayLbl.Name = "scheduleDailyByDayLbl";



            this.scheduleDailyByWeekday.AccessibleDescription = null;
            this.scheduleDailyByWeekday.AccessibleName = null;
            resources.ApplyResources(this.scheduleDailyByWeekday, "scheduleDailyByWeekday");
            this.scheduleDailyByWeekday.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleDailyByWeekday, resources.GetString("scheduleDailyByWeekday.Error"));
            this.scheduleDailyByWeekday.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleDailyByWeekday, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleDailyByWeekday.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleDailyByWeekday, ((int)(resources.GetObject("scheduleDailyByWeekday.IconPadding"))));
            this.scheduleDailyByWeekday.Name = "scheduleDailyByWeekday";
            this.scheduleDailyByWeekday.UseVisualStyleBackColor = true;
            this.scheduleDailyByWeekday.CheckedChanged += new System.EventHandler(this.scheduleDailySpan_CheckedChanged);



            this.scheduleWeeklyDays.AccessibleDescription = null;
            this.scheduleWeeklyDays.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklyDays, "scheduleWeeklyDays");
            this.scheduleWeeklyDays.BackgroundImage = null;
            this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklyMonday);
            this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklyTuesday);
            this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklyWednesday);
            this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklyThursday);
            this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklyFriday);
            this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklySaturday);
            this.scheduleWeeklyDays.Controls.Add(this.scheduleWeeklySunday);
            this.errorProvider.SetError(this.scheduleWeeklyDays, resources.GetString("scheduleWeeklyDays.Error"));
            this.scheduleWeeklyDays.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklyDays, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklyDays.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklyDays, ((int)(resources.GetObject("scheduleWeeklyDays.IconPadding"))));
            this.scheduleWeeklyDays.Name = "scheduleWeeklyDays";



            this.scheduleWeeklyMonday.AccessibleDescription = null;
            this.scheduleWeeklyMonday.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklyMonday, "scheduleWeeklyMonday");
            this.scheduleWeeklyMonday.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleWeeklyMonday, resources.GetString("scheduleWeeklyMonday.Error"));
            this.scheduleWeeklyMonday.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklyMonday, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklyMonday.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklyMonday, ((int)(resources.GetObject("scheduleWeeklyMonday.IconPadding"))));
            this.scheduleWeeklyMonday.Name = "scheduleWeeklyMonday";
            this.scheduleWeeklyMonday.UseVisualStyleBackColor = true;



            this.scheduleWeeklyTuesday.AccessibleDescription = null;
            this.scheduleWeeklyTuesday.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklyTuesday, "scheduleWeeklyTuesday");
            this.scheduleWeeklyTuesday.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleWeeklyTuesday, resources.GetString("scheduleWeeklyTuesday.Error"));
            this.scheduleWeeklyTuesday.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklyTuesday, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklyTuesday.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklyTuesday, ((int)(resources.GetObject("scheduleWeeklyTuesday.IconPadding"))));
            this.scheduleWeeklyTuesday.Name = "scheduleWeeklyTuesday";
            this.scheduleWeeklyTuesday.UseVisualStyleBackColor = true;



            this.scheduleWeeklyWednesday.AccessibleDescription = null;
            this.scheduleWeeklyWednesday.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklyWednesday, "scheduleWeeklyWednesday");
            this.scheduleWeeklyWednesday.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleWeeklyWednesday, resources.GetString("scheduleWeeklyWednesday.Error"));
            this.scheduleWeeklyWednesday.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklyWednesday, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklyWednesday.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklyWednesday, ((int)(resources.GetObject("scheduleWeeklyWednesday.IconPadding"))));
            this.scheduleWeeklyWednesday.Name = "scheduleWeeklyWednesday";
            this.scheduleWeeklyWednesday.UseVisualStyleBackColor = true;



            this.scheduleWeeklyThursday.AccessibleDescription = null;
            this.scheduleWeeklyThursday.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklyThursday, "scheduleWeeklyThursday");
            this.scheduleWeeklyThursday.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleWeeklyThursday, resources.GetString("scheduleWeeklyThursday.Error"));
            this.scheduleWeeklyThursday.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklyThursday, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklyThursday.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklyThursday, ((int)(resources.GetObject("scheduleWeeklyThursday.IconPadding"))));
            this.scheduleWeeklyThursday.Name = "scheduleWeeklyThursday";
            this.scheduleWeeklyThursday.UseVisualStyleBackColor = true;



            this.scheduleWeeklyFriday.AccessibleDescription = null;
            this.scheduleWeeklyFriday.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklyFriday, "scheduleWeeklyFriday");
            this.scheduleWeeklyFriday.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleWeeklyFriday, resources.GetString("scheduleWeeklyFriday.Error"));
            this.scheduleWeeklyFriday.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklyFriday, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklyFriday.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklyFriday, ((int)(resources.GetObject("scheduleWeeklyFriday.IconPadding"))));
            this.scheduleWeeklyFriday.Name = "scheduleWeeklyFriday";
            this.scheduleWeeklyFriday.UseVisualStyleBackColor = true;



            this.scheduleWeeklySaturday.AccessibleDescription = null;
            this.scheduleWeeklySaturday.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklySaturday, "scheduleWeeklySaturday");
            this.scheduleWeeklySaturday.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleWeeklySaturday, resources.GetString("scheduleWeeklySaturday.Error"));
            this.scheduleWeeklySaturday.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklySaturday, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklySaturday.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklySaturday, ((int)(resources.GetObject("scheduleWeeklySaturday.IconPadding"))));
            this.scheduleWeeklySaturday.Name = "scheduleWeeklySaturday";
            this.scheduleWeeklySaturday.UseVisualStyleBackColor = true;



            this.scheduleWeeklySunday.AccessibleDescription = null;
            this.scheduleWeeklySunday.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklySunday, "scheduleWeeklySunday");
            this.scheduleWeeklySunday.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleWeeklySunday, resources.GetString("scheduleWeeklySunday.Error"));
            this.scheduleWeeklySunday.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklySunday, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklySunday.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklySunday, ((int)(resources.GetObject("scheduleWeeklySunday.IconPadding"))));
            this.scheduleWeeklySunday.Name = "scheduleWeeklySunday";
            this.scheduleWeeklySunday.UseVisualStyleBackColor = true;



            this.scheduleWeeklyFreqLbl.AccessibleDescription = null;
            this.scheduleWeeklyFreqLbl.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklyFreqLbl, "scheduleWeeklyFreqLbl");
            this.errorProvider.SetError(this.scheduleWeeklyFreqLbl, resources.GetString("scheduleWeeklyFreqLbl.Error"));
            this.scheduleWeeklyFreqLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklyFreqLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklyFreqLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklyFreqLbl, ((int)(resources.GetObject("scheduleWeeklyFreqLbl.IconPadding"))));
            this.scheduleWeeklyFreqLbl.Name = "scheduleWeeklyFreqLbl";



            this.scheduleWeeklyLbl.AccessibleDescription = null;
            this.scheduleWeeklyLbl.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeeklyLbl, "scheduleWeeklyLbl");
            this.errorProvider.SetError(this.scheduleWeeklyLbl, resources.GetString("scheduleWeeklyLbl.Error"));
            this.scheduleWeeklyLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeeklyLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeeklyLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeeklyLbl, ((int)(resources.GetObject("scheduleWeeklyLbl.IconPadding"))));
            this.scheduleWeeklyLbl.Name = "scheduleWeeklyLbl";



            this.scheduleWeekly.AccessibleDescription = null;
            this.scheduleWeekly.AccessibleName = null;
            resources.ApplyResources(this.scheduleWeekly, "scheduleWeekly");
            this.scheduleWeekly.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleWeekly, resources.GetString("scheduleWeekly.Error"));
            this.scheduleWeekly.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleWeekly, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleWeekly.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleWeekly, ((int)(resources.GetObject("scheduleWeekly.IconPadding"))));
            this.scheduleWeekly.Name = "scheduleWeekly";
            this.scheduleWeekly.UseVisualStyleBackColor = true;
            this.scheduleWeekly.CheckedChanged += new System.EventHandler(this.scheduleSpan_CheckedChanged);



            this.scheduleMonthly.AccessibleDescription = null;
            this.scheduleMonthly.AccessibleName = null;
            resources.ApplyResources(this.scheduleMonthly, "scheduleMonthly");
            this.scheduleMonthly.BackgroundImage = null;
            this.errorProvider.SetError(this.scheduleMonthly, resources.GetString("scheduleMonthly.Error"));
            this.scheduleMonthly.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleMonthly, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleMonthly.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleMonthly, ((int)(resources.GetObject("scheduleMonthly.IconPadding"))));
            this.scheduleMonthly.Name = "scheduleMonthly";
            this.scheduleMonthly.TabStop = true;
            this.scheduleMonthly.UseVisualStyleBackColor = true;
            this.scheduleMonthly.CheckedChanged += new System.EventHandler(this.scheduleSpan_CheckedChanged);



            this.nonRecurringPanel.AccessibleDescription = null;
            this.nonRecurringPanel.AccessibleName = null;
            resources.ApplyResources(this.nonRecurringPanel, "nonRecurringPanel");
            this.nonRecurringPanel.BackgroundImage = null;
            this.nonRecurringPanel.Controls.Add(this.nonRecurringLbl);
            this.nonRecurringPanel.Controls.Add(this.nonRecurringBitmap);
            this.errorProvider.SetError(this.nonRecurringPanel, resources.GetString("nonRecurringPanel.Error"));
            this.nonRecurringPanel.Font = null;
            this.errorProvider.SetIconAlignment(this.nonRecurringPanel, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("nonRecurringPanel.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.nonRecurringPanel, ((int)(resources.GetObject("nonRecurringPanel.IconPadding"))));
            this.nonRecurringPanel.Name = "nonRecurringPanel";



            this.nonRecurringLbl.AccessibleDescription = null;
            this.nonRecurringLbl.AccessibleName = null;
            resources.ApplyResources(this.nonRecurringLbl, "nonRecurringLbl");
            this.errorProvider.SetError(this.nonRecurringLbl, resources.GetString("nonRecurringLbl.Error"));
            this.nonRecurringLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.nonRecurringLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("nonRecurringLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.nonRecurringLbl, ((int)(resources.GetObject("nonRecurringLbl.IconPadding"))));
            this.nonRecurringLbl.Name = "nonRecurringLbl";



            this.nonRecurringBitmap.AccessibleDescription = null;
            this.nonRecurringBitmap.AccessibleName = null;
            resources.ApplyResources(this.nonRecurringBitmap, "nonRecurringBitmap");
            this.nonRecurringBitmap.BackgroundImage = null;
            this.errorProvider.SetError(this.nonRecurringBitmap, resources.GetString("nonRecurringBitmap.Error"));
            this.nonRecurringBitmap.Font = null;
            this.errorProvider.SetIconAlignment(this.nonRecurringBitmap, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("nonRecurringBitmap.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.nonRecurringBitmap, ((int)(resources.GetObject("nonRecurringBitmap.IconPadding"))));
            this.nonRecurringBitmap.Image = global::Eraser.Properties.Resources.Information;
            this.nonRecurringBitmap.ImageLocation = null;
            this.nonRecurringBitmap.Name = "nonRecurringBitmap";
            this.nonRecurringBitmap.TabStop = false;



            this.scheduleTimePanel.AccessibleDescription = null;
            this.scheduleTimePanel.AccessibleName = null;
            resources.ApplyResources(this.scheduleTimePanel, "scheduleTimePanel");
            this.scheduleTimePanel.BackgroundImage = null;
            this.scheduleTimePanel.Controls.Add(this.scheduleTime);
            this.scheduleTimePanel.Controls.Add(this.scheduleTimeLbl);
            this.errorProvider.SetError(this.scheduleTimePanel, resources.GetString("scheduleTimePanel.Error"));
            this.scheduleTimePanel.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleTimePanel, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleTimePanel.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleTimePanel, ((int)(resources.GetObject("scheduleTimePanel.IconPadding"))));
            this.scheduleTimePanel.Name = "scheduleTimePanel";



            this.scheduleTime.AccessibleDescription = null;
            this.scheduleTime.AccessibleName = null;
            resources.ApplyResources(this.scheduleTime, "scheduleTime");
            this.scheduleTime.BackgroundImage = null;
            this.scheduleTime.CalendarFont = null;
            this.scheduleTime.CustomFormat = null;
            this.errorProvider.SetError(this.scheduleTime, resources.GetString("scheduleTime.Error"));
            this.scheduleTime.Font = null;
            this.scheduleTime.Format = System.Windows.Forms.DateTimePickerFormat.Custom;
            this.errorProvider.SetIconAlignment(this.scheduleTime, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleTime.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleTime, ((int)(resources.GetObject("scheduleTime.IconPadding"))));
            this.scheduleTime.Name = "scheduleTime";
            this.scheduleTime.ShowUpDown = true;



            this.scheduleTimeLbl.AccessibleDescription = null;
            this.scheduleTimeLbl.AccessibleName = null;
            resources.ApplyResources(this.scheduleTimeLbl, "scheduleTimeLbl");
            this.errorProvider.SetError(this.scheduleTimeLbl, resources.GetString("scheduleTimeLbl.Error"));
            this.scheduleTimeLbl.Font = null;
            this.errorProvider.SetIconAlignment(this.scheduleTimeLbl, ((System.Windows.Forms.ErrorIconAlignment)(resources.GetObject("scheduleTimeLbl.IconAlignment"))));
            this.errorProvider.SetIconPadding(this.scheduleTimeLbl, ((int)(resources.GetObject("scheduleTimeLbl.IconPadding"))));
            this.scheduleTimeLbl.Name = "scheduleTimeLbl";



            this.errorProvider.ContainerControl = this;
            resources.ApplyResources(this.errorProvider, "errorProvider");



            this.AcceptButton = this.ok;
            this.AccessibleDescription = null;
            this.AccessibleName = null;
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Inherit;
            resources.ApplyResources(this, "$this");
            this.BackgroundImage = null;
            this.CancelButton = this.cancel;
            this.Controls.Add(this.container);
            this.Controls.Add(this.cancel);
            this.Controls.Add(this.ok);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = null;
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
