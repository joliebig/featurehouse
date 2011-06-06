namespace Eraser
{
 partial class SchedulerPanel
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SchedulerPanel));
            this.scheduler = new System.Windows.Forms.ListView();
            this.schedulerColName = new System.Windows.Forms.ColumnHeader();
            this.schedulerColNextRun = new System.Windows.Forms.ColumnHeader();
            this.schedulerColStatus = new System.Windows.Forms.ColumnHeader();
            this.schedulerMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.runNowToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.cancelTaskToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.viewTaskLogToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.editTaskToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.deleteTaskToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.schedulerProgress = new System.Windows.Forms.ProgressBar();
            this.schedulerDefaultMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.newTaskToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            ((System.ComponentModel.ISupportInitialize)(this.titleIcon)).BeginInit();
            this.content.SuspendLayout();
            this.schedulerMenu.SuspendLayout();
            this.schedulerDefaultMenu.SuspendLayout();
            this.SuspendLayout();
            this.titleLabel.AccessibleDescription = null;
            this.titleLabel.AccessibleName = null;
            resources.ApplyResources(this.titleLabel, "titleLabel");
            this.titleIcon.AccessibleDescription = null;
            this.titleIcon.AccessibleName = null;
            resources.ApplyResources(this.titleIcon, "titleIcon");
            this.titleIcon.BackgroundImage = null;
            this.titleIcon.Font = null;
            this.titleIcon.Image = global::Eraser.Properties.Resources.ToolbarSchedule;
            this.titleIcon.ImageLocation = null;
            this.content.AccessibleDescription = null;
            this.content.AccessibleName = null;
            resources.ApplyResources(this.content, "content");
            this.content.BackgroundImage = null;
            this.content.Controls.Add(this.schedulerProgress);
            this.content.Controls.Add(this.scheduler);
            this.content.Font = null;
            this.scheduler.AccessibleDescription = null;
            this.scheduler.AccessibleName = null;
            resources.ApplyResources(this.scheduler, "scheduler");
            this.scheduler.AllowDrop = true;
            this.scheduler.BackgroundImage = null;
            this.scheduler.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.schedulerColName,
            this.schedulerColNextRun,
            this.schedulerColStatus});
            this.scheduler.ContextMenuStrip = this.schedulerMenu;
            this.scheduler.Font = null;
            this.scheduler.FullRowSelect = true;
            this.scheduler.Groups.AddRange(new System.Windows.Forms.ListViewGroup[] {
            ((System.Windows.Forms.ListViewGroup)(resources.GetObject("scheduler.Groups"))),
            ((System.Windows.Forms.ListViewGroup)(resources.GetObject("scheduler.Groups1"))),
            ((System.Windows.Forms.ListViewGroup)(resources.GetObject("scheduler.Groups2")))});
            this.scheduler.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
            this.scheduler.Name = "scheduler";
            this.scheduler.OwnerDraw = true;
            this.scheduler.UseCompatibleStateImageBehavior = false;
            this.scheduler.View = System.Windows.Forms.View.Details;
            this.scheduler.DrawColumnHeader += new System.Windows.Forms.DrawListViewColumnHeaderEventHandler(this.scheduler_DrawColumnHeader);
            this.scheduler.ItemActivate += new System.EventHandler(this.scheduler_ItemActivate);
            this.scheduler.DragDrop += new System.Windows.Forms.DragEventHandler(this.scheduler_DragDrop);
            this.scheduler.DragEnter += new System.Windows.Forms.DragEventHandler(this.scheduler_DragEnter);
            this.scheduler.DragLeave += new System.EventHandler(this.scheduler_DragLeave);
            this.scheduler.KeyDown += new System.Windows.Forms.KeyEventHandler(this.scheduler_KeyDown);
            this.scheduler.DragOver += new System.Windows.Forms.DragEventHandler(this.scheduler_DragOver);
            this.scheduler.DrawSubItem += new System.Windows.Forms.DrawListViewSubItemEventHandler(this.scheduler_DrawSubItem);
            resources.ApplyResources(this.schedulerColName, "schedulerColName");
            resources.ApplyResources(this.schedulerColNextRun, "schedulerColNextRun");
            resources.ApplyResources(this.schedulerColStatus, "schedulerColStatus");
            this.schedulerMenu.AccessibleDescription = null;
            this.schedulerMenu.AccessibleName = null;
            resources.ApplyResources(this.schedulerMenu, "schedulerMenu");
            this.schedulerMenu.BackgroundImage = null;
            this.schedulerMenu.Font = null;
            this.schedulerMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.runNowToolStripMenuItem,
            this.cancelTaskToolStripMenuItem,
            this.toolStripSeparator2,
            this.viewTaskLogToolStripMenuItem,
            this.toolStripSeparator1,
            this.editTaskToolStripMenuItem,
            this.deleteTaskToolStripMenuItem});
            this.schedulerMenu.Name = "schedulerMenu";
            this.schedulerMenu.Opening += new System.ComponentModel.CancelEventHandler(this.schedulerMenu_Opening);
            this.runNowToolStripMenuItem.AccessibleDescription = null;
            this.runNowToolStripMenuItem.AccessibleName = null;
            resources.ApplyResources(this.runNowToolStripMenuItem, "runNowToolStripMenuItem");
            this.runNowToolStripMenuItem.BackgroundImage = null;
            this.runNowToolStripMenuItem.Name = "runNowToolStripMenuItem";
            this.runNowToolStripMenuItem.ShortcutKeyDisplayString = null;
            this.runNowToolStripMenuItem.Click += new System.EventHandler(this.runNowToolStripMenuItem_Click);
            this.cancelTaskToolStripMenuItem.AccessibleDescription = null;
            this.cancelTaskToolStripMenuItem.AccessibleName = null;
            resources.ApplyResources(this.cancelTaskToolStripMenuItem, "cancelTaskToolStripMenuItem");
            this.cancelTaskToolStripMenuItem.BackgroundImage = null;
            this.cancelTaskToolStripMenuItem.Name = "cancelTaskToolStripMenuItem";
            this.cancelTaskToolStripMenuItem.ShortcutKeyDisplayString = null;
            this.cancelTaskToolStripMenuItem.Click += new System.EventHandler(this.cancelTaskToolStripMenuItem_Click);
            this.toolStripSeparator2.AccessibleDescription = null;
            this.toolStripSeparator2.AccessibleName = null;
            resources.ApplyResources(this.toolStripSeparator2, "toolStripSeparator2");
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.viewTaskLogToolStripMenuItem.AccessibleDescription = null;
            this.viewTaskLogToolStripMenuItem.AccessibleName = null;
            resources.ApplyResources(this.viewTaskLogToolStripMenuItem, "viewTaskLogToolStripMenuItem");
            this.viewTaskLogToolStripMenuItem.BackgroundImage = null;
            this.viewTaskLogToolStripMenuItem.Name = "viewTaskLogToolStripMenuItem";
            this.viewTaskLogToolStripMenuItem.ShortcutKeyDisplayString = null;
            this.viewTaskLogToolStripMenuItem.Click += new System.EventHandler(this.viewTaskLogToolStripMenuItem_Click);
            this.toolStripSeparator1.AccessibleDescription = null;
            this.toolStripSeparator1.AccessibleName = null;
            resources.ApplyResources(this.toolStripSeparator1, "toolStripSeparator1");
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.editTaskToolStripMenuItem.AccessibleDescription = null;
            this.editTaskToolStripMenuItem.AccessibleName = null;
            resources.ApplyResources(this.editTaskToolStripMenuItem, "editTaskToolStripMenuItem");
            this.editTaskToolStripMenuItem.BackgroundImage = null;
            this.editTaskToolStripMenuItem.Name = "editTaskToolStripMenuItem";
            this.editTaskToolStripMenuItem.ShortcutKeyDisplayString = null;
            this.editTaskToolStripMenuItem.Click += new System.EventHandler(this.editTaskToolStripMenuItem_Click);
            this.deleteTaskToolStripMenuItem.AccessibleDescription = null;
            this.deleteTaskToolStripMenuItem.AccessibleName = null;
            resources.ApplyResources(this.deleteTaskToolStripMenuItem, "deleteTaskToolStripMenuItem");
            this.deleteTaskToolStripMenuItem.BackgroundImage = null;
            this.deleteTaskToolStripMenuItem.Name = "deleteTaskToolStripMenuItem";
            this.deleteTaskToolStripMenuItem.ShortcutKeyDisplayString = null;
            this.deleteTaskToolStripMenuItem.Click += new System.EventHandler(this.deleteTaskToolStripMenuItem_Click);
            this.schedulerProgress.AccessibleDescription = null;
            this.schedulerProgress.AccessibleName = null;
            resources.ApplyResources(this.schedulerProgress, "schedulerProgress");
            this.schedulerProgress.BackgroundImage = null;
            this.schedulerProgress.Font = null;
            this.schedulerProgress.Maximum = 1000;
            this.schedulerProgress.Name = "schedulerProgress";
            this.schedulerDefaultMenu.AccessibleDescription = null;
            this.schedulerDefaultMenu.AccessibleName = null;
            resources.ApplyResources(this.schedulerDefaultMenu, "schedulerDefaultMenu");
            this.schedulerDefaultMenu.BackgroundImage = null;
            this.schedulerDefaultMenu.Font = null;
            this.schedulerDefaultMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.newTaskToolStripMenuItem});
            this.schedulerDefaultMenu.Name = "schedulerDefaultMenu";
            this.newTaskToolStripMenuItem.AccessibleDescription = null;
            this.newTaskToolStripMenuItem.AccessibleName = null;
            resources.ApplyResources(this.newTaskToolStripMenuItem, "newTaskToolStripMenuItem");
            this.newTaskToolStripMenuItem.BackgroundImage = null;
            this.newTaskToolStripMenuItem.Name = "newTaskToolStripMenuItem";
            this.newTaskToolStripMenuItem.ShortcutKeyDisplayString = null;
            this.newTaskToolStripMenuItem.Click += new System.EventHandler(this.newTaskToolStripMenuItem_Click);
            this.AccessibleDescription = null;
            this.AccessibleName = null;
            resources.ApplyResources(this, "$this");
            this.BackgroundImage = null;
            this.DoubleBuffered = true;
            this.Font = null;
            this.Name = "SchedulerPanel";
            this.Controls.SetChildIndex(this.titleLabel, 0);
            this.Controls.SetChildIndex(this.titleIcon, 0);
            this.Controls.SetChildIndex(this.content, 0);
            ((System.ComponentModel.ISupportInitialize)(this.titleIcon)).EndInit();
            this.content.ResumeLayout(false);
            this.schedulerMenu.ResumeLayout(false);
            this.schedulerDefaultMenu.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();
  }
  private System.Windows.Forms.ColumnHeader schedulerColName;
  private System.Windows.Forms.ColumnHeader schedulerColNextRun;
  private System.Windows.Forms.ColumnHeader schedulerColStatus;
  private System.Windows.Forms.ListView scheduler;
  private System.Windows.Forms.ProgressBar schedulerProgress;
  private System.Windows.Forms.ContextMenuStrip schedulerMenu;
  private System.Windows.Forms.ToolStripMenuItem runNowToolStripMenuItem;
  private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
  private System.Windows.Forms.ToolStripMenuItem viewTaskLogToolStripMenuItem;
  private System.Windows.Forms.ToolStripMenuItem editTaskToolStripMenuItem;
  private System.Windows.Forms.ToolStripMenuItem deleteTaskToolStripMenuItem;
  private System.Windows.Forms.ToolStripMenuItem cancelTaskToolStripMenuItem;
  private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
  private System.Windows.Forms.ContextMenuStrip schedulerDefaultMenu;
  private System.Windows.Forms.ToolStripMenuItem newTaskToolStripMenuItem;
 }
}
