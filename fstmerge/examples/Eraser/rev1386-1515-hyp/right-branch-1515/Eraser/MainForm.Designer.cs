namespace Eraser
{
 partial class MainForm
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
   System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
   this.contentPanel = new System.Windows.Forms.Panel();
   this.tbSchedule = new System.Windows.Forms.ToolStripMenuItem();
   this.notificationIcon = new System.Windows.Forms.NotifyIcon(this.components);
   this.notificationMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
   this.openEraserToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripSeparator();
   this.hideWhenMinimisedToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.notificationIconTimer = new System.Windows.Forms.Timer(this.components);
   this.ToolBar = new Eraser.ToolBar();
   this.tbScheduleDropDown = new System.Windows.Forms.ToolStripMenuItem();
   this.tbScheduleMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
   this.newTaskToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.toolStripMenuItem2 = new System.Windows.Forms.ToolStripSeparator();
   this.exportTaskListToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.importTaskListToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.tbSettings = new System.Windows.Forms.ToolStripMenuItem();
   this.tbHelp = new System.Windows.Forms.ToolStripMenuItem();
   this.tbHelpDropDown = new System.Windows.Forms.ToolStripMenuItem();
   this.tbHelpMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
   this.checkForUpdatesToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
   this.aboutEraserToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.checkForUpdatesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
   this.eraserLogo = new System.Windows.Forms.PictureBox();
   this.notificationMenu.SuspendLayout();
   this.ToolBar.SuspendLayout();
   this.tbScheduleMenu.SuspendLayout();
   this.tbHelpMenu.SuspendLayout();
   ((System.ComponentModel.ISupportInitialize)(this.eraserLogo)).BeginInit();
   this.SuspendLayout();
   this.contentPanel.AccessibleDescription = null;
   this.contentPanel.AccessibleName = null;
   resources.ApplyResources(this.contentPanel, "contentPanel");
   this.contentPanel.BackColor = System.Drawing.Color.White;
   this.contentPanel.BackgroundImage = null;
   this.contentPanel.Font = null;
   this.contentPanel.Name = "contentPanel";
   this.tbSchedule.AccessibleDescription = null;
   this.tbSchedule.AccessibleName = null;
   resources.ApplyResources(this.tbSchedule, "tbSchedule");
   this.tbSchedule.BackgroundImage = null;
   this.tbSchedule.Image = global::Eraser.Properties.Resources.ToolbarSchedule;
   this.tbSchedule.Name = "tbSchedule";
   this.tbSchedule.Padding = new System.Windows.Forms.Padding(0);
   this.tbSchedule.ShortcutKeyDisplayString = null;
   this.tbSchedule.Click += new System.EventHandler(this.tbSchedule_Click);
   resources.ApplyResources(this.notificationIcon, "notificationIcon");
   this.notificationIcon.ContextMenuStrip = this.notificationMenu;
   this.notificationIcon.DoubleClick += new System.EventHandler(this.openToolStripMenuItem_Click);
   this.notificationMenu.AccessibleDescription = null;
   this.notificationMenu.AccessibleName = null;
   resources.ApplyResources(this.notificationMenu, "notificationMenu");
   this.notificationMenu.BackgroundImage = null;
   this.notificationMenu.Font = null;
   this.notificationMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.openEraserToolStripMenuItem,
            this.toolStripMenuItem1,
            this.hideWhenMinimisedToolStripMenuItem,
            this.exitToolStripMenuItem});
   this.notificationMenu.Name = "notificationMenu";
   this.openEraserToolStripMenuItem.AccessibleDescription = null;
   this.openEraserToolStripMenuItem.AccessibleName = null;
   resources.ApplyResources(this.openEraserToolStripMenuItem, "openEraserToolStripMenuItem");
   this.openEraserToolStripMenuItem.BackgroundImage = null;
   this.openEraserToolStripMenuItem.Name = "openEraserToolStripMenuItem";
   this.openEraserToolStripMenuItem.ShortcutKeyDisplayString = null;
   this.openEraserToolStripMenuItem.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
   this.toolStripMenuItem1.AccessibleDescription = null;
   this.toolStripMenuItem1.AccessibleName = null;
   resources.ApplyResources(this.toolStripMenuItem1, "toolStripMenuItem1");
   this.toolStripMenuItem1.Name = "toolStripMenuItem1";
   this.hideWhenMinimisedToolStripMenuItem.AccessibleDescription = null;
   this.hideWhenMinimisedToolStripMenuItem.AccessibleName = null;
   resources.ApplyResources(this.hideWhenMinimisedToolStripMenuItem, "hideWhenMinimisedToolStripMenuItem");
   this.hideWhenMinimisedToolStripMenuItem.BackgroundImage = null;
   this.hideWhenMinimisedToolStripMenuItem.CheckOnClick = true;
   this.hideWhenMinimisedToolStripMenuItem.Name = "hideWhenMinimisedToolStripMenuItem";
   this.hideWhenMinimisedToolStripMenuItem.ShortcutKeyDisplayString = null;
   this.hideWhenMinimisedToolStripMenuItem.Click += new System.EventHandler(this.hideWhenMinimiseToolStripMenuItem_Click);
   this.exitToolStripMenuItem.AccessibleDescription = null;
   this.exitToolStripMenuItem.AccessibleName = null;
   resources.ApplyResources(this.exitToolStripMenuItem, "exitToolStripMenuItem");
   this.exitToolStripMenuItem.BackgroundImage = null;
   this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
   this.exitToolStripMenuItem.ShortcutKeyDisplayString = null;
   this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
   this.notificationIconTimer.Tick += new System.EventHandler(this.notificationIconTimer_Tick);
   this.ToolBar.AccessibleDescription = null;
   this.ToolBar.AccessibleName = null;
   this.ToolBar.AllowItemReorder = true;
   resources.ApplyResources(this.ToolBar, "ToolBar");
   this.ToolBar.BackColor = System.Drawing.Color.Transparent;
   this.ToolBar.BackgroundImage = null;
   this.ToolBar.Font = null;
   this.ToolBar.ImageScalingSize = new System.Drawing.Size(24, 24);
   this.ToolBar.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.tbSchedule,
            this.tbScheduleDropDown,
            this.tbSettings,
            this.tbHelp,
            this.tbHelpDropDown});
   this.ToolBar.Name = "ToolBar";
   this.tbScheduleDropDown.AccessibleDescription = null;
   this.tbScheduleDropDown.AccessibleName = null;
   resources.ApplyResources(this.tbScheduleDropDown, "tbScheduleDropDown");
   this.tbScheduleDropDown.BackgroundImage = null;
   this.tbScheduleDropDown.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
   this.tbScheduleDropDown.DropDown = this.tbScheduleMenu;
   this.tbScheduleDropDown.Image = global::Eraser.Properties.Resources.ToolbarArrow;
   this.tbScheduleDropDown.Margin = new System.Windows.Forms.Padding(0, 0, 10, 0);
   this.tbScheduleDropDown.Name = "tbScheduleDropDown";
   this.tbScheduleDropDown.Padding = new System.Windows.Forms.Padding(0);
   this.tbScheduleDropDown.ShortcutKeyDisplayString = null;
   this.tbScheduleMenu.AccessibleDescription = null;
   this.tbScheduleMenu.AccessibleName = null;
   resources.ApplyResources(this.tbScheduleMenu, "tbScheduleMenu");
   this.tbScheduleMenu.BackgroundImage = null;
   this.tbScheduleMenu.Font = null;
   this.tbScheduleMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.newTaskToolStripMenuItem,
            this.toolStripMenuItem2,
            this.exportTaskListToolStripMenuItem,
            this.importTaskListToolStripMenuItem});
   this.tbScheduleMenu.Name = "tbScheduleMenu";
   this.tbScheduleMenu.OwnerItem = this.tbScheduleDropDown;
   this.newTaskToolStripMenuItem.AccessibleDescription = null;
   this.newTaskToolStripMenuItem.AccessibleName = null;
   resources.ApplyResources(this.newTaskToolStripMenuItem, "newTaskToolStripMenuItem");
   this.newTaskToolStripMenuItem.BackgroundImage = null;
   this.newTaskToolStripMenuItem.Name = "newTaskToolStripMenuItem";
   this.newTaskToolStripMenuItem.ShortcutKeyDisplayString = null;
   this.newTaskToolStripMenuItem.Click += new System.EventHandler(this.newTaskToolStripMenuItem_Click);
   this.toolStripMenuItem2.AccessibleDescription = null;
   this.toolStripMenuItem2.AccessibleName = null;
   resources.ApplyResources(this.toolStripMenuItem2, "toolStripMenuItem2");
   this.toolStripMenuItem2.Name = "toolStripMenuItem2";
   this.exportTaskListToolStripMenuItem.AccessibleDescription = null;
   this.exportTaskListToolStripMenuItem.AccessibleName = null;
   resources.ApplyResources(this.exportTaskListToolStripMenuItem, "exportTaskListToolStripMenuItem");
   this.exportTaskListToolStripMenuItem.BackgroundImage = null;
   this.exportTaskListToolStripMenuItem.Name = "exportTaskListToolStripMenuItem";
   this.exportTaskListToolStripMenuItem.ShortcutKeyDisplayString = null;
   this.exportTaskListToolStripMenuItem.Click += new System.EventHandler(this.exportTaskListToolStripMenuItem_Click);
   this.importTaskListToolStripMenuItem.AccessibleDescription = null;
   this.importTaskListToolStripMenuItem.AccessibleName = null;
   resources.ApplyResources(this.importTaskListToolStripMenuItem, "importTaskListToolStripMenuItem");
   this.importTaskListToolStripMenuItem.BackgroundImage = null;
   this.importTaskListToolStripMenuItem.Name = "importTaskListToolStripMenuItem";
   this.importTaskListToolStripMenuItem.ShortcutKeyDisplayString = null;
   this.importTaskListToolStripMenuItem.Click += new System.EventHandler(this.importTaskListToolStripMenuItem_Click);
   this.tbSettings.AccessibleDescription = null;
   this.tbSettings.AccessibleName = null;
   resources.ApplyResources(this.tbSettings, "tbSettings");
   this.tbSettings.BackgroundImage = null;
   this.tbSettings.Image = global::Eraser.Properties.Resources.ToolbarSettings;
   this.tbSettings.Margin = new System.Windows.Forms.Padding(0, 0, 10, 0);
   this.tbSettings.Name = "tbSettings";
   this.tbSettings.Padding = new System.Windows.Forms.Padding(0);
   this.tbSettings.ShortcutKeyDisplayString = null;
   this.tbSettings.Click += new System.EventHandler(this.tbSettings_Click);
   this.tbHelp.AccessibleDescription = null;
   this.tbHelp.AccessibleName = null;
   resources.ApplyResources(this.tbHelp, "tbHelp");
   this.tbHelp.BackgroundImage = null;
   this.tbHelp.Image = global::Eraser.Properties.Resources.ToolbarHelp;
   this.tbHelp.Name = "tbHelp";
   this.tbHelp.Padding = new System.Windows.Forms.Padding(0);
   this.tbHelp.ShortcutKeyDisplayString = null;
   this.tbHelp.Click += new System.EventHandler(this.tbHelp_Click);
   this.tbHelpDropDown.AccessibleDescription = null;
   this.tbHelpDropDown.AccessibleName = null;
   resources.ApplyResources(this.tbHelpDropDown, "tbHelpDropDown");
   this.tbHelpDropDown.BackgroundImage = null;
   this.tbHelpDropDown.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
   this.tbHelpDropDown.DropDown = this.tbHelpMenu;
   this.tbHelpDropDown.Image = global::Eraser.Properties.Resources.ToolbarArrow;
   this.tbHelpDropDown.Name = "tbHelpDropDown";
   this.tbHelpDropDown.Padding = new System.Windows.Forms.Padding(0);
   this.tbHelpDropDown.ShortcutKeyDisplayString = null;
   this.tbHelpMenu.AccessibleDescription = null;
   this.tbHelpMenu.AccessibleName = null;
   resources.ApplyResources(this.tbHelpMenu, "tbHelpMenu");
   this.tbHelpMenu.BackgroundImage = null;
   this.tbHelpMenu.Font = null;
   this.tbHelpMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.checkForUpdatesToolStripMenuItem1,
            this.aboutEraserToolStripMenuItem});
   this.tbHelpMenu.Name = "tbHelpMenu";
   this.tbHelpMenu.OwnerItem = this.tbHelpDropDown;
   this.checkForUpdatesToolStripMenuItem1.AccessibleDescription = null;
   this.checkForUpdatesToolStripMenuItem1.AccessibleName = null;
   resources.ApplyResources(this.checkForUpdatesToolStripMenuItem1, "checkForUpdatesToolStripMenuItem1");
   this.checkForUpdatesToolStripMenuItem1.BackgroundImage = null;
   this.checkForUpdatesToolStripMenuItem1.Name = "checkForUpdatesToolStripMenuItem1";
   this.checkForUpdatesToolStripMenuItem1.ShortcutKeyDisplayString = null;
   this.checkForUpdatesToolStripMenuItem1.Click += new System.EventHandler(this.checkForUpdatesToolStripMenuItem_Click);
   this.aboutEraserToolStripMenuItem.AccessibleDescription = null;
   this.aboutEraserToolStripMenuItem.AccessibleName = null;
   resources.ApplyResources(this.aboutEraserToolStripMenuItem, "aboutEraserToolStripMenuItem");
   this.aboutEraserToolStripMenuItem.BackgroundImage = null;
   this.aboutEraserToolStripMenuItem.Name = "aboutEraserToolStripMenuItem";
   this.aboutEraserToolStripMenuItem.ShortcutKeyDisplayString = null;
   this.aboutEraserToolStripMenuItem.Click += new System.EventHandler(this.aboutEraserToolStripMenuItem_Click);
   this.checkForUpdatesToolStripMenuItem.AccessibleDescription = null;
   this.checkForUpdatesToolStripMenuItem.AccessibleName = null;
   resources.ApplyResources(this.checkForUpdatesToolStripMenuItem, "checkForUpdatesToolStripMenuItem");
   this.checkForUpdatesToolStripMenuItem.BackgroundImage = null;
   this.checkForUpdatesToolStripMenuItem.Name = "checkForUpdatesToolStripMenuItem";
   this.checkForUpdatesToolStripMenuItem.ShortcutKeyDisplayString = null;
   this.checkForUpdatesToolStripMenuItem.Click += new System.EventHandler(this.checkForUpdatesToolStripMenuItem_Click);
   this.eraserLogo.AccessibleDescription = null;
   this.eraserLogo.AccessibleName = null;
   resources.ApplyResources(this.eraserLogo, "eraserLogo");
   this.eraserLogo.BackColor = System.Drawing.Color.Transparent;
   this.eraserLogo.BackgroundImage = null;
   this.eraserLogo.Cursor = System.Windows.Forms.Cursors.Hand;
   this.eraserLogo.Font = null;
   this.eraserLogo.Image = global::Eraser.Properties.Resources.BackgroundLogo;
   this.eraserLogo.ImageLocation = null;
   this.eraserLogo.Name = "eraserLogo";
   this.eraserLogo.TabStop = false;
   this.eraserLogo.Click += new System.EventHandler(this.eraserLogo_Click);
   this.AccessibleDescription = null;
   this.AccessibleName = null;
   resources.ApplyResources(this, "$this");
   this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
   this.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(41)))), ((int)(((byte)(41)))), ((int)(((byte)(41)))));
   this.BackgroundImage = null;
   this.Controls.Add(this.eraserLogo);
   this.Controls.Add(this.contentPanel);
   this.Controls.Add(this.ToolBar);
   this.DoubleBuffered = true;
   this.MainMenuStrip = this.ToolBar;
   this.Name = "MainForm";
   this.Paint += new System.Windows.Forms.PaintEventHandler(this.MainForm_Paint);
   this.VisibleChanged += new System.EventHandler(this.MainForm_VisibleChanged);
   this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MainForm_FormClosing);
   this.Resize += new System.EventHandler(this.MainForm_Resize);
   this.notificationMenu.ResumeLayout(false);
   this.ToolBar.ResumeLayout(false);
   this.ToolBar.PerformLayout();
   this.tbScheduleMenu.ResumeLayout(false);
   this.tbHelpMenu.ResumeLayout(false);
   ((System.ComponentModel.ISupportInitialize)(this.eraserLogo)).EndInit();
   this.ResumeLayout(false);
   this.PerformLayout();
  }
  private System.Windows.Forms.Panel contentPanel;
  private System.Windows.Forms.NotifyIcon notificationIcon;
  private System.Windows.Forms.Timer notificationIconTimer;
  private System.Windows.Forms.ContextMenuStrip notificationMenu;
  private System.Windows.Forms.ToolStripMenuItem openEraserToolStripMenuItem;
  private System.Windows.Forms.ToolStripSeparator toolStripMenuItem1;
  private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
  private System.Windows.Forms.ToolStripMenuItem hideWhenMinimisedToolStripMenuItem;
  private ToolBar ToolBar;
  private System.Windows.Forms.ToolStripMenuItem tbSchedule;
  private System.Windows.Forms.ToolStripMenuItem tbSettings;
  private System.Windows.Forms.ToolStripMenuItem checkForUpdatesToolStripMenuItem;
  private System.Windows.Forms.ToolStripMenuItem tbHelp;
  private System.Windows.Forms.ToolStripMenuItem tbScheduleDropDown;
  private System.Windows.Forms.ToolStripMenuItem tbHelpDropDown;
  private System.Windows.Forms.ContextMenuStrip tbScheduleMenu;
  private System.Windows.Forms.ToolStripMenuItem newTaskToolStripMenuItem;
  private System.Windows.Forms.ContextMenuStrip tbHelpMenu;
  private System.Windows.Forms.ToolStripMenuItem checkForUpdatesToolStripMenuItem1;
  private System.Windows.Forms.ToolStripMenuItem aboutEraserToolStripMenuItem;
  private System.Windows.Forms.ToolStripSeparator toolStripMenuItem2;
  private System.Windows.Forms.ToolStripMenuItem exportTaskListToolStripMenuItem;
  private System.Windows.Forms.ToolStripMenuItem importTaskListToolStripMenuItem;
  private System.Windows.Forms.PictureBox eraserLogo;
 }
}
