namespace SysCallHacker
{
    partial class MainWindow
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainWindow));
            this.mainMenu = new System.Windows.Forms.MainMenu(this.components);
            this.hackerMenuItem = new System.Windows.Forms.MenuItem();
            this.clearHackerMenuItem = new System.Windows.Forms.MenuItem();
            this.exitMenuItem = new System.Windows.Forms.MenuItem();
            this.menuItem1 = new System.Windows.Forms.MenuItem();
            this.removeAllFiltersMenuItem = new System.Windows.Forms.MenuItem();
            this.addProcessFiltersMenuItem = new System.Windows.Forms.MenuItem();
            this.listEvents = new System.Windows.Forms.ListView();
            this.columnTime = new System.Windows.Forms.ColumnHeader();
            this.columnClient = new System.Windows.Forms.ColumnHeader();
            this.columnCall = new System.Windows.Forms.ColumnHeader();
            this.columnMode = new System.Windows.Forms.ColumnHeader();
            this.columnArguments = new System.Windows.Forms.ColumnHeader();
            this.imageList = new System.Windows.Forms.ImageList(this.components);
            this.toolBarButtonStop = new System.Windows.Forms.ToolBarButton();
            this.toolBarButtonStart = new System.Windows.Forms.ToolBarButton();
            this.toolBar = new System.Windows.Forms.ToolBar();
            this.statusBar = new System.Windows.Forms.StatusBar();
            this.timerUpdate = new System.Windows.Forms.Timer(this.components);
            this.SuspendLayout();



            this.mainMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.hackerMenuItem,
            this.menuItem1});



            this.hackerMenuItem.Index = 0;
            this.hackerMenuItem.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.clearHackerMenuItem,
            this.exitMenuItem});
            this.hackerMenuItem.Text = "Hacker";



            this.clearHackerMenuItem.Index = 0;
            this.clearHackerMenuItem.Text = "Clear";
            this.clearHackerMenuItem.Click += new System.EventHandler(this.clearHackerMenuItem_Click);



            this.exitMenuItem.Index = 1;
            this.exitMenuItem.Text = "E&xit";
            this.exitMenuItem.Click += new System.EventHandler(this.exitMenuItem_Click);



            this.menuItem1.Index = 1;
            this.menuItem1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.removeAllFiltersMenuItem,
            this.addProcessFiltersMenuItem});
            this.menuItem1.Text = "Filters";



            this.removeAllFiltersMenuItem.Index = 0;
            this.removeAllFiltersMenuItem.Text = "Remove All";
            this.removeAllFiltersMenuItem.Click += new System.EventHandler(this.removeAllFiltersMenuItem_Click);



            this.addProcessFiltersMenuItem.Index = 1;
            this.addProcessFiltersMenuItem.Text = "Add Process...";
            this.addProcessFiltersMenuItem.Click += new System.EventHandler(this.addProcessFiltersMenuItem_Click);



            this.listEvents.AllowColumnReorder = true;
            this.listEvents.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.listEvents.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnTime,
            this.columnClient,
            this.columnCall,
            this.columnMode,
            this.columnArguments});
            this.listEvents.FullRowSelect = true;
            this.listEvents.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
            this.listEvents.HideSelection = false;
            this.listEvents.Location = new System.Drawing.Point(0, 28);
            this.listEvents.Name = "listEvents";
            this.listEvents.ShowItemToolTips = true;
            this.listEvents.Size = new System.Drawing.Size(813, 435);
            this.listEvents.TabIndex = 1;
            this.listEvents.UseCompatibleStateImageBehavior = false;
            this.listEvents.View = System.Windows.Forms.View.Details;
            this.listEvents.VirtualMode = true;
            this.listEvents.DoubleClick += new System.EventHandler(this.listEvents_DoubleClick);
            this.listEvents.RetrieveVirtualItem += new System.Windows.Forms.RetrieveVirtualItemEventHandler(this.listEvents_RetrieveVirtualItem);



            this.columnTime.Text = "Time";
            this.columnTime.Width = 140;



            this.columnClient.Text = "Client";
            this.columnClient.Width = 120;



            this.columnCall.Text = "Call";
            this.columnCall.Width = 160;



            this.columnMode.Text = "Mode";
            this.columnMode.Width = 70;



            this.columnArguments.Text = "Arguments";
            this.columnArguments.Width = 300;



            this.imageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList.ImageStream")));
            this.imageList.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList.Images.SetKeyName(0, "control_play");
            this.imageList.Images.SetKeyName(1, "control_stop");



            this.toolBarButtonStop.ImageKey = "control_stop";
            this.toolBarButtonStop.Name = "toolBarButtonStop";
            this.toolBarButtonStop.ToolTipText = "Stop";



            this.toolBarButtonStart.ImageKey = "control_play";
            this.toolBarButtonStart.Name = "toolBarButtonStart";
            this.toolBarButtonStart.ToolTipText = "Start";



            this.toolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
            this.toolBar.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
            this.toolBarButtonStart,
            this.toolBarButtonStop});
            this.toolBar.DropDownArrows = true;
            this.toolBar.ImageList = this.imageList;
            this.toolBar.Location = new System.Drawing.Point(0, 0);
            this.toolBar.Name = "toolBar";
            this.toolBar.ShowToolTips = true;
            this.toolBar.Size = new System.Drawing.Size(813, 28);
            this.toolBar.TabIndex = 0;
            this.toolBar.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.toolBar_ButtonClick);



            this.statusBar.Location = new System.Drawing.Point(0, 462);
            this.statusBar.Name = "statusBar";
            this.statusBar.Size = new System.Drawing.Size(813, 22);
            this.statusBar.TabIndex = 2;



            this.timerUpdate.Enabled = true;
            this.timerUpdate.Interval = 500;
            this.timerUpdate.Tick += new System.EventHandler(this.timerUpdate_Tick);



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(813, 484);
            this.Controls.Add(this.statusBar);
            this.Controls.Add(this.listEvents);
            this.Controls.Add(this.toolBar);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Menu = this.mainMenu;
            this.Name = "MainWindow";
            this.Text = "System Call Hacker";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MainWindow_FormClosing);
            this.ResumeLayout(false);
            this.PerformLayout();

        }



        private System.Windows.Forms.MainMenu mainMenu;
        private System.Windows.Forms.MenuItem hackerMenuItem;
        private System.Windows.Forms.MenuItem exitMenuItem;
        private System.Windows.Forms.ListView listEvents;
        private System.Windows.Forms.ColumnHeader columnTime;
        private System.Windows.Forms.ColumnHeader columnCall;
        private System.Windows.Forms.ColumnHeader columnMode;
        private System.Windows.Forms.ColumnHeader columnArguments;
        private System.Windows.Forms.ImageList imageList;
        private System.Windows.Forms.ToolBarButton toolBarButtonStop;
        private System.Windows.Forms.ToolBarButton toolBarButtonStart;
        private System.Windows.Forms.ToolBar toolBar;
        private System.Windows.Forms.StatusBar statusBar;
        private System.Windows.Forms.Timer timerUpdate;
        private System.Windows.Forms.ColumnHeader columnClient;
        private System.Windows.Forms.MenuItem menuItem1;
        private System.Windows.Forms.MenuItem removeAllFiltersMenuItem;
        private System.Windows.Forms.MenuItem addProcessFiltersMenuItem;
        private System.Windows.Forms.MenuItem clearHackerMenuItem;


    }
}
