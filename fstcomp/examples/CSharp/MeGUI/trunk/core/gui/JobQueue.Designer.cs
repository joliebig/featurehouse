namespace MeGUI.core.gui
{
    partial class JobQueue
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.Windows.Forms.Button abortButton;
            System.Windows.Forms.Button loadJobButton;
            System.Windows.Forms.Button button9;
            this.jobColumHeader = new System.Windows.Forms.ColumnHeader();
            this.inputColumnHeader = new System.Windows.Forms.ColumnHeader();
            this.outputColumnHeader = new System.Windows.Forms.ColumnHeader();
            this.codecHeader = new System.Windows.Forms.ColumnHeader();
            this.modeHeader = new System.Windows.Forms.ColumnHeader();
            this.statusColumn = new System.Windows.Forms.ColumnHeader();
            this.startColumn = new System.Windows.Forms.ColumnHeader();
            this.endColumn = new System.Windows.Forms.ColumnHeader();
            this.fpsColumn = new System.Windows.Forms.ColumnHeader();
            this.upButton = new System.Windows.Forms.Button();
            this.downButton = new System.Windows.Forms.Button();
            this.queueListView = new System.Windows.Forms.ListView();
            this.ownerHeader = new System.Windows.Forms.ColumnHeader();
            this.queueContextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.DeleteMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.StatusMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.PostponedMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.WaitingMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.AbortMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.LoadMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.flowLayoutPanel1 = new System.Windows.Forms.FlowLayoutPanel();
            this.startStopButton = new System.Windows.Forms.Button();
            this.stopButton = new System.Windows.Forms.Button();
            this.pauseButton = new System.Windows.Forms.Button();
            abortButton = new System.Windows.Forms.Button();
            loadJobButton = new System.Windows.Forms.Button();
            button9 = new System.Windows.Forms.Button();
            this.queueContextMenu.SuspendLayout();
            this.flowLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // abortButton
            // 
            abortButton.AutoSize = true;
            abortButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            abortButton.Location = new System.Drawing.Point(123, 3);
            abortButton.Margin = new System.Windows.Forms.Padding(3, 3, 7, 3);
            abortButton.Name = "abortButton";
            abortButton.Size = new System.Drawing.Size(42, 23);
            abortButton.TabIndex = 3;
            abortButton.Text = "Abort";
            abortButton.UseVisualStyleBackColor = true;
            abortButton.Click += new System.EventHandler(this.abortButton_Click);
            // 
            // loadJobButton
            // 
            loadJobButton.AutoSize = true;
            loadJobButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            loadJobButton.Location = new System.Drawing.Point(179, 3);
            loadJobButton.Margin = new System.Windows.Forms.Padding(7, 3, 3, 3);
            loadJobButton.Name = "loadJobButton";
            loadJobButton.Size = new System.Drawing.Size(41, 23);
            loadJobButton.TabIndex = 4;
            loadJobButton.Text = "Load";
            loadJobButton.UseVisualStyleBackColor = true;
            loadJobButton.Click += new System.EventHandler(this.loadJobButton_Click);
            // 
            // button9
            // 
            button9.AutoSize = true;
            button9.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            button9.Location = new System.Drawing.Point(322, 3);
            button9.Name = "button9";
            button9.Size = new System.Drawing.Size(48, 23);
            button9.TabIndex = 8;
            button9.Text = "Delete";
            button9.UseVisualStyleBackColor = true;
            button9.Click += new System.EventHandler(this.deleteJobButton_Click);
            // 
            // jobColumHeader
            // 
            this.jobColumHeader.Text = "Name";
            // 
            // inputColumnHeader
            // 
            this.inputColumnHeader.Text = "Input";
            // 
            // outputColumnHeader
            // 
            this.outputColumnHeader.Text = "Output";
            // 
            // codecHeader
            // 
            this.codecHeader.Text = "Codec";
            // 
            // modeHeader
            // 
            this.modeHeader.Text = "Mode";
            // 
            // statusColumn
            // 
            this.statusColumn.Text = "Status";
            // 
            // startColumn
            // 
            this.startColumn.Text = "Start";
            // 
            // endColumn
            // 
            this.endColumn.Text = "End";
            // 
            // fpsColumn
            // 
            this.fpsColumn.Text = "FPS";
            // 
            // upButton
            // 
            this.upButton.AutoSize = true;
            this.upButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.upButton.Enabled = false;
            this.upButton.Location = new System.Drawing.Point(230, 3);
            this.upButton.Margin = new System.Windows.Forms.Padding(7, 3, 3, 3);
            this.upButton.Name = "upButton";
            this.upButton.Size = new System.Drawing.Size(31, 23);
            this.upButton.TabIndex = 6;
            this.upButton.Text = "Up";
            this.upButton.UseVisualStyleBackColor = true;
            this.upButton.Click += new System.EventHandler(this.upButton_Click);
            // 
            // downButton
            // 
            this.downButton.AutoSize = true;
            this.downButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.downButton.Enabled = false;
            this.downButton.Location = new System.Drawing.Point(267, 3);
            this.downButton.Margin = new System.Windows.Forms.Padding(3, 3, 7, 3);
            this.downButton.Name = "downButton";
            this.downButton.Size = new System.Drawing.Size(45, 23);
            this.downButton.TabIndex = 7;
            this.downButton.Text = "Down";
            this.downButton.UseVisualStyleBackColor = true;
            this.downButton.Click += new System.EventHandler(this.downButton_Click);
            // 
            // queueListView
            // 
            this.queueListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.jobColumHeader,
            this.inputColumnHeader,
            this.outputColumnHeader,
            this.codecHeader,
            this.modeHeader,
            this.statusColumn,
            this.ownerHeader,
            this.startColumn,
            this.endColumn,
            this.fpsColumn});
            this.queueListView.ContextMenuStrip = this.queueContextMenu;
            this.queueListView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.queueListView.FullRowSelect = true;
            this.queueListView.HideSelection = false;
            this.queueListView.Location = new System.Drawing.Point(0, 0);
            this.queueListView.Name = "queueListView";
            this.queueListView.Size = new System.Drawing.Size(692, 513);
            this.queueListView.TabIndex = 0;
            this.queueListView.UseCompatibleStateImageBehavior = false;
            this.queueListView.View = System.Windows.Forms.View.Details;
            this.queueListView.VisibleChanged += new System.EventHandler(this.queueListView_VisibleChanged);
            this.queueListView.DoubleClick += new System.EventHandler(this.queueListView_DoubleClick);
            this.queueListView.ItemSelectionChanged += new System.Windows.Forms.ListViewItemSelectionChangedEventHandler(this.queueListView_ItemSelectionChanged);
            this.queueListView.KeyDown += new System.Windows.Forms.KeyEventHandler(this.queueListView_KeyDown);
            // 
            // ownerHeader
            // 
            this.ownerHeader.Text = "Owner";
            // 
            // queueContextMenu
            // 
            this.queueContextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.DeleteMenuItem,
            this.StatusMenuItem,
            this.AbortMenuItem,
            this.LoadMenuItem});
            this.queueContextMenu.Name = "queueContextMenu";
            this.queueContextMenu.Size = new System.Drawing.Size(150, 92);
            this.queueContextMenu.Opened += new System.EventHandler(this.queueContextMenu_Opened);
            // 
            // DeleteMenuItem
            // 
            this.DeleteMenuItem.Name = "DeleteMenuItem";
            this.DeleteMenuItem.ShortcutKeyDisplayString = "";
            this.DeleteMenuItem.Size = new System.Drawing.Size(149, 22);
            this.DeleteMenuItem.Text = "&Delete";
            this.DeleteMenuItem.ToolTipText = "Delete this job";
            this.DeleteMenuItem.Click += new System.EventHandler(this.deleteJobButton_Click);
            // 
            // StatusMenuItem
            // 
            this.StatusMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.PostponedMenuItem,
            this.WaitingMenuItem});
            this.StatusMenuItem.Name = "StatusMenuItem";
            this.StatusMenuItem.Size = new System.Drawing.Size(149, 22);
            this.StatusMenuItem.Text = "&Change status";
            // 
            // PostponedMenuItem
            // 
            this.PostponedMenuItem.Name = "PostponedMenuItem";
            this.PostponedMenuItem.Size = new System.Drawing.Size(131, 22);
            this.PostponedMenuItem.Text = "&Postponed";
            this.PostponedMenuItem.Click += new System.EventHandler(this.postponeMenuItem_Click);
            // 
            // WaitingMenuItem
            // 
            this.WaitingMenuItem.Name = "WaitingMenuItem";
            this.WaitingMenuItem.Size = new System.Drawing.Size(131, 22);
            this.WaitingMenuItem.Text = "&Waiting";
            this.WaitingMenuItem.Click += new System.EventHandler(this.waitingMenuItem_Click);
            // 
            // AbortMenuItem
            // 
            this.AbortMenuItem.Name = "AbortMenuItem";
            this.AbortMenuItem.ShortcutKeyDisplayString = "";
            this.AbortMenuItem.Size = new System.Drawing.Size(149, 22);
            this.AbortMenuItem.Text = "&Abort";
            this.AbortMenuItem.ToolTipText = "Abort this job";
            this.AbortMenuItem.Click += new System.EventHandler(this.AbortMenuItem_Click);
            // 
            // LoadMenuItem
            // 
            this.LoadMenuItem.Enabled = false;
            this.LoadMenuItem.Name = "LoadMenuItem";
            this.LoadMenuItem.ShortcutKeyDisplayString = "";
            this.LoadMenuItem.Size = new System.Drawing.Size(149, 22);
            this.LoadMenuItem.Text = "&Load";
            this.LoadMenuItem.ToolTipText = "Load into MeGUI";
            // 
            // flowLayoutPanel1
            // 
            this.flowLayoutPanel1.AutoSize = true;
            this.flowLayoutPanel1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.flowLayoutPanel1.Controls.Add(this.startStopButton);
            this.flowLayoutPanel1.Controls.Add(this.stopButton);
            this.flowLayoutPanel1.Controls.Add(this.pauseButton);
            this.flowLayoutPanel1.Controls.Add(abortButton);
            this.flowLayoutPanel1.Controls.Add(loadJobButton);
            this.flowLayoutPanel1.Controls.Add(this.upButton);
            this.flowLayoutPanel1.Controls.Add(this.downButton);
            this.flowLayoutPanel1.Controls.Add(button9);
            this.flowLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.flowLayoutPanel1.Location = new System.Drawing.Point(0, 513);
            this.flowLayoutPanel1.Name = "flowLayoutPanel1";
            this.flowLayoutPanel1.Size = new System.Drawing.Size(692, 29);
            this.flowLayoutPanel1.TabIndex = 1;
            // 
            // startStopButton
            // 
            this.startStopButton.AutoSize = true;
            this.startStopButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.startStopButton.Location = new System.Drawing.Point(3, 3);
            this.startStopButton.Name = "startStopButton";
            this.startStopButton.Size = new System.Drawing.Size(39, 23);
            this.startStopButton.TabIndex = 0;
            this.startStopButton.Text = "Start";
            this.startStopButton.UseVisualStyleBackColor = true;
            this.startStopButton.Click += new System.EventHandler(this.startStopButton_Click);
            // 
            // stopButton
            // 
            this.stopButton.AutoSize = true;
            this.stopButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.stopButton.Location = new System.Drawing.Point(48, 3);
            this.stopButton.Name = "stopButton";
            this.stopButton.Size = new System.Drawing.Size(39, 23);
            this.stopButton.TabIndex = 1;
            this.stopButton.Text = "Stop";
            this.stopButton.UseVisualStyleBackColor = true;
            this.stopButton.Click += new System.EventHandler(this.stopButton_Click);
            // 
            // pauseButton
            // 
            this.pauseButton.Location = new System.Drawing.Point(93, 3);
            this.pauseButton.Name = "pauseButton";
            this.pauseButton.Size = new System.Drawing.Size(24, 23);
            this.pauseButton.TabIndex = 2;
            this.pauseButton.UseVisualStyleBackColor = true;
            this.pauseButton.Click += new System.EventHandler(this.pauseButton_Click);
            // 
            // JobQueue
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.Controls.Add(this.queueListView);
            this.Controls.Add(this.flowLayoutPanel1);
            this.Name = "JobQueue";
            this.Size = new System.Drawing.Size(692, 542);
            this.Load += new System.EventHandler(this.JobQueue_Load);
            this.queueContextMenu.ResumeLayout(false);
            this.flowLayoutPanel1.ResumeLayout(false);
            this.flowLayoutPanel1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ListView queueListView;
        private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;
        private System.Windows.Forms.Button startStopButton;
        private System.Windows.Forms.Button pauseButton;
        private System.Windows.Forms.Button upButton;
        private System.Windows.Forms.Button downButton;
        private System.Windows.Forms.ContextMenuStrip queueContextMenu;
        private System.Windows.Forms.ToolStripMenuItem DeleteMenuItem;
        private System.Windows.Forms.ToolStripMenuItem StatusMenuItem;
        private System.Windows.Forms.ToolStripMenuItem PostponedMenuItem;
        private System.Windows.Forms.ToolStripMenuItem WaitingMenuItem;
        private System.Windows.Forms.ToolStripMenuItem AbortMenuItem;
        private System.Windows.Forms.ToolStripMenuItem LoadMenuItem;
        private System.Windows.Forms.Button stopButton;
        private System.Windows.Forms.ColumnHeader ownerHeader;
        private System.Windows.Forms.ColumnHeader jobColumHeader;
        private System.Windows.Forms.ColumnHeader inputColumnHeader;
        private System.Windows.Forms.ColumnHeader outputColumnHeader;
        private System.Windows.Forms.ColumnHeader codecHeader;
        private System.Windows.Forms.ColumnHeader modeHeader;
        private System.Windows.Forms.ColumnHeader statusColumn;
        private System.Windows.Forms.ColumnHeader startColumn;
        private System.Windows.Forms.ColumnHeader endColumn;
        private System.Windows.Forms.ColumnHeader fpsColumn;
    }
}
