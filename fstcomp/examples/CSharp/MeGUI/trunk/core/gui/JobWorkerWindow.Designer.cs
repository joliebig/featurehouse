namespace MeGUI.core.gui
{
    partial class JobWorker
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

        

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.Windows.Forms.GroupBox groupBox1;
            MeGUI.core.gui.HelpButton helpButton1;
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(JobWorker));
            this.jobQueue1 = new MeGUI.core.gui.JobQueue();
            this.panel1 = new System.Windows.Forms.Panel();
            this.progressLabel = new System.Windows.Forms.Label();
            this.jobProgress = new System.Windows.Forms.ProgressBar();
            this.flowLayoutPanel2 = new System.Windows.Forms.FlowLayoutPanel();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.workerToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.changeNameToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.shutDownWhenFinishedLocalQueueToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.shutDownWorkerNowToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.progressWindowToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.showProgressWindowToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            groupBox1 = new System.Windows.Forms.GroupBox();
            helpButton1 = new MeGUI.core.gui.HelpButton();
            groupBox1.SuspendLayout();
            this.panel1.SuspendLayout();
            this.flowLayoutPanel2.SuspendLayout();
            this.menuStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            groupBox1.Controls.Add(this.jobQueue1);
            groupBox1.Controls.Add(this.panel1);
            groupBox1.Dock = System.Windows.Forms.DockStyle.Fill;
            groupBox1.Location = new System.Drawing.Point(4, 28);
            groupBox1.Name = "groupBox1";
            groupBox1.Size = new System.Drawing.Size(557, 437);
            groupBox1.TabIndex = 33;
            groupBox1.TabStop = false;
            groupBox1.Text = "Worker-Local Queue";
            // 
            // jobQueue1
            // 
            this.jobQueue1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.jobQueue1.Location = new System.Drawing.Point(3, 16);
            this.jobQueue1.Name = "jobQueue1";
            this.jobQueue1.Padding = new System.Windows.Forms.Padding(2);
            this.jobQueue1.PauseResumeMode = MeGUI.core.gui.PauseResumeMode.Disabled;
            this.jobQueue1.SaveSettings = true;
            this.jobQueue1.SettingsKey = "JobQueue";
            this.jobQueue1.Size = new System.Drawing.Size(551, 392);
            this.jobQueue1.StartStopMode = MeGUI.core.gui.StartStopMode.Start;
            this.jobQueue1.TabIndex = 0;
            this.jobQueue1.StartClicked += new System.EventHandler(this.jobQueue1_StartClicked);
            this.jobQueue1.AbortClicked += new System.EventHandler(this.jobQueue1_AbortClicked);
            this.jobQueue1.StopClicked += new System.EventHandler(this.jobQueue1_StopClicked);
            // 
            // panel1
            // 
            this.panel1.AutoSize = true;
            this.panel1.Controls.Add(this.progressLabel);
            this.panel1.Controls.Add(this.jobProgress);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel1.Location = new System.Drawing.Point(3, 408);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(551, 26);
            this.panel1.TabIndex = 2;
            // 
            // progressLabel
            // 
            this.progressLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.progressLabel.Location = new System.Drawing.Point(0, 0);
            this.progressLabel.Margin = new System.Windows.Forms.Padding(3);
            this.progressLabel.Name = "progressLabel";
            this.progressLabel.Padding = new System.Windows.Forms.Padding(0, 5, 0, 5);
            this.progressLabel.Size = new System.Drawing.Size(55, 26);
            this.progressLabel.TabIndex = 29;
            this.progressLabel.Text = "Progress";
            this.progressLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // jobProgress
            // 
            this.jobProgress.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.jobProgress.Location = new System.Drawing.Point(58, 3);
            this.jobProgress.Margin = new System.Windows.Forms.Padding(0);
            this.jobProgress.Name = "jobProgress";
            this.jobProgress.Size = new System.Drawing.Size(493, 23);
            this.jobProgress.TabIndex = 30;
            // 
            // helpButton1
            // 
            helpButton1.ArticleName = "Job Worker Window";
            helpButton1.AutoSize = true;
            helpButton1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            helpButton1.Location = new System.Drawing.Point(3, 3);
            helpButton1.Name = "helpButton1";
            helpButton1.Size = new System.Drawing.Size(48, 23);
            helpButton1.TabIndex = 36;
            // 
            // flowLayoutPanel2
            // 
            this.flowLayoutPanel2.AutoSize = true;
            this.flowLayoutPanel2.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.flowLayoutPanel2.Controls.Add(helpButton1);
            this.flowLayoutPanel2.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.flowLayoutPanel2.Location = new System.Drawing.Point(4, 465);
            this.flowLayoutPanel2.Name = "flowLayoutPanel2";
            this.flowLayoutPanel2.Size = new System.Drawing.Size(557, 29);
            this.flowLayoutPanel2.TabIndex = 36;
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.workerToolStripMenuItem,
            this.progressWindowToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(4, 4);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(557, 24);
            this.menuStrip1.TabIndex = 37;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // workerToolStripMenuItem
            // 
            this.workerToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.changeNameToolStripMenuItem,
            this.toolStripSeparator1,
            this.shutDownWhenFinishedLocalQueueToolStripMenuItem,
            this.shutDownWorkerNowToolStripMenuItem});
            this.workerToolStripMenuItem.Name = "workerToolStripMenuItem";
            this.workerToolStripMenuItem.Size = new System.Drawing.Size(57, 20);
            this.workerToolStripMenuItem.Text = "Worker";
            // 
            // changeNameToolStripMenuItem
            // 
            this.changeNameToolStripMenuItem.Name = "changeNameToolStripMenuItem";
            this.changeNameToolStripMenuItem.ShortcutKeys = System.Windows.Forms.Keys.F2;
            this.changeNameToolStripMenuItem.Size = new System.Drawing.Size(272, 22);
            this.changeNameToolStripMenuItem.Text = "Change name";
            this.changeNameToolStripMenuItem.Click += new System.EventHandler(this.changeNameToolStripMenuItem_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(269, 6);
            // 
            // shutDownWhenFinishedLocalQueueToolStripMenuItem
            // 
            this.shutDownWhenFinishedLocalQueueToolStripMenuItem.Name = "shutDownWhenFinishedLocalQueueToolStripMenuItem";
            this.shutDownWhenFinishedLocalQueueToolStripMenuItem.Size = new System.Drawing.Size(272, 22);
            this.shutDownWhenFinishedLocalQueueToolStripMenuItem.Text = "Shut down when finished local queue";
            this.shutDownWhenFinishedLocalQueueToolStripMenuItem.Click += new System.EventHandler(this.shutDownWhenFinishedLocalQueueToolStripMenuItem_Click);
            // 
            // shutDownWorkerNowToolStripMenuItem
            // 
            this.shutDownWorkerNowToolStripMenuItem.Name = "shutDownWorkerNowToolStripMenuItem";
            this.shutDownWorkerNowToolStripMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.W)));
            this.shutDownWorkerNowToolStripMenuItem.Size = new System.Drawing.Size(272, 22);
            this.shutDownWorkerNowToolStripMenuItem.Text = "Shut down worker now";
            this.shutDownWorkerNowToolStripMenuItem.Click += new System.EventHandler(this.shutDownWorkerNowToolStripMenuItem_Click);
            // 
            // progressWindowToolStripMenuItem
            // 
            this.progressWindowToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.showProgressWindowToolStripMenuItem});
            this.progressWindowToolStripMenuItem.Name = "progressWindowToolStripMenuItem";
            this.progressWindowToolStripMenuItem.Size = new System.Drawing.Size(111, 20);
            this.progressWindowToolStripMenuItem.Text = "Progress Window";
            this.progressWindowToolStripMenuItem.DropDownOpened += new System.EventHandler(this.progressWindowToolStripMenuItem_DropDownOpened);
            // 
            // showProgressWindowToolStripMenuItem
            // 
            this.showProgressWindowToolStripMenuItem.Name = "showProgressWindowToolStripMenuItem";
            this.showProgressWindowToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.showProgressWindowToolStripMenuItem.Text = "Show Progress Window";
            this.showProgressWindowToolStripMenuItem.Click += new System.EventHandler(this.showProgressWindowToolStripMenuItem_Click);
            // 
            // JobWorker
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = global::MeGUI.Properties.Settings.Default.JobWorkerSize;
            this.Controls.Add(groupBox1);
            this.Controls.Add(this.flowLayoutPanel2);
            this.Controls.Add(this.menuStrip1);
            this.DataBindings.Add(new System.Windows.Forms.Binding("ClientSize", global::MeGUI.Properties.Settings.Default, "JobWorkerSize", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.DataBindings.Add(new System.Windows.Forms.Binding("WindowState", global::MeGUI.Properties.Settings.Default, "JobWorkerWindowState", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "JobWorker";
            this.Padding = new System.Windows.Forms.Padding(4);
            this.Text = "Job Worker Window";
            this.WindowState = global::MeGUI.Properties.Settings.Default.JobWorkerWindowState;
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.JobWorker_FormClosed);
            groupBox1.ResumeLayout(false);
            groupBox1.PerformLayout();
            ((System.Configuration.IPersistComponentSettings)(this.jobQueue1)).LoadComponentSettings();
            this.panel1.ResumeLayout(false);
            this.flowLayoutPanel2.ResumeLayout(false);
            this.flowLayoutPanel2.PerformLayout();
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        

        private JobQueue jobQueue1;
        private System.Windows.Forms.ProgressBar jobProgress;
        private System.Windows.Forms.Label progressLabel;
        private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel2;
        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem workerToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem progressWindowToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem showProgressWindowToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem changeNameToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripMenuItem shutDownWhenFinishedLocalQueueToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem shutDownWorkerNowToolStripMenuItem;
        private System.Windows.Forms.Panel panel1;
    }
}
