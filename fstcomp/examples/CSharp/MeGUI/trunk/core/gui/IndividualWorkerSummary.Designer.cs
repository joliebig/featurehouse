namespace MeGUI.core.gui
{
    partial class IndividualWorkerSummary
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
            this.components = new System.ComponentModel.Container();
            this.workerNameAndJob = new System.Windows.Forms.GroupBox();
            this.progressBar1 = new System.Windows.Forms.ProgressBar();
            this.contextMenuStrip1 = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.startEncodingToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.renameToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.abortToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.stopToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.shutDownToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.showProgressWindowToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.showQueueToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.workerNameAndJob.SuspendLayout();
            this.contextMenuStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // workerNameAndJob
            // 
            this.workerNameAndJob.Controls.Add(this.progressBar1);
            this.workerNameAndJob.Dock = System.Windows.Forms.DockStyle.Fill;
            this.workerNameAndJob.Location = new System.Drawing.Point(0, 0);
            this.workerNameAndJob.Name = "workerNameAndJob";
            this.workerNameAndJob.Padding = new System.Windows.Forms.Padding(5);
            this.workerNameAndJob.Size = new System.Drawing.Size(292, 47);
            this.workerNameAndJob.TabIndex = 0;
            this.workerNameAndJob.TabStop = false;
            this.workerNameAndJob.Text = "[Name]: [status]";
            // 
            // progressBar1
            // 
            this.progressBar1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.progressBar1.Location = new System.Drawing.Point(5, 18);
            this.progressBar1.Name = "progressBar1";
            this.progressBar1.Size = new System.Drawing.Size(282, 24);
            this.progressBar1.TabIndex = 0;
            // 
            // contextMenuStrip1
            // 
            this.contextMenuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.startEncodingToolStripMenuItem,
            this.abortToolStripMenuItem,
            this.renameToolStripMenuItem,
            this.stopToolStripMenuItem,
            this.shutDownToolStripMenuItem,
            this.showProgressWindowToolStripMenuItem,
            this.showQueueToolStripMenuItem});
            this.contextMenuStrip1.Name = "contextMenuStrip1";
            this.contextMenuStrip1.Size = new System.Drawing.Size(185, 158);
            this.contextMenuStrip1.Opening += new System.ComponentModel.CancelEventHandler(this.contextMenuStrip1_Opening);
            // 
            // startEncodingToolStripMenuItem
            // 
            this.startEncodingToolStripMenuItem.Name = "startEncodingToolStripMenuItem";
            this.startEncodingToolStripMenuItem.Size = new System.Drawing.Size(184, 22);
            this.startEncodingToolStripMenuItem.Text = "Start encoding";
            this.startEncodingToolStripMenuItem.Click += new System.EventHandler(this.startEncodingToolStripMenuItem_Click);
            // 
            // renameToolStripMenuItem
            // 
            this.renameToolStripMenuItem.Name = "renameToolStripMenuItem";
            this.renameToolStripMenuItem.Size = new System.Drawing.Size(184, 22);
            this.renameToolStripMenuItem.Text = "Rename";
            this.renameToolStripMenuItem.Click += new System.EventHandler(this.renameToolStripMenuItem_Click);
            // 
            // abortToolStripMenuItem
            // 
            this.abortToolStripMenuItem.Name = "abortToolStripMenuItem";
            this.abortToolStripMenuItem.Size = new System.Drawing.Size(184, 22);
            this.abortToolStripMenuItem.Text = "Abort";
            this.abortToolStripMenuItem.Click += new System.EventHandler(this.abortToolStripMenuItem_Click);
            // 
            // stopToolStripMenuItem
            // 
            this.stopToolStripMenuItem.Name = "stopToolStripMenuItem";
            this.stopToolStripMenuItem.Size = new System.Drawing.Size(184, 22);
            this.stopToolStripMenuItem.Text = "Stop";
            this.stopToolStripMenuItem.Click += new System.EventHandler(this.stopToolStripMenuItem_Click);
            // 
            // shutDownToolStripMenuItem
            // 
            this.shutDownToolStripMenuItem.Name = "shutDownToolStripMenuItem";
            this.shutDownToolStripMenuItem.Size = new System.Drawing.Size(184, 22);
            this.shutDownToolStripMenuItem.Text = "Shut down";
            this.shutDownToolStripMenuItem.Click += new System.EventHandler(this.shutDownToolStripMenuItem_Click);
            // 
            // showProgressWindowToolStripMenuItem
            // 
            this.showProgressWindowToolStripMenuItem.Name = "showProgressWindowToolStripMenuItem";
            this.showProgressWindowToolStripMenuItem.Size = new System.Drawing.Size(184, 22);
            this.showProgressWindowToolStripMenuItem.Text = "Show progress window";
            this.showProgressWindowToolStripMenuItem.Click += new System.EventHandler(this.showProgressWindowToolStripMenuItem_Click);
            // 
            // showQueueToolStripMenuItem
            // 
            this.showQueueToolStripMenuItem.Name = "showQueueToolStripMenuItem";
            this.showQueueToolStripMenuItem.Size = new System.Drawing.Size(184, 22);
            this.showQueueToolStripMenuItem.Text = "Show queue";
            this.showQueueToolStripMenuItem.Click += new System.EventHandler(this.showQueueToolStripMenuItem_Click);
            // 
            // IndividualWorkerSummary
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.ContextMenuStrip = this.contextMenuStrip1;
            this.Controls.Add(this.workerNameAndJob);
            this.MaximumSize = new System.Drawing.Size(1000, 47);
            this.MinimumSize = new System.Drawing.Size(0, 47);
            this.Name = "IndividualWorkerSummary";
            this.Size = new System.Drawing.Size(292, 47);
            this.workerNameAndJob.ResumeLayout(false);
            this.contextMenuStrip1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        

        private System.Windows.Forms.GroupBox workerNameAndJob;
        private System.Windows.Forms.ProgressBar progressBar1;
        private System.Windows.Forms.ContextMenuStrip contextMenuStrip1;
        private System.Windows.Forms.ToolStripMenuItem startEncodingToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem abortToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem renameToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem stopToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem shutDownToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem showProgressWindowToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem showQueueToolStripMenuItem;
    }
}
