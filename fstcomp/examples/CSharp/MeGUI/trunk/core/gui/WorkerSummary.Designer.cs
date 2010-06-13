namespace MeGUI.core.gui
{
    partial class WorkerSummary
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(WorkerSummary));
            this.panel1 = new System.Windows.Forms.Panel();
            this.individualWorkerSummary3 = new MeGUI.core.gui.IndividualWorkerSummary();
            this.individualWorkerSummary2 = new MeGUI.core.gui.IndividualWorkerSummary();
            this.individualWorkerSummary1 = new MeGUI.core.gui.IndividualWorkerSummary();
            this.contextMenuStrip1 = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.newWorkerToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.panel1.SuspendLayout();
            this.contextMenuStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // panel1
            // 
            this.panel1.AutoSize = true;
            this.panel1.Controls.Add(this.individualWorkerSummary3);
            this.panel1.Controls.Add(this.individualWorkerSummary2);
            this.panel1.Controls.Add(this.individualWorkerSummary1);
            this.panel1.Location = new System.Drawing.Point(0, 4);
            this.panel1.Name = "panel1";
            this.panel1.Padding = new System.Windows.Forms.Padding(3);
            this.panel1.Size = new System.Drawing.Size(348, 147);
            this.panel1.TabIndex = 0;
            // 
            // individualWorkerSummary3
            // 
            this.individualWorkerSummary3.AutoSize = true;
            this.individualWorkerSummary3.Dock = System.Windows.Forms.DockStyle.Top;
            this.individualWorkerSummary3.Location = new System.Drawing.Point(3, 97);
            this.individualWorkerSummary3.MaximumSize = new System.Drawing.Size(1000, 47);
            this.individualWorkerSummary3.MinimumSize = new System.Drawing.Size(200, 47);
            this.individualWorkerSummary3.Name = "individualWorkerSummary3";
            this.individualWorkerSummary3.Size = new System.Drawing.Size(342, 47);
            this.individualWorkerSummary3.TabIndex = 2;
            // 
            // individualWorkerSummary2
            // 
            this.individualWorkerSummary2.AutoSize = true;
            this.individualWorkerSummary2.Dock = System.Windows.Forms.DockStyle.Top;
            this.individualWorkerSummary2.Location = new System.Drawing.Point(3, 50);
            this.individualWorkerSummary2.MaximumSize = new System.Drawing.Size(1000, 47);
            this.individualWorkerSummary2.MinimumSize = new System.Drawing.Size(200, 47);
            this.individualWorkerSummary2.Name = "individualWorkerSummary2";
            this.individualWorkerSummary2.Size = new System.Drawing.Size(342, 47);
            this.individualWorkerSummary2.TabIndex = 1;
            // 
            // individualWorkerSummary1
            // 
            this.individualWorkerSummary1.AutoSize = true;
            this.individualWorkerSummary1.Dock = System.Windows.Forms.DockStyle.Top;
            this.individualWorkerSummary1.Location = new System.Drawing.Point(3, 3);
            this.individualWorkerSummary1.MaximumSize = new System.Drawing.Size(1000, 47);
            this.individualWorkerSummary1.MinimumSize = new System.Drawing.Size(200, 47);
            this.individualWorkerSummary1.Name = "individualWorkerSummary1";
            this.individualWorkerSummary1.Size = new System.Drawing.Size(342, 47);
            this.individualWorkerSummary1.TabIndex = 0;
            // 
            // contextMenuStrip1
            // 
            this.contextMenuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.newWorkerToolStripMenuItem});
            this.contextMenuStrip1.Name = "contextMenuStrip1";
            this.contextMenuStrip1.Size = new System.Drawing.Size(138, 26);
            // 
            // newWorkerToolStripMenuItem
            // 
            this.newWorkerToolStripMenuItem.Name = "newWorkerToolStripMenuItem";
            this.newWorkerToolStripMenuItem.Size = new System.Drawing.Size(137, 22);
            this.newWorkerToolStripMenuItem.Text = "New worker";
            this.newWorkerToolStripMenuItem.Click += new System.EventHandler(this.newWorkerToolStripMenuItem_Click);
            // 
            // WorkerSummary
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.ClientSize = global::MeGUI.Properties.Settings.Default.WorkerSummarySize;
            this.WindowState = global::MeGUI.Properties.Settings.Default.WorkerSummaryWindowState;
            this.ContextMenuStrip = this.contextMenuStrip1;
            this.Controls.Add(this.panel1);
            this.DataBindings.Add(new System.Windows.Forms.Binding("Location", global::MeGUI.Properties.Settings.Default, "WorkerSummaryLocation", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.DataBindings.Add(new System.Windows.Forms.Binding("ClientSize", global::MeGUI.Properties.Settings.Default, "WorkerSummarySize", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.DataBindings.Add(new System.Windows.Forms.Binding("WindowState", global::MeGUI.Properties.Settings.Default, "WorkerSummaryWindowState", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Location = global::MeGUI.Properties.Settings.Default.WorkerSummaryLocation;
            this.Name = "WorkerSummary";
            this.Text = "WorkerSummary";
            this.VisibleChanged += new System.EventHandler(this.WorkerSummary_VisibleChanged);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.contextMenuStrip1.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        

        private System.Windows.Forms.Panel panel1;
        private IndividualWorkerSummary individualWorkerSummary3;
        private IndividualWorkerSummary individualWorkerSummary2;
        private IndividualWorkerSummary individualWorkerSummary1;
        private System.Windows.Forms.ContextMenuStrip contextMenuStrip1;
        private System.Windows.Forms.ToolStripMenuItem newWorkerToolStripMenuItem;


    }
}