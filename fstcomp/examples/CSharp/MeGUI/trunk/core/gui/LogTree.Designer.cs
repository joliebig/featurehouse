namespace MeGUI.core.gui
{
    partial class LogTree
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
            this.treeView = new System.Windows.Forms.TreeView();
            this.contextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.editTextToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.editIndividualNode = new System.Windows.Forms.ToolStripMenuItem();
            this.editBranch = new System.Windows.Forms.ToolStripMenuItem();
            this.editLog = new System.Windows.Forms.ToolStripMenuItem();
            this.saveToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.saveBranch = new System.Windows.Forms.ToolStripMenuItem();
            this.saveLog = new System.Windows.Forms.ToolStripMenuItem();
            this.expandAllSubitemsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.expandLog = new System.Windows.Forms.ToolStripMenuItem();
            this.expandBranch = new System.Windows.Forms.ToolStripMenuItem();
            this.collapseAllSubitemsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.collapseLog = new System.Windows.Forms.ToolStripMenuItem();
            this.collapseBranch = new System.Windows.Forms.ToolStripMenuItem();
            this.saveDialog = new System.Windows.Forms.SaveFileDialog();
            this.contextMenu.SuspendLayout();
            this.SuspendLayout();
            // 
            // treeView
            // 
            this.treeView.ContextMenuStrip = this.contextMenu;
            this.treeView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.treeView.Location = new System.Drawing.Point(0, 0);
            this.treeView.Name = "treeView";
            this.treeView.Size = new System.Drawing.Size(596, 478);
            this.treeView.TabIndex = 0;
            // 
            // contextMenu
            // 
            this.contextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.editTextToolStripMenuItem,
            this.saveToolStripMenuItem,
            this.expandAllSubitemsToolStripMenuItem,
            this.collapseAllSubitemsToolStripMenuItem});
            this.contextMenu.Name = "contextMenuStrip1";
            this.contextMenu.Size = new System.Drawing.Size(186, 92);
            // 
            // editTextToolStripMenuItem
            // 
            this.editTextToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.editIndividualNode,
            this.editBranch,
            this.editLog});
            this.editTextToolStripMenuItem.Name = "editTextToolStripMenuItem";
            this.editTextToolStripMenuItem.Size = new System.Drawing.Size(185, 22);
            this.editTextToolStripMenuItem.Text = "Edit text";
            // 
            // editIndividualNode
            // 
            this.editIndividualNode.Name = "editIndividualNode";
            this.editIndividualNode.Size = new System.Drawing.Size(111, 22);
            this.editIndividualNode.Text = "node";
            this.editIndividualNode.Click += new System.EventHandler(this.ofIndividualNodeToolStripMenuItem_Click);
            // 
            // editBranch
            // 
            this.editBranch.Name = "editBranch";
            this.editBranch.Size = new System.Drawing.Size(111, 22);
            this.editBranch.Text = "branch";
            this.editBranch.Click += new System.EventHandler(this.ofBranchToolStripMenuItem_Click);
            // 
            // editLog
            // 
            this.editLog.Name = "editLog";
            this.editLog.Size = new System.Drawing.Size(111, 22);
            this.editLog.Text = "log";
            this.editLog.Click += new System.EventHandler(this.editLog_Click);
            // 
            // saveToolStripMenuItem
            // 
            this.saveToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.saveBranch,
            this.saveLog});
            this.saveToolStripMenuItem.Name = "saveToolStripMenuItem";
            this.saveToolStripMenuItem.Size = new System.Drawing.Size(185, 22);
            this.saveToolStripMenuItem.Text = "Save";
            // 
            // saveBranch
            // 
            this.saveBranch.Name = "saveBranch";
            this.saveBranch.Size = new System.Drawing.Size(111, 22);
            this.saveBranch.Text = "branch";
            this.saveBranch.Click += new System.EventHandler(this.saveBranch_Click);
            // 
            // saveLog
            // 
            this.saveLog.Name = "saveLog";
            this.saveLog.Size = new System.Drawing.Size(111, 22);
            this.saveLog.Text = "log";
            this.saveLog.Click += new System.EventHandler(this.saveLog_Click);
            // 
            // expandAllSubitemsToolStripMenuItem
            // 
            this.expandAllSubitemsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.expandLog,
            this.expandBranch});
            this.expandAllSubitemsToolStripMenuItem.Name = "expandAllSubitemsToolStripMenuItem";
            this.expandAllSubitemsToolStripMenuItem.Size = new System.Drawing.Size(185, 22);
            this.expandAllSubitemsToolStripMenuItem.Text = "Expand all subitems";
            // 
            // expandLog
            // 
            this.expandLog.Name = "expandLog";
            this.expandLog.Size = new System.Drawing.Size(125, 22);
            this.expandLog.Text = "of log";
            this.expandLog.Click += new System.EventHandler(this.expandLog_Click);
            // 
            // expandBranch
            // 
            this.expandBranch.Name = "expandBranch";
            this.expandBranch.Size = new System.Drawing.Size(125, 22);
            this.expandBranch.Text = "of branch";
            this.expandBranch.Click += new System.EventHandler(this.expandBranch_Click);
            // 
            // collapseAllSubitemsToolStripMenuItem
            // 
            this.collapseAllSubitemsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.collapseLog,
            this.collapseBranch});
            this.collapseAllSubitemsToolStripMenuItem.Name = "collapseAllSubitemsToolStripMenuItem";
            this.collapseAllSubitemsToolStripMenuItem.Size = new System.Drawing.Size(185, 22);
            this.collapseAllSubitemsToolStripMenuItem.Text = "Collapse all subitems";
            // 
            // collapseLog
            // 
            this.collapseLog.Name = "collapseLog";
            this.collapseLog.Size = new System.Drawing.Size(125, 22);
            this.collapseLog.Text = "of log";
            this.collapseLog.Click += new System.EventHandler(this.collapseLog_Click);
            // 
            // collapseBranch
            // 
            this.collapseBranch.Name = "collapseBranch";
            this.collapseBranch.Size = new System.Drawing.Size(125, 22);
            this.collapseBranch.Text = "of branch";
            this.collapseBranch.Click += new System.EventHandler(this.collapseBranch_Click);
            // 
            // saveDialog
            // 
            this.saveDialog.Filter = "Log files (*.log)|*.log|All files (*.*)|*.*";
            this.saveDialog.FilterIndex = 0;
            this.saveDialog.Title = "Select output file";
            // 
            // LogTree
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.treeView);
            this.Name = "LogTree";
            this.Size = new System.Drawing.Size(596, 478);
            this.Load += new System.EventHandler(this.LogTree_Load);
            this.contextMenu.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        

        private System.Windows.Forms.TreeView treeView;
        private System.Windows.Forms.ContextMenuStrip contextMenu;
        private System.Windows.Forms.ToolStripMenuItem editTextToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem editIndividualNode;
        private System.Windows.Forms.ToolStripMenuItem editBranch;
        private System.Windows.Forms.SaveFileDialog saveDialog;
        private System.Windows.Forms.ToolStripMenuItem saveToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem saveBranch;
        private System.Windows.Forms.ToolStripMenuItem saveLog;
        private System.Windows.Forms.ToolStripMenuItem editLog;
        private System.Windows.Forms.ToolStripMenuItem expandAllSubitemsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem expandLog;
        private System.Windows.Forms.ToolStripMenuItem expandBranch;
        private System.Windows.Forms.ToolStripMenuItem collapseAllSubitemsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem collapseLog;
        private System.Windows.Forms.ToolStripMenuItem collapseBranch;
    }
}
