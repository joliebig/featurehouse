namespace ProcessHacker
{
    partial class TerminatorWindow
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(TerminatorWindow));
            this.labelProgress = new System.Windows.Forms.Label();
            this.listTests = new System.Windows.Forms.ListView();
            this.columnID = new System.Windows.Forms.ColumnHeader();
            this.columnDescription = new System.Windows.Forms.ColumnHeader();
            this.imageList = new System.Windows.Forms.ImageList(this.components);
            this.buttonRun = new System.Windows.Forms.Button();
            this.SuspendLayout();



            this.labelProgress.AutoSize = true;
            this.labelProgress.Location = new System.Drawing.Point(12, 9);
            this.labelProgress.Name = "labelProgress";
            this.labelProgress.Size = new System.Drawing.Size(53, 13);
            this.labelProgress.TabIndex = 0;
            this.labelProgress.Text = "Message.";



            this.listTests.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnID,
            this.columnDescription});
            this.listTests.FullRowSelect = true;
            this.listTests.Location = new System.Drawing.Point(12, 38);
            this.listTests.Margin = new System.Windows.Forms.Padding(3, 4, 3, 4);
            this.listTests.Name = "listTests";
            this.listTests.ShowItemToolTips = true;
            this.listTests.Size = new System.Drawing.Size(442, 356);
            this.listTests.SmallImageList = this.imageList;
            this.listTests.TabIndex = 1;
            this.listTests.UseCompatibleStateImageBehavior = false;
            this.listTests.View = System.Windows.Forms.View.Details;
            this.listTests.DoubleClick += new System.EventHandler(this.listTests_DoubleClick);



            this.columnID.Text = "ID";



            this.columnDescription.Text = "Description";
            this.columnDescription.Width = 350;



            this.imageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList.ImageStream")));
            this.imageList.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList.Images.SetKeyName(0, "tick");
            this.imageList.Images.SetKeyName(1, "cross");



            this.buttonRun.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonRun.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.buttonRun.Location = new System.Drawing.Point(379, 402);
            this.buttonRun.Margin = new System.Windows.Forms.Padding(3, 4, 3, 4);
            this.buttonRun.Name = "buttonRun";
            this.buttonRun.Size = new System.Drawing.Size(75, 22);
            this.buttonRun.TabIndex = 2;
            this.buttonRun.Text = "&Run";
            this.buttonRun.UseVisualStyleBackColor = true;
            this.buttonRun.Click += new System.EventHandler(this.buttonRun_Click);



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(466, 437);
            this.Controls.Add(this.buttonRun);
            this.Controls.Add(this.listTests);
            this.Controls.Add(this.labelProgress);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(2, 4, 2, 4);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "TerminatorWindow";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Process Terminator";
            this.ResumeLayout(false);
            this.PerformLayout();

        }



        private System.Windows.Forms.Label labelProgress;
        private System.Windows.Forms.ListView listTests;
        private System.Windows.Forms.Button buttonRun;
        private System.Windows.Forms.ColumnHeader columnDescription;
        private System.Windows.Forms.ImageList imageList;
        private System.Windows.Forms.ColumnHeader columnID;

    }
}
