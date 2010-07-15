namespace ProcessHacker
{
    partial class JobWindow
    {



        private System.ComponentModel.IContainer components = null;





        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }

            _jobProps.Dispose();

            base.Dispose(disposing);
        }







        private void InitializeComponent()
        {
            this.panelJob = new System.Windows.Forms.Panel();
            this.buttonClose = new System.Windows.Forms.Button();
            this.SuspendLayout();



            this.panelJob.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panelJob.Location = new System.Drawing.Point(12, 12);
            this.panelJob.Name = "panelJob";
            this.panelJob.Size = new System.Drawing.Size(444, 372);
            this.panelJob.TabIndex = 0;



            this.buttonClose.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonClose.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.buttonClose.Location = new System.Drawing.Point(381, 390);
            this.buttonClose.Name = "buttonClose";
            this.buttonClose.Size = new System.Drawing.Size(75, 23);
            this.buttonClose.TabIndex = 1;
            this.buttonClose.Text = "Close";
            this.buttonClose.UseVisualStyleBackColor = true;
            this.buttonClose.Click += new System.EventHandler(this.buttonClose_Click);



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(468, 425);
            this.Controls.Add(this.buttonClose);
            this.Controls.Add(this.panelJob);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "JobWindow";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Job";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.JobWindow_FormClosing);
            this.ResumeLayout(false);

        }



        private System.Windows.Forms.Panel panelJob;
        private System.Windows.Forms.Button buttonClose;
    }
}
