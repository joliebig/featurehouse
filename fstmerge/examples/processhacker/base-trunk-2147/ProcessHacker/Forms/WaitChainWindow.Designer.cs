namespace ProcessHacker
{
    partial class WaitChainWindow
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
            this.textDescription = new System.Windows.Forms.TextBox();
            this.labelIntro = new System.Windows.Forms.Label();
            this.buttonCancel = new System.Windows.Forms.Button();
            this.buttonEndThread = new System.Windows.Forms.Button();
            this.moreInfoLink = new System.Windows.Forms.LinkLabel();
            this.buttonProperties = new System.Windows.Forms.Button();
            this.threadTree = new VistaTreeView();
            this.SuspendLayout();



            this.textDescription.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textDescription.BackColor = System.Drawing.SystemColors.Control;
            this.textDescription.Location = new System.Drawing.Point(12, 12);
            this.textDescription.Name = "textDescription";
            this.textDescription.Size = new System.Drawing.Size(361, 20);
            this.textDescription.TabIndex = 1;



            this.labelIntro.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.labelIntro.Location = new System.Drawing.Point(12, 253);
            this.labelIntro.Name = "labelIntro";
            this.labelIntro.Size = new System.Drawing.Size(390, 41);
            this.labelIntro.TabIndex = 2;
            this.labelIntro.Text = "Analyzing the Wait Chain for a process helps diagnose application hangs and deadl" +
                "ocks caused by a process using or waiting to use a resource that is being used b" +
                "y another process.";
            this.labelIntro.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;



            this.buttonCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonCancel.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.buttonCancel.Location = new System.Drawing.Point(328, 294);
            this.buttonCancel.Name = "buttonCancel";
            this.buttonCancel.Size = new System.Drawing.Size(75, 23);
            this.buttonCancel.TabIndex = 3;
            this.buttonCancel.Text = "Cancel";
            this.buttonCancel.UseVisualStyleBackColor = true;
            this.buttonCancel.Click += new System.EventHandler(this.buttonCancel_Click);



            this.buttonEndThread.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonEndThread.Enabled = false;
            this.buttonEndThread.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.buttonEndThread.Location = new System.Drawing.Point(247, 294);
            this.buttonEndThread.Name = "buttonEndThread";
            this.buttonEndThread.Size = new System.Drawing.Size(75, 23);
            this.buttonEndThread.TabIndex = 4;
            this.buttonEndThread.Text = "End Thread";
            this.buttonEndThread.UseVisualStyleBackColor = true;



            this.moreInfoLink.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.moreInfoLink.AutoSize = true;
            this.moreInfoLink.Location = new System.Drawing.Point(9, 304);
            this.moreInfoLink.Name = "moreInfoLink";
            this.moreInfoLink.Size = new System.Drawing.Size(121, 13);
            this.moreInfoLink.TabIndex = 5;
            this.moreInfoLink.TabStop = true;
            this.moreInfoLink.Text = "More about Wait Chains";
            this.moreInfoLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.moreInfoLink_LinkClicked);



            this.buttonProperties.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonProperties.Image = global::ProcessHacker.Properties.Resources.application_form_magnify;
            this.buttonProperties.Location = new System.Drawing.Point(379, 9);
            this.buttonProperties.Name = "buttonProperties";
            this.buttonProperties.Size = new System.Drawing.Size(24, 24);
            this.buttonProperties.TabIndex = 7;
            this.buttonProperties.UseVisualStyleBackColor = true;
            this.buttonProperties.Click += new System.EventHandler(this.buttonProperties_Click);



            this.threadTree.CheckBoxes = true;
            this.threadTree.Location = new System.Drawing.Point(12, 38);
            this.threadTree.Name = "threadTree";
            this.threadTree.Size = new System.Drawing.Size(390, 212);
            this.threadTree.TabIndex = 9;



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(415, 329);
            this.Controls.Add(this.threadTree);
            this.Controls.Add(this.buttonProperties);
            this.Controls.Add(this.moreInfoLink);
            this.Controls.Add(this.buttonEndThread);
            this.Controls.Add(this.buttonCancel);
            this.Controls.Add(this.labelIntro);
            this.Controls.Add(this.textDescription);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "WaitChainWindow";
            this.ShowIcon = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
            this.Text = "WaitChainWindow";
            this.Load += new System.EventHandler(this.WaitChainWindow_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }



        private System.Windows.Forms.TextBox textDescription;
        private System.Windows.Forms.Label labelIntro;
        private System.Windows.Forms.Button buttonCancel;
        private System.Windows.Forms.Button buttonEndThread;
        private System.Windows.Forms.LinkLabel moreInfoLink;
        private System.Windows.Forms.Button buttonProperties;
        private VistaTreeView threadTree;
    }
}
