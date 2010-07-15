namespace ProcessHacker
{
    partial class TokenWindow
    {



        private System.ComponentModel.IContainer components = null;





        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }

            _tokenProps.Dispose();

            base.Dispose(disposing);
        }







        private void InitializeComponent()
        {
            this.buttonClose = new System.Windows.Forms.Button();
            this.panelToken = new System.Windows.Forms.Panel();
            this.SuspendLayout();



            this.buttonClose.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonClose.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.buttonClose.Location = new System.Drawing.Point(378, 397);
            this.buttonClose.Name = "buttonClose";
            this.buttonClose.Size = new System.Drawing.Size(75, 23);
            this.buttonClose.TabIndex = 1;
            this.buttonClose.Text = "&Close";
            this.buttonClose.UseVisualStyleBackColor = true;
            this.buttonClose.Click += new System.EventHandler(this.buttonClose_Click);



            this.panelToken.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panelToken.Location = new System.Drawing.Point(12, 12);
            this.panelToken.Name = "panelToken";
            this.panelToken.Size = new System.Drawing.Size(441, 379);
            this.panelToken.TabIndex = 2;



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(465, 432);
            this.Controls.Add(this.panelToken);
            this.Controls.Add(this.buttonClose);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "TokenWindow";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Token";
            this.Load += new System.EventHandler(this.TokenWindow_Load);
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.TokenWindow_FormClosing);
            this.ResumeLayout(false);

        }



        private System.Windows.Forms.Button buttonClose;
        private System.Windows.Forms.Panel panelToken;
    }
}
