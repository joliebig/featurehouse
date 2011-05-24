namespace ProcessHacker
{
    partial class HelpWindow
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(HelpWindow));
            this.webBrowser = new System.Windows.Forms.WebBrowser();
            this.listBoxContents = new System.Windows.Forms.ListBox();
            this.SuspendLayout();



            this.webBrowser.AllowNavigation = false;
            this.webBrowser.AllowWebBrowserDrop = false;
            this.webBrowser.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.webBrowser.IsWebBrowserContextMenuEnabled = false;
            this.webBrowser.Location = new System.Drawing.Point(201, 12);
            this.webBrowser.MinimumSize = new System.Drawing.Size(20, 20);
            this.webBrowser.Name = "webBrowser";
            this.webBrowser.Size = new System.Drawing.Size(535, 490);
            this.webBrowser.TabIndex = 0;
            this.webBrowser.PreviewKeyDown += new System.Windows.Forms.PreviewKeyDownEventHandler(this.webBrowser_PreviewKeyDown);



            this.listBoxContents.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)));
            this.listBoxContents.FormattingEnabled = true;
            this.listBoxContents.IntegralHeight = false;
            this.listBoxContents.Location = new System.Drawing.Point(12, 12);
            this.listBoxContents.Name = "listBoxContents";
            this.listBoxContents.Size = new System.Drawing.Size(183, 490);
            this.listBoxContents.TabIndex = 1;
            this.listBoxContents.SelectedIndexChanged += new System.EventHandler(this.listBoxContents_SelectedIndexChanged);



            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(748, 514);
            this.Controls.Add(this.listBoxContents);
            this.Controls.Add(this.webBrowser);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "HelpWindow";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Help";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.HelpWindow_FormClosing);
            this.ResumeLayout(false);

        }



        private System.Windows.Forms.WebBrowser webBrowser;
        private System.Windows.Forms.ListBox listBoxContents;
    }
}
