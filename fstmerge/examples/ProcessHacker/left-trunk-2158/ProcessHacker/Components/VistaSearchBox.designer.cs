namespace ProcessHacker
{
 partial class VistaSearchBox
 {




  protected override void Dispose(bool disposing)
  {
   if (disposing)
   {
    _inactiveFont.Dispose();
   }
   base.Dispose(disposing);
  }







  private void InitializeComponent()
  {
            this.searchOverlayLabel = new System.Windows.Forms.Label();
            this.searchText = new System.Windows.Forms.TextBox();
            this.searchImage = new System.Windows.Forms.PictureBox();
            ((System.ComponentModel.ISupportInitialize)(this.searchImage)).BeginInit();
            this.SuspendLayout();



            this.searchOverlayLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)));
            this.searchOverlayLabel.AutoSize = true;
            this.searchOverlayLabel.Location = new System.Drawing.Point(2, 3);
            this.searchOverlayLabel.Margin = new System.Windows.Forms.Padding(0);
            this.searchOverlayLabel.Name = "searchOverlayLabel";
            this.searchOverlayLabel.Size = new System.Drawing.Size(0, 13);
            this.searchOverlayLabel.TabIndex = 0;
            this.searchOverlayLabel.Click += new System.EventHandler(this.searchOverlayLabel_Click);



            this.searchText.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.searchText.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.searchText.Location = new System.Drawing.Point(2, 3);
            this.searchText.Margin = new System.Windows.Forms.Padding(0);
            this.searchText.Name = "searchText";
            this.searchText.Size = new System.Drawing.Size(125, 13);
            this.searchText.TabIndex = 0;
            this.searchText.TabStop = false;
            this.searchText.TextChanged += new System.EventHandler(this.searchText_TextChanged);
            this.searchText.GotFocus += new System.EventHandler(this.searchText_GotFocus);
            this.searchText.LostFocus += new System.EventHandler(this.searchText_LostFocus);



            this.searchImage.AccessibleRole = System.Windows.Forms.AccessibleRole.PushButton;
            this.searchImage.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.searchImage.Cursor = System.Windows.Forms.Cursors.Arrow;
            this.searchImage.Image = global::ProcessHacker.Properties.Resources.inactive_search;
            this.searchImage.Location = new System.Drawing.Point(127, 0);
            this.searchImage.Margin = new System.Windows.Forms.Padding(0);
            this.searchImage.Name = "searchImage";
            this.searchImage.Size = new System.Drawing.Size(23, 20);
            this.searchImage.TabIndex = 1;
            this.searchImage.TabStop = false;
            this.searchImage.MouseMove += new System.Windows.Forms.MouseEventHandler(this.searchImage_MouseMove);
            this.searchImage.Click += new System.EventHandler(this.searchImage_Click);



            this.BackColor = System.Drawing.SystemColors.Window;
            this.Controls.Add(this.searchOverlayLabel);
            this.Controls.Add(this.searchText);
            this.Controls.Add(this.searchImage);
            this.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.Size = new System.Drawing.Size(150, 20);
            ((System.ComponentModel.ISupportInitialize)(this.searchImage)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

  }



  private System.Windows.Forms.Label searchOverlayLabel;
  private System.Windows.Forms.TextBox searchText;
  private System.Windows.Forms.PictureBox searchImage;
 }
}
