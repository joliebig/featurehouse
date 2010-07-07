namespace MeGUI
{
    partial class FileBar
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
            this.filename = new System.Windows.Forms.TextBox();
            this.openButton = new System.Windows.Forms.Button();
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.tableLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // filename
            // 
            this.filename.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.filename.Location = new System.Drawing.Point(0, 3);
            this.filename.Margin = new System.Windows.Forms.Padding(0);
            this.filename.Name = "filename";
            this.filename.ReadOnly = true;
            this.filename.Size = new System.Drawing.Size(239, 20);
            this.filename.TabIndex = 5;
            this.filename.TextChanged += new System.EventHandler(this.filename_TextChanged);
            // 
            // openButton
            // 
            this.openButton.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.openButton.AutoSize = true;
            this.openButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.openButton.Location = new System.Drawing.Point(242, 1);
            this.openButton.Margin = new System.Windows.Forms.Padding(3, 1, 1, 1);
            this.openButton.Name = "openButton";
            this.openButton.Size = new System.Drawing.Size(26, 23);
            this.openButton.TabIndex = 1;
            this.openButton.Text = "...";
            this.openButton.Click += new System.EventHandler(this.openButton_Click);
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.ColumnCount = 2;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.Controls.Add(this.filename, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.openButton, 1, 0);
            this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Margin = new System.Windows.Forms.Padding(0);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 1;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tableLayoutPanel1.Size = new System.Drawing.Size(269, 26);
            this.tableLayoutPanel1.TabIndex = 7;
            // 
            // FileBar
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tableLayoutPanel1);
            this.Name = "FileBar";
            this.Size = new System.Drawing.Size(269, 26);
            this.tableLayoutPanel1.ResumeLayout(false);
            this.tableLayoutPanel1.PerformLayout();
            this.ResumeLayout(false);

        }

        

        private System.Windows.Forms.TextBox filename;
        private System.Windows.Forms.Button openButton;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
    }
}
