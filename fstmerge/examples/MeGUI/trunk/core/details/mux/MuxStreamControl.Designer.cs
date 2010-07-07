namespace MeGUI.core.details.mux
{
    partial class MuxStreamControl
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
            this.subName = new System.Windows.Forms.TextBox();
            this.SubNamelabel = new System.Windows.Forms.Label();
            this.removeSubtitleTrack = new System.Windows.Forms.Button();
            this.subtitleLanguage = new System.Windows.Forms.ComboBox();
            this.subtitleLanguageLabel = new System.Windows.Forms.Label();
            this.subtitleInputLabel = new System.Windows.Forms.Label();
            this.delayLabel = new System.Windows.Forms.Label();
            this.audioDelay = new System.Windows.Forms.NumericUpDown();
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.input = new MeGUI.FileBar();
            ((System.ComponentModel.ISupportInitialize)(this.audioDelay)).BeginInit();
            this.tableLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // subName
            // 
            this.subName.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.subName.Location = new System.Drawing.Point(256, 33);
            this.subName.MaxLength = 100;
            this.subName.Name = "subName";
            this.subName.Size = new System.Drawing.Size(145, 20);
            this.subName.TabIndex = 40;
            // 
            // SubNamelabel
            // 
            this.SubNamelabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.SubNamelabel.AutoSize = true;
            this.SubNamelabel.Location = new System.Drawing.Point(215, 37);
            this.SubNamelabel.Name = "SubNamelabel";
            this.SubNamelabel.Size = new System.Drawing.Size(35, 13);
            this.SubNamelabel.TabIndex = 39;
            this.SubNamelabel.Text = "Name";
            // 
            // removeSubtitleTrack
            // 
            this.removeSubtitleTrack.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.removeSubtitleTrack.Location = new System.Drawing.Point(407, 32);
            this.removeSubtitleTrack.Name = "removeSubtitleTrack";
            this.removeSubtitleTrack.Size = new System.Drawing.Size(24, 23);
            this.removeSubtitleTrack.TabIndex = 38;
            this.removeSubtitleTrack.Text = "X";
            this.removeSubtitleTrack.Click += new System.EventHandler(this.removeSubtitleTrack_Click);
            // 
            // subtitleLanguage
            // 
            this.subtitleLanguage.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.subtitleLanguage.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.subtitleLanguage.Location = new System.Drawing.Point(64, 33);
            this.subtitleLanguage.Name = "subtitleLanguage";
            this.subtitleLanguage.Size = new System.Drawing.Size(145, 21);
            this.subtitleLanguage.Sorted = true;
            this.subtitleLanguage.TabIndex = 37;
            // 
            // subtitleLanguageLabel
            // 
            this.subtitleLanguageLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.subtitleLanguageLabel.AutoSize = true;
            this.subtitleLanguageLabel.Location = new System.Drawing.Point(3, 37);
            this.subtitleLanguageLabel.Name = "subtitleLanguageLabel";
            this.subtitleLanguageLabel.Size = new System.Drawing.Size(55, 13);
            this.subtitleLanguageLabel.TabIndex = 36;
            this.subtitleLanguageLabel.Text = "Language";
            // 
            // subtitleInputLabel
            // 
            this.subtitleInputLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.subtitleInputLabel.AutoSize = true;
            this.subtitleInputLabel.Location = new System.Drawing.Point(3, 8);
            this.subtitleInputLabel.Name = "subtitleInputLabel";
            this.subtitleInputLabel.Size = new System.Drawing.Size(55, 13);
            this.subtitleInputLabel.TabIndex = 33;
            this.subtitleInputLabel.Text = "Input";
            // 
            // delayLabel
            // 
            this.delayLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.delayLabel.AutoSize = true;
            this.delayLabel.Location = new System.Drawing.Point(3, 67);
            this.delayLabel.Name = "delayLabel";
            this.delayLabel.Size = new System.Drawing.Size(55, 13);
            this.delayLabel.TabIndex = 43;
            this.delayLabel.Text = "Delay";
            // 
            // audioDelay
            // 
            this.audioDelay.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.audioDelay.Location = new System.Drawing.Point(64, 64);
            this.audioDelay.Maximum = new decimal(new int[] {
            2147483647,
            0,
            0,
            0});
            this.audioDelay.Minimum = new decimal(new int[] {
            -2147483648,
            0,
            0,
            -2147483648});
            this.audioDelay.Name = "audioDelay";
            this.audioDelay.Size = new System.Drawing.Size(145, 20);
            this.audioDelay.TabIndex = 42;
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.ColumnCount = 5;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.Controls.Add(this.removeSubtitleTrack, 4, 1);
            this.tableLayoutPanel1.Controls.Add(this.input, 1, 0);
            this.tableLayoutPanel1.Controls.Add(this.delayLabel, 0, 2);
            this.tableLayoutPanel1.Controls.Add(this.subtitleInputLabel, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.subName, 3, 1);
            this.tableLayoutPanel1.Controls.Add(this.audioDelay, 1, 2);
            this.tableLayoutPanel1.Controls.Add(this.SubNamelabel, 2, 1);
            this.tableLayoutPanel1.Controls.Add(this.subtitleLanguage, 1, 1);
            this.tableLayoutPanel1.Controls.Add(this.subtitleLanguageLabel, 0, 1);
            this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 3;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.Size = new System.Drawing.Size(434, 90);
            this.tableLayoutPanel1.TabIndex = 44;
            // 
            // input
            // 
            this.input.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.tableLayoutPanel1.SetColumnSpan(this.input, 4);
            this.input.Filename = "";
            this.input.Filter = null;
            this.input.FolderMode = false;
            this.input.Location = new System.Drawing.Point(64, 3);
            this.input.Name = "input";
            this.input.ReadOnly = true;
            this.input.SaveMode = false;
            this.input.Size = new System.Drawing.Size(367, 23);
            this.input.TabIndex = 41;
            this.input.Title = null;
            this.input.FileSelected += new MeGUI.FileBarEventHandler(this.input_FileSelected);
            // 
            // MuxStreamControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tableLayoutPanel1);
            this.Name = "MuxStreamControl";
            this.Size = new System.Drawing.Size(434, 90);
            ((System.ComponentModel.ISupportInitialize)(this.audioDelay)).EndInit();
            this.tableLayoutPanel1.ResumeLayout(false);
            this.tableLayoutPanel1.PerformLayout();
            this.ResumeLayout(false);

        }

        

        protected System.Windows.Forms.TextBox subName;
        protected System.Windows.Forms.Label SubNamelabel;
        protected System.Windows.Forms.Button removeSubtitleTrack;
        protected System.Windows.Forms.ComboBox subtitleLanguage;
        protected System.Windows.Forms.Label subtitleLanguageLabel;
        protected System.Windows.Forms.Label subtitleInputLabel;
        private FileBar input;
        protected System.Windows.Forms.Label delayLabel;
        protected System.Windows.Forms.NumericUpDown audioDelay;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
    }
}
