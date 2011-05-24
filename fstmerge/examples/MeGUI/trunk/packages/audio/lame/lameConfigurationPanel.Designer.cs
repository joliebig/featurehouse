namespace MeGUI.packages.audio.lame
{
    partial class lameConfigurationPanel
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
            this.qualityLabel = new System.Windows.Forms.Label();
            this.quality = new System.Windows.Forms.NumericUpDown();
            this.bitrateLabel = new System.Windows.Forms.Label();
            this.encodingModeLabel = new System.Windows.Forms.Label();
            this.encodingMode = new System.Windows.Forms.ComboBox();
            this.bitrate = new System.Windows.Forms.NumericUpDown();
            this.encoderGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.quality)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.bitrate)).BeginInit();
            this.SuspendLayout();
            // 
            // encoderGroupBox
            // 
            this.encoderGroupBox.Controls.Add(this.qualityLabel);
            this.encoderGroupBox.Controls.Add(this.quality);
            this.encoderGroupBox.Controls.Add(this.bitrateLabel);
            this.encoderGroupBox.Controls.Add(this.encodingModeLabel);
            this.encoderGroupBox.Controls.Add(this.encodingMode);
            this.encoderGroupBox.Controls.Add(this.bitrate);
            this.encoderGroupBox.Location = new System.Drawing.Point(5, 158);
            this.encoderGroupBox.Size = new System.Drawing.Size(382, 92);
            // 
            // besweetOptionsGroupbox
            // 
            this.besweetOptionsGroupbox.Location = new System.Drawing.Point(5, 3);
            this.besweetOptionsGroupbox.Size = new System.Drawing.Size(382, 149);
            // 
            // qualityLabel
            // 
            this.qualityLabel.Location = new System.Drawing.Point(8, 64);
            this.qualityLabel.Name = "qualityLabel";
            this.qualityLabel.Size = new System.Drawing.Size(100, 18);
            this.qualityLabel.TabIndex = 11;
            this.qualityLabel.Text = "Quality";
            // 
            // quality
            // 
            this.quality.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.quality.Increment = new decimal(new int[] {
            10,
            0,
            0,
            0});
            this.quality.Location = new System.Drawing.Point(322, 61);
            this.quality.Minimum = new decimal(new int[] {
            10,
            0,
            0,
            0});
            this.quality.Name = "quality";
            this.quality.Size = new System.Drawing.Size(48, 20);
            this.quality.TabIndex = 10;
            this.quality.Value = new decimal(new int[] {
            50,
            0,
            0,
            0});
            this.quality.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.textField_KeyPress);
            // 
            // bitrateLabel
            // 
            this.bitrateLabel.Location = new System.Drawing.Point(8, 40);
            this.bitrateLabel.Name = "bitrateLabel";
            this.bitrateLabel.Size = new System.Drawing.Size(100, 23);
            this.bitrateLabel.TabIndex = 9;
            this.bitrateLabel.Text = "Bitrate";
            // 
            // encodingModeLabel
            // 
            this.encodingModeLabel.Location = new System.Drawing.Point(8, 16);
            this.encodingModeLabel.Name = "encodingModeLabel";
            this.encodingModeLabel.Size = new System.Drawing.Size(100, 23);
            this.encodingModeLabel.TabIndex = 8;
            this.encodingModeLabel.Text = "Encoding Mode";
            // 
            // encodingMode
            // 
            this.encodingMode.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.encodingMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.encodingMode.Location = new System.Drawing.Point(250, 13);
            this.encodingMode.Name = "encodingMode";
            this.encodingMode.Size = new System.Drawing.Size(121, 21);
            this.encodingMode.TabIndex = 7;
            this.encodingMode.SelectedIndexChanged += new System.EventHandler(this.encodingMode_SelectedIndexChanged);
            // 
            // bitrate
            // 
            this.bitrate.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.bitrate.Increment = new decimal(new int[] {
            8,
            0,
            0,
            0});
            this.bitrate.Location = new System.Drawing.Point(322, 37);
            this.bitrate.Maximum = new decimal(new int[] {
            320,
            0,
            0,
            0});
            this.bitrate.Minimum = new decimal(new int[] {
            8,
            0,
            0,
            0});
            this.bitrate.Name = "bitrate";
            this.bitrate.Size = new System.Drawing.Size(48, 20);
            this.bitrate.TabIndex = 6;
            this.bitrate.Value = new decimal(new int[] {
            128,
            0,
            0,
            0});
            this.bitrate.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.textField_KeyPress);
            // 
            // lameConfigurationPanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.Name = "lameConfigurationPanel";
            this.Size = new System.Drawing.Size(394, 253);
            this.encoderGroupBox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.quality)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.bitrate)).EndInit();
            this.ResumeLayout(false);

        }

        

        private System.Windows.Forms.Label qualityLabel;
        private System.Windows.Forms.NumericUpDown quality;
        private System.Windows.Forms.Label bitrateLabel;
        private System.Windows.Forms.Label encodingModeLabel;
        private System.Windows.Forms.ComboBox encodingMode;
        private System.Windows.Forms.NumericUpDown bitrate;

    }
}
