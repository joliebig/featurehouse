namespace MeGUI.packages.audio.faac
{
    partial class faacConfigurationPanel
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
            this.vbrQuality = new System.Windows.Forms.NumericUpDown();
            this.qualityModeRadioButton = new System.Windows.Forms.RadioButton();
            this.cbrBitrate = new System.Windows.Forms.ComboBox();
            this.cbrBitrateRadioButton = new System.Windows.Forms.RadioButton();
            this.encoderGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.vbrQuality)).BeginInit();
            this.SuspendLayout();
            // 
            // encoderGroupBox
            // 
            this.encoderGroupBox.Controls.Add(this.vbrQuality);
            this.encoderGroupBox.Controls.Add(this.qualityModeRadioButton);
            this.encoderGroupBox.Controls.Add(this.cbrBitrate);
            this.encoderGroupBox.Controls.Add(this.cbrBitrateRadioButton);
            this.encoderGroupBox.Location = new System.Drawing.Point(5, 158);
            this.encoderGroupBox.Size = new System.Drawing.Size(380, 85);
            this.encoderGroupBox.Text = "AAC Options";
            // 
            // besweetOptionsGroupbox
            // 
            this.besweetOptionsGroupbox.Location = new System.Drawing.Point(5, 3);
            this.besweetOptionsGroupbox.Size = new System.Drawing.Size(380, 149);
            // 
            // vbrQuality
            // 
            this.vbrQuality.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.vbrQuality.Increment = new decimal(new int[] {
            10,
            0,
            0,
            0});
            this.vbrQuality.Location = new System.Drawing.Point(107, 51);
            this.vbrQuality.Maximum = new decimal(new int[] {
            500,
            0,
            0,
            0});
            this.vbrQuality.Minimum = new decimal(new int[] {
            10,
            0,
            0,
            0});
            this.vbrQuality.Name = "vbrQuality";
            this.vbrQuality.Size = new System.Drawing.Size(264, 20);
            this.vbrQuality.TabIndex = 7;
            this.vbrQuality.Value = new decimal(new int[] {
            100,
            0,
            0,
            0});
            // 
            // qualityModeRadioButton
            // 
            this.qualityModeRadioButton.Location = new System.Drawing.Point(16, 51);
            this.qualityModeRadioButton.Name = "qualityModeRadioButton";
            this.qualityModeRadioButton.Size = new System.Drawing.Size(68, 24);
            this.qualityModeRadioButton.TabIndex = 6;
            this.qualityModeRadioButton.Text = "VBR :";
            // 
            // cbrBitrate
            // 
            this.cbrBitrate.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.cbrBitrate.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbrBitrate.Location = new System.Drawing.Point(107, 19);
            this.cbrBitrate.Name = "cbrBitrate";
            this.cbrBitrate.Size = new System.Drawing.Size(264, 21);
            this.cbrBitrate.TabIndex = 5;
            // 
            // cbrBitrateRadioButton
            // 
            this.cbrBitrateRadioButton.Checked = true;
            this.cbrBitrateRadioButton.Location = new System.Drawing.Point(16, 19);
            this.cbrBitrateRadioButton.Name = "cbrBitrateRadioButton";
            this.cbrBitrateRadioButton.Size = new System.Drawing.Size(56, 24);
            this.cbrBitrateRadioButton.TabIndex = 4;
            this.cbrBitrateRadioButton.TabStop = true;
            this.cbrBitrateRadioButton.Text = "ABR :";
            this.cbrBitrateRadioButton.CheckedChanged += new System.EventHandler(this.bitrateModeChanged);
            // 
            // faacConfigurationPanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.Name = "faacConfigurationPanel";
            this.Size = new System.Drawing.Size(394, 246);
            this.encoderGroupBox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.vbrQuality)).EndInit();
            this.ResumeLayout(false);

        }

        

        private System.Windows.Forms.NumericUpDown vbrQuality;
        private System.Windows.Forms.RadioButton qualityModeRadioButton;
        private System.Windows.Forms.ComboBox cbrBitrate;
        private System.Windows.Forms.RadioButton cbrBitrateRadioButton;

    }
}
