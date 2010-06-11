namespace MeGUI.packages.video.snow
{
    partial class snowConfigurationPanel
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.logfile = new System.Windows.Forms.TextBox();
            this.logfileOpenButton = new System.Windows.Forms.Button();
            this.logfileLabel = new System.Windows.Forms.Label();
            this.snowMotionEstimationGroupbox = new System.Windows.Forms.GroupBox();
            this.snowMbCompare = new System.Windows.Forms.ComboBox();
            this.snowMeCmpHpel = new System.Windows.Forms.ComboBox();
            this.snowMeCmpFullpel = new System.Windows.Forms.ComboBox();
            this.snowMbCompareLabel = new System.Windows.Forms.Label();
            this.snowMeCmpHpelLabel = new System.Windows.Forms.Label();
            this.snowMeCmpFullpelLabel = new System.Windows.Forms.Label();
            this.snowToolsGroupbox = new System.Windows.Forms.GroupBox();
            this.snowV4mv = new System.Windows.Forms.CheckBox();
            this.snowNBPredictors = new System.Windows.Forms.NumericUpDown();
            this.snowNBPredictorsLabel = new System.Windows.Forms.Label();
            this.snowV4mvLabel = new System.Windows.Forms.Label();
            this.snowQpel = new System.Windows.Forms.CheckBox();
            this.snowQpelLabel = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.snowCreditsQuantizer = new System.Windows.Forms.NumericUpDown();
            this.snowLosslessModeLabel = new System.Windows.Forms.Label();
            this.snowLosslessMode = new System.Windows.Forms.CheckBox();
            this.snowCodecGeneralGroupbox = new System.Windows.Forms.GroupBox();
            this.snowPredictionMode = new System.Windows.Forms.ComboBox();
            this.snowPredictionModeLabel = new System.Windows.Forms.Label();
            this.snowQuantizer = new System.Windows.Forms.NumericUpDown();
            this.snowEncodingMode = new System.Windows.Forms.ComboBox();
            this.snowEncodingModeLabel = new System.Windows.Forms.Label();
            this.snowQuantizerLabel = new System.Windows.Forms.Label();
            this.snowBitrate = new System.Windows.Forms.TextBox();
            this.snowBitrateLabel = new System.Windows.Forms.Label();
            this.saveFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.tabControl1.SuspendLayout();
            this.mainTabPage.SuspendLayout();
            this.snowMotionEstimationGroupbox.SuspendLayout();
            this.snowToolsGroupbox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.snowNBPredictors)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.snowCreditsQuantizer)).BeginInit();
            this.snowCodecGeneralGroupbox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.snowQuantizer)).BeginInit();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Size = new System.Drawing.Size(446, 337);
            // 
            // commandline
            // 
            this.commandline.Location = new System.Drawing.Point(3, 339);
            this.commandline.Size = new System.Drawing.Size(438, 82);
            this.commandline.Text = " ";
            // 
            // mainTabPage
            // 
            this.mainTabPage.Controls.Add(this.logfile);
            this.mainTabPage.Controls.Add(this.logfileOpenButton);
            this.mainTabPage.Controls.Add(this.logfileLabel);
            this.mainTabPage.Controls.Add(this.snowMotionEstimationGroupbox);
            this.mainTabPage.Controls.Add(this.snowToolsGroupbox);
            this.mainTabPage.Controls.Add(this.snowCodecGeneralGroupbox);
            this.mainTabPage.Size = new System.Drawing.Size(438, 311);
            // 
            // logfile
            // 
            this.logfile.Location = new System.Drawing.Point(83, 281);
            this.logfile.Name = "logfile";
            this.logfile.ReadOnly = true;
            this.logfile.Size = new System.Drawing.Size(168, 20);
            this.logfile.TabIndex = 56;
            this.logfile.Text = "2pass.stats";
            // 
            // logfileOpenButton
            // 
            this.logfileOpenButton.Location = new System.Drawing.Point(259, 281);
            this.logfileOpenButton.Name = "logfileOpenButton";
            this.logfileOpenButton.Size = new System.Drawing.Size(24, 23);
            this.logfileOpenButton.TabIndex = 57;
            this.logfileOpenButton.Text = "...";
            this.logfileOpenButton.Click += new System.EventHandler(this.logfileOpenButton_Click);
            // 
            // logfileLabel
            // 
            this.logfileLabel.Location = new System.Drawing.Point(19, 284);
            this.logfileLabel.Name = "logfileLabel";
            this.logfileLabel.Size = new System.Drawing.Size(56, 14);
            this.logfileLabel.TabIndex = 55;
            this.logfileLabel.Text = "Logfile";
            // 
            // snowMotionEstimationGroupbox
            // 
            this.snowMotionEstimationGroupbox.Controls.Add(this.snowMbCompare);
            this.snowMotionEstimationGroupbox.Controls.Add(this.snowMeCmpHpel);
            this.snowMotionEstimationGroupbox.Controls.Add(this.snowMeCmpFullpel);
            this.snowMotionEstimationGroupbox.Controls.Add(this.snowMbCompareLabel);
            this.snowMotionEstimationGroupbox.Controls.Add(this.snowMeCmpHpelLabel);
            this.snowMotionEstimationGroupbox.Controls.Add(this.snowMeCmpFullpelLabel);
            this.snowMotionEstimationGroupbox.Location = new System.Drawing.Point(0, 171);
            this.snowMotionEstimationGroupbox.Name = "snowMotionEstimationGroupbox";
            this.snowMotionEstimationGroupbox.Size = new System.Drawing.Size(437, 104);
            this.snowMotionEstimationGroupbox.TabIndex = 54;
            this.snowMotionEstimationGroupbox.TabStop = false;
            this.snowMotionEstimationGroupbox.Text = "Motion Estimation";
            // 
            // snowMbCompare
            // 
            this.snowMbCompare.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.snowMbCompare.Items.AddRange(new object[] {
            "SAD",
            "SSD",
            "5/3 Wavelet",
            "9/7 Wavelet"});
            this.snowMbCompare.Location = new System.Drawing.Point(298, 69);
            this.snowMbCompare.Name = "snowMbCompare";
            this.snowMbCompare.Size = new System.Drawing.Size(121, 21);
            this.snowMbCompare.TabIndex = 17;
            this.snowMbCompare.SelectedIndexChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowMeCmpHpel
            // 
            this.snowMeCmpHpel.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.snowMeCmpHpel.Items.AddRange(new object[] {
            "SAD",
            "SSD",
            "5/3 Wavelet",
            "9/7 Wavelet"});
            this.snowMeCmpHpel.Location = new System.Drawing.Point(298, 45);
            this.snowMeCmpHpel.Name = "snowMeCmpHpel";
            this.snowMeCmpHpel.Size = new System.Drawing.Size(121, 21);
            this.snowMeCmpHpel.TabIndex = 16;
            this.snowMeCmpHpel.SelectedIndexChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowMeCmpFullpel
            // 
            this.snowMeCmpFullpel.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.snowMeCmpFullpel.Items.AddRange(new object[] {
            "SAD",
            "SSD",
            "5/3 Wavelet",
            "9/7 Wavelet"});
            this.snowMeCmpFullpel.Location = new System.Drawing.Point(298, 21);
            this.snowMeCmpFullpel.Name = "snowMeCmpFullpel";
            this.snowMeCmpFullpel.Size = new System.Drawing.Size(121, 21);
            this.snowMeCmpFullpel.TabIndex = 15;
            this.snowMeCmpFullpel.SelectedIndexChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowMbCompareLabel
            // 
            this.snowMbCompareLabel.Location = new System.Drawing.Point(8, 72);
            this.snowMbCompareLabel.Name = "snowMbCompareLabel";
            this.snowMbCompareLabel.Size = new System.Drawing.Size(152, 23);
            this.snowMbCompareLabel.TabIndex = 14;
            this.snowMbCompareLabel.Text = "Macroblock Compare Fct";
            // 
            // snowMeCmpHpelLabel
            // 
            this.snowMeCmpHpelLabel.Location = new System.Drawing.Point(8, 48);
            this.snowMeCmpHpelLabel.Name = "snowMeCmpHpelLabel";
            this.snowMeCmpHpelLabel.Size = new System.Drawing.Size(224, 23);
            this.snowMeCmpHpelLabel.TabIndex = 13;
            this.snowMeCmpHpelLabel.Text = "Motion Estimation Compare Fct HPel/QPel";
            // 
            // snowMeCmpFullpelLabel
            // 
            this.snowMeCmpFullpelLabel.Location = new System.Drawing.Point(8, 24);
            this.snowMeCmpFullpelLabel.Name = "snowMeCmpFullpelLabel";
            this.snowMeCmpFullpelLabel.Size = new System.Drawing.Size(208, 23);
            this.snowMeCmpFullpelLabel.TabIndex = 12;
            this.snowMeCmpFullpelLabel.Text = "Motion Estimation Compare Fct. FullPel";
            // 
            // snowToolsGroupbox
            // 
            this.snowToolsGroupbox.Controls.Add(this.snowV4mv);
            this.snowToolsGroupbox.Controls.Add(this.snowNBPredictors);
            this.snowToolsGroupbox.Controls.Add(this.snowNBPredictorsLabel);
            this.snowToolsGroupbox.Controls.Add(this.snowV4mvLabel);
            this.snowToolsGroupbox.Controls.Add(this.snowQpel);
            this.snowToolsGroupbox.Controls.Add(this.snowQpelLabel);
            this.snowToolsGroupbox.Controls.Add(this.label1);
            this.snowToolsGroupbox.Controls.Add(this.snowCreditsQuantizer);
            this.snowToolsGroupbox.Controls.Add(this.snowLosslessModeLabel);
            this.snowToolsGroupbox.Controls.Add(this.snowLosslessMode);
            this.snowToolsGroupbox.Location = new System.Drawing.Point(235, 3);
            this.snowToolsGroupbox.Name = "snowToolsGroupbox";
            this.snowToolsGroupbox.Size = new System.Drawing.Size(203, 168);
            this.snowToolsGroupbox.TabIndex = 53;
            this.snowToolsGroupbox.TabStop = false;
            this.snowToolsGroupbox.Text = "Tools";
            // 
            // snowV4mv
            // 
            this.snowV4mv.Location = new System.Drawing.Point(168, 40);
            this.snowV4mv.Name = "snowV4mv";
            this.snowV4mv.Size = new System.Drawing.Size(16, 24);
            this.snowV4mv.TabIndex = 11;
            this.snowV4mv.CheckedChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowNBPredictors
            // 
            this.snowNBPredictors.Location = new System.Drawing.Point(152, 72);
            this.snowNBPredictors.Maximum = new decimal(new int[] {
            3,
            0,
            0,
            0});
            this.snowNBPredictors.Name = "snowNBPredictors";
            this.snowNBPredictors.Size = new System.Drawing.Size(32, 20);
            this.snowNBPredictors.TabIndex = 10;
            this.snowNBPredictors.ValueChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowNBPredictorsLabel
            // 
            this.snowNBPredictorsLabel.Location = new System.Drawing.Point(8, 72);
            this.snowNBPredictorsLabel.Name = "snowNBPredictorsLabel";
            this.snowNBPredictorsLabel.Size = new System.Drawing.Size(136, 32);
            this.snowNBPredictorsLabel.TabIndex = 9;
            this.snowNBPredictorsLabel.Text = "# Motion Predictors from prev. frame";
            // 
            // snowV4mvLabel
            // 
            this.snowV4mvLabel.Location = new System.Drawing.Point(8, 48);
            this.snowV4mvLabel.Name = "snowV4mvLabel";
            this.snowV4mvLabel.Size = new System.Drawing.Size(100, 23);
            this.snowV4mvLabel.TabIndex = 8;
            this.snowV4mvLabel.Text = "V4MV";
            // 
            // snowQpel
            // 
            this.snowQpel.Location = new System.Drawing.Point(168, 16);
            this.snowQpel.Name = "snowQpel";
            this.snowQpel.Size = new System.Drawing.Size(16, 24);
            this.snowQpel.TabIndex = 7;
            this.snowQpel.CheckedChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowQpelLabel
            // 
            this.snowQpelLabel.Location = new System.Drawing.Point(8, 24);
            this.snowQpelLabel.Name = "snowQpelLabel";
            this.snowQpelLabel.Size = new System.Drawing.Size(100, 23);
            this.snowQpelLabel.TabIndex = 6;
            this.snowQpelLabel.Text = "QPel";
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(8, 112);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(100, 23);
            this.label1.TabIndex = 5;
            this.label1.Text = "Credits Quantizer";
            // 
            // snowCreditsQuantizer
            // 
            this.snowCreditsQuantizer.Location = new System.Drawing.Point(144, 112);
            this.snowCreditsQuantizer.Maximum = new decimal(new int[] {
            255,
            0,
            0,
            0});
            this.snowCreditsQuantizer.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.snowCreditsQuantizer.Name = "snowCreditsQuantizer";
            this.snowCreditsQuantizer.Size = new System.Drawing.Size(40, 20);
            this.snowCreditsQuantizer.TabIndex = 4;
            this.snowCreditsQuantizer.Value = new decimal(new int[] {
            20,
            0,
            0,
            0});
            this.snowCreditsQuantizer.ValueChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowLosslessModeLabel
            // 
            this.snowLosslessModeLabel.Location = new System.Drawing.Point(8, 136);
            this.snowLosslessModeLabel.Name = "snowLosslessModeLabel";
            this.snowLosslessModeLabel.Size = new System.Drawing.Size(80, 23);
            this.snowLosslessModeLabel.TabIndex = 4;
            this.snowLosslessModeLabel.Text = "Lossless Mode";
            // 
            // snowLosslessMode
            // 
            this.snowLosslessMode.Location = new System.Drawing.Point(168, 136);
            this.snowLosslessMode.Name = "snowLosslessMode";
            this.snowLosslessMode.Size = new System.Drawing.Size(16, 24);
            this.snowLosslessMode.TabIndex = 4;
            this.snowLosslessMode.CheckedChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowCodecGeneralGroupbox
            // 
            this.snowCodecGeneralGroupbox.Controls.Add(this.snowPredictionMode);
            this.snowCodecGeneralGroupbox.Controls.Add(this.snowPredictionModeLabel);
            this.snowCodecGeneralGroupbox.Controls.Add(this.snowQuantizer);
            this.snowCodecGeneralGroupbox.Controls.Add(this.snowEncodingMode);
            this.snowCodecGeneralGroupbox.Controls.Add(this.snowEncodingModeLabel);
            this.snowCodecGeneralGroupbox.Controls.Add(this.snowQuantizerLabel);
            this.snowCodecGeneralGroupbox.Controls.Add(this.snowBitrate);
            this.snowCodecGeneralGroupbox.Controls.Add(this.snowBitrateLabel);
            this.snowCodecGeneralGroupbox.Location = new System.Drawing.Point(0, 3);
            this.snowCodecGeneralGroupbox.Name = "snowCodecGeneralGroupbox";
            this.snowCodecGeneralGroupbox.Size = new System.Drawing.Size(224, 168);
            this.snowCodecGeneralGroupbox.TabIndex = 52;
            this.snowCodecGeneralGroupbox.TabStop = false;
            this.snowCodecGeneralGroupbox.Text = "General";
            // 
            // snowPredictionMode
            // 
            this.snowPredictionMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.snowPredictionMode.Items.AddRange(new object[] {
            "9/7 Wavelet",
            "5/3 Wavelet",
            "13/7 Wavelet"});
            this.snowPredictionMode.Location = new System.Drawing.Point(128, 133);
            this.snowPredictionMode.Name = "snowPredictionMode";
            this.snowPredictionMode.Size = new System.Drawing.Size(88, 21);
            this.snowPredictionMode.TabIndex = 5;
            this.snowPredictionMode.SelectedIndexChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowPredictionModeLabel
            // 
            this.snowPredictionModeLabel.Location = new System.Drawing.Point(8, 136);
            this.snowPredictionModeLabel.Name = "snowPredictionModeLabel";
            this.snowPredictionModeLabel.Size = new System.Drawing.Size(90, 16);
            this.snowPredictionModeLabel.TabIndex = 4;
            this.snowPredictionModeLabel.Text = "Prediction Mode";
            // 
            // snowQuantizer
            // 
            this.snowQuantizer.DecimalPlaces = 2;
            this.snowQuantizer.Location = new System.Drawing.Point(160, 45);
            this.snowQuantizer.Maximum = new decimal(new int[] {
            255,
            0,
            0,
            0});
            this.snowQuantizer.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            131072});
            this.snowQuantizer.Name = "snowQuantizer";
            this.snowQuantizer.Size = new System.Drawing.Size(56, 20);
            this.snowQuantizer.TabIndex = 3;
            this.snowQuantizer.Value = new decimal(new int[] {
            5,
            0,
            0,
            0});
            this.snowQuantizer.ValueChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowEncodingMode
            // 
            this.snowEncodingMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.snowEncodingMode.Items.AddRange(new object[] {
            "CBR",
            "CQ",
            "2pass - 1st pass",
            "2pass - 2nd pass",
            "Automated 2pass",
            "3pass - 1st pass",
            "3pass - 2nd pass",
            "3pass - 3rd pass",
            "Automated 3pass"});
            this.snowEncodingMode.Location = new System.Drawing.Point(104, 21);
            this.snowEncodingMode.Name = "snowEncodingMode";
            this.snowEncodingMode.Size = new System.Drawing.Size(112, 21);
            this.snowEncodingMode.TabIndex = 2;
            this.snowEncodingMode.SelectedIndexChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowEncodingModeLabel
            // 
            this.snowEncodingModeLabel.Location = new System.Drawing.Point(8, 24);
            this.snowEncodingModeLabel.Name = "snowEncodingModeLabel";
            this.snowEncodingModeLabel.Size = new System.Drawing.Size(90, 16);
            this.snowEncodingModeLabel.TabIndex = 1;
            this.snowEncodingModeLabel.Text = "Encoding Mode";
            // 
            // snowQuantizerLabel
            // 
            this.snowQuantizerLabel.Location = new System.Drawing.Point(8, 47);
            this.snowQuantizerLabel.Name = "snowQuantizerLabel";
            this.snowQuantizerLabel.Size = new System.Drawing.Size(64, 16);
            this.snowQuantizerLabel.TabIndex = 0;
            this.snowQuantizerLabel.Text = "Quantizer";
            // 
            // snowBitrate
            // 
            this.snowBitrate.Location = new System.Drawing.Point(176, 69);
            this.snowBitrate.Name = "snowBitrate";
            this.snowBitrate.Size = new System.Drawing.Size(40, 20);
            this.snowBitrate.TabIndex = 5;
            this.snowBitrate.Text = "700";
            this.snowBitrate.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.textField_KeyPress);
            this.snowBitrate.TextChanged += new System.EventHandler(this.updateEvent);
            // 
            // snowBitrateLabel
            // 
            this.snowBitrateLabel.Location = new System.Drawing.Point(8, 71);
            this.snowBitrateLabel.Name = "snowBitrateLabel";
            this.snowBitrateLabel.Size = new System.Drawing.Size(100, 16);
            this.snowBitrateLabel.TabIndex = 4;
            this.snowBitrateLabel.Text = "Bitrate";
            // 
            // snowConfigurationPanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Name = "snowConfigurationPanel";
            this.Size = new System.Drawing.Size(446, 424);
            this.tabControl1.ResumeLayout(false);
            this.mainTabPage.ResumeLayout(false);
            this.mainTabPage.PerformLayout();
            this.snowMotionEstimationGroupbox.ResumeLayout(false);
            this.snowToolsGroupbox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.snowNBPredictors)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.snowCreditsQuantizer)).EndInit();
            this.snowCodecGeneralGroupbox.ResumeLayout(false);
            this.snowCodecGeneralGroupbox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.snowQuantizer)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox logfile;
        private System.Windows.Forms.Button logfileOpenButton;
        private System.Windows.Forms.Label logfileLabel;
        private System.Windows.Forms.GroupBox snowMotionEstimationGroupbox;
        private System.Windows.Forms.ComboBox snowMbCompare;
        private System.Windows.Forms.ComboBox snowMeCmpHpel;
        private System.Windows.Forms.ComboBox snowMeCmpFullpel;
        private System.Windows.Forms.Label snowMbCompareLabel;
        private System.Windows.Forms.Label snowMeCmpHpelLabel;
        private System.Windows.Forms.Label snowMeCmpFullpelLabel;
        private System.Windows.Forms.GroupBox snowToolsGroupbox;
        private System.Windows.Forms.CheckBox snowV4mv;
        private System.Windows.Forms.NumericUpDown snowNBPredictors;
        private System.Windows.Forms.Label snowNBPredictorsLabel;
        private System.Windows.Forms.Label snowV4mvLabel;
        private System.Windows.Forms.CheckBox snowQpel;
        private System.Windows.Forms.Label snowQpelLabel;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.NumericUpDown snowCreditsQuantizer;
        private System.Windows.Forms.Label snowLosslessModeLabel;
        private System.Windows.Forms.CheckBox snowLosslessMode;
        private System.Windows.Forms.GroupBox snowCodecGeneralGroupbox;
        private System.Windows.Forms.ComboBox snowPredictionMode;
        private System.Windows.Forms.Label snowPredictionModeLabel;
        private System.Windows.Forms.NumericUpDown snowQuantizer;
        private System.Windows.Forms.ComboBox snowEncodingMode;
        private System.Windows.Forms.Label snowEncodingModeLabel;
        private System.Windows.Forms.Label snowQuantizerLabel;
        private System.Windows.Forms.TextBox snowBitrate;
        private System.Windows.Forms.Label snowBitrateLabel;
        private System.Windows.Forms.SaveFileDialog saveFileDialog;
    }
}
