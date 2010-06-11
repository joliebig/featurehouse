namespace MeGUI.packages.tools.oneclick
{
    partial class OneClickConfigPanel
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.Windows.Forms.Label label3;
            this.otherGroupBox = new System.Windows.Forms.GroupBox();
            this.keepInputResolution = new System.Windows.Forms.CheckBox();
            this.autoCrop = new System.Windows.Forms.CheckBox();
            this.avsProfile = new MeGUI.core.gui.ConfigableProfilesControl();
            this.splitSize = new MeGUI.core.gui.TargetSizeSCBox();
            this.fileSize = new MeGUI.core.gui.TargetSizeSCBox();
            this.preprocessVideo = new System.Windows.Forms.CheckBox();
            this.label2 = new System.Windows.Forms.Label();
            this.filesizeLabel = new System.Windows.Forms.Label();
            this.autoDeint = new System.Windows.Forms.CheckBox();
            this.signalAR = new System.Windows.Forms.CheckBox();
            this.horizontalResolution = new System.Windows.Forms.NumericUpDown();
            this.outputResolutionLabel = new System.Windows.Forms.Label();
            this.extraGroupbox = new System.Windows.Forms.GroupBox();
            this.usechaptersmarks = new System.Windows.Forms.CheckBox();
            this.label1 = new System.Windows.Forms.Label();
            this.audioProfile = new MeGUI.core.gui.ConfigableProfilesControl();
            this.videoProfile = new MeGUI.core.gui.ConfigableProfilesControl();
            this.audioProfileLabel = new System.Windows.Forms.Label();
            this.dontEncodeAudio = new System.Windows.Forms.CheckBox();
            this.videoCodecLabel = new System.Windows.Forms.Label();
            this.containerFormatLabel = new System.Windows.Forms.Label();
            this.containerTypeList = new System.Windows.Forms.CheckedListBox();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            label3 = new System.Windows.Forms.Label();
            this.otherGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.horizontalResolution)).BeginInit();
            this.extraGroupbox.SuspendLayout();
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.SuspendLayout();
            // 
            // label3
            // 
            label3.AutoSize = true;
            label3.Location = new System.Drawing.Point(5, 128);
            label3.Name = "label3";
            label3.Size = new System.Drawing.Size(78, 13);
            label3.TabIndex = 40;
            label3.Text = "Avisynth profile";
            // 
            // otherGroupBox
            // 
            this.otherGroupBox.Controls.Add(this.keepInputResolution);
            this.otherGroupBox.Controls.Add(this.autoCrop);
            this.otherGroupBox.Controls.Add(label3);
            this.otherGroupBox.Controls.Add(this.avsProfile);
            this.otherGroupBox.Controls.Add(this.splitSize);
            this.otherGroupBox.Controls.Add(this.fileSize);
            this.otherGroupBox.Controls.Add(this.preprocessVideo);
            this.otherGroupBox.Controls.Add(this.label2);
            this.otherGroupBox.Controls.Add(this.filesizeLabel);
            this.otherGroupBox.Controls.Add(this.autoDeint);
            this.otherGroupBox.Controls.Add(this.signalAR);
            this.otherGroupBox.Controls.Add(this.horizontalResolution);
            this.otherGroupBox.Controls.Add(this.outputResolutionLabel);
            this.otherGroupBox.Dock = System.Windows.Forms.DockStyle.Top;
            this.otherGroupBox.Location = new System.Drawing.Point(3, 3);
            this.otherGroupBox.Name = "otherGroupBox";
            this.otherGroupBox.Size = new System.Drawing.Size(419, 175);
            this.otherGroupBox.TabIndex = 38;
            this.otherGroupBox.TabStop = false;
            this.otherGroupBox.Text = "Filesize and Avisynth setup";
            // 
            // keepInputResolution
            // 
            this.keepInputResolution.AutoSize = true;
            this.keepInputResolution.Location = new System.Drawing.Point(60, 103);
            this.keepInputResolution.Name = "keepInputResolution";
            this.keepInputResolution.Size = new System.Drawing.Size(242, 17);
            this.keepInputResolution.TabIndex = 42;
            this.keepInputResolution.Text = "Keep Input Resolution (disable Crop && Resize)";
            this.keepInputResolution.UseVisualStyleBackColor = true;
            this.keepInputResolution.CheckedChanged += new System.EventHandler(this.keepInputResolution_CheckedChanged);
            // 
            // autoCrop
            // 
            this.autoCrop.AutoSize = true;
            this.autoCrop.Checked = true;
            this.autoCrop.CheckState = System.Windows.Forms.CheckState.Checked;
            this.autoCrop.Location = new System.Drawing.Point(189, 81);
            this.autoCrop.Name = "autoCrop";
            this.autoCrop.Size = new System.Drawing.Size(70, 17);
            this.autoCrop.TabIndex = 41;
            this.autoCrop.Text = "AutoCrop";
            this.autoCrop.UseVisualStyleBackColor = true;
            // 
            // avsProfile
            // 
            this.avsProfile.Location = new System.Drawing.Point(109, 123);
            this.avsProfile.Name = "avsProfile";
            this.avsProfile.ProfileSet = "AviSynth";
            this.avsProfile.Size = new System.Drawing.Size(298, 22);
            this.avsProfile.TabIndex = 39;
            // 
            // splitSize
            // 
            this.splitSize.CustomSizes = new MeGUI.core.util.FileSize[0];
            this.splitSize.Location = new System.Drawing.Point(110, 45);
            this.splitSize.MaximumSize = new System.Drawing.Size(1000, 29);
            this.splitSize.MinimumSize = new System.Drawing.Size(64, 29);
            this.splitSize.Name = "splitSize";
            this.splitSize.NullString = "Dont split";
            this.splitSize.SelectedIndex = 0;
            this.splitSize.Size = new System.Drawing.Size(208, 29);
            this.splitSize.TabIndex = 38;
            // 
            // fileSize
            // 
            this.fileSize.CustomSizes = new MeGUI.core.util.FileSize[0];
            this.fileSize.Location = new System.Drawing.Point(110, 19);
            this.fileSize.MaximumSize = new System.Drawing.Size(1000, 29);
            this.fileSize.MinimumSize = new System.Drawing.Size(64, 29);
            this.fileSize.Name = "fileSize";
            this.fileSize.NullString = "Don\'t care";
            this.fileSize.SelectedIndex = 0;
            this.fileSize.Size = new System.Drawing.Size(208, 29);
            this.fileSize.TabIndex = 38;
            // 
            // preprocessVideo
            // 
            this.preprocessVideo.AutoSize = true;
            this.preprocessVideo.Location = new System.Drawing.Point(60, 152);
            this.preprocessVideo.Name = "preprocessVideo";
            this.preprocessVideo.Size = new System.Drawing.Size(179, 17);
            this.preprocessVideo.TabIndex = 37;
            this.preprocessVideo.Text = "Prerender video (for slow scripts)";
            this.preprocessVideo.UseVisualStyleBackColor = true;
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(6, 53);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(90, 13);
            this.label2.TabIndex = 25;
            this.label2.Text = "Splitting:";
            // 
            // filesizeLabel
            // 
            this.filesizeLabel.Location = new System.Drawing.Point(6, 27);
            this.filesizeLabel.Name = "filesizeLabel";
            this.filesizeLabel.Size = new System.Drawing.Size(90, 13);
            this.filesizeLabel.TabIndex = 25;
            this.filesizeLabel.Text = "Filesize";
            // 
            // autoDeint
            // 
            this.autoDeint.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.autoDeint.AutoSize = true;
            this.autoDeint.Location = new System.Drawing.Point(273, 152);
            this.autoDeint.Name = "autoDeint";
            this.autoDeint.Size = new System.Drawing.Size(138, 17);
            this.autoDeint.TabIndex = 35;
            this.autoDeint.Text = "Automatic Deinterlacing";
            this.autoDeint.UseVisualStyleBackColor = true;
            // 
            // signalAR
            // 
            this.signalAR.Location = new System.Drawing.Point(274, 77);
            this.signalAR.Name = "signalAR";
            this.signalAR.Size = new System.Drawing.Size(76, 24);
            this.signalAR.TabIndex = 32;
            this.signalAR.Text = "Signal AR";
            // 
            // horizontalResolution
            // 
            this.horizontalResolution.Increment = new decimal(new int[] {
            16,
            0,
            0,
            0});
            this.horizontalResolution.Location = new System.Drawing.Point(110, 80);
            this.horizontalResolution.Maximum = new decimal(new int[] {
            5000,
            0,
            0,
            0});
            this.horizontalResolution.Minimum = new decimal(new int[] {
            16,
            0,
            0,
            0});
            this.horizontalResolution.Name = "horizontalResolution";
            this.horizontalResolution.Size = new System.Drawing.Size(64, 20);
            this.horizontalResolution.TabIndex = 27;
            this.horizontalResolution.Value = new decimal(new int[] {
            640,
            0,
            0,
            0});
            // 
            // outputResolutionLabel
            // 
            this.outputResolutionLabel.Location = new System.Drawing.Point(6, 84);
            this.outputResolutionLabel.Name = "outputResolutionLabel";
            this.outputResolutionLabel.Size = new System.Drawing.Size(100, 13);
            this.outputResolutionLabel.TabIndex = 30;
            this.outputResolutionLabel.Text = "Output Resolution";
            // 
            // extraGroupbox
            // 
            this.extraGroupbox.Controls.Add(this.usechaptersmarks);
            this.extraGroupbox.Controls.Add(this.label1);
            this.extraGroupbox.Controls.Add(this.audioProfile);
            this.extraGroupbox.Controls.Add(this.videoProfile);
            this.extraGroupbox.Controls.Add(this.audioProfileLabel);
            this.extraGroupbox.Controls.Add(this.dontEncodeAudio);
            this.extraGroupbox.Controls.Add(this.videoCodecLabel);
            this.extraGroupbox.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.extraGroupbox.Location = new System.Drawing.Point(3, 184);
            this.extraGroupbox.Name = "extraGroupbox";
            this.extraGroupbox.Size = new System.Drawing.Size(419, 143);
            this.extraGroupbox.TabIndex = 39;
            this.extraGroupbox.TabStop = false;
            this.extraGroupbox.Text = "Encoding Setup";
            // 
            // usechaptersmarks
            // 
            this.usechaptersmarks.AutoSize = true;
            this.usechaptersmarks.Location = new System.Drawing.Point(168, 77);
            this.usechaptersmarks.Name = "usechaptersmarks";
            this.usechaptersmarks.Size = new System.Drawing.Size(229, 17);
            this.usechaptersmarks.TabIndex = 40;
            this.usechaptersmarks.Text = "Force using Key-Frames for chapters marks";
            this.usechaptersmarks.UseVisualStyleBackColor = true;
            // 
            // label1
            // 
            this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.label1.Location = new System.Drawing.Point(16, 110);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(396, 31);
            this.label1.TabIndex = 29;
            this.label1.Text = "Note: unless changed in the One Click Window, these audio settings will be used f" +
                "or all selected tracks.";
            // 
            // audioProfile
            // 
            this.audioProfile.Location = new System.Drawing.Point(123, 47);
            this.audioProfile.Name = "audioProfile";
            this.audioProfile.ProfileSet = "Audio";
            this.audioProfile.Size = new System.Drawing.Size(285, 22);
            this.audioProfile.TabIndex = 39;
            // 
            // videoProfile
            // 
            this.videoProfile.Location = new System.Drawing.Point(123, 19);
            this.videoProfile.Name = "videoProfile";
            this.videoProfile.ProfileSet = "Video";
            this.videoProfile.Size = new System.Drawing.Size(285, 22);
            this.videoProfile.TabIndex = 39;
            // 
            // audioProfileLabel
            // 
            this.audioProfileLabel.Location = new System.Drawing.Point(16, 52);
            this.audioProfileLabel.Name = "audioProfileLabel";
            this.audioProfileLabel.Size = new System.Drawing.Size(100, 13);
            this.audioProfileLabel.TabIndex = 27;
            this.audioProfileLabel.Text = "Audio Codec";
            // 
            // dontEncodeAudio
            // 
            this.dontEncodeAudio.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.dontEncodeAudio.AutoSize = true;
            this.dontEncodeAudio.Location = new System.Drawing.Point(28, 77);
            this.dontEncodeAudio.Name = "dontEncodeAudio";
            this.dontEncodeAudio.Size = new System.Drawing.Size(119, 17);
            this.dontEncodeAudio.TabIndex = 25;
            this.dontEncodeAudio.Text = "Don\'t encode audio";
            this.dontEncodeAudio.CheckedChanged += new System.EventHandler(this.dontEncodeAudio_CheckedChanged);
            // 
            // videoCodecLabel
            // 
            this.videoCodecLabel.Location = new System.Drawing.Point(16, 19);
            this.videoCodecLabel.Name = "videoCodecLabel";
            this.videoCodecLabel.Size = new System.Drawing.Size(90, 13);
            this.videoCodecLabel.TabIndex = 17;
            this.videoCodecLabel.Text = "Video Preset";
            // 
            // containerFormatLabel
            // 
            this.containerFormatLabel.Dock = System.Windows.Forms.DockStyle.Top;
            this.containerFormatLabel.Location = new System.Drawing.Point(3, 3);
            this.containerFormatLabel.Name = "containerFormatLabel";
            this.containerFormatLabel.Size = new System.Drawing.Size(419, 56);
            this.containerFormatLabel.TabIndex = 17;
            this.containerFormatLabel.Text = "Text change later for resource behavior reasons";
            // 
            // containerTypeList
            // 
            this.containerTypeList.CheckOnClick = true;
            this.containerTypeList.Dock = System.Windows.Forms.DockStyle.Fill;
            this.containerTypeList.Location = new System.Drawing.Point(3, 59);
            this.containerTypeList.Name = "containerTypeList";
            this.containerTypeList.Size = new System.Drawing.Size(419, 259);
            this.containerTypeList.TabIndex = 0;
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(433, 356);
            this.tabControl1.TabIndex = 39;
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.otherGroupBox);
            this.tabPage1.Controls.Add(this.extraGroupbox);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(425, 330);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Encoding Setup";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.containerTypeList);
            this.tabPage2.Controls.Add(this.containerFormatLabel);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(425, 330);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "Container type";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // OneClickConfigPanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tabControl1);
            this.Name = "OneClickConfigPanel";
            this.Size = new System.Drawing.Size(433, 356);
            this.otherGroupBox.ResumeLayout(false);
            this.otherGroupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.horizontalResolution)).EndInit();
            this.extraGroupbox.ResumeLayout(false);
            this.extraGroupbox.PerformLayout();
            this.tabControl1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.tabPage2.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox otherGroupBox;
        private System.Windows.Forms.Label filesizeLabel;
        private System.Windows.Forms.CheckBox autoDeint;
        private System.Windows.Forms.CheckBox signalAR;
        private System.Windows.Forms.NumericUpDown horizontalResolution;
        private System.Windows.Forms.Label outputResolutionLabel;
        private System.Windows.Forms.GroupBox extraGroupbox;
        private System.Windows.Forms.Label audioProfileLabel;
        private System.Windows.Forms.CheckBox dontEncodeAudio;
        private System.Windows.Forms.Label videoCodecLabel;
        private System.Windows.Forms.Label containerFormatLabel;
        private System.Windows.Forms.CheckBox preprocessVideo;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckedListBox containerTypeList;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.TabPage tabPage2;
        private MeGUI.core.gui.TargetSizeSCBox splitSize;
        private MeGUI.core.gui.TargetSizeSCBox fileSize;
        private System.Windows.Forms.Label label2;
        private MeGUI.core.gui.ConfigableProfilesControl avsProfile;
        private MeGUI.core.gui.ConfigableProfilesControl audioProfile;
        private MeGUI.core.gui.ConfigableProfilesControl videoProfile;
        private System.Windows.Forms.CheckBox autoCrop;
        private System.Windows.Forms.CheckBox keepInputResolution;
        private System.Windows.Forms.CheckBox usechaptersmarks;
    }
}
