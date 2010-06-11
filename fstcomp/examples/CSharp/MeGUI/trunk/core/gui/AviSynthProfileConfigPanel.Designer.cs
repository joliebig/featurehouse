namespace MeGUI.core.gui
{
    partial class AviSynthProfileConfigPanel
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
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.templatePage = new System.Windows.Forms.TabPage();
            this.dllBar = new MeGUI.FileBar();
            this.flowLayoutPanel1 = new System.Windows.Forms.FlowLayoutPanel();
            this.insertInput = new System.Windows.Forms.Button();
            this.insertDeinterlace = new System.Windows.Forms.Button();
            this.insertDenoise = new System.Windows.Forms.Button();
            this.insertResize = new System.Windows.Forms.Button();
            this.insertCrop = new System.Windows.Forms.Button();
            this.avisynthScript = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.extraSetupPage = new System.Windows.Forms.TabPage();
            this.mod16Box = new System.Windows.Forms.ComboBox();
            this.signalAR = new System.Windows.Forms.CheckBox();
            this.mpegOptGroupBox = new System.Windows.Forms.GroupBox();
            this.colourCorrect = new System.Windows.Forms.CheckBox();
            this.mpeg2Deblocking = new System.Windows.Forms.CheckBox();
            this.filtersGroupbox = new System.Windows.Forms.GroupBox();
            this.noiseFilterType = new System.Windows.Forms.ComboBox();
            this.resize = new System.Windows.Forms.CheckBox();
            this.noiseFilter = new System.Windows.Forms.CheckBox();
            this.resizeFilterType = new System.Windows.Forms.ComboBox();
            this.dss2 = new System.Windows.Forms.CheckBox();
            this.tabControl1.SuspendLayout();
            this.templatePage.SuspendLayout();
            this.flowLayoutPanel1.SuspendLayout();
            this.extraSetupPage.SuspendLayout();
            this.mpegOptGroupBox.SuspendLayout();
            this.filtersGroupbox.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.templatePage);
            this.tabControl1.Controls.Add(this.extraSetupPage);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(402, 405);
            this.tabControl1.TabIndex = 1;
            // 
            // templatePage
            // 
            this.templatePage.Controls.Add(this.dllBar);
            this.templatePage.Controls.Add(this.flowLayoutPanel1);
            this.templatePage.Controls.Add(this.avisynthScript);
            this.templatePage.Controls.Add(this.label1);
            this.templatePage.Location = new System.Drawing.Point(4, 22);
            this.templatePage.Name = "templatePage";
            this.templatePage.Size = new System.Drawing.Size(394, 379);
            this.templatePage.TabIndex = 0;
            this.templatePage.Text = "Template";
            // 
            // dllBar
            // 
            this.dllBar.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.dllBar.Filename = "";
            this.dllBar.Filter = "DLL Files (*.dll)|*.dll";
            this.dllBar.FilterIndex = 0;
            this.dllBar.FolderMode = false;
            this.dllBar.Location = new System.Drawing.Point(63, 350);
            this.dllBar.Name = "dllBar";
            this.dllBar.ReadOnly = true;
            this.dllBar.SaveMode = false;
            this.dllBar.Size = new System.Drawing.Size(328, 26);
            this.dllBar.TabIndex = 46;
            this.dllBar.Title = "Select AviSynth Plugin DLL to open";
            this.dllBar.FileSelected += new MeGUI.FileBarEventHandler(this.dllBar_FileSelected);
            // 
            // flowLayoutPanel1
            // 
            this.flowLayoutPanel1.AutoSize = true;
            this.flowLayoutPanel1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.flowLayoutPanel1.Controls.Add(this.insertInput);
            this.flowLayoutPanel1.Controls.Add(this.insertDeinterlace);
            this.flowLayoutPanel1.Controls.Add(this.insertDenoise);
            this.flowLayoutPanel1.Controls.Add(this.insertResize);
            this.flowLayoutPanel1.Controls.Add(this.insertCrop);
            this.flowLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Top;
            this.flowLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.flowLayoutPanel1.Name = "flowLayoutPanel1";
            this.flowLayoutPanel1.Size = new System.Drawing.Size(394, 29);
            this.flowLayoutPanel1.TabIndex = 45;
            // 
            // insertInput
            // 
            this.insertInput.AutoSize = true;
            this.insertInput.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.insertInput.Location = new System.Drawing.Point(3, 3);
            this.insertInput.Name = "insertInput";
            this.insertInput.Size = new System.Drawing.Size(62, 23);
            this.insertInput.TabIndex = 54;
            this.insertInput.Tag = "<input>";
            this.insertInput.Text = "Add input";
            this.insertInput.Click += new System.EventHandler(this.insert_Click);
            // 
            // insertDeinterlace
            // 
            this.insertDeinterlace.AutoSize = true;
            this.insertDeinterlace.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.insertDeinterlace.Location = new System.Drawing.Point(71, 3);
            this.insertDeinterlace.Name = "insertDeinterlace";
            this.insertDeinterlace.Size = new System.Drawing.Size(91, 23);
            this.insertDeinterlace.TabIndex = 55;
            this.insertDeinterlace.Tag = "<deinterlace>";
            this.insertDeinterlace.Text = "Add deinterlace";
            this.insertDeinterlace.Click += new System.EventHandler(this.insert_Click);
            // 
            // insertDenoise
            // 
            this.insertDenoise.AutoSize = true;
            this.insertDenoise.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.insertDenoise.Location = new System.Drawing.Point(168, 3);
            this.insertDenoise.Name = "insertDenoise";
            this.insertDenoise.Size = new System.Drawing.Size(76, 23);
            this.insertDenoise.TabIndex = 51;
            this.insertDenoise.Tag = "<denoise>";
            this.insertDenoise.Text = "Add denoise";
            this.insertDenoise.Click += new System.EventHandler(this.insert_Click);
            // 
            // insertResize
            // 
            this.insertResize.AutoSize = true;
            this.insertResize.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.insertResize.Location = new System.Drawing.Point(250, 3);
            this.insertResize.Name = "insertResize";
            this.insertResize.Size = new System.Drawing.Size(66, 23);
            this.insertResize.TabIndex = 52;
            this.insertResize.Tag = "<resize>";
            this.insertResize.Text = "Add resize";
            this.insertResize.Click += new System.EventHandler(this.insert_Click);
            // 
            // insertCrop
            // 
            this.insertCrop.AutoSize = true;
            this.insertCrop.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.insertCrop.Location = new System.Drawing.Point(322, 3);
            this.insertCrop.Name = "insertCrop";
            this.insertCrop.Size = new System.Drawing.Size(60, 23);
            this.insertCrop.TabIndex = 53;
            this.insertCrop.Tag = "<crop>";
            this.insertCrop.Text = "Add crop";
            this.insertCrop.Click += new System.EventHandler(this.insert_Click);
            // 
            // avisynthScript
            // 
            this.avisynthScript.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.avisynthScript.Location = new System.Drawing.Point(3, 32);
            this.avisynthScript.Multiline = true;
            this.avisynthScript.Name = "avisynthScript";
            this.avisynthScript.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.avisynthScript.Size = new System.Drawing.Size(388, 312);
            this.avisynthScript.TabIndex = 46;
            // 
            // label1
            // 
            this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(3, 356);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(54, 13);
            this.label1.TabIndex = 42;
            this.label1.Text = "Load DLL";
            // 
            // extraSetupPage
            // 
            this.extraSetupPage.Controls.Add(this.dss2);
            this.extraSetupPage.Controls.Add(this.mod16Box);
            this.extraSetupPage.Controls.Add(this.signalAR);
            this.extraSetupPage.Controls.Add(this.mpegOptGroupBox);
            this.extraSetupPage.Controls.Add(this.filtersGroupbox);
            this.extraSetupPage.Location = new System.Drawing.Point(4, 22);
            this.extraSetupPage.Name = "extraSetupPage";
            this.extraSetupPage.Size = new System.Drawing.Size(394, 379);
            this.extraSetupPage.TabIndex = 1;
            this.extraSetupPage.Text = "Extra Setup";
            // 
            // mod16Box
            // 
            this.mod16Box.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.mod16Box.Enabled = false;
            this.mod16Box.FormattingEnabled = true;
            this.mod16Box.Items.AddRange(new object[] {
            "Resize to mod16",
            "Overcrop to achieve mod16",
            "Encode non-mod16",
            "Crop mod4 horizontally",
            "Undercrop to achieve mod16"});
            this.mod16Box.Location = new System.Drawing.Point(210, 181);
            this.mod16Box.Name = "mod16Box";
            this.mod16Box.Size = new System.Drawing.Size(157, 21);
            this.mod16Box.TabIndex = 21;
            // 
            // signalAR
            // 
            this.signalAR.AutoSize = true;
            this.signalAR.Location = new System.Drawing.Point(13, 183);
            this.signalAR.Name = "signalAR";
            this.signalAR.Size = new System.Drawing.Size(189, 17);
            this.signalAR.TabIndex = 20;
            this.signalAR.Text = "Clever (TM) anamorphic encoding:";
            this.signalAR.Click += new System.EventHandler(this.signalAR_CheckedChanged);
            // 
            // mpegOptGroupBox
            // 
            this.mpegOptGroupBox.Controls.Add(this.colourCorrect);
            this.mpegOptGroupBox.Controls.Add(this.mpeg2Deblocking);
            this.mpegOptGroupBox.Location = new System.Drawing.Point(8, 98);
            this.mpegOptGroupBox.Name = "mpegOptGroupBox";
            this.mpegOptGroupBox.Size = new System.Drawing.Size(197, 79);
            this.mpegOptGroupBox.TabIndex = 12;
            this.mpegOptGroupBox.TabStop = false;
            this.mpegOptGroupBox.Text = "Mpeg Options";
            // 
            // colourCorrect
            // 
            this.colourCorrect.Location = new System.Drawing.Point(5, 45);
            this.colourCorrect.Name = "colourCorrect";
            this.colourCorrect.Size = new System.Drawing.Size(124, 18);
            this.colourCorrect.TabIndex = 9;
            this.colourCorrect.Text = "Colour Correction";
            // 
            // mpeg2Deblocking
            // 
            this.mpeg2Deblocking.Location = new System.Drawing.Point(5, 20);
            this.mpeg2Deblocking.Name = "mpeg2Deblocking";
            this.mpeg2Deblocking.Size = new System.Drawing.Size(124, 18);
            this.mpeg2Deblocking.TabIndex = 8;
            this.mpeg2Deblocking.Text = "Mpeg2 Deblocking";
            // 
            // filtersGroupbox
            // 
            this.filtersGroupbox.Controls.Add(this.noiseFilterType);
            this.filtersGroupbox.Controls.Add(this.resize);
            this.filtersGroupbox.Controls.Add(this.noiseFilter);
            this.filtersGroupbox.Controls.Add(this.resizeFilterType);
            this.filtersGroupbox.Location = new System.Drawing.Point(8, 8);
            this.filtersGroupbox.Name = "filtersGroupbox";
            this.filtersGroupbox.Size = new System.Drawing.Size(400, 84);
            this.filtersGroupbox.TabIndex = 1;
            this.filtersGroupbox.TabStop = false;
            this.filtersGroupbox.Text = "Filters";
            // 
            // noiseFilterType
            // 
            this.noiseFilterType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.noiseFilterType.Enabled = false;
            this.noiseFilterType.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.noiseFilterType.ItemHeight = 13;
            this.noiseFilterType.Location = new System.Drawing.Point(152, 51);
            this.noiseFilterType.Name = "noiseFilterType";
            this.noiseFilterType.Size = new System.Drawing.Size(121, 21);
            this.noiseFilterType.TabIndex = 5;
            // 
            // resize
            // 
            this.resize.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.resize.Location = new System.Drawing.Point(6, 24);
            this.resize.Name = "resize";
            this.resize.Size = new System.Drawing.Size(104, 24);
            this.resize.TabIndex = 3;
            this.resize.Text = "Resize Filter";
            this.resize.CheckedChanged += new System.EventHandler(this.resize_CheckedChanged);
            // 
            // noiseFilter
            // 
            this.noiseFilter.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.noiseFilter.Location = new System.Drawing.Point(6, 49);
            this.noiseFilter.Name = "noiseFilter";
            this.noiseFilter.Size = new System.Drawing.Size(104, 24);
            this.noiseFilter.TabIndex = 3;
            this.noiseFilter.Text = "Noise Filter";
            this.noiseFilter.CheckedChanged += new System.EventHandler(this.noiseFilter_CheckedChanged);
            // 
            // resizeFilterType
            // 
            this.resizeFilterType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.resizeFilterType.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.resizeFilterType.ItemHeight = 13;
            this.resizeFilterType.Location = new System.Drawing.Point(152, 24);
            this.resizeFilterType.Name = "resizeFilterType";
            this.resizeFilterType.Size = new System.Drawing.Size(121, 21);
            this.resizeFilterType.TabIndex = 1;
            // 
            // dss2
            // 
            this.dss2.AutoSize = true;
            this.dss2.Enabled = false;
            this.dss2.Location = new System.Drawing.Point(13, 218);
            this.dss2.Name = "dss2";
            this.dss2.Size = new System.Drawing.Size(201, 17);
            this.dss2.TabIndex = 22;
            this.dss2.Text = "Prefer DSS2 over DirectShowSource";
            this.dss2.UseVisualStyleBackColor = true;
            // 
            // AviSynthProfileConfigPanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tabControl1);
            this.Name = "AviSynthProfileConfigPanel";
            this.Size = new System.Drawing.Size(402, 405);
            this.tabControl1.ResumeLayout(false);
            this.templatePage.ResumeLayout(false);
            this.templatePage.PerformLayout();
            this.flowLayoutPanel1.ResumeLayout(false);
            this.flowLayoutPanel1.PerformLayout();
            this.extraSetupPage.ResumeLayout(false);
            this.extraSetupPage.PerformLayout();
            this.mpegOptGroupBox.ResumeLayout(false);
            this.filtersGroupbox.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage templatePage;
        private System.Windows.Forms.Button insertCrop;
        private System.Windows.Forms.Button insertInput;
        private System.Windows.Forms.Button insertDeinterlace;
        private System.Windows.Forms.Button insertDenoise;
        private System.Windows.Forms.Button insertResize;
        private System.Windows.Forms.TextBox avisynthScript;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TabPage extraSetupPage;
        private System.Windows.Forms.ComboBox mod16Box;
        private System.Windows.Forms.CheckBox signalAR;
        private System.Windows.Forms.GroupBox mpegOptGroupBox;
        private System.Windows.Forms.CheckBox colourCorrect;
        private System.Windows.Forms.CheckBox mpeg2Deblocking;
        private System.Windows.Forms.GroupBox filtersGroupbox;
        private System.Windows.Forms.ComboBox noiseFilterType;
        private System.Windows.Forms.CheckBox resize;
        private System.Windows.Forms.CheckBox noiseFilter;
        private System.Windows.Forms.ComboBox resizeFilterType;
        private FileBar dllBar;
        private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;
        private System.Windows.Forms.CheckBox dss2;

    }
}
