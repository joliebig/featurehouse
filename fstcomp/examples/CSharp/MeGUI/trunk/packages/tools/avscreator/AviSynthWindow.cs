// ****************************************************************************
// 
// Copyright (C) 2005-2009  Doom9 & al
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// 
// ****************************************************************************

using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Imaging;
using System.Globalization;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using MeGUI.core.details.video;
using MeGUI.core.plugins.interfaces;
using MeGUI.core.util;

namespace MeGUI
{

    /// <summary>
	/// Summary description for AviSynthWindow.
	/// </summary>
	public class AviSynthWindow : System.Windows.Forms.Form
	{
		
        string originalScript;
        bool originalInlineAvs;
        bool loaded;


        bool isPreviewMode = false;
        private CultureInfo ci = new CultureInfo("en-us");
        private bool eventsOn = true;
		private string path;
		private VideoPlayer player;
        private IMediaFile file;
        private IVideoReader reader;
		private StringBuilder script;
		public event OpenScriptCallback OpenScript;
        private Dar? suggestedDar;
        private MainForm mainForm;
        private JobUtil jobUtil;
        private PossibleSources sourceType;
        private SourceDetector detector;
        
        private System.Windows.Forms.GroupBox resNCropGroupbox;
		private System.Windows.Forms.CheckBox crop;
        private System.Windows.Forms.Button autoCropButton;
		private System.Windows.Forms.TabControl tabControl1;
		private System.Windows.Forms.TabPage optionsTab;
        private System.Windows.Forms.TabPage editTab;
		private System.Windows.Forms.GroupBox videoGroupBox;
        private System.Windows.Forms.Label videoInputLabel;
        private System.Windows.Forms.Label inputDARLabel;
        private System.Windows.Forms.Label tvTypeLabel;
		private System.Windows.Forms.SaveFileDialog saveAvisynthScriptDialog;
        private System.Windows.Forms.TextBox avisynthScript;
		private System.Windows.Forms.NumericUpDown horizontalResolution;
		private System.Windows.Forms.NumericUpDown verticalResolution;
		private System.Windows.Forms.NumericUpDown cropLeft;
		private System.Windows.Forms.NumericUpDown cropRight;
		private System.Windows.Forms.NumericUpDown cropBottom;
		private System.Windows.Forms.NumericUpDown cropTop;
        private System.Windows.Forms.OpenFileDialog openFilterDialog;
        private System.Windows.Forms.OpenFileDialog openSubsDialog;
		private System.Windows.Forms.CheckBox suggestResolution;
        private CheckBox signalAR;


        private List<Control> controlsToDisable;
        private TabPage filterTab;
        private GroupBox deinterlacingGroupBox;
        private CheckBox deintIsAnime;
        private Button analyseButton;
        private CheckBox deinterlace;
        private ComboBox deinterlaceType;
        private GroupBox filtersGroupbox;
        private ComboBox noiseFilterType;
        private CheckBox noiseFilter;
        private ComboBox resizeFilterType;
        private Label resizeFilterLabel;
        private CheckBox resize;
        private ComboBox mod16Box;
        private Label label1;
        private Button openDLLButton;
        private TextBox dllPath;
        private ComboBox deintFieldOrder;
        private ComboBox deintSourceType;
        private NumericUpDown deintM;
        private StatusStrip statusStrip1;
        private ToolStripProgressBar deintProgressBar;
        private ToolStripStatusLabel deintStatusLabel;
        private Button reopenOriginal;
        private MeGUI.core.gui.ARChooser arChooser;
        private MeGUI.core.gui.ConfigableProfilesControl avsProfile;
        private Label label6;
        private CheckBox chAutoPreview;
        private GroupBox gbOutput;
        private Label label7;
        private FileBar videoOutput;
        private FileBar input;
        private CheckBox onSaveLoadScript;
        private Button saveButton;
        private Button previewAvsButton;
        private MeGUI.core.gui.HelpButton helpButton1;
        private TabControl tabSources;
        private TabPage tabPage1;
        private GroupBox mpegOptGroupBox;
        private CheckBox colourCorrect;
        private CheckBox mpeg2Deblocking;
        private TabPage tabPage2;
        private GroupBox aviOptGroupBox;
        private NumericUpDown fpsBox;
        private Label fpsLabel;
        private CheckBox flipVertical;
        private TabPage tabPage3;
        private GroupBox dgOptions;
        private ComboBox cbNvDeInt;
        private CheckBox nvDeInt;
        private Label SubtitlesLabel;
        private Button openSubtitlesButton;
        private TextBox SubtitlesPath;
        private CheckBox dss2;
        private Label label8;
        private ComboBox cbCharset;
        private CheckBox nvResize;

		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
		
		
        public AviSynthWindow(MainForm mainForm)
        {
            this.loaded = false;
            this.mainForm = mainForm;
			InitializeComponent();
            avsProfile.Manager = MainForm.Instance.Profiles;

            this.controlsToDisable = new List<Control>();

            this.controlsToDisable.Add(reopenOriginal);
            this.controlsToDisable.Add(filtersGroupbox);
            this.controlsToDisable.Add(deinterlacingGroupBox);
            this.controlsToDisable.Add(mpegOptGroupBox);
            this.controlsToDisable.Add(aviOptGroupBox);
            this.controlsToDisable.Add(resNCropGroupbox);
            this.controlsToDisable.Add(previewAvsButton);
            this.controlsToDisable.Add(saveButton);
            this.controlsToDisable.Add(arChooser);
            this.controlsToDisable.Add(inputDARLabel);
            this.controlsToDisable.Add(signalAR);
            this.controlsToDisable.Add(avisynthScript);
            this.controlsToDisable.Add(openDLLButton);
            this.controlsToDisable.Add(dgOptions);

            enableControls(false);
            script = new StringBuilder();

			this.path = mainForm.MeGUIPath;

            this.resizeFilterType.Items.Clear();
            this.resizeFilterType.DataSource = ScriptServer.ListOfResizeFilterType;
            this.resizeFilterType.BindingContext = new BindingContext();
            this.noiseFilterType.Items.Clear();
            this.noiseFilterType.DataSource = ScriptServer.ListOfDenoiseFilterType;
            this.noiseFilterType.BindingContext = new BindingContext();

            this.deintFieldOrder.Items.Clear();
            this.deintFieldOrder.DataSource = ScriptServer.ListOfFieldOrders;
            this.deintFieldOrder.BindingContext = new BindingContext();
            this.deintSourceType.Items.Clear();
            this.deintSourceType.DataSource = ScriptServer.ListOfSourceTypes;
            this.deintSourceType.BindingContext = new BindingContext();
            this.cbNvDeInt.Items.Clear();
            this.cbNvDeInt.DataSource = ScriptServer.ListOfNvDeIntType;
            this.cbNvDeInt.BindingContext = new BindingContext();
            deintFieldOrder.SelectedIndex = -1;
            deintSourceType.SelectedIndex = -1;
            cbNvDeInt.SelectedIndex = 0;
            cbCharset.SelectedIndex = 0;

            this.noiseFilterType.SelectedIndexChanged += new System.EventHandler(this.noiseFilterType_SelectedIndexChanged);
            this.resizeFilterType.SelectedIndexChanged += new System.EventHandler(this.resizeFilterType_SelectedIndexChanged);
            this.cbNvDeInt.SelectedIndexChanged += new System.EventHandler(this.cbNvDeInt_SelectedIndexChanged);

			player = null;
			this.crop.Checked = false;
			this.cropLeft.Value = 0;
			this.cropTop.Value = 0;
			this.cropRight.Value = 0;
			this.cropBottom.Value = 0;

            deinterlaceType.DataSource = new DeinterlaceFilter[] { new DeinterlaceFilter("Do nothing (source not detected)", "#blank deinterlace line") };
            this.jobUtil = new JobUtil(mainForm);
			this.showScript();

            this.loaded = true;
            ProfileChanged(null, null);
		}

        void ProfileChanged(object sender, EventArgs e)
        {
            this.Settings = GetProfileSettings();
        }
		/// <summary>
		/// constructor that first initializes everything using the default constructor
		/// then opens a preview window with the video given as parameter
		/// </summary>
		/// <param name="videoInput">the DGIndex script to be loaded</param>
		public AviSynthWindow(MainForm mainForm, string videoInput) : this(mainForm)
		{
            openVideoSource(videoInput);
		}

		protected override void OnClosing(CancelEventArgs e)
		{
            if (player != null)
				player.Close();
            if (detector != null)
                detector.stop();
            detector = null;
			base.OnClosing (e);
		}
		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if(components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}
		
		
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
            System.Windows.Forms.Label label2;
            System.Windows.Forms.Label label3;
            System.Windows.Forms.Label label4;
            System.Windows.Forms.Label label5;
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AviSynthWindow));
            this.resNCropGroupbox = new System.Windows.Forms.GroupBox();
            this.chAutoPreview = new System.Windows.Forms.CheckBox();
            this.resize = new System.Windows.Forms.CheckBox();
            this.suggestResolution = new System.Windows.Forms.CheckBox();
            this.cropLeft = new System.Windows.Forms.NumericUpDown();
            this.cropRight = new System.Windows.Forms.NumericUpDown();
            this.cropBottom = new System.Windows.Forms.NumericUpDown();
            this.cropTop = new System.Windows.Forms.NumericUpDown();
            this.autoCropButton = new System.Windows.Forms.Button();
            this.crop = new System.Windows.Forms.CheckBox();
            this.verticalResolution = new System.Windows.Forms.NumericUpDown();
            this.horizontalResolution = new System.Windows.Forms.NumericUpDown();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.optionsTab = new System.Windows.Forms.TabPage();
            this.gbOutput = new System.Windows.Forms.GroupBox();
            this.videoOutput = new MeGUI.FileBar();
            this.label7 = new System.Windows.Forms.Label();
            this.videoGroupBox = new System.Windows.Forms.GroupBox();
            this.input = new MeGUI.FileBar();
            this.avsProfile = new MeGUI.core.gui.ConfigableProfilesControl();
            this.arChooser = new MeGUI.core.gui.ARChooser();
            this.reopenOriginal = new System.Windows.Forms.Button();
            this.mod16Box = new System.Windows.Forms.ComboBox();
            this.signalAR = new System.Windows.Forms.CheckBox();
            this.tvTypeLabel = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.inputDARLabel = new System.Windows.Forms.Label();
            this.videoInputLabel = new System.Windows.Forms.Label();
            this.filterTab = new System.Windows.Forms.TabPage();
            this.tabSources = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.mpegOptGroupBox = new System.Windows.Forms.GroupBox();
            this.colourCorrect = new System.Windows.Forms.CheckBox();
            this.mpeg2Deblocking = new System.Windows.Forms.CheckBox();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.aviOptGroupBox = new System.Windows.Forms.GroupBox();
            this.dss2 = new System.Windows.Forms.CheckBox();
            this.fpsBox = new System.Windows.Forms.NumericUpDown();
            this.fpsLabel = new System.Windows.Forms.Label();
            this.flipVertical = new System.Windows.Forms.CheckBox();
            this.tabPage3 = new System.Windows.Forms.TabPage();
            this.dgOptions = new System.Windows.Forms.GroupBox();
            this.nvResize = new System.Windows.Forms.CheckBox();
            this.cbNvDeInt = new System.Windows.Forms.ComboBox();
            this.nvDeInt = new System.Windows.Forms.CheckBox();
            this.deinterlacingGroupBox = new System.Windows.Forms.GroupBox();
            this.deintM = new System.Windows.Forms.NumericUpDown();
            this.deintFieldOrder = new System.Windows.Forms.ComboBox();
            this.deintSourceType = new System.Windows.Forms.ComboBox();
            this.deintIsAnime = new System.Windows.Forms.CheckBox();
            this.analyseButton = new System.Windows.Forms.Button();
            this.deinterlace = new System.Windows.Forms.CheckBox();
            this.deinterlaceType = new System.Windows.Forms.ComboBox();
            this.filtersGroupbox = new System.Windows.Forms.GroupBox();
            this.cbCharset = new System.Windows.Forms.ComboBox();
            this.label8 = new System.Windows.Forms.Label();
            this.openSubtitlesButton = new System.Windows.Forms.Button();
            this.SubtitlesPath = new System.Windows.Forms.TextBox();
            this.SubtitlesLabel = new System.Windows.Forms.Label();
            this.noiseFilterType = new System.Windows.Forms.ComboBox();
            this.noiseFilter = new System.Windows.Forms.CheckBox();
            this.resizeFilterType = new System.Windows.Forms.ComboBox();
            this.resizeFilterLabel = new System.Windows.Forms.Label();
            this.editTab = new System.Windows.Forms.TabPage();
            this.openDLLButton = new System.Windows.Forms.Button();
            this.dllPath = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.avisynthScript = new System.Windows.Forms.TextBox();
            this.saveAvisynthScriptDialog = new System.Windows.Forms.SaveFileDialog();
            this.openFilterDialog = new System.Windows.Forms.OpenFileDialog();
            this.openSubsDialog = new System.Windows.Forms.OpenFileDialog();
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.deintProgressBar = new System.Windows.Forms.ToolStripProgressBar();
            this.deintStatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.onSaveLoadScript = new System.Windows.Forms.CheckBox();
            this.saveButton = new System.Windows.Forms.Button();
            this.previewAvsButton = new System.Windows.Forms.Button();
            this.helpButton1 = new MeGUI.core.gui.HelpButton();
            label2 = new System.Windows.Forms.Label();
            label3 = new System.Windows.Forms.Label();
            label4 = new System.Windows.Forms.Label();
            label5 = new System.Windows.Forms.Label();
            this.resNCropGroupbox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.cropLeft)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.cropRight)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.cropBottom)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.cropTop)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.verticalResolution)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.horizontalResolution)).BeginInit();
            this.tabControl1.SuspendLayout();
            this.optionsTab.SuspendLayout();
            this.gbOutput.SuspendLayout();
            this.videoGroupBox.SuspendLayout();
            this.filterTab.SuspendLayout();
            this.tabSources.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.mpegOptGroupBox.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.aviOptGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.fpsBox)).BeginInit();
            this.tabPage3.SuspendLayout();
            this.dgOptions.SuspendLayout();
            this.deinterlacingGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.deintM)).BeginInit();
            this.filtersGroupbox.SuspendLayout();
            this.editTab.SuspendLayout();
            this.statusStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // label2
            // 
            label2.AutoSize = true;
            label2.Location = new System.Drawing.Point(9, 22);
            label2.Name = "label2";
            label2.Size = new System.Drawing.Size(258, 13);
            label2.TabIndex = 11;
            label2.Text = "Source Info (Click on \'Analyse...\' for autodetection):";
            // 
            // label3
            // 
            label3.AutoSize = true;
            label3.Location = new System.Drawing.Point(9, 50);
            label3.Name = "label3";
            label3.Size = new System.Drawing.Size(69, 13);
            label3.TabIndex = 13;
            label3.Text = "Source type:";
            // 
            // label4
            // 
            label4.AutoSize = true;
            label4.Location = new System.Drawing.Point(9, 79);
            label4.Name = "label4";
            label4.Size = new System.Drawing.Size(62, 13);
            label4.TabIndex = 14;
            label4.Text = "Field order:";
            // 
            // label5
            // 
            label5.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            label5.AutoSize = true;
            label5.Location = new System.Drawing.Point(342, 50);
            label5.Name = "label5";
            label5.Size = new System.Drawing.Size(19, 13);
            label5.TabIndex = 17;
            label5.Text = "M:";
            // 
            // resNCropGroupbox
            // 
            this.resNCropGroupbox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.resNCropGroupbox.Controls.Add(this.chAutoPreview);
            this.resNCropGroupbox.Controls.Add(this.resize);
            this.resNCropGroupbox.Controls.Add(this.suggestResolution);
            this.resNCropGroupbox.Controls.Add(this.cropLeft);
            this.resNCropGroupbox.Controls.Add(this.cropRight);
            this.resNCropGroupbox.Controls.Add(this.cropBottom);
            this.resNCropGroupbox.Controls.Add(this.cropTop);
            this.resNCropGroupbox.Controls.Add(this.autoCropButton);
            this.resNCropGroupbox.Controls.Add(this.crop);
            this.resNCropGroupbox.Controls.Add(this.verticalResolution);
            this.resNCropGroupbox.Controls.Add(this.horizontalResolution);
            this.resNCropGroupbox.Enabled = false;
            this.resNCropGroupbox.Location = new System.Drawing.Point(3, 196);
            this.resNCropGroupbox.Name = "resNCropGroupbox";
            this.resNCropGroupbox.Size = new System.Drawing.Size(450, 158);
            this.resNCropGroupbox.TabIndex = 0;
            this.resNCropGroupbox.TabStop = false;
            this.resNCropGroupbox.Text = "Crop && Resize";
            // 
            // chAutoPreview
            // 
            this.chAutoPreview.AutoSize = true;
            this.chAutoPreview.Location = new System.Drawing.Point(11, 124);
            this.chAutoPreview.Name = "chAutoPreview";
            this.chAutoPreview.Size = new System.Drawing.Size(119, 17);
            this.chAutoPreview.TabIndex = 10;
            this.chAutoPreview.Text = "Apply auto Preview";
            this.chAutoPreview.UseVisualStyleBackColor = true;
            this.chAutoPreview.CheckedChanged += new System.EventHandler(this.chAutoPreview_CheckedChanged);
            // 
            // resize
            // 
            this.resize.AutoSize = true;
            this.resize.Location = new System.Drawing.Point(11, 80);
            this.resize.Name = "resize";
            this.resize.Size = new System.Drawing.Size(57, 17);
            this.resize.TabIndex = 9;
            this.resize.Text = "Resize";
            this.resize.UseVisualStyleBackColor = true;
            this.resize.CheckedChanged += new System.EventHandler(this.resize_CheckedChanged);
            // 
            // suggestResolution
            // 
            this.suggestResolution.Location = new System.Drawing.Point(238, 80);
            this.suggestResolution.Name = "suggestResolution";
            this.suggestResolution.Size = new System.Drawing.Size(168, 17);
            this.suggestResolution.TabIndex = 8;
            this.suggestResolution.Text = "Suggest Resolution (mod16)";
            this.suggestResolution.CheckedChanged += new System.EventHandler(this.suggestResolution_CheckedChanged);
            // 
            // cropLeft
            // 
            this.cropLeft.Enabled = false;
            this.cropLeft.Increment = new decimal(new int[] {
            2,
            0,
            0,
            0});
            this.cropLeft.Location = new System.Drawing.Point(114, 35);
            this.cropLeft.Maximum = new decimal(new int[] {
            200,
            0,
            0,
            0});
            this.cropLeft.Name = "cropLeft";
            this.cropLeft.Size = new System.Drawing.Size(48, 21);
            this.cropLeft.TabIndex = 7;
            this.cropLeft.ValueChanged += new System.EventHandler(this.crop_CheckedChanged);
            // 
            // cropRight
            // 
            this.cropRight.Enabled = false;
            this.cropRight.Increment = new decimal(new int[] {
            2,
            0,
            0,
            0});
            this.cropRight.Location = new System.Drawing.Point(222, 35);
            this.cropRight.Maximum = new decimal(new int[] {
            200,
            0,
            0,
            0});
            this.cropRight.Name = "cropRight";
            this.cropRight.Size = new System.Drawing.Size(48, 21);
            this.cropRight.TabIndex = 6;
            this.cropRight.ValueChanged += new System.EventHandler(this.crop_CheckedChanged);
            // 
            // cropBottom
            // 
            this.cropBottom.Enabled = false;
            this.cropBottom.Increment = new decimal(new int[] {
            2,
            0,
            0,
            0});
            this.cropBottom.Location = new System.Drawing.Point(168, 49);
            this.cropBottom.Maximum = new decimal(new int[] {
            200,
            0,
            0,
            0});
            this.cropBottom.Name = "cropBottom";
            this.cropBottom.Size = new System.Drawing.Size(48, 21);
            this.cropBottom.TabIndex = 5;
            this.cropBottom.ValueChanged += new System.EventHandler(this.crop_CheckedChanged);
            // 
            // cropTop
            // 
            this.cropTop.Enabled = false;
            this.cropTop.Increment = new decimal(new int[] {
            2,
            0,
            0,
            0});
            this.cropTop.Location = new System.Drawing.Point(168, 22);
            this.cropTop.Maximum = new decimal(new int[] {
            200,
            0,
            0,
            0});
            this.cropTop.Name = "cropTop";
            this.cropTop.Size = new System.Drawing.Size(48, 21);
            this.cropTop.TabIndex = 4;
            this.cropTop.ValueChanged += new System.EventHandler(this.crop_CheckedChanged);
            // 
            // autoCropButton
            // 
            this.autoCropButton.Location = new System.Drawing.Point(331, 34);
            this.autoCropButton.Name = "autoCropButton";
            this.autoCropButton.Size = new System.Drawing.Size(75, 23);
            this.autoCropButton.TabIndex = 3;
            this.autoCropButton.Text = "Auto Crop";
            this.autoCropButton.Click += new System.EventHandler(this.autoCropButton_Click);
            // 
            // crop
            // 
            this.crop.Location = new System.Drawing.Point(11, 28);
            this.crop.Name = "crop";
            this.crop.Size = new System.Drawing.Size(97, 42);
            this.crop.TabIndex = 2;
            this.crop.Text = "Crop";
            this.crop.CheckedChanged += new System.EventHandler(this.crop_CheckedChanged);
            // 
            // verticalResolution
            // 
            this.verticalResolution.Enabled = false;
            this.verticalResolution.Increment = new decimal(new int[] {
            16,
            0,
            0,
            0});
            this.verticalResolution.Location = new System.Drawing.Point(168, 79);
            this.verticalResolution.Maximum = new decimal(new int[] {
            1200,
            0,
            0,
            0});
            this.verticalResolution.Name = "verticalResolution";
            this.verticalResolution.Size = new System.Drawing.Size(48, 21);
            this.verticalResolution.TabIndex = 1;
            this.verticalResolution.Value = new decimal(new int[] {
            272,
            0,
            0,
            0});
            this.verticalResolution.ValueChanged += new System.EventHandler(this.verticalResolution_ValueChanged);
            // 
            // horizontalResolution
            // 
            this.horizontalResolution.Enabled = false;
            this.horizontalResolution.Increment = new decimal(new int[] {
            16,
            0,
            0,
            0});
            this.horizontalResolution.Location = new System.Drawing.Point(114, 79);
            this.horizontalResolution.Maximum = new decimal(new int[] {
            1920,
            0,
            0,
            0});
            this.horizontalResolution.Name = "horizontalResolution";
            this.horizontalResolution.Size = new System.Drawing.Size(48, 21);
            this.horizontalResolution.TabIndex = 0;
            this.horizontalResolution.Value = new decimal(new int[] {
            640,
            0,
            0,
            0});
            this.horizontalResolution.ValueChanged += new System.EventHandler(this.horizontalResolution_ValueChanged);
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.optionsTab);
            this.tabControl1.Controls.Add(this.filterTab);
            this.tabControl1.Controls.Add(this.editTab);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Top;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(463, 459);
            this.tabControl1.TabIndex = 5;
            // 
            // optionsTab
            // 
            this.optionsTab.Controls.Add(this.gbOutput);
            this.optionsTab.Controls.Add(this.videoGroupBox);
            this.optionsTab.Controls.Add(this.resNCropGroupbox);
            this.optionsTab.Location = new System.Drawing.Point(4, 22);
            this.optionsTab.Name = "optionsTab";
            this.optionsTab.Size = new System.Drawing.Size(455, 433);
            this.optionsTab.TabIndex = 0;
            this.optionsTab.Text = "I/O";
            this.optionsTab.UseVisualStyleBackColor = true;
            // 
            // gbOutput
            // 
            this.gbOutput.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.gbOutput.Controls.Add(this.videoOutput);
            this.gbOutput.Controls.Add(this.label7);
            this.gbOutput.Location = new System.Drawing.Point(3, 360);
            this.gbOutput.Name = "gbOutput";
            this.gbOutput.Size = new System.Drawing.Size(450, 55);
            this.gbOutput.TabIndex = 13;
            this.gbOutput.TabStop = false;
            this.gbOutput.Text = "Output";
            // 
            // videoOutput
            // 
            this.videoOutput.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.videoOutput.Filename = "";
            this.videoOutput.Filter = "AVI Synth Scripts|*.avs";
            this.videoOutput.FilterIndex = 0;
            this.videoOutput.FolderMode = false;
            this.videoOutput.Location = new System.Drawing.Point(96, 17);
            this.videoOutput.Name = "videoOutput";
            this.videoOutput.ReadOnly = true;
            this.videoOutput.SaveMode = true;
            this.videoOutput.Size = new System.Drawing.Size(348, 26);
            this.videoOutput.TabIndex = 7;
            this.videoOutput.Title = null;
            // 
            // label7
            // 
            this.label7.Location = new System.Drawing.Point(8, 24);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(80, 13);
            this.label7.TabIndex = 6;
            this.label7.Text = "Video Output";
            // 
            // videoGroupBox
            // 
            this.videoGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.videoGroupBox.Controls.Add(this.input);
            this.videoGroupBox.Controls.Add(this.avsProfile);
            this.videoGroupBox.Controls.Add(this.arChooser);
            this.videoGroupBox.Controls.Add(this.reopenOriginal);
            this.videoGroupBox.Controls.Add(this.mod16Box);
            this.videoGroupBox.Controls.Add(this.signalAR);
            this.videoGroupBox.Controls.Add(this.tvTypeLabel);
            this.videoGroupBox.Controls.Add(this.label6);
            this.videoGroupBox.Controls.Add(this.inputDARLabel);
            this.videoGroupBox.Controls.Add(this.videoInputLabel);
            this.videoGroupBox.Location = new System.Drawing.Point(3, 8);
            this.videoGroupBox.Name = "videoGroupBox";
            this.videoGroupBox.Size = new System.Drawing.Size(450, 182);
            this.videoGroupBox.TabIndex = 5;
            this.videoGroupBox.TabStop = false;
            this.videoGroupBox.Text = "Input";
            // 
            // input
            // 
            this.input.AllowDrop = true;
            this.input.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.input.Filename = "";
            this.input.Filter = resources.GetString("input.Filter");
            this.input.FilterIndex = 6;
            this.input.FolderMode = false;
            this.input.Location = new System.Drawing.Point(77, 15);
            this.input.Name = "input";
            this.input.ReadOnly = true;
            this.input.SaveMode = false;
            this.input.Size = new System.Drawing.Size(367, 26);
            this.input.TabIndex = 1;
            this.input.Title = "Select a source file";
            this.input.FileSelected += new MeGUI.FileBarEventHandler(this.input_FileSelected);
            // 
            // avsProfile
            // 
            this.avsProfile.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.avsProfile.Location = new System.Drawing.Point(96, 147);
            this.avsProfile.Name = "avsProfile";
            this.avsProfile.ProfileSet = "AviSynth";
            this.avsProfile.Size = new System.Drawing.Size(348, 22);
            this.avsProfile.TabIndex = 22;
            this.avsProfile.SelectedProfileChanged += new System.EventHandler(this.ProfileChanged);
            // 
            // arChooser
            // 
            this.arChooser.CustomDARs = new MeGUI.core.util.Dar[0];
            this.arChooser.HasLater = false;
            this.arChooser.Location = new System.Drawing.Point(96, 76);
            this.arChooser.MaximumSize = new System.Drawing.Size(1000, 29);
            this.arChooser.MinimumSize = new System.Drawing.Size(64, 29);
            this.arChooser.Name = "arChooser";
            this.arChooser.SelectedIndex = 0;
            this.arChooser.Size = new System.Drawing.Size(214, 29);
            this.arChooser.TabIndex = 21;
            this.arChooser.SelectionChanged += new MeGUI.StringChanged(this.arChooser_SelectionChanged);
            // 
            // reopenOriginal
            // 
            this.reopenOriginal.AutoSize = true;
            this.reopenOriginal.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.reopenOriginal.Location = new System.Drawing.Point(77, 47);
            this.reopenOriginal.Name = "reopenOriginal";
            this.reopenOriginal.Size = new System.Drawing.Size(157, 23);
            this.reopenOriginal.TabIndex = 20;
            this.reopenOriginal.Text = "Re-open original video player";
            this.reopenOriginal.UseVisualStyleBackColor = true;
            this.reopenOriginal.Click += new System.EventHandler(this.reopenOriginal_Click);
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
            this.mod16Box.Location = new System.Drawing.Point(249, 119);
            this.mod16Box.Name = "mod16Box";
            this.mod16Box.Size = new System.Drawing.Size(157, 21);
            this.mod16Box.TabIndex = 19;
            this.mod16Box.SelectedIndexChanged += new System.EventHandler(this.mod16Box_SelectedIndexChanged);
            // 
            // signalAR
            // 
            this.signalAR.AutoSize = true;
            this.signalAR.Location = new System.Drawing.Point(11, 121);
            this.signalAR.Name = "signalAR";
            this.signalAR.Size = new System.Drawing.Size(190, 17);
            this.signalAR.TabIndex = 11;
            this.signalAR.Text = "Clever (TM) anamorphic encoding:";
            this.signalAR.CheckedChanged += new System.EventHandler(this.signalAR_Checkedchanged);
            // 
            // tvTypeLabel
            // 
            this.tvTypeLabel.Location = new System.Drawing.Point(316, 82);
            this.tvTypeLabel.Name = "tvTypeLabel";
            this.tvTypeLabel.Size = new System.Drawing.Size(48, 23);
            this.tvTypeLabel.TabIndex = 10;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(8, 152);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(82, 13);
            this.label6.TabIndex = 7;
            this.label6.Text = "Avisynth profile";
            // 
            // inputDARLabel
            // 
            this.inputDARLabel.Location = new System.Drawing.Point(8, 83);
            this.inputDARLabel.Name = "inputDARLabel";
            this.inputDARLabel.Size = new System.Drawing.Size(72, 13);
            this.inputDARLabel.TabIndex = 7;
            this.inputDARLabel.Text = "Input DAR";
            // 
            // videoInputLabel
            // 
            this.videoInputLabel.Location = new System.Drawing.Point(8, 24);
            this.videoInputLabel.Name = "videoInputLabel";
            this.videoInputLabel.Size = new System.Drawing.Size(80, 13);
            this.videoInputLabel.TabIndex = 0;
            this.videoInputLabel.Text = "Video Input";
            // 
            // filterTab
            // 
            this.filterTab.Controls.Add(this.tabSources);
            this.filterTab.Controls.Add(this.deinterlacingGroupBox);
            this.filterTab.Controls.Add(this.filtersGroupbox);
            this.filterTab.Location = new System.Drawing.Point(4, 22);
            this.filterTab.Name = "filterTab";
            this.filterTab.Size = new System.Drawing.Size(455, 433);
            this.filterTab.TabIndex = 2;
            this.filterTab.Text = "Filters";
            this.filterTab.UseVisualStyleBackColor = true;
            // 
            // tabSources
            // 
            this.tabSources.Controls.Add(this.tabPage1);
            this.tabSources.Controls.Add(this.tabPage2);
            this.tabSources.Controls.Add(this.tabPage3);
            this.tabSources.Location = new System.Drawing.Point(3, 3);
            this.tabSources.Name = "tabSources";
            this.tabSources.SelectedIndex = 0;
            this.tabSources.Size = new System.Drawing.Size(449, 116);
            this.tabSources.TabIndex = 14;
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.mpegOptGroupBox);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(441, 90);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "MPEG2 Source";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // mpegOptGroupBox
            // 
            this.mpegOptGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.mpegOptGroupBox.Controls.Add(this.colourCorrect);
            this.mpegOptGroupBox.Controls.Add(this.mpeg2Deblocking);
            this.mpegOptGroupBox.Enabled = false;
            this.mpegOptGroupBox.Location = new System.Drawing.Point(6, 3);
            this.mpegOptGroupBox.Name = "mpegOptGroupBox";
            this.mpegOptGroupBox.Size = new System.Drawing.Size(426, 80);
            this.mpegOptGroupBox.TabIndex = 22;
            this.mpegOptGroupBox.TabStop = false;
            // 
            // colourCorrect
            // 
            this.colourCorrect.Location = new System.Drawing.Point(10, 43);
            this.colourCorrect.Name = "colourCorrect";
            this.colourCorrect.Size = new System.Drawing.Size(111, 17);
            this.colourCorrect.TabIndex = 9;
            this.colourCorrect.Text = "Colour Correction";
            this.colourCorrect.CheckedChanged += new System.EventHandler(this.checkedChanged);
            // 
            // mpeg2Deblocking
            // 
            this.mpeg2Deblocking.Location = new System.Drawing.Point(10, 20);
            this.mpeg2Deblocking.Name = "mpeg2Deblocking";
            this.mpeg2Deblocking.Size = new System.Drawing.Size(124, 17);
            this.mpeg2Deblocking.TabIndex = 8;
            this.mpeg2Deblocking.Text = "Mpeg2 Deblocking";
            this.mpeg2Deblocking.CheckedChanged += new System.EventHandler(this.checkedChanged);
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.aviOptGroupBox);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(441, 90);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "AVI Source / DSSource";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // aviOptGroupBox
            // 
            this.aviOptGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.aviOptGroupBox.Controls.Add(this.dss2);
            this.aviOptGroupBox.Controls.Add(this.fpsBox);
            this.aviOptGroupBox.Controls.Add(this.fpsLabel);
            this.aviOptGroupBox.Controls.Add(this.flipVertical);
            this.aviOptGroupBox.Enabled = false;
            this.aviOptGroupBox.Location = new System.Drawing.Point(6, 3);
            this.aviOptGroupBox.Name = "aviOptGroupBox";
            this.aviOptGroupBox.Size = new System.Drawing.Size(426, 80);
            this.aviOptGroupBox.TabIndex = 23;
            this.aviOptGroupBox.TabStop = false;
            // 
            // dss2
            // 
            this.dss2.AutoSize = true;
            this.dss2.Location = new System.Drawing.Point(120, 20);
            this.dss2.Name = "dss2";
            this.dss2.Size = new System.Drawing.Size(185, 17);
            this.dss2.TabIndex = 4;
            this.dss2.Text = "Prefer DSSource2 over DSSource";
            this.dss2.UseVisualStyleBackColor = true;
            this.dss2.CheckedChanged += new System.EventHandler(this.checkedChanged);
            // 
            // fpsBox
            // 
            this.fpsBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.fpsBox.DecimalPlaces = 3;
            this.fpsBox.Location = new System.Drawing.Point(40, 43);
            this.fpsBox.Maximum = new decimal(new int[] {
            1000,
            0,
            0,
            0});
            this.fpsBox.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            196608});
            this.fpsBox.Name = "fpsBox";
            this.fpsBox.Size = new System.Drawing.Size(130, 21);
            this.fpsBox.TabIndex = 3;
            this.fpsBox.Value = new decimal(new int[] {
            1,
            0,
            0,
            196608});
            this.fpsBox.ValueChanged += new System.EventHandler(this.checkedChanged);
            // 
            // fpsLabel
            // 
            this.fpsLabel.Location = new System.Drawing.Point(9, 45);
            this.fpsLabel.Name = "fpsLabel";
            this.fpsLabel.Size = new System.Drawing.Size(25, 13);
            this.fpsLabel.TabIndex = 2;
            this.fpsLabel.Text = "FPS";
            // 
            // flipVertical
            // 
            this.flipVertical.Location = new System.Drawing.Point(10, 20);
            this.flipVertical.Name = "flipVertical";
            this.flipVertical.Size = new System.Drawing.Size(90, 17);
            this.flipVertical.TabIndex = 0;
            this.flipVertical.Text = "Vertical Flip";
            this.flipVertical.CheckedChanged += new System.EventHandler(this.checkedChanged);
            // 
            // tabPage3
            // 
            this.tabPage3.Controls.Add(this.dgOptions);
            this.tabPage3.Location = new System.Drawing.Point(4, 22);
            this.tabPage3.Name = "tabPage3";
            this.tabPage3.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage3.Size = new System.Drawing.Size(441, 90);
            this.tabPage3.TabIndex = 2;
            this.tabPage3.Text = "DGx Source";
            this.tabPage3.UseVisualStyleBackColor = true;
            // 
            // dgOptions
            // 
            this.dgOptions.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.dgOptions.Controls.Add(this.nvResize);
            this.dgOptions.Controls.Add(this.cbNvDeInt);
            this.dgOptions.Controls.Add(this.nvDeInt);
            this.dgOptions.Location = new System.Drawing.Point(6, 3);
            this.dgOptions.Name = "dgOptions";
            this.dgOptions.Size = new System.Drawing.Size(426, 80);
            this.dgOptions.TabIndex = 14;
            this.dgOptions.TabStop = false;
            // 
            // nvResize
            // 
            this.nvResize.AutoSize = true;
            this.nvResize.Location = new System.Drawing.Point(10, 52);
            this.nvResize.Name = "nvResize";
            this.nvResize.Size = new System.Drawing.Size(93, 17);
            this.nvResize.TabIndex = 2;
            this.nvResize.Text = "Nvidia Resizer";
            this.nvResize.UseVisualStyleBackColor = true;
            this.nvResize.Click += new System.EventHandler(this.nvDeInt_Click);
            this.nvResize.CheckedChanged += new System.EventHandler(this.nvResize_CheckedChanged);
            // 
            // cbNvDeInt
            // 
            this.cbNvDeInt.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.cbNvDeInt.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbNvDeInt.Enabled = false;
            this.cbNvDeInt.FormattingEnabled = true;
            this.cbNvDeInt.Location = new System.Drawing.Point(155, 18);
            this.cbNvDeInt.Name = "cbNvDeInt";
            this.cbNvDeInt.Size = new System.Drawing.Size(265, 21);
            this.cbNvDeInt.TabIndex = 1;
            // 
            // nvDeInt
            // 
            this.nvDeInt.AutoSize = true;
            this.nvDeInt.Enabled = false;
            this.nvDeInt.Location = new System.Drawing.Point(10, 20);
            this.nvDeInt.Name = "nvDeInt";
            this.nvDeInt.Size = new System.Drawing.Size(116, 17);
            this.nvDeInt.TabIndex = 0;
            this.nvDeInt.Text = "Nvidia Deinterlacer";
            this.nvDeInt.UseVisualStyleBackColor = true;
            this.nvDeInt.Click += new System.EventHandler(this.nvDeInt_Click);
            this.nvDeInt.CheckedChanged += new System.EventHandler(this.nvDeInt_CheckedChanged);
            // 
            // deinterlacingGroupBox
            // 
            this.deinterlacingGroupBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.deinterlacingGroupBox.Controls.Add(label5);
            this.deinterlacingGroupBox.Controls.Add(this.deintM);
            this.deinterlacingGroupBox.Controls.Add(this.deintFieldOrder);
            this.deinterlacingGroupBox.Controls.Add(label4);
            this.deinterlacingGroupBox.Controls.Add(label3);
            this.deinterlacingGroupBox.Controls.Add(this.deintSourceType);
            this.deinterlacingGroupBox.Controls.Add(label2);
            this.deinterlacingGroupBox.Controls.Add(this.deintIsAnime);
            this.deinterlacingGroupBox.Controls.Add(this.analyseButton);
            this.deinterlacingGroupBox.Controls.Add(this.deinterlace);
            this.deinterlacingGroupBox.Controls.Add(this.deinterlaceType);
            this.deinterlacingGroupBox.Enabled = false;
            this.deinterlacingGroupBox.Location = new System.Drawing.Point(3, 121);
            this.deinterlacingGroupBox.Name = "deinterlacingGroupBox";
            this.deinterlacingGroupBox.Size = new System.Drawing.Size(449, 164);
            this.deinterlacingGroupBox.TabIndex = 12;
            this.deinterlacingGroupBox.TabStop = false;
            this.deinterlacingGroupBox.Text = "Deinterlacing";
            // 
            // deintM
            // 
            this.deintM.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.deintM.Location = new System.Drawing.Point(367, 47);
            this.deintM.Name = "deintM";
            this.deintM.Size = new System.Drawing.Size(76, 21);
            this.deintM.TabIndex = 16;
            this.deintM.ValueChanged += new System.EventHandler(this.deintSourceType_SelectedIndexChanged);
            // 
            // deintFieldOrder
            // 
            this.deintFieldOrder.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.deintFieldOrder.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.deintFieldOrder.FormattingEnabled = true;
            this.deintFieldOrder.Location = new System.Drawing.Point(97, 76);
            this.deintFieldOrder.Name = "deintFieldOrder";
            this.deintFieldOrder.Size = new System.Drawing.Size(239, 21);
            this.deintFieldOrder.TabIndex = 15;
            this.deintFieldOrder.SelectedIndexChanged += new System.EventHandler(this.deintSourceType_SelectedIndexChanged);
            // 
            // deintSourceType
            // 
            this.deintSourceType.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.deintSourceType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.deintSourceType.FormattingEnabled = true;
            this.deintSourceType.Location = new System.Drawing.Point(97, 47);
            this.deintSourceType.Name = "deintSourceType";
            this.deintSourceType.Size = new System.Drawing.Size(239, 21);
            this.deintSourceType.TabIndex = 12;
            this.deintSourceType.SelectedIndexChanged += new System.EventHandler(this.deintSourceType_SelectedIndexChanged);
            // 
            // deintIsAnime
            // 
            this.deintIsAnime.AutoSize = true;
            this.deintIsAnime.Location = new System.Drawing.Point(9, 133);
            this.deintIsAnime.Name = "deintIsAnime";
            this.deintIsAnime.Size = new System.Drawing.Size(297, 17);
            this.deintIsAnime.TabIndex = 10;
            this.deintIsAnime.Text = "Source is Anime (not automatically detected by Analysis)";
            this.deintIsAnime.UseVisualStyleBackColor = true;
            this.deintIsAnime.CheckedChanged += new System.EventHandler(this.deintSourceType_SelectedIndexChanged);
            // 
            // analyseButton
            // 
            this.analyseButton.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.analyseButton.Location = new System.Drawing.Point(345, 17);
            this.analyseButton.Name = "analyseButton";
            this.analyseButton.Size = new System.Drawing.Size(98, 23);
            this.analyseButton.TabIndex = 8;
            this.analyseButton.Text = "Analyse...";
            this.analyseButton.UseVisualStyleBackColor = true;
            this.analyseButton.Click += new System.EventHandler(this.analyseButton_Click);
            // 
            // deinterlace
            // 
            this.deinterlace.Location = new System.Drawing.Point(9, 103);
            this.deinterlace.Name = "deinterlace";
            this.deinterlace.Size = new System.Drawing.Size(82, 24);
            this.deinterlace.TabIndex = 2;
            this.deinterlace.Text = "Deinterlace";
            this.deinterlace.CheckedChanged += new System.EventHandler(this.deinterlace_CheckedChanged);
            // 
            // deinterlaceType
            // 
            this.deinterlaceType.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.deinterlaceType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.deinterlaceType.Enabled = false;
            this.deinterlaceType.Items.AddRange(new object[] {
            "Leak Kernel Deinterlace",
            "Field Deinterlace",
            "Field Deinterlace (no blend)",
            "Telecide for PAL"});
            this.deinterlaceType.Location = new System.Drawing.Point(97, 105);
            this.deinterlaceType.Name = "deinterlaceType";
            this.deinterlaceType.Size = new System.Drawing.Size(239, 21);
            this.deinterlaceType.TabIndex = 4;
            this.deinterlaceType.SelectedIndexChanged += new System.EventHandler(this.deinterlaceType_SelectedIndexChanged);
            // 
            // filtersGroupbox
            // 
            this.filtersGroupbox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.filtersGroupbox.Controls.Add(this.cbCharset);
            this.filtersGroupbox.Controls.Add(this.label8);
            this.filtersGroupbox.Controls.Add(this.openSubtitlesButton);
            this.filtersGroupbox.Controls.Add(this.SubtitlesPath);
            this.filtersGroupbox.Controls.Add(this.SubtitlesLabel);
            this.filtersGroupbox.Controls.Add(this.noiseFilterType);
            this.filtersGroupbox.Controls.Add(this.noiseFilter);
            this.filtersGroupbox.Controls.Add(this.resizeFilterType);
            this.filtersGroupbox.Controls.Add(this.resizeFilterLabel);
            this.filtersGroupbox.Enabled = false;
            this.filtersGroupbox.Location = new System.Drawing.Point(3, 291);
            this.filtersGroupbox.Name = "filtersGroupbox";
            this.filtersGroupbox.Size = new System.Drawing.Size(449, 139);
            this.filtersGroupbox.TabIndex = 9;
            this.filtersGroupbox.TabStop = false;
            this.filtersGroupbox.Text = "Filters";
            // 
            // cbCharset
            // 
            this.cbCharset.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbCharset.FormattingEnabled = true;
            this.cbCharset.Items.AddRange(new object[] {
            "Default",
            "ANSI",
            "Symbol",
            "Shiftjis",
            "Hangeul",
            "Hangul",
            "GB2312",
            "Chinese Big 5",
            "OEM",
            "Johab",
            "Hebrew",
            "Arabic",
            "Greek",
            "Turkish",
            "Vietnamese",
            "Thai",
            "East Europe",
            "Russian",
            "Mac",
            "Baltic"});
            this.cbCharset.Location = new System.Drawing.Point(151, 108);
            this.cbCharset.Name = "cbCharset";
            this.cbCharset.Size = new System.Drawing.Size(121, 21);
            this.cbCharset.TabIndex = 11;
            this.cbCharset.SelectedIndexChanged += new System.EventHandler(this.cbCharset_SelectedIndexChanged);
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(96, 111);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(49, 13);
            this.label8.TabIndex = 10;
            this.label8.Text = "Charset:";
            // 
            // openSubtitlesButton
            // 
            this.openSubtitlesButton.Location = new System.Drawing.Point(416, 80);
            this.openSubtitlesButton.Name = "openSubtitlesButton";
            this.openSubtitlesButton.Size = new System.Drawing.Size(27, 22);
            this.openSubtitlesButton.TabIndex = 9;
            this.openSubtitlesButton.Text = "...";
            this.openSubtitlesButton.UseVisualStyleBackColor = true;
            this.openSubtitlesButton.Click += new System.EventHandler(this.openSubtitlesButton_Click);
            // 
            // SubtitlesPath
            // 
            this.SubtitlesPath.BackColor = System.Drawing.SystemColors.Control;
            this.SubtitlesPath.Location = new System.Drawing.Point(97, 80);
            this.SubtitlesPath.Name = "SubtitlesPath";
            this.SubtitlesPath.ReadOnly = true;
            this.SubtitlesPath.Size = new System.Drawing.Size(313, 21);
            this.SubtitlesPath.TabIndex = 8;
            // 
            // SubtitlesLabel
            // 
            this.SubtitlesLabel.AutoSize = true;
            this.SubtitlesLabel.Location = new System.Drawing.Point(9, 83);
            this.SubtitlesLabel.Name = "SubtitlesLabel";
            this.SubtitlesLabel.Size = new System.Drawing.Size(52, 13);
            this.SubtitlesLabel.TabIndex = 7;
            this.SubtitlesLabel.Text = "Subtitles:";
            // 
            // noiseFilterType
            // 
            this.noiseFilterType.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.noiseFilterType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.noiseFilterType.Enabled = false;
            this.noiseFilterType.Location = new System.Drawing.Point(97, 44);
            this.noiseFilterType.Name = "noiseFilterType";
            this.noiseFilterType.Size = new System.Drawing.Size(239, 21);
            this.noiseFilterType.TabIndex = 5;
            this.noiseFilterType.SelectedIndexChanged += new System.EventHandler(this.noiseFilterType_SelectedIndexChanged);
            // 
            // noiseFilter
            // 
            this.noiseFilter.Location = new System.Drawing.Point(9, 44);
            this.noiseFilter.Name = "noiseFilter";
            this.noiseFilter.Size = new System.Drawing.Size(104, 24);
            this.noiseFilter.TabIndex = 3;
            this.noiseFilter.Text = "Noise Filter";
            this.noiseFilter.CheckedChanged += new System.EventHandler(this.noiseFilter_CheckedChanged);
            // 
            // resizeFilterType
            // 
            this.resizeFilterType.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.resizeFilterType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.resizeFilterType.Location = new System.Drawing.Point(97, 17);
            this.resizeFilterType.Name = "resizeFilterType";
            this.resizeFilterType.Size = new System.Drawing.Size(239, 21);
            this.resizeFilterType.TabIndex = 1;
            this.resizeFilterType.SelectedIndexChanged += new System.EventHandler(this.resizeFilterType_SelectedIndexChanged);
            // 
            // resizeFilterLabel
            // 
            this.resizeFilterLabel.Location = new System.Drawing.Point(9, 17);
            this.resizeFilterLabel.Name = "resizeFilterLabel";
            this.resizeFilterLabel.Size = new System.Drawing.Size(100, 23);
            this.resizeFilterLabel.TabIndex = 0;
            this.resizeFilterLabel.Text = "Resize Filter";
            this.resizeFilterLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // editTab
            // 
            this.editTab.Controls.Add(this.openDLLButton);
            this.editTab.Controls.Add(this.dllPath);
            this.editTab.Controls.Add(this.label1);
            this.editTab.Controls.Add(this.avisynthScript);
            this.editTab.Location = new System.Drawing.Point(4, 22);
            this.editTab.Name = "editTab";
            this.editTab.Size = new System.Drawing.Size(455, 433);
            this.editTab.TabIndex = 1;
            this.editTab.Text = "Script";
            this.editTab.UseVisualStyleBackColor = true;
            // 
            // openDLLButton
            // 
            this.openDLLButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.openDLLButton.Location = new System.Drawing.Point(420, 399);
            this.openDLLButton.Name = "openDLLButton";
            this.openDLLButton.Size = new System.Drawing.Size(27, 21);
            this.openDLLButton.TabIndex = 3;
            this.openDLLButton.Text = "...";
            this.openDLLButton.Click += new System.EventHandler(this.openDLLButton_Click);
            // 
            // dllPath
            // 
            this.dllPath.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.dllPath.Location = new System.Drawing.Point(65, 399);
            this.dllPath.Name = "dllPath";
            this.dllPath.ReadOnly = true;
            this.dllPath.Size = new System.Drawing.Size(338, 21);
            this.dllPath.TabIndex = 2;
            // 
            // label1
            // 
            this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.label1.Location = new System.Drawing.Point(9, 402);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(66, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Load DLL";
            // 
            // avisynthScript
            // 
            this.avisynthScript.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.avisynthScript.Location = new System.Drawing.Point(8, 15);
            this.avisynthScript.Multiline = true;
            this.avisynthScript.Name = "avisynthScript";
            this.avisynthScript.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.avisynthScript.Size = new System.Drawing.Size(439, 356);
            this.avisynthScript.TabIndex = 0;
            // 
            // saveAvisynthScriptDialog
            // 
            this.saveAvisynthScriptDialog.DefaultExt = "avs";
            this.saveAvisynthScriptDialog.Filter = "AviSynth Script Files|*.avs";
            this.saveAvisynthScriptDialog.Title = "Select a name for the AviSynth script";
            // 
            // openFilterDialog
            // 
            this.openFilterDialog.Filter = "AviSynth Filters|*.dll";
            this.openFilterDialog.Title = "Select an AviSynth Filter";
            // 
            // openSubsDialog
            // 
            this.openSubsDialog.Filter = "Subs Files|*.srt;*.ass;*.ssa;*.idx";
            this.openSubsDialog.Title = "Select a subtitle file";
            // 
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.deintProgressBar,
            this.deintStatusLabel});
            this.statusStrip1.Location = new System.Drawing.Point(0, 511);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(463, 22);
            this.statusStrip1.TabIndex = 6;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // deintProgressBar
            // 
            this.deintProgressBar.Name = "deintProgressBar";
            this.deintProgressBar.Size = new System.Drawing.Size(100, 16);
            // 
            // deintStatusLabel
            // 
            this.deintStatusLabel.Name = "deintStatusLabel";
            this.deintStatusLabel.Size = new System.Drawing.Size(0, 17);
            // 
            // onSaveLoadScript
            // 
            this.onSaveLoadScript.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.onSaveLoadScript.AutoSize = true;
            this.onSaveLoadScript.Checked = true;
            this.onSaveLoadScript.CheckState = System.Windows.Forms.CheckState.Checked;
            this.onSaveLoadScript.Location = new System.Drawing.Point(84, 478);
            this.onSaveLoadScript.Name = "onSaveLoadScript";
            this.onSaveLoadScript.Size = new System.Drawing.Size(210, 17);
            this.onSaveLoadScript.TabIndex = 18;
            this.onSaveLoadScript.Text = "On Save close and load to be encoded";
            // 
            // saveButton
            // 
            this.saveButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.saveButton.AutoSize = true;
            this.saveButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.saveButton.Location = new System.Drawing.Point(413, 475);
            this.saveButton.Name = "saveButton";
            this.saveButton.Size = new System.Drawing.Size(41, 23);
            this.saveButton.TabIndex = 20;
            this.saveButton.Text = "Save";
            this.saveButton.Click += new System.EventHandler(this.saveButton_Click);
            // 
            // previewAvsButton
            // 
            this.previewAvsButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.previewAvsButton.AutoSize = true;
            this.previewAvsButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.previewAvsButton.Location = new System.Drawing.Point(300, 475);
            this.previewAvsButton.Name = "previewAvsButton";
            this.previewAvsButton.Size = new System.Drawing.Size(107, 23);
            this.previewAvsButton.TabIndex = 19;
            this.previewAvsButton.Text = "Preview AVS Script";
            this.previewAvsButton.Click += new System.EventHandler(this.previewButton_Click);
            // 
            // helpButton1
            // 
            this.helpButton1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.helpButton1.ArticleName = "Avisynth script creator";
            this.helpButton1.AutoSize = true;
            this.helpButton1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.helpButton1.Location = new System.Drawing.Point(15, 472);
            this.helpButton1.Name = "helpButton1";
            this.helpButton1.Size = new System.Drawing.Size(38, 23);
            this.helpButton1.TabIndex = 17;
            // 
            // AviSynthWindow
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.ClientSize = new System.Drawing.Size(463, 533);
            this.Controls.Add(this.onSaveLoadScript);
            this.Controls.Add(this.saveButton);
            this.Controls.Add(this.previewAvsButton);
            this.Controls.Add(this.helpButton1);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.tabControl1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "AviSynthWindow";
            this.Text = "MeGUI - AviSynth script creator";
            this.resNCropGroupbox.ResumeLayout(false);
            this.resNCropGroupbox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.cropLeft)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.cropRight)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.cropBottom)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.cropTop)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.verticalResolution)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.horizontalResolution)).EndInit();
            this.tabControl1.ResumeLayout(false);
            this.optionsTab.ResumeLayout(false);
            this.gbOutput.ResumeLayout(false);
            this.videoGroupBox.ResumeLayout(false);
            this.videoGroupBox.PerformLayout();
            this.filterTab.ResumeLayout(false);
            this.tabSources.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.mpegOptGroupBox.ResumeLayout(false);
            this.tabPage2.ResumeLayout(false);
            this.aviOptGroupBox.ResumeLayout(false);
            this.aviOptGroupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.fpsBox)).EndInit();
            this.tabPage3.ResumeLayout(false);
            this.dgOptions.ResumeLayout(false);
            this.dgOptions.PerformLayout();
            this.deinterlacingGroupBox.ResumeLayout(false);
            this.deinterlacingGroupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.deintM)).EndInit();
            this.filtersGroupbox.ResumeLayout(false);
            this.filtersGroupbox.PerformLayout();
            this.editTab.ResumeLayout(false);
            this.editTab.PerformLayout();
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

		}

		
        
        private void input_FileSelected(FileBar sender, FileBarEventArgs args)
        {
            openVideoSource(input.Filename);
            if (chAutoPreview.Checked == true)
                previewButton_Click(sender, args);
            signalAR_Checkedchanged(null, null);
		}
		private void openDLLButton_Click(object sender, System.EventArgs e)
		{
            this.openFilterDialog.InitialDirectory = MainForm.Instance.Settings.AvisynthPluginsPath;
			if (this.openFilterDialog.ShowDialog() == DialogResult.OK)
			{
				dllPath.Text = openFilterDialog.FileName;
                string temp = avisynthScript.Text;
				script = new StringBuilder();
				script.Append("LoadPlugin(\"" + openFilterDialog.FileName + "\")\r\n");
				script.Append(temp);
				avisynthScript.Text = script.ToString();
			}
		}
		private void previewButton_Click(object sender, System.EventArgs e)
		{
            // If the player is null, create a new one.
            // Otherwise use the existing player to load the latest preview.
            if (player == null || player.IsDisposed) 
                player = new VideoPlayer();

            bool videoLoaded = player.loadVideo(mainForm, avisynthScript.Text, PREVIEWTYPE.REGULAR, false, true, player.CurrentFrame);
			if (videoLoaded)
			{
				player.disableIntroAndCredits();
                reader = player.Reader;
                isPreviewMode = true;
                sendCropValues();
				player.Show();
			}
		}
		private void saveButton_Click(object sender, System.EventArgs e)
		{
            string fileName = videoOutput.Filename;
            writeScript(fileName);
			if (onSaveLoadScript.Checked)
			{
                if(player != null)
				    player.Close();
				this.Close();
                OpenScript(fileName);
            }
		}
		
		
		private string generateScript()
		{
            if (!this.loaded)
                return "";
			script = new StringBuilder();
            //scriptLoad = new StringBuilder(); Better to use AviSynth plugin dir and it is easier for avs templates/profiles
			
			string inputLine = "#input";
			string deinterlaceLines = "#deinterlace";
			string denoiseLines = "#denoise";
			string cropLine = "#crop";
			string resizeLine = "#resize";

            double fps = (double)fpsBox.Value;
            inputLine = ScriptServer.GetInputLine(this.input.Filename, 
                                                  deinterlace.Checked, 
                                                  sourceType, 
                                                  colourCorrect.Checked, 
                                                  mpeg2Deblocking.Checked, 
                                                  flipVertical.Checked, 
                                                  fps,
                                                  dss2.Checked);
            if (nvDeInt.Enabled)
            {
                if (nvDeInt.Checked)
                     inputLine += ScriptServer.GetNvDeInterlacerLine(nvDeInt.Checked, (NvDeinterlacerType)(cbNvDeInt.SelectedItem as EnumProxy).RealValue);
                if (nvResize.Checked)
                    inputLine += ", resize_w=" + horizontalResolution.Value.ToString() + ", resize_h=" + verticalResolution.Value.ToString();                
                inputLine += ")";
            }
            if (deinterlace.Checked && deinterlaceType.SelectedItem is DeinterlaceFilter)
                deinterlaceLines = ((DeinterlaceFilter)deinterlaceType.SelectedItem).Script;
            cropLine = ScriptServer.GetCropLine(crop.Checked, Cropping);
            if (!nvResize.Checked)
                resizeLine = ScriptServer.GetResizeLine(resize.Checked, (int)horizontalResolution.Value, (int)verticalResolution.Value, (ResizeFilterType)(resizeFilterType.SelectedItem as EnumProxy).RealValue);
            denoiseLines = ScriptServer.GetDenoiseLines(noiseFilter.Checked, (DenoiseFilterType)(noiseFilterType.SelectedItem as EnumProxy).RealValue);

            string newScript = ScriptServer.CreateScriptFromTemplate(GetProfileSettings().Template, inputLine, cropLine, resizeLine, denoiseLines, deinterlaceLines);

            if (this.signalAR.Checked && suggestedDar.HasValue)
                newScript = string.Format("# Set DAR in encoder to {0} : {1}. The following line is for automatic signalling\r\nglobal MeGUI_darx = {0}\r\nglobal MeGUI_dary = {1}\r\n",
                    suggestedDar.Value.X, suggestedDar.Value.Y) + newScript;

            if (this.SubtitlesPath.Text != "")
            {
                newScript += "\r\nLoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "VSFilter.dll") + "\")";
                if (cbCharset.Enabled)
                {
                    string charset = CharsetValue();
                    newScript += "\r\nTextSub(\"" + SubtitlesPath.Text + "\"" + ", " + charset + ")\r\n";
                }
                else
                    newScript += "\r\nVobSub(\"" + SubtitlesPath.Text + "\")\r\n";
            }
            return newScript;
		}

        private AviSynthSettings GetProfileSettings()
        {
            return (AviSynthSettings)avsProfile.SelectedProfile.BaseSettings;
        }

		private void showScript()
		{
			string text = this.generateScript();
			avisynthScript.Text = text;
		}
		
		
        /// <summary>
        /// Opens a video source using the correct method based on the extension of the file name
        /// </summary>
        /// <param name="videoInput"></param>
        private void openVideoSource (string videoInput)
        {
            string projectPath;
            string fileNameNoPath = Path.GetFileName(videoInput);
            if (string.IsNullOrEmpty(projectPath = mainForm.Settings.DefaultOutputDir))
                projectPath = Path.GetDirectoryName(videoInput);
            videoOutput.Filename = Path.Combine(projectPath, Path.ChangeExtension(fileNameNoPath, ".avs"));
            
            if (File.Exists(videoInput + ".ffindex"))
            {
                sourceType = PossibleSources.ffindex;
                openVideo(videoInput + ".ffindex");
            }
            else
            {
                string ext = Path.GetExtension(videoInput).ToLower();
                switch (ext)
                {
                    case ".avs":
                        sourceType = PossibleSources.avs;
                        videoOutput.Filename = Path.Combine(projectPath, Path.ChangeExtension(fileNameNoPath, "_new.avs")); // to avoid overwritten
                        openAVSScript(videoInput);
                        break;           
                    case ".d2v":
                        sourceType = PossibleSources.d2v;
                        openVideo(videoInput);
                        break;
                    case ".dga":
                        sourceType = PossibleSources.dga;                    
                        openVideo(videoInput);
                        break;
                    case ".dgi":
                        sourceType = PossibleSources.dgi;
                        if (VideoUtil.manageCUVIDServer())
                            openVideo(videoInput); 
                        break;
                    case ".ffindex":
                        sourceType = PossibleSources.ffindex;
                        openVideo(videoInput);
                        break;
                    case ".mpeg": // include case variants 
                    case ".mpg":
                    case ".m2v":
                    case ".m2p":
                    case ".mpv":
                    case ".ts":
                    case ".tp":
                    case ".vob":
                        sourceType = PossibleSources.mpeg2;
                        if (gotoD2vCreator(videoInput) == DialogResult.Cancel )
                        {
                            sourceType = PossibleSources.directShow;
                            openDirectShow(videoInput);
                        }
                        break;
                    case ".vdr":
                        sourceType = PossibleSources.vdr;
                        openVDubFrameServer(videoInput);
                        break;
                    default:
                        sourceType = PossibleSources.directShow;
                        openDirectShow(videoInput);
                        break;
                }
            }
            setSourceInterface();
        }
		/// <summary>
		/// writes the AviSynth script currently shown in the GUI to the given path
		/// </summary>
		/// <param name="path">path and name of the AviSynth script to be written</param>
		private void writeScript(string path)
		{
			try
			{
				using (StreamWriter sw = new StreamWriter(path, false, Encoding.Default))
                {
				    sw.Write(avisynthScript.Text);
				    sw.Close();
                }
			}
			catch (IOException i)
			{
				MessageBox.Show("An error occurred when trying to save the AviSynth script:\r\n" + i.Message);
			}
		}
        /// <summary>
        /// Set the correct states of the interface elements that are only valid for certain inputs
        /// </summary>
        private void setSourceInterface()
        {
            if (Mod16Method.Equals(mod16Method.none)) 
                mod16Box.SelectedIndex = 0;

            switch (this.sourceType)
            {            
                case PossibleSources.d2v:
                case PossibleSources.dga:
                case PossibleSources.mpeg2:
                    this.mpeg2Deblocking.Enabled = true;
                    this.colourCorrect.Enabled = true;
                    this.fpsBox.Enabled = false;
                    this.flipVertical.Enabled = false;
                    this.flipVertical.Checked = false;
                    this.cbNvDeInt.Enabled = false;
                    this.nvDeInt.Enabled = false;
                    this.nvDeInt.Checked = false;
                    this.nvResize.Enabled = false;                    
                    this.nvResize.Checked = false;
                    this.dss2.Enabled = false;
                    this.tabSources.SelectedTab = tabPage1;
                    break;
                case PossibleSources.vdr:
                case PossibleSources.avs:
                    this.mpeg2Deblocking.Checked = false;
                    this.mpeg2Deblocking.Enabled = false;
                    this.colourCorrect.Enabled = false;
                    this.colourCorrect.Checked = false;
                    this.flipVertical.Enabled = false;
                    this.flipVertical.Checked = false;
                    this.dss2.Enabled = false;
                    this.fpsBox.Enabled = false;
                    this.cbNvDeInt.Enabled = false;
                    this.nvDeInt.Enabled = false;
                    this.nvDeInt.Checked = false;
                    this.nvResize.Enabled = false;
                    this.nvResize.Checked = false;
                    this.tabSources.SelectedTab = tabPage1;
                    break;
                case PossibleSources.ffindex:
                    this.mpeg2Deblocking.Checked = false;
                    this.mpeg2Deblocking.Enabled = false;
                    this.colourCorrect.Enabled = false;
                    this.colourCorrect.Checked = false;
                    this.dss2.Enabled = false;
                    this.fpsBox.Enabled = false;
                    this.flipVertical.Enabled = true;
                    this.cbNvDeInt.Enabled = false;
                    this.nvDeInt.Enabled = false;
                    this.nvDeInt.Checked = false;
                    this.nvResize.Enabled = false;
                    this.nvResize.Checked = false;
                    this.tabSources.SelectedTab = tabPage1;
                    break;
                case PossibleSources.directShow:
                    this.mpeg2Deblocking.Checked = false;
                    this.mpeg2Deblocking.Enabled = false;
                    this.colourCorrect.Enabled = false;
                    this.colourCorrect.Checked = false;
                    this.dss2.Enabled = true;
                    this.fpsBox.Enabled = true;
                    this.flipVertical.Enabled = true;
                    this.cbNvDeInt.Enabled = false;
                    this.nvDeInt.Enabled = false;
                    this.nvDeInt.Checked = false;
                    this.nvResize.Enabled = false;
                    this.nvResize.Checked = false;
                    this.tabSources.SelectedTab = tabPage2;
                    break;
                case PossibleSources.dgi:
                    this.mpeg2Deblocking.Checked = false;
                    this.mpeg2Deblocking.Enabled = false;
                    this.colourCorrect.Enabled = false;
                    this.colourCorrect.Checked = false;
                    this.flipVertical.Enabled = false;
                    this.flipVertical.Checked = false;
                    this.dss2.Enabled = false;
                    this.fpsBox.Enabled = false;
                    this.cbNvDeInt.Enabled = false;
                    this.nvDeInt.Enabled = true;
                    this.nvDeInt.Checked = false;
                    this.nvResize.Enabled = true;
                    this.nvResize.Checked = false;
                    this.cbNvDeInt.SelectedIndex = 0;
                    this.tabSources.SelectedTab = tabPage3;
                    this.showScript();
                    break;
            }
        }

        /// <summary>
        /// check whether or not it's an NV file compatible (for DGxNV tools)
        /// </summary>
        ///
        private void checkNVCompatibleFile(string input)
        {
            bool flag = false;
            using (StreamReader sr = new StreamReader(input))
            {
                string line = sr.ReadLine();
                switch (this.sourceType)
                {
                    case PossibleSources.dgi:
                        if (line.Contains("DGMPGIndexFileNV")) flag = true;
                        if (line.Contains("DGAVCIndexFileNV")) flag = true;
                        if (line.Contains("DGVC1IndexFileNV")) flag = true; 
                        break; 
                }
            }
            if (!flag)
            {
                if (MessageBox.Show("You cannot use this option with the " + Path.GetFileName(input) + " file. It's not compatible...",
                    "Information", MessageBoxButtons.OK, MessageBoxIcon.Information) == DialogResult.OK)
                {
                    this.nvDeInt.Checked = false;
                    this.nvResize.Checked = false;
                }
            }
        }

        private SourceInfo DeintInfo
        {
            get
            {
                SourceInfo info = new SourceInfo();
                try { info.sourceType = (SourceType)((EnumProxy)deintSourceType.SelectedItem).Tag; }
                catch (NullReferenceException) { info.sourceType = SourceType.UNKNOWN; }
                try { info.fieldOrder = (FieldOrder)((EnumProxy)deintFieldOrder.SelectedItem).Tag; }
                catch (NullReferenceException) { info.fieldOrder = FieldOrder.UNKNOWN; }
                info.decimateM = (int)deintM.Value;
                try
                {
                    info.majorityFilm = ((UserSourceType)((EnumProxy)deintSourceType.SelectedItem).RealValue)
                   == UserSourceType.HybridFilmInterlaced;
                }
                catch (NullReferenceException) { }
                info.isAnime = deintIsAnime.Checked;
                return info;
            }
            set
            {
                if (value.sourceType == SourceType.UNKNOWN || value.sourceType == SourceType.NOT_ENOUGH_SECTIONS)
                {
                    MessageBox.Show("Source detection couldn't determine the source type!", "Source detection failed", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    return;
                }
                foreach (EnumProxy o in deintSourceType.Items)
                {
                    if ((SourceType)o.Tag == value.sourceType) deintSourceType.SelectedItem = o;
                }
                foreach (EnumProxy o in deintFieldOrder.Items)
                {
                    if ((FieldOrder)o.Tag == value.fieldOrder) deintFieldOrder.SelectedItem = o;
                }
                if (value.fieldOrder == FieldOrder.UNKNOWN) deintFieldOrder.SelectedIndex = -1;
                deintM.Value = value.decimateM;
                if (value.sourceType == SourceType.HYBRID_FILM_INTERLACED)
                {
                    if (value.majorityFilm) deintSourceType.SelectedItem = ScriptServer.ListOfSourceTypes[(int)UserSourceType.HybridFilmInterlaced];
                    else deintSourceType.SelectedItem = ScriptServer.ListOfSourceTypes[(int)UserSourceType.HybridInterlacedFilm];
                }
                this.deinterlaceType.DataSource = ScriptServer.GetDeinterlacers(value);
                this.deinterlaceType.BindingContext = new BindingContext();
            }
        }
        /// <summary>
        /// Check whether direct show can render the avi and then open it through an avisynth script.
        /// The avs is being used in order to allow more preview flexibility later.
        /// </summary>
        /// <param name="fileName">Input video file</param>
        private void openDirectShow(string fileName)
        {
            if (!File.Exists(fileName))
            {
                MessageBox.Show(fileName + " could not be found", "File Not Found", MessageBoxButtons.OK);
                return;
            }
            else
            {
                DirectShow ds = new DirectShow();
                if (!ds.checkRender(fileName)) // make sure graphedit can render the file
                {
                    MessageBox.Show("Unable to render the file.\r\nYou probably don't have the correct filters installed", "Direct Show Error", MessageBoxButtons.OK);
                }
                else
                {
                    string frameRateString = null;
                    try
                    {
                        MediaInfoFile info = new MediaInfoFile(fileName);
                        if (info.Info.HasVideo && info.Info.FPS > 0)
                            frameRateString = info.Info.FPS.ToString(System.Globalization.CultureInfo.InvariantCulture);
                    }
                    catch (Exception)
                    { }
                    string tempAvs = string.Format(
                            @"DirectShowSource(""{0}"", audio=false{1}, convertfps=true){2}",
                            fileName,
                            frameRateString == null ? string.Empty : (", fps=" + frameRateString),
                            this.flipVertical.Checked ? ".FlipVertical()" : string.Empty
                            );
                    if (file != null)
                        file.Dispose();
                   openVideo(tempAvs, fileName, true);
                }
            }
        }
        /// <summary>
        /// Create a temporary avs to wrap the frameserver file then open it as for any other avs
        /// </summary>
        /// <param name="fileName">Name of the .vdr file</param>
        private void openVDubFrameServer(string fileName)
        {
            if (!File.Exists(fileName))
            {
                MessageBox.Show(fileName + " could not be found","File Not Found",MessageBoxButtons.OK);
                return;
            }
            else
            {
                openVideo("AviSource(\"" + fileName + ", audio=false\")\r\n", fileName, true);
            }
        }
        /// <summary>
        /// Create a temporary avs to wrap the frameserver file then open it as for any other avs
        /// </summary>
        /// <param name="fileName">Name of the avs script</param>
        private void openAVSScript(string fileName)
        {
            if (!File.Exists(fileName))
            {
                MessageBox.Show(fileName + " could not be found", "File Not Found", MessageBoxButtons.OK);
                return;
            }
            else
            {
                openVideo("Import(\"" + fileName + "\")\r\n", fileName, true);
            }
        }
        /// <summary>
        ///  call the file indexer which will pass a queued job back to the main form
        /// </summary>
        /// <param name="fileName"></param>
        private DialogResult gotoD2vCreator(string fileName)
        {
            return MessageBox.Show("You can't open such files with the AviSynth Script Creator. You'll have to index it first with a DG indexer, and open the created project file here.",
                "Can't open files", MessageBoxButtons.OKCancel, MessageBoxIcon.Error);
        }

        private void enableControls(bool enable)
        {
            foreach (Control ctrl in this.controlsToDisable)
                ctrl.Enabled = enable;
                if (deintSourceType.SelectedIndex < 1)
                {
                    deinterlace.Enabled = false;
                    deinterlace.Checked = false;
                }
                else
                    deinterlace.Enabled = true;
        }

        private void openVideo(string videoInput)
        {
            openVideo(videoInput, videoInput, false);
        }

        private bool showOriginal()
        {
            if (player == null || player.IsDisposed)
            {
                player = new VideoPlayer();
            }
            this.isPreviewMode = false;
            if (player.loadVideo(mainForm, originalScript, PREVIEWTYPE.REGULAR, false, originalInlineAvs, player.CurrentFrame))
            {
                player.Show();
                reader = player.Reader;
                sendCropValues();
                if (mainForm.Settings.AlwaysOnTop) player.TopMost = true;
                return true;
            }
            else
            {
                player.Close();
                player = null;
                return false;
            }
        }

		/// <summary>
		/// opens a given DGIndex script
		/// </summary>
		/// <param name="videoInput">the DGIndex script to be opened</param>
		private void openVideo(string videoInput, string textBoxName, bool inlineAvs)
		{
			this.crop.Checked = false;
            this.input.Filename = "";
            this.originalScript = videoInput;
            this.originalInlineAvs = inlineAvs;
            if (player != null)
                player.Dispose();
            bool videoLoaded = showOriginal();
            enableControls(videoLoaded);
			if (videoLoaded)
			{
                this.input.Filename = textBoxName;
                file = player.File;
                reader = player.Reader;
                this.fpsBox.Value = (decimal)file.Info.FPS;
                if (file.Info.FPS.Equals(25.0)) // disable ivtc for pal sources
					this.tvTypeLabel.Text = "PAL";
				else
					this.tvTypeLabel.Text = "NTSC";
                horizontalResolution.Maximum = file.Info.Width;
                verticalResolution.Maximum = file.Info.Height;
                horizontalResolution.Value = file.Info.Width;
                verticalResolution.Value = file.Info.Height;
                arChooser.Value = file.Info.DAR;

                cropLeft.Maximum = cropRight.Maximum = file.Info.Width / 2;
                cropTop.Maximum = cropBottom.Maximum = file.Info.Height / 2;
                /// Commented out to ensure to keep the source file resolution when opening it
                /// if (resize.Enabled && resize.Checked)
                ///    suggestResolution.Checked = true;
                /// --------------------------------------------------------------------------
				this.showScript();
			}
		}
		
		
		private void horizontalResolution_ValueChanged(object sender, System.EventArgs e)
		{
            if (eventsOn)
            {
                suggestResolution_CheckedChanged(null, null);
                chAutoPreview_CheckedChanged(null, null);
                this.showScript();
            }
		}

		private void verticalResolution_ValueChanged(object sender, System.EventArgs e)
		{
            chAutoPreview_CheckedChanged(null, null);
            this.showScript();
		}
		private void sendCropValues()
		{
            if (player != null && player.Visible)
            {
                if (isPreviewMode)
                    player.crop(0, 0, 0, 0);
                else
                    player.crop(Cropping);
            }
		}
		
		
		private void crop_CheckedChanged(object sender, System.EventArgs e)
		{
			if (crop.Checked)
			{
				this.cropLeft.Enabled = true;
				this.cropTop.Enabled = true;
				this.cropRight.Enabled = true;
				this.cropBottom.Enabled = true;
				sendCropValues();
			}
			else
			{
				this.cropLeft.Enabled = false;
				this.cropTop.Enabled = false;
				this.cropRight.Enabled = false;
				this.cropBottom.Enabled = false;
				if (player != null && player.Visible)
					player.crop(0, 0, 0, 0);
			}
            suggestResolution_CheckedChanged(null, null);
            chAutoPreview_CheckedChanged(null, null);
            this.showScript();
		}
		private void deinterlace_CheckedChanged(object sender, System.EventArgs e)
		{
			if (deinterlace.Checked)
			{
				deinterlaceType.Enabled = true;
				if (deinterlaceType.SelectedIndex == -1)
					deinterlaceType.SelectedIndex = 0; // make sure something is selected
			}
			else
			{
				deinterlaceType.Enabled = false;
			}
			this.showScript();
		}
		private void noiseFilter_CheckedChanged(object sender, System.EventArgs e)
		{
			if (noiseFilter.Checked)
			{
				this.noiseFilterType.Enabled = true;
			}
			else
				this.noiseFilterType.Enabled = false;
			this.showScript();
		}
        private void signalAR_Checkedchanged(object sender, System.EventArgs e)
        {
            if (signalAR.Checked)
            {
                this.mod16Box.Enabled = true;
                this.suggestResolution.Enabled = true;
                this.suggestResolution.Checked = true;
                if (mod16Box.SelectedIndex == -1)
                    mod16Box.SelectedIndex = 0;
                mod16Box_SelectedIndexChanged(null, null);
                suggestResolution_CheckedChanged(null, null);
            }
            else
            {
                this.mod16Box.Enabled = false;
                this.suggestResolution.Enabled = true;
                this.suggestResolution.Checked = false;
                mod16Box_SelectedIndexChanged(null, null);
            }
            this.showScript();
        }
        void checkedChanged(object sender, EventArgs e)
        {
            this.showScript();
        }

		
		
		private void resizeFilterType_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			this.showScript();
		}

		private void deinterlaceType_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			this.showScript();
		}

		private void noiseFilterType_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			this.showScript();
		}

        private void cbNvDeInt_SelectedIndexChanged(object sender, System.EventArgs e)
        {
            this.showScript();
        }
		
		
		/// <summary>
		/// gets the autocrop values
		/// we start at 25% of the video, then advance by 5% and analyze 10 frames in total
		/// (thus at position 25%, 30%, 35%, ... 70%)
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void autoCropButton_Click(object sender, System.EventArgs e)
		{
            if (isPreviewMode || player == null || !player.Visible)
            {
                MessageBox.Show(this, "No AutoCropping without the original video window open",
                    "AutoCropping not possible",MessageBoxButtons.OK,MessageBoxIcon.Error);
                return;
            }
            CropValues final = Autocrop.autocrop(reader);
			bool error = (final.left == -1);
			if (!error)
			{
				cropLeft.Value = final.left;
				cropTop.Value = final.top;
				cropRight.Value = final.right;
				cropBottom.Value = final.bottom;
				if (!crop.Checked)
					crop.Checked = true;
                chAutoPreview_CheckedChanged(null, null);
                this.showScript();
			}
			else
				MessageBox.Show("I'm afraid I was unable to find 3 frames that have matching crop values");
		}

		/// <summary>
		/// if enabled, each change of the horizontal resolution triggers this method
		/// it calculates the ideal mod 16 vertical resolution that matches the desired horizontal resolution
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
        private void suggestResolution_CheckedChanged(object sender, System.EventArgs e)
        {
            if (file == null) return;

            try
            {
                double dar = 1.0;
                dar = (double)arChooser.RealValue.ar;
                Dar? suggestedDar;

                bool signalAR = this.signalAR.Checked;
                int scriptVerticalResolution = Resolution.suggestResolution((int)file.Info.Height, (int)file.Info.Width, dar,
                    Cropping, (int)horizontalResolution.Value, signalAR, mainForm.Settings.AcceptableAspectErrorPercent, out suggestedDar);


                if (suggestResolution.Checked)
                {
                    this.verticalResolution.Enabled = false;
                    if (scriptVerticalResolution > verticalResolution.Maximum)
                    { // Reduce horizontal resolution until a fit is found that doesn't require upsizing. This is really only needed for oddball DAR scenarios
                        int hres = (int)horizontalResolution.Value;
                        do
                        {
                            hres -= 16;
                            scriptVerticalResolution = Resolution.suggestResolution((int)file.Info.Height, (int)file.Info.Width, dar, Cropping, hres, signalAR, mainForm.Settings.AcceptableAspectErrorPercent, out suggestedDar);
                        }
                        while (scriptVerticalResolution > verticalResolution.Maximum && hres > 0);
                        eventsOn = false;
                        horizontalResolution.Value = hres;
                        eventsOn = true;
                    }
                    verticalResolution.Value = (decimal)scriptVerticalResolution;
                    chAutoPreview_CheckedChanged(null, null);
                    this.showScript();
                }
                else
                    this.verticalResolution.Enabled = resize.Checked;


                if (signalAR)
                    this.suggestedDar = suggestedDar;
            }
            catch (Exception exc)
            {
                MessageBox.Show("Error in computing resolution\r\n" + exc.Message, "Unspecified Error", MessageBoxButtons.OK);
            }
        }

		private void customDAR_TextChanged(object sender, System.EventArgs e)
		{
			suggestResolution_CheckedChanged(null, null);
            this.showScript();
		}
		
        
        void fpsBox_ValueChanged(object sender, EventArgs e)
        {
            this.showScript();
        }
        
        
        private AviSynthSettings Settings
		{
			set
			{
				this.resizeFilterType.SelectedItem =  EnumProxy.Create( value.ResizeMethod);
                this.noiseFilterType.SelectedItem = EnumProxy.Create(value.DenoiseMethod);
				this.mpeg2Deblocking.Checked = value.MPEG2Deblock;
				this.colourCorrect.Checked = value.ColourCorrect;
				this.deinterlace.Checked = value.Deinterlace;
				this.noiseFilter.Checked = value.Denoise;
                this.resize.Checked = value.Resize;
                this.mod16Box.SelectedIndex = (int)value.Mod16Method;
                this.signalAR.Checked = (value.Mod16Method != mod16Method.none);
                this.dss2.Checked = value.DSS2;
				this.showScript();
			}
        }
        private CropValues Cropping
        {
            get
            {
                CropValues returnValue = new CropValues();
                if (crop.Checked)
                {
                    returnValue.bottom = (int)cropBottom.Value;
                    returnValue.top = (int)cropTop.Value;
                    returnValue.left = (int)cropLeft.Value;
                    returnValue.right = (int)cropRight.Value;
                    if (Mod16Method == mod16Method.overcrop)
                        ScriptServer.overcrop(ref returnValue);
                    else if (Mod16Method == mod16Method.mod4Horizontal)
                        ScriptServer.cropMod4Horizontal(ref returnValue);
                    else if (Mod16Method == mod16Method.undercrop)
                        ScriptServer.undercrop(ref returnValue);
                }
                return returnValue;
            }
        }
        private mod16Method Mod16Method
        {
            get
            {
                mod16Method m = (mod16Method)mod16Box.SelectedIndex;
                if (!mod16Box.Enabled)
                    m = mod16Method.none;
                return m;
            }
        }

        
        
        private void analyseButton_Click(object sender, EventArgs e)
        {
            if (input.Filename.Length > 0)
            {
                if (detector == null) // We want to start the analysis
                {
                    string source = ScriptServer.GetInputLine(input.Filename, false, sourceType, false, false, false, 25, false);
                    if (nvDeInt.Enabled) source += ")";
                    detector = new SourceDetector(source,
                        input.Filename, deintIsAnime.Checked,
                        mainForm.Settings.SourceDetectorSettings,
                        new UpdateSourceDetectionStatus(analyseUpdate),
                        new FinishedAnalysis(finishedAnalysis));
                        detector.analyse();
                        deintStatusLabel.Text = "Analysing...";
                        analyseButton.Text = "Abort";
                }
                else // We want to cancel the analysis
                {
                    detector.stop();
                    deintStatusLabel.Text = "Analysis aborted!";
                    detector = null;
                    analyseButton.Text = "Analyse";
                    this.deintProgressBar.Value = 0;
                }
            }
            else
                MessageBox.Show("Can't run any analysis as there is no selected video to analyse.",
                    "Please select a video input file", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }

        public void finishedAnalysis(SourceInfo info, bool error, string errorMessage)
        {
            if (error)
                Invoke(new MethodInvoker(delegate
                {
                    MessageBox.Show(this, errorMessage, "Error in analysis", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }));
            else
            {
                try
                {
                    info.isAnime = deintIsAnime.Checked;
                    Invoke(new MethodInvoker(delegate
                    {
                        deintProgressBar.Enabled = false;
                        this.DeintInfo = info;
                        if (deintSourceType.SelectedIndex < 1)
                        {
                            deinterlace.Enabled = false;
                            deinterlace.Checked = false;
                        }
                        else
                            deinterlace.Enabled = true;
                        if (deinterlaceType.Text == "Do nothing")
                            deinterlace.Checked = false;
                        else
                            deinterlace.Checked = true;
                        deintStatusLabel.Text = "Analysis finished!";
                        analyseButton.Text = "Analyse";
                    }));
                }
                catch (Exception) { } // If we get any errors, it's most likely because the window was closed, so just ignore
            }
            detector = null;
        }

        public void analyseUpdate(int amountDone, int total)
        {
            try
            {
                Invoke(new MethodInvoker(delegate
                    {
                        this.deintProgressBar.Value = amountDone;
                        this.deintProgressBar.Maximum = total;
                    }));
            }
            catch (Exception) { } // If we get any errors, just ignore -- it's only a cosmetic thing.
        }
        

        private void resize_CheckedChanged(object sender, EventArgs e)
        {
            if (resize.Checked)
            {
                this.horizontalResolution.Enabled = true;
                this.verticalResolution.Enabled = !suggestResolution.Checked;
                if (Mod16Method == mod16Method.resize)
                    this.suggestResolution.Enabled = false;
                else
                    this.suggestResolution.Enabled = true;
            }
            else
            {
                this.horizontalResolution.Enabled = this.verticalResolution.Enabled = false;
                this.suggestResolution.Enabled = this.suggestResolution.Checked = false;
            }
            chAutoPreview_CheckedChanged(null, null);
            this.showScript();
        }

        private void mod16Box_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (Mod16Method == mod16Method.overcrop)
                crop.Text = "Crop (will be rounded up to mod16)";
            else if (Mod16Method == mod16Method.undercrop)
                crop.Text = "Crop (will be rounded down to mod16)";
            else
                crop.Text = "Crop";
            crop_CheckedChanged(null, null);

            if (Mod16Method == mod16Method.resize)
            {
                this.suggestResolution.Enabled = false;
                this.suggestResolution.Checked = true;
                resize.Enabled = false;
                resize.Checked = true;
                horizontalResolution.Value = horizontalResolution.Maximum;
                suggestResolution_CheckedChanged(null, null);
            }
            else if (Mod16Method == mod16Method.none)
            {
                resize.Enabled = true;
                suggestResolution.Enabled = true;
                resize_CheckedChanged(null, null);
            }
            else
            {
                resize.Checked = false;
                resize.Enabled = false;
                resize_CheckedChanged(null, null);
            }
            showScript();
        }

        private void deintSourceType_SelectedIndexChanged(object sender, EventArgs e)
        {
            deintM.Enabled = (deintSourceType.SelectedItem == ScriptServer.ListOfSourceTypes[(int)UserSourceType.Decimating]);
            deintFieldOrder.Enabled = !(deintSourceType.SelectedItem == ScriptServer.ListOfSourceTypes[(int)UserSourceType.Progressive]);
            deinterlaceType.DataSource = ScriptServer.GetDeinterlacers(DeintInfo);
            deinterlaceType.BindingContext = new BindingContext();
            if (deintSourceType.SelectedIndex < 1)
            {
                deinterlace.Enabled = false;
                deinterlace.Checked = false;
            }
            else
                deinterlace.Enabled = true;
            showScript();
        }

        private void reopenOriginal_Click(object sender, EventArgs e)
        {
            if (chAutoPreview.Checked)
                chAutoPreview.Checked = false;
            else
                showOriginal();
        }

        private void arChooser_SelectionChanged(object sender, string val)
        {
            suggestResolution_CheckedChanged(null, null);
            showScript();
        }

        private void chAutoPreview_CheckedChanged(object sender, EventArgs e)
        {
            if (chAutoPreview.Checked)
                previewButton_Click(null, null);
            else if (this.isPreviewMode == true)
                showOriginal();
        }

        private void nvDeInt_CheckedChanged(object sender, EventArgs e)
        {
            if (nvDeInt.Checked)
                 cbNvDeInt.Enabled = true;
            else cbNvDeInt.Enabled = false;
            this.showScript();
        }

        private void nvDeInt_Click(object sender, EventArgs e)
        {
            // just to be sure
            checkNVCompatibleFile(input.Filename);
        }

        private void openSubtitlesButton_Click(object sender, EventArgs e)
        {
            if (this.openSubsDialog.ShowDialog() == DialogResult.OK)
            {
                if (this.SubtitlesPath.Text != openSubsDialog.FileName)
                {
                    string ext = Path.GetExtension(openSubsDialog.FileName).ToString().ToLower();
                    this.SubtitlesPath.Text = openSubsDialog.FileName;
                    if (ext == ".idx")
                        cbCharset.Enabled = false;
                    MessageBox.Show("Subtitles successfully added to the script...", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
                else MessageBox.Show("The subtitles you chosen was already added...", "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning);
            }
            this.showScript();
        }

        private string CharsetValue()
        {
            string c = string.Empty;

            if (!string.IsNullOrEmpty(SubtitlesPath.Text))
            {
                switch (cbCharset.SelectedIndex)
                {
                    case 1: c = "0"; break;
                    case 2: c = "2"; break;
                    case 3: c = "128"; break;
                    case 4:
                    case 5: c = "129"; break;
                    case 6: c = "134"; break;
                    case 7: c = "136"; break;
                    case 8: c = "255"; break;
                    case 9: c = "130"; break;
                    case 10: c = "177"; break;
                    case 11: c = "178"; break;
                    case 12: c = "161"; break;
                    case 13: c = "162"; break;
                    case 14: c = "163"; break;
                    case 15: c = "222"; break;
                    case 16: c = "238"; break;
                    case 17: c = "204"; break;
                    case 18: c = "77"; break;
                    case 19: c = "186"; break;
                    default: c = "1"; break;
                }
            }
            return c;
        }

        private void cbCharset_SelectedIndexChanged(object sender, EventArgs e)
        {
            this.showScript();
        }

        private void nvResize_CheckedChanged(object sender, EventArgs e)
        {
            this.showScript();
        }
    }
    public delegate void OpenScriptCallback(string avisynthScript);
    public enum PossibleSources { d2v, dga, dgi, mpeg2, vdr, directShow, avs, ffindex };
    public enum mod16Method : int { none = -1, resize = 0, overcrop, nonMod16, mod4Horizontal, undercrop };

    public class AviSynthWindowTool : MeGUI.core.plugins.interfaces.ITool
    {

        

        public string Name
        {
            get { return "AVS Script Creator"; }
        }

        public void Run(MainForm info)
        {
            info.ClosePlayer();
            AviSynthWindow asw = new AviSynthWindow(info);
            asw.OpenScript += new OpenScriptCallback(info.Video.openVideoFile);
            asw.Show();
        }

        public Shortcut[] Shortcuts
        {
            get { return new Shortcut[] { Shortcut.CtrlR }; }
        }

        

        

        public string ID
        {
            get { return "AvsCreator"; }
        }

        
    }
}
