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
using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace MeGUI
{
	/// <summary>
	/// Summary description for SettingsForm.
	/// </summary>
	public class SettingsForm : System.Windows.Forms.Form
	{
		
        private System.Windows.Forms.Button saveButton;
		private System.Windows.Forms.Button cancelButton;
        private MeGUISettings internalSettings = new MeGUISettings();
        private Button resetDialogs;
        private TabControl tabControl1;
        private TabPage tabPage1;
        private SourceDetectorSettings sdSettings;
        private System.Windows.Forms.FolderBrowserDialog openFolderDialog;
        private CheckBox chkboxUseAdvancedTooltips;
        private Button configSourceDetector;
        private System.Windows.Forms.GroupBox otherGroupBox;
		private System.Windows.Forms.ComboBox priority;
		private System.Windows.Forms.OpenFileDialog openExecutableDialog;
        private System.Windows.Forms.CheckBox autostartQueue;
		private System.Windows.Forms.CheckBox openScript;
		private System.Windows.Forms.Label priorityLabel;
		private System.Windows.Forms.CheckBox deleteCompletedJobs;
		private System.Windows.Forms.CheckBox deleteAbortedOutput;
		private System.Windows.Forms.CheckBox deleteIntermediateFiles;
        private System.Windows.Forms.CheckBox openProgressWindow;
        private NumericUpDown acceptableAspectError;
        private Label acceptableAspectErrorLabel;
        private AutoEncodeDefaultsSettings autoEncodeDefaults;
        private TabPage tabPage3;
        private GroupBox autoUpdateGroupBox;
        private CheckBox useAutoUpdateCheckbox;
        private GroupBox outputExtensions;
        private TextBox videoExtension;
        private Label label11;
        private Label label12;
        private TextBox audioExtension;
        private GroupBox autoModeGroupbox;
        private Label label13;
        private NumericUpDown nbPasses;
        private Label audioExtLabel;
        private Label videoExtLabel;
        private Button autoEncodeDefaultsButton;
        private TextBox command;
        private RadioButton runCommand;
        private RadioButton shutdown;
        private RadioButton donothing;
        private Button configureServersButton;
        private Label label14;
        private NumericUpDown maxServersToTry;
        private NumericUpDown acceptableFPSError;
        private Label label15;
        private NumericUpDown audiosamplesperupdate;
        private Label label6;
        private MeGUI.core.gui.HelpButton helpButton1;
        private CheckBox keep2ndPassOutput;
        private CheckBox keep2ndPassLogFile;
        private Button configAutoEncodeDefaults;
        private GroupBox gbVideoPreview;
        private CheckBox chAlwaysOnTop;
        private GroupBox groupBox2;
        private TextBox txt_httpproxyuid;
        private TextBox txt_httpproxyaddress;
        private Label label21;
        private Label label20;
        private Label label19;
        private Label label18;
        private CheckBox cbx_usehttpproxy;
        private TextBox txt_httpproxypwd;
        private TextBox txt_httpproxyport;
        private GroupBox gbDefaultOutput;
        private Button clearDefaultOutputDir;
        private FileBar defaultOutputDir;
        private CheckBox cbAddTimePos;
        private CheckBox backupfiles;
        private CheckBox forcerawavcuse;
        private TabPage tabPage2;
        private TextBox textBox2;
        private TextBox textBox8;
        private CheckBox checkBox1;
        private Label label2;
        private Button button2;
        private Label besplit;
        private Button button8;
        private GroupBox vobGroupBox;
        private CheckBox btnCUVIDServer;
        private ComboBox defaultLanguage2;
        private Label defaultAudioTrack2Label;
        private ComboBox defaultLanguage1;
        private Label defaultAudioTrack1Label;
        private Label percentLabel;
        private NumericUpDown forceFilmPercentage;
        private CheckBox autoForceFilm;
        private GroupBox groupBox4;
        private Button btnClearMP4TempDirectory;
        private FileBar tempDirMP4;
        private GroupBox groupBox3;
        private GroupBox groupBox5;
        private GroupBox groupBox6;

		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
		
		
		public SettingsForm()
		{
			InitializeComponent();
            List<string> keys = new List<string>(LanguageSelectionContainer.Languages.Keys);
            defaultLanguage2.DataSource = defaultLanguage1.DataSource = keys;
            defaultLanguage2.BindingContext = new BindingContext();
            defaultLanguage1.BindingContext = new BindingContext();
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
            System.Windows.Forms.GroupBox groupBox1;
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SettingsForm));
            this.command = new System.Windows.Forms.TextBox();
            this.runCommand = new System.Windows.Forms.RadioButton();
            this.shutdown = new System.Windows.Forms.RadioButton();
            this.donothing = new System.Windows.Forms.RadioButton();
            this.saveButton = new System.Windows.Forms.Button();
            this.cancelButton = new System.Windows.Forms.Button();
            this.otherGroupBox = new System.Windows.Forms.GroupBox();
            this.forcerawavcuse = new System.Windows.Forms.CheckBox();
            this.audiosamplesperupdate = new System.Windows.Forms.NumericUpDown();
            this.label6 = new System.Windows.Forms.Label();
            this.acceptableFPSError = new System.Windows.Forms.NumericUpDown();
            this.label15 = new System.Windows.Forms.Label();
            this.acceptableAspectError = new System.Windows.Forms.NumericUpDown();
            this.acceptableAspectErrorLabel = new System.Windows.Forms.Label();
            this.resetDialogs = new System.Windows.Forms.Button();
            this.configSourceDetector = new System.Windows.Forms.Button();
            this.chkboxUseAdvancedTooltips = new System.Windows.Forms.CheckBox();
            this.openProgressWindow = new System.Windows.Forms.CheckBox();
            this.deleteIntermediateFiles = new System.Windows.Forms.CheckBox();
            this.deleteAbortedOutput = new System.Windows.Forms.CheckBox();
            this.deleteCompletedJobs = new System.Windows.Forms.CheckBox();
            this.openScript = new System.Windows.Forms.CheckBox();
            this.autostartQueue = new System.Windows.Forms.CheckBox();
            this.priority = new System.Windows.Forms.ComboBox();
            this.priorityLabel = new System.Windows.Forms.Label();
            this.openExecutableDialog = new System.Windows.Forms.OpenFileDialog();
            this.openFolderDialog = new System.Windows.Forms.FolderBrowserDialog();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.gbDefaultOutput = new System.Windows.Forms.GroupBox();
            this.clearDefaultOutputDir = new System.Windows.Forms.Button();
            this.defaultOutputDir = new MeGUI.FileBar();
            this.tabPage3 = new System.Windows.Forms.TabPage();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.txt_httpproxyport = new System.Windows.Forms.TextBox();
            this.txt_httpproxypwd = new System.Windows.Forms.TextBox();
            this.txt_httpproxyuid = new System.Windows.Forms.TextBox();
            this.txt_httpproxyaddress = new System.Windows.Forms.TextBox();
            this.label21 = new System.Windows.Forms.Label();
            this.label20 = new System.Windows.Forms.Label();
            this.label19 = new System.Windows.Forms.Label();
            this.label18 = new System.Windows.Forms.Label();
            this.cbx_usehttpproxy = new System.Windows.Forms.CheckBox();
            this.gbVideoPreview = new System.Windows.Forms.GroupBox();
            this.cbAddTimePos = new System.Windows.Forms.CheckBox();
            this.chAlwaysOnTop = new System.Windows.Forms.CheckBox();
            this.autoUpdateGroupBox = new System.Windows.Forms.GroupBox();
            this.backupfiles = new System.Windows.Forms.CheckBox();
            this.label14 = new System.Windows.Forms.Label();
            this.maxServersToTry = new System.Windows.Forms.NumericUpDown();
            this.configureServersButton = new System.Windows.Forms.Button();
            this.useAutoUpdateCheckbox = new System.Windows.Forms.CheckBox();
            this.outputExtensions = new System.Windows.Forms.GroupBox();
            this.videoExtension = new System.Windows.Forms.TextBox();
            this.label11 = new System.Windows.Forms.Label();
            this.label12 = new System.Windows.Forms.Label();
            this.audioExtension = new System.Windows.Forms.TextBox();
            this.autoModeGroupbox = new System.Windows.Forms.GroupBox();
            this.configAutoEncodeDefaults = new System.Windows.Forms.Button();
            this.keep2ndPassLogFile = new System.Windows.Forms.CheckBox();
            this.keep2ndPassOutput = new System.Windows.Forms.CheckBox();
            this.label13 = new System.Windows.Forms.Label();
            this.nbPasses = new System.Windows.Forms.NumericUpDown();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.groupBox6 = new System.Windows.Forms.GroupBox();
            this.checkBox1 = new System.Windows.Forms.CheckBox();
            this.groupBox5 = new System.Windows.Forms.GroupBox();
            this.textBox2 = new System.Windows.Forms.TextBox();
            this.button2 = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.btnClearMP4TempDirectory = new System.Windows.Forms.Button();
            this.tempDirMP4 = new MeGUI.FileBar();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.besplit = new System.Windows.Forms.Label();
            this.textBox8 = new System.Windows.Forms.TextBox();
            this.button8 = new System.Windows.Forms.Button();
            this.vobGroupBox = new System.Windows.Forms.GroupBox();
            this.btnCUVIDServer = new System.Windows.Forms.CheckBox();
            this.defaultLanguage2 = new System.Windows.Forms.ComboBox();
            this.defaultAudioTrack2Label = new System.Windows.Forms.Label();
            this.defaultLanguage1 = new System.Windows.Forms.ComboBox();
            this.defaultAudioTrack1Label = new System.Windows.Forms.Label();
            this.percentLabel = new System.Windows.Forms.Label();
            this.forceFilmPercentage = new System.Windows.Forms.NumericUpDown();
            this.autoForceFilm = new System.Windows.Forms.CheckBox();
            this.audioExtLabel = new System.Windows.Forms.Label();
            this.videoExtLabel = new System.Windows.Forms.Label();
            this.autoEncodeDefaultsButton = new System.Windows.Forms.Button();
            this.helpButton1 = new MeGUI.core.gui.HelpButton();
            groupBox1 = new System.Windows.Forms.GroupBox();
            groupBox1.SuspendLayout();
            this.otherGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.audiosamplesperupdate)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.acceptableFPSError)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.acceptableAspectError)).BeginInit();
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.gbDefaultOutput.SuspendLayout();
            this.tabPage3.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.gbVideoPreview.SuspendLayout();
            this.autoUpdateGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.maxServersToTry)).BeginInit();
            this.outputExtensions.SuspendLayout();
            this.autoModeGroupbox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nbPasses)).BeginInit();
            this.tabPage2.SuspendLayout();
            this.groupBox6.SuspendLayout();
            this.groupBox5.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.vobGroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.forceFilmPercentage)).BeginInit();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            groupBox1.Controls.Add(this.command);
            groupBox1.Controls.Add(this.runCommand);
            groupBox1.Controls.Add(this.shutdown);
            groupBox1.Controls.Add(this.donothing);
            groupBox1.Location = new System.Drawing.Point(4, 187);
            groupBox1.Name = "groupBox1";
            groupBox1.Size = new System.Drawing.Size(217, 117);
            groupBox1.TabIndex = 2;
            groupBox1.TabStop = false;
            groupBox1.Text = "After encoding";
            // 
            // command
            // 
            this.command.Enabled = false;
            this.command.Location = new System.Drawing.Point(10, 89);
            this.command.Name = "command";
            this.command.Size = new System.Drawing.Size(197, 21);
            this.command.TabIndex = 3;
            // 
            // runCommand
            // 
            this.runCommand.AutoSize = true;
            this.runCommand.Location = new System.Drawing.Point(11, 66);
            this.runCommand.Name = "runCommand";
            this.runCommand.Size = new System.Drawing.Size(96, 17);
            this.runCommand.TabIndex = 2;
            this.runCommand.Text = "Run command:";
            this.runCommand.UseVisualStyleBackColor = true;
            this.runCommand.CheckedChanged += new System.EventHandler(this.runCommand_CheckedChanged);
            // 
            // shutdown
            // 
            this.shutdown.AutoSize = true;
            this.shutdown.Location = new System.Drawing.Point(11, 43);
            this.shutdown.Name = "shutdown";
            this.shutdown.Size = new System.Drawing.Size(73, 17);
            this.shutdown.TabIndex = 1;
            this.shutdown.Text = "Shutdown";
            this.shutdown.UseVisualStyleBackColor = true;
            // 
            // donothing
            // 
            this.donothing.AutoSize = true;
            this.donothing.Checked = true;
            this.donothing.Location = new System.Drawing.Point(11, 20);
            this.donothing.Name = "donothing";
            this.donothing.Size = new System.Drawing.Size(77, 17);
            this.donothing.TabIndex = 0;
            this.donothing.TabStop = true;
            this.donothing.Text = "Do nothing";
            this.donothing.UseVisualStyleBackColor = true;
            // 
            // saveButton
            // 
            this.saveButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.saveButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.saveButton.Location = new System.Drawing.Point(359, 418);
            this.saveButton.Name = "saveButton";
            this.saveButton.Size = new System.Drawing.Size(48, 23);
            this.saveButton.TabIndex = 2;
            this.saveButton.Text = "Save";
            // 
            // cancelButton
            // 
            this.cancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.cancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancelButton.Location = new System.Drawing.Point(430, 418);
            this.cancelButton.Name = "cancelButton";
            this.cancelButton.Size = new System.Drawing.Size(48, 23);
            this.cancelButton.TabIndex = 3;
            this.cancelButton.Text = "Cancel";
            // 
            // otherGroupBox
            // 
            this.otherGroupBox.Controls.Add(this.forcerawavcuse);
            this.otherGroupBox.Controls.Add(this.audiosamplesperupdate);
            this.otherGroupBox.Controls.Add(this.label6);
            this.otherGroupBox.Controls.Add(this.acceptableFPSError);
            this.otherGroupBox.Controls.Add(this.label15);
            this.otherGroupBox.Controls.Add(this.acceptableAspectError);
            this.otherGroupBox.Controls.Add(this.acceptableAspectErrorLabel);
            this.otherGroupBox.Controls.Add(this.resetDialogs);
            this.otherGroupBox.Controls.Add(this.configSourceDetector);
            this.otherGroupBox.Controls.Add(this.chkboxUseAdvancedTooltips);
            this.otherGroupBox.Controls.Add(this.openProgressWindow);
            this.otherGroupBox.Controls.Add(this.deleteIntermediateFiles);
            this.otherGroupBox.Controls.Add(this.deleteAbortedOutput);
            this.otherGroupBox.Controls.Add(this.deleteCompletedJobs);
            this.otherGroupBox.Controls.Add(this.openScript);
            this.otherGroupBox.Controls.Add(this.autostartQueue);
            this.otherGroupBox.Controls.Add(this.priority);
            this.otherGroupBox.Controls.Add(this.priorityLabel);
            this.otherGroupBox.Location = new System.Drawing.Point(2, 6);
            this.otherGroupBox.Name = "otherGroupBox";
            this.otherGroupBox.Size = new System.Drawing.Size(467, 293);
            this.otherGroupBox.TabIndex = 1;
            this.otherGroupBox.TabStop = false;
            this.otherGroupBox.Text = "Other";
            // 
            // forcerawavcuse
            // 
            this.forcerawavcuse.Location = new System.Drawing.Point(13, 184);
            this.forcerawavcuse.Name = "forcerawavcuse";
            this.forcerawavcuse.Size = new System.Drawing.Size(258, 17);
            this.forcerawavcuse.TabIndex = 18;
            this.forcerawavcuse.Text = "Force Video File Extension for QT compatibility";
            this.forcerawavcuse.UseVisualStyleBackColor = true;
            // 
            // audiosamplesperupdate
            // 
            this.audiosamplesperupdate.Location = new System.Drawing.Point(357, 17);
            this.audiosamplesperupdate.Maximum = new decimal(new int[] {
            100000000,
            0,
            0,
            0});
            this.audiosamplesperupdate.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.audiosamplesperupdate.Name = "audiosamplesperupdate";
            this.audiosamplesperupdate.Size = new System.Drawing.Size(95, 21);
            this.audiosamplesperupdate.TabIndex = 3;
            this.audiosamplesperupdate.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // label6
            // 
            this.label6.Location = new System.Drawing.Point(249, 17);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(102, 42);
            this.label6.TabIndex = 2;
            this.label6.Text = "Samples between audio progress updates";
            // 
            // acceptableFPSError
            // 
            this.acceptableFPSError.DecimalPlaces = 3;
            this.acceptableFPSError.Increment = new decimal(new int[] {
            1,
            0,
            0,
            196608});
            this.acceptableFPSError.Location = new System.Drawing.Point(150, 70);
            this.acceptableFPSError.Maximum = new decimal(new int[] {
            5,
            0,
            0,
            0});
            this.acceptableFPSError.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            196608});
            this.acceptableFPSError.Name = "acceptableFPSError";
            this.acceptableFPSError.Size = new System.Drawing.Size(79, 21);
            this.acceptableFPSError.TabIndex = 7;
            this.acceptableFPSError.Value = new decimal(new int[] {
            1,
            0,
            0,
            196608});
            // 
            // label15
            // 
            this.label15.Location = new System.Drawing.Point(9, 70);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(130, 32);
            this.label15.TabIndex = 6;
            this.label15.Text = "Acceptable FPS rounding error (bitrate calculator)";
            // 
            // acceptableAspectError
            // 
            this.acceptableAspectError.Location = new System.Drawing.Point(175, 43);
            this.acceptableAspectError.Maximum = new decimal(new int[] {
            5,
            0,
            0,
            0});
            this.acceptableAspectError.Name = "acceptableAspectError";
            this.acceptableAspectError.Size = new System.Drawing.Size(54, 21);
            this.acceptableAspectError.TabIndex = 5;
            this.acceptableAspectError.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // acceptableAspectErrorLabel
            // 
            this.acceptableAspectErrorLabel.AutoSize = true;
            this.acceptableAspectErrorLabel.Location = new System.Drawing.Point(8, 45);
            this.acceptableAspectErrorLabel.Name = "acceptableAspectErrorLabel";
            this.acceptableAspectErrorLabel.Size = new System.Drawing.Size(145, 13);
            this.acceptableAspectErrorLabel.TabIndex = 4;
            this.acceptableAspectErrorLabel.Text = "Acceptable Aspect Error (%)";
            // 
            // resetDialogs
            // 
            this.resetDialogs.Location = new System.Drawing.Point(13, 236);
            this.resetDialogs.Name = "resetDialogs";
            this.resetDialogs.Size = new System.Drawing.Size(149, 23);
            this.resetDialogs.TabIndex = 16;
            this.resetDialogs.Text = "Reset All Dialogs";
            this.resetDialogs.UseVisualStyleBackColor = true;
            this.resetDialogs.Click += new System.EventHandler(this.resetDialogs_Click);
            // 
            // configSourceDetector
            // 
            this.configSourceDetector.Location = new System.Drawing.Point(298, 236);
            this.configSourceDetector.Name = "configSourceDetector";
            this.configSourceDetector.Size = new System.Drawing.Size(154, 23);
            this.configSourceDetector.TabIndex = 17;
            this.configSourceDetector.Text = "Configure Source Detector";
            this.configSourceDetector.UseVisualStyleBackColor = true;
            this.configSourceDetector.Click += new System.EventHandler(this.configSourceDetector_Click);
            // 
            // chkboxUseAdvancedTooltips
            // 
            this.chkboxUseAdvancedTooltips.Location = new System.Drawing.Point(13, 115);
            this.chkboxUseAdvancedTooltips.Name = "chkboxUseAdvancedTooltips";
            this.chkboxUseAdvancedTooltips.Size = new System.Drawing.Size(152, 17);
            this.chkboxUseAdvancedTooltips.TabIndex = 8;
            this.chkboxUseAdvancedTooltips.Text = "Use Advanced ToolTips";
            // 
            // openProgressWindow
            // 
            this.openProgressWindow.Checked = true;
            this.openProgressWindow.CheckState = System.Windows.Forms.CheckState.Checked;
            this.openProgressWindow.Location = new System.Drawing.Point(300, 184);
            this.openProgressWindow.Name = "openProgressWindow";
            this.openProgressWindow.Size = new System.Drawing.Size(144, 17);
            this.openProgressWindow.TabIndex = 15;
            this.openProgressWindow.Text = "Open Progress Window";
            // 
            // deleteIntermediateFiles
            // 
            this.deleteIntermediateFiles.Location = new System.Drawing.Point(300, 161);
            this.deleteIntermediateFiles.Name = "deleteIntermediateFiles";
            this.deleteIntermediateFiles.Size = new System.Drawing.Size(152, 17);
            this.deleteIntermediateFiles.TabIndex = 13;
            this.deleteIntermediateFiles.Text = "Delete intermediate files";
            // 
            // deleteAbortedOutput
            // 
            this.deleteAbortedOutput.Location = new System.Drawing.Point(13, 161);
            this.deleteAbortedOutput.Name = "deleteAbortedOutput";
            this.deleteAbortedOutput.Size = new System.Drawing.Size(184, 17);
            this.deleteAbortedOutput.TabIndex = 12;
            this.deleteAbortedOutput.Text = "Delete Output of aborted jobs";
            // 
            // deleteCompletedJobs
            // 
            this.deleteCompletedJobs.Location = new System.Drawing.Point(300, 138);
            this.deleteCompletedJobs.Name = "deleteCompletedJobs";
            this.deleteCompletedJobs.Size = new System.Drawing.Size(144, 17);
            this.deleteCompletedJobs.TabIndex = 11;
            this.deleteCompletedJobs.Text = "Delete completed Jobs";
            // 
            // openScript
            // 
            this.openScript.Location = new System.Drawing.Point(13, 138);
            this.openScript.Name = "openScript";
            this.openScript.Size = new System.Drawing.Size(248, 17);
            this.openScript.TabIndex = 10;
            this.openScript.Text = "Open Preview after AviSynth script selection";
            // 
            // autostartQueue
            // 
            this.autostartQueue.Location = new System.Drawing.Point(300, 115);
            this.autostartQueue.Name = "autostartQueue";
            this.autostartQueue.Size = new System.Drawing.Size(112, 17);
            this.autostartQueue.TabIndex = 9;
            this.autostartQueue.Text = "Autostart Queue";
            // 
            // priority
            // 
            this.priority.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.priority.Items.AddRange(new object[] {
            "Low",
            "Below Normal",
            "Normal",
            "Above Normal",
            "High"});
            this.priority.Location = new System.Drawing.Point(150, 16);
            this.priority.Name = "priority";
            this.priority.Size = new System.Drawing.Size(80, 21);
            this.priority.TabIndex = 1;
            // 
            // priorityLabel
            // 
            this.priorityLabel.Location = new System.Drawing.Point(8, 19);
            this.priorityLabel.Name = "priorityLabel";
            this.priorityLabel.Size = new System.Drawing.Size(88, 13);
            this.priorityLabel.TabIndex = 0;
            this.priorityLabel.Text = "Default Priority";
            this.priorityLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage3);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Top;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(483, 413);
            this.tabControl1.TabIndex = 0;
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.gbDefaultOutput);
            this.tabPage1.Controls.Add(this.otherGroupBox);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(475, 387);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Main";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // gbDefaultOutput
            // 
            this.gbDefaultOutput.Controls.Add(this.clearDefaultOutputDir);
            this.gbDefaultOutput.Controls.Add(this.defaultOutputDir);
            this.gbDefaultOutput.Location = new System.Drawing.Point(2, 305);
            this.gbDefaultOutput.Name = "gbDefaultOutput";
            this.gbDefaultOutput.Size = new System.Drawing.Size(467, 74);
            this.gbDefaultOutput.TabIndex = 7;
            this.gbDefaultOutput.TabStop = false;
            this.gbDefaultOutput.Text = "Default Output Directory";
            // 
            // clearDefaultOutputDir
            // 
            this.clearDefaultOutputDir.Location = new System.Drawing.Point(430, 29);
            this.clearDefaultOutputDir.Name = "clearDefaultOutputDir";
            this.clearDefaultOutputDir.Size = new System.Drawing.Size(24, 26);
            this.clearDefaultOutputDir.TabIndex = 41;
            this.clearDefaultOutputDir.Text = "x";
            this.clearDefaultOutputDir.Click += new System.EventHandler(this.clearDefaultOutputDir_Click);
            // 
            // defaultOutputDir
            // 
            this.defaultOutputDir.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.defaultOutputDir.Filename = "";
            this.defaultOutputDir.Filter = null;
            this.defaultOutputDir.FilterIndex = 0;
            this.defaultOutputDir.FolderMode = true;
            this.defaultOutputDir.Location = new System.Drawing.Point(12, 29);
            this.defaultOutputDir.Name = "defaultOutputDir";
            this.defaultOutputDir.ReadOnly = true;
            this.defaultOutputDir.SaveMode = false;
            this.defaultOutputDir.Size = new System.Drawing.Size(417, 26);
            this.defaultOutputDir.TabIndex = 40;
            this.defaultOutputDir.Title = null;
            // 
            // tabPage3
            // 
            this.tabPage3.Controls.Add(this.groupBox2);
            this.tabPage3.Controls.Add(this.gbVideoPreview);
            this.tabPage3.Controls.Add(groupBox1);
            this.tabPage3.Controls.Add(this.autoUpdateGroupBox);
            this.tabPage3.Controls.Add(this.outputExtensions);
            this.tabPage3.Controls.Add(this.autoModeGroupbox);
            this.tabPage3.Location = new System.Drawing.Point(4, 22);
            this.tabPage3.Name = "tabPage3";
            this.tabPage3.Size = new System.Drawing.Size(475, 387);
            this.tabPage3.TabIndex = 2;
            this.tabPage3.Text = "Extra config";
            this.tabPage3.UseVisualStyleBackColor = true;
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.txt_httpproxyport);
            this.groupBox2.Controls.Add(this.txt_httpproxypwd);
            this.groupBox2.Controls.Add(this.txt_httpproxyuid);
            this.groupBox2.Controls.Add(this.txt_httpproxyaddress);
            this.groupBox2.Controls.Add(this.label21);
            this.groupBox2.Controls.Add(this.label20);
            this.groupBox2.Controls.Add(this.label19);
            this.groupBox2.Controls.Add(this.label18);
            this.groupBox2.Controls.Add(this.cbx_usehttpproxy);
            this.groupBox2.Location = new System.Drawing.Point(227, 187);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(240, 191);
            this.groupBox2.TabIndex = 5;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Auto Update Http Proxy:";
            // 
            // txt_httpproxyport
            // 
            this.txt_httpproxyport.Enabled = false;
            this.txt_httpproxyport.Location = new System.Drawing.Point(191, 43);
            this.txt_httpproxyport.Name = "txt_httpproxyport";
            this.txt_httpproxyport.Size = new System.Drawing.Size(43, 21);
            this.txt_httpproxyport.TabIndex = 8;
            // 
            // txt_httpproxypwd
            // 
            this.txt_httpproxypwd.Enabled = false;
            this.txt_httpproxypwd.Location = new System.Drawing.Point(55, 99);
            this.txt_httpproxypwd.Name = "txt_httpproxypwd";
            this.txt_httpproxypwd.PasswordChar = '*';
            this.txt_httpproxypwd.Size = new System.Drawing.Size(179, 21);
            this.txt_httpproxypwd.TabIndex = 7;
            // 
            // txt_httpproxyuid
            // 
            this.txt_httpproxyuid.Enabled = false;
            this.txt_httpproxyuid.Location = new System.Drawing.Point(55, 72);
            this.txt_httpproxyuid.Name = "txt_httpproxyuid";
            this.txt_httpproxyuid.Size = new System.Drawing.Size(179, 21);
            this.txt_httpproxyuid.TabIndex = 6;
            // 
            // txt_httpproxyaddress
            // 
            this.txt_httpproxyaddress.Enabled = false;
            this.txt_httpproxyaddress.Location = new System.Drawing.Point(55, 43);
            this.txt_httpproxyaddress.Name = "txt_httpproxyaddress";
            this.txt_httpproxyaddress.Size = new System.Drawing.Size(103, 21);
            this.txt_httpproxyaddress.TabIndex = 5;
            // 
            // label21
            // 
            this.label21.AutoSize = true;
            this.label21.Location = new System.Drawing.Point(6, 102);
            this.label21.Name = "label21";
            this.label21.Size = new System.Drawing.Size(31, 13);
            this.label21.TabIndex = 4;
            this.label21.Text = "Pwd:";
            // 
            // label20
            // 
            this.label20.AutoSize = true;
            this.label20.Location = new System.Drawing.Point(6, 75);
            this.label20.Name = "label20";
            this.label20.Size = new System.Drawing.Size(36, 13);
            this.label20.TabIndex = 3;
            this.label20.Text = "Login:";
            // 
            // label19
            // 
            this.label19.AutoSize = true;
            this.label19.Location = new System.Drawing.Point(164, 45);
            this.label19.Name = "label19";
            this.label19.Size = new System.Drawing.Size(31, 13);
            this.label19.TabIndex = 2;
            this.label19.Text = "Port:";
            // 
            // label18
            // 
            this.label18.AutoSize = true;
            this.label18.Location = new System.Drawing.Point(6, 45);
            this.label18.Name = "label18";
            this.label18.Size = new System.Drawing.Size(43, 13);
            this.label18.TabIndex = 1;
            this.label18.Text = "Server:";
            // 
            // cbx_usehttpproxy
            // 
            this.cbx_usehttpproxy.AutoSize = true;
            this.cbx_usehttpproxy.Location = new System.Drawing.Point(9, 21);
            this.cbx_usehttpproxy.Name = "cbx_usehttpproxy";
            this.cbx_usehttpproxy.Size = new System.Drawing.Size(75, 17);
            this.cbx_usehttpproxy.TabIndex = 0;
            this.cbx_usehttpproxy.Text = "Use Proxy";
            this.cbx_usehttpproxy.UseVisualStyleBackColor = true;
            this.cbx_usehttpproxy.CheckedChanged += new System.EventHandler(this.cbx_usehttpproxy_CheckedChanged);
            // 
            // gbVideoPreview
            // 
            this.gbVideoPreview.Controls.Add(this.cbAddTimePos);
            this.gbVideoPreview.Controls.Add(this.chAlwaysOnTop);
            this.gbVideoPreview.Location = new System.Drawing.Point(4, 309);
            this.gbVideoPreview.Name = "gbVideoPreview";
            this.gbVideoPreview.Size = new System.Drawing.Size(217, 69);
            this.gbVideoPreview.TabIndex = 4;
            this.gbVideoPreview.TabStop = false;
            this.gbVideoPreview.Text = "Video Preview";
            // 
            // cbAddTimePos
            // 
            this.cbAddTimePos.AutoSize = true;
            this.cbAddTimePos.Location = new System.Drawing.Point(8, 40);
            this.cbAddTimePos.Name = "cbAddTimePos";
            this.cbAddTimePos.Size = new System.Drawing.Size(110, 17);
            this.cbAddTimePos.TabIndex = 1;
            this.cbAddTimePos.Text = "Add Time Position";
            this.cbAddTimePos.UseVisualStyleBackColor = true;
            // 
            // chAlwaysOnTop
            // 
            this.chAlwaysOnTop.AutoSize = true;
            this.chAlwaysOnTop.Location = new System.Drawing.Point(8, 17);
            this.chAlwaysOnTop.Name = "chAlwaysOnTop";
            this.chAlwaysOnTop.Size = new System.Drawing.Size(169, 17);
            this.chAlwaysOnTop.TabIndex = 0;
            this.chAlwaysOnTop.Text = "Set the Form \"Always on Top\"";
            this.chAlwaysOnTop.UseVisualStyleBackColor = true;
            // 
            // autoUpdateGroupBox
            // 
            this.autoUpdateGroupBox.Controls.Add(this.backupfiles);
            this.autoUpdateGroupBox.Controls.Add(this.label14);
            this.autoUpdateGroupBox.Controls.Add(this.maxServersToTry);
            this.autoUpdateGroupBox.Controls.Add(this.configureServersButton);
            this.autoUpdateGroupBox.Controls.Add(this.useAutoUpdateCheckbox);
            this.autoUpdateGroupBox.Location = new System.Drawing.Point(227, 82);
            this.autoUpdateGroupBox.Name = "autoUpdateGroupBox";
            this.autoUpdateGroupBox.Size = new System.Drawing.Size(240, 99);
            this.autoUpdateGroupBox.TabIndex = 3;
            this.autoUpdateGroupBox.TabStop = false;
            this.autoUpdateGroupBox.Text = "Auto Update";
            // 
            // backupfiles
            // 
            this.backupfiles.AutoSize = true;
            this.backupfiles.Checked = true;
            this.backupfiles.CheckState = System.Windows.Forms.CheckState.Checked;
            this.backupfiles.Location = new System.Drawing.Point(9, 76);
            this.backupfiles.Name = "backupfiles";
            this.backupfiles.Size = new System.Drawing.Size(187, 17);
            this.backupfiles.TabIndex = 4;
            this.backupfiles.Text = "Always backup files when needed";
            this.backupfiles.UseVisualStyleBackColor = true;
            this.backupfiles.CheckedChanged += new System.EventHandler(this.backupfiles_CheckedChanged);
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Location = new System.Drawing.Point(6, 53);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(152, 13);
            this.label14.TabIndex = 2;
            this.label14.Text = "Max number of servers to try:";
            // 
            // maxServersToTry
            // 
            this.maxServersToTry.Location = new System.Drawing.Point(190, 51);
            this.maxServersToTry.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.maxServersToTry.Name = "maxServersToTry";
            this.maxServersToTry.Size = new System.Drawing.Size(44, 21);
            this.maxServersToTry.TabIndex = 3;
            this.maxServersToTry.Value = new decimal(new int[] {
            5,
            0,
            0,
            0});
            // 
            // configureServersButton
            // 
            this.configureServersButton.AutoSize = true;
            this.configureServersButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.configureServersButton.Location = new System.Drawing.Point(119, 18);
            this.configureServersButton.Name = "configureServersButton";
            this.configureServersButton.Size = new System.Drawing.Size(115, 23);
            this.configureServersButton.TabIndex = 1;
            this.configureServersButton.Text = "Configure servers...";
            this.configureServersButton.UseVisualStyleBackColor = true;
            this.configureServersButton.Click += new System.EventHandler(this.configureServersButton_Click);
            // 
            // useAutoUpdateCheckbox
            // 
            this.useAutoUpdateCheckbox.AutoSize = true;
            this.useAutoUpdateCheckbox.Location = new System.Drawing.Point(9, 22);
            this.useAutoUpdateCheckbox.Name = "useAutoUpdateCheckbox";
            this.useAutoUpdateCheckbox.Size = new System.Drawing.Size(105, 17);
            this.useAutoUpdateCheckbox.TabIndex = 0;
            this.useAutoUpdateCheckbox.Text = "Use AutoUpdate";
            this.useAutoUpdateCheckbox.UseVisualStyleBackColor = true;
            // 
            // outputExtensions
            // 
            this.outputExtensions.Controls.Add(this.videoExtension);
            this.outputExtensions.Controls.Add(this.label11);
            this.outputExtensions.Controls.Add(this.label12);
            this.outputExtensions.Controls.Add(this.audioExtension);
            this.outputExtensions.Location = new System.Drawing.Point(3, 82);
            this.outputExtensions.Name = "outputExtensions";
            this.outputExtensions.Size = new System.Drawing.Size(218, 99);
            this.outputExtensions.TabIndex = 1;
            this.outputExtensions.TabStop = false;
            this.outputExtensions.Text = "Optional output extensions";
            // 
            // videoExtension
            // 
            this.videoExtension.Location = new System.Drawing.Point(11, 20);
            this.videoExtension.Name = "videoExtension";
            this.videoExtension.Size = new System.Drawing.Size(120, 21);
            this.videoExtension.TabIndex = 0;
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(137, 51);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(34, 13);
            this.label11.TabIndex = 3;
            this.label11.Text = "Audio";
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Location = new System.Drawing.Point(137, 23);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(33, 13);
            this.label12.TabIndex = 1;
            this.label12.Text = "Video";
            // 
            // audioExtension
            // 
            this.audioExtension.Location = new System.Drawing.Point(11, 48);
            this.audioExtension.Name = "audioExtension";
            this.audioExtension.Size = new System.Drawing.Size(120, 21);
            this.audioExtension.TabIndex = 2;
            // 
            // autoModeGroupbox
            // 
            this.autoModeGroupbox.Controls.Add(this.configAutoEncodeDefaults);
            this.autoModeGroupbox.Controls.Add(this.keep2ndPassLogFile);
            this.autoModeGroupbox.Controls.Add(this.keep2ndPassOutput);
            this.autoModeGroupbox.Controls.Add(this.label13);
            this.autoModeGroupbox.Controls.Add(this.nbPasses);
            this.autoModeGroupbox.Location = new System.Drawing.Point(4, 3);
            this.autoModeGroupbox.Name = "autoModeGroupbox";
            this.autoModeGroupbox.Size = new System.Drawing.Size(463, 73);
            this.autoModeGroupbox.TabIndex = 0;
            this.autoModeGroupbox.TabStop = false;
            this.autoModeGroupbox.Text = "Automated Encoding";
            // 
            // configAutoEncodeDefaults
            // 
            this.configAutoEncodeDefaults.AutoSize = true;
            this.configAutoEncodeDefaults.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.configAutoEncodeDefaults.Location = new System.Drawing.Point(11, 43);
            this.configAutoEncodeDefaults.Name = "configAutoEncodeDefaults";
            this.configAutoEncodeDefaults.Size = new System.Drawing.Size(179, 23);
            this.configAutoEncodeDefaults.TabIndex = 5;
            this.configAutoEncodeDefaults.Text = "Configure AutoEncode defaults...";
            this.configAutoEncodeDefaults.UseVisualStyleBackColor = true;
            this.configAutoEncodeDefaults.Click += new System.EventHandler(this.autoEncodeDefaultsButton_Click);
            // 
            // keep2ndPassLogFile
            // 
            this.keep2ndPassLogFile.AutoSize = true;
            this.keep2ndPassLogFile.Checked = true;
            this.keep2ndPassLogFile.CheckState = System.Windows.Forms.CheckState.Checked;
            this.keep2ndPassLogFile.Location = new System.Drawing.Point(232, 22);
            this.keep2ndPassLogFile.Name = "keep2ndPassLogFile";
            this.keep2ndPassLogFile.Size = new System.Drawing.Size(176, 17);
            this.keep2ndPassLogFile.TabIndex = 4;
            this.keep2ndPassLogFile.Text = "Overwrite Stats File in 3rd Pass";
            this.keep2ndPassLogFile.UseVisualStyleBackColor = true;
            // 
            // keep2ndPassOutput
            // 
            this.keep2ndPassOutput.AutoSize = true;
            this.keep2ndPassOutput.Checked = true;
            this.keep2ndPassOutput.CheckState = System.Windows.Forms.CheckState.Checked;
            this.keep2ndPassOutput.Location = new System.Drawing.Point(232, 47);
            this.keep2ndPassOutput.Name = "keep2ndPassOutput";
            this.keep2ndPassOutput.Size = new System.Drawing.Size(207, 17);
            this.keep2ndPassOutput.TabIndex = 3;
            this.keep2ndPassOutput.Text = "Keep 2nd pass Output in 3 pass mode";
            this.keep2ndPassOutput.UseVisualStyleBackColor = true;
            // 
            // label13
            // 
            this.label13.Location = new System.Drawing.Point(11, 22);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(100, 13);
            this.label13.TabIndex = 0;
            this.label13.Text = "Number of passes";
            // 
            // nbPasses
            // 
            this.nbPasses.Location = new System.Drawing.Point(117, 20);
            this.nbPasses.Maximum = new decimal(new int[] {
            3,
            0,
            0,
            0});
            this.nbPasses.Minimum = new decimal(new int[] {
            2,
            0,
            0,
            0});
            this.nbPasses.Name = "nbPasses";
            this.nbPasses.Size = new System.Drawing.Size(40, 21);
            this.nbPasses.TabIndex = 1;
            this.nbPasses.Value = new decimal(new int[] {
            2,
            0,
            0,
            0});
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.groupBox6);
            this.tabPage2.Controls.Add(this.groupBox5);
            this.tabPage2.Controls.Add(this.groupBox4);
            this.tabPage2.Controls.Add(this.groupBox3);
            this.tabPage2.Controls.Add(this.vobGroupBox);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(475, 387);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "External Program Settings";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // groupBox6
            // 
            this.groupBox6.Controls.Add(this.checkBox1);
            this.groupBox6.Location = new System.Drawing.Point(4, 311);
            this.groupBox6.Name = "groupBox6";
            this.groupBox6.Size = new System.Drawing.Size(467, 70);
            this.groupBox6.TabIndex = 33;
            this.groupBox6.TabStop = false;
            this.groupBox6.Text = " Misc ";
            // 
            // checkBox1
            // 
            this.checkBox1.AutoSize = true;
            this.checkBox1.Location = new System.Drawing.Point(11, 31);
            this.checkBox1.Name = "checkBox1";
            this.checkBox1.Size = new System.Drawing.Size(177, 17);
            this.checkBox1.TabIndex = 27;
            this.checkBox1.Text = "I\'m using OggEnc2 v2.8 or later";
            this.checkBox1.UseVisualStyleBackColor = true;
            // 
            // groupBox5
            // 
            this.groupBox5.Controls.Add(this.textBox2);
            this.groupBox5.Controls.Add(this.button2);
            this.groupBox5.Controls.Add(this.label2);
            this.groupBox5.Location = new System.Drawing.Point(4, 25);
            this.groupBox5.Name = "groupBox5";
            this.groupBox5.Size = new System.Drawing.Size(467, 54);
            this.groupBox5.TabIndex = 32;
            this.groupBox5.TabStop = false;
            this.groupBox5.Text = " NeroAacEnc ";
            // 
            // textBox2
            // 
            this.textBox2.Location = new System.Drawing.Point(84, 20);
            this.textBox2.Name = "textBox2";
            this.textBox2.ReadOnly = true;
            this.textBox2.Size = new System.Drawing.Size(315, 21);
            this.textBox2.TabIndex = 25;
            this.textBox2.Text = "neroAacEnc.exe";
            // 
            // button2
            // 
            this.button2.Location = new System.Drawing.Point(405, 18);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(24, 23);
            this.button2.TabIndex = 26;
            this.button2.Text = "...";
            this.button2.Click += new System.EventHandler(this.button2_Click);
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(8, 23);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(80, 13);
            this.label2.TabIndex = 24;
            this.label2.Text = "neroAacEnc";
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.btnClearMP4TempDirectory);
            this.groupBox4.Controls.Add(this.tempDirMP4);
            this.groupBox4.Location = new System.Drawing.Point(4, 228);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(467, 77);
            this.groupBox4.TabIndex = 31;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Temp Directory for MP4 Muxer";
            // 
            // btnClearMP4TempDirectory
            // 
            this.btnClearMP4TempDirectory.Location = new System.Drawing.Point(437, 33);
            this.btnClearMP4TempDirectory.Name = "btnClearMP4TempDirectory";
            this.btnClearMP4TempDirectory.Size = new System.Drawing.Size(24, 26);
            this.btnClearMP4TempDirectory.TabIndex = 42;
            this.btnClearMP4TempDirectory.Text = "x";
            this.btnClearMP4TempDirectory.Click += new System.EventHandler(this.btnClearMP4TempDirectory_Click);
            // 
            // tempDirMP4
            // 
            this.tempDirMP4.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.tempDirMP4.Filename = "";
            this.tempDirMP4.Filter = null;
            this.tempDirMP4.FilterIndex = 0;
            this.tempDirMP4.FolderMode = true;
            this.tempDirMP4.Location = new System.Drawing.Point(7, 33);
            this.tempDirMP4.Name = "tempDirMP4";
            this.tempDirMP4.ReadOnly = true;
            this.tempDirMP4.SaveMode = false;
            this.tempDirMP4.Size = new System.Drawing.Size(424, 26);
            this.tempDirMP4.TabIndex = 41;
            this.tempDirMP4.Title = null;
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.besplit);
            this.groupBox3.Controls.Add(this.textBox8);
            this.groupBox3.Controls.Add(this.button8);
            this.groupBox3.Location = new System.Drawing.Point(4, 85);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(467, 54);
            this.groupBox3.TabIndex = 30;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = " BeSplit ";
            // 
            // besplit
            // 
            this.besplit.Location = new System.Drawing.Point(8, 23);
            this.besplit.Name = "besplit";
            this.besplit.Size = new System.Drawing.Size(68, 13);
            this.besplit.TabIndex = 21;
            this.besplit.Text = "BeSplit";
            // 
            // textBox8
            // 
            this.textBox8.Location = new System.Drawing.Point(84, 20);
            this.textBox8.Name = "textBox8";
            this.textBox8.ReadOnly = true;
            this.textBox8.Size = new System.Drawing.Size(315, 21);
            this.textBox8.TabIndex = 22;
            this.textBox8.Text = "besplit.exe";
            // 
            // button8
            // 
            this.button8.Location = new System.Drawing.Point(405, 20);
            this.button8.Name = "button8";
            this.button8.Size = new System.Drawing.Size(24, 23);
            this.button8.TabIndex = 23;
            this.button8.Text = "...";
            this.button8.Click += new System.EventHandler(this.button8_Click);
            // 
            // vobGroupBox
            // 
            this.vobGroupBox.Controls.Add(this.btnCUVIDServer);
            this.vobGroupBox.Controls.Add(this.defaultLanguage2);
            this.vobGroupBox.Controls.Add(this.defaultAudioTrack2Label);
            this.vobGroupBox.Controls.Add(this.defaultLanguage1);
            this.vobGroupBox.Controls.Add(this.defaultAudioTrack1Label);
            this.vobGroupBox.Controls.Add(this.percentLabel);
            this.vobGroupBox.Controls.Add(this.forceFilmPercentage);
            this.vobGroupBox.Controls.Add(this.autoForceFilm);
            this.vobGroupBox.Location = new System.Drawing.Point(4, 145);
            this.vobGroupBox.Name = "vobGroupBox";
            this.vobGroupBox.Size = new System.Drawing.Size(467, 77);
            this.vobGroupBox.TabIndex = 29;
            this.vobGroupBox.TabStop = false;
            this.vobGroupBox.Text = " DGIndex ";
            // 
            // btnCUVIDServer
            // 
            this.btnCUVIDServer.AutoSize = true;
            this.btnCUVIDServer.Location = new System.Drawing.Point(239, 49);
            this.btnCUVIDServer.Name = "btnCUVIDServer";
            this.btnCUVIDServer.Size = new System.Drawing.Size(110, 17);
            this.btnCUVIDServer.TabIndex = 39;
            this.btnCUVIDServer.Text = "Use CUVIDServer";
            this.btnCUVIDServer.UseVisualStyleBackColor = true;
            // 
            // defaultLanguage2
            // 
            this.defaultLanguage2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.defaultLanguage2.Location = new System.Drawing.Point(126, 47);
            this.defaultLanguage2.Name = "defaultLanguage2";
            this.defaultLanguage2.Size = new System.Drawing.Size(104, 21);
            this.defaultLanguage2.TabIndex = 6;
            // 
            // defaultAudioTrack2Label
            // 
            this.defaultAudioTrack2Label.Location = new System.Drawing.Point(8, 50);
            this.defaultAudioTrack2Label.Name = "defaultAudioTrack2Label";
            this.defaultAudioTrack2Label.Size = new System.Drawing.Size(112, 13);
            this.defaultAudioTrack2Label.TabIndex = 5;
            this.defaultAudioTrack2Label.Text = "Default Audio Track 2";
            this.defaultAudioTrack2Label.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // defaultLanguage1
            // 
            this.defaultLanguage1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.defaultLanguage1.Location = new System.Drawing.Point(126, 20);
            this.defaultLanguage1.Name = "defaultLanguage1";
            this.defaultLanguage1.Size = new System.Drawing.Size(104, 21);
            this.defaultLanguage1.TabIndex = 1;
            // 
            // defaultAudioTrack1Label
            // 
            this.defaultAudioTrack1Label.Location = new System.Drawing.Point(8, 24);
            this.defaultAudioTrack1Label.Name = "defaultAudioTrack1Label";
            this.defaultAudioTrack1Label.Size = new System.Drawing.Size(112, 13);
            this.defaultAudioTrack1Label.TabIndex = 0;
            this.defaultAudioTrack1Label.Text = "Default Audio Track 1";
            this.defaultAudioTrack1Label.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // percentLabel
            // 
            this.percentLabel.Location = new System.Drawing.Point(411, 24);
            this.percentLabel.Margin = new System.Windows.Forms.Padding(3);
            this.percentLabel.Name = "percentLabel";
            this.percentLabel.Size = new System.Drawing.Size(50, 13);
            this.percentLabel.TabIndex = 4;
            this.percentLabel.Text = "Percent";
            this.percentLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // forceFilmPercentage
            // 
            this.forceFilmPercentage.Location = new System.Drawing.Point(365, 20);
            this.forceFilmPercentage.Name = "forceFilmPercentage";
            this.forceFilmPercentage.Size = new System.Drawing.Size(40, 21);
            this.forceFilmPercentage.TabIndex = 3;
            this.forceFilmPercentage.Value = new decimal(new int[] {
            95,
            0,
            0,
            0});
            // 
            // autoForceFilm
            // 
            this.autoForceFilm.Location = new System.Drawing.Point(239, 22);
            this.autoForceFilm.Name = "autoForceFilm";
            this.autoForceFilm.Size = new System.Drawing.Size(120, 17);
            this.autoForceFilm.TabIndex = 2;
            this.autoForceFilm.Text = "Auto Force Film at";
            // 
            // audioExtLabel
            // 
            this.audioExtLabel.AutoSize = true;
            this.audioExtLabel.Location = new System.Drawing.Point(137, 51);
            this.audioExtLabel.Name = "audioExtLabel";
            this.audioExtLabel.Size = new System.Drawing.Size(34, 13);
            this.audioExtLabel.TabIndex = 24;
            this.audioExtLabel.Text = "Audio";
            // 
            // videoExtLabel
            // 
            this.videoExtLabel.AutoSize = true;
            this.videoExtLabel.Location = new System.Drawing.Point(137, 24);
            this.videoExtLabel.Name = "videoExtLabel";
            this.videoExtLabel.Size = new System.Drawing.Size(34, 13);
            this.videoExtLabel.TabIndex = 23;
            this.videoExtLabel.Text = "Video";
            // 
            // autoEncodeDefaultsButton
            // 
            this.autoEncodeDefaultsButton.Location = new System.Drawing.Point(11, 51);
            this.autoEncodeDefaultsButton.Name = "autoEncodeDefaultsButton";
            this.autoEncodeDefaultsButton.Size = new System.Drawing.Size(114, 23);
            this.autoEncodeDefaultsButton.TabIndex = 4;
            this.autoEncodeDefaultsButton.Text = "Configure Defaults";
            this.autoEncodeDefaultsButton.UseVisualStyleBackColor = true;
            // 
            // helpButton1
            // 
            this.helpButton1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.helpButton1.ArticleName = "Settings window";
            this.helpButton1.AutoSize = true;
            this.helpButton1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.helpButton1.Location = new System.Drawing.Point(21, 418);
            this.helpButton1.Name = "helpButton1";
            this.helpButton1.Size = new System.Drawing.Size(38, 23);
            this.helpButton1.TabIndex = 1;
            // 
            // SettingsForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.ClientSize = new System.Drawing.Size(483, 446);
            this.Controls.Add(this.helpButton1);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.cancelButton);
            this.Controls.Add(this.saveButton);
            this.DataBindings.Add(new System.Windows.Forms.Binding("Location", global::MeGUI.Properties.Settings.Default, "SettingsFormSize", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Location = global::MeGUI.Properties.Settings.Default.SettingsFormSize;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "SettingsForm";
            this.ShowInTaskbar = false;
            this.Text = "Settings";
            groupBox1.ResumeLayout(false);
            groupBox1.PerformLayout();
            this.otherGroupBox.ResumeLayout(false);
            this.otherGroupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.audiosamplesperupdate)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.acceptableFPSError)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.acceptableAspectError)).EndInit();
            this.tabControl1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.gbDefaultOutput.ResumeLayout(false);
            this.tabPage3.ResumeLayout(false);
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.gbVideoPreview.ResumeLayout(false);
            this.gbVideoPreview.PerformLayout();
            this.autoUpdateGroupBox.ResumeLayout(false);
            this.autoUpdateGroupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.maxServersToTry)).EndInit();
            this.outputExtensions.ResumeLayout(false);
            this.outputExtensions.PerformLayout();
            this.autoModeGroupbox.ResumeLayout(false);
            this.autoModeGroupbox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.nbPasses)).EndInit();
            this.tabPage2.ResumeLayout(false);
            this.groupBox6.ResumeLayout(false);
            this.groupBox6.PerformLayout();
            this.groupBox5.ResumeLayout(false);
            this.groupBox5.PerformLayout();
            this.groupBox4.ResumeLayout(false);
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.vobGroupBox.ResumeLayout(false);
            this.vobGroupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.forceFilmPercentage)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

		}
        
        
        private bool selectExe(string exe)
        {
            openExecutableDialog.Filter = exe + " executable|" + exe +"*.exe|Any executable|*.exe";
            openExecutableDialog.DefaultExt = "exe";
            openExecutableDialog.FileName = exe + ".exe";
            openExecutableDialog.Title = "Select " + exe + " executable";
            return openExecutableDialog.ShowDialog() == DialogResult.OK;
        }
        private void configSourceDetector_Click(object sender, EventArgs e)
        {
            SourceDetectorConfigWindow sdcWindow = new SourceDetectorConfigWindow();
            sdcWindow.Settings = sdSettings;
            if (sdcWindow.ShowDialog() == DialogResult.OK)
                sdSettings = sdcWindow.Settings;
        }
        private void button2_Click(object sender, EventArgs e)
        {
            if (selectExe("neroAacEnc"))
            {
                textBox2.Text = openExecutableDialog.FileName;
            }
        }
  
        private void resetDialogs_Click(object sender, EventArgs e)
        {
            internalSettings.DialogSettings = new DialogSettings();
            MessageBox.Show(this, "Successfully reset all dialogs", "Success", MessageBoxButtons.OK, MessageBoxIcon.None);
        }

        private void button8_Click(object sender, EventArgs e)
        {
            if (selectExe("besplit"))
            {
                textBox8.Text = openExecutableDialog.FileName;
            }
        }
        private void runCommand_CheckedChanged(object sender, EventArgs e)
        {
            command.Enabled = runCommand.Checked;
        }
        /// <summary>
        /// launches the autoencode default settings window
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void autoEncodeDefaultsButton_Click(object sender, EventArgs e)
        {
            using (AutoEncodeDefaults aed = new AutoEncodeDefaults())
            {
                aed.Settings = this.autoEncodeDefaults;
                DialogResult dr = aed.ShowDialog();
                if (dr == DialogResult.OK)
                {
                    this.autoEncodeDefaults = aed.Settings;
                }
            }
        }

        /// <summary>
        /// Launches configuration of auto-update servers
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void configureServersButton_Click(object sender, EventArgs e)
        {
            using (MeGUI.core.gui.AutoUpdateServerConfigWindow w = new MeGUI.core.gui.AutoUpdateServerConfigWindow())
            {
                w.ServerList = internalSettings.AutoUpdateServerLists;
                w.ServerListIndex = internalSettings.AutoUpdateServerSubList;
                if (w.ShowDialog() == DialogResult.OK)
                {
                    internalSettings.AutoUpdateServerLists = w.ServerList;
                    internalSettings.AutoUpdateServerSubList = w.ServerListIndex;
                }
            }
        }

        private void cbx_usehttpproxy_CheckedChanged(object sender, EventArgs e)
        {
            txt_httpproxyaddress.Enabled = cbx_usehttpproxy.Checked;
            txt_httpproxyport.Enabled = cbx_usehttpproxy.Checked;
            txt_httpproxyuid.Enabled = cbx_usehttpproxy.Checked;
            txt_httpproxypwd.Enabled = cbx_usehttpproxy.Checked;
        }

        private void clearDefaultOutputDir_Click(object sender, EventArgs e)
        {
            defaultOutputDir.Filename = "";
        }

		
		
		public MeGUISettings Settings
		{
			get 
			{
                MeGUISettings settings = internalSettings;
                settings.AudioSamplesPerUpdate = (ulong)audiosamplesperupdate.Value;
                settings.AcceptableFPSError = acceptableFPSError.Value; 
                settings.MaxServersToTry = (int)maxServersToTry.Value;
                settings.AutoUpdate = useAutoUpdateCheckbox.Checked;
                settings.AcceptableAspectErrorPercent = (int)acceptableAspectError.Value;
                settings.SourceDetectorSettings = sdSettings;
                settings.NeroAacEncPath = textBox2.Text;
                settings.VideoExtension = videoExtension.Text;
                settings.AudioExtension = audioExtension.Text;
                settings.UseAdvancedTooltips = chkboxUseAdvancedTooltips.Checked;
                settings.BeSplitPath = textBox8.Text;
				settings.DefaultLanguage1 = defaultLanguage1.Text;
				settings.DefaultLanguage2 = defaultLanguage2.Text;
				settings.AutoForceFilm = autoForceFilm.Checked;
				settings.ForceFilmThreshold = forceFilmPercentage.Value;
				settings.DefaultPriority = (ProcessPriority)priority.SelectedIndex;
				settings.AutoStartQueue = this.autostartQueue.Checked;
                if (donothing.Checked) settings.AfterEncoding = AfterEncoding.DoNothing;
                else if (shutdown.Checked) settings.AfterEncoding = AfterEncoding.Shutdown;
                else
                {
                    settings.AfterEncoding = AfterEncoding.RunCommand;
                    settings.AfterEncodingCommand = command.Text;
                }
                settings.AutoOpenScript = openScript.Checked;
				settings.DeleteCompletedJobs = deleteCompletedJobs.Checked;
				settings.DeleteIntermediateFiles = deleteIntermediateFiles.Checked;
				settings.DeleteAbortedOutput = deleteAbortedOutput.Checked;
				settings.OpenProgressWindow = openProgressWindow.Checked;
				settings.Keep2of3passOutput = keep2ndPassOutput.Checked;
				settings.OverwriteStats = keep2ndPassLogFile.Checked;
				settings.NbPasses = (int)nbPasses.Value;
                settings.FreshOggEnc2 = checkBox1.Checked;
                settings.AedSettings = this.autoEncodeDefaults;
                settings.AlwaysOnTop = chAlwaysOnTop.Checked;
                settings.UseHttpProxy = cbx_usehttpproxy.Checked;
                settings.HttpProxyAddress = txt_httpproxyaddress.Text;
                settings.HttpProxyPort = txt_httpproxyport.Text;
                settings.HttpProxyUid = txt_httpproxyuid.Text;
                settings.HttpProxyPwd = txt_httpproxypwd.Text;
                settings.DefaultOutputDir = defaultOutputDir.Filename;
                settings.TempDirMP4 = tempDirMP4.Filename;
                settings.AddTimePosition = cbAddTimePos.Checked;
                settings.AlwaysBackUpFiles = backupfiles.Checked;
                settings.ForceRawAVCExtension = forcerawavcuse.Checked;
                settings.UseCUVIDserver = btnCUVIDServer.Checked;
				return settings;
			}
			set
			{
                internalSettings = value;
                MeGUISettings settings = value;
                audiosamplesperupdate.Value = settings.AudioSamplesPerUpdate;
                acceptableFPSError.Value = settings.AcceptableFPSError;
                maxServersToTry.Value = settings.MaxServersToTry;
                useAutoUpdateCheckbox.Checked = settings.AutoUpdate;
                acceptableAspectError.Value = (decimal)settings.AcceptableAspectErrorPercent;
                textBox2.Text = settings.NeroAacEncPath;
                textBox8.Text = settings.BeSplitPath;
                sdSettings = settings.SourceDetectorSettings;
                chkboxUseAdvancedTooltips.Checked = settings.UseAdvancedTooltips;
                videoExtension.Text = settings.VideoExtension;
                audioExtension.Text = settings.AudioExtension;
				int index = this.defaultLanguage1.Items.IndexOf(settings.DefaultLanguage1);
				if (index != -1)
					defaultLanguage1.SelectedIndex = index;
				index = defaultLanguage2.Items.IndexOf(settings.DefaultLanguage2);
				if (index != -1)
					defaultLanguage2.SelectedIndex = index;
				autoForceFilm.Checked = settings.AutoForceFilm;
				forceFilmPercentage.Value = settings.ForceFilmThreshold;
				priority.SelectedIndex = (int)settings.DefaultPriority;
				autostartQueue.Checked = settings.AutoStartQueue;
                donothing.Checked = settings.AfterEncoding == AfterEncoding.DoNothing;
                shutdown.Checked = settings.AfterEncoding == AfterEncoding.Shutdown;
                runCommand.Checked = settings.AfterEncoding == AfterEncoding.RunCommand;
                command.Text = settings.AfterEncodingCommand;
				deleteCompletedJobs.Checked = settings.DeleteCompletedJobs;
                openScript.Checked = settings.AutoOpenScript;
				deleteIntermediateFiles.Checked = settings.DeleteIntermediateFiles;
				deleteAbortedOutput.Checked = settings.DeleteAbortedOutput;
				openProgressWindow.Checked = settings.OpenProgressWindow;
				keep2ndPassOutput.Checked = settings.Keep2of3passOutput;
				keep2ndPassLogFile.Checked = settings.OverwriteStats;
				nbPasses.Value = (decimal)settings.NbPasses;
                checkBox1.Checked = settings.FreshOggEnc2;
                this.autoEncodeDefaults = settings.AedSettings;
                chAlwaysOnTop.Checked = settings.AlwaysOnTop;
                cbx_usehttpproxy.Checked = settings.UseHttpProxy;
                txt_httpproxyaddress.Text = settings.HttpProxyAddress;
                txt_httpproxyport.Text = settings.HttpProxyPort;
                txt_httpproxyuid.Text = settings.HttpProxyUid;
                txt_httpproxypwd.Text = settings.HttpProxyPwd;
                defaultOutputDir.Filename = settings.DefaultOutputDir;
                tempDirMP4.Filename = settings.TempDirMP4;
                cbAddTimePos.Checked = settings.AddTimePosition;
                backupfiles.Checked = settings.AlwaysBackUpFiles;
                forcerawavcuse.Checked = settings.ForceRawAVCExtension;
                btnCUVIDServer.Checked = settings.UseCUVIDserver;
			}
		}
		

        private void backupfiles_CheckedChanged(object sender, EventArgs e)
        {
            if (!backupfiles.Checked)
            {
                string meguiToolsFolder = Application.ExecutablePath.Substring(0, Application.ExecutablePath.LastIndexOf('\\')) + "\\tools\\";
                string meguiAvisynthFolder = MainForm.Instance.Settings.AvisynthPluginsPath + "\\";
                if (Directory.Exists(meguiToolsFolder))
                {
                    try
                    {  // remove all backup files found
                        Array.ForEach(Directory.GetFiles(meguiToolsFolder, "*.backup", SearchOption.AllDirectories),
                          delegate(string path) { File.Delete(path); });
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }
                }
                if (Directory.Exists(meguiAvisynthFolder))
                {
                    try
                    {  // remove all backup files found
                        Array.ForEach(Directory.GetFiles(meguiAvisynthFolder, "*.backup", SearchOption.AllDirectories),
                          delegate(string path) { File.Delete(path); });
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }
                }
            }
        }

        private void btnClearMP4TempDirectory_Click(object sender, EventArgs e)
        {
            tempDirMP4.Filename = "";
        }
	}
}
