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
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Text;
using System.Windows.Forms;

using MeGUI.core.util;

namespace MeGUI
{

	/// <summary>
	/// Summary description for Vobinput.
	/// </summary>
	public class FileIndexerWindow : System.Windows.Forms.Form
	{ 
		
        private D2VIndexJob lastJob = null;
        
        private enum IndexType
        {
            D2V, DGA, DGI, FFMS
        };
        private IndexType IndexerUsed = IndexType.D2V;
        private bool bDGIAvailable = false;

        private string strVideoCodec = "";
        private string strVideoScanType = "";
        private string strContainerFormat = "";
        private List<AudioTrackInfo> audioTracks = new List<AudioTrackInfo>();

        private bool dialogMode = false; // $%£%$^>*"%$%%$#{"!!! Affects the public behaviour!
		private bool configured = false;
		private MainForm mainForm;
		private VideoUtil vUtil;
		private JobUtil jobUtil;

		private System.Windows.Forms.GroupBox gbAudio;
        private System.Windows.Forms.GroupBox gbOutput;
		private System.Windows.Forms.Label inputLabel;
		private System.Windows.Forms.TextBox output;
		private System.Windows.Forms.Label outputLabel;
        private System.Windows.Forms.SaveFileDialog saveProjectDialog;
		private System.Windows.Forms.Button pickOutputButton;
		private System.Windows.Forms.GroupBox gbInput;
        private System.Windows.Forms.RadioButton demuxTracks;
        private System.Windows.Forms.RadioButton demuxNoAudiotracks;
		private System.Windows.Forms.Button queueButton;
		private System.Windows.Forms.CheckBox loadOnComplete;
		private System.Windows.Forms.CheckBox closeOnQueue;
        private MeGUI.core.gui.HelpButton helpButton1;
        private CheckedListBox AudioTracks;
        private RadioButton demuxAll;
        private FileBar input;
        private CheckBox demuxVideo;
        private GroupBox gbIndexer;
        private RadioButton btnDGI;
        private RadioButton btnD2V;
        private RadioButton btnFFMS;
        private RadioButton btnDGA;
        private GroupBox gbFileInformation;
        private Label lblContainer;
        private Label lblScanType;
        private Label lblCodec;
        private TextBox txtCodecInformation;
        private TextBox txtContainerInformation;
        private TextBox txtScanTypeInformation;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
		
		
		public void setConfig(string input, string projectName, int demuxType,
            bool showCloseOnQueue, bool closeOnQueue, bool loadOnComplete, bool updateMode)
		{
			openVideo(input);
			if (!string.IsNullOrEmpty(projectName))
                this.output.Text = projectName;
			if (demuxType == 0)
				demuxNoAudiotracks.Checked = true;
			else
				demuxTracks.Checked = true;
			this.loadOnComplete.Checked = loadOnComplete;
            if (updateMode)
            {
                this.dialogMode = true;
                queueButton.Text = "Update";
            }
            else
                this.dialogMode = false;
			checkIndexIO();
            if (!showCloseOnQueue)
            {
                this.closeOnQueue.Hide();
                this.Controls.Remove(this.closeOnQueue);
            }
            this.closeOnQueue.Checked = closeOnQueue;

		}
		
		public FileIndexerWindow(MainForm mainForm)
		{
			InitializeComponent();
			this.mainForm = mainForm;
			this.vUtil = new VideoUtil(mainForm);
			this.jobUtil = new JobUtil(mainForm);
            CheckDGIIndexer();
		}

		public FileIndexerWindow(MainForm mainForm, string fileName): this(mainForm)
		{
            CheckDGIIndexer();
			openVideo(fileName);
		}

        public FileIndexerWindow(MainForm mainForm, string fileName, bool autoReturn) : this(mainForm, fileName)
        {
            CheckDGIIndexer();
            openVideo(fileName);
            this.loadOnComplete.Checked = true;
            this.closeOnQueue.Checked = true;
            checkIndexIO();
        }

        private void CheckDGIIndexer()
        {
            if (File.Exists(MainForm.Instance.Settings.DgnvIndexPath) &&
                File.Exists(Path.Combine(Path.GetDirectoryName(MainForm.Instance.Settings.DgnvIndexPath), "license.txt")))
            {
                input.Filter = "All DGAVCIndex supported files|*.264;*.h264;*.avc;*.m2t*;*.m2ts;*.mts;*.tp;*.ts;*.trp|All DGIndex supported files|*.vob;*.mpg;*.mpeg;*.m1v;*.m2v;*.mpv;*.tp;*.ts;*.trp;*.m2t;*.m2ts;*.pva;*.vro|All DGIndexNV supported files|*.264;*.h264;*.avc;*.m2v;*.mpv;*.vc1;*.mkv;*.vob;*.mpg;*.mpeg;*.m2t;*.m2ts;*.mts;*.tp;*.ts;*.trp|All FFMS Indexer supported files|*.mkv;*.avi;*.mp4;*.flv;*.wmv;*.ogm;*.vob;*.mpg;*.m2ts;*.ts|All supported files|*.mkv;*.avi;*.mp4;*.flv;*.wmv;*.ogm;*.264;*.h264;*.avc;*.m2t*;*.m2ts;*.mts;*.tp;*.ts;*.trp;*.vob;*.mpg;*.mpeg;*.m1v;*.m2v;*.mpv;*.pva;*.vro;*.vc1|All files|*.*";
                input.FilterIndex = 5;
                bDGIAvailable = true;
            }
            else
            {
                input.Filter = "All DGAVCIndex supported files|*.264;*.h264;*.avc;*.m2t*;*.m2ts;*.mts;*.tp;*.ts;*.trp|All DGIndex supported files|*.vob;*.mpg;*.mpeg;*.m1v;*.m2v;*.mpv;*.tp;*.ts;*.trp;*.m2t;*.m2ts;*.pva;*.vro|All FFMS Indexer supported files|*.mkv;*.avi;*.mp4;*.flv;*.wmv;*.ogm;*.vob;*.mpg;*.m2ts;*.ts|All supported files|*.mkv;*.avi;*.mp4;*.flv;*.wmv;*.ogm;*.264;*.h264;*.avc;*.m2t*;*.m2ts;*.mts;*.tp;*.ts;*.trp;*.vob;*.mpg;*.mpeg;*.m1v;*.m2v;*.mpv;*.pva;*.vro|All files|*.*";
                input.FilterIndex = 4;
                bDGIAvailable = false;
                btnDGI.Enabled = false;
            }
        }

        private void changeIndexer(IndexType dgType)
        {
            switch (dgType)
            {
                case IndexType.DGI:
                {
                    this.Text = "MeGUI - DGI Project Creator";
                    this.saveProjectDialog.Filter = "DGIndexNV project files|*.dgi";
                    if (this.demuxTracks.Checked)
                        this.demuxAll.Checked = true;
                    this.demuxTracks.Enabled = false;
                    this.gbOutput.Enabled = true;
                    this.gbAudio.Enabled = true;
                    IndexerUsed = IndexType.DGI;
                    btnDGI.Checked = true;
                    break;
                }
                case IndexType.DGA:
                {
                    this.Text = "MeGUI - D2V Project Creator";
                    this.saveProjectDialog.Filter = "DGAVCIndex project files|*.dga";
                    this.gbOutput.Enabled = true;
                    this.gbAudio.Enabled = true;
                    if (this.demuxTracks.Checked)
                        this.demuxAll.Checked = true;
                    this.demuxTracks.Enabled = false;
                    IndexerUsed = IndexType.DGA;
                    btnDGA.Checked = true;
                    break;
                }
                case IndexType.D2V:
                {
                    this.Text = "MeGUI - DGA Project Creator";
                    this.saveProjectDialog.Filter = "DGIndex project files|*.d2v";
                    this.demuxTracks.Enabled = true;
                    this.gbOutput.Enabled = true;
                    this.gbAudio.Enabled = true;
                    IndexerUsed = IndexType.D2V;
                    btnD2V.Checked = true;
                    break;
                }
                case IndexType.FFMS:
                {
                    this.Text = "MeGUI - FFMS Indexer";
                    this.saveProjectDialog.Filter = "FFMSIndex project files|*.ffindex";
                    this.gbOutput.Enabled = false;
                    this.gbAudio.Enabled = false;
                    this.demuxNoAudiotracks.Checked = true;
                    this.demuxVideo.Checked = false;
                    IndexerUsed = IndexType.FFMS;
                    btnFFMS.Checked = true;
                    break;
                }
            }
            setOutputFileName();
            recommendSettings();
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FileIndexerWindow));
            this.gbInput = new System.Windows.Forms.GroupBox();
            this.input = new MeGUI.FileBar();
            this.inputLabel = new System.Windows.Forms.Label();
            this.queueButton = new System.Windows.Forms.Button();
            this.loadOnComplete = new System.Windows.Forms.CheckBox();
            this.gbAudio = new System.Windows.Forms.GroupBox();
            this.demuxAll = new System.Windows.Forms.RadioButton();
            this.AudioTracks = new System.Windows.Forms.CheckedListBox();
            this.demuxNoAudiotracks = new System.Windows.Forms.RadioButton();
            this.demuxTracks = new System.Windows.Forms.RadioButton();
            this.gbOutput = new System.Windows.Forms.GroupBox();
            this.demuxVideo = new System.Windows.Forms.CheckBox();
            this.pickOutputButton = new System.Windows.Forms.Button();
            this.output = new System.Windows.Forms.TextBox();
            this.outputLabel = new System.Windows.Forms.Label();
            this.saveProjectDialog = new System.Windows.Forms.SaveFileDialog();
            this.closeOnQueue = new System.Windows.Forms.CheckBox();
            this.gbIndexer = new System.Windows.Forms.GroupBox();
            this.btnDGA = new System.Windows.Forms.RadioButton();
            this.btnFFMS = new System.Windows.Forms.RadioButton();
            this.btnD2V = new System.Windows.Forms.RadioButton();
            this.btnDGI = new System.Windows.Forms.RadioButton();
            this.gbFileInformation = new System.Windows.Forms.GroupBox();
            this.txtContainerInformation = new System.Windows.Forms.TextBox();
            this.txtScanTypeInformation = new System.Windows.Forms.TextBox();
            this.txtCodecInformation = new System.Windows.Forms.TextBox();
            this.lblScanType = new System.Windows.Forms.Label();
            this.lblCodec = new System.Windows.Forms.Label();
            this.lblContainer = new System.Windows.Forms.Label();
            this.helpButton1 = new MeGUI.core.gui.HelpButton();
            this.gbInput.SuspendLayout();
            this.gbAudio.SuspendLayout();
            this.gbOutput.SuspendLayout();
            this.gbIndexer.SuspendLayout();
            this.gbFileInformation.SuspendLayout();
            this.SuspendLayout();
            // 
            // gbInput
            // 
            this.gbInput.Controls.Add(this.input);
            this.gbInput.Controls.Add(this.inputLabel);
            this.gbInput.Location = new System.Drawing.Point(12, 6);
            this.gbInput.Name = "gbInput";
            this.gbInput.Size = new System.Drawing.Size(424, 50);
            this.gbInput.TabIndex = 0;
            this.gbInput.TabStop = false;
            this.gbInput.Text = " Input ";
            // 
            // input
            // 
            this.input.AllowDrop = true;
            this.input.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.input.Filename = "";
            this.input.Filter = "";
            this.input.FilterIndex = 0;
            this.input.FolderMode = false;
            this.input.Location = new System.Drawing.Point(77, 10);
            this.input.Name = "input";
            this.input.ReadOnly = true;
            this.input.SaveMode = false;
            this.input.Size = new System.Drawing.Size(329, 34);
            this.input.TabIndex = 4;
            this.input.Title = null;
            this.input.FileSelected += new MeGUI.FileBarEventHandler(this.input_FileSelected);
            // 
            // inputLabel
            // 
            this.inputLabel.Location = new System.Drawing.Point(9, 22);
            this.inputLabel.Name = "inputLabel";
            this.inputLabel.Size = new System.Drawing.Size(100, 13);
            this.inputLabel.TabIndex = 0;
            this.inputLabel.Text = "Input File";
            // 
            // queueButton
            // 
            this.queueButton.Location = new System.Drawing.Point(362, 395);
            this.queueButton.Name = "queueButton";
            this.queueButton.Size = new System.Drawing.Size(74, 23);
            this.queueButton.TabIndex = 10;
            this.queueButton.Text = "Queue";
            this.queueButton.Click += new System.EventHandler(this.queueButton_Click);
            // 
            // loadOnComplete
            // 
            this.loadOnComplete.Checked = true;
            this.loadOnComplete.CheckState = System.Windows.Forms.CheckState.Checked;
            this.loadOnComplete.Location = new System.Drawing.Point(91, 395);
            this.loadOnComplete.Name = "loadOnComplete";
            this.loadOnComplete.Size = new System.Drawing.Size(144, 24);
            this.loadOnComplete.TabIndex = 11;
            this.loadOnComplete.Text = "On completion load files";
            // 
            // gbAudio
            // 
            this.gbAudio.Controls.Add(this.demuxAll);
            this.gbAudio.Controls.Add(this.AudioTracks);
            this.gbAudio.Controls.Add(this.demuxNoAudiotracks);
            this.gbAudio.Controls.Add(this.demuxTracks);
            this.gbAudio.Enabled = false;
            this.gbAudio.Location = new System.Drawing.Point(12, 187);
            this.gbAudio.Name = "gbAudio";
            this.gbAudio.Size = new System.Drawing.Size(424, 125);
            this.gbAudio.TabIndex = 8;
            this.gbAudio.TabStop = false;
            this.gbAudio.Text = " Audio ";
            // 
            // demuxAll
            // 
            this.demuxAll.Location = new System.Drawing.Point(304, 20);
            this.demuxAll.Name = "demuxAll";
            this.demuxAll.Size = new System.Drawing.Size(106, 17);
            this.demuxAll.TabIndex = 15;
            this.demuxAll.TabStop = true;
            this.demuxAll.Text = "Demux All Tracks";
            this.demuxAll.UseVisualStyleBackColor = true;
            this.demuxAll.CheckedChanged += new System.EventHandler(this.rbtracks_CheckedChanged);
            // 
            // AudioTracks
            // 
            this.AudioTracks.CheckOnClick = true;
            this.AudioTracks.Enabled = false;
            this.AudioTracks.FormattingEnabled = true;
            this.AudioTracks.Location = new System.Drawing.Point(16, 43);
            this.AudioTracks.Name = "AudioTracks";
            this.AudioTracks.Size = new System.Drawing.Size(394, 68);
            this.AudioTracks.TabIndex = 14;
            // 
            // demuxNoAudiotracks
            // 
            this.demuxNoAudiotracks.Checked = true;
            this.demuxNoAudiotracks.Location = new System.Drawing.Point(19, 16);
            this.demuxNoAudiotracks.Name = "demuxNoAudiotracks";
            this.demuxNoAudiotracks.Size = new System.Drawing.Size(120, 24);
            this.demuxNoAudiotracks.TabIndex = 13;
            this.demuxNoAudiotracks.TabStop = true;
            this.demuxNoAudiotracks.Text = "No Audio demux";
            this.demuxNoAudiotracks.CheckedChanged += new System.EventHandler(this.rbtracks_CheckedChanged);
            // 
            // demuxTracks
            // 
            this.demuxTracks.Enabled = false;
            this.demuxTracks.Location = new System.Drawing.Point(155, 16);
            this.demuxTracks.Name = "demuxTracks";
            this.demuxTracks.Size = new System.Drawing.Size(120, 24);
            this.demuxTracks.TabIndex = 7;
            this.demuxTracks.Text = "Select Audio Tracks";
            this.demuxTracks.CheckedChanged += new System.EventHandler(this.rbtracks_CheckedChanged);
            // 
            // gbOutput
            // 
            this.gbOutput.Controls.Add(this.demuxVideo);
            this.gbOutput.Controls.Add(this.pickOutputButton);
            this.gbOutput.Controls.Add(this.output);
            this.gbOutput.Controls.Add(this.outputLabel);
            this.gbOutput.Enabled = false;
            this.gbOutput.Location = new System.Drawing.Point(12, 318);
            this.gbOutput.Name = "gbOutput";
            this.gbOutput.Size = new System.Drawing.Size(424, 69);
            this.gbOutput.TabIndex = 12;
            this.gbOutput.TabStop = false;
            this.gbOutput.Text = " Output ";
            // 
            // demuxVideo
            // 
            this.demuxVideo.AutoSize = true;
            this.demuxVideo.Location = new System.Drawing.Point(81, 44);
            this.demuxVideo.Name = "demuxVideo";
            this.demuxVideo.Size = new System.Drawing.Size(125, 17);
            this.demuxVideo.TabIndex = 6;
            this.demuxVideo.Text = "Demux Video Stream";
            this.demuxVideo.UseVisualStyleBackColor = true;
            // 
            // pickOutputButton
            // 
            this.pickOutputButton.Location = new System.Drawing.Point(380, 17);
            this.pickOutputButton.Name = "pickOutputButton";
            this.pickOutputButton.Size = new System.Drawing.Size(30, 23);
            this.pickOutputButton.TabIndex = 5;
            this.pickOutputButton.Text = "...";
            this.pickOutputButton.Click += new System.EventHandler(this.pickOutputButton_Click);
            // 
            // output
            // 
            this.output.Location = new System.Drawing.Point(81, 17);
            this.output.Name = "output";
            this.output.ReadOnly = true;
            this.output.Size = new System.Drawing.Size(289, 21);
            this.output.TabIndex = 4;
            // 
            // outputLabel
            // 
            this.outputLabel.Location = new System.Drawing.Point(11, 21);
            this.outputLabel.Name = "outputLabel";
            this.outputLabel.Size = new System.Drawing.Size(100, 13);
            this.outputLabel.TabIndex = 3;
            this.outputLabel.Text = "Output File";
            // 
            // saveProjectDialog
            // 
            this.saveProjectDialog.Filter = "DGIndex project files|*.d2v";
            this.saveProjectDialog.Title = "Pick a name for your DGIndex project";
            // 
            // closeOnQueue
            // 
            this.closeOnQueue.Checked = true;
            this.closeOnQueue.CheckState = System.Windows.Forms.CheckState.Checked;
            this.closeOnQueue.Location = new System.Drawing.Point(281, 395);
            this.closeOnQueue.Name = "closeOnQueue";
            this.closeOnQueue.Size = new System.Drawing.Size(72, 24);
            this.closeOnQueue.TabIndex = 13;
            this.closeOnQueue.Text = "and close";
            // 
            // gbIndexer
            // 
            this.gbIndexer.Controls.Add(this.btnDGA);
            this.gbIndexer.Controls.Add(this.btnFFMS);
            this.gbIndexer.Controls.Add(this.btnD2V);
            this.gbIndexer.Controls.Add(this.btnDGI);
            this.gbIndexer.Enabled = false;
            this.gbIndexer.Location = new System.Drawing.Point(12, 135);
            this.gbIndexer.Name = "gbIndexer";
            this.gbIndexer.Size = new System.Drawing.Size(424, 46);
            this.gbIndexer.TabIndex = 15;
            this.gbIndexer.TabStop = false;
            this.gbIndexer.Text = " File Indexer ";
            // 
            // btnDGA
            // 
            this.btnDGA.AutoSize = true;
            this.btnDGA.Location = new System.Drawing.Point(115, 19);
            this.btnDGA.Name = "btnDGA";
            this.btnDGA.Size = new System.Drawing.Size(87, 17);
            this.btnDGA.TabIndex = 3;
            this.btnDGA.TabStop = true;
            this.btnDGA.Text = "DGAVCIndex";
            this.btnDGA.UseVisualStyleBackColor = true;
            this.btnDGA.Click += new System.EventHandler(this.btnDGA_Click);
            // 
            // btnFFMS
            // 
            this.btnFFMS.AutoSize = true;
            this.btnFFMS.Location = new System.Drawing.Point(329, 19);
            this.btnFFMS.Name = "btnFFMS";
            this.btnFFMS.Size = new System.Drawing.Size(79, 17);
            this.btnFFMS.TabIndex = 2;
            this.btnFFMS.TabStop = true;
            this.btnFFMS.Text = "FFMSIndex";
            this.btnFFMS.UseVisualStyleBackColor = true;
            this.btnFFMS.Click += new System.EventHandler(this.btnFFMS_Click);
            // 
            // btnD2V
            // 
            this.btnD2V.AutoSize = true;
            this.btnD2V.Location = new System.Drawing.Point(12, 20);
            this.btnD2V.Name = "btnD2V";
            this.btnD2V.Size = new System.Drawing.Size(67, 17);
            this.btnD2V.TabIndex = 1;
            this.btnD2V.TabStop = true;
            this.btnD2V.Text = "DGIndex";
            this.btnD2V.UseVisualStyleBackColor = true;
            this.btnD2V.Click += new System.EventHandler(this.btnD2V_Click);
            // 
            // btnDGI
            // 
            this.btnDGI.AutoSize = true;
            this.btnDGI.Location = new System.Drawing.Point(229, 19);
            this.btnDGI.Name = "btnDGI";
            this.btnDGI.Size = new System.Drawing.Size(80, 17);
            this.btnDGI.TabIndex = 0;
            this.btnDGI.TabStop = true;
            this.btnDGI.Text = "DGIndexNV";
            this.btnDGI.UseVisualStyleBackColor = true;
            this.btnDGI.Click += new System.EventHandler(this.btnDGI_Click);
            // 
            // gbFileInformation
            // 
            this.gbFileInformation.Controls.Add(this.txtContainerInformation);
            this.gbFileInformation.Controls.Add(this.txtScanTypeInformation);
            this.gbFileInformation.Controls.Add(this.txtCodecInformation);
            this.gbFileInformation.Controls.Add(this.lblScanType);
            this.gbFileInformation.Controls.Add(this.lblCodec);
            this.gbFileInformation.Controls.Add(this.lblContainer);
            this.gbFileInformation.Enabled = false;
            this.gbFileInformation.Location = new System.Drawing.Point(12, 62);
            this.gbFileInformation.Name = "gbFileInformation";
            this.gbFileInformation.Size = new System.Drawing.Size(424, 67);
            this.gbFileInformation.TabIndex = 16;
            this.gbFileInformation.TabStop = false;
            this.gbFileInformation.Text = " File Information ";
            // 
            // txtContainerInformation
            // 
            this.txtContainerInformation.Enabled = false;
            this.txtContainerInformation.Location = new System.Drawing.Point(300, 34);
            this.txtContainerInformation.Name = "txtContainerInformation";
            this.txtContainerInformation.Size = new System.Drawing.Size(108, 21);
            this.txtContainerInformation.TabIndex = 5;
            // 
            // txtScanTypeInformation
            // 
            this.txtScanTypeInformation.Enabled = false;
            this.txtScanTypeInformation.Location = new System.Drawing.Point(153, 34);
            this.txtScanTypeInformation.Name = "txtScanTypeInformation";
            this.txtScanTypeInformation.Size = new System.Drawing.Size(108, 21);
            this.txtScanTypeInformation.TabIndex = 4;
            // 
            // txtCodecInformation
            // 
            this.txtCodecInformation.Enabled = false;
            this.txtCodecInformation.Location = new System.Drawing.Point(12, 34);
            this.txtCodecInformation.Name = "txtCodecInformation";
            this.txtCodecInformation.Size = new System.Drawing.Size(108, 21);
            this.txtCodecInformation.TabIndex = 3;
            // 
            // lblScanType
            // 
            this.lblScanType.AutoSize = true;
            this.lblScanType.Location = new System.Drawing.Point(150, 18);
            this.lblScanType.Name = "lblScanType";
            this.lblScanType.Size = new System.Drawing.Size(57, 13);
            this.lblScanType.TabIndex = 2;
            this.lblScanType.Text = "Scan Type";
            // 
            // lblCodec
            // 
            this.lblCodec.AutoSize = true;
            this.lblCodec.Location = new System.Drawing.Point(12, 18);
            this.lblCodec.Name = "lblCodec";
            this.lblCodec.Size = new System.Drawing.Size(37, 13);
            this.lblCodec.TabIndex = 1;
            this.lblCodec.Text = "Codec";
            // 
            // lblContainer
            // 
            this.lblContainer.AutoSize = true;
            this.lblContainer.Location = new System.Drawing.Point(297, 18);
            this.lblContainer.Name = "lblContainer";
            this.lblContainer.Size = new System.Drawing.Size(54, 13);
            this.lblContainer.TabIndex = 0;
            this.lblContainer.Text = "Container";
            // 
            // helpButton1
            // 
            this.helpButton1.ArticleName = "File Indexer window";
            this.helpButton1.AutoSize = true;
            this.helpButton1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.helpButton1.Location = new System.Drawing.Point(13, 394);
            this.helpButton1.Name = "helpButton1";
            this.helpButton1.Size = new System.Drawing.Size(38, 23);
            this.helpButton1.TabIndex = 14;
            // 
            // FileIndexerWindow
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.ClientSize = new System.Drawing.Size(444, 425);
            this.Controls.Add(this.gbFileInformation);
            this.Controls.Add(this.gbIndexer);
            this.Controls.Add(this.helpButton1);
            this.Controls.Add(this.closeOnQueue);
            this.Controls.Add(this.gbInput);
            this.Controls.Add(this.gbOutput);
            this.Controls.Add(this.loadOnComplete);
            this.Controls.Add(this.queueButton);
            this.Controls.Add(this.gbAudio);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "FileIndexerWindow";
            this.Text = "MeGUI";
            this.gbInput.ResumeLayout(false);
            this.gbAudio.ResumeLayout(false);
            this.gbOutput.ResumeLayout(false);
            this.gbOutput.PerformLayout();
            this.gbIndexer.ResumeLayout(false);
            this.gbIndexer.PerformLayout();
            this.gbFileInformation.ResumeLayout(false);
            this.gbFileInformation.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		
		
		private void pickOutputButton_Click(object sender, System.EventArgs e)
		{
			if (saveProjectDialog.ShowDialog() == DialogResult.OK)
			{
				output.Text = saveProjectDialog.FileName;
				checkIndexIO();
			}
		}

        private void input_FileSelected(FileBar sender, FileBarEventArgs args)
        {
			openVideo(input.Filename);
			checkIndexIO();
		}
		private void openVideo(string fileName)
		{           
            if (!VideoUtil.getMediaInformation(fileName, out strVideoCodec, out strVideoScanType, out strContainerFormat, out audioTracks))
                return;

            if (String.IsNullOrEmpty(strVideoCodec))
                txtCodecInformation.Text = "unknown";
            else
                txtCodecInformation.Text = strVideoCodec;
            if (String.IsNullOrEmpty(strContainerFormat))
                txtContainerInformation.Text = "unknown";
            else
                txtContainerInformation.Text = strContainerFormat;
            if (String.IsNullOrEmpty(strVideoScanType))
                txtScanTypeInformation.Text = "unknown";
            else
                txtScanTypeInformation.Text = strVideoScanType;

			if (input.Filename != fileName)
                input.Filename = fileName;

            gbIndexer.Enabled = true;
            gbFileInformation.Enabled = true;

            AudioTracks.Items.Clear();
            if (AudioTracks.Items.Count < 1)
            {
                foreach (AudioTrackInfo atrack in audioTracks)
                    AudioTracks.Items.Add(atrack);
            }

            switch (strVideoCodec.ToUpper())
            {
                case "AVC":
                {
                    btnD2V.Enabled = false;
                    btnFFMS.Enabled = true;
                    if (strContainerFormat.ToUpper().Equals("FLASH VIDEO") || strContainerFormat.ToUpper().Equals("MPEG-4"))
                    {
                        btnDGA.Enabled = false;
                        btnDGI.Enabled = false;
                        changeIndexer(IndexType.FFMS);
                    }
                    else
                    {
                        btnDGA.Enabled = true;
                        btnDGI.Enabled = true;
                        if (bDGIAvailable)
                            changeIndexer(IndexType.DGI);
                        else
                            changeIndexer(IndexType.DGA);
                    }
                    break;
                }
                case "VC-1":
                {
                    btnD2V.Enabled = false;
                    btnDGA.Enabled = false;
                    btnDGI.Enabled = true;
                    btnFFMS.Enabled = true;
                    if (bDGIAvailable)
                        changeIndexer(IndexType.DGI);
                    else
                        changeIndexer(IndexType.FFMS);
                    break;
                }
                case "MPEG-2 VIDEO":
                {
                    btnD2V.Enabled = true;
                    btnDGA.Enabled = false;
                    btnDGI.Enabled = true;
                    btnFFMS.Enabled = true;
                    if (bDGIAvailable)
                        changeIndexer(IndexType.DGI);
                    else
                        changeIndexer(IndexType.D2V);
                    break;
                }
                case "MPEG-1 VIDEO":
                {
                    btnD2V.Enabled = true;
                    btnDGA.Enabled = false;
                    btnDGI.Enabled = false;
                    btnFFMS.Enabled = true;
                    changeIndexer(IndexType.D2V);
                    break;
                }
                default:
                {
                    if (strContainerFormat.ToUpper().Equals("AVI") || strContainerFormat.ToUpper().Equals("FLASH VIDEO"))
                    {
                        btnD2V.Enabled = false;
                        btnDGA.Enabled = false;
                        btnDGI.Enabled = false;
                        btnFFMS.Enabled = true;
                        changeIndexer(IndexType.FFMS);
                    }
                    else
                    {
                        btnD2V.Enabled = true;
                        btnDGA.Enabled = true;
                        btnDGI.Enabled = true;
                        btnFFMS.Enabled = true;
                        changeIndexer(IndexType.FFMS);
                    }
                    break;
                }
            }

            setOutputFileName();
            recommendSettings();

            if (!bDGIAvailable)
                btnDGI.Enabled = false; 
		}

        /// <summary>
		/// recommend input settings based upon the input file
		/// </summary>
        private void recommendSettings()
        {
            if (AudioTracks.Items.Count > 0)
            {
                if (IndexerUsed == IndexType.D2V)
                {
                    if (strContainerFormat.Equals("MPEG-PS"))
                    {
                        demuxTracks.Checked = true;
                        demuxTracks.Enabled = true;
                        AudioTracks.Enabled = true;
                    }
                    else
                    {
                        AudioTracks.Enabled = false;
                    }

                }
                else
                {
                    AudioTracks.Enabled = false;
                    demuxTracks.Enabled = false;
                }
            }
            else
            {
                AudioTracks.Enabled = false;
                demuxNoAudiotracks.Checked = true;
            }

            if (IndexerUsed == IndexType.FFMS)
            {
                if (strVideoCodec.ToUpper().Equals("VC-1"))
                {
                    if (!strVideoScanType.ToUpper().Equals("PROGRESSIVE"))
                        MessageBox.Show("Interlaced VC-1 is not supported by FFMS2", "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                    if (!strContainerFormat.ToUpper().Equals("MATROSKA"))
                        MessageBox.Show("Please use a MKV container to index VC-1 files", "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                }
                else if (!strContainerFormat.ToUpper().Equals("MATROSKA") &&
                         !strContainerFormat.ToUpper().Equals("AVI") &&
                         !strContainerFormat.ToUpper().Equals("MPEG-4") &&
                         !strContainerFormat.ToUpper().Equals("FLASH VIDEO"))
                {
                    MessageBox.Show("Please use a MKV, AVI, MP4 or FLV container to index files with the FFMS2 indexer", "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                }
            }
        }

        /// <summary>
		/// sets the output file name
		/// </summary>
        private void setOutputFileName()
        {
            if (!String.IsNullOrEmpty(this.input.Filename))
            {
                if (IndexerUsed == IndexType.FFMS)
                {
                    this.output.Text = this.input.Filename + ".ffindex";
                }
                else
                {
                    string projectPath = "";
                    string fileNameNoPath = Path.GetFileName(this.input.Filename);
                    if (string.IsNullOrEmpty(projectPath = mainForm.Settings.DefaultOutputDir))
                        projectPath = Path.GetDirectoryName(this.input.Filename);
                    switch (IndexerUsed)
                    {
                        case IndexType.D2V: output.Text = Path.Combine(projectPath, Path.ChangeExtension(fileNameNoPath, ".d2v")); ; break;
                        case IndexType.DGA: output.Text = Path.Combine(projectPath, Path.ChangeExtension(fileNameNoPath, ".dga")); ; break;
                        case IndexType.DGI: output.Text = Path.Combine(projectPath, Path.ChangeExtension(fileNameNoPath, ".dgi")); ; break;
                    }
                }
            }   
        }

		/// <summary>
		/// creates a project
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void queueButton_Click(object sender, System.EventArgs e)
		{

            if (configured)
            {
                if (Drives.ableToWriteOnThisDrive(Path.GetPathRoot(output.Text)))
                {
                    if (!dialogMode)
                    {
                        switch (IndexerUsed)
                        {
                            case IndexType.D2V:
                            {
                                D2VIndexJob job = generateIndexJob();
                                lastJob = job;
                                mainForm.Jobs.addJobsToQueue(job);
                                if (this.closeOnQueue.Checked)
                                    this.Close();
                                break;
                            }
                            case IndexType.DGI:
                            {
                                DGIIndexJob job = generateDGNVIndexJob();
                                //lastJob = job;
                                mainForm.Jobs.addJobsToQueue(job);
                                if (this.closeOnQueue.Checked)
                                    this.Close();
                                break;
                            }
                            case IndexType.DGA:
                            {
                                DGAIndexJob job = generateDGAIndexJob();
                                //lastJob = job;
                                mainForm.Jobs.addJobsToQueue(job);
                                if (this.closeOnQueue.Checked)
                                    this.Close();
                                break;
                            }
                            case IndexType.FFMS:
                            {
                                FFMSIndexJob job = generateFFMSIndexJob();
                                //lastJob = job;
                                mainForm.Jobs.addJobsToQueue(job);
                                if (this.closeOnQueue.Checked)
                                    this.Close();
                                break;
                            }
                        }
                    }
                }
                else
                    MessageBox.Show("MeGUI cannot write on the disc " + Path.GetPathRoot(output.Text) + "\n" +
                                                    "Please, select another output path to save your project...", "Configuration Incomplete", MessageBoxButtons.OK, MessageBoxIcon.Warning);
            }
            else
                MessageBox.Show("You must select the input and output file to continue",
                       "Configuration incomplete", MessageBoxButtons.OK);
		}
		
		
		private void checkIndexIO()
		{
			configured = (!input.Filename.Equals("") && !output.Text.Equals(""));
			if (configured && dialogMode)
				queueButton.DialogResult = DialogResult.OK;
			else
                queueButton.DialogResult = DialogResult.None;
		}
		private D2VIndexJob generateIndexJob()
		{
			int demuxType = 0;
			if (demuxTracks.Checked)
				demuxType = 1;
            else if (demuxNoAudiotracks.Checked)
                demuxType = 0;
            else
                demuxType = 2;

            List<AudioTrackInfo> audioTracks = new List<AudioTrackInfo>();
            foreach (AudioTrackInfo ati in AudioTracks.CheckedItems)
            {
                audioTracks.Add(ati);
            }

            return new D2VIndexJob(this.input.Filename, this.output.Text, demuxType, audioTracks, null, loadOnComplete.Checked, demuxVideo.Checked);
		}
        private DGIIndexJob generateDGNVIndexJob()
        {
            int demuxType = 0;
            if (demuxTracks.Checked)
                demuxType = 1;
            else if (demuxNoAudiotracks.Checked)
                demuxType = 0;
            else
                demuxType = 2;

            List<AudioTrackInfo> audioTracks = new List<AudioTrackInfo>();
            foreach (AudioTrackInfo ati in AudioTracks.CheckedItems)
            {
                audioTracks.Add(ati);
            }

            return new DGIIndexJob(this.input.Filename, this.output.Text, demuxType, audioTracks, null, loadOnComplete.Checked, demuxVideo.Checked);
        }
        private DGAIndexJob generateDGAIndexJob()
        {
            int demuxType = 0;
            if (demuxTracks.Checked)
                demuxType = 1;
            else if (demuxNoAudiotracks.Checked)
                demuxType = 0;
            else
                demuxType = 2;

            List<AudioTrackInfo> audioTracks = new List<AudioTrackInfo>();
            foreach (AudioTrackInfo ati in AudioTracks.CheckedItems)
            {
                audioTracks.Add(ati);
            }

            return new DGAIndexJob(this.input.Filename, this.output.Text, demuxType, audioTracks, null, loadOnComplete.Checked, demuxVideo.Checked);
        }
        private FFMSIndexJob generateFFMSIndexJob()
        {
            return new FFMSIndexJob(this.input.Filename, loadOnComplete.Checked);
        }
		
		
		/// <summary>
		/// gets the index job created from the current configuration
		/// </summary>
		public D2VIndexJob Job
		{
			get {return generateIndexJob();}
		}
        
        public D2VIndexJob LastJob
        {
            get { return lastJob; }
            set { lastJob = value; }
        }
        public bool JobCreated
        {
            get { return lastJob != null; }
        }
        

        private void rbtracks_CheckedChanged(object sender, EventArgs e)
        {
            // Now defaults to starting with every track selected
            for (int i = 0; i < AudioTracks.Items.Count; i++)
                AudioTracks.SetItemChecked(i, !demuxNoAudiotracks.Checked);
            AudioTracks.Enabled = demuxTracks.Checked;
        }

        private void btnFFMS_Click(object sender, EventArgs e)
        {
            changeIndexer(IndexType.FFMS);
        }

        private void btnDGI_Click(object sender, EventArgs e)
        {
            changeIndexer(IndexType.DGI);
        }

        private void btnDGA_Click(object sender, EventArgs e)
        {
            changeIndexer(IndexType.DGA);
        }

        private void btnD2V_Click(object sender, EventArgs e)
        {
            changeIndexer(IndexType.D2V);
        }
    }

    public class D2VCreatorTool : MeGUI.core.plugins.interfaces.ITool
    {

        

        public string Name
        {
            get { return "File Indexer"; }
        }

        public void Run(MainForm info)
        {
            new FileIndexerWindow(info).Show();

        }

        public Shortcut[] Shortcuts
        {
            get { return new Shortcut[] { Shortcut.Ctrl2 }; }
        }

        

        

        public string ID
        {
            get { return "d2v_creator"; }
        }

        
    }

    public class d2vIndexJobPostProcessor
    {
        public static JobPostProcessor PostProcessor = new JobPostProcessor(postprocess, "D2V_postprocessor");
        private static LogItem postprocess(MainForm mainForm, Job ajob)
        {
            if (!(ajob is D2VIndexJob)) return null;
            D2VIndexJob job = (D2VIndexJob)ajob;
            if (job.PostprocessingProperties != null) return null;

            StringBuilder logBuilder = new StringBuilder();
            VideoUtil vUtil = new VideoUtil(mainForm);
            Dictionary<int, string> audioFiles = vUtil.getAllDemuxedAudio(job.AudioTracks, job.Output, 8);
            if (job.LoadSources)
            {
                if (job.DemuxMode != 0 && audioFiles.Count > 0)
                {
                    string[] files = new string[audioFiles.Values.Count];
                    audioFiles.Values.CopyTo(files, 0);
                    Util.ThreadSafeRun(mainForm, new MethodInvoker(
                        delegate
                        {
                            mainForm.Audio.openAudioFile(files);
                        }));
                }
                // if the above needed delegation for openAudioFile this needs it for openVideoFile?
                // It seems to fix the problem of ASW dissapearing as soon as it appears on a system (Vista X64)
                Util.ThreadSafeRun(mainForm, new MethodInvoker(
                    delegate
                    {
                        AviSynthWindow asw = new AviSynthWindow(mainForm, job.Output);
                        asw.OpenScript += new OpenScriptCallback(mainForm.Video.openVideoFile);
                        asw.Show();
                    }));
            }

            return null;
        }
    }

    public class dgiIndexJobPostProcessor
    {
        public static JobPostProcessor PostProcessor = new JobPostProcessor(postprocess, "Dgi_postprocessor");
        private static LogItem postprocess(MainForm mainForm, Job ajob)
        {
            if (!(ajob is DGIIndexJob)) return null;
            DGIIndexJob job = (DGIIndexJob)ajob;
            if (job.PostprocessingProperties != null) return null;

            StringBuilder logBuilder = new StringBuilder();
            VideoUtil vUtil = new VideoUtil(mainForm);
            Dictionary<int, string> audioFiles = vUtil.getAllDemuxedAudio(job.AudioTracks, job.Output, 8);
            if (job.LoadSources)
            {
                if (job.DemuxMode != 0 && audioFiles.Count > 0)
                {
                    string[] files = new string[audioFiles.Values.Count];
                    audioFiles.Values.CopyTo(files, 0);
                    Util.ThreadSafeRun(mainForm, new MethodInvoker(
                        delegate
                        {
                            mainForm.Audio.openAudioFile(files);
                        }));
                }
                // if the above needed delegation for openAudioFile this needs it for openVideoFile?
                // It seems to fix the problem of ASW dissapearing as soon as it appears on a system (Vista X64)
                Util.ThreadSafeRun(mainForm, new MethodInvoker(
                    delegate
                    {
                        AviSynthWindow asw = new AviSynthWindow(mainForm, job.Output);
                        asw.OpenScript += new OpenScriptCallback(mainForm.Video.openVideoFile);
                        asw.Show();
                    }));
            }

            return null;
        }
    }

    public class dgaIndexJobPostProcessor
    {
        public static JobPostProcessor PostProcessor = new JobPostProcessor(postprocess, "Dga_postprocessor");
        private static LogItem postprocess(MainForm mainForm, Job ajob)
        {
            if (!(ajob is DGAIndexJob)) return null;
            DGAIndexJob job = (DGAIndexJob)ajob;
            if (job.PostprocessingProperties != null) return null;

            StringBuilder logBuilder = new StringBuilder();
            VideoUtil vUtil = new VideoUtil(mainForm);
            Dictionary<int, string> audioFiles = vUtil.getAllDemuxedAudio(job.AudioTracks, job.Output, 8);
            if (job.LoadSources)
            {
                if (job.DemuxMode != 0 && audioFiles.Count > 0)
                {
                    string[] files = new string[audioFiles.Values.Count];
                    audioFiles.Values.CopyTo(files, 0);
                    Util.ThreadSafeRun(mainForm, new MethodInvoker(
                        delegate
                        {
                            mainForm.Audio.openAudioFile(files);
                        }));
                }
                // if the above needed delegation for openAudioFile this needs it for openVideoFile?
                // It seems to fix the problem of ASW dissapearing as soon as it appears on a system (Vista X64)
                Util.ThreadSafeRun(mainForm, new MethodInvoker(
                    delegate
                    {
                        AviSynthWindow asw = new AviSynthWindow(mainForm, job.Output);
                        asw.OpenScript += new OpenScriptCallback(mainForm.Video.openVideoFile);
                        asw.Show();
                    }));
            }

            return null;
        }
    }

    public class ffmsIndexJobPostProcessor
    {
        public static JobPostProcessor PostProcessor = new JobPostProcessor(postprocess, "FFMS_postprocessor");
        private static LogItem postprocess(MainForm mainForm, Job ajob)
        {
            if (!(ajob is FFMSIndexJob)) return null;
            FFMSIndexJob job = (FFMSIndexJob)ajob;

            StringBuilder logBuilder = new StringBuilder();
            //VideoUtil vUtil = new VideoUtil(mainForm);

            if (job.LoadSources)
            {
                // if the above needed delegation for openAudioFile this needs it for openVideoFile?
                // It seems to fix the problem of ASW dissapearing as soon as it appears on a system (Vista X64)
                Util.ThreadSafeRun(mainForm, new MethodInvoker(
                    delegate
                    {
                        AviSynthWindow asw = new AviSynthWindow(mainForm, job.Output);
                        asw.OpenScript += new OpenScriptCallback(mainForm.Video.openVideoFile);
                        asw.Show();
                    }));
            }

            return null;
        }
    }

    public delegate void ProjectCreationComplete(); // this event is fired when the dgindex thread finishes
}
