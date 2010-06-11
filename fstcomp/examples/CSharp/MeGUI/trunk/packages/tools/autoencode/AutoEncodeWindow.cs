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
using System.Text;
using System.Windows.Forms;

using MeGUI.core.details;
using MeGUI.core.plugins.interfaces;
using MeGUI.core.util;
using MeGUI.packages.tools.calculator;

namespace MeGUI
{
	/// <summary>
	/// Summary description for AutoEncode.
	/// </summary>
	public class AutoEncodeWindow : System.Windows.Forms.Form
	{
		#region variables
        private List<AudioJob> audioStreams;
        private bool prerender;
        private MainForm mainForm;
		private JobUtil jobUtil;
		private VideoUtil vUtil;
        private VideoInfo vInfo;
        private LogItem log = new LogItem("AutoEncode job generation log", ImageType.Information);
		private bool isBitrateMode = true;

        private System.Windows.Forms.GroupBox AutomaticEncodingGroup;
        private System.Windows.Forms.RadioButton FileSizeRadio;
        private System.Windows.Forms.Label AverageBitrateLabel;
        private System.Windows.Forms.GroupBox OutputGroupBox;
		private System.Windows.Forms.Button queueButton;
        private System.Windows.Forms.Button cancelButton;
        private System.Windows.Forms.TextBox projectedBitrateKBits;
		private System.Windows.Forms.CheckBox addSubsNChapters;
		private System.Windows.Forms.RadioButton averageBitrateRadio;
        private RadioButton noTargetRadio;
        private Label muxedOutputLabel;
        private Label containerLabel;
        private ComboBox container;
        private ToolTip defaultToolTip;
        private MuxProvider muxProvider;
        private VideoStream videoStream;
		#endregion

        private MeGUI.core.gui.HelpButton helpButton1;
        private MeGUI.core.gui.TargetSizeSCBox splitting;
        private MeGUI.core.gui.TargetSizeSCBox targetSize;
        private TextBox videoSize;
        private Label label2;
        protected FileBar muxedOutput;
        private Label DeviceLabel;
        private ComboBox device;


        private IContainer components;
		#region start / stop
		public AutoEncodeWindow()
		{
            InitializeComponent();
        }
        public AutoEncodeWindow(VideoStream videoStream, List<AudioJob> audioStreams, MainForm mainForm, bool prerender, VideoInfo vInfo)
            : this()
        {
            this.vInfo = vInfo;
            mainForm.Log.Add(log);
            this.videoStream = videoStream;
            this.audioStreams = audioStreams;
            this.prerender = prerender;
			this.mainForm = mainForm;
			jobUtil = new JobUtil(mainForm);
			vUtil = new VideoUtil(mainForm);
            muxProvider = mainForm.MuxProvider;
            container.Items.AddRange(muxProvider.GetSupportedContainers().ToArray());
            splitting.MinimumFileSize = new FileSize(Unit.MB, 1);
        }
        /// <summary>
        /// does the final initialization of the dialog
        /// gets all audio types from the audio streams, then asks the muxprovider for a list of containers it can mux the video and audio streams into
        /// if there is no muxer that can deliver any container for the video / audio combination, we can abort right away
        /// </summary>
        /// <returns>true if the given video/audio combination can be muxed to at least a single container, false if not</returns>
        public bool init()
        {
            List<AudioEncoderType> aTypes = new List<AudioEncoderType>();
            AudioEncoderType[] audioTypes;
            foreach (AudioJob stream in this.audioStreams)
            {
                if (stream.Settings != null && !String.IsNullOrEmpty(stream.Input) && !String.IsNullOrEmpty(stream.Output))
                    aTypes.Add(stream.Settings.EncoderType);
            }
            audioTypes = aTypes.ToArray();
            List<ContainerType> supportedOutputTypes = this.muxProvider.GetSupportedContainers(
                this.videoStream.Settings.EncoderType, audioTypes);
            
            if (supportedOutputTypes.Count <= 0)
                return false;

            this.container.Items.Clear();
            this.container.Items.AddRange(supportedOutputTypes.ToArray());
            this.container.SelectedIndex = 0;

            List<DeviceType> supportedOutputDeviceTypes = this.muxProvider.GetSupportedDevices();
            this.device.Items.AddRange(supportedOutputDeviceTypes.ToArray());
            this.device.SelectedIndex = 0;

            if (this.container.Text != "MP4")
                this.device.Enabled = false;
            else
                this.device.Enabled = true;
                
            string muxedName = FileUtil.AddToFileName(vInfo.VideoOutput, "-muxed");

            this.muxedOutput.Filename = Path.ChangeExtension(muxedName, (this.container.SelectedItem as ContainerType).Extension);

            splitting.Value = mainForm.Settings.AedSettings.SplitSize;
            if (mainForm.Settings.AedSettings.FileSizeMode)
            {
                FileSizeRadio.Checked = true;
                targetSize.Value = mainForm.Settings.AedSettings.FileSize;
            }
            else if (mainForm.Settings.AedSettings.BitrateMode)
            {
                averageBitrateRadio.Checked = true;
                projectedBitrateKBits.Text = mainForm.Settings.AedSettings.Bitrate.ToString();
            }
            else
                noTargetRadio.Checked = true;
            if (mainForm.Settings.AedSettings.AddAdditionalContent)
                addSubsNChapters.Checked = true;
            foreach (object o in container.Items) // I know this is ugly, but using the ContainerType doesn't work unless we're switching to manual serialization
            {
                if (o.ToString().Equals(mainForm.Settings.AedSettings.Container))
                {
                    container.SelectedItem = o;
                    break;
                }
            }
            return true;
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
		#endregion
		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
            this.components = new System.ComponentModel.Container();
            System.Windows.Forms.Label label1;
            this.AutomaticEncodingGroup = new System.Windows.Forms.GroupBox();
            this.videoSize = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.projectedBitrateKBits = new System.Windows.Forms.TextBox();
            this.targetSize = new MeGUI.core.gui.TargetSizeSCBox();
            this.noTargetRadio = new System.Windows.Forms.RadioButton();
            this.averageBitrateRadio = new System.Windows.Forms.RadioButton();
            this.FileSizeRadio = new System.Windows.Forms.RadioButton();
            this.AverageBitrateLabel = new System.Windows.Forms.Label();
            this.queueButton = new System.Windows.Forms.Button();
            this.OutputGroupBox = new System.Windows.Forms.GroupBox();
            this.device = new System.Windows.Forms.ComboBox();
            this.DeviceLabel = new System.Windows.Forms.Label();
            this.splitting = new MeGUI.core.gui.TargetSizeSCBox();
            this.container = new System.Windows.Forms.ComboBox();
            this.containerLabel = new System.Windows.Forms.Label();
            this.muxedOutputLabel = new System.Windows.Forms.Label();
            this.muxedOutput = new MeGUI.FileBar();
            this.cancelButton = new System.Windows.Forms.Button();
            this.addSubsNChapters = new System.Windows.Forms.CheckBox();
            this.defaultToolTip = new System.Windows.Forms.ToolTip(this.components);
            this.helpButton1 = new MeGUI.core.gui.HelpButton();
            label1 = new System.Windows.Forms.Label();
            this.AutomaticEncodingGroup.SuspendLayout();
            this.OutputGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            label1.AutoSize = true;
            label1.Location = new System.Drawing.Point(191, 23);
            label1.Name = "label1";
            label1.Size = new System.Drawing.Size(49, 13);
            label1.TabIndex = 27;
            label1.Text = "Splitting:";
            // 
            // AutomaticEncodingGroup
            // 
            this.AutomaticEncodingGroup.Controls.Add(this.videoSize);
            this.AutomaticEncodingGroup.Controls.Add(this.label2);
            this.AutomaticEncodingGroup.Controls.Add(this.projectedBitrateKBits);
            this.AutomaticEncodingGroup.Controls.Add(this.targetSize);
            this.AutomaticEncodingGroup.Controls.Add(this.noTargetRadio);
            this.AutomaticEncodingGroup.Controls.Add(this.averageBitrateRadio);
            this.AutomaticEncodingGroup.Controls.Add(this.FileSizeRadio);
            this.AutomaticEncodingGroup.Controls.Add(this.AverageBitrateLabel);
            this.AutomaticEncodingGroup.Location = new System.Drawing.Point(10, 116);
            this.AutomaticEncodingGroup.Name = "AutomaticEncodingGroup";
            this.AutomaticEncodingGroup.Size = new System.Drawing.Size(456, 106);
            this.AutomaticEncodingGroup.TabIndex = 17;
            this.AutomaticEncodingGroup.TabStop = false;
            this.AutomaticEncodingGroup.Text = "Size and Bitrate";
            // 
            // videoSize
            // 
            this.videoSize.Location = new System.Drawing.Point(310, 48);
            this.videoSize.Name = "videoSize";
            this.videoSize.ReadOnly = true;
            this.videoSize.Size = new System.Drawing.Size(137, 21);
            this.videoSize.TabIndex = 28;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(246, 51);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(58, 13);
            this.label2.TabIndex = 27;
            this.label2.Text = "Video size:";
            // 
            // projectedBitrateKBits
            // 
            this.projectedBitrateKBits.Enabled = false;
            this.projectedBitrateKBits.Location = new System.Drawing.Point(119, 45);
            this.projectedBitrateKBits.Name = "projectedBitrateKBits";
            this.projectedBitrateKBits.Size = new System.Drawing.Size(85, 21);
            this.projectedBitrateKBits.TabIndex = 9;
            this.projectedBitrateKBits.TextChanged += new System.EventHandler(this.projectedBitrate_TextChanged);
            this.projectedBitrateKBits.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.textField_KeyPress);
            // 
            // targetSize
            // 
            this.targetSize.CustomSizes = new MeGUI.core.util.FileSize[0];
            this.targetSize.Location = new System.Drawing.Point(116, 15);
            this.targetSize.MaximumSize = new System.Drawing.Size(1000, 29);
            this.targetSize.MinimumSize = new System.Drawing.Size(64, 29);
            this.targetSize.Name = "targetSize";
            this.targetSize.NullString = "Not calculated";
            this.targetSize.SelectedIndex = 0;
            this.targetSize.Size = new System.Drawing.Size(208, 29);
            this.targetSize.TabIndex = 25;
            this.targetSize.SelectionChanged += new MeGUI.StringChanged(this.targetSize_SelectionChanged);
            // 
            // noTargetRadio
            // 
            this.noTargetRadio.Location = new System.Drawing.Point(16, 75);
            this.noTargetRadio.Name = "noTargetRadio";
            this.noTargetRadio.Size = new System.Drawing.Size(218, 18);
            this.noTargetRadio.TabIndex = 22;
            this.noTargetRadio.TabStop = true;
            this.noTargetRadio.Text = "No Target Size (use profile settings)";
            this.defaultToolTip.SetToolTip(this.noTargetRadio, "Checking this allows the use of a previously defined bitrate or a non bitrate mod" +
                    "e (CQ, CRF)");
            this.noTargetRadio.UseVisualStyleBackColor = true;
            this.noTargetRadio.CheckedChanged += new System.EventHandler(this.calculationMode_CheckedChanged);
            // 
            // averageBitrateRadio
            // 
            this.averageBitrateRadio.AutoSize = true;
            this.averageBitrateRadio.Location = new System.Drawing.Point(16, 49);
            this.averageBitrateRadio.Name = "averageBitrateRadio";
            this.averageBitrateRadio.Size = new System.Drawing.Size(101, 17);
            this.averageBitrateRadio.TabIndex = 16;
            this.averageBitrateRadio.Text = "Average Bitrate";
            this.averageBitrateRadio.CheckedChanged += new System.EventHandler(this.calculationMode_CheckedChanged);
            // 
            // FileSizeRadio
            // 
            this.FileSizeRadio.Checked = true;
            this.FileSizeRadio.Location = new System.Drawing.Point(16, 20);
            this.FileSizeRadio.Name = "FileSizeRadio";
            this.FileSizeRadio.Size = new System.Drawing.Size(100, 18);
            this.FileSizeRadio.TabIndex = 15;
            this.FileSizeRadio.TabStop = true;
            this.FileSizeRadio.Text = "File Size";
            this.FileSizeRadio.CheckedChanged += new System.EventHandler(this.calculationMode_CheckedChanged);
            // 
            // AverageBitrateLabel
            // 
            this.AverageBitrateLabel.AutoSize = true;
            this.AverageBitrateLabel.Location = new System.Drawing.Point(207, 51);
            this.AverageBitrateLabel.Name = "AverageBitrateLabel";
            this.AverageBitrateLabel.Size = new System.Drawing.Size(33, 13);
            this.AverageBitrateLabel.TabIndex = 10;
            this.AverageBitrateLabel.Text = "kbit/s";
            // 
            // queueButton
            // 
            this.queueButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.queueButton.AutoSize = true;
            this.queueButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.queueButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.queueButton.Location = new System.Drawing.Point(358, 228);
            this.queueButton.Name = "queueButton";
            this.queueButton.Size = new System.Drawing.Size(49, 23);
            this.queueButton.TabIndex = 8;
            this.queueButton.Text = "Queue";
            this.queueButton.Click += new System.EventHandler(this.queueButton_Click);
            // 
            // OutputGroupBox
            // 
            this.OutputGroupBox.Controls.Add(this.device);
            this.OutputGroupBox.Controls.Add(this.DeviceLabel);
            this.OutputGroupBox.Controls.Add(label1);
            this.OutputGroupBox.Controls.Add(this.splitting);
            this.OutputGroupBox.Controls.Add(this.container);
            this.OutputGroupBox.Controls.Add(this.containerLabel);
            this.OutputGroupBox.Controls.Add(this.muxedOutputLabel);
            this.OutputGroupBox.Controls.Add(this.muxedOutput);
            this.OutputGroupBox.Location = new System.Drawing.Point(10, 4);
            this.OutputGroupBox.Name = "OutputGroupBox";
            this.OutputGroupBox.Size = new System.Drawing.Size(458, 106);
            this.OutputGroupBox.TabIndex = 18;
            this.OutputGroupBox.TabStop = false;
            this.OutputGroupBox.Text = "Output Options";
            // 
            // device
            // 
            this.device.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.device.FormattingEnabled = true;
            this.device.Items.AddRange(new object[] {
            "Standard"});
            this.device.Location = new System.Drawing.Point(97, 47);
            this.device.Name = "device";
            this.device.Size = new System.Drawing.Size(85, 21);
            this.device.TabIndex = 38;
            // 
            // DeviceLabel
            // 
            this.DeviceLabel.AutoSize = true;
            this.DeviceLabel.Location = new System.Drawing.Point(6, 51);
            this.DeviceLabel.Name = "DeviceLabel";
            this.DeviceLabel.Size = new System.Drawing.Size(39, 13);
            this.DeviceLabel.TabIndex = 37;
            this.DeviceLabel.Text = "Device";
            // 
            // splitting
            // 
            this.splitting.CustomSizes = new MeGUI.core.util.FileSize[0];
            this.splitting.Location = new System.Drawing.Point(243, 16);
            this.splitting.MaximumSize = new System.Drawing.Size(1000, 29);
            this.splitting.MinimumSize = new System.Drawing.Size(64, 29);
            this.splitting.Name = "splitting";
            this.splitting.NullString = "No splitting";
            this.splitting.SelectedIndex = 0;
            this.splitting.Size = new System.Drawing.Size(208, 29);
            this.splitting.TabIndex = 26;
            // 
            // container
            // 
            this.container.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.container.FormattingEnabled = true;
            this.container.Location = new System.Drawing.Point(97, 20);
            this.container.Name = "container";
            this.container.Size = new System.Drawing.Size(85, 21);
            this.container.TabIndex = 25;
            this.container.SelectedIndexChanged += new System.EventHandler(this.container_SelectedIndexChanged);
            // 
            // containerLabel
            // 
            this.containerLabel.AutoSize = true;
            this.containerLabel.Location = new System.Drawing.Point(6, 23);
            this.containerLabel.Name = "containerLabel";
            this.containerLabel.Size = new System.Drawing.Size(54, 13);
            this.containerLabel.TabIndex = 24;
            this.containerLabel.Text = "Container";
            // 
            // muxedOutputLabel
            // 
            this.muxedOutputLabel.AutoSize = true;
            this.muxedOutputLabel.Location = new System.Drawing.Point(6, 81);
            this.muxedOutputLabel.Name = "muxedOutputLabel";
            this.muxedOutputLabel.Size = new System.Drawing.Size(82, 13);
            this.muxedOutputLabel.TabIndex = 23;
            this.muxedOutputLabel.Text = "Name of output";
            // 
            // muxedOutput
            // 
            this.muxedOutput.Filename = "";
            this.muxedOutput.Filter = null;
            this.muxedOutput.FilterIndex = 0;
            this.muxedOutput.FolderMode = false;
            this.muxedOutput.Location = new System.Drawing.Point(97, 74);
            this.muxedOutput.Name = "muxedOutput";
            this.muxedOutput.ReadOnly = false;
            this.muxedOutput.SaveMode = true;
            this.muxedOutput.Size = new System.Drawing.Size(352, 26);
            this.muxedOutput.TabIndex = 36;
            this.muxedOutput.Title = null;
            // 
            // cancelButton
            // 
            this.cancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.cancelButton.AutoSize = true;
            this.cancelButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.cancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancelButton.Location = new System.Drawing.Point(413, 228);
            this.cancelButton.Name = "cancelButton";
            this.cancelButton.Size = new System.Drawing.Size(49, 23);
            this.cancelButton.TabIndex = 19;
            this.cancelButton.Text = "Cancel";
            // 
            // addSubsNChapters
            // 
            this.addSubsNChapters.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.addSubsNChapters.Location = new System.Drawing.Point(88, 228);
            this.addSubsNChapters.Name = "addSubsNChapters";
            this.addSubsNChapters.Size = new System.Drawing.Size(256, 24);
            this.addSubsNChapters.TabIndex = 20;
            this.addSubsNChapters.Text = "Add additional content (audio, subs, chapters)";
            this.defaultToolTip.SetToolTip(this.addSubsNChapters, "Checking this option allows you to specify pre-encoded audio and subtitle files t" +
                    "o be added to your output, as well as assign audio/subtitle languages and assign" +
                    " a chapter file");
            // 
            // helpButton1
            // 
            this.helpButton1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.helpButton1.ArticleName = "AutoEncode window";
            this.helpButton1.AutoSize = true;
            this.helpButton1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.helpButton1.Location = new System.Drawing.Point(10, 228);
            this.helpButton1.Name = "helpButton1";
            this.helpButton1.Size = new System.Drawing.Size(38, 23);
            this.helpButton1.TabIndex = 21;
            // 
            // AutoEncodeWindow
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.ClientSize = new System.Drawing.Size(471, 258);
            this.Controls.Add(this.helpButton1);
            this.Controls.Add(this.addSubsNChapters);
            this.Controls.Add(this.cancelButton);
            this.Controls.Add(this.OutputGroupBox);
            this.Controls.Add(this.AutomaticEncodingGroup);
            this.Controls.Add(this.queueButton);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "AutoEncodeWindow";
            this.ShowInTaskbar = false;
            this.Text = "MeGUI - Automatic Encoding";
            this.AutomaticEncodingGroup.ResumeLayout(false);
            this.AutomaticEncodingGroup.PerformLayout();
            this.OutputGroupBox.ResumeLayout(false);
            this.OutputGroupBox.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion
		#region dropdowns
        /// <summary>
        /// adjusts the output extension when the container is being changed
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void container_SelectedIndexChanged(object sender, EventArgs e)
        {
            ContainerType cot = this.container.SelectedItem as ContainerType;
            this.muxedOutput.Filter = cot.OutputFilterString;
            if (!String.IsNullOrEmpty (muxedOutput.Filename))
            {
                if (this.container.Text != "MP4")
                    this.device.Enabled = false;
                else this.device.Enabled = true;
                this.muxedOutput.Filename = Path.ChangeExtension(muxedOutput.Filename, (this.container.SelectedItem as ContainerType).Extension);
            }
        }
        #endregion
		#region additional events
		/// <summary>
		/// handles the selection of the output format
		/// in case of avi, if an encodeable audio stream is already present,
		/// the selection of additional streams needs to be completely disabled
		/// if not, it an be left enabled bt the text has to indicate the fact
		/// that you can only add an audio track and nothing else
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void outputFormat_CheckedChanged(object sender, System.EventArgs e)
		{
            ContainerType cot = this.container.SelectedItem as ContainerType;
            this.muxedOutput.Filename = Path.ChangeExtension(this.muxedOutput.Filename, cot.Extension);
        }
		/// <summary>
		/// separates encodable from muxable audio streams
		/// in addition to returning the two types separately an array of SubStreams is returned
		/// which is plugged into the muxer.. it contains the names of all the audio files
		/// that have to be muxed
		/// </summary>
		/// <param name="encodable">encodeable audio streams</param>
		/// <param name="muxable">muxable Audio Streams with the path filled out and a blank language</param>
        private void separateEncodableAndMuxableAudioStreams(out AudioJob[] encodable, out MuxStream[] muxable, out AudioEncoderType[] muxTypes)
		{
			encodable = this.getConfiguredAudioJobs(); // discards improperly configured ones
			// the rest of the job is all encodeable
			muxable = new MuxStream[encodable.Length];
            muxTypes = new AudioEncoderType[encodable.Length];
			int j = 0;
            foreach (AudioJob stream in encodable)
			{
                muxable[j] = stream.ToMuxStream();
                muxTypes[j] = stream.Settings.EncoderType;
				j++;
			}
		}
		#endregion
		#region helper methods
		/// <summary>
		/// sets the projected video bitrate field in the GUI
		/// </summary>
        private void setVideoBitrate()
        {
            try
            {
                CalcData data = GetCalcData();
                data.TotalSize = new FileSize(targetSize.Value.Value.Bytes);
                data.CalcByTotalSize();

                this.videoSize.Text = data.VideoSize.ToString();
                this.projectedBitrateKBits.Text = data.VideoBitrate.ToString();
            }
            catch (Exception)
            {
                this.projectedBitrateKBits.Text = "";
                videoSize.Text = "";
            }
        }

        private CalcData GetCalcData()
        {
            CalcData data = new CalcData((long)videoStream.NumberOfFrames, videoStream.Framerate, (ContainerType)container.SelectedItem,
                videoStream.Settings.Codec, videoStream.Settings.NbBframes > 0, getAudioStreamsForBitrate());
            return data;
        }

        private AudioBitrateCalculationStream[] getAudioStreamsForBitrate()
        {
            List<AudioBitrateCalculationStream> streams = new List<AudioBitrateCalculationStream>();
            foreach (AudioJob s in audioStreams)
                streams.Add(new AudioBitrateCalculationStream(s.Output));
            return streams.ToArray();
        }

		/// <summary>
		/// sets the size of the output given the desired bitrate
		/// </summary>
		private void setTargetSize()
		{
			try
            {
                CalcData data = GetCalcData();
                data.VideoBitrate = Int32.Parse(this.projectedBitrateKBits.Text);
                data.CalcByVideoSize();
                this.videoSize.Text = data.VideoSize.ToString();
                this.targetSize.Value = new FileSize(Unit.MB, data.TotalSize.MBExact);
            }
            catch (Exception)
            {
                videoSize.Text = "";
                targetSize.Value = null;
			}
		}
		#region audio
        /// <summary>
        /// sets the projected audio size for all audio streams that use CBR mode
        /// </summary>
        private void setAudioSize()
        {
            long[] sizes = new long[this.audioStreams.Count];
            int index = 0;
            foreach (AudioJob stream in this.audioStreams)
            {
                if (!string.IsNullOrEmpty(stream.Output)) // if we don't have the video length or the audio is not fully configured we can give up now
                {
                    long bytesPerSecond = 0;
                    if (stream.BitrateMode == BitrateManagementMode.CBR)
                    {
                        bytesPerSecond = stream.Settings.Bitrate * 1000 / 8;
                    }
                    double lengthInSeconds = (double)this.videoStream.NumberOfFrames / (double)this.videoStream.Framerate;
                    long sizeInBytes = (long)(lengthInSeconds * bytesPerSecond);
                    this.audioStreams[index].SizeBytes = sizeInBytes;
                    index++;
                }
            }
        }
		#endregion

		/// <summary>
		/// returns all audio streams that can be encoded or muxed
		/// </summary>
		/// <returns></returns>
        private AudioJob[] getConfiguredAudioJobs()
		{
            List<AudioJob> list = new List<AudioJob>();
            foreach (AudioJob stream in audioStreams)
			{
                if (String.IsNullOrEmpty(stream.Input))
                {
                    // no audio is ok, just skip
                    break;
                }
                list.Add(stream);

			}
            return list.ToArray();
		}
		#endregion
		#region button events
		/// <summary>
		/// handles the go button for automated encoding
		/// checks if we're in automated 2 pass video mode and that we're not using the snow codec
		/// then the video and audio configuration is checked, and if it checks out
		/// the audio job, video jobs and muxing job are generated, audio and video job are linked
		/// and encoding is started
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void queueButton_Click(object sender, System.EventArgs e)
		{
			if (!string.IsNullOrEmpty(this.muxedOutput.Filename))
			{

                FileSize? desiredSize = targetSize.Value;
                FileSize? splitSize = splitting.Value;

                if (FileSizeRadio.Checked)
                    log.LogValue("Desired Size ", desiredSize);
                else if (averageBitrateRadio.Checked)
                    log.LogValue("Projected Bitrate ", string.Format("{0}kbps", projectedBitrateKBits.Text));

                log.LogValue("Split Size ", splitSize);

                MuxStream[] audio;
                AudioJob[] aStreams;
                AudioEncoderType[] muxTypes;
				separateEncodableAndMuxableAudioStreams(out aStreams, out audio, out muxTypes);
				MuxStream[] subtitles = new MuxStream[0];
				string chapters = "";
				string videoInput = vInfo.VideoInput;
                string videoOutput = vInfo.VideoOutput;
                string muxedOutput = this.muxedOutput.Filename;
                ContainerType cot = this.container.SelectedItem as ContainerType;

                if (addSubsNChapters.Checked)
				{
                    AdaptiveMuxWindow amw = new AdaptiveMuxWindow(mainForm);
                    amw.setMinimizedMode(videoOutput, videoStream.Settings.EncoderType, jobUtil.getFramerate(videoInput), audio,
                        muxTypes, muxedOutput, splitSize, cot);
                    if (amw.ShowDialog() == DialogResult.OK)
                        amw.getAdditionalStreams(out audio, out subtitles, out chapters, out muxedOutput, out cot);
                    else // user aborted, abort the whole process
                        return;
                }
                removeStreamsToBeEncoded(ref audio, aStreams);
                mainForm.Jobs.addJobsWithDependencies(vUtil.GenerateJobSeries(this.videoStream, muxedOutput, aStreams, subtitles, chapters,
                    desiredSize, splitSize, cot, this.prerender, audio, log, this.device.Text));
                this.Close();
			}
		}

        /// <summary>
        /// Reallocates the audio array so that it only has the files to be muxed and not the files to be encoded, then muxed
        /// </summary>
        /// <param name="audio">All files to be muxed (including the ones which will be encoded first)</param>
        /// <param name="aStreams">All files being encoded (these will be removed from the audio array)</param>
        private void removeStreamsToBeEncoded(ref MuxStream[] audio, AudioJob[] aStreams)
        {
            List<MuxStream> newAudio = new List<MuxStream>();
            foreach (MuxStream stream in audio)
            {
                bool matchFound = false;
                foreach (AudioJob a in aStreams)
                {
                    if (stream.path == a.Output)
                    {
                        matchFound = true; // In this case we have found a file which needs to be encoded
                        break;
                    }
                }
                if (!matchFound) // in this case we have not found any files which will be encoded first to produce this file
                {
                    newAudio.Add(stream);
                }
            }
            audio = newAudio.ToArray();
        }
		#endregion
		#region event helper methods
		private void textField_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (! char.IsDigit(e.KeyChar) && (int)Keys.Back != (int)e.KeyChar)
				e.Handled = true;
		}

		private void containerOverhead_ValueChanged(object sender, System.EventArgs e)
		{
			if (isBitrateMode)
				this.setVideoBitrate();
			else
				this.setTargetSize();
		}
		private void projectedBitrate_TextChanged(object sender, System.EventArgs e)
		{
			if (!this.isBitrateMode)
				this.setTargetSize();
		}


		private void calculationMode_CheckedChanged(object sender, System.EventArgs e)
		{
			if (averageBitrateRadio.Checked)
			{
				targetSize.Enabled = false;
                this.targetSize.SelectedIndex = 3;
				this.projectedBitrateKBits.Enabled = true;
				this.isBitrateMode = false;
			}
            else if (noTargetRadio.Checked)
            {
                targetSize.Enabled = false;
                this.targetSize.SelectedIndex = 0;
                this.projectedBitrateKBits.Enabled = false;
                this.isBitrateMode = false;
            } 
            else
			{
				targetSize.Enabled = true;
                this.targetSize.SelectedIndex = 3;
				this.projectedBitrateKBits.Enabled = false;
				this.isBitrateMode = true;
			}
		}
		#endregion

        private void targetSize_SelectionChanged(object sender, string val)
        {
            if (isBitrateMode)
                this.setVideoBitrate();
        }
    }
    public class AutoEncodeTool : ITool
    {
        #region ITool Members

        public string Name
        {
            get { return "AutoEncode"; }
        }

        public void Run(MainForm info)
        {
            // normal video verification
            string error = null;
            if ((error = info.Video.verifyVideoSettings()) != null)
            {
                MessageBox.Show(error, "Unsupported video configuration", MessageBoxButtons.OK, MessageBoxIcon.Stop);
                return;
            }
            if ((error = info.Audio.verifyAudioSettings()) != null && !error.Equals("No audio input defined."))            {
                MessageBox.Show(error, "Unsupported audio configuration", MessageBoxButtons.OK, MessageBoxIcon.Stop);
                return;
            }
#warning must be fixed up to be more generic
            if (info.Video.CurrentSettings.EncodingMode == 2 || info.Video.CurrentSettings.EncodingMode == 5)
            {
                MessageBox.Show("First pass encoding is not supported for automated encoding as no output is generated.\nPlease choose another encoding mode", "Improper configuration",
                    MessageBoxButtons.OK, MessageBoxIcon.Stop);
                return;
            }

            VideoCodecSettings vSettings = info.Video.CurrentSettings.Clone();
            Zone[] zones = info.Video.Info.Zones; // We can't simply modify the zones in place because that would reveal the final zones config to the user, including the credits/start zones
            bool cont = info.JobUtil.getFinalZoneConfiguration(vSettings, info.Video.Info.IntroEndFrame, info.Video.Info.CreditsStartFrame, ref zones);
            if (cont)
            {
                ulong length = 0;
                double framerate = 0.0;
                VideoStream myVideo = new VideoStream();
                JobUtil.getInputProperties(out length, out framerate, info.Video.VideoInput);
                myVideo.Input = info.Video.Info.VideoInput;
                myVideo.Output = info.Video.Info.VideoOutput;
                myVideo.NumberOfFrames = length;
                myVideo.Framerate = (decimal)framerate;
                myVideo.DAR = info.Video.Info.DAR;
                myVideo.VideoType = info.Video.CurrentMuxableVideoType;
                myVideo.Settings = vSettings;
                
                VideoInfo vInfo = info.Video.Info.Clone(); // so we don't modify the data on the main form
                vInfo.Zones = zones;

                using (AutoEncodeWindow aew = new AutoEncodeWindow(myVideo, info.Audio.AudioStreams, info, info.Video.PrerenderJob, vInfo))
                {
                    if (aew.init())
                    {
                        info.ClosePlayer();
                        aew.ShowDialog();
                    }
                    else
                        MessageBox.Show("The currently selected combination of video and audio output cannot be muxed", "Unsupported configuration", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                }
            }           
        }

        public Shortcut[] Shortcuts
        {
            get { return new Shortcut[] { Shortcut.CtrlF6 }; }
        }

        #endregion

        #region IIDable Members

        public string ID
        {
            get { return "AutoEncode"; }
        }

        #endregion
    }
}
