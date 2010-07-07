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
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Threading;
using System.Windows.Forms;

using MeGUI.core.gui;
using MeGUI.core.util;

namespace MeGUI
{
	public delegate void IntroCreditsFrameSetCallback(int frameNumber, bool isCredits);
	public delegate void ZoneSetCallback(int start, int end);
	public delegate void ChapterSetCallback(int frameNumber);
	public delegate void SimpleDelegate();
	public enum PREVIEWTYPE {REGULAR, CREDITS, ZONES, CHAPTERS};
	/// <summary>
	/// The video player is used to preview an AviSynth script or DGIndex project. 
	/// It can also be used to set the credits and intro and is used to generate zones visually
	/// </summary>
	public class VideoPlayer : System.Windows.Forms.Form
	{
		
		private System.Windows.Forms.PictureBox videoPreview;
		private System.Windows.Forms.GroupBox previewGroupbox;
		private System.Windows.Forms.TrackBar positionSlider;

		public event IntroCreditsFrameSetCallback IntroCreditsFrameSet; // event to update the status in the GUI
		public event ZoneSetCallback ZoneSet;
		public event ChapterSetCallback ChapterSet;

		private IMediaFile file;
        private IVideoReader reader;
        private bool hasAR = false;
		private int millisecondsPerFrame; // delay in between displaying two frames
		private int currentPosition, creditsStartFrame = -1, introEndFrame = -1;
		private int zoneStart = -1, zoneEnd = -1; // zone start and end frames
		private bool isRunning; // whether the player is running and whether the input is an avisynth script
		private int right, top, left, bottom; // cropping values
		private const int formWidthDelta = 48; // width delta of the form versus the size of the picturebox (reference)
		private int formHeightDelta = 176; // height delta of the form versus the size of the picturebox (the reference for the gui size)
		private const int previewGrouboxWidthDelta = 6; // width delta of the preview groupbox versus the picturebox
		private const int previewGroupboxHeightDelta = 18; // height delta of the preview groupbox versus the picturebox
		private const int positionSliderWidthDelta = 16; // size of the slider versus the preview picturebox
		private const int positionSliderHeight = 45; // height of the position slider
		private const int defaultSpacing = 8; // default spacing from GUI elements
		private const int formHeightZonesDelta = 28; // additional form height needed to display the zones buttons
        private int videoWindowWidth, videoWindowHeight;
        private PREVIEWTYPE viewerType;
        private static bool sizeLock; // recursion lock for resize event handler
        private int zoomWidth;
        private string totalTime;
        private string currentTime;
        private MainForm mainForm = MainForm.Instance;

		private System.Windows.Forms.Button playButton;
		private System.Windows.Forms.Button nextFrameButton;
		private System.Windows.Forms.Button previousFrameButton;
		private System.Windows.Forms.Button ffButton;
		private System.Windows.Forms.Button fwdButton;
		private System.Windows.Forms.Button creditsStartButton;
		private System.Windows.Forms.Panel buttonPanel;
		private System.Windows.Forms.Button zoneStartButton;
		private System.Windows.Forms.Button zoneEndButton;
		private System.Windows.Forms.Button setZoneButton;
		private System.Windows.Forms.ContextMenu contextMenu1;
		private System.Windows.Forms.MenuItem mnuZoneStart;
		private System.Windows.Forms.MenuItem mnuZoneEnd;
		private System.Windows.Forms.MenuItem mnuIntroEnd;
		private System.Windows.Forms.MenuItem mnuCreditsStart;
		private System.Windows.Forms.Button introEndButton;
		private System.Windows.Forms.ToolTip defaultToolTip;
		private System.Windows.Forms.Button chapterButton;
        private Button originalSizeButton;
        private CheckBox showPAR;
        private ARChooser arChooser;
        private Button zoomInButton;
        private Button zoomOutButton;
		private System.ComponentModel.IContainer components;
		
		
		public VideoPlayer()
		{
			InitializeComponent();
            sizeLock = false;
            this.Resize += new EventHandler(formResized);
            right = top = left = bottom = 0;
		}

        public bool AllowClose
        {
            set
            {
                this.ControlBox = value;
            }
            get
            {
                return this.ControlBox;
            }
        }

		/// <summary>
		/// loads the video, sets up the proper window size and enables / disables the GUI buttons depending on the
		/// preview type set
		/// </summary>
		/// <param name="path">path of the video file to be loaded</param>
		/// <param name="type">type of window</param>
		/// <returns>true if the video could be opened, false if not</returns>
        public bool loadVideo(MainForm mainForm, string path, PREVIEWTYPE type, bool hasAR)
        {
            return loadVideo(mainForm, path, type, hasAR, false, -1);
        }

		/// <summary>
		/// loads the video, sets up the proper window size and enables / disables the GUI buttons depending on the
		/// preview type set
		/// </summary>
		/// <param name="path">path of the video file to be loaded</param>
		/// <param name="type">type of window</param>
        /// <param name="inlineAvs">true if path contain not filename but avsynth script to be parsed</param>
		/// <returns>true if the video could be opened, false if not</returns>
        public bool loadVideo(MainForm mainForm, string path, PREVIEWTYPE type, bool hasAR, bool inlineAvs)
        {
            return loadVideo(mainForm, path, type, hasAR, inlineAvs, -1);
        }

        /// <summary>
		/// loads the video, sets up the proper window size and enables / disables the GUI buttons depending on the
		/// preview type set
		/// </summary>
		/// <param name="path">path of the video file to be loaded</param>
		/// <param name="type">type of window</param>
        /// <param name="inlineAvs">true if path contain not filename but avsynth script to be parsed</param>
        /// <param name="startFrame">Select a specific frame to start off with or -1 for middle of video</param>
		/// <returns>true if the video could be opened, false if not</returns>
		public bool loadVideo(MainForm mainForm, string path, PREVIEWTYPE type, bool hasAR, bool inlineAvs, int startFrame)
		{
            lock (this)
            {
                if (file != null)
                    file.Dispose();
                if (videoPreview.Image != null)
                    videoPreview.Image.Dispose(); // get rid of previous bitmap
            }

            try
            {
                if (inlineAvs)
                {
                    file = AvsFile.ParseScript(path);
                }
                else
                {
                    file = mainForm.MediaFileFactory.Open(path);
                    if (file == null && !(file.Info.HasVideo && file.CanReadVideo))
                        throw new ArgumentException("The video stream cannot be opened");
                }
                reader = file.GetVideoReader();
            }
            catch (AviSynthException e)
            {
                MessageBox.Show("AviSynth script error:\r\n" + e.Message, "AviSynth error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return false;
            }
            catch (ArgumentException e)
            {
                MessageBox.Show("AviSynth script error:\r\n" + e.Message, "AviSynth error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return false;
            }
            catch (Exception e)
            {

                
                MessageBox.Show("The file " + path + " cannot be opened.\r\n Please make sure it's a valid AviSynth script and that AviSynth is "
                    + " properly installed.\r\nYou can check the validity of your script and AviSynth installation by opening the file in your favorite media player.\r\n"
                    + " If that works, try opening the video in VirtualDub(Mod) as well. If the former works and the latter doesn't, install a YV12 codec.\r\n"
                    + "Error message for your reference: " + e.Message,
                    "Cannot open video input", MessageBoxButtons.OK, MessageBoxIcon.Stop);
                return false;
            }

			if (reader != null && reader.FrameCount > 0)
			{
                this.positionSlider.Minimum = 0;
				this.positionSlider.Maximum = reader.FrameCount - 1;
				this.positionSlider.Value = startFrame >= 0 ? startFrame : reader.FrameCount / 2;
                this.positionSlider.TickFrequency = this.positionSlider.Maximum / 20;
                this.viewerType = type;
                this.hasAR = hasAR;
                this.videoWindowWidth = (int)file.Info.Width;
                this.videoWindowHeight = (int)file.Info.Height;
                zoomWidth = (int)file.Info.Width;
                doInitialAdjustment();
                adjustSize();
				positionSlider_Scroll(null, null); // makes the image visible
                this.Text = "Current position: " + this.positionSlider.Value + "/" + this.positionSlider.Maximum;
				isRunning = false;
                millisecondsPerFrame = (int)(1000 / file.Info.FPS);                
				return true;
			}
			return false;
		}
		/// <summary>
		/// disables intro and credits setting
		/// </summary>
		public void disableIntroAndCredits()
		{
			buttonPanel.Controls.Remove(introEndButton);
			buttonPanel.Controls.Remove(creditsStartButton);
		}
		
		
        /// <summary>
        /// Reset the video preview to the size of the input stream
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        void originalSizeButton_Click(object sender, EventArgs e)
        {
            zoomWidth = (int)file.Info.Width;
            resize(zoomWidth, showPAR.Checked);
        }
        private void zoomInButton_Click(object sender, EventArgs e)
        {
            if (zoomWidth <= (int)file.Info.Width * 4) // hardcoded 4x limit
            {
                zoomWidth = (int)(zoomWidth * 2);
                resize(zoomWidth, showPAR.Checked);
            }
        }
        private void zoomOutButton_Click(object sender, EventArgs e)
        {
            if (zoomWidth > (int)file.Info.Width / 4) // hardcoded 4x limit
            {
                zoomWidth = (int)(zoomWidth / 2);
                resize(zoomWidth, showPAR.Checked);
            }
        }
        private void doInitialAdjustment()
        {
            switch (this.viewerType)
            {
                case PREVIEWTYPE.REGULAR:
                    buttonPanel.Controls.Remove(creditsStartButton);
                    buttonPanel.Controls.Remove(introEndButton);
                    buttonPanel.Controls.Remove(chapterButton);
                    buttonPanel.Controls.Remove(zoneStartButton);
                    buttonPanel.Controls.Remove(zoneEndButton);
                    buttonPanel.Controls.Remove(setZoneButton);
                    break;
                case PREVIEWTYPE.ZONES:
                    buttonPanel.Controls.Remove(creditsStartButton);
                    buttonPanel.Controls.Remove(introEndButton);
                    buttonPanel.Controls.Remove(chapterButton);
                    formHeightDelta += formHeightZonesDelta;
                    break;
                case PREVIEWTYPE.CREDITS:
                    buttonPanel.Controls.Remove(zoneStartButton);
                    buttonPanel.Controls.Remove(zoneEndButton);
                    buttonPanel.Controls.Remove(setZoneButton);
                    buttonPanel.Controls.Remove(chapterButton);
                    break;
                case PREVIEWTYPE.CHAPTERS:
                    buttonPanel.Controls.Remove(creditsStartButton);
                    buttonPanel.Controls.Remove(introEndButton);
                    buttonPanel.Controls.Remove(zoneStartButton);
                    buttonPanel.Controls.Remove(zoneEndButton);
                    buttonPanel.Controls.Remove(setZoneButton);
                    break;
            }
        }
		/// <summary>
		/// adjusts the size of the GUI to match the source video
		/// </summary>
		private void adjustSize()
		{
            if (!hasAR)
            {
                buttonPanel.Controls.Remove(arChooser);
                buttonPanel.Controls.Remove(showPAR);
            }
            SuspendLayout();
            sizeLock = true;
            this.Size = new Size(this.videoWindowWidth + formWidthDelta, this.videoWindowHeight + formHeightDelta);
            sizeLock = false;
			this.previewGroupbox.Size = new Size(this.videoWindowWidth + previewGrouboxWidthDelta, this.videoWindowHeight + previewGroupboxHeightDelta);
			this.videoPreview.Size = new Size(this.videoWindowWidth, this.videoWindowHeight);
			this.positionSlider.Size = new Size(this.videoWindowWidth + positionSliderWidthDelta, positionSliderHeight);
			this.positionSlider.Location = new Point(defaultSpacing, previewGroupbox.Size.Height + defaultSpacing);
            ResumeLayout();
        }
        private void formResized(object sender, EventArgs e)
        {
            if (!sizeLock)
            {
                Control formControl = (Control)sender;
                if ((formControl.Width <= this.MaximumSize.Width) &&
                    (formControl.Height <= this.MaximumSize.Height) &&
                    (formControl.Width >= this.MinimumSize.Width) &&
                    (formControl.Height >= this.MinimumSize.Height))
                {
                    // Unusable without events from .NET 2.0 
                    resize(formControl.Width - formWidthDelta, showPAR.Checked);
                }
            }
        }
        /// <summary>
        /// Resizes the video frame
        /// http://www.peterprovost.org/archive/2003/05/29/516.aspx
        /// </summary>
        /// <param name="b"></param>
        /// <param name="nWidth"></param>
        /// <param name="nHeight"></param>
        /// <returns>A resized bitmap (needs disposal)</returns>
        private Bitmap resizeBitmap(Bitmap b, int nWidth, int nHeight)
        {
            Bitmap result = new Bitmap(nWidth, nHeight);
            using (Graphics g = Graphics.FromImage((Image)result))
            {
                g.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.Bilinear;
                g.DrawImage(b, 0, 0, nWidth, nHeight);
            }
            return result;
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
		/// performs additional tasks when the window is closed
		/// ensures that if the AviReader/d2vreader is valid, access to the file is properly closed
		/// </summary>
		/// <param name="e"></param>
		protected override void OnClosing(CancelEventArgs e)
		{
            if (!this.AllowClose)
            {
                e.Cancel = true;
                return;
            }

            if (file != null)
                file.Dispose();
			base.OnClosing (e);
		}
		
		
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
            this.components = new System.ComponentModel.Container();
            System.Windows.Forms.Button goToFrameButton;
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(VideoPlayer));
            this.videoPreview = new System.Windows.Forms.PictureBox();
            this.contextMenu1 = new System.Windows.Forms.ContextMenu();
            this.mnuIntroEnd = new System.Windows.Forms.MenuItem();
            this.mnuCreditsStart = new System.Windows.Forms.MenuItem();
            this.mnuZoneStart = new System.Windows.Forms.MenuItem();
            this.mnuZoneEnd = new System.Windows.Forms.MenuItem();
            this.previewGroupbox = new System.Windows.Forms.GroupBox();
            this.positionSlider = new System.Windows.Forms.TrackBar();
            this.playButton = new System.Windows.Forms.Button();
            this.nextFrameButton = new System.Windows.Forms.Button();
            this.previousFrameButton = new System.Windows.Forms.Button();
            this.ffButton = new System.Windows.Forms.Button();
            this.fwdButton = new System.Windows.Forms.Button();
            this.creditsStartButton = new System.Windows.Forms.Button();
            this.buttonPanel = new System.Windows.Forms.Panel();
            this.zoomOutButton = new System.Windows.Forms.Button();
            this.zoomInButton = new System.Windows.Forms.Button();
            this.arChooser = new MeGUI.core.gui.ARChooser();
            this.showPAR = new System.Windows.Forms.CheckBox();
            this.originalSizeButton = new System.Windows.Forms.Button();
            this.introEndButton = new System.Windows.Forms.Button();
            this.zoneStartButton = new System.Windows.Forms.Button();
            this.setZoneButton = new System.Windows.Forms.Button();
            this.zoneEndButton = new System.Windows.Forms.Button();
            this.chapterButton = new System.Windows.Forms.Button();
            this.defaultToolTip = new System.Windows.Forms.ToolTip(this.components);
            goToFrameButton = new System.Windows.Forms.Button();
            ((System.ComponentModel.ISupportInitialize)(this.videoPreview)).BeginInit();
            this.previewGroupbox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.positionSlider)).BeginInit();
            this.buttonPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // goToFrameButton
            // 
            goToFrameButton.Location = new System.Drawing.Point(200, 8);
            goToFrameButton.Name = "goToFrameButton";
            goToFrameButton.Size = new System.Drawing.Size(82, 18);
            goToFrameButton.TabIndex = 9;
            goToFrameButton.Text = "Go to frame";
            goToFrameButton.Click += new System.EventHandler(this.goToFrameButton_Click);
            // 
            // videoPreview
            // 
            this.videoPreview.ContextMenu = this.contextMenu1;
            this.videoPreview.Location = new System.Drawing.Point(3, 17);
            this.videoPreview.Name = "videoPreview";
            this.videoPreview.Size = new System.Drawing.Size(274, 164);
            this.videoPreview.TabIndex = 0;
            this.videoPreview.TabStop = false;
            // 
            // contextMenu1
            // 
            this.contextMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.mnuIntroEnd,
            this.mnuCreditsStart,
            this.mnuZoneStart,
            this.mnuZoneEnd});
            this.contextMenu1.Popup += new System.EventHandler(this.contextMenu1_Popup);
            // 
            // mnuIntroEnd
            // 
            this.mnuIntroEnd.Index = 0;
            this.mnuIntroEnd.Shortcut = System.Windows.Forms.Shortcut.CtrlI;
            this.mnuIntroEnd.ShowShortcut = false;
            this.mnuIntroEnd.Text = "Go to End of &Intro";
            this.mnuIntroEnd.Click += new System.EventHandler(this.mnuIntroEnd_Click);
            // 
            // mnuCreditsStart
            // 
            this.mnuCreditsStart.Index = 1;
            this.mnuCreditsStart.Shortcut = System.Windows.Forms.Shortcut.CtrlC;
            this.mnuCreditsStart.ShowShortcut = false;
            this.mnuCreditsStart.Text = "Go to Start of &Credits";
            this.mnuCreditsStart.Click += new System.EventHandler(this.mnuCreditsStart_Click);
            // 
            // mnuZoneStart
            // 
            this.mnuZoneStart.Index = 2;
            this.mnuZoneStart.Shortcut = System.Windows.Forms.Shortcut.CtrlS;
            this.mnuZoneStart.ShowShortcut = false;
            this.mnuZoneStart.Text = "Go to &Start of Zone";
            this.mnuZoneStart.Click += new System.EventHandler(this.mnuZoneStart_Click);
            // 
            // mnuZoneEnd
            // 
            this.mnuZoneEnd.Index = 3;
            this.mnuZoneEnd.Shortcut = System.Windows.Forms.Shortcut.CtrlE;
            this.mnuZoneEnd.ShowShortcut = false;
            this.mnuZoneEnd.Text = "Go to &End of Zone";
            this.mnuZoneEnd.Click += new System.EventHandler(this.mnuZoneEnd_Click);
            // 
            // previewGroupbox
            // 
            this.previewGroupbox.Controls.Add(this.videoPreview);
            this.previewGroupbox.Location = new System.Drawing.Point(8, 8);
            this.previewGroupbox.Name = "previewGroupbox";
            this.previewGroupbox.Size = new System.Drawing.Size(280, 184);
            this.previewGroupbox.TabIndex = 1;
            this.previewGroupbox.TabStop = false;
            this.previewGroupbox.Text = "Video Preview";
            // 
            // positionSlider
            // 
            this.positionSlider.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.positionSlider.AutoSize = false;
            this.positionSlider.LargeChange = 1000;
            this.positionSlider.Location = new System.Drawing.Point(46, 259);
            this.positionSlider.Minimum = -1;
            this.positionSlider.Name = "positionSlider";
            this.positionSlider.Size = new System.Drawing.Size(280, 45);
            this.positionSlider.TabIndex = 1;
            this.positionSlider.TickFrequency = 1500;
            this.positionSlider.TickStyle = System.Windows.Forms.TickStyle.Both;
            this.positionSlider.Value = -1;
            this.positionSlider.Scroll += new System.EventHandler(this.positionSlider_Scroll);
            // 
            // playButton
            // 
            this.playButton.Location = new System.Drawing.Point(80, 8);
            this.playButton.Name = "playButton";
            this.playButton.Size = new System.Drawing.Size(40, 18);
            this.playButton.TabIndex = 2;
            this.playButton.Text = "Play";
            this.playButton.Click += new System.EventHandler(this.playButton_Click);
            // 
            // nextFrameButton
            // 
            this.nextFrameButton.Location = new System.Drawing.Point(176, 8);
            this.nextFrameButton.Name = "nextFrameButton";
            this.nextFrameButton.Size = new System.Drawing.Size(16, 18);
            this.nextFrameButton.TabIndex = 3;
            this.nextFrameButton.Text = ">";
            this.defaultToolTip.SetToolTip(this.nextFrameButton, "Advance by 1 frame");
            this.nextFrameButton.Click += new System.EventHandler(this.nextFrameButton_Click);
            this.nextFrameButton.KeyDown += new System.Windows.Forms.KeyEventHandler(this.nextFrameButton_KeyDown);
            // 
            // previousFrameButton
            // 
            this.previousFrameButton.Location = new System.Drawing.Point(8, 8);
            this.previousFrameButton.Name = "previousFrameButton";
            this.previousFrameButton.Size = new System.Drawing.Size(16, 18);
            this.previousFrameButton.TabIndex = 4;
            this.previousFrameButton.Text = "<";
            this.defaultToolTip.SetToolTip(this.previousFrameButton, "Go back 1 frame");
            this.previousFrameButton.Click += new System.EventHandler(this.previousFrameButton_Click);
            this.previousFrameButton.KeyDown += new System.Windows.Forms.KeyEventHandler(this.previousFrameButton_KeyDown);
            // 
            // ffButton
            // 
            this.ffButton.Location = new System.Drawing.Point(136, 8);
            this.ffButton.Name = "ffButton";
            this.ffButton.Size = new System.Drawing.Size(30, 18);
            this.ffButton.TabIndex = 5;
            this.ffButton.Text = ">>";
            this.defaultToolTip.SetToolTip(this.ffButton, "Advance 10 frames");
            this.ffButton.Click += new System.EventHandler(this.ffButton_Click);
            // 
            // fwdButton
            // 
            this.fwdButton.Location = new System.Drawing.Point(32, 8);
            this.fwdButton.Name = "fwdButton";
            this.fwdButton.Size = new System.Drawing.Size(30, 18);
            this.fwdButton.TabIndex = 6;
            this.fwdButton.Text = "<<";
            this.defaultToolTip.SetToolTip(this.fwdButton, "Go back 10 frames");
            this.fwdButton.Click += new System.EventHandler(this.fwdButton_Click);
            // 
            // creditsStartButton
            // 
            this.creditsStartButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.creditsStartButton.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.creditsStartButton.Location = new System.Drawing.Point(278, 30);
            this.creditsStartButton.Name = "creditsStartButton";
            this.creditsStartButton.Size = new System.Drawing.Size(44, 19);
            this.creditsStartButton.TabIndex = 7;
            this.creditsStartButton.Text = "Credits";
            this.defaultToolTip.SetToolTip(this.creditsStartButton, "Set the frame where the credits start");
            this.creditsStartButton.Click += new System.EventHandler(this.creditsStartButton_Click);
            // 
            // buttonPanel
            // 
            this.buttonPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.buttonPanel.Controls.Add(this.zoomOutButton);
            this.buttonPanel.Controls.Add(this.zoomInButton);
            this.buttonPanel.Controls.Add(this.arChooser);
            this.buttonPanel.Controls.Add(this.showPAR);
            this.buttonPanel.Controls.Add(this.originalSizeButton);
            this.buttonPanel.Controls.Add(this.introEndButton);
            this.buttonPanel.Controls.Add(this.previousFrameButton);
            this.buttonPanel.Controls.Add(this.fwdButton);
            this.buttonPanel.Controls.Add(this.playButton);
            this.buttonPanel.Controls.Add(this.ffButton);
            this.buttonPanel.Controls.Add(this.nextFrameButton);
            this.buttonPanel.Controls.Add(this.zoneStartButton);
            this.buttonPanel.Controls.Add(this.creditsStartButton);
            this.buttonPanel.Controls.Add(goToFrameButton);
            this.buttonPanel.Controls.Add(this.setZoneButton);
            this.buttonPanel.Controls.Add(this.zoneEndButton);
            this.buttonPanel.Controls.Add(this.chapterButton);
            this.buttonPanel.Location = new System.Drawing.Point(14, 310);
            this.buttonPanel.Name = "buttonPanel";
            this.buttonPanel.Size = new System.Drawing.Size(333, 80);
            this.buttonPanel.TabIndex = 8;
            // 
            // zoomOutButton
            // 
            this.zoomOutButton.Location = new System.Drawing.Point(52, 30);
            this.zoomOutButton.Name = "zoomOutButton";
            this.zoomOutButton.Size = new System.Drawing.Size(17, 19);
            this.zoomOutButton.TabIndex = 9;
            this.zoomOutButton.Text = "-";
            this.zoomOutButton.UseVisualStyleBackColor = true;
            this.zoomOutButton.Click += new System.EventHandler(this.zoomOutButton_Click);
            // 
            // zoomInButton
            // 
            this.zoomInButton.Location = new System.Drawing.Point(8, 30);
            this.zoomInButton.Name = "zoomInButton";
            this.zoomInButton.Size = new System.Drawing.Size(14, 19);
            this.zoomInButton.TabIndex = 9;
            this.zoomInButton.Text = "+";
            this.zoomInButton.UseVisualStyleBackColor = true;
            this.zoomInButton.Click += new System.EventHandler(this.zoomInButton_Click);
            // 
            // arChooser
            // 
            this.arChooser.CustomDARs = new MeGUI.core.util.Dar[0];
            this.arChooser.HasLater = false;
            this.arChooser.Location = new System.Drawing.Point(8, 48);
            this.arChooser.MaximumSize = new System.Drawing.Size(1000, 29);
            this.arChooser.MinimumSize = new System.Drawing.Size(64, 29);
            this.arChooser.Name = "arChooser";
            this.arChooser.SelectedIndex = 0;
            this.arChooser.Size = new System.Drawing.Size(208, 29);
            this.arChooser.TabIndex = 15;
            this.arChooser.SelectionChanged += new MeGUI.StringChanged(this.arChooser_SelectionChanged);
            // 
            // showPAR
            // 
            this.showPAR.AutoSize = true;
            this.showPAR.Location = new System.Drawing.Point(236, 54);
            this.showPAR.Name = "showPAR";
            this.showPAR.Size = new System.Drawing.Size(76, 17);
            this.showPAR.TabIndex = 2;
            this.showPAR.Text = "Show DAR";
            this.showPAR.UseVisualStyleBackColor = true;
            this.showPAR.CheckedChanged += new System.EventHandler(this.showPAR_CheckedChanged);
            // 
            // originalSizeButton
            // 
            this.originalSizeButton.Location = new System.Drawing.Point(22, 30);
            this.originalSizeButton.Name = "originalSizeButton";
            this.originalSizeButton.Size = new System.Drawing.Size(30, 19);
            this.originalSizeButton.TabIndex = 14;
            this.originalSizeButton.Text = "1x";
            this.originalSizeButton.Click += new System.EventHandler(this.originalSizeButton_Click);
            // 
            // introEndButton
            // 
            this.introEndButton.Location = new System.Drawing.Point(236, 30);
            this.introEndButton.Name = "introEndButton";
            this.introEndButton.Size = new System.Drawing.Size(38, 19);
            this.introEndButton.TabIndex = 12;
            this.introEndButton.Text = "Intro";
            this.defaultToolTip.SetToolTip(this.introEndButton, "Set the frame where the intro ends");
            this.introEndButton.Click += new System.EventHandler(this.introEndButton_Click);
            // 
            // zoneStartButton
            // 
            this.zoneStartButton.Location = new System.Drawing.Point(70, 30);
            this.zoneStartButton.Name = "zoneStartButton";
            this.zoneStartButton.Size = new System.Drawing.Size(64, 19);
            this.zoneStartButton.TabIndex = 9;
            this.zoneStartButton.Text = "Zone Start";
            this.defaultToolTip.SetToolTip(this.zoneStartButton, "Sets the start frame of a new zone");
            this.zoneStartButton.Click += new System.EventHandler(this.zoneStartButton_Click);
            // 
            // setZoneButton
            // 
            this.setZoneButton.Location = new System.Drawing.Point(204, 30);
            this.setZoneButton.Name = "setZoneButton";
            this.setZoneButton.Size = new System.Drawing.Size(30, 19);
            this.setZoneButton.TabIndex = 9;
            this.setZoneButton.Text = "Set";
            this.defaultToolTip.SetToolTip(this.setZoneButton, "Adds the zone to the codec configuration");
            this.setZoneButton.Click += new System.EventHandler(this.setZoneButton_Click);
            // 
            // zoneEndButton
            // 
            this.zoneEndButton.Location = new System.Drawing.Point(137, 30);
            this.zoneEndButton.Name = "zoneEndButton";
            this.zoneEndButton.Size = new System.Drawing.Size(64, 19);
            this.zoneEndButton.TabIndex = 11;
            this.zoneEndButton.Text = "Zone End";
            this.defaultToolTip.SetToolTip(this.zoneEndButton, "Sets the end frame of a new zone");
            this.zoneEndButton.Click += new System.EventHandler(this.zoneEndButton_Click);
            // 
            // chapterButton
            // 
            this.chapterButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.chapterButton.Location = new System.Drawing.Point(236, 30);
            this.chapterButton.Name = "chapterButton";
            this.chapterButton.Size = new System.Drawing.Size(72, 18);
            this.chapterButton.TabIndex = 13;
            this.chapterButton.Text = "Set Chapter";
            this.defaultToolTip.SetToolTip(this.chapterButton, "Sets the end frame of a new zone");
            this.chapterButton.Click += new System.EventHandler(this.chapterButton_Click);
            // 
            // VideoPlayer
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.ClientSize = new System.Drawing.Size(360, 392);
            this.Controls.Add(this.buttonPanel);
            this.Controls.Add(this.previewGroupbox);
            this.Controls.Add(this.positionSlider);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximumSize = new System.Drawing.Size(1920, 1600);
            this.MinimumSize = new System.Drawing.Size(368, 416);
            this.Name = "VideoPlayer";
            this.ShowIcon = false;
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Show;
            this.Text = "VideoPlayer";
            ((System.ComponentModel.ISupportInitialize)(this.videoPreview)).EndInit();
            this.previewGroupbox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.positionSlider)).EndInit();
            this.buttonPanel.ResumeLayout(false);
            this.buttonPanel.PerformLayout();
            this.ResumeLayout(false);

		}
		
		
		/// <summary>
		/// handles changes in the slider position
		/// updates the currentPosition indicator (this is used by the playback, as the playback thread cannot access
		/// the slider.value in a non GUI context), then reads the appropriate image and displays it
		/// finally, the video position is updated in the window's title
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void positionSlider_Scroll(object sender, System.EventArgs e)
		{
			this.currentPosition = positionSlider.Value;
			Bitmap b = reader.ReadFrameBitmap(currentPosition);
			if (this.left > 0 || this.top > 0 || this.right > 0 || this.bottom > 0) // only crop when necessary
				cropImage(ref b);
            if (this.videoPreview.Image != null)
                this.videoPreview.Image.Dispose(); // get rid of previous bitmap
			this.videoPreview.Image = resizeBitmap(b, this.videoWindowWidth,this.videoWindowHeight);
            b.Dispose ();
			setTitleText();
		}
		/// <summary>
		/// sets the text in the title bar in function of the position, credits and zone settings
		/// </summary>
		private void setTitleText()
		{
            totalTime = Util.converFrameNumberToTimecode(this.positionSlider.Maximum, file.Info.FPS);
            currentTime = Util.converFrameNumberToTimecode(this.positionSlider.Value, file.Info.FPS);
            if (this.zoneStart > -1 || this.zoneEnd > -1)
            {
                this.Text = "Pos: " + positionSlider.Value + "/" + positionSlider.Maximum + " Zone start: ";
                if (zoneStart > -1)
                    this.Text += zoneStart;
                else
                    this.Text += "?";
                this.Text += " end: ";
                if (zoneEnd > -1)
                    this.Text += zoneEnd;
                else
                    this.Text += "?";
            }
            else
                this.Text = "Current position: " + this.positionSlider.Value + "/" + this.positionSlider.Maximum;
            if (this.introEndFrame > -1)
				this.Text += " Intro end: " + this.introEndFrame;
			if (this.creditsStartFrame > -1)
				this.Text += " Credits start: " + this.creditsStartFrame;
            if (mainForm.Settings.AddTimePosition)
                this.Text += "   -   " + currentTime + "/" + totalTime;
		}
		
		
        public void crop(CropValues cropping)
        {
            crop(cropping.left, cropping.top, cropping.right, cropping.bottom);
        }
		/// <summary>
		/// sets the cropping values for this player
		/// </summary>
		/// <param name="left">number of pixels to crop from the left</param>
		/// <param name="top">number of pixels to crop from the top</param>
		/// <param name="right">number of pixels to crop from the right</param>
		/// <param name="bottom">number of pixels to crop from the bottom</param>
		public void crop(int left, int top, int right, int bottom)
		{
			this.left = left;
			this.top = top;
			this.right = right;
			this.bottom = bottom;
			positionSlider_Scroll(null, null);
		}
		/// <summary>
		/// crops the image given as a reference by the values that were previously transmitted
		/// </summary>
		/// <param name="b">the image to where the cropping has to be applied</param>
		private unsafe void cropImage(ref Bitmap b)
		{
			BitmapData image = b.LockBits(new Rectangle(0, 0, b.Width, b.Height), ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb);
			byte* pointer = (byte*)image.Scan0.ToPointer();
			byte* pixel;
			int stride = image.Stride;
			byte white = (byte) Color.White.R;
	
			pixel = pointer;
			int width = b.Width;
			int height = b.Height;
			int width3 = 3 * width;
			int left3 = 3 * left;
			int right3 = 3 * right;

			int lineGap = stride - width3;
			int centerJump = width3 - left3 - right3;
			for (int j = 0; j < top; j++) 
			{
				for (int i = 0; i < width3; i++) 
				{
					*pixel = white;
					pixel++;
				}
				pixel += lineGap;
			}
			int heightb = height - bottom;
			for (int j = top; j < heightb; j++)
			{
				for (int i = 0; i < left3; i++) 
				{
					*pixel = white;
					pixel++;
				}
				pixel += centerJump;
				for (int i = 0; i < right3; i++) 
				{
					*pixel = white;
					pixel++;
				}
				pixel += lineGap;
			}
			for (int j = b.Height-bottom; j < height; j++)
			{
				for (int i = 0; i < width3; i++)
				{
					*pixel = white;
					pixel++;
				}
				pixel += lineGap;
			}
			b.UnlockBits(image);
		}
		
		
		/// <summary>
		/// handles the play button
		/// starts video playback or stops it
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void playButton_Click(object sender, System.EventArgs e)
		{
			if (this.playButton.Text.Equals("Play"))
			{
				this.isRunning = true;
				this.playButton.Text = "Stop";
				new Thread(new ThreadStart(this.playVideo)).Start();
			}
			else
			{
				this.isRunning = false;
				this.playButton.Text = "Play";
			}
		}
		/// <summary>
		/// updates the frame in the picturebox and sets the slider to the position defined in currentPosition
		/// then calls the eventhandler for the slider so that its position is updated
		/// </summary>
		private void updateGUI()
		{
			this.positionSlider.Value = this.currentPosition;
			positionSlider_Scroll(null, null);
		}
		/// <summary>
		/// plays the video
		/// calls updateGUI in the GUI thread context, then goes for sleep for approximately the time a frame should be displayed
		/// this is repeated until isRunning is set to false, which can happen either by pressing the stop button, or
		/// if we have reached the end of the video
		/// </summary>
		private void playVideo()
		{
			while (isRunning)
			{
				try
				{
					if (this.currentPosition + 1 < reader.FrameCount)
						currentPosition++;
					else
						isRunning = false;
					this.videoPreview.Invoke(new SimpleDelegate(updateGUI));
					Thread.Sleep(this.millisecondsPerFrame);
				}
				catch (Exception e)
				{
					Console.Write(e.Message);
				}
			}
		}

        private void safeChangePosition(int frameChange)
        {
            int newFrameNumber = this.currentPosition + frameChange;
            if (newFrameNumber >= 0 && newFrameNumber < reader.FrameCount)
            {
                currentPosition = newFrameNumber;
            }
            else
            {
                if (frameChange > 0) currentPosition = reader.FrameCount - 1;
                else currentPosition = 0;
            }
            updateGUI();
        }

		private void previousFrameButton_Click(object sender, System.EventArgs e)
		{
            safeChangePosition(-1);
		}

		private void nextFrameButton_Click(object sender, System.EventArgs e)
		{
            safeChangePosition(1);
		}

		private void fwdButton_Click(object sender, System.EventArgs e)
		{
            safeChangePosition(-25);
		}

		private void ffButton_Click(object sender, System.EventArgs e)
		{
            safeChangePosition(25);
        }
		
		
		/// <summary>
		/// fires an event indicating the credits start position has been set.
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void creditsStartButton_Click(object sender, System.EventArgs e)
		{
			if (IntroCreditsFrameSet != null)
				IntroCreditsFrameSet(this.currentPosition, true);
		}
		/// <summary>
		/// fires an event indicating that the intro end position has been set
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void introEndButton_Click(object sender, System.EventArgs e)
		{
			if (IntroCreditsFrameSet != null)
				IntroCreditsFrameSet(this.currentPosition, false);
		}
		
		
		private void zoneEndButton_Click(object sender, System.EventArgs e)
		{
			int pos = (int)positionSlider.Value;
			if (creditsStartFrame > -1 && pos >= creditsStartFrame)
			{
				MessageBox.Show("Zone end intersects with credits zone\nPlease adjust zone end or credits zone", "Zone interesection detected", 
					MessageBoxButtons.OK, MessageBoxIcon.Stop);
			}
			else
			{
				zoneEnd = pos;
				setTitleText();
			}
		}

		private void zoneStartButton_Click(object sender, System.EventArgs e)
		{
			int pos = (int)positionSlider.Value;
			if (pos > this.introEndFrame) // else we have an intersection with the credits which is not allowed
			{
				if (this.creditsStartFrame > -1 && pos >= creditsStartFrame) // zone starts inside credits zone, not allowed
				{
					MessageBox.Show("Zone start intersects with credits zone\nPlease adjust zone start or credits zone", "Zone interesection detected", 
						MessageBoxButtons.OK, MessageBoxIcon.Stop);
				}
				else
				{
					this.zoneStart = positionSlider.Value;
					setTitleText();	
				}
			}
			else
				MessageBox.Show("Zone start intersects with with intro zone\nPlease adjust zone start or intro zone.", "Zone intersection detected",
					MessageBoxButtons.OK, MessageBoxIcon.Stop);
		}

		private void setZoneButton_Click(object sender, System.EventArgs e)
		{
			if (ZoneSet != null)
			{
				if (zoneEnd > zoneStart)
				{
					ZoneSet(this.zoneStart, this.zoneEnd);
					this.zoneStart = -1;
					this.zoneEnd = -1;
					setTitleText();
				}
				else
					MessageBox.Show("The end of a zone must be after its start", "Invalid zone configuration", MessageBoxButtons.OK, MessageBoxIcon.Stop);
			}
		}
		private void chapterButton_Click(object sender, System.EventArgs e)
		{
			if (ChapterSet != null)
			{
				ChapterSet(positionSlider.Value);
			}
		}
		
		
		private void mnuIntroEnd_Click(object sender, System.EventArgs e)
		{
			this.positionSlider.Value = this.introEndFrame;
			positionSlider_Scroll(null, null);

		}

		private void mnuCreditsStart_Click(object sender, System.EventArgs e)
		{
			this.positionSlider.Value = this.creditsStartFrame;
			positionSlider_Scroll(null, null);
		}

		private void mnuZoneStart_Click(object sender, System.EventArgs e)
		{
			this.positionSlider.Value = this.zoneStart;
			positionSlider_Scroll(null, null);
		}

		private void mnuZoneEnd_Click(object sender, System.EventArgs e)
		{
			this.positionSlider.Value = this.zoneEnd;
			positionSlider_Scroll(null, null);
		}
		private void contextMenu1_Popup(object sender, System.EventArgs e)
		{
			if (this.introEndFrame > -1)
				this.mnuIntroEnd.Enabled = true;
			else
				this.mnuIntroEnd.Enabled = false;
			if (this.creditsStartFrame > -1)
				this.mnuCreditsStart.Enabled = true;
			else
				this.mnuCreditsStart.Enabled = false;
			if (this.zoneStart > -1)
				this.mnuZoneStart.Enabled = true;
			else
				this.mnuZoneStart.Enabled = false;
			if (this.zoneEnd > -1)
				this.mnuZoneEnd.Enabled = true;
			else
				this.mnuZoneEnd.Enabled = false;
		}
		
		
        public Dar? DAR
        {
            get { return arChooser.Value; }
            set { arChooser.Value = value; }
        }

		/// <summary>
		/// returns the underlying video reader
		/// </summary>
		/// <returns>the VideoReader object used by this window for the preview</returns>
		public IVideoReader Reader
		{
			get {return this.reader;}
		}
        /// <summary>
        /// returns the underlying media file
        /// </summary>
        /// <returns>the IMediaFile used by this window for the preview</returns>
        public IMediaFile File
        {
            get { return this.file; }
        }
		/// <summary>
		/// gets /sets the frame where the credits start
		/// </summary>
		public int CreditsStart
		{
			get {return this.creditsStartFrame;}
			set 
			{
				creditsStartFrame = value;
				setTitleText();
			}
		}
		/// <summary>
		/// gets / sets the frame where the intro ends
		/// </summary>
		public int IntroEnd
		{
			get {return this.introEndFrame;}
			set 
			{
				introEndFrame = value;
				setTitleText();
			}
		}
		/// <summary>
		/// gets / sets the frame where the current zone starts
		/// </summary>
		public int ZoneStart
		{
			get {return this.zoneStart;}
			set 
			{
				zoneStart = value;
				positionSlider_Scroll(null, null);
			}
		}
		/// <summary>
		/// gets / sets the frame where the current zone starts
		/// </summary>
		public int ZoneEnd
		{
			get {return this.zoneEnd;}
			set
			{
				zoneEnd = value;
				setTitleText();
			}
		}
		/// <summary>
		/// gets the framerate of the video that is currently loaded
		/// </summary>
		public double Framerate
		{
            get { return file.Info.FPS; }
		}
		/// <summary>
		/// gets / sets the frame currently visible
		/// </summary>
		public int CurrentFrame
		{
			get {return positionSlider.Value;}
			set 
			{
				if (value <= positionSlider.Maximum && value >= positionSlider.Minimum)
				{
					positionSlider.Value = value;
					positionSlider_Scroll(null, null); // makes the image visible
				}
			}
		}
		

        private void resize(int targetWidth, bool PAR)
        {
            Dar d = new Dar(file.Info.Width, file.Info.Height);
            if (PAR) d = arChooser.Value ?? d;

            int height = (int)Math.Round((decimal)targetWidth / d.ar);

            videoWindowWidth = targetWidth;
            videoWindowHeight = (int)height;
            sizeLock = true;
            adjustSize();
            sizeLock = false;
            positionSlider_Scroll(null, null);
        }

        private void showPAR_CheckedChanged(object sender, EventArgs e)
        {
            resize(videoWindowWidth, showPAR.Checked);
        }

        private void goToFrameButton_Click(object sender, EventArgs e)
        {
            decimal val;
            if (NumberChooser.ShowDialog("Enter a frame number:", "Go to frame",
                0, positionSlider.Minimum, positionSlider.Maximum, positionSlider.Value, out val)
                == DialogResult.OK)
            {
                positionSlider.Value = (int)val;
                positionSlider_Scroll(null, null);
            }
        }

        private void arChooser_SelectionChanged(object sender, string val)
        {
            resize(videoWindowWidth, showPAR.Checked);
        }

        private void nextFrameButton_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Right)
                safeChangePosition(1);
        }

        private void previousFrameButton_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Right)
                safeChangePosition(-1);
        }
	}
}
