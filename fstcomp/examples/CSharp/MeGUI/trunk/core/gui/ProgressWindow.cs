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
using System.Windows.Forms;

using MeGUI.core.util;

namespace MeGUI
{
	public delegate void WindowClosedCallback(bool hideOnly); // delegate for WindowClosed event
	public delegate void AbortCallback(); // delegate for Abort event
	public delegate void UpdateStatusCallback(StatusUpdate su); // deletgate for UpdateStatus event
	public delegate void PriorityChangedCallback(ProcessPriority priority); // delegate for PriorityChanged event
	/// <summary>
	/// ProgressWindow is a window that is being shown during encoding and shows the current encoding status
	/// it is being fed by UpdateStatus events fired from the main GUI class (Form1)
	/// </summary>
	public class ProgressWindow : System.Windows.Forms.Form
    {
        #region variables
		public event AbortCallback Abort; // event fired if the abort button has been pressed
		public event PriorityChangedCallback PriorityChanged; // event fired if the priority dropdown has changed
		private System.Windows.Forms.Label currentVideoFrameLabel;
		private System.Windows.Forms.TextBox currentVideoFrame;
		private System.Windows.Forms.GroupBox groupBox1;
		private System.Windows.Forms.Button abortButton;
		private System.Windows.Forms.Label videoDataLabel;
        private System.Windows.Forms.TextBox videoData;
		private System.Windows.Forms.Label fpsLabel;
		private System.Windows.Forms.TextBox fps;
		private System.Windows.Forms.Label timeElapsedLabel;
		private System.Windows.Forms.TextBox timeElapsed;
		private System.Windows.Forms.Label totalTimeLabel;
		private System.Windows.Forms.TextBox totalTime;
		private System.Windows.Forms.Label progressLabel;
		private System.Windows.Forms.ProgressBar progress;
		private System.Windows.Forms.Label priorityLabel;
		private System.Windows.Forms.ComboBox priority;
		private bool isUserClosing;
        private TextBox positionInClip;
        private Label currentPositionLabel;
        private StatusStrip statusStrip1;
        private ToolStripStatusLabel statusLabel;
        private MeGUI.core.gui.HelpButton helpButton1;
        private ITaskbarList3 taskbarProgress;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
        #endregion
        #region start / stop
        /// <summary>
		/// default constructor, initializes the GUI components
		/// </summary>
		public ProgressWindow(JobTypes type)
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
			isUserClosing = true;
            if ((Environment.OSVersion.Version.Major == 6 && Environment.OSVersion.Version.Minor >= 1) 
                || Environment.OSVersion.Version.Major > 6)
                taskbarProgress = (ITaskbarList3)new ProgressTaskbar();
		}
		/// <summary>
		/// handles the onclosing event
		/// ensures that if the user closed the window, it will only be hidden
		/// whereas if the system closed it, it is allowed to close
		/// </summary>
		/// <param name="e"></param>
        protected override void OnClosing(CancelEventArgs e)
        {
            if (this.IsUserAbort)
            {
                e.Cancel = true;
                this.Hide();
            }
            else
            {
                base.OnClosing(e);
            }
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ProgressWindow));
            this.currentVideoFrameLabel = new System.Windows.Forms.Label();
            this.currentVideoFrame = new System.Windows.Forms.TextBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.totalTime = new System.Windows.Forms.TextBox();
            this.totalTimeLabel = new System.Windows.Forms.Label();
            this.timeElapsed = new System.Windows.Forms.TextBox();
            this.timeElapsedLabel = new System.Windows.Forms.Label();
            this.fps = new System.Windows.Forms.TextBox();
            this.fpsLabel = new System.Windows.Forms.Label();
            this.videoData = new System.Windows.Forms.TextBox();
            this.videoDataLabel = new System.Windows.Forms.Label();
            this.currentPositionLabel = new System.Windows.Forms.Label();
            this.positionInClip = new System.Windows.Forms.TextBox();
            this.abortButton = new System.Windows.Forms.Button();
            this.progressLabel = new System.Windows.Forms.Label();
            this.progress = new System.Windows.Forms.ProgressBar();
            this.priorityLabel = new System.Windows.Forms.Label();
            this.priority = new System.Windows.Forms.ComboBox();
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.statusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.helpButton1 = new MeGUI.core.gui.HelpButton();
            this.groupBox1.SuspendLayout();
            this.statusStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // currentVideoFrameLabel
            // 
            this.currentVideoFrameLabel.AutoSize = true;
            this.currentVideoFrameLabel.Location = new System.Drawing.Point(8, 44);
            this.currentVideoFrameLabel.Name = "currentVideoFrameLabel";
            this.currentVideoFrameLabel.Size = new System.Drawing.Size(118, 13);
            this.currentVideoFrameLabel.TabIndex = 2;
            this.currentVideoFrameLabel.Text = "Current / Total frames:";
            // 
            // currentVideoFrame
            // 
            this.currentVideoFrame.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.currentVideoFrame.Location = new System.Drawing.Point(173, 41);
            this.currentVideoFrame.Name = "currentVideoFrame";
            this.currentVideoFrame.ReadOnly = true;
            this.currentVideoFrame.Size = new System.Drawing.Size(130, 21);
            this.currentVideoFrame.TabIndex = 17;
            this.currentVideoFrame.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // groupBox1
            // 
            this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox1.Controls.Add(this.totalTime);
            this.groupBox1.Controls.Add(this.totalTimeLabel);
            this.groupBox1.Controls.Add(this.timeElapsed);
            this.groupBox1.Controls.Add(this.timeElapsedLabel);
            this.groupBox1.Controls.Add(this.fps);
            this.groupBox1.Controls.Add(this.fpsLabel);
            this.groupBox1.Controls.Add(this.videoData);
            this.groupBox1.Controls.Add(this.videoDataLabel);
            this.groupBox1.Controls.Add(this.currentPositionLabel);
            this.groupBox1.Controls.Add(this.currentVideoFrame);
            this.groupBox1.Controls.Add(this.currentVideoFrameLabel);
            this.groupBox1.Controls.Add(this.positionInClip);
            this.groupBox1.Location = new System.Drawing.Point(8, 8);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(309, 177);
            this.groupBox1.TabIndex = 3;
            this.groupBox1.TabStop = false;
            // 
            // totalTime
            // 
            this.totalTime.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.totalTime.Location = new System.Drawing.Point(173, 149);
            this.totalTime.Name = "totalTime";
            this.totalTime.ReadOnly = true;
            this.totalTime.Size = new System.Drawing.Size(130, 21);
            this.totalTime.TabIndex = 25;
            this.totalTime.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // totalTimeLabel
            // 
            this.totalTimeLabel.AutoSize = true;
            this.totalTimeLabel.Location = new System.Drawing.Point(8, 152);
            this.totalTimeLabel.Name = "totalTimeLabel";
            this.totalTimeLabel.Size = new System.Drawing.Size(82, 13);
            this.totalTimeLabel.TabIndex = 10;
            this.totalTimeLabel.Text = "Time remaining:";
            // 
            // timeElapsed
            // 
            this.timeElapsed.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.timeElapsed.Location = new System.Drawing.Point(173, 122);
            this.timeElapsed.Name = "timeElapsed";
            this.timeElapsed.ReadOnly = true;
            this.timeElapsed.Size = new System.Drawing.Size(130, 21);
            this.timeElapsed.TabIndex = 23;
            this.timeElapsed.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // timeElapsedLabel
            // 
            this.timeElapsedLabel.AutoSize = true;
            this.timeElapsedLabel.Location = new System.Drawing.Point(8, 125);
            this.timeElapsedLabel.Name = "timeElapsedLabel";
            this.timeElapsedLabel.Size = new System.Drawing.Size(73, 13);
            this.timeElapsedLabel.TabIndex = 8;
            this.timeElapsedLabel.Text = "Time elapsed:";
            // 
            // fps
            // 
            this.fps.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.fps.Location = new System.Drawing.Point(173, 95);
            this.fps.Name = "fps";
            this.fps.ReadOnly = true;
            this.fps.Size = new System.Drawing.Size(130, 21);
            this.fps.TabIndex = 21;
            this.fps.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // fpsLabel
            // 
            this.fpsLabel.AutoSize = true;
            this.fpsLabel.Location = new System.Drawing.Point(8, 98);
            this.fpsLabel.Name = "fpsLabel";
            this.fpsLabel.Size = new System.Drawing.Size(85, 13);
            this.fpsLabel.TabIndex = 6;
            this.fpsLabel.Text = "Processing rate:";
            // 
            // videoData
            // 
            this.videoData.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.videoData.Location = new System.Drawing.Point(173, 68);
            this.videoData.Name = "videoData";
            this.videoData.ReadOnly = true;
            this.videoData.Size = new System.Drawing.Size(130, 21);
            this.videoData.TabIndex = 19;
            this.videoData.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // videoDataLabel
            // 
            this.videoDataLabel.AutoSize = true;
            this.videoDataLabel.Location = new System.Drawing.Point(8, 71);
            this.videoDataLabel.Name = "videoDataLabel";
            this.videoDataLabel.Size = new System.Drawing.Size(139, 13);
            this.videoDataLabel.TabIndex = 4;
            this.videoDataLabel.Text = "Current / Projected filesize:";
            // 
            // currentPositionLabel
            // 
            this.currentPositionLabel.AutoSize = true;
            this.currentPositionLabel.Location = new System.Drawing.Point(8, 17);
            this.currentPositionLabel.Name = "currentPositionLabel";
            this.currentPositionLabel.Size = new System.Drawing.Size(144, 13);
            this.currentPositionLabel.TabIndex = 0;
            this.currentPositionLabel.Text = "Position in clip / Total length:";
            // 
            // positionInClip
            // 
            this.positionInClip.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.positionInClip.Location = new System.Drawing.Point(173, 14);
            this.positionInClip.Name = "positionInClip";
            this.positionInClip.ReadOnly = true;
            this.positionInClip.Size = new System.Drawing.Size(130, 21);
            this.positionInClip.TabIndex = 15;
            this.positionInClip.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // abortButton
            // 
            this.abortButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.abortButton.Location = new System.Drawing.Point(239, 252);
            this.abortButton.Name = "abortButton";
            this.abortButton.Size = new System.Drawing.Size(75, 23);
            this.abortButton.TabIndex = 6;
            this.abortButton.Text = "Abort";
            this.abortButton.Click += new System.EventHandler(this.abortButton_Click);
            // 
            // progressLabel
            // 
            this.progressLabel.Location = new System.Drawing.Point(12, 195);
            this.progressLabel.Name = "progressLabel";
            this.progressLabel.Size = new System.Drawing.Size(100, 15);
            this.progressLabel.TabIndex = 1;
            this.progressLabel.Text = "Progress";
            // 
            // progress
            // 
            this.progress.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.progress.Location = new System.Drawing.Point(122, 191);
            this.progress.Name = "progress";
            this.progress.Size = new System.Drawing.Size(195, 23);
            this.progress.TabIndex = 1;
            // 
            // priorityLabel
            // 
            this.priorityLabel.Location = new System.Drawing.Point(12, 223);
            this.priorityLabel.Name = "priorityLabel";
            this.priorityLabel.Size = new System.Drawing.Size(100, 15);
            this.priorityLabel.TabIndex = 3;
            this.priorityLabel.Text = "Priority";
            // 
            // priority
            // 
            this.priority.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.priority.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.priority.Items.AddRange(new object[] {
            "LOW",
            "BELOW NORMAL",
            "NORMAL",
            "ABOVE NORMAL",
            "HIGH"});
            this.priority.Location = new System.Drawing.Point(122, 220);
            this.priority.Name = "priority";
            this.priority.Size = new System.Drawing.Size(122, 21);
            this.priority.TabIndex = 2;
            this.priority.SelectedIndexChanged += new System.EventHandler(this.priority_SelectedIndexChanged);
            // 
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.statusLabel});
            this.statusStrip1.Location = new System.Drawing.Point(0, 284);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(326, 22);
            this.statusStrip1.SizingGrip = false;
            this.statusStrip1.TabIndex = 7;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // statusLabel
            // 
            this.statusLabel.Name = "statusLabel";
            this.statusLabel.Size = new System.Drawing.Size(45, 17);
            this.statusLabel.Text = "Status: ";
            // 
            // helpButton1
            // 
            this.helpButton1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.helpButton1.ArticleName = "Status window";
            this.helpButton1.AutoSize = true;
            this.helpButton1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.helpButton1.Location = new System.Drawing.Point(12, 252);
            this.helpButton1.Name = "helpButton1";
            this.helpButton1.Size = new System.Drawing.Size(44, 23);
            this.helpButton1.TabIndex = 5;
            // 
            // ProgressWindow
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.ClientSize = new System.Drawing.Size(326, 306);
            this.Controls.Add(this.helpButton1);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.priority);
            this.Controls.Add(this.priorityLabel);
            this.Controls.Add(this.progress);
            this.Controls.Add(this.progressLabel);
            this.Controls.Add(this.abortButton);
            this.Controls.Add(this.groupBox1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimumSize = new System.Drawing.Size(332, 330);
            this.Name = "ProgressWindow";
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
            this.Text = "Status";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion
        #region statusupdate processing
        /// <summary>
		/// catches the StatusUpdate event fired from Form1 and updates the GUI accordingly
		/// </summary>
		/// <param name="su"></param>
		public void UpdateStatus(StatusUpdate su)
		{
            try
            {

                // Current position
                positionInClip.Text = (Util.ToString(su.ClipPosition) ?? "---") +
                    " / " + (Util.ToString(su.ClipLength) ?? "---");

                // Current frame
                currentVideoFrame.Text = (Util.ToString(su.NbFramesDone) ?? "---") +
                    " / " + (Util.ToString(su.NbFramesTotal) ?? "---");

                // Data
                videoData.Text = (su.CurrentFileSize.HasValue ? su.CurrentFileSize.Value.ToString() : "---") +
                    " / " + (su.ProjectedFileSize.HasValue ? su.ProjectedFileSize.Value.ToString() : "---");

                // Processing speed
                fps.Text = su.ProcessingSpeed ?? "---";

                // Time elapsed 
                // Now we use TotalHours to avoid 24h+ resets...
                if (su.TimeElapsed.TotalHours > 24)
                    timeElapsed.Text = string.Format("{0:00}:{1:00}:{2:00}:{3:00}", (int)su.TimeElapsed.TotalDays, su.TimeElapsed.Hours, su.TimeElapsed.Minutes, su.TimeElapsed.Seconds);
                else
                    timeElapsed.Text = string.Format("{0:00}:{1:00}:{2:00}", (int)su.TimeElapsed.Hours, su.TimeElapsed.Minutes, su.TimeElapsed.Seconds);

                // Estimated time
                // go back to the old function ;-)
                totalTime.Text = getTimeString(su.TimeElapsed, su.PercentageDoneExact ?? 0M);

                this.Text = "Status: " + (su.PercentageDoneExact ?? 0M).ToString("##.##") + " %";
                statusLabel.Text = su.Status ?? "";

                progress.Value = su.PercentageDone;

                if ((Environment.OSVersion.Version.Major == 6 && Environment.OSVersion.Version.Minor >= 1)
                    || Environment.OSVersion.Version.Major > 6)
                    taskbarProgress.SetProgressValue(this.Handle, Convert.ToUInt64(su.PercentageDone), 100);
            }
            catch (Exception) { }
        }
        #endregion
        #region helper methods
        
        /// <summary>
		/// calculates the remaining encoding time from the elapsed timespan and the percentage the job is done
		/// </summary>
		/// <param name="span">timespan elapsed since the start of the job</param>
		/// <param name="percentageDone">percentage the job is currently done</param>
		/// <returns>presentable string in hh:mm:ss format</returns>
		private string getTimeString(TimeSpan span, decimal percentageDone)
		{
			if (percentageDone == 0)
				return "---";
			else
			{
                long ratio = (long)((decimal)span.Ticks / percentageDone * 100M);
				TimeSpan t = new TimeSpan(ratio - span.Ticks);
                string retval = "";
                if (t.TotalHours > 24)
                {
                    retval += string.Format("{0:00}:{1:00}:{2:00}:{3:00}", (int)t.TotalDays, t.Hours, t.Minutes, t.Seconds);
                }
                else
                {
                    retval += string.Format("{0:00}:{1:00}:{2:00}", (int)t.Hours, t.Minutes, t.Seconds);
                }
				return retval;
			}
		}         

        private bool isSettingPriority = false;
        /// <summary>
        /// sets the priority
        /// </summary>
        /// <param name="priority"></param>
        public void setPriority(ProcessPriority priority)
        {
            Util.ThreadSafeRun(this.priority, delegate {
                isSettingPriority = true;
                this.priority.SelectedIndex = (int)priority;
                isSettingPriority = false;
            });
        }
        #endregion
        #region events
        /// <summary>
		///  handles the abort button
		///  fires an Abort() event to the main GUI, which looks up the encoder and makes it stop
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void abortButton_Click(object sender, System.EventArgs e)
		{
		    Abort();
		}
		/// <summary>Handles changes in the priority dropdwon</summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
        private void priority_SelectedIndexChanged(object sender, System.EventArgs e)
        {
            if (PriorityChanged != null && !isSettingPriority)
            {
                if (!WarnPriority((ProcessPriority)priority.SelectedIndex))
                {
                    // priority.Tag contains previous SelectedIndex
                    setPriority((ProcessPriority)priority.Tag);
                    return;
                }
                else
                {
                    PriorityChanged((ProcessPriority)priority.SelectedIndex);
                }
                
                priority.Tag = priority.SelectedIndex;
            }
		}

		private bool WarnPriority(ProcessPriority priority)
		{
            if (priority == ProcessPriority.HIGH)
			{
			    // when user selected 'HIGH' priority
				DialogResult res = MessageBox.Show("On Windows System, running processes at high priority causes them to compete against the window manager and compositor processes. Are you sure you want to proceed?", "MeGUI", MessageBoxButtons.YesNo, MessageBoxIcon.Warning);
                return res == DialogResult.Yes;
			}
			else return true;
 		}
		#endregion
        #region properties
        /// <summary>
		/// gets / sets whether the user closed this window or if the system is closing it
		/// </summary>
		public bool IsUserAbort
		{
			get {return isUserClosing;}
			set {isUserClosing = value;}
        }
        #endregion
    }
}
