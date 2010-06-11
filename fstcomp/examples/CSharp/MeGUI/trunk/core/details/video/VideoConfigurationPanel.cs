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
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using MeGUI.core.plugins.interfaces;

namespace MeGUI.core.details.video
{
    public class VideoConfigurationPanel : UserControl
    {
        #region variables
        protected bool updating = false;
        private double bytesPerFrame;
        private bool advancedToolTips;
        protected int lastEncodingMode = 0;

        private bool loaded;
        protected string input = "input", output = "output", encoderPath = "program";
        #endregion
        protected ToolTip tooltipHelp;
        private IContainer components;
        #region start / stop
        public VideoConfigurationPanel()
            : this(null, new VideoInfo())
        { }


        public VideoConfigurationPanel(MainForm mainForm, VideoInfo info)
        {
            loaded = false;
            InitializeComponent();

            input = info.VideoInput;
            output = info.VideoOutput;
        }

        private void VideoConfigurationPanel_Load(object sender, EventArgs e)
        {
            loaded = true;
            doCodecSpecificLoadAdjustments();
            genericUpdate();
        }

        #endregion
        #region codec specific adjustments

        /// <summary>
        /// Generates the commandline
        /// </summary>
        /// <returns></returns>
        protected virtual string getCommandline() {
            return "";
        }

        /// <summary>
        /// The method by which codecs can do their pre-commandline generation adjustments (eg tri-state adjustment).
        /// </summary>
        protected virtual void doCodecSpecificAdjustments() { }

        /// <summary>
        /// The method by which codecs can add things to the Load event
        /// </summary>
        protected virtual void doCodecSpecificLoadAdjustments() { }
        
        /// <summary>
        /// Returns whether settings is a valid settings object for this instance. Should be implemented by one line:
        /// return (settings is xxxxSettings);
        /// </summary>
        /// <param name="settings">The settings to check</param>
        /// <returns>Whether the settings are valid</returns>
        protected virtual bool isValidSettings(VideoCodecSettings settings)
        {
            throw new Exception("A bug in the program -- ProfilableConfigurationDialog.isValidSettings(GenericSettings) is not overridden");
        }

        /// <summary>
        /// Returns a new instance of the codec settings. This must be specific to the type of the config dialog, so
        /// that it can be set with the Settings.set property.
        /// </summary>
        /// <returns>A new instance of xxxSettings</returns>
        protected virtual VideoCodecSettings defaultSettings()
        {
            throw new Exception("A bug in the program -- ProfilableConfigurationDialog.defaultSettings() is not overridden");
        }
        #endregion
        #region showCommandline
        protected void showCommandLine()
        {
            if (!loaded)
                return;
            if (updating)
                return;
            updating = true;

            doCodecSpecificAdjustments();

            this.commandline.Text = encoderPath + " " + getCommandline();
            updating = false;
        }
        #endregion
        #region GUI events
        protected void genericUpdate()
        {
            showCommandLine();
        }
        #endregion
        #region properties

        public bool AdvancedToolTips
        {
            get { return advancedToolTips; }
            set { advancedToolTips = value; }
        }

        public double BytesPerFrame
        {
            get { return bytesPerFrame; }
            set { bytesPerFrame = value; }
        }

        #endregion




        protected TabControl tabControl1;
        protected TextBox commandline;
        protected TabPage mainTabPage;
    

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.mainTabPage = new System.Windows.Forms.TabPage();
            this.commandline = new System.Windows.Forms.TextBox();
            this.tooltipHelp = new System.Windows.Forms.ToolTip(this.components);
            this.tabControl1.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.tabControl1.Controls.Add(this.mainTabPage);
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(368, 344);
            this.tabControl1.TabIndex = 39;
            // 
            // mainTabPage
            // 
            this.mainTabPage.Location = new System.Drawing.Point(4, 22);
            this.mainTabPage.Name = "mainTabPage";
            this.mainTabPage.Padding = new System.Windows.Forms.Padding(3);
            this.mainTabPage.Size = new System.Drawing.Size(360, 318);
            this.mainTabPage.TabIndex = 0;
            this.mainTabPage.Text = "Main";
            this.mainTabPage.UseVisualStyleBackColor = true;
            // 
            // commandline
            // 
            this.commandline.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.commandline.Location = new System.Drawing.Point(3, 350);
            this.commandline.Multiline = true;
            this.commandline.Name = "commandline";
            this.commandline.ReadOnly = true;
            this.commandline.Size = new System.Drawing.Size(365, 59);
            this.commandline.TabIndex = 41;
            // 
            // tooltipHelp
            // 
            this.tooltipHelp.AutoPopDelay = 30000;
            this.tooltipHelp.InitialDelay = 500;
            this.tooltipHelp.IsBalloon = true;
            this.tooltipHelp.ReshowDelay = 100;
            this.tooltipHelp.ShowAlways = true;
            // 
            // VideoConfigurationPanel
            // 
            this.Controls.Add(this.commandline);
            this.Controls.Add(this.tabControl1);
            this.Name = "VideoConfigurationPanel";
            this.Size = new System.Drawing.Size(372, 409);
            this.Load += new System.EventHandler(this.VideoConfigurationPanel_Load);
            this.tabControl1.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }
    }
}
