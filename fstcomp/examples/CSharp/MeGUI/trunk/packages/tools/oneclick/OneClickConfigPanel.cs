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
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;



using MeGUI.core.details.video;
using MeGUI.core.plugins.interfaces;
using MeGUI.core.util;

namespace MeGUI.packages.tools.oneclick
{
    public partial class OneClickConfigPanel : UserControl, Editable<OneClickSettings>
    {
        private MainForm mainForm;
        #region profiles
        #region AVS profiles
        private void initAvsHandler()
        {
            // Init AVS handlers
            avsProfile.Manager = mainForm.Profiles;
        }
        #endregion
        #region Video profiles
        private void initVideoHandler()
        {
            videoProfile.Manager = mainForm.Profiles;
        }
        #endregion
        #region Audio profiles
        private void initAudioHandler()
        {
            audioProfile.Manager = mainForm.Profiles;
        }
        #endregion
        #endregion
        
        public OneClickConfigPanel() 
        {
            InitializeComponent();
            mainForm = MainForm.Instance;
            // We do this because the designer will attempt to put such a long string in the resources otherwise
            containerFormatLabel.Text = "Since the possible output filetypes are not known until the input is configured, the output type cannot be configured in a profile. Instead, here is a list of known file-types. You choose which you are happy with, and MeGUI will attempt to encode to one of those on the list.";

            foreach (ContainerType t in mainForm.MuxProvider.GetSupportedContainers())
                containerTypeList.Items.Add(t.ToString());
            initAudioHandler();
            initAvsHandler();
            initVideoHandler();
        }

        #region Gettable<OneClickSettings> Members

        public OneClickSettings Settings
        {
            get
            {
                OneClickSettings val = new OneClickSettings();
                val.AudioProfileName = audioProfile.SelectedProfile.FQName;
                val.AutomaticDeinterlacing = autoDeint.Checked;
                val.AvsProfileName = avsProfile.SelectedProfile.FQName;
                val.ContainerCandidates = ContainerCandidates;
                val.DontEncodeAudio = dontEncodeAudio.Checked;
                val.Filesize = fileSize.Value;
                val.OutputResolution = (long)horizontalResolution.Value;
                val.PrerenderVideo = preprocessVideo.Checked;
                val.SignalAR = signalAR.Checked;
                val.SplitSize = splitSize.Value;
                val.AutoCrop = autoCrop.Checked;
                val.KeepInputResolution = keepInputResolution.Checked;
                val.VideoProfileName = videoProfile.SelectedProfile.FQName;
                val.UseChaptersMarks = usechaptersmarks.Checked;
                return val;
            }
            set
            {
                audioProfile.SetProfileNameOrWarn(value.AudioProfileName);
                autoDeint.Checked = value.AutomaticDeinterlacing;
                avsProfile.SetProfileNameOrWarn(value.AvsProfileName);
                ContainerCandidates = value.ContainerCandidates; 
                dontEncodeAudio.Checked = value.DontEncodeAudio;
                fileSize.Value = value.Filesize;
                horizontalResolution.Value = value.OutputResolution;
                preprocessVideo.Checked = value.PrerenderVideo;
                signalAR.Checked = value.SignalAR;
                splitSize.Value = value.SplitSize;
                autoCrop.Checked = value.AutoCrop;
                keepInputResolution.Checked = value.KeepInputResolution;
                videoProfile.SetProfileNameOrWarn(value.VideoProfileName);
                usechaptersmarks.Checked = value.UseChaptersMarks;
            }
        }

        #endregion

        private void dontEncodeAudio_CheckedChanged(object sender, EventArgs e)
        {
            audioProfile.Enabled = !dontEncodeAudio.Checked;
        }

        private string[] ContainerCandidates
        {
            get
            {
                string[] val = new string[containerTypeList.CheckedItems.Count];
                containerTypeList.CheckedItems.CopyTo(val, 0);
                return val;
            }
            set
            {
                for (int i = 0; i < containerTypeList.Items.Count; i++)
                    containerTypeList.SetItemChecked(i, false);

                foreach (string val in value)
                {
                    int index = containerTypeList.Items.IndexOf(val);
                    if (index > -1)
                        containerTypeList.SetItemChecked(index, true);
                }
            }
        }

        private void keepInputResolution_CheckedChanged(object sender, EventArgs e)
        {
            if (keepInputResolution.Checked)
            {
                horizontalResolution.Enabled = false;
                autoCrop.Checked = false;
                autoCrop.Enabled = false;
                signalAR.Enabled = false;
                signalAR.Checked = false;
            }
            else
            {
                horizontalResolution.Enabled = true;
                autoCrop.Checked = true;
                autoCrop.Enabled = true;
                signalAR.Enabled = true;
            }
        }

        private void videoProfile_SelectedProfileChanged(object sender, EventArgs e)
        {
            if (Settings.VideoProfileName.StartsWith("x264"))
                usechaptersmarks.Enabled = true;
            else
            {
                usechaptersmarks.Enabled = false;
                usechaptersmarks.Checked = usechaptersmarks.Enabled;
            }
        }
    }
}
