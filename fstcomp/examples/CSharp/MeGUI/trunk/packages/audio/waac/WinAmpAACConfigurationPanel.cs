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

namespace MeGUI.packages.audio.waac
{
    public partial class WinAmpAACConfigurationPanel : MeGUI.core.details.audio.AudioConfigurationPanel, Editable<WinAmpAACSettings>
    {
        public WinAmpAACConfigurationPanel():base()
        {
            InitializeComponent();
            comboBox1.Items.AddRange(EnumProxy.CreateArray(WinAmpAACSettings.SupportedProfiles));
            comboBox2.Items.AddRange(EnumProxy.CreateArray(typeof(WinAmpAACSettings.AacStereoMode)));
            vBitrate_ValueChanged(null, null);
        }
        #region properties
        /// <summary>
        /// gets / sets the settings that are being shown in this configuration dialog
        /// </summary>
        protected override AudioCodecSettings CodecSettings
        {
            get
            {
                WinAmpAACSettings nas = new WinAmpAACSettings();
                nas.Mpeg2AAC = checkBox2.Checked;
                nas.Profile = (AacProfile)(comboBox1.SelectedItem as EnumProxy).RealValue;
                nas.StereoMode = (WinAmpAACSettings.AacStereoMode)(comboBox2.SelectedItem as EnumProxy).RealValue;
                nas.Bitrate = vBitrate.Value;
                return nas;
            }
            set
            {
                WinAmpAACSettings nas = value as WinAmpAACSettings;
                checkBox2.Checked = nas.Mpeg2AAC;
                comboBox1.SelectedItem = EnumProxy.Create(nas.Profile);
                comboBox2.SelectedItem = EnumProxy.Create(nas.StereoMode);
                vBitrate.Value = Math.Max(Math.Min(nas.Bitrate, vBitrate.Maximum), vBitrate.Minimum);
            }
        }
        #endregion

        private void vBitrate_ValueChanged(object sender, EventArgs e)
        {
            label3.Text = "CBR @ " + vBitrate.Value + " kbps";
        }



        #region Editable<WinAmpAACSettings> Members

        WinAmpAACSettings Editable<WinAmpAACSettings>.Settings
        {
            get
            {
                return (WinAmpAACSettings)Settings;
            }
            set
            {
                Settings = value;
            }
        }

        #endregion
    }
}




