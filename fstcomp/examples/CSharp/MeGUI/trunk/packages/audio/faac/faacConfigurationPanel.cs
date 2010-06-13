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

namespace MeGUI.packages.audio.faac
{
    public partial class faacConfigurationPanel : MeGUI.core.details.audio.AudioConfigurationPanel, Editable<FaacSettings>
    {
        public faacConfigurationPanel():base()
        {
            InitializeComponent();
            cbrBitrate.DataSource = FaacSettings.SupportedBitrates;
            cbrBitrate.BindingContext = new BindingContext();
        }
	    
		/// <summary>
		/// gets / sets the settings that are being shown in this configuration dialog
		/// </summary>
		protected override AudioCodecSettings CodecSettings
		{
			get
			{
				FaacSettings fas = new FaacSettings();
                fas.BitrateMode = qualityModeRadioButton.Checked ? BitrateManagementMode.VBR : BitrateManagementMode.ABR;
                fas.Bitrate = (int)cbrBitrate.SelectedItem;
				fas.Quality = vbrQuality.Value;
				return fas;
			}
			set
			{
                FaacSettings fas = value as FaacSettings;
                cbrBitrate.SelectedItem = Array.IndexOf(FaacSettings.SupportedBitrates, fas.Bitrate) < 0 ? FaacSettings.SupportedBitrates[0] : fas.Bitrate;
                vbrQuality.Value = fas.Quality;
                qualityModeRadioButton.Checked = !(cbrBitrateRadioButton.Checked = (fas.BitrateMode != BitrateManagementMode.VBR));
                bitrateModeChanged(null, null);
			}
		}
		
        
        private void bitrateModeChanged(object sender, EventArgs e)
        {
            cbrBitrate.Enabled = !(vbrQuality.Enabled = qualityModeRadioButton.Checked);
        }
        

        

        FaacSettings Editable<FaacSettings>.Settings
        {
            get
            {
                return (FaacSettings)Settings;
            }
            set
            {
                Settings = value;
            }
        }

        
    }
}




