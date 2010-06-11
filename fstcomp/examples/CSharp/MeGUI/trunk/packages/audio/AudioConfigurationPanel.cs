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

namespace MeGUI.core.details.audio
{
    public partial class AudioConfigurationPanel : UserControl
    {
        private EnumProxy[] _avisynthChannelSet;
	    
        #region start / stop

	    public AudioConfigurationPanel()
	    {
            _avisynthChannelSet =
                EnumProxy.CreateArray(
                    this.IsMultichanelSupported
                    ?
                    (
                    this.IsMultichanelRequed
                    ?
                    new object[]{
                                ChannelMode.Upmix,
                                ChannelMode.UpmixUsingSoxEq,
                                ChannelMode.UpmixWithCenterChannelDialog
                    }
                    :
                    new object[]{
                                ChannelMode.KeepOriginal,
                                ChannelMode.StereoDownmix,
                                ChannelMode.DPLDownmix,
                                ChannelMode.DPLIIDownmix,
                                ChannelMode.ConvertToMono,
                                ChannelMode.Upmix,
                                ChannelMode.UpmixUsingSoxEq,
                                ChannelMode.UpmixWithCenterChannelDialog
                    }

                    )
                                :
                    new object[]{
                                ChannelMode.KeepOriginal,
                                ChannelMode.StereoDownmix,
                                ChannelMode.DPLDownmix,
                                ChannelMode.DPLIIDownmix,
                                ChannelMode.ConvertToMono
                    }
                    );

            InitializeComponent();
            this.besweetDownmixMode.DataSource = _avisynthChannelSet;
            this.besweetDownmixMode.BindingContext = new BindingContext();
            this.cbSampleRate.SelectedIndex = 0;
        }
	    
		#endregion
		#region dropdowns
			
		#endregion
		#region checkboxes

        protected void showCommandLine()
	    {
	        
	    }

		#endregion
		#region properties


	    protected virtual bool IsMultichanelSupported
	    {
	        get
	        {
                return true;
	        }
	    }

        protected virtual bool IsMultichanelRequed
        {
            get
            {
                return false;
            }
        }

        /// <summary>
        /// Must collect data from UI / Fill UI from Data
        /// </summary>
        [Browsable(false)]
        protected virtual AudioCodecSettings CodecSettings
        {
            get
            {
                throw new NotImplementedException("Must be overridden");
            }
            set
            {
                throw new NotImplementedException("Must be overridden");
            }
        }

	    
		/// <summary>
		/// gets / sets the settings that are being shown in this configuration dialog
		/// </summary>
        [Browsable(false)]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
		public AudioCodecSettings Settings
		{
			get
			{
                AudioCodecSettings fas = CodecSettings;
                fas.ForceDecodingViaDirectShow = forceDShowDecoding.Checked;
                EnumProxy o = besweetDownmixMode.SelectedItem as EnumProxy;
			    if(o != null)
				    fas.DownmixMode = (ChannelMode)o.RealValue ;
				fas.AutoGain = autoGain.Checked;
                fas.SampleRateType = cbSampleRate.SelectedIndex;
                fas.ApplyDRC = applyDRC.Checked;
                fas.Normalize = (int)normalize.Value;
				return fas;
			}
			set
			{
				AudioCodecSettings fas = value;
                besweetDownmixMode.SelectedItem = EnumProxy.Create(fas.DownmixMode);
                forceDShowDecoding.Checked = fas.ForceDecodingViaDirectShow;
				autoGain.Checked = fas.AutoGain;
                cbSampleRate.SelectedIndex = fas.SampleRateType;
                applyDRC.Checked = fas.ApplyDRC;
                normalize.Value = fas.Normalize;
                CodecSettings = fas;
			}
		}

		#endregion
		#region buttons
		/// <summary>
		/// handles entires into textfiels, blocks entry of non digit characters
		/// </summary>
		/// <param name="sender"></param>
		/// <param name="e"></param>
		private void textField_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (! char.IsDigit(e.KeyChar) && (int)Keys.Back != (int)e.KeyChar)
				e.Handled = true;
		}
		#endregion
		#region commandline
		private void besweetDelay_TextChanged(object sender, System.EventArgs e)
		{
		}
		#endregion

        private void applyDRC_CheckedChanged(object sender, EventArgs e)
        {
            autoGain.Checked = applyDRC.Checked;
            autoGain_CheckedChanged(sender, e);
        }

        private void autoGain_CheckedChanged(object sender, EventArgs e)
        {
            normalize.Enabled = autoGain.Checked;
        }
    }
}
