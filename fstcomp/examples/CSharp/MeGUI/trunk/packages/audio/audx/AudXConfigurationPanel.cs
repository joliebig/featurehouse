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

namespace MeGUI.packages.audio.audx
{
    public partial class AudXConfigurationPanel : MeGUI.core.details.audio.AudioConfigurationPanel, Editable<AudXSettings>
    {
        public AudXConfigurationPanel():base()
        {
            InitializeComponent();
            comboBox1.Items.AddRange(EnumProxy.CreateArray(typeof(AudXSettings.QualityMode)));
        }
        #region properties
        protected override bool IsMultichanelRequed
        {
            get
            {
                return true;
            }
        }

        /// <summary>
        /// gets / sets the settings that are being shown in this configuration dialog
        /// </summary>
        protected override AudioCodecSettings CodecSettings
        {
            get
            {
                AudXSettings nas = new AudXSettings();
                nas.Quality = (AudXSettings.QualityMode)(comboBox1.SelectedItem as EnumProxy).RealValue;
                return nas;
            }
            set
            {
                AudXSettings nas = value as AudXSettings;
                comboBox1.SelectedItem = EnumProxy.Create(nas.Quality);
            }
        }
        #endregion

        #region Editable<AudXSettings> Members

        AudXSettings Editable<AudXSettings>.Settings
        {
            get
            {
                return (AudXSettings)Settings;
            }
            set
            {
                Settings = value;
            }
        }

        #endregion
    }
}




