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

namespace MeGUI.core.gui
{
    public partial class AviSynthProfileConfigPanel : UserControl, Editable<AviSynthSettings>
    {     
        public AviSynthProfileConfigPanel() 
        {
            InitializeComponent();
            this.resizeFilterType.DataSource = ScriptServer.ListOfResizeFilterType;
            this.resizeFilterType.BindingContext = new BindingContext();
            this.noiseFilterType.DataSource = ScriptServer.ListOfDenoiseFilterType;
            this.noiseFilterType.BindingContext = new BindingContext();
            if (!string.IsNullOrEmpty(MeGUISettings.HaaliMSPath))
                dss2.Enabled = true;
        }

        #region Gettable<AviSynthSettings> Members

        public AviSynthSettings Settings
        {
            get
            {
                mod16Method method = (mod16Method)mod16Box.SelectedIndex;
                if (!signalAR.Checked)
                    method = mod16Method.none;
                return new AviSynthSettings(avisynthScript.Text,
                    (ResizeFilterType)(resizeFilterType.SelectedItem as EnumProxy).RealValue,
                    resize.Checked,
                    (DenoiseFilterType)(noiseFilterType.SelectedItem as EnumProxy).RealValue,
                    noiseFilter.Checked,
                    mpeg2Deblocking.Checked,
                    colourCorrect.Checked,
                    method,
                    dss2.Checked);
            }
            set
            {
                avisynthScript.Text = value.Template;
                resize.Checked = value.Resize;
                resizeFilterType.SelectedItem = EnumProxy.Create(value.ResizeMethod);
                noiseFilterType.SelectedItem = EnumProxy.Create(value.DenoiseMethod);
                noiseFilter.Checked = value.Denoise;
                mpeg2Deblocking.Checked = value.MPEG2Deblock;
                colourCorrect.Checked = value.ColourCorrect;
                signalAR.Checked = (value.Mod16Method != mod16Method.none);
                mod16Box.Enabled = signalAR.Checked;
                mod16Box.SelectedIndex = (int)value.Mod16Method;
                dss2.Checked = value.DSS2;
            }
        }

        #endregion

        #region event handlers
        private void signalAR_CheckedChanged(object sender, EventArgs e)
        {
            mod16Box.Enabled = signalAR.Checked;
        }
        #endregion

        private void insert_Click(object sender, EventArgs e)
        {
            string text = (sender as Control).Tag as string;
            string avsScript = avisynthScript.Text;
			string avsScriptA = avsScript.Substring(0, avisynthScript.SelectionStart);
			string avsScriptB = avsScript.Substring(avisynthScript.SelectionStart + avisynthScript.SelectionLength);
			avisynthScript.Text = avsScriptA + text + avsScriptB;
        }

        private void dllBar_FileSelected(FileBar sender, FileBarEventArgs args)
        {
            avisynthScript.Text = "LoadPlugin(\"" + args.NewFileName + "\")\r\n" + avisynthScript.Text;
        }

        private void noiseFilter_CheckedChanged(object sender, EventArgs e)
        {
            noiseFilterType.Enabled = noiseFilter.Checked;
        }

        private void resize_CheckedChanged(object sender, EventArgs e)
        {
            resizeFilterType.Enabled = resize.Checked;
        }
    }
}
