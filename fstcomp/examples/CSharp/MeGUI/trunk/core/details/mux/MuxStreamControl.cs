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

using MeGUI.core.util;

namespace MeGUI.core.details.mux
{
    public partial class MuxStreamControl : UserControl
    {
        public MuxStreamControl()
        {
            InitializeComponent();
            subtitleLanguage.Items.AddRange(new List<string>(LanguageSelectionContainer.Languages.Keys).ToArray());
        }

        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public MuxStream Stream
        {
            get
            {
                if (string.IsNullOrEmpty(input.Filename))
                    return null;

                string language = null;
                if (subtitleLanguage.Text != null && LanguageSelectionContainer.Languages.ContainsKey(subtitleLanguage.Text))
                    language = LanguageSelectionContainer.Languages[subtitleLanguage.Text];
                return new MuxStream(input.Filename, subtitleLanguage.Text, subName.Text, (int)audioDelay.Value);
            }

            set
            {
                if (value == null)
                {
                    removeSubtitleTrack_Click(null, null);
                    return;
                }

                input.Filename = value.path;
                if (!string.IsNullOrEmpty(value.language))
                    subtitleLanguage.SelectedValue = value.language;
                subName.Text = value.name;
                audioDelay.Value = value.delay;
            }
        }

        private bool showDelay;
        public bool ShowDelay
        {
            set
            {
                showDelay = value;
                delayLabel.Visible = value;
                audioDelay.Visible = value;
                if (!value) audioDelay.Value = 0;
            }
            get
            {
                return showDelay;
            }
        }

        public string Filter
        {
            get { return input.Filter; }
            set { input.Filter = value; }
        }

        public void SetLanguage(string lang)
        {
            subtitleLanguage.SelectedItem = lang;
        }

        private void removeSubtitleTrack_Click(object sender, EventArgs e)
        {
            input.Text = "";
            subtitleLanguage.SelectedIndex = -1;
            subName.Text = "";
            audioDelay.Value = 0;
            raiseEvent();
        }

        private void raiseEvent()
        {
            if (FileUpdated != null)
                FileUpdated(this, new EventArgs());
        }

        public event EventHandler FileUpdated;

        private void input_FileSelected(FileBar sender, FileBarEventArgs args)
        {
            audioDelay.Value = PrettyFormatting.getDelayAndCheck(input.Filename) ?? 0;

            foreach (KeyValuePair<string,string> strLanguage in LanguageSelectionContainer.Languages)
            {
                if (input.Filename.ToLower().Contains(strLanguage.Key.ToLower()))
                {
                    SetLanguage(strLanguage.Key);
                    break;
                }
            }
            raiseEvent();
        }
    }
}
