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
using System.Diagnostics;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using MeGUI.core.details.video;
using MeGUI.core.util;

namespace MeGUI.core.gui
{
    public partial class SimpleProfilesControl : UserControl
    {
        public SimpleProfilesControl()
        {
            InitializeComponent();
        }

        public void SetSettings(AudioCodecSettings value)
        {
            manager.SetSettings(value);
        }


        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public Profile SelectedProfile
        {
            get { return ((Named<Profile>)comboBox1.SelectedItem).Data; }
        }

        public void SelectProfile(string fqname)
        {
            foreach (Named<Profile> n in comboBox1.Items)
                if (n.Data.FQName == fqname)
                {
                    comboBox1.SelectedItem = n;
                    return;
                }

            throw new ProfileCouldntBeSelectedException(fqname);
        }

        public void SelectProfile(Profile prof)
        {
            SelectProfile(prof.FQName);
        }

        public void SetProfileNameOrWarn(string fqname)
        {
            if (string.IsNullOrEmpty(fqname))
                return;
            try
            {
                SelectProfile(fqname);
            }
            catch (ProfileCouldntBeSelectedException)
            {
                MessageBox.Show("The profile, " + fqname + ", could not be selected.", "Profile couldn't be selected", MessageBoxButtons.OK, MessageBoxIcon.Warning);
            }
        }

        private ProfileManager manager = new ProfileManager("");

        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public ProfileManager Manager
        {
            get { return manager; }
            set {
                if (!string.IsNullOrEmpty(ProfileSet))
                {
                    manager.RemoveProfilesChangedListener(ProfileSet, ProfilesChanged);
                    value.AddProfilesChangedListener(ProfileSet, ProfilesChanged);
                }
                manager = value; 
                RefreshProfiles(); 
            }
        }

        private void ProfilesChanged(object _, EventArgs __)
        {
            RefreshProfiles();
        }

        private string profileSet;
        /// <summary>
        /// The string describing the profile set to request from the profile manager
        /// </summary>
        public string ProfileSet
        {
            get { return profileSet; }
            set {
                if (!string.IsNullOrEmpty(profileSet))
                    Manager.RemoveProfilesChangedListener(profileSet, ProfilesChanged);
                if (!string.IsNullOrEmpty(value))
                    Manager.AddProfilesChangedListener(value, ProfilesChanged);
                
                profileSet = value; 
            }
        }

        protected void RefreshProfiles()
        {
            comboBox1.Items.Clear();
            comboBox1.Items.AddRange(Util.ToArray(Manager.Profiles(ProfileSet)));
            SelectProfile(Manager.GetSelectedProfile(ProfileSet));
        }

        protected void raiseProfileChangedEvent()
        {
            if (SelectedProfileChanged != null)
                SelectedProfileChanged(this, EventArgs.Empty);
        }

        public event EventHandler SelectedProfileChanged;

        private void comboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            Manager.SetSelectedProfile(SelectedProfile);
            raiseProfileChangedEvent();
        }
    }

    public class ProfileCouldntBeSelectedException : MeGUIException
    {
        private string name;
        public string ProfileName
        {
            get { return name; }
        }

        public ProfileCouldntBeSelectedException(string name)
            : base("The profile '" + name + "' couldn't be selected.")
        {
            this.name = name;
        }
    }

}
