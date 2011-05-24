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
using System.Configuration;
using System.ComponentModel;
using System.Text;

using MeGUI.core.gui;

namespace MeGUI.core.util
{
    class CustomUserSettings : ApplicationSettingsBase
    {
        private CustomUserSettings() : base() { }

        private static CustomUserSettings defaultInstance = ((CustomUserSettings)(global::System.Configuration.ApplicationSettingsBase.Synchronized(new CustomUserSettings())));

        public static CustomUserSettings Default
        {
            get
            {
                return defaultInstance;
            }
        }

        [UserScopedSetting]
        public Dar[] CustomDARs
        {
            get
            {
                object o = this["CustomDARs"];
                return (Dar[])this["CustomDARs"];
            }
            set
            {
                this["CustomDARs"] = value; // new ArrayConverter<Named<Dar>, DarConverter>().ToString(value);
                object o = this["CustomDARs"];
            }
        }

        [UserScopedSetting]
        public FileSize[] CustomSizes
        {
            get
            {
                return (FileSize[])this["CustomSizes"];
            }
            set
            {
                this["CustomSizes"] = value;
            }
        }


        [UserScopedSetting]
        public FPS[] CustomFPSs
        {
            get
            {
                return (FPS[])this["CustomFPSs"];
            }
            set
            {
                this["CustomFPSs"] = value;
            }
        }
    }
}
