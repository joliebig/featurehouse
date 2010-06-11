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

using MeGUI.core.plugins.interfaces;
using MeGUI.core.util;

namespace MeGUI
{
	/// <summary>
	/// Summary description for OneClickDefaults.
	/// </summary>
    public class OneClickSettings : GenericSettings
	{
        public string SettingsID { get { return "OneClick"; } }

        public virtual void FixFileNames(Dictionary<string, string> _) { }
        public override bool Equals(object obj)
        {
            return Equals(obj as GenericSettings);
        }

        public bool Equals(GenericSettings other)
        {
            return other == null ? false : PropertyEqualityTester.AreEqual(this, other);
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

        private string videoProfileName;
        public string VideoProfileName
        {
            get { return videoProfileName; }
            set { videoProfileName = value; }
        }

        private string audioProfileName;
        public string AudioProfileName
        {
            get { return audioProfileName; }
            set { audioProfileName = value; }
        }

        private string avsProfileName;
        public string AvsProfileName
        {
            get { return avsProfileName; }
            set { avsProfileName = value; }
        }

        private bool prerenderVideo;
        public bool PrerenderVideo
        {
            get { return prerenderVideo; }
            set { prerenderVideo = value; }
        }

        private bool dontEncodeAudio;
        public bool DontEncodeAudio
        {
            get { return dontEncodeAudio; }
            set { dontEncodeAudio = value; }
        }

        private bool signalAR;
        public bool SignalAR
        {
            get { return signalAR; }
            set { signalAR = value; }
        }

        private bool automaticDeinterlacing;
        public bool AutomaticDeinterlacing
        {
            get { return automaticDeinterlacing; }
            set { automaticDeinterlacing = value; }
        }

        private bool autoCrop;
        public bool AutoCrop
        {
            get { return autoCrop; }
            set { autoCrop = value; }
        }

        private bool keepInputResolution;
        public bool KeepInputResolution
        {
            get { return keepInputResolution; }
            set { keepInputResolution = value; }
        }

        private long outputResolution;
        public long OutputResolution
        {
            get { return outputResolution; }
            set { outputResolution = value; }
        }

        private FileSize? filesize;
        public FileSize? Filesize
        {
            get { return filesize; }
            set { filesize = value; }
        }

        private FileSize? splitSize;
        public FileSize? SplitSize
        {
            get { return splitSize; }
            set { splitSize = value; }
        }

        private string[] containerCandidates;
        public string[] ContainerCandidates
        {
            get { return containerCandidates; }
            set { containerCandidates = value; }
        }

        private string deviceType;
        public string DeviceOutputType
        {
            get { return deviceType; }
            set { deviceType = value; }
        }

        private bool useChaptersMarks;
        public bool UseChaptersMarks
        {
            get { return useChaptersMarks; }
            set { useChaptersMarks = value; }
        }

        object ICloneable.Clone()
        {
            return Clone();
        }

        GenericSettings GenericSettings.Clone()
        {
            return Clone();
        }
        
        public OneClickSettings Clone()
        {
            return this.MemberwiseClone() as OneClickSettings;
        }

		public OneClickSettings()
		{
			VideoProfileName = "";
			AudioProfileName = "";
            AvsProfileName = "";
            AutomaticDeinterlacing = true;
            PrerenderVideo = false;
			DontEncodeAudio = false;
			SignalAR = false;
            AutoCrop = true;
            KeepInputResolution = false;
			OutputResolution = 720;
            SplitSize = null;
            ContainerCandidates = new string[] {"MKV"};
		}

        #region GenericSettings Members


        public string[] RequiredFiles
        {
            get { return new string[0]; }
        }

        public string[] RequiredProfiles
        {
            get { return new string[]{VideoProfileName, AudioProfileName};}
        }

        #endregion
    }
}
