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

using MeGUI.core.details;
using MeGUI.core.util;

namespace MeGUI
{
	/// <summary>
	/// Summary description for MuxSettings.
	/// </summary>
	public class MuxSettings
	{
		private List<MuxStream> audioStreams, subtitleStreams;
		private decimal? framerate;
		private string chapterFile, videoName;
        private string muxedInput, videoInput, muxedOutput, deviceType;

        private FileSize? splitSize;

		public MuxSettings()
		{
			audioStreams = new List<MuxStream>();
			subtitleStreams = new List<MuxStream>();
            framerate = 0.0M;
            muxedInput = "";
			chapterFile = "";
            videoName = "";
            videoInput = "";
            muxedOutput = "";
            deviceType = "";
			splitSize = null;
		}

        public string MuxedInput
        {
            get { return muxedInput; }
            set { muxedInput = value; }
        }
        public string MuxedOutput
        {
            get { return muxedOutput; }
            set { muxedOutput = value; }
        }
        public string VideoInput
        {
            get { return videoInput; }
            set { videoInput = value; }
        }

		/// <summary>
		/// Array of all the audio streams to be muxed
		/// </summary>
		public List<MuxStream> AudioStreams
		{
			get {return audioStreams;}
			set {audioStreams = value;}
		}
		/// <summary>
		/// Array of subtitle tracks to be muxed
		/// </summary>
		public List<MuxStream> SubtitleStreams
		{
			get {return subtitleStreams;}
			set {subtitleStreams = value;}
		}
		/// <summary>
		/// framerate of the video
		/// </summary>
		public decimal? Framerate
		{
			get {return framerate;}
			set {framerate = value;}
		}
		/// <summary>
		/// the file containing the chapter information
		/// </summary>
		public string ChapterFile
		{
			get {return chapterFile;}
			set {chapterFile = value;}
		}
		/// <summary>
		/// file size at which the output file is to be split
		/// </summary>
		public FileSize? SplitSize
		{
			get {return splitSize;}
			set {splitSize = value;}
		}

        private Dar? dar;

        public Dar? DAR
        {
            get { return dar; }
            set { dar = value; }
        }

        public string DeviceType
        {
            get { return deviceType; }
            set { deviceType = value; }
        }
		
        /// <summary>
        /// gets / sets the name of the video track
        /// </summary>
        public string VideoName
        {
           get { return videoName; }
           set { videoName = value; }
        }
	
	}
}