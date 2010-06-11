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

namespace MeGUI
{
    public class DGAIndexJob : Job
    {
		private bool loadSources;
        private bool demuxVideo;
		private int demuxMode;
        private List<AudioTrackInfo> audioTracks;
		private DGIndexPostprocessingProperties postprocessingProperties;
		
		public DGAIndexJob():base()
		{
			loadSources = false;
            demuxVideo = false;
			demuxMode = 0;
            audioTracks = new List<AudioTrackInfo>();
		}

        public DGAIndexJob(string input, string output, int demuxType, List<AudioTrackInfo> audioTracks,
            DGIndexPostprocessingProperties properties, bool loadSources, bool demuxVideo)
        {
            Input = input;
            Output = output;
            DemuxMode = demuxType;
            this.audioTracks = audioTracks;
            PostprocessingProperties = properties;
            LoadSources = loadSources;
            DemuxVideo = demuxVideo;
        }

        /// <summary>
        /// gets / sets whether video stream is extracted
        /// </summary>
        public bool DemuxVideo
        {
            get { return demuxVideo; }
            set { demuxVideo = value; }
        }
		/// <summary>
		/// gets / sets whether the audio and video files should be loaded after indexing
		/// </summary>
		public bool LoadSources
		{
			get {return loadSources;}
			set {loadSources = value;}
		}
    	/// <summary>
		/// gets / sets the demux mode
		/// 0 = no audio demux
		/// 1 = demux selected audio track
		/// 2 = demux all audio tracks
		/// </summary>
		public int DemuxMode
		{
			get {return demuxMode;}
			set {demuxMode = value;}
		}

        public List<AudioTrackInfo> AudioTracks
        {
            get { return audioTracks; }
            set { audioTracks = value; }
        }

		/// <summary>
		/// gets / sets the postprocessing properties
		/// if this is not set, we're just dealing with a regular demuxing job
		/// if it is defined, we're dealing with an index job in one click mode
		/// and all the postprocessing that has to be done prior to audio encoding
		/// is defined in this property
		/// </summary>
		public DGIndexPostprocessingProperties PostprocessingProperties
		{
			get {return postprocessingProperties;}
			set {postprocessingProperties = value;}
		}

        public override string CodecString
        {
            get { return ""; }
        }

        public override string EncodingMode
        {
            get { return "idx"; }
        }
    }
}
