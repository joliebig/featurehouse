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
using System.Text;

namespace MeGUI
{
    public class SubtitleIndexJob : Job
    {
        private bool indexAllTracks;
        private List<int> trackIDs;
        private int pgc;
        public SubtitleIndexJob()
            : base()
		{
            indexAllTracks = true;
            trackIDs = new List<int>();
            pgc = 1;
		}

        public SubtitleIndexJob(string input, string output,
            bool indexAllTracks, List<int> trackIDs, int pgc)
        {
            Input = input;
            Output = output;
            IndexAllTracks = indexAllTracks;
            TrackIDs = trackIDs;
            PGC = pgc;
        }

        
        public bool IndexAllTracks
        {
            get { return indexAllTracks; }
            set { indexAllTracks = value; }
        }
        public List<int> TrackIDs
        {
            get { return trackIDs; }
            set { trackIDs = value; }
        }
        public int PGC
        {
            get { return pgc; }
            set { pgc = value; }
        }

        public override string CodecString
        {
            get { return ""; }
        }

        public override string EncodingMode
        {
            get { return "sub"; }
        }
    }
}
