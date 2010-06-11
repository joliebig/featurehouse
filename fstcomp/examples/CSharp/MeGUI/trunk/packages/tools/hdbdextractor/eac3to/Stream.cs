// ****************************************************************************
// 
// Copyright (C) 2005-2008  Doom9 & al
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

namespace eac3to
{
    /// <summary>A Stream</summary>
    public abstract class Stream
    {
        virtual public int Number { get; set; }
        virtual public string Name { get; set; }
        virtual public StreamType Type { get; set; }
        virtual public string Description { get; set; }
        virtual public string Language { get; set; }
        abstract public object[] ExtractTypes { get; }

        protected Stream() { }

        protected Stream(string s)
        {
            if (string.IsNullOrEmpty(s))
                throw new ArgumentNullException("s", "The string 's' cannot be null or empty.");

            Number = int.Parse(s.Substring(0, s.IndexOf(":")));

            if (s.Contains("Joined EVO"))
                Name = "Joined EVO";
            else if (s.Contains("Joined VOB"))
                Name = "Joined VOB";
            else if (s.Contains("Subtitle"))
                Name = s.Substring(s.LastIndexOf(",") + 1);
            else
                Name = s.Substring(s.IndexOf(":") + 1, s.IndexOf(',') - s.IndexOf(":") - 1).Trim();

            Description = s.Substring(s.IndexOf(":") + 1);
        }

        public static Stream Parse(string s)
        {
            //EVO, 1 video track, 1 audio track, 3 subtitle tracks, 1:43:54
            //"director"

            /////////////////////////////////////////////////////////////////
            //////// input file
            /*
            M2TS, 1 video track, 1 audio track, 0:00:11, 60i /1.001
            1: h264/AVC, 1080i60 /1.001 (16:9)
            2: AC3, 5.1 channels, 640kbps, 48khz             
              */

            if (string.IsNullOrEmpty(s))
                throw new ArgumentNullException("s", "The string 's' cannot be null or empty.");

            Stream stream = null;

            if (s.Contains("AVC") || s.Contains("VC-1") || s.Contains("MPEG") || s.Contains("DIRAC") || s.Contains("THEORA"))
                stream = VideoStream.Parse(s);
            else if (s.Contains("AC3") || s.Contains("TrueHD") || s.Contains("DTS") || 
                     s.Contains("RAW") || s.Contains("PCM") || s.Contains("MP") || s.Contains("AAC") ||
                     s.Contains("FLAC") || s.Contains("WAVPACK") || s.Contains("TTA") || s.Contains("VORBIS"))
                stream = AudioStream.Parse(s);
            else if (s.Contains("Subtitle"))
                stream = SubtitleStream.Parse(s);
            else if (s.Contains("Chapters"))
                stream = ChapterStream.Parse(s);
            else if (s.Contains("Joined"))
                stream = JoinStream.Parse(s);

            return stream;
        }

        public override string ToString()
        {
            return string.Format("{0}: {1}", Number, Description);
        }
    }
}
