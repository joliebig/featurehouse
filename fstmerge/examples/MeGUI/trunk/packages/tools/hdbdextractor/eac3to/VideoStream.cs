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
    /// <summary>A Stream of StreamType Video</summary>
    public class VideoStream : Stream
    {
        public VStreamType VType { get; set; }
        public string Resolution { get; set; }
        public bool IsProgerssive { get; set; }
        public double Framerate { get; set; }
        public double Ratio { get; set; }

        public override object[] ExtractTypes
        {
            get
            {
                switch (VType)
                {
                    case VStreamType.AVC:
                        return new object[] { "MKV", "H264" };
                    case VStreamType.VC1:
                        return new object[] { "MKV", "VC1" };
                    case VStreamType.MPEG:
                        return new object[] { "MKV", "M2V" };
                    case VStreamType.THEORA:
                        return new object[] { "MKV", "OGG" };
                    case VStreamType.DIRAC:
                        return new object[] { "MKV", "DRC" };
                    default:
                        return new object[] { "MKV" };
                }
            }
        }

        public VideoStream(string s) : base(s)
        {
            if (string.IsNullOrEmpty(s))
                throw new ArgumentNullException("s", "The string 's' cannot be null or empty.");

            base.Type = StreamType.Video;
        }

        new public static Stream Parse(string s)
        {
            //3: VC-1, 1080p24 /1.001 (16:9) with pulldown flags

            if (string.IsNullOrEmpty(s))
                throw new ArgumentNullException("s", "The string 's' cannot be null or empty.");

            VideoStream videoStream = new VideoStream(s);

            switch (videoStream.Name.ToUpper())
            {
                case "AVC":
                    videoStream.VType = VStreamType.AVC;
                    break;
                case "VC-1":
                    videoStream.VType = VStreamType.VC1;
                    break;
                case "MPEG":
                case "MPEG2":
                    videoStream.VType = VStreamType.MPEG;
                    break;
                case "THEORA":
                    videoStream.VType = VStreamType.THEORA;
                    break;
                case "DIRAC":
                    videoStream.VType = VStreamType.DIRAC;
                    break;
            }

            return videoStream;
        }

        public override string ToString()
        {
            return base.ToString();
        }
    }
}
