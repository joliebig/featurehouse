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

using MeGUI.core.util;

namespace MeGUI.core.details
{
    public class TrackInfo
    {
        public string Language;
        public string Name;

        public TrackInfo(string language, string name)
        {
            Language = language;
            Name = name;
        }

        public TrackInfo() : this("", "") { }
    }

    public class MuxStream
    {
        public string path;
        public TrackInfo TrackInfo;
        public int delay;

        public MuxStream(string path, TrackInfo info, int delay)
        {
            this.path = path;
            TrackInfo = info;
            this.delay = delay;
        }

        public MuxStream(string path, string language, string name, int delay)
            :
            this(path, new TrackInfo(language, name), delay) { }

        public MuxStream() : this(null, new TrackInfo(), 0) { }

        public string language
        {
            get
            {
                if (TrackInfo == null)
                    return null;
                return TrackInfo.Language;
            }
            set
            {
                if (TrackInfo == null)
                    TrackInfo = new TrackInfo();
                TrackInfo.Language = value;
            }
        }

        public string name
        {
            get
            {
                if (TrackInfo == null)
                    return null;
                return TrackInfo.Name;
            }
            set
            {
                if (TrackInfo == null)
                    TrackInfo = new TrackInfo();
                TrackInfo.Name = value;
            }
        }
    }

    public class BitrateCalculationStream
    {
        public BitrateCalculationStream(string filename)
        {
            this.Filename = filename;
            if (Filename != null) fillInfo();
        }

        public string Filename;
        public FileSize? Size;
        public OutputType Type;

        public virtual void fillInfo()
        {
            Size = FileSize.Of2(Filename);
        }

    }

    public class AudioBitrateCalculationStream : BitrateCalculationStream
    {
        public AudioBitrateCalculationStream() : this(null) { }

        public AudioBitrateCalculationStream(string filename)
            : base(filename) {}


        public AudioType AType;

        public override void fillInfo()
        {
            base.fillInfo();
            Type = AType = VideoUtil.guessAudioType(Filename);
        }
    }
}

