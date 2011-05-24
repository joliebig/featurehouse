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
    public class AudXSettings: AudioCodecSettings
	{
        public static readonly string ID = "Aud-X MP3";
        public enum QualityMode
        {
            //0 (STRQ 80 kbps), 1 (STDQ 128 kbps), 2 (HGHQ 192 kbps) or 3 (SPBQ 192 kbps), default is 1
            [EnumTitle("STRQ 80 kbps", 80)]
            STRQ = 0,
            [EnumTitle("STDQ 128 kbps", 128)]
            STDQ = 1,
            [EnumTitle("HGHQ 192 kbps", 192)]
            HGHQ = 2,
            [EnumTitle("SPBQ 192 kbps", 192)]
            SPBQ = 3
        }

        public QualityMode Quality;

        public AudXSettings()
            : base(ID, AudioCodec.MP3, AudioEncoderType.AUDX, 80)
		{
            this.Quality = QualityMode.STDQ;
		}


        public override int Bitrate
        {
            get
            {
                return (int)EnumProxy.Create(this.Quality).Tag ;
            }
            set
            {
                // Do Nothing
            }
        }

        public override BitrateManagementMode BitrateMode
        {
            get
            {
                return BitrateManagementMode.CBR;
            }
            set
            {
                // Do Nothing
            }
        }


	}
}
