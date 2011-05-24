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
    /// <summary>A Stream of StreamType Audio</summary>
    public class AudioStream : Stream
    {
        public AudioStreamType AudioType { get; set; }
        public override string Language { get; set; }
        public string Channels { get; set; }
        public string Bitrate { get; set; }
        public string DialogNormalization { get; set; }
        public string Delay { get; set; }

        public override object[] ExtractTypes
        {
            get
            {
                switch (AudioType)
                {
                    case AudioStreamType.EAC3:
                        return new object[] { "EAC3", "AC3", "FLAC", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                    case AudioStreamType.DTS:                        
                        return new object[] { "DTS", "DTSHD", "AC3", "FLAC", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                    case AudioStreamType.TrueHD:
                        return new object[] { "THD", "THD+AC3", "AC3", "FLAC", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                    case AudioStreamType.MP2:
                        return new object[] { "MP2", "AC3", "FLAC", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                    case AudioStreamType.MP3:
                        return new object[] { "MP3", "AC3", "FLAC", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                    case AudioStreamType.AAC:
                        return new object[] { "AAC", "AC3", "FLAC", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                    case AudioStreamType.FLAC:
                        return new object[] { "FLAC", "AC3", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                    case AudioStreamType.VORBIS:
                        return new object[] { "OGG", "AC3", "FLAC", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                    case AudioStreamType.TTA:
                        return new object[] { "TTA", "AC3", "FLAC", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                    case AudioStreamType.WAVPACK:
                        return new object[] { "WV", "AC3", "FLAC", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                    default:
                        return new object[] { "AC3", "FLAC", "AAC", "WAV", "WAVS", "RAW", "W64", "RF64", "AGM" };
                }
            }
        }

        public AudioStream(string s) : base(s)
        {
            if (string.IsNullOrEmpty(s))
                throw new ArgumentNullException("s", "The string 's' cannot be null or empty.");

            base.Type = StreamType.Audio;
        }

        new public static Stream Parse(string s)
        {
            //2: AC3, English, 2.0 channels, 192kbps, 48khz, dialnorm: -27dB, -8ms
            //4: TrueHD, English, 5.1 channels, 48khz, dialnorm: -27dB

            if (string.IsNullOrEmpty(s))
                throw new ArgumentNullException("s", "The string 's' cannot be null or empty.");

            AudioStream audioStream = new AudioStream(s);

            switch (audioStream.Name.ToUpper())
            {
                case "AC3":
                case "AC3 EX":
                case "AC3 SURROUND":
                    audioStream.AudioType = AudioStreamType.AC3;
                    break;
                case "DTS":
                case "DTS-ES":
                case "DTS MASTER AUDIO":
                case "DTS HI-RES":
                    audioStream.AudioType = AudioStreamType.DTS;
                    break;
                case "E-AC3":
                    audioStream.AudioType = AudioStreamType.EAC3;
                    break;
                case "TRUEHD":
                    audioStream.AudioType = AudioStreamType.TrueHD;
                    break;
                case "TRUEHD/AC3":
                    audioStream.AudioType = AudioStreamType.TrueHD | AudioStreamType.AC3;
                    break;
                case "PCM":
                    audioStream.AudioType = AudioStreamType.PCM;
                    break;
                case "WAV":
                    audioStream.AudioType = AudioStreamType.WAV;
                    break;
                case "WAVS":
                    audioStream.AudioType = AudioStreamType.WAVS;
                    break;
                case "MP2":
                    audioStream.AudioType = AudioStreamType.MP2;
                    break;
                case "MP3":
                    audioStream.AudioType = AudioStreamType.MP3;
                    break;
                case "AAC":
                    audioStream.AudioType = AudioStreamType.AAC;
                    break;
                case "FLAC":
                    audioStream.AudioType = AudioStreamType.FLAC;
                    break;
                case "TTA1":
                    audioStream.AudioType = AudioStreamType.TTA;
                    break;
                case "WAVPACK4":
                    audioStream.AudioType = AudioStreamType.WAVPACK;
                    break;
                case "VORBIS":
                    audioStream.AudioType = AudioStreamType.VORBIS;
                    break;
                case "RAW":
                default:
                    audioStream.AudioType = AudioStreamType.RAW;
                    break;
            }

            return audioStream;
        }

        public override string ToString()
        {
            return base.ToString();
        }
    }
}
