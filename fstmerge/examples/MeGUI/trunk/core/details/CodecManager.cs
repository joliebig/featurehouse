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
using System.Xml.Serialization;

namespace MeGUI
{
    
    /// <summary>
    /// Dummy interface to avoid some runtime type errors. This should be implemented by VideoCodec and AudioCodec
    /// </summary>
    public interface ICodec { }
    
    public class VideoCodec : ICodec, IIDable
    {
        private string id;
        public string ID
        {
            get { return id; }
        }
        public VideoCodec(string id)
        {
            this.id = id;
        }
        public static readonly VideoCodec ASP   = new VideoCodec("ASP");
        public static readonly VideoCodec AVC   = new VideoCodec("AVC");
        public static readonly VideoCodec SNOW  = new VideoCodec("SNOW");
        public static readonly VideoCodec HFYU  = new VideoCodec("HFYU");
        public static readonly VideoCodec VC1   = new VideoCodec("VC1");
        public static readonly VideoCodec MPEG2 = new VideoCodec("MPEG2");
    }
    public class AudioCodec : ICodec, IIDable
    {
        private string id;
        public string ID
        {
            get { return id; }
        }
        public AudioCodec(string id)
        {
            this.id = id;
        }
        public static readonly AudioCodec MP3    = new AudioCodec("MP3");
        public static readonly AudioCodec AAC    = new AudioCodec("AAC");
        public static readonly AudioCodec VORBIS = new AudioCodec("VORBIS");
        public static readonly AudioCodec DTS    = new AudioCodec("DTS");
        public static readonly AudioCodec AC3    = new AudioCodec("AC3");
        public static readonly AudioCodec MP2    = new AudioCodec("MP2");
        public static readonly AudioCodec WAV    = new AudioCodec("WAV");
        public static readonly AudioCodec PCM    = new AudioCodec("PCM");
        public static readonly AudioCodec EAC3   = new AudioCodec("EAC3");
        public static readonly AudioCodec THD    = new AudioCodec("THD");
        public static readonly AudioCodec DTSHD  = new AudioCodec("DTSHD");
        public static readonly AudioCodec DTSMA  = new AudioCodec("DTSMA");
    }
    public class SubtitleCodec : ICodec, IIDable
    {
        private string id;
        public string ID
        {
            get { return id; }
        }
        public SubtitleCodec(string id)
        {
            this.id = id;
        }
        public static readonly SubtitleCodec TEXT  = new SubtitleCodec("TEXT");
        public static readonly SubtitleCodec IMAGE = new SubtitleCodec("IMAGE");
    }
    
    
    /// <summary>
    /// Dummy interface so runtime typing problems don't arise, and we can avoid ugly (object) casts
    /// </summary>
    public interface IEncoderType
    {
        ICodec Codec
        {
            get;
        }
    }

    public class VideoEncoderType : IEncoderType, IIDable
    {
        private string id;
        private VideoCodec codec;
        public VideoCodec VCodec
        {
            get { return codec; }
        }
        public ICodec Codec
        {
            get { return VCodec; }
        }

        public string ID
        {
            get { return id; }
        }
        public VideoEncoderType(string id, VideoCodec codec)
        {
            this.id = id;
            this.codec = codec;
        }

        public override string ToString()
        {
            return ID;
        }
        public static readonly VideoEncoderType XVID = new VideoEncoderType("Xvid", VideoCodec.ASP);
        public static readonly VideoEncoderType X264 = new VideoEncoderType("x264", VideoCodec.AVC);
        public static readonly VideoEncoderType SNOW = new VideoEncoderType("Snow", VideoCodec.SNOW);
        public static readonly VideoEncoderType HFYU = new VideoEncoderType("Huffyuv", VideoCodec.HFYU);
    }
    public class AudioEncoderType : IEncoderType, IIDable
    {
        private string id;
        private AudioCodec codec;
        public ICodec Codec
        {
            get { return ACodec; }
        }
        public AudioCodec ACodec
        {
            get { return codec; }
        }
        public string ID
        {
            get { return id; }
        }
        public AudioEncoderType(string id, AudioCodec codec)
        {
            this.id = id;
            this.codec = codec;
        }
        public static readonly AudioEncoderType LAME   = new AudioEncoderType("LAME", AudioCodec.MP3);
        public static readonly AudioEncoderType AUDX   = new AudioEncoderType("AUDX", AudioCodec.MP3);
        public static readonly AudioEncoderType WAAC   = new AudioEncoderType("WAAC", AudioCodec.AAC);
        public static readonly AudioEncoderType NAAC   = new AudioEncoderType("NAAC", AudioCodec.AAC);
        public static readonly AudioEncoderType FAAC   = new AudioEncoderType("FAAC", AudioCodec.AAC);
        public static readonly AudioEncoderType VORBIS = new AudioEncoderType("VORBIS", AudioCodec.VORBIS);
        public static readonly AudioEncoderType FFAC3  = new AudioEncoderType("FFAC3", AudioCodec.AC3);
        public static readonly AudioEncoderType FFMP2  = new AudioEncoderType("FFMP2", AudioCodec.MP2);
        public static readonly AudioEncoderType AFTEN  = new AudioEncoderType("AFTEN", AudioCodec.AC3);
    }
    
    
    class CodecManager
    {
        public static GenericRegisterer<VideoCodec> VideoCodecs = new GenericRegisterer<VideoCodec>();
        public static GenericRegisterer<AudioCodec> AudioCodecs = new GenericRegisterer<AudioCodec>();
        public static GenericRegisterer<VideoEncoderType> VideoEncoderTypes = new GenericRegisterer<VideoEncoderType>();
        public static GenericRegisterer<AudioEncoderType> AudioEncoderTypes = new GenericRegisterer<AudioEncoderType>();
        
        static CodecManager()
        {
            if (!(
                VideoCodecs.Register(VideoCodec.ASP)  &&
                VideoCodecs.Register(VideoCodec.AVC)  &&
                VideoCodecs.Register(VideoCodec.HFYU) &&
                VideoCodecs.Register(VideoCodec.SNOW) &&
                VideoCodecs.Register(VideoCodec.MPEG2) &&
                VideoCodecs.Register(VideoCodec.VC1)))
                throw new Exception("Failed to register a standard video codec");
            if (!(
                AudioCodecs.Register(AudioCodec.AAC) &&
                AudioCodecs.Register(AudioCodec.AC3) &&
                AudioCodecs.Register(AudioCodec.DTS) &&
                AudioCodecs.Register(AudioCodec.MP2) &&
                AudioCodecs.Register(AudioCodec.MP3) &&
                AudioCodecs.Register(AudioCodec.VORBIS) &&
                AudioCodecs.Register(AudioCodec.DTSHD)  &&
                AudioCodecs.Register(AudioCodec.DTSMA)  &&
                AudioCodecs.Register(AudioCodec.EAC3)   &&
                AudioCodecs.Register(AudioCodec.PCM)    &&
                AudioCodecs.Register(AudioCodec.THD)    &&
                AudioCodecs.Register(AudioCodec.WAV)))
                throw new Exception("Failed to register a standard audio codec");
            if (!(
                VideoEncoderTypes.Register(VideoEncoderType.HFYU) &&
                VideoEncoderTypes.Register(VideoEncoderType.SNOW) &&
                VideoEncoderTypes.Register(VideoEncoderType.X264) &&
                VideoEncoderTypes.Register(VideoEncoderType.XVID)))
                throw new Exception("Failed to register a standard video encoder type");
            if (!(
                AudioEncoderTypes.Register(AudioEncoderType.AUDX)   &&
                AudioEncoderTypes.Register(AudioEncoderType.FAAC)   &&
                AudioEncoderTypes.Register(AudioEncoderType.FFAC3)  &&
                AudioEncoderTypes.Register(AudioEncoderType.FFMP2)  &&
                AudioEncoderTypes.Register(AudioEncoderType.LAME)   &&
                AudioEncoderTypes.Register(AudioEncoderType.NAAC)   &&
                AudioEncoderTypes.Register(AudioEncoderType.VORBIS) &&
                AudioEncoderTypes.Register(AudioEncoderType.WAAC)   &&
                AudioEncoderTypes.Register(AudioEncoderType.AFTEN)))
                throw new Exception("Failed to register a standard audio encoder type");
                
        }
    }
    
    public class VideoType : OutputType
    {
        private VideoCodec[] supportedCodecs;

        public VideoCodec[] SupportedCodecs
        {
            get { return supportedCodecs; }
        }

        public VideoType(string name, string filterName, string extension, ContainerType containerType, VideoCodec supportedCodec)
            : this(name, filterName, extension, containerType, new VideoCodec[] { supportedCodec }) { }

        public VideoType(string name, string filterName, string extension, ContainerType containerType, VideoCodec[] supportedCodecs)
            : base(name, filterName, extension, containerType) {
                this.supportedCodecs = supportedCodecs;
        }
        public static readonly VideoType MP4     = new VideoType("MP4", "MP4 Files", "mp4", ContainerType.MP4, new VideoCodec[] { VideoCodec.ASP, VideoCodec.AVC, VideoCodec.MPEG2 });
        public static readonly VideoType RAWASP  = new VideoType("RAWASP", "RAW MPEG-4 ASP Files", "m4v", null, VideoCodec.ASP);
        public static readonly VideoType RAWAVC  = new VideoType("RAWAVC", "RAW MPEG-4 AVC Files", "264", null, VideoCodec.AVC);
        public static readonly VideoType RAWAVC2 = new VideoType("RAWAVC", "RAW MPEG-4 AVC Files", "h264", null, VideoCodec.AVC);
        public static readonly VideoType MKV     = new VideoType("MKV", "Matroska Files", "mkv", ContainerType.MKV, new VideoCodec[] { VideoCodec.ASP, VideoCodec.AVC, VideoCodec.SNOW, VideoCodec.HFYU, VideoCodec.MPEG2, VideoCodec.VC1});
        public static readonly VideoType AVI     = new VideoType("AVI", "AVI Files", "avi", ContainerType.AVI, new VideoCodec[] { VideoCodec.ASP, VideoCodec.AVC, VideoCodec.HFYU, VideoCodec.SNOW });
        public static readonly VideoType MPEG2   = new VideoType("MPEG2", "MPEG-2 Files", "m2v", null, VideoCodec.MPEG2);
        public static readonly VideoType VC1     = new VideoType("VC1", "VC-1 Files", "vc1", null, VideoCodec.VC1);
        public static readonly VideoType M2TS    = new VideoType("M2TS", "M2TS Files", "m2ts", ContainerType.M2TS, new VideoCodec[] { VideoCodec.AVC, VideoCodec.MPEG2, VideoCodec.VC1 });
    }
    public class AudioType : OutputType
    {
        private AudioCodec[] supportedCodecs;

        public AudioCodec[] SupportedCodecs
        {
            get { return supportedCodecs; }
        }

        public AudioType(string name, string filterName, string extension, ContainerType containerType, AudioCodec supportedCodec)
            : this(name, filterName, extension, containerType, new AudioCodec[] { supportedCodec }) { }

        public AudioType(string name, string filterName, string extension, ContainerType containerType, AudioCodec[] supportedCodecs)
            : base(name, filterName, extension, containerType) {
                this.supportedCodecs = supportedCodecs;
        }
        public static readonly AudioType MP4AAC = new AudioType("MP4-AAC", "MP4 AAC Files", "mp4", ContainerType.MP4, AudioCodec.AAC);
        public static readonly AudioType M4A    = new AudioType("M4A", "MP4 Audio Files", "m4a", ContainerType.MP4, AudioCodec.AAC);
        public static readonly AudioType RAWAAC = new AudioType("Raw-AAC", "RAW AAC Files", "aac", null, AudioCodec.AAC);
        public static readonly AudioType MP3    = new AudioType("MP3", "MP3 Files", "mp3", null, AudioCodec.MP3);
        public static readonly AudioType VORBIS = new AudioType("Ogg", "Ogg Vorbis Files", "ogg", null, AudioCodec.VORBIS);
        public static readonly AudioType AC3    = new AudioType("AC3", "AC3 Files", "ac3", null, AudioCodec.AC3);
        public static readonly AudioType MP2    = new AudioType("MP2", "MP2 Files", "mp2", null, AudioCodec.MP2);
        public static readonly AudioType DTS    = new AudioType("DTS", "DTS Files", "dts", null, AudioCodec.DTS);
        public static readonly AudioType WAV    = new AudioType("WAV", "WAV Files", "wav", null, AudioCodec.WAV);
        public static readonly AudioType PCM    = new AudioType("DTS", "DTS Files", "dts", null, AudioCodec.PCM);
        public static readonly AudioType CBRMP3 = new AudioType("CBR MP3", "CBR MP3 Files", "mp3", null, AudioCodec.MP3);
        public static readonly AudioType VBRMP3 = new AudioType("VBR MP3", "VBR MP3 Files", "mp3", null, AudioCodec.MP3);
        public static readonly AudioType EAC3   = new AudioType("EAC3", "EAC3 Files", "ddp", null, AudioCodec.EAC3);
        public static readonly AudioType THD    = new AudioType("THD", "TrueHD Files", "thd", null, AudioCodec.THD);
        public static readonly AudioType DTSHD  = new AudioType("DTSHD", "DTS-HD High Resolution Files", "dtshd", null, AudioCodec.DTSHD);
        public static readonly AudioType DTSMA  = new AudioType("DTSMA", "DTS Master Audio Files", "dtsma", null, AudioCodec.DTSMA);
    }
    public class SubtitleType : OutputType
    {
        public SubtitleType(string name, string filterName, string extension, ContainerType containerType)
            : base(name, filterName, extension, containerType) { }
        public static readonly SubtitleType SSA    = new SubtitleType("SubStationAlpha", "SubStation Alpha Subtitle Files", "ssa", null);
        public static readonly SubtitleType ASS    = new SubtitleType("Advanced SubStationAlpha", "Advanced SubStation Alpha Subtitle Files", "ass", null);
        public static readonly SubtitleType SUBRIP = new SubtitleType("Subrip", "Subrip Subtitle Files", "srt", null);
        public static readonly SubtitleType BDSUP  = new SubtitleType("BDSup", "Blu-ray Sup Subtitle Files", "sup", null); 
        public static readonly SubtitleType VOBSUB = new SubtitleType("Vobsub", "Vobsub Subtitle Files", "idx", null);
    }
    public class ChapterType : OutputType
    {
        public ChapterType(string name, string filterName, string extension, ContainerType containerType)
            : base(name, filterName, extension, containerType) { }
        public static readonly ChapterType OGG_TXT = new ChapterType("Ogg Chapter", "Ogg Chapter Files", "txt", null);
        public static readonly ChapterType MKV_XML = new ChapterType("Matroska Chapter", "Matroska Chapter Files", "xml", null);
    }
    public class DeviceType : OutputType
    {
        public DeviceType(string name, string filterName, string extension, ContainerType containerType)
            : base(name, filterName, extension, containerType) { }
        public static readonly DeviceType IPOD = new DeviceType("iPod", "iPod", "iPod", ContainerType.MP4);
        public static readonly DeviceType IPHONE = new DeviceType("iPhone", "iPhone", "iPhone", ContainerType.MP4);
        public static readonly DeviceType ISMA = new DeviceType("ISMA", "ISMA", "ISMA", ContainerType.MP4);
        public static readonly DeviceType PSP = new DeviceType("PSP", "PSP", "PSP", ContainerType.MP4);
    //    public static readonly DeviceType BD = new DeviceType("Blu-ray", "Blu-ray", "Blu-ray", ContainerType.M2TS);
    //    public static readonly DeviceType AVCHD = new DeviceType("AVCHD", "AVCHD", "AVCHD", ContainerType.M2TS);
    }
    public class ContainerType : OutputFileType
    {
        public ContainerType(string name, string filterName, string extension)
            : base(name, filterName, extension) { }
        public static readonly ContainerType MP4  = new ContainerType("MP4", "MP4 Files", "mp4");
        public static readonly ContainerType MKV  = new ContainerType("MKV", "Matroska Files", "mkv");
        public static readonly ContainerType AVI  = new ContainerType("AVI", "AVI Files", "avi");
        public static readonly ContainerType M2TS = new ContainerType("M2TS", "M2TS Files", "m2ts");
        public static readonly ContainerType[] Containers = new ContainerType[] { MP4, MKV, AVI, M2TS };

        public static ContainerType ByName(string id)
        {
            foreach (ContainerType t in Containers)
                if (t.ID == id)
                    return t;
            return null;
        }
    }
    
    public class ContainerManager
    {
        public static GenericRegisterer<VideoType> VideoTypes = new GenericRegisterer<VideoType>();
        public static GenericRegisterer<AudioType> AudioTypes = new GenericRegisterer<AudioType>();
        public static GenericRegisterer<SubtitleType> SubtitleTypes = new GenericRegisterer<SubtitleType>();
        public static GenericRegisterer<ContainerType> ContainerTypes = new GenericRegisterer<ContainerType>();
        public static GenericRegisterer<ChapterType> ChapterTypes = new GenericRegisterer<ChapterType>();
        public static GenericRegisterer<DeviceType> DeviceTypes = new GenericRegisterer<DeviceType>();

        static ContainerManager()
        {
            if (!(
                VideoTypes.Register(VideoType.AVI)     &&
                VideoTypes.Register(VideoType.MKV)     &&
                VideoTypes.Register(VideoType.MP4)     &&
                VideoTypes.Register(VideoType.RAWASP)  &&
                VideoTypes.Register(VideoType.RAWAVC)  &&
                VideoTypes.Register(VideoType.MPEG2)   &&
                VideoTypes.Register(VideoType.VC1)     &&
                VideoTypes.Register(VideoType.M2TS)))
                throw new Exception("Failed to register a video type");
            if (!(
                AudioTypes.Register(AudioType.AC3)    &&
                AudioTypes.Register(AudioType.MP3)    &&
                AudioTypes.Register(AudioType.DTS)    &&
                AudioTypes.Register(AudioType.MP2)    &&
                AudioTypes.Register(AudioType.MP4AAC) &&
                AudioTypes.Register(AudioType.M4A)    &&
                AudioTypes.Register(AudioType.RAWAAC) &&
                AudioTypes.Register(AudioType.VORBIS) &&
                AudioTypes.Register(AudioType.DTSHD)  &&
                AudioTypes.Register(AudioType.DTSMA)  &&
                AudioTypes.Register(AudioType.EAC3)   &&
                AudioTypes.Register(AudioType.THD)))
                throw new Exception("Failed to register an audio type");
            if (!(
                SubtitleTypes.Register(SubtitleType.ASS)    &&
                SubtitleTypes.Register(SubtitleType.SSA)    &&
                SubtitleTypes.Register(SubtitleType.SUBRIP) &&
                SubtitleTypes.Register(SubtitleType.BDSUP)  &&
                SubtitleTypes.Register(SubtitleType.VOBSUB)))
                throw new Exception("Failed to register a subtitle type");
            if (!(
                ContainerTypes.Register(ContainerType.AVI) &&
                ContainerTypes.Register(ContainerType.MKV) &&
                ContainerTypes.Register(ContainerType.MP4) &&
                ContainerTypes.Register(ContainerType.M2TS)))
                throw new Exception("Failed to register a container type");
            if (!(
	            ChapterTypes.Register(ChapterType.OGG_TXT) &&
                ChapterTypes.Register(ChapterType.MKV_XML)))
		        throw new Exception("Failed to register a chapter type");
            if (!(
            //    DeviceTypes.Register(DeviceType.AVCHD) &&
            //    DeviceTypes.Register(DeviceType.BD) &&
                DeviceTypes.Register(DeviceType.IPOD) &&
                DeviceTypes.Register(DeviceType.PSP) &&
                DeviceTypes.Register(DeviceType.IPHONE) &&
                DeviceTypes.Register(DeviceType.ISMA)))
                throw new Exception("Failed to register a device type");
        }
    }

    public class OutputFileType : IIDable
    {
        public OutputFileType(string name, string filterName, string extension)
        {
            this.name = name;
            this.filterName = filterName;
            this.extension = extension;
        }

        public string ID
        {
            get { return name; }
        }

        private string name, filterName, extension;
        /// <summary>
        /// used to display the output type in dropdowns
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return this.name;
        }
        public string OutputFilter
        {
            get { return "*." + extension; }
        }
        /// <summary>
        /// gets a valid filter string for file dialogs based on the known extension
        /// </summary>
        /// <returns></returns>
        public string OutputFilterString
        {
            get {return filterName + " (*." + extension + ")|*." + extension;}
        }
        /// <summary>
        /// gets the extension for this file type
        /// </summary>
        public string Extension
        {
            get {return this.extension;}
        }
    }

    public class OutputType : OutputFileType
    {
        public OutputType(string name, string filterName, string extension, ContainerType containerType)
            : base(name, filterName, extension)
        {
            this.containerType = containerType;
        }

        private ContainerType containerType;
        public ContainerType ContainerType
        {
            get { return this.containerType; }
        }
    }
}
