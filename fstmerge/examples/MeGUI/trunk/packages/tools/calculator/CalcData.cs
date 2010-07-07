using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using MeGUI.core.util;
using MeGUI.core.details;
using System.Diagnostics;
using eac3to;
using MeGUI.Properties;

namespace MeGUI.packages.tools.calculator
{
    public class CalcData
    {
        private static readonly decimal mp4OverheadWithBframes = 10.4M;
        private static readonly decimal mp4OverheadWithoutBframes = 4.3M;
        private static readonly decimal aviVideoOverhead = 24M;
        private static readonly decimal cbrMP3Overhead = 23.75M;
        private static readonly decimal vbrMP3Overhead = 40M;
        private static readonly decimal ac3Overhead = 23.75M;
        private static readonly int AACBlockSize = 1024;
        private static readonly int AC3BlockSize = 1536;
        private static readonly int MP3BlockSize = 1152;
        private static readonly int VorbisBlockSize = 1024;
        private static readonly int mkvAudioTrackHeaderSize = 140;
        private static readonly int mkvVorbisTrackHeaderSize = 4096;
        private static readonly uint mkvIframeOverhead = 26;
        private static readonly uint mkvPframeOverhead = 13;
        private static readonly uint mkvBframeOverhead = 16;
        private static readonly string qualityCodecModifierValues = "MPEG2=1.022,ASP=1.018,AVC=1.0,VC1=1.004";

        public CalcData(long frames, decimal fps) :
            this(frames, fps, null, null, false, null) { }

        public CalcData(long frames, decimal fps, ContainerType container, VideoCodec codec, bool bframes, AudioBitrateCalculationStream[] audios)
        {
            if (fps <= 0) throw new ArgumentException("Frames per second must be greater than zero.", "fps");
            if (frames <= 0) throw new ArgumentException("Frames must be greater than zero.", "frames");

            Frames = frames;
            FramesPerSecond = fps;
            ContainerType = container;
            VideoCodec = codec;
            HasBFrames = bframes;
            AudioStreams = audios;
            VideoOverheadRatio = 1;
            AudioOverheadRatio = 1;
            ExtraOverheadRatio = 1;
            QualityCoeffient = 0.75F;
        }

        public ContainerType ContainerType { get; set; }
        public VideoCodec VideoCodec { get; set; }
        public AudioBitrateCalculationStream[] AudioStreams { get; set; }
        public bool HasBFrames { get; set; }
        public decimal FramesPerSecond { get; protected set; }
        public long Frames { get; protected set; }
        public Size FrameSize { get; set; }
        public float BitsPerPixel { get; set; }

        public float QualityCodecModifier { get; set; }
        public float QualityCoeffient { get; set; }
        public float QualityEstimate { get; set; }

        public FileSize VideoOverhead { get; set; }
        public FileSize AudioOverhead { get; set; }
        public FileSize ExtraOverhead { get; set; }

        public float VideoOverheadRatio { get; set; }
        public float AudioOverheadRatio { get; set; }
        public float ExtraOverheadRatio { get; set; }

        public FileSize ExtraSize { get; set; }
        public FileSize VideoSize { get; set; }
        public FileSize AudioSize { get; set; }
        public FileSize TotalSize { get; set; }

        public decimal VideoBitrate
        {
            get { return (decimal)VideoSize.Bytes / 1000M / TotalSeconds * 8M; }
            set { VideoSize = new FileSize((ulong)(TotalSeconds * value * 1000M / 8M)); }
        }

        public decimal TotalSeconds { get { return (decimal)Frames / (decimal)FramesPerSecond; } }
        public FileSize VideoMuxSize { get { return new FileSize((ulong)((float)VideoSize.Bytes * VideoOverheadRatio) + VideoOverhead.Bytes); } }
        public FileSize AudioMuxSize { get { return new FileSize((ulong)((float)AudioSize.Bytes * AudioOverheadRatio) + AudioOverhead.Bytes); } }
        public FileSize ExtraMuxSize { get { return new FileSize((ulong)((float)ExtraSize.Bytes * ExtraOverheadRatio) + ExtraOverhead.Bytes); } }
        
        public void CalcByTotalSize()
        {
            CalcVideoOverhead();
            CalcAudioOverheadAndSize();
            CalcExtraOverhead();
            CalcQualityCodecModifier();
            VideoSize = new FileSize((ulong)((float)TotalSize.Bytes / VideoOverheadRatio)) - AudioMuxSize - ExtraMuxSize - VideoOverhead;
            CalcBitsPerPixel();
            CalcQualityEstimate();
        }

        public void CalcByVideoSize()
        {
            CalcVideoOverhead();
            CalcAudioOverheadAndSize();
            CalcExtraOverhead();
            CalcQualityCodecModifier();
            TotalSize = VideoMuxSize + AudioMuxSize + ExtraMuxSize;
            CalcBitsPerPixel();
            CalcQualityEstimate();
        }

        public void CalcByBitsPerPixel()
        {
            CalcVideoOverhead();
            CalcAudioOverheadAndSize();
            CalcExtraOverhead();
            CalcQualityCodecModifier();
            VideoSize = new FileSize((ulong)((
                BitsPerPixel / 8F * (float)(FrameSize.Width * FrameSize.Height) * Frames / VideoOverheadRatio)) - VideoOverhead.Bytes);
            TotalSize = VideoMuxSize + AudioMuxSize + ExtraMuxSize;
            CalcQualityEstimate();
        }

        public void CalcByQualityEstimate()
        {
            CalcVideoOverhead();
            CalcAudioOverheadAndSize();
            CalcExtraOverhead();
            CalcQualityCodecModifier();
            VideoBitrate = (decimal)QualityEstimate * ((decimal)Math.Pow((float)(FrameSize.Width * FrameSize.Height), QualityCoeffient * QualityCodecModifier) * FramesPerSecond) / 1000M;
            TotalSize = VideoMuxSize + AudioMuxSize + ExtraMuxSize;
            CalcBitsPerPixel();
        }

        private void CalcBitsPerPixel()
        {
            BitsPerPixel = (float)VideoSize.Bytes * 8F / (float)Frames / (float)(FrameSize.Width * FrameSize.Height);
        }

        private void CalcQualityEstimate()
        {
            QualityEstimate = (float)VideoBitrate / ((float)Math.Pow((float)(FrameSize.Width * FrameSize.Height), QualityCoeffient * QualityCodecModifier) * (float)FramesPerSecond) * 1000F;
        }

        private void CalcQualityCodecModifier()
        {
            var qualityCodecModifiers = new Dictionary<string, float>();

            // read the values into the dictionary
            foreach (string mod in qualityCodecModifierValues.Split(','))
            {
                qualityCodecModifiers.Add(mod.Split('=')[0], float.Parse(mod.Split('=')[1], System.Globalization.CultureInfo.InvariantCulture));
            }

            // use values when found in dictionary, otherwise default to no-modification
            if (qualityCodecModifiers.ContainsKey(VideoCodec.ID))
                QualityCodecModifier = qualityCodecModifiers[VideoCodec.ID];
            else
                QualityCodecModifier = 1F;
        }

        private void CalcVideoOverhead()
        {
            VideoOverheadRatio = 1;

            if (ContainerType == ContainerType.MP4)
            {
                VideoOverhead = new FileSize(Unit.B,
                    (HasBFrames ? mp4OverheadWithBframes : mp4OverheadWithoutBframes) * Frames);
            }
            else if (ContainerType == ContainerType.MKV)
            {
                long nbIframes = Frames / 10;
                long nbBframes = HasBFrames ? (Frames - nbIframes) / 2 : 0;
                long nbPframes = Frames - nbIframes - nbBframes;
                VideoOverhead = new FileSize(Unit.B,
                    (4300M + 1400M + nbIframes * mkvIframeOverhead + nbPframes * mkvPframeOverhead +
                    nbBframes * mkvBframeOverhead + 
                    TotalSeconds * 12 / 2 // this line for 12 bytes per cluster overhoad
                    ));
            }
            else if (ContainerType == ContainerType.AVI)
            {
                VideoOverhead = new FileSize(Unit.B, Frames * aviVideoOverhead);
            }
            else if (ContainerType == ContainerType.M2TS)
            {
                // for m2ts, video overhead is a ratio (rather than constant)
                VideoOverheadRatio = 106F / 100F;
            }
        }

        private void CalcAudioOverheadAndSize()
        {
            AudioOverheadRatio = 1;
            AudioSize = FileSize.Empty;
            AudioOverhead = FileSize.Empty;

            if (ContainerType == ContainerType.M2TS)
                AudioOverheadRatio = 106F / 100F;

            foreach (var audio in AudioStreams)
            {
                AudioSize += (audio.Size ?? FileSize.Empty);

                if (ContainerType == ContainerType.MKV)
                    AudioOverhead += new FileSize(Unit.B, GetMkvAudioOverhead(audio.AType, 48000, (double)TotalSeconds));
                else if (ContainerType == ContainerType.M2TS)
                    AudioOverhead +=  new FileSize(Unit.B, GetM2tsAudioOverhead(audio.AType, 48000, (double)TotalSeconds));
                else if (ContainerType == ContainerType.AVI)
                    AudioOverhead +=  new FileSize(Unit.B, GetAviAudioOverhead(audio.AType) * Frames);
            }
        }

        private void CalcExtraOverhead()
        {
            if (ContainerType == ContainerType.M2TS)
                ExtraOverheadRatio = 2;
            else
                ExtraOverheadRatio = 1;
        }

        /// <summary>
        /// gets the overhead a given audio type will incurr in the matroska container
        /// given its length and sampling rate
        /// </summary>
        /// <param name="AudioType">type of the audio track</param>
        /// <param name="samplingRate">sampling rate of the audio track</param>
        /// <param name="length">length of the audio track</param>
        /// <returns>overhead this audio track will incurr</returns>
        private static int GetMkvAudioOverhead(AudioType audioType, int samplingRate, double length)
        {
            if (audioType == null)
                return 0;
            Int64 nbSamples = Convert.ToInt64((double)samplingRate * length);
            int headerSize = mkvAudioTrackHeaderSize;
            int samplesPerBlock = 0;
            if (audioType == AudioType.MP4AAC || audioType == AudioType.M4A || audioType == AudioType.RAWAAC)
                samplesPerBlock = AACBlockSize;
            else if (audioType == AudioType.VBRMP3 || audioType == AudioType.CBRMP3 || audioType == AudioType.MP3 || audioType == AudioType.DTS)
                samplesPerBlock = MP3BlockSize;
            else if (audioType == AudioType.AC3)
                samplesPerBlock = AC3BlockSize;
            else if (audioType == AudioType.VORBIS)
            {
                samplesPerBlock = VorbisBlockSize;
                headerSize = mkvVorbisTrackHeaderSize;
            }
            else // unknown types..
            {
                samplesPerBlock = AC3BlockSize;
            }
            double blockOverhead = (double)nbSamples / (double)samplesPerBlock * 22.0 / 8.0;
            int overhead = (int)(headerSize + 5 * length + blockOverhead);
            return overhead;
        }

        /// <summary>
        /// gets the overhead a given audio type will incurr in the m2ts container
        /// given its length and sampling rate
        /// </summary>
        /// <param name="AudioType">type of the audio track</param>
        /// <param name="samplingRate">sampling rate of the audio track</param>
        /// <param name="length">length of the audio track</param>
        /// <returns>overhead this audio track will incurr</returns>
        private static int GetM2tsAudioOverhead(AudioType audioType, int samplingRate, double length)
        {
            if (audioType == null)
                return 0;
            // TODO: ??
            return 0;
        }

        /// <summary>
        /// gets the avi container overhead for the given audio type and bitrate mode
        /// bitrate mode only needs to be taken into account for MP3 but it's there for all cases nontheless
        /// </summary>
        /// <param name="AudioType">the type of audio</param>
        /// <param name="bitrateMode">the bitrate mode of the given audio type</param>
        /// <returns>the overhead in bytes per frame</returns>
        private static decimal GetAviAudioOverhead(AudioType audioType)
        {
            if (audioType == AudioType.AC3)
                return ac3Overhead;
            else if (audioType == AudioType.MP3)
                return vbrMP3Overhead;
            else if (audioType == AudioType.VBRMP3)
                return vbrMP3Overhead;
            else if (audioType == AudioType.CBRMP3)
                return cbrMP3Overhead;
            else if (audioType == AudioType.RAWAAC)
                return vbrMP3Overhead;
            else if (audioType == AudioType.DTS)
                return ac3Overhead;
            else
                return 0;
        }
    }
}
