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
using System.Diagnostics;
using System.Drawing;
using System.Text;

using MeGUI.core.details;
using MeGUI.core.util;

namespace MeGUI
{
    public sealed class MediaFile
    {
        public ContainerType Container;

        public List<MediaTrack> Tracks;
        
        public VideoTrack VideoTrack;
        public List<AudioTrack> AudioTracks;
        public List<SubtitleTrack> SubtitleTracks;
        public Chapters Chapters;

        public TimeSpan PlayTime;

        public MediaFile(List<MediaTrack> tracks, Chapters chapters, TimeSpan playTime, ContainerType container)
        {
            this.Container = container;

            AudioTracks = new List<AudioTrack>();
            SubtitleTracks = new List<SubtitleTrack>();

            foreach (MediaTrack m in tracks)
            {
                if (m is VideoTrack)
                {
                    Debug.Assert(VideoTrack == null, "Only one video track per file supported");
                    VideoTrack = (VideoTrack)m;
                }
                if (m is AudioTrack)
                    AudioTracks.Add((AudioTrack)m);
                if (m is SubtitleTrack)
                    SubtitleTracks.Add((SubtitleTrack)m);
            }

            tracks.Sort(
                delegate(MediaTrack a, MediaTrack b)
                {
                    return (int)a.TrackNumber - (int)b.TrackNumber;
                });

            Tracks = tracks;

            Chapters = chapters;
            PlayTime = playTime;
        }
    }

    public class MediaTrack
    {
        public TrackInfo Info;
        public uint TrackNumber;
        public ICodec Codec;
    }

    public sealed class VideoTrack : MediaTrack 
    {
        public VideoCodec VCodec;
        public VideoInfo2 StreamInfo;
    }

    public sealed class AudioTrack : MediaTrack 
    {
        public AudioCodec ACodec;
        public AudioInfo StreamInfo;
    }

    public sealed class SubtitleTrack : MediaTrack 
    {
        public SubtitleCodec SCodec;
        public SubtitleInfo2 StreamInfo;
    }

    public sealed class Chapters {
        public List<Chapter> Data;
    }


    public class VideoInfo2
    {
        public ulong Width;
        public ulong Height;
        public Dar DAR;
        public ulong FrameCount;
        public double FPS;

        public VideoInfo2() { }

        public VideoInfo2(ulong width, ulong height,
            Dar dar, ulong framecount, double fps)
        {
            Width = width;
            Height = height;
            DAR = dar;
            FrameCount = framecount;
            FPS = fps;
        }
    }

    public class AudioInfo
    { }

    public class SubtitleInfo2
    { }

    #region old
    public interface IMediaFileFactory : IIDable
    {
        /// <summary>
        /// Tries to open the given file. Returns null if impossible.
        /// </summary>
        /// <param name="file">The media file to open</param>
        /// <returns></returns>
        IMediaFile Open(string file);
        /// <summary>
        /// Returns how well a given file is expected to be handled. This is so that better handlers
        /// can be given preference. This should be implemented only by filename and not by opening the file.
        /// </summary>
        /// <param name="extension">The filename to be checked.</param>
        /// <returns>Higher number for better handling, negative for impossible.</returns>
        int HandleLevel(string file);
    }


    public class MediaFileInfo
    {
        public bool HasVideo;
        public ulong Width;
        public ulong Height;
        public Dar DAR;
        public ulong FrameCount;
        public double FPS;
        public bool HasAudio;

        public MediaFileInfo(bool hasVideo, 
            ulong width, ulong height,
            Dar dar, ulong frameCount,
            double fps, bool hasAudio)
        {
            HasVideo = hasVideo;
            Width = width;
            Height = height;
            DAR = dar;
            FrameCount = frameCount;
            FPS = fps;
            HasAudio = hasAudio;
        }

        public MediaFileInfo Clone()
        {
            return (MediaFileInfo)this.MemberwiseClone();
        }
    }
    public interface IMediaFile : IDisposable
    {
        MediaFileInfo Info
        {
            get;
        }

        bool CanReadVideo
        {
            get;
        }
        bool CanReadAudio
        {
            get;
        }
        IVideoReader GetVideoReader();
        IAudioReader GetAudioReader(int track);

    }
    
    public interface IAudioReader
    {
        /// <summary>
        /// Returns the number of samples readable.
        /// </summary>
        long SampleCount
        {
            get;
        }
        
        /// <summary>
        /// Gets whether ReadAudioSamples(long, int, IntPtr) is a supported operation.
        /// </summary>
        bool SupportsFastReading
        {
            get;
        }
        
        /// <summary>
        /// Reads up to nAmount samples into an array at buf. Fast method because it avoids the Marshaller.
        /// This needn't be supported; the SupportsFastReading property indicates whether it is.
        /// </summary>
        /// <param name="nStart">the index of the first sample</param>
        /// <param name="nAmount">the maximum number of samples to read</param>
        /// <param name="buf">a pointer to the memory for the samples</param>
        /// <returns>the number of samples read</returns>
        long ReadAudioSamples(long nStart, int nAmount, IntPtr buf);

        /// <summary>
        /// Reads up to nAmount samples into an array and returns it. Slow method because of Marshaller.
        /// This must be supported.
        /// </summary>
        /// <param name="nStart">the index of the first sample</param>
        /// <param name="nAmount">the maximum number of samples to read</param>
        /// <returns>a newly-constructed array of samples</returns>
        byte[] ReadAudioSamples(long nStart, int nAmount);
    }
    /// <summary>
    /// The interface for sourcing frames
    /// </summary>
    public interface IVideoReader
    {
        /// <summary>
        /// Returns the number of frames readable.
        /// </summary>
        int FrameCount
        {
            get;
        }

        /// <summary>
        /// Reads and returns frame 'framenumber' from the video stream. Slow method, because of Marshaller.
        /// This must be supported
        /// </summary>
        /// <param name="framenumber">the 0-indexed frame number to get</param>
        /// <returns>The frame just read</returns>
        Bitmap ReadFrameBitmap(int framenumber);
    }
    #endregion
}