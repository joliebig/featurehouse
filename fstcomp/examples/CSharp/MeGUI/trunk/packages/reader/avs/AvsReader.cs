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
using System.Drawing;
using System.Text;

using MeGUI.core.util;

namespace MeGUI
{
    public class AvsFileFactory : IMediaFileFactory
    {

        #region IMediaFileFactory Members

        public IMediaFile Open(string file)
        {
            return AvsFile.OpenScriptFile(file);
        }

        #endregion

        #region IMediaFileFactory Members


        public int HandleLevel(string file)
        {
            if (file.ToLower().EndsWith(".avs"))
                return 10;
            return -1;
        }

        #endregion

        #region IIDable Members

        public string ID
        {
            get { return "AviSynth"; }
        }

        #endregion
    }
    public sealed class AvsFile : IMediaFile
    {
        private AviSynthClip clip = null;
        private AviSynthScriptEnvironment enviroment = null;
        private IAudioReader audioReader;
        private IVideoReader videoReader;
        private MediaFileInfo info;
        #region construction
        public AviSynthClip Clip
        {
            get
            {
                return this.clip;
            }
        }

        public static AvsFile OpenScriptFile(string fileName)
        {
            return new AvsFile(fileName, false);
        }

        public static AvsFile ParseScript(string scriptBody)
        {
            return new AvsFile(scriptBody, true);
        }

        private AvsFile(string script, bool parse)
        {
            try
            {
                this.enviroment = new AviSynthScriptEnvironment();
                this.clip = parse ? enviroment.ParseScript(script, AviSynthColorspace.RGB24) : enviroment.OpenScriptFile(script, AviSynthColorspace.RGB24);

                checked
                {
                    ulong width = (ulong)clip.VideoWidth;
                    ulong height = (ulong)clip.VideoHeight;
                    info = new MediaFileInfo(
                        clip.HasVideo, width, height,
                        new Dar(clip.GetIntVariable("MeGUI_darx", -1),
                              clip.GetIntVariable("MeGUI_dary", -1),
                              width, height),
                              (ulong)clip.num_frames,
                              ((double)clip.raten) / ((double)clip.rated),
                              (clip.SamplesCount != 0));
                }
            }
            catch (Exception)
            {
                cleanup();
                throw;
            }
        }

        private void cleanup()
        {
            System.Threading.Thread.Sleep(100);
            if (this.clip != null)
            {
                (this.clip as IDisposable).Dispose();
                this.clip = null;
            }
            if (this.enviroment != null)
            {
                (this.enviroment as IDisposable).Dispose();
                this.enviroment = null;
            }
            GC.SuppressFinalize(this);
        }
        #endregion
        #region properties
        public MediaFileInfo Info
        {
            get { return info; }
        }
        public bool CanReadVideo
        {
            get { return true; }
        }
        public bool CanReadAudio
        {
            get { return true; }
        }
        #endregion
        public IAudioReader GetAudioReader(int track)
        {
            if (track != 0 || !info.HasAudio)
                throw new Exception(string.Format("Can't read audio track {0}, because it can't be found", track));
            if (audioReader == null)
                lock (this)
                {
                    if (audioReader == null)
                        audioReader = new AvsAudioReader(clip);
                }
            return audioReader;
        }
        public IVideoReader GetVideoReader()
        {
            if (!this.Info.HasVideo)
                throw new Exception("Can't get Video Reader, since there is no video stream!");
            if (videoReader == null)
                lock (this)
                {
                    if (videoReader == null)
                        videoReader = new AvsVideoReader(clip, (int)Info.Width, (int)Info.Height);
                }
            return videoReader;
        }

        sealed class AvsVideoReader : IVideoReader
        {
            public AvsVideoReader(AviSynthClip clip, int width, int height)
            {
                this.clip = clip;
                this.width = width;
                this.height = height;
            }
            private AviSynthClip clip = null;
            int width, height;

            public int FrameCount
            {
                get { return this.clip.num_frames;}
            }

            public Bitmap ReadFrameBitmap(int position)
            {
                Bitmap bmp = new Bitmap(width, height, System.Drawing.Imaging.PixelFormat.Format24bppRgb);
                try
                {
                    // Lock the bitmap's bits.  
                    Rectangle rect = new Rectangle(0, 0, bmp.Width, bmp.Height);
                    System.Drawing.Imaging.BitmapData bmpData =
                        bmp.LockBits(rect, System.Drawing.Imaging.ImageLockMode.ReadWrite,
                        bmp.PixelFormat);
                    try
                    {
                        // Get the address of the first line.
                        IntPtr ptr = bmpData.Scan0;
                        // Read data
                        clip.ReadFrame(ptr, bmpData.Stride, position);
                    }
                    finally
                    {
                        // Unlock the bits.
                        bmp.UnlockBits(bmpData);
                    }
                    bmp.RotateFlip(RotateFlipType.Rotate180FlipX);
                    return bmp;
                }
                catch (Exception)
                {
                    bmp.Dispose();
                    throw;
                }
            }
        }
        sealed class AvsAudioReader : IAudioReader
        {
            public AvsAudioReader(AviSynthClip clip)
            {
                this.clip = clip;
            }
            private AviSynthClip clip;
            public long SampleCount
            {
                get { return clip.SamplesCount; }
            }

            public bool SupportsFastReading
            {
                get { return true; }
            }

            public long ReadAudioSamples(long nStart, int nAmount, IntPtr buf)
            {
                clip.ReadAudio(buf, nStart, nAmount);
#warning Need to check whether nAmount samples are guaranteed to be returned
                return nAmount;
            }

            public byte[] ReadAudioSamples(long nStart, int nAmount)
            {
#warning slow reading is not supported here
                return null;
            }
        }
        #region IDisposable Members

        public void Dispose()
        {
            cleanup();
        }

        #endregion
    }
}
    