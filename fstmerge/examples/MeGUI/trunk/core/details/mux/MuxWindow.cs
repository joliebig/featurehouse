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
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using MeGUI.core.util;
using MeGUI.core.details;

namespace MeGUI
{
    public partial class MuxWindow : baseMuxWindow
    {
        public static readonly IDable<ReconfigureJob> Configurer = new IDable<ReconfigureJob>("mux_reconfigure", new ReconfigureJob(ReconfigureJob));

        private static Job ReconfigureJob(Job j)
        {
            if (!(j is MuxJob))
                return null;

            MuxJob m = (MuxJob)j;
            MuxWindow w = new MuxWindow(
                MainForm.Instance.MuxProvider.GetMuxer(m.MuxType), 
                MainForm.Instance);

            w.Job = m;
            if (w.ShowDialog() == DialogResult.OK)
                return w.Job;
            else
                return m;
        }

        private IMuxing muxer;

        public MuxWindow(IMuxing muxer, MainForm mainForm)
            : base(mainForm)
        {
            InitializeComponent();
            this.muxer = muxer;
            if (muxer.GetSupportedAudioTypes().Count == 0)
                audio.Enabled = false;
            if (muxer.GetSupportedChapterTypes().Count == 0)
                chaptersGroupbox.Enabled = false;
            if (muxer.GetSupportedSubtitleTypes().Count == 0)
                subtitles.Enabled = false;
            if (muxer.GetSupportedChapterTypes().Count == 0)
                chaptersGroupbox.Enabled = false;
            if (muxer.GetSupportedDeviceTypes().Count == 0)
                cbType.Enabled = false;
            muxedInput.Filter = muxer.GetMuxedInputFilter();

            audioTracks[0].Filter = muxer.GetAudioInputFilter();
            output.Filter = muxer.GetOutputTypeFilter();
            subtitleTracks[0].Filter = muxer.GetSubtitleInputFilter();
            vInput.Filter = muxer.GetVideoInputFilter();
            chapters.Filter = muxer.GetChapterInputFilter();
        }

        protected virtual MuxJob generateMuxJob()
        {
            MuxJob job = new MuxJob();
            string chapters;
            MuxStream[] aStreams, sStreams;
            getAdditionalStreams(out aStreams, out sStreams, out chapters);
            job.Settings.AudioStreams.AddRange(aStreams);
            job.Settings.SubtitleStreams.AddRange(sStreams);
            job.Settings.ChapterFile = this.chapters.Filename;
            job.Settings.VideoName = this.videoName.Text;
            job.Settings.VideoInput = vInput.Filename;
            job.Settings.MuxedOutput = output.Filename;
            job.Settings.MuxedInput = this.muxedInput.Filename;
            job.Settings.DAR = base.dar;
            job.Settings.DeviceType = this.cbType.Text;
            
            if (string.IsNullOrEmpty(job.Settings.VideoInput))
                job.Input = job.Settings.MuxedInput;
            else
                job.Input = job.Settings.VideoInput;

            job.Output = job.Settings.MuxedOutput;
            job.MuxType = muxer.MuxerType;
            job.ContainerType = getContainerType(job.Settings.MuxedOutput);
            job.Settings.Framerate = fps.Value;
            
            Debug.Assert(!splitting.Value.HasValue || splitting.Value.Value >= new FileSize(Unit.MB, 1));
            job.Settings.SplitSize = splitting.Value;
            return job;
        }

        public MuxJob Job
        {
            get { return generateMuxJob(); }
            set
            {
                setConfig(value.Settings.VideoInput, value.Settings.MuxedInput, value.Settings.Framerate,
                    value.Settings.AudioStreams.ToArray(), value.Settings.SubtitleStreams.ToArray(),
                    value.Settings.ChapterFile, value.Settings.MuxedOutput, value.Settings.SplitSize,
                    value.Settings.DAR, value.Settings.DeviceType);
            }
        }

        private void setConfig(string videoInput, string muxedInput, decimal? framerate, MuxStream[] audioStreams,
            MuxStream[] subtitleStreams, string chapterFile, string output, FileSize? splitSize, Dar? dar, string deviceType)
        {
            base.setConfig(videoInput, framerate, audioStreams, subtitleStreams, chapterFile, output, splitSize, dar, deviceType);
            this.muxedInput.Filename = muxedInput;
            this.checkIO();
        }

        protected override void ChangeOutputExtension()
        {
            foreach (ContainerType t in muxer.GetSupportedContainers())
            {
                if (output.Filename.ToLower().EndsWith(t.Extension.ToLower()))
                    return;
            }
            output.Filename = Path.ChangeExtension(output.Filename, muxer.GetSupportedContainers()[0].Extension);
        }

        private ContainerType getContainerType(string outputFilename)
        {
            Debug.Assert(outputFilename != null);
            foreach (ContainerType t in muxer.GetSupportedContainers())
            {
                if (outputFilename.ToLower().EndsWith(t.Extension.ToLower()))
                    return t;
            }
            Debug.Assert(false);
            return null;
        }

        protected override bool isFPSRequired()
        {
            if (vInput.Filename.Length > 0)
                return base.isFPSRequired();
            else if (muxedInput.Filename.Length > 0)
                return false;
            else
                return true;
        }

        private void muxedInput_FileSelected(FileBar sender, FileBarEventArgs args)
        {
            checkIO();
            fileUpdated();
        }
    }
}