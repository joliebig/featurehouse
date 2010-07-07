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
using System.Globalization;
using System.IO;
using System.Text;
using System.Windows.Forms;

using MeGUI.core.details;
using MeGUI.core.util;

namespace MeGUI
{
    class MP4BoxMuxer : CommandlineMuxer
    {
        public static readonly JobProcessorFactory Factory =
new JobProcessorFactory(new ProcessorFactory(init), "MP4BoxMuxer");

        private static IJobProcessor init(MainForm mf, Job j)
        {
            if (j is MuxJob && (j as MuxJob).MuxType == MuxerType.MP4BOX)
                return new MP4BoxMuxer(mf.Settings.Mp4boxPath);
            return null;
        }
        
        private int numberOfAudioTracks, numberOfSubtitleTracks, trackNumber;
        private enum LineType : int {other = 0, importing, writing, splitting, empty, error };
        private string lastLine;

        public MP4BoxMuxer(string executablePath)
        {
            this.executable = executablePath;
            trackNumber = 0;
            lastLine = "";
        }
        
        protected override void checkJobIO()
        {
            this.numberOfAudioTracks = job.Settings.AudioStreams.Count;
            this.numberOfSubtitleTracks = job.Settings.SubtitleStreams.Count;
            
            base.checkJobIO();
        }

        

        
        /// <summary>
        /// looks at a line and returns its type
        /// </summary>
        /// <param name="line">the line to be analyzed</param>
        /// <returns>the line type</returns>
        private LineType getLineType(string line)
        {
            if (line.StartsWith("Importing"))
                return LineType.importing;
            if (line.StartsWith("ISO File Writing"))
                return LineType.writing;
            if (line.StartsWith("Splitting"))
                return LineType.splitting;
            if (isEmptyLine(line))
                return LineType.empty;
            if (line.IndexOf("Error") != -1 || line.IndexOf("unknown") != -1)
                return LineType.error;
            return LineType.other;
        }
        /// <summary>
        /// gets the completion percentage of an mp4box line
        /// </summary>
        /// <param name="line"></param>
        /// <returns></returns>
        private decimal? getPercentage(string line)
        {
            try
            {
                int start = line.IndexOf("(") + 1;
                int end = line.IndexOf("/");
                string perc = line.Substring(start, end - start);
                int percentage = Int32.Parse(perc);
                return percentage;
            }
            catch (Exception e)
            {
                log.LogValue("Exception in getPercentage(" + line + ") ", e, ImageType.Warning);
                return null;
            }
        }
        /// <summary>
        /// determines if a read line is empty
        /// </summary>
        /// <param name="line"></param>
        /// <returns></returns>
        private bool isEmptyLine(string line)
        {
            char[] characters = line.ToCharArray();
            bool isEmpty = true;
            foreach (char c in characters)
            {
                if (c != 32)
                {
                    isEmpty = false;
                    break;
                }
            }
            return isEmpty;
        }
        
        
        /// <summary>
        /// compiles mp4 overhead statistics and dumps them to the log and a logfile
        /// </summary>
        public void printStatistics()
        {
            try
            {
                FileSize len = FileSize.Of(job.Output);
                FileSize Empty = FileSize.Empty;
                if (!Path.GetExtension(job.Input).ToLower().Equals(".mp4"))
                {
                    FileSize rawSize = FileSize.Of(job.Input);
                    LogItem i = new LogItem("MP4 Muxing statistics");
                    i.LogValue("Size of raw video stream", rawSize);
                    i.LogValue("Size of final MP4 file", len);
                    i.LogValue("Codec", job.Codec);
                    i.LogValue("Number of b-frames", job.NbOfBFrames);
                    i.LogValue("Number of source frames", job.NbOfFrames);
                    log.Add(i);
                }
            }
            catch (Exception e)
            {
                log.LogValue("An exception occurred when printing mux statistics", e, ImageType.Warning);
            }
        }
        

        public override void ProcessLine(string line, StreamType stream)
        {
            switch (getLineType(line))
            {
                case LineType.empty:
                    if (getLineType(lastLine) == LineType.importing) // moving from one track to another
                        trackNumber++;
                    break;
                case LineType.importing:
                    su.PercentageDoneExact = getPercentage(line);
                    if (trackNumber == 1) // video
                    {
                        if (String.IsNullOrEmpty(su.Status) || !su.Status.Equals("Importing Video Track..."))
                        {
                            su.Status = "Importing Video Track...";
                            startTime = DateTime.Now;
                        }
                    }
                    else if (trackNumber == 2 && numberOfAudioTracks > 0) // first audio track
                    {
                        if (String.IsNullOrEmpty(su.Status) || !su.Status.Equals("Importing Audio Track 1..."))
                        {
                            su.Status = "Importing Audio Track 1...";
                            startTime = DateTime.Now;
                        }
                    }
                    else if (trackNumber == 3 && numberOfAudioTracks > 1) // second audio track
                    {
                        if (String.IsNullOrEmpty(su.Status) || !su.Status.Equals("Importing Audio Track 2..."))
                        {
                            su.Status = "Importing Audio Track 2...";
                            startTime = DateTime.Now;
                        }
                    }
                    else
                    {
                        if (String.IsNullOrEmpty(su.Status) || !su.Status.Equals("Importing Tracks..."))
                        {
                            su.Status = "Importing Tracks...";
                            startTime = DateTime.Now;
                        }
                    }
                    break;

                case LineType.splitting:
                    su.PercentageDoneExact = getPercentage(line);
                    su.Status = "Splitting...";
                    break;

                case LineType.writing:
                    su.PercentageDoneExact = getPercentage(line);
                    if (String.IsNullOrEmpty(su.Status) || !su.Status.Equals("Writing..."))
                    {
                        su.Status = "Writing...";
                        startTime = DateTime.Now;
                    }
                    break;

                case LineType.other:
                    base.ProcessLine(line, stream);
                    break;

                case LineType.error:
                    log.LogValue("Error line", line, ImageType.Error);
                    su.HasError = true;
                    base.ProcessLine(line, stream);
                    break;
            }
            lastLine = line;
        }

        protected override string Commandline
        {
            get
            {
                MuxSettings settings = job.Settings;
                CultureInfo ci = new CultureInfo("en-us");
                StringBuilder sb = new StringBuilder();

                if (!string.IsNullOrEmpty(settings.VideoInput))
                {
                    sb.Append("-add \"" + settings.VideoInput);
                    if (settings.VideoInput.ToLower().EndsWith(".mp4"))
                    {
                        int trackID = VideoUtil.getIDFromFirstVideoStream(settings.VideoInput);
                        sb.Append("#trackID=" + trackID);
                    }
                    if (settings.Framerate.HasValue)
                    {
                        string fpsString = settings.Framerate.Value.ToString(ci);
                        sb.Append(":fps=" + fpsString);
                    }
                    if (!string.IsNullOrEmpty(settings.VideoName))
                        sb.Append(":name=" + settings.VideoName);
                    sb.Append("\"");
                }
                if (!string.IsNullOrEmpty(settings.MuxedInput))
                {
                    sb.Append(" -add \"" + settings.MuxedInput);
                    if (settings.MuxedInput.ToLower().EndsWith(".mp4"))
                    {
                        int trackID = VideoUtil.getIDFromFirstVideoStream(settings.MuxedInput);
                        sb.Append("#trackID=" + trackID);
                    }
                    if (settings.Framerate.HasValue)
                    {
                        string fpsString = settings.Framerate.Value.ToString(ci);
                        sb.Append(":fps=" + fpsString);
                    }
                    if (!string.IsNullOrEmpty(settings.VideoName))
                        sb.Append(":name=" + settings.VideoName);
                    sb.Append("\"");
                }
                foreach (object o in settings.AudioStreams)
                {
                    MuxStream stream = (MuxStream)o;
                    sb.Append(" -add \"" + stream.path);
                    if (stream.path.ToLower().EndsWith(".mp4") || stream.path.ToLower().EndsWith(".m4a"))
                    {
                        int trackID = VideoUtil.getIDFromAudioStream(stream.path);
                        sb.Append("#trackID=" + trackID);
                        int heaac_flag = VideoUtil.getSBRFlagFromAACStream(stream.path);
                        if (heaac_flag > 0)
                            sb.Append(":sbr");
                    }
                    if (stream.path.ToLower().EndsWith(".aac"))
                    {
                        int heaac_flag = VideoUtil.getSBRFlagFromAACStream(stream.path);
                        if (heaac_flag > 0)
                            sb.Append(":sbr");
                    }
                    if (!string.IsNullOrEmpty(stream.language))
                        sb.Append(":lang=" + stream.language);
                    if (!string.IsNullOrEmpty(stream.name))
                        sb.Append(":name=" + stream.name);
                    if (stream.delay != 0)
                        sb.AppendFormat(":delay={0}", stream.delay);
                    sb.Append("\"");
                }
                foreach (object o in settings.SubtitleStreams)
                {
                    MuxStream stream = (MuxStream)o;
                    sb.Append(" -add \"" + stream.path);
                    if (!string.IsNullOrEmpty(stream.language))
                        sb.Append(":lang=" + stream.language);
                    if (!string.IsNullOrEmpty(stream.name))
                        sb.Append(":name=" + stream.name);
                    sb.Append("\"");
                }

                if (!string.IsNullOrEmpty(settings.ChapterFile)) // a chapter file is defined
                    sb.Append(" -chap \"" + settings.ChapterFile + "\"");

                if (settings.SplitSize.HasValue)
                    sb.Append(" -splits " + settings.SplitSize.Value.KB);

                if (settings.DeviceType != "Standard")
                {
                    switch (settings.DeviceType)
                    {
                        case "iPod": sb.Append(" -ipod"); break;
                        case "iPhone": sb.Append(" -ipod -brand M4VP:1"); break;
                        case "ISMA": sb.Append(" -isma"); break;
                        case "PSP": sb.Append(" -psp"); break;
                    }
                }

                // tmp directory
                // due to a bug from MP4Box, we need to test the path delimiter number
                if (!String.IsNullOrEmpty(MainForm.Instance.Settings.TempDirMP4) && Directory.Exists(MainForm.Instance.Settings.TempDirMP4))
                {
                    if (Util.CountStrings(MainForm.Instance.Settings.TempDirMP4, '\\') > 1)
                        sb.AppendFormat(" -tmp \"{0}\"", MainForm.Instance.Settings.TempDirMP4);
                    else
                        sb.AppendFormat(" -tmp {0}", MainForm.Instance.Settings.TempDirMP4);
                }
                else
                {
                    if (Util.CountStrings(settings.MuxedOutput, '\\') > 1)
                        sb.AppendFormat(" -tmp \"{0}\"", Path.GetDirectoryName(settings.MuxedOutput));
                    else
                        sb.AppendFormat(" -tmp {0}", Path.GetDirectoryName(settings.MuxedOutput));
                }
                // force to create a new output file
                sb.Append(" -new \"" + settings.MuxedOutput + "\"");
                return sb.ToString();
            }
        }
    }
}
