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
using System.Windows.Forms;

using MeGUI.core.details;
using MeGUI.core.util;

namespace MeGUI
{
    class MkvMergeMuxer : CommandlineMuxer
    {
        public static readonly JobProcessorFactory Factory =
new JobProcessorFactory(new ProcessorFactory(init), "MkvMergeMuxer");
        
        private static IJobProcessor init(MainForm mf, Job j)
        {
            if (j is MuxJob && (j as MuxJob).MuxType == MuxerType.MKVMERGE)
                return new MkvMergeMuxer(mf.Settings.MkvmergePath);
            return null;
        }
        
        public MkvMergeMuxer(string executablePath)
        {
            this.executable = executablePath;
        }


        
        /// <summary>
        /// gets the framenumber from an mkvmerge status update line
        /// </summary>
        /// <param name="line">mkvmerge commandline output</param>
        /// <returns>the framenumber included in the line</returns>
        public decimal? getPercentage(string line)
        {
            try
            {
                int percentageStart = 10;
                int percentageEnd = line.IndexOf("%");
                string frameNumber = line.Substring(percentageStart, percentageEnd - percentageStart).Trim();
                return Int32.Parse(frameNumber);
            }
            catch (Exception e)
            {
                log.LogValue("Exception in getPercentage(" + line + ")", e, MeGUI.core.util.ImageType.Warning);
                return null;
            }
        }
        

        protected override bool checkExitCode
        {
            get { return false; }
        }

        public override void ProcessLine(string line, StreamType stream)
        {
            if (line.StartsWith("Progress: ")) //status update
                su.PercentageDoneExact = getPercentage(line);
            else if (line.StartsWith("Error: "))
            {
                log.LogValue("An error occurred", line, ImageType.Error);
                su.HasError = true;
            }
            else if (line.StartsWith("Warning: "))
                log.LogValue("A warning occurred", line, ImageType.Warning);
            else
                base.ProcessLine(line, stream);
        }

        protected override string Commandline
        {
            get
            {
                StringBuilder sb = new StringBuilder();
                MuxSettings settings = job.Settings;
                int trackID;
                
                sb.Append("-o \"" + settings.MuxedOutput + "\"");

                if (!string.IsNullOrEmpty(settings.VideoInput))
                {
                    if (settings.VideoInput.ToLower().EndsWith(".mp4") || settings.VideoInput.ToLower().EndsWith(".mkv"))
                         trackID = VideoUtil.getIDFromFirstVideoStream(settings.VideoInput);
                    else trackID = 0;
                    sb.Append(" --engage keep_bitstream_ar_info"); // assuming that SAR info is already in the stream...
                    if (!string.IsNullOrEmpty(settings.VideoName))
                        sb.Append(" --track-name \"" + trackID + ":" + settings.VideoName + "\"");
                    if (settings.Framerate.HasValue)
                        sb.Append(" --default-duration " + trackID + ":" + PrettyFormatting.ReplaceFPSValue(settings.Framerate.Value.ToString()) + "fps");
                    sb.Append(" -d " + trackID + " -A -S \"" + settings.VideoInput + "\"");                    
                }
                
                if (!string.IsNullOrEmpty(settings.MuxedInput))
                {
                    if (settings.MuxedInput.ToLower().EndsWith(".mp4") || settings.MuxedInput.ToLower().EndsWith(".mkv"))
                         trackID = VideoUtil.getIDFromFirstVideoStream(settings.MuxedInput);
                    else trackID = 0;
                    if (settings.DAR.HasValue)
                        sb.Append(" --aspect-ratio " + trackID + ":" + settings.DAR.Value.X + "/" + settings.DAR.Value.Y);
                    else
                        sb.Append(" --engage keep_bitstream_ar_info");
                    if (!string.IsNullOrEmpty(settings.VideoName))
                        sb.Append(" --track-name \"" + trackID + ":" + settings.VideoName + "\"");
                    if (settings.Framerate.HasValue)
                        sb.Append(" --default-duration " + trackID + ":" + PrettyFormatting.ReplaceFPSValue(settings.Framerate.Value.ToString()) + "fps");
                    sb.Append(" -d " + trackID + " -A -S \"" + settings.MuxedInput + "\""); 
                }

                foreach (object o in settings.AudioStreams)
                {
                    MuxStream stream = (MuxStream)o;
                    trackID = 0; int heaac_flag = 0; 
                    if (stream.path.ToLower().EndsWith(".mp4") || stream.path.ToLower().EndsWith(".m4a"))
                    {
                        trackID = VideoUtil.getIDFromAudioStream(stream.path);
                        heaac_flag = VideoUtil.getSBRFlagFromAACStream(stream.path);
                        if (heaac_flag > 0)
                            sb.Append(" --aac-is-sbr "+ trackID + ":1");
                    }
                    if (stream.path.ToLower().EndsWith(".aac"))
                    {
                        heaac_flag = VideoUtil.getSBRFlagFromAACStream(stream.path);
                        if (heaac_flag > 0)
                            sb.Append(" --aac-is-sbr 0:1");
                    }
                    if (!string.IsNullOrEmpty(stream.language))
                    {
                        foreach (KeyValuePair<string, string> strLanguage in LanguageSelectionContainer.Languages)
                        {
                            if (stream.language.ToLower().Equals(strLanguage.Key.ToLower()))
                            {
                                sb.Append(" --language " + trackID + ":" + strLanguage.Value);
                                break;
                            }
                        }
                    }
                    if (!string.IsNullOrEmpty(stream.name))
                        sb.Append(" --track-name \"" + trackID + ":" + stream.name + "\"");
                    if (stream.delay != 0)
                        sb.AppendFormat(" --sync {0}:{1}ms", trackID, stream.delay);
                    sb.Append(" -a " + trackID + " -D -S \"" + stream.path + "\"");
                }

                foreach (object o in settings.SubtitleStreams)
                {
                    MuxStream stream = (MuxStream)o;
                    List<SubtitleInfo> subTracks;
                    idxReader.readFileProperties(stream.path, out subTracks);
                    trackID = 0; int nb = 0; int nt = 0;
                    if (stream.path.ToLower().EndsWith(".idx"))
                    {
                        foreach (SubtitleInfo strack in subTracks)
                        {
                            if (nt > 0)
                            {
                                if (!string.IsNullOrEmpty(stream.language))
                                {
                                    foreach (KeyValuePair<string, string> strLanguage in LanguageSelectionContainer.Languages)
                                    {
                                        if (stream.language.ToLower().Equals(strLanguage.Key.ToLower()))
                                        {
                                            sb.Append(" --language " + strack.Index.ToString() + ":" + strLanguage.Value);
                                            break;
                                        }
                                    }
                                }
                                else sb.Append(" --language " + strack.Index.ToString() + ":" + stream.name);
                                if (!string.IsNullOrEmpty(stream.name))
                                    sb.Append(" --track-name \"" + strack.Index.ToString() + ":" + stream.name + "\"");
                            }
                            else
                            {
                                if (!string.IsNullOrEmpty(stream.language))
                                {
                                    foreach (KeyValuePair<string, string> strLanguage in LanguageSelectionContainer.Languages)
                                    {
                                        if (stream.language.ToLower().Equals(strLanguage.Key.ToLower()))
                                        {
                                            sb.Append(" --language " + "0:" + strLanguage.Value);
                                            break;
                                        }
                                    }
                                }  
                                else sb.Append(" --language " + "0:" + strack.Name);
                                if (!string.IsNullOrEmpty(stream.name))
                                    sb.Append(" --track-name \"" + "0:" + stream.name + "\"");
                            }
                            ++nt;
                        }
                        sb.Append(" -s ");
                        foreach (SubtitleInfo strack in subTracks)
                        {
                            if (nb > 0)
                                 sb.Append("," + strack.Index.ToString());
                            else sb.Append("0");
                            ++nb;
                        }
                        sb.Append(" -D -A \"" + stream.path + "\"");
                    }
                    else
                    {
                        if (!string.IsNullOrEmpty(stream.language))
                        {
                            foreach (KeyValuePair<string, string> strLanguage in LanguageSelectionContainer.Languages)
                            {
                                if (stream.language.ToLower().Equals(strLanguage.Key.ToLower()))
                                {
                                    sb.Append(" --language " + trackID + ":" + strLanguage.Value);
                                    break;
                                }
                            }
                        }
                        if (!string.IsNullOrEmpty(stream.name))
                             sb.Append(" --track-name \"" + trackID + ":" + stream.name + "\"");
                        sb.Append(" -s 0 -D -A \"" + stream.path + "\"");
                    }
                }
                if (!string.IsNullOrEmpty(settings.ChapterFile)) // a chapter file is defined
                    sb.Append(" --chapters \"" + settings.ChapterFile + "\"");

                if (settings.SplitSize.HasValue)
                    sb.Append(" --split " + (settings.SplitSize.Value.MB) + "M");

                sb.Append(" --no-clusters-in-meta-seek"); // ensures lower overhead

                return sb.ToString();
            }
        }
    }
}
