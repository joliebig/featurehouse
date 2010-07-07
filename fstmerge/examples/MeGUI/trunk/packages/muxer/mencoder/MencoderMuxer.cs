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

using MeGUI.core.util;
using MeGUI.core.details;

namespace MeGUI
{
    class MencoderMuxer : CommandlineMuxer
    {
        public MencoderMuxer(string executablePath)
        {
            this.executable = executablePath;
        }

        /// <summary>
        /// gets the framenumber from an mencoder status update line
        /// </summary>
        /// <param name="line">mencoder stdout line</param>
        /// <returns>the framenumber included in the line</returns>
        public ulong? getFrameNumber(string line)
        {
            try
            {
                int frameNumberStart = line.IndexOf("s", 4) + 1;
                int frameNumberEnd = line.IndexOf("f");
                string frameNumber = line.Substring(frameNumberStart, frameNumberEnd - frameNumberStart).Trim();
                return ulong.Parse(frameNumber);
            }
            catch (Exception e)
            {
                log.LogValue("Exception in getFrameNumber(" + line + ")", e, ImageType.Warning);
                return null;
            }
        }

        public override void ProcessLine(string line, StreamType stream)
        {
            if (line.StartsWith("Pos:")) // status update
            {
                su.NbFramesDone = getFrameNumber(line);
                return;
            }
            
            if (line.IndexOf("error") != -1)
            {
                log.LogValue("An error occurred", line, ImageType.Error);
                su.HasError = true;
            }
            else if (line.IndexOf("not an MEncoder option") != -1)
            {
                log.LogValue("Unrecognized commandline parameter", line, ImageType.Error);
                su.HasError = true;
            }

            base.ProcessLine(line, stream);
        }

        protected override string Commandline
        {
            get
            {
                StringBuilder sb = new StringBuilder();
                MuxSettings settings = job.Settings;

                sb.Append("-ovc copy -oac copy ");
                if (!string.IsNullOrEmpty(settings.VideoInput))
                {
                    sb.Append("\"" + settings.MuxedInput + "\" ");
                }
                if (settings.VideoInput.Length > 0)
                {
                    sb.Append("\"" + settings.VideoInput + "\" ");
                }
                if (!string.IsNullOrEmpty(settings.MuxedInput))
                {
                    MuxStream stream = (MuxStream)settings.AudioStreams[0];
                    sb.Append("-audiofile \"" + stream.path + "\" ");
                }
                sb.Append(" -mc 0 -noskip -o \"" + settings.VideoInput + "\"");
                return sb.ToString();
            }
        }
    }
}
