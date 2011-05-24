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
using System.IO;
using System.Text;
using System.Windows.Forms;

using MeGUI.core.details;
using MeGUI.core.util;

namespace MeGUI
{
    class AMGMuxer : CommandlineMuxer
    {
        public static readonly JobProcessorFactory Factory =
new JobProcessorFactory(new ProcessorFactory(init), "AMGMuxer");

        private static IJobProcessor init(MainForm mf, Job j)
        {
            if (j is MuxJob && (j as MuxJob).MuxType == MuxerType.AVIMUXGUI)
                return new AMGMuxer(mf.Settings.AviMuxGUIPath);
            return null;
        }

        string script_filename;

        public AMGMuxer(string executablePath)
        {
            this.executable = executablePath;
        }
        
        protected override void checkJobIO()
        {
            script_filename = writeScript(job);
            
            base.checkJobIO();
        }

        protected override string Commandline
        {
            get
            {
                return "\"" + script_filename + "\"";
            }
        }

        private string writeScript(MuxJob job)
        {
            MuxSettings settings = job.Settings;

            // First, generate the script

            StringBuilder script = new StringBuilder();
            script.AppendLine("CLEAR");

            int fileNum = 1; // the number of the file at the top
            
            int audioNum = 1; // the audio track number
            // add the audio streams
            foreach (MuxStream s in settings.AudioStreams)
            {
                script.AppendFormat("LOAD {1}{0}", Environment.NewLine, s.path);
                script.AppendLine("SET OUTPUT OPTIONS");
                if (!string.IsNullOrEmpty(s.name))
                    script.AppendFormat("SET OPTION AUDIO NAME {0} {1}{2}", audioNum, s.name, Environment.NewLine);
                if (!string.IsNullOrEmpty(s.language))
                    script.AppendFormat("SET OPTION AUDIO LNGCODE {0} {1}{2}", audioNum, s.language, Environment.NewLine);
                if (s.delay != 0)
                    script.AppendFormat("SET OPTION DELAY {0} {1}{2}", audioNum, s.delay, Environment.NewLine);

                audioNum++;
                fileNum++;
            }

            int subtitleNum = 1; // the subtitle track number
            // add the subtitle streams
            foreach (MuxStream s in settings.SubtitleStreams)
            {
                script.AppendFormat("LOAD {1}{0}", Environment.NewLine, s.path);
                script.AppendLine("SET OUTPUT OPTIONS");
                if (!string.IsNullOrEmpty(s.name))
                    script.AppendFormat("SET OPTION SUBTITLE NAME {0} {1}{2}", subtitleNum, s.name, Environment.NewLine);

                subtitleNum++;
                fileNum++;
            }

            // add the video stream if it exists
            if (!string.IsNullOrEmpty(settings.VideoInput))
            {
                script.AppendFormat("LOAD {1}{0}SELECT FILE {2}{0}ADD VIDEOSOURCE{0}DESELECT FILE {2}{0}", 
                    Environment.NewLine, settings.VideoInput, fileNum);
                fileNum++;
            }

            // mux in the rest if it exists
            if (!string.IsNullOrEmpty(settings.MuxedInput))
            {
                script.AppendFormat("LOAD {1}{0}SELECT FILE {2}{0}ADD VIDEOSOURCE{0}DESELECT FILE {2}{0}",
                    Environment.NewLine, settings.MuxedInput, fileNum);
                fileNum++;
            }
            
            // AR can't be signalled in AVI

            // split size
            script.AppendLine("SET OUTPUT OPTIONS");
            if (settings.SplitSize.HasValue)
            {
                script.AppendLine("SET OPTION NUMBERING ON");
                script.AppendLine("SET OPTION MAXFILESIZE ON");
                script.AppendFormat("SET OPTION MAXFILESIZE {0}{1}", ((MeGUI.core.util.FileSize)settings.SplitSize).MB, Environment.NewLine);
            }
            else
            {
                script.AppendLine("SET OPTION NUMBERING OFF");
                script.AppendLine("SET OPTION MAXFILESIZE OFF");
            }

            // Now do the rest of the setup
            script.AppendLine(
@"SET INPUT OPTIONS
SET OPTION MP3 VERIFY CBR ALWAYS
SET OPTION MP3 VERIFY RESDLG OFF
SET OPTION AVI FIXDX50 1
SET OPTION CHAPTERS IMPORT 1
SET OUTPUT OPTIONS
SET OPTION ALL SUBTITLES 1
SET OPTION ALL AUDIO 1
SET OPTION CLOSEAPP 1
SET OPTION DONEDLG 0
SET OPTION OVERWRITEDLG 0
SET OPTION STDIDX AUTO");

            script.AppendFormat("START {0}{1}", settings.MuxedOutput, Environment.NewLine);            

            /// the script is now created; let's write it to a temp file
            string filename = Path.GetTempFileName();
            using (StreamWriter output = new StreamWriter(File.OpenWrite(filename), System.Text.Encoding.UTF8))
            {
                output.Write(script.ToString());
            }
            return filename;
        }

        protected override void doExitConfig()
        {
            try
            {
                File.Delete(script_filename);
            }
            catch (IOException) { }
            base.doExitConfig();
        }

        

        protected override bool checkExitCode
        {
            get { return false; }
        }
    }
}
