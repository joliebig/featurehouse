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
using System.IO;
using System.Text;

using MeGUI.core.util;

namespace MeGUI
{
    public class VobSubIndexer : CommandlineJobProcessor<SubtitleIndexJob>
    {
        public static readonly JobProcessorFactory Factory =
       new JobProcessorFactory(new ProcessorFactory(init), "VobSubIndexer");

        private string configFile = null;

        private static IJobProcessor init(MainForm mf, Job j)
        {
            if (j is SubtitleIndexJob) return new VobSubIndexer();
            return null;
        }

        protected override string Commandline
        {
            get
            {
                return "vobsub.dll,Configure " + configFile;
            }
        }

        public VobSubIndexer()
        {
            executable = Environment.GetFolderPath(Environment.SpecialFolder.System) + @"\rundll32.exe";
        }
        #region IJobProcessor Members
        protected override void checkJobIO()
        {
            base.checkJobIO();
            generateScript();
            Util.ensureExists(configFile);
        }

        private void generateScript()
        {
            configFile = Path.ChangeExtension(job.Output, ".vobsub");

            using (StreamWriter sw = new StreamWriter(configFile, false, Encoding.Default))
            {
                sw.WriteLine(job.Input);
                sw.WriteLine(FileUtil.GetPathWithoutExtension(job.Output));
                sw.WriteLine(job.PGC);
                sw.WriteLine("0"); // we presume angle processing has been done before
                if (job.IndexAllTracks)
                    sw.WriteLine("ALL");
                else
                {
                    foreach (int id in job.TrackIDs)
                    {
                        sw.Write(id + " ");
                    }
                    sw.Write(sw.NewLine);
                }
                sw.WriteLine("CLOSE");
            }

            job.FilesToDelete.Add(configFile);
        }

        public override bool canPause
        {
            get { return false; }
        }

        #endregion

        protected override bool checkExitCode
        {
            get { return false; }
        }
    }
}
