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

using MeGUI.core.details;
using MeGUI.core.util;

namespace MeGUI
{
    public delegate void MuxerOutputCallback(string line, int type);

    abstract class CommandlineMuxer : CommandlineJobProcessor<MuxJob>
    {
        protected virtual void setProjectedFileSize()
        {
            su.ProjectedFileSize = FileSize.Empty;
            su.ProjectedFileSize += (FileSize.Of2(job.Settings.VideoInput) ?? FileSize.Empty);
            su.ProjectedFileSize += (FileSize.Of2(job.Settings.MuxedInput) ?? FileSize.Empty);

            foreach (MuxStream s in job.Settings.AudioStreams)
                su.ProjectedFileSize += FileSize.Of2(s.path) ?? FileSize.Empty;

            foreach (MuxStream s in job.Settings.SubtitleStreams)
                su.ProjectedFileSize += FileSize.Of2(s.path) ?? FileSize.Empty;
        }

        protected override void checkJobIO()
        {
            ensureInputFilesExistIfNeeded(job.Settings);
            setProjectedFileSize();
        }

        private void ensureInputFilesExistIfNeeded(MuxSettings settings)
        {
            Util.ensureExistsIfNeeded(settings.MuxedInput);
            Util.ensureExistsIfNeeded(settings.VideoInput);
            Util.ensureExistsIfNeeded(settings.ChapterFile);
            foreach (MuxStream s in settings.AudioStreams)
                Util.ensureExistsIfNeeded(s.path);
            foreach (MuxStream s in settings.SubtitleStreams)
                Util.ensureExistsIfNeeded(s.path);
        }
    }
}
