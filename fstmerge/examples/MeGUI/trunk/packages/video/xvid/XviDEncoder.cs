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
using System.Windows.Forms; // used for the MethodInvoker

using MeGUI.core.plugins.implemented;
using MeGUI.core.util;

namespace MeGUI
{
    class XviDEncoder : CommandlineVideoEncoder
    {
        public static readonly JobProcessorFactory Factory =
new JobProcessorFactory(new ProcessorFactory(init), "XviDEncoder");

        private static IJobProcessor init(MainForm mf, Job j)
        {
            if (j is VideoJob &&
                (j as VideoJob).Settings is xvidSettings)
                return new XviDEncoder(mf.Settings.XviDEncrawPath);
            return null;
        }

        public XviDEncoder(string exePath)
            : base()
        {
            executable = exePath;
        }

        public override string GetFrameString(string line, StreamType stream)
        {
            if (line.IndexOf(": key") != -1) // we found a position line, parse it
            {
                int frameNumberEnd = line.IndexOf(":");
                return line.Substring(0, frameNumberEnd).Trim();
            }
            return null;
        }

        public override string GetErrorString(string line, StreamType stream)
        {
            if ((line.IndexOf("Usage") != -1)  || // we get the usage message if there's an unrecognized parameter
                (line.IndexOf("error") != -1)  ||
                (line.IndexOf("AVIStreamWrite") != -1))
               return line;
            
            return null;
        }

        protected override string Commandline
        {
            get
            {
                return genCommandline(job.Input, job.Output, job.DAR, job.Settings as xvidSettings, hres, vres, job.Zones);
            }
        }

        public static string genCommandline(string input, string output, Dar? d, xvidSettings xs, int hres, int vres, Zone[] zones)
        {
            StringBuilder sb = new StringBuilder();
            CultureInfo ci = new CultureInfo("en-us");
            sb.Append("-i \"" + input + "\" ");
            switch (xs.EncodingMode)
            {
                case 0: // CBR
                    sb.Append("-single -bitrate " + xs.BitrateQuantizer + " "); // add bitrate
                    break;
                case 1: // CQ
                    sb.Append("-single -cq " + xs.Quantizer.ToString(ci) + " "); // add quantizer
                    break;
                case 2: // 2 pass first pass
                    sb.Append("-pass1 " + "\"" + xs.Logfile + "\" -bitrate " + xs.BitrateQuantizer + " "); // add logfile
                    break;
                case 3: // 2 pass second pass
                case 4: // automated twopass
                    sb.Append("-pass2 " + "\"" + xs.Logfile + "\" -bitrate " + xs.BitrateQuantizer + " "); // add logfile
                    break;
            }
            if (xs.EncodingMode <= 1) // 1 pass modes
            {
                if (xs.ReactionDelayFactor != 16)
                    sb.Append("-reaction " + xs.ReactionDelayFactor + " ");
                if (xs.AveragingPeriod != 100)
                    sb.Append("-averaging " + xs.AveragingPeriod + " ");
                if (xs.RateControlBuffer != 100)
                    sb.Append("-smoother " + xs.RateControlBuffer + " ");
            }
            else // two pass modes
            {
                if (xs.KeyFrameBoost != 10)
                    sb.Append("-kboost " + xs.KeyFrameBoost + " ");
                if (xs.KeyframeThreshold != 1)
                    sb.Append("-kthresh " + xs.KeyframeThreshold + " ");
                if (xs.KeyframeReduction != 20)
                    sb.Append("-kreduction " + xs.KeyframeReduction + " ");
                if (xs.OverflowControlStrength != 5)
                    sb.Append("-ostrength " + xs.OverflowControlStrength + " ");
                if (xs.MaxOverflowImprovement != 5)
                    sb.Append("-oimprove " + xs.MaxOverflowImprovement + " ");
                if (xs.MaxOverflowDegradation != 5)
                    sb.Append("-odegrade " + xs.MaxOverflowDegradation + " ");
                if (xs.HighBitrateDegradation != 0)
                    sb.Append("-chigh " + xs.HighBitrateDegradation + " ");
                if (xs.LowBitrateImprovement != 0)
                    sb.Append("-clow " + xs.LowBitrateImprovement + " ");
                sb.Append("-overhead 0 ");
                if (xs.XvidProfile != 0)
                {
                    switch (xs.XvidProfile)
                    {
                        case 0:
                            break;
                        case 1:
                            sb.Append("-vbvmax 4854000 -vbvsize 3145728 -vbvpeak 2359296 ");
                            break;
                        case 2:
                            sb.Append("-vbvmax 9708400 -vbvsize 6291456 -vbvpeak 4718592 ");
                            break;
                        case 3:
                            sb.Append("-vbvmax 20000000 -vbvsize 16000000 -vbvpeak 12000000 ");
                            break;
                        case 4:
                            sb.Append("-vbvmax 200000 -vbvsize 262144 -vbvpeak 196608 ");
                            break;
                        case 5:
                            sb.Append("-vbvmax 600000 -vbvsize 655360 -vbvpeak 491520 ");
                            break;
                        case 6:
                            if (xs.VbvBuffer != 0)
                                sb.Append("-vbvsize " + xs.VbvBuffer + " ");
                            if (xs.VbvMaxRate != 0)
                                sb.Append("-vbvmax " + xs.VbvMaxRate + " ");
                            if (xs.VbvPeakRate != 0)
                                sb.Append("-vbvpeak " + xs.VbvPeakRate + " ");
                            break;
                    }
                }
            }
            if (xs.Turbo)
                sb.Append("-turbo ");
            if (xs.KeyframeInterval != 300)
                sb.Append("-max_key_interval " + xs.KeyframeInterval + " ");
            if (!xs.PackedBitstream) // default is on in encraw
                sb.Append("-nopacked ");
            if (xs.MotionSearchPrecision != 6)
                sb.Append("-quality " + xs.MotionSearchPrecision + " ");
            if (xs.VHQMode != 1)
                sb.Append("-vhqmode " + xs.VHQMode + " ");
            if (xs.QPel)
                sb.Append("-qpel ");
            if (xs.GMC)
                sb.Append("-gmc ");
            if (xs.QuantizerMatrix == xvidSettings.MPEGMatrix)
                sb.Append("-qtype 1 ");
            else if (xs.QuantizerMatrix != xvidSettings.H263Matrix && !string.IsNullOrEmpty(xs.QuantizerMatrix))
                sb.Append("-qmatrix \"" + xs.QuantizerMatrix + "\" ");
            if (xs.Interlaced)
            {
                sb.Append("-interlaced ");
                if (xs.BottomFieldFirst)
                    sb.Append("1 ");
                else
                    sb.Append("2 ");
            }
            if (xs.HVSMasking != 0)
                sb.Append("-masking " + xs.HVSMasking + " ");
            if (!xs.Trellis)
                sb.Append("-notrellis ");
            if (!xs.ChromaMotion)
                sb.Append("-nochromame ");
            if (xs.MinQuantizer != 2)
                sb.Append("-imin " + xs.MinQuantizer + " ");
            if (xs.MaxQuantizer != 31)
                sb.Append("-imax " + xs.MaxQuantizer + " ");
            if (xs.MinPQuant != 2)
                sb.Append("-pmin " + xs.MinPQuant + " ");
            if (xs.MaxPQuant != 31)
                sb.Append("-pmax " + xs.MaxPQuant + " ");
            if (!xs.ClosedGOP)
                sb.Append("-noclosed_gop ");
            if (xs.FrameDropRatio != 0)
                sb.Append("-drop " + xs.FrameDropRatio + " ");

            if (xs.NbBframes != 2) sb.Append("-max_bframes " + xs.NbBframes + " ");
            if (xs.NbBframes > 0)
            {
                if (xs.VHQForBframes)
                    sb.Append("-bvhq ");
                if (xs.BQuantRatio != 150)
                    sb.Append("-bquant_ratio " + xs.BQuantRatio + " ");
                if (xs.BQuantOffset != 100)
                    sb.Append("-bquant_offset " + xs.BQuantOffset + " ");
                if (xs.MinBQuant != 2)
                    sb.Append("-bmin " + xs.MinBQuant + " ");
                if (xs.MaxBQuant != 31)
                    sb.Append("-bmax " + xs.MaxBQuant + " ");
            }
            if (d.HasValue) // custom PAR mode
            {
                Sar s = d.Value.ToSar(hres, vres);
                sb.Append("-par " + s.X + ":" + s.Y + " ");
            }
            sb.Append("-threads " + xs.NbThreads + " ");
            if (zones != null && zones.Length > 0 && xs.CreditsQuantizer >= new decimal(1)
                && xs.EncodingMode != 1) // only for non CQ mode at the moment
            {
                foreach (Zone zone in zones)
                {
                    if (zone.mode == ZONEMODE.Quantizer)
                        sb.Append("-zq " + zone.startFrame + " " + zone.modifier + " ");
                    if (zone.mode == ZONEMODE.Weight)
                    {
                        sb.Append("-zw " + zone.startFrame + " ");
                        double mod = (double)zone.modifier / 100.0;
                        sb.Append(mod.ToString(ci) + " ");
                    }
                }
            }
            if (xs.EncodingMode != 2) // not 2 pass vbr first pass, add output filename and output type
            {
                string extension = Path.GetExtension(output).ToLower();
                if (extension.Equals(".mkv"))
                    sb.Append(" -mkv \"" + output + "\"");
                else if (extension.Equals(".avi"))
                    sb.Append(" -avi \"" + output + "\"");
                else
                    sb.Append(" -o \"" + output + "\"");
            }
            if (!xs.CustomEncoderOptions.Equals("")) // add custom encoder options
                sb.Append(" " + xs.CustomEncoderOptions);
            return sb.ToString();
        }
    }
}
