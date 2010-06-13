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
using System.Text;
using System.Windows.Forms; // used for the MethodInvoker

using MeGUI.core.util;

namespace MeGUI
{
    class x264Encoder : CommandlineVideoEncoder
    {
        public static readonly JobProcessorFactory Factory =
new JobProcessorFactory(new ProcessorFactory(init), "x264Encoder");

        private static IJobProcessor init(MainForm mf, Job j)
        {
            if (j is VideoJob &&
                (j as VideoJob).Settings is x264Settings)
                return new x264Encoder(mf.Settings.X264Path);
            return null;
        }

        public x264Encoder(string encoderPath)
            : base()
        {
            executable = encoderPath;

            if (OSInfo.isWow64())
            {
                string vfw4x264Path =  System.IO.Path.Combine(System.IO.Path.GetDirectoryName(encoderPath), "vfw4x264.exe");
                if (System.IO.File.Exists(vfw4x264Path))
                    executable = vfw4x264Path;
            }

        }

        public override string GetFrameString(string line, StreamType stream)
        {
            if (line.StartsWith("[")) // status update
            {
                int frameNumberStart = line.IndexOf("]", 4) + 2;
                int frameNumberEnd = line.IndexOf("/");
                return line.Substring(frameNumberStart, frameNumberEnd - frameNumberStart).Trim();
            }
            return null;
        }

        public override string GetErrorString(string line, StreamType stream)
        {
            if (line.IndexOf("Syntax") != -1 ||
                (line.IndexOf("error") != -1)
                || line.IndexOf("could not open") != -1)
            {
                if (line.IndexOf("converge") == -1 && line.IndexOf("try reducing") == -1 &&
                    (line.IndexOf("target:") == -1 || line.IndexOf("expected:") == -1))
                    return line;
            }
            return null;
        }

        public static string genCommandline(string input, string output, Dar? d, int hres, int vres, x264Settings xs, Zone[] zones)
        {
            int qp;
            bool display = false;
            StringBuilder sb = new StringBuilder();
            CultureInfo ci = new CultureInfo("en-us");

            
            ///<summary>
            /// x264 Main Tab Settings
            ///</summary>
            // AVC Profiles
            if (!xs.CustomEncoderOptions.Contains("--profile "))
            {
                switch (xs.Profile)
                {
                    case 0: sb.Append("--profile baseline "); break;
                    case 1: sb.Append("--profile main "); break;
                    case 2: break; // --profile high is the default value
                }
            }

            // AVC Levels
            if (!xs.CustomEncoderOptions.Contains("--level "))
                if (xs.Level != 15) // unrestricted
                    sb.Append("--level " + AVCLevels.getCLILevelNames()[xs.Level] + " ");

            // x264 Presets
            if (!xs.CustomEncoderOptions.Contains("--preset "))
            {
                switch (xs.x264PresetLevel)
                {
                    case x264Settings.x264PresetLevelModes.ultrafast: sb.Append("--preset ultrafast "); break;
                    case x264Settings.x264PresetLevelModes.superfast: sb.Append("--preset superfast "); break;
                    case x264Settings.x264PresetLevelModes.veryfast: sb.Append("--preset veryfast "); break;
                    case x264Settings.x264PresetLevelModes.faster: sb.Append("--preset faster "); break;
                    case x264Settings.x264PresetLevelModes.fast: sb.Append("--preset fast "); break;
                    //case x264Settings.x264PresetLevelModes.medium: sb.Append("--preset medium "); break; // default value
                    case x264Settings.x264PresetLevelModes.slow: sb.Append("--preset slow "); break;
                    case x264Settings.x264PresetLevelModes.slower: sb.Append("--preset slower "); break;
                    case x264Settings.x264PresetLevelModes.veryslow: sb.Append("--preset veryslow "); break;
                    case x264Settings.x264PresetLevelModes.placebo: sb.Append("--preset placebo "); break;
                }
            }

            // x264 Tunings
            if (!xs.CustomEncoderOptions.Contains("--tune"))
            {
                switch (xs.x264Tuning)
                {
                    case 1: sb.Append("--tune film "); break;
                    case 2: sb.Append("--tune animation "); break;
                    case 3: sb.Append("--tune grain "); break;
                    case 4: sb.Append("--tune psnr "); break;
                    case 5: sb.Append("--tune ssim "); break;
                    case 6: sb.Append("--tune fastdecode "); break;
                    case 7: sb.Append("--tune touhou "); break;
                    default: break; // default
                }
            }

            // Encoding Modes
            switch (xs.EncodingMode)
            {
                case 0: // ABR
                    if (!xs.CustomEncoderOptions.Contains("--bitrate")) sb.Append("--bitrate " + xs.BitrateQuantizer + " ");
                    break;
                case 1: // CQ
                    if (!xs.CustomEncoderOptions.Contains("--qp "))
                    {
                        qp = (int)xs.QuantizerCRF;
                        sb.Append("--qp " + qp.ToString(ci) + " ");
                    }
                    break;
                case 2: // 2 pass first pass
                    sb.Append("--pass 1 --bitrate " + xs.BitrateQuantizer + " --stats " + "\"" + xs.Logfile + "\" ");
                    break;
                case 3: // 2 pass second pass
                case 4: // automated twopass
                    sb.Append("--pass 2 --bitrate " + xs.BitrateQuantizer + " --stats " + "\"" + xs.Logfile + "\" ");
                    break;
                case 5: // 3 pass first pass
                    sb.Append("--pass 1 --bitrate " + xs.BitrateQuantizer + " --stats " + "\"" + xs.Logfile + "\" ");
                    break;
                case 6: // 3 pass 2nd pass
                    sb.Append("--pass 3 --bitrate " + xs.BitrateQuantizer + " --stats " + "\"" + xs.Logfile + "\" ");
                    break;
                case 7: // 3 pass 3rd pass
                case 8: // automated threepass, show third pass options
                    sb.Append("--pass 3 --bitrate " + xs.BitrateQuantizer + " --stats " + "\"" + xs.Logfile + "\" ");
                    break;
                case 9: // constant quality
                    if (!xs.CustomEncoderOptions.Contains("--crf"))
                        if (xs.QuantizerCRF != 23)
                            sb.Append("--crf " + xs.QuantizerCRF.ToString(ci) + " ");
                    break;
            } 

            // Slow 1st Pass
            if (!xs.CustomEncoderOptions.Contains("--slow-firstpass"))
                if ((xs.X264SlowFirstpass) && xs.x264PresetLevel < x264Settings.x264PresetLevelModes.placebo &&
                   ((xs.EncodingMode == 2) || // 2 pass first pass
                    (xs.EncodingMode == 4) || // automated twopass
                    (xs.EncodingMode == 5) || // 3 pass first pass
                    (xs.EncodingMode == 8)))  // automated threepass
                    sb.Append("--slow-firstpass ");

            // Threads
            if (!xs.CustomEncoderOptions.Contains("--thread-input"))
                if (xs.ThreadInput && xs.NbThreads == 1)
                    sb.Append("--thread-input ");
            if (!xs.CustomEncoderOptions.Contains("--threads"))
                if (xs.NbThreads > 0)
                    sb.Append("--threads " + xs.NbThreads + " ");
            

            
            ///<summary>
            /// x264 Frame-Type Tab Settings
            ///</summary>

            // H.264 Features
            if (xs.Deblock)
            {
                display = false;

                switch (xs.x264Tuning)
                {
                    case 1:
                    case 7: if (xs.AlphaDeblock != -1 || xs.BetaDeblock != -1) display = true; break;
                    case 2: if (xs.AlphaDeblock != 1 || xs.BetaDeblock != 1) display = true; break;
                    case 3: if (xs.AlphaDeblock != -2 || xs.BetaDeblock != -2) display = true; break;
                    default: if (xs.AlphaDeblock != 0 || xs.BetaDeblock != 0) display = true; break;
                }

                if (!xs.CustomEncoderOptions.Contains("--deblock "))
                    if (display)
                        sb.Append("--deblock " + xs.AlphaDeblock + ":" + xs.BetaDeblock + " ");
            }
            else
            {
                if (!xs.CustomEncoderOptions.Contains("--no-deblock"))
                    if (xs.x264PresetLevel != x264Settings.x264PresetLevelModes.ultrafast || (xs.x264Tuning != 0 && xs.x264Tuning != 6)) 
                        sb.Append("--no-deblock ");
            }

            if (!xs.CustomEncoderOptions.Contains("--no-cabac"))
            {
                if (!xs.Cabac)
                {
                    if (xs.Profile > 0 && (xs.x264PresetLevel != x264Settings.x264PresetLevelModes.ultrafast || (xs.x264Tuning != 0 && xs.x264Tuning != 6)))
                        sb.Append("--no-cabac ");
                }
            }

            // GOP Size
            if (!xs.CustomEncoderOptions.Contains("--keyint"))
                if (xs.KeyframeInterval != 250) // gop size of 250 is default
                    sb.Append("--keyint " + xs.KeyframeInterval + " ");
            if (!xs.CustomEncoderOptions.Contains("--min-keyint"))
                if (xs.MinGOPSize != 25)
                    sb.Append("--min-keyint " + xs.MinGOPSize + " ");

            // B-Frames
            if (xs.Profile > 0 && !xs.CustomEncoderOptions.Contains("--bframes"))  // baseline profile always uses 0 bframes
            {
                int iDefaultSettings = 3;
                switch (xs.x264PresetLevel)
                {
                    case x264Settings.x264PresetLevelModes.ultrafast:   iDefaultSettings = 0; break;
                    case x264Settings.x264PresetLevelModes.veryslow:    iDefaultSettings = 8; break;
                    case x264Settings.x264PresetLevelModes.placebo:     iDefaultSettings = 16; break;
                }
                if (xs.x264Tuning == 2) // animation
                    iDefaultSettings += 2;
                if (xs.NbBframes != iDefaultSettings)
                    sb.Append("--bframes " + xs.NbBframes + " ");
            }

            if (xs.NbBframes > 0)
            {
                if (!xs.CustomEncoderOptions.Contains("-b-adapt"))
                {
                    display = false;
                    if (xs.x264PresetLevel > x264Settings.x264PresetLevelModes.medium)
                    {
                        if (xs.NewAdaptiveBFrames != 2)
                            display = true;
                    }
                    else if (xs.x264PresetLevel > x264Settings.x264PresetLevelModes.ultrafast)
                    {
                        if (xs.NewAdaptiveBFrames != 1)
                            display = true;
                    }
                    else
                    {
                        if (xs.NewAdaptiveBFrames != 0)
                            display = true;
                    }
                    if (display)
                        sb.Append("--b-adapt " + xs.NewAdaptiveBFrames + " ");
                }

                if (xs.NbBframes > 1 && !xs.CustomEncoderOptions.Contains("--b-pyramid"))
                {
                    switch (xs.x264BFramePyramid) // pyramid needs a minimum of 2 b frames
                    {
                        case 1: sb.Append("--b-pyramid strict "); break;
                        case 0: sb.Append("--b-pyramid none "); break;
                    }
                }

                if (!xs.CustomEncoderOptions.Contains("--no-weightb"))
                    if (!xs.WeightedBPrediction && xs.x264Tuning != 6 && xs.x264PresetLevel != x264Settings.x264PresetLevelModes.ultrafast)
                        sb.Append("--no-weightb ");                    
            }

            // B-Frames bias
            if (!xs.CustomEncoderOptions.Contains("--b-bias "))
                if (xs.BframeBias != 0.0M)
                    sb.Append("--b-bias " + xs.BframeBias.ToString(ci) + " ");

            // Other
            if (!xs.CustomEncoderOptions.Contains("--interlaced"))
                if (xs.EncodeInterlaced)
                    sb.Append("--interlaced ");

            if (xs.Scenecut)
            {
                if (!xs.CustomEncoderOptions.Contains("--scenecut "))
                    if ((xs.SCDSensitivity != 40M && xs.x264PresetLevel != x264Settings.x264PresetLevelModes.ultrafast) ||
                        (xs.SCDSensitivity != 0M && xs.x264PresetLevel == x264Settings.x264PresetLevelModes.ultrafast))
                        sb.Append("--scenecut " + xs.SCDSensitivity.ToString(ci) + " ");
            }
            else
            {
                if (!xs.CustomEncoderOptions.Contains("--no-scenecut"))
                    if (xs.x264PresetLevel != x264Settings.x264PresetLevelModes.ultrafast)
                        sb.Append("--no-scenecut ");
            }

            // reference frames
            if (!xs.CustomEncoderOptions.Contains("--ref "))
            {
                int iDefaultSettings = 0;
                switch (xs.x264PresetLevel)
                {
                    case x264Settings.x264PresetLevelModes.ultrafast:
                    case x264Settings.x264PresetLevelModes.superfast:
                    case x264Settings.x264PresetLevelModes.veryfast:    iDefaultSettings = 1; break;
                    case x264Settings.x264PresetLevelModes.faster:
                    case x264Settings.x264PresetLevelModes.fast:        iDefaultSettings = 2; break;
                    case x264Settings.x264PresetLevelModes.medium:      iDefaultSettings = 3; break;
                    case x264Settings.x264PresetLevelModes.slow:        iDefaultSettings = 5; break;
                    case x264Settings.x264PresetLevelModes.slower:      iDefaultSettings = 8; break;
                    case x264Settings.x264PresetLevelModes.veryslow:
                    case x264Settings.x264PresetLevelModes.placebo:     iDefaultSettings = 16; break;
                }
                if ((xs.x264Tuning == 2 || xs.x264Tuning == 7) && iDefaultSettings > 1)
                    iDefaultSettings = iDefaultSettings * 2;

                if (iDefaultSettings != xs.NbRefFrames)
                    sb.Append("--ref " + xs.NbRefFrames + " ");
            }

            // WeightedPPrediction
            if (!xs.CustomEncoderOptions.Contains("--weightp "))
            {
                display = false;
                switch (xs.x264PresetLevel)
                {
                    case x264Settings.x264PresetLevelModes.ultrafast:
                    case x264Settings.x264PresetLevelModes.superfast:
                    case x264Settings.x264PresetLevelModes.veryfast: if (xs.WeightedPPrediction != 0) display = true; break;
                    case x264Settings.x264PresetLevelModes.faster: if (xs.WeightedPPrediction != 1) display = true; break;
                    default: if (xs.WeightedPPrediction != 2) display = true; break;
                }
                if (xs.x264Tuning == 6 && xs.WeightedPPrediction != 0)
                    display = true;
                if (xs.Profile == 0)
                    display = false;
                if (display)
                    sb.Append("--weightp " + xs.WeightedPPrediction + " ");
            }

            // Slicing
            if (!xs.CustomEncoderOptions.Contains("--slices "))
                if (xs.SlicesNb != 0)
                    sb.Append("--slices " + xs.SlicesNb + " ");
            if (!xs.CustomEncoderOptions.Contains("--slice-max-size "))
                if (xs.MaxSliceSyzeBytes != 0)
                    sb.Append("--slice-max-size " + xs.MaxSliceSyzeBytes + " ");
            if (!xs.CustomEncoderOptions.Contains("--slice-max-mbs "))
                if (xs.MaxSliceSyzeMBs != 0)
                    sb.Append("--slice-max-mbs " + xs.MaxSliceSyzeMBs + " ");
            

            
            ///<summary>
            /// x264 Rate Control Tab Settings
            /// </summary>


            if (!xs.CustomEncoderOptions.Contains("--qpmin "))
                if (xs.MinQuantizer != 10)
                    sb.Append("--qpmin " + xs.MinQuantizer + " ");
            if (!xs.CustomEncoderOptions.Contains("--qpmax "))
                if (xs.MaxQuantizer != 51)
                    sb.Append("--qpmax " + xs.MaxQuantizer + " ");
            if (!xs.CustomEncoderOptions.Contains("--qpstep "))
                if (xs.MaxQuantDelta != 4)
                    sb.Append("--qpstep " + xs.MaxQuantDelta + " ");

            if (xs.IPFactor != 1.4M)
            {
                display = true;
                if (xs.x264Tuning == 3 && xs.IPFactor == 1.1M)
                    display = false;

                if (!xs.CustomEncoderOptions.Contains("--ipratio "))
                    if (display)
                        sb.Append("--ipratio " + xs.IPFactor.ToString(ci) + " ");
            }

            if (xs.PBFactor != 1.3M) 
            {
                display = true;
                if (xs.x264Tuning == 3 && xs.PBFactor == 1.1M)
                    display = false;

                if (!xs.CustomEncoderOptions.Contains("--pbratio "))
                    if (display)
                        sb.Append("--pbratio " + xs.PBFactor.ToString(ci) + " ");
            }

            if (!xs.CustomEncoderOptions.Contains("--chroma-qp-offset "))
                if (xs.ChromaQPOffset != 0.0M)
                    sb.Append("--chroma-qp-offset " + xs.ChromaQPOffset.ToString(ci) + " ");

            if (xs.EncodingMode != 1) // doesn't apply to CQ mode
            {
                if (!xs.CustomEncoderOptions.Contains("--vbv-bufsize "))
                    if (xs.VBVBufferSize > 0)
                        sb.Append("--vbv-bufsize " + xs.VBVBufferSize + " ");
                if (!xs.CustomEncoderOptions.Contains("--vbv-maxrate "))
                    if (xs.VBVMaxBitrate > 0)
                        sb.Append("--vbv-maxrate " + xs.VBVMaxBitrate + " ");
                if (!xs.CustomEncoderOptions.Contains("--vbv-init "))
                    if (xs.VBVInitialBuffer != 0.9M)
                        sb.Append("--vbv-init " + xs.VBVInitialBuffer.ToString(ci) + " ");
                if (!xs.CustomEncoderOptions.Contains("--ratetol "))
                    if (xs.BitrateVariance != 1.0M)
                        sb.Append("--ratetol " + xs.BitrateVariance.ToString(ci) + " ");

                if (!xs.CustomEncoderOptions.Contains("--qcomp "))
                {
                    display = true;
                    if ((xs.x264Tuning == 3 && xs.QuantCompression == 0.8M) || (xs.x264Tuning != 3 && xs.QuantCompression == 0.6M))
                        display = false;
                    if (display)
                        sb.Append("--qcomp " + xs.QuantCompression.ToString(ci) + " ");
                }

                if (xs.EncodingMode > 1) // applies only to twopass
                {
                    if (!xs.CustomEncoderOptions.Contains("--cplxblur "))
                        if (xs.TempComplexityBlur != 20)
                            sb.Append("--cplxblur " + xs.TempComplexityBlur.ToString(ci) + " ");
                    if (!xs.CustomEncoderOptions.Contains("--qblur "))
                        if (xs.TempQuanBlurCC != 0.5M)
                            sb.Append("--qblur " + xs.TempQuanBlurCC.ToString(ci) + " ");
                }
            }

            // Dead Zones
            if (!xs.CustomEncoderOptions.Contains("--deadzone-inter "))
            {
                display = true;
                if ((xs.x264Tuning != 3 && xs.DeadZoneInter == 21) || (xs.x264Tuning == 3 && xs.DeadZoneInter == 6))
                    display = false;
                if (display)
                    sb.Append("--deadzone-inter " + xs.DeadZoneInter + " ");
            }

            if (!xs.CustomEncoderOptions.Contains("--deadzone-intra "))
            {
                display = true;
                if ((xs.x264Tuning != 3 && xs.DeadZoneIntra == 11) || (xs.x264Tuning == 3 && xs.DeadZoneIntra == 6))
                    display = false;
                if (display)
                    sb.Append("--deadzone-intra " + xs.DeadZoneIntra + " ");
            }

            // Disable Macroblok Tree
            if (!xs.NoMBTree)
            {
                if (!xs.CustomEncoderOptions.Contains("--no-mbtree"))
                    if (xs.x264PresetLevel > x264Settings.x264PresetLevelModes.veryfast)
                        sb.Append("--no-mbtree ");
            }
            else
            {
                // RC Lookahead
                if (!xs.CustomEncoderOptions.Contains("--rc-lookahead "))
                {
                    display = false;
                    switch (xs.x264PresetLevel)
                    {
                        case x264Settings.x264PresetLevelModes.faster: if (xs.Lookahead != 20) display = true; break;
                        case x264Settings.x264PresetLevelModes.fast: if (xs.Lookahead != 30) display = true; break;
                        case x264Settings.x264PresetLevelModes.medium: if (xs.Lookahead != 40) display = true; break;
                        case x264Settings.x264PresetLevelModes.slow: if (xs.Lookahead != 50) display = true; break;
                        case x264Settings.x264PresetLevelModes.slower:
                        case x264Settings.x264PresetLevelModes.veryslow:
                        case x264Settings.x264PresetLevelModes.placebo: if (xs.Lookahead != 60) display = true; break;
                    }
                    if (display)
                        sb.Append("--rc-lookahead " + xs.Lookahead + " ");
                }
            }

            // AQ-Mode
            if (xs.EncodingMode != (int)VideoCodecSettings.Mode.CQ)
            {
                if (xs.AQmode > 0)
                {
                    if (!xs.CustomEncoderOptions.Contains("--aq-mode "))
                    {
                        display = true;
                        if ((xs.x264Tuning != 5 && xs.AQmode == 1) || (xs.x264Tuning == 5 && xs.AQmode == 2))
                            display = false;
                        if (display)
                            sb.Append("--aq-mode " + xs.AQmode.ToString() + " ");
                    }

                    display = false;
                    switch (xs.x264Tuning)
                    {
                        case 2: if (xs.AQstrength != 0.6M) display = true; break;
                        case 3: if (xs.AQstrength != 0.5M) display = true; break;
                        case 7: if (xs.AQstrength != 1.3M) display = true; break;
                        default: if (xs.AQstrength != 1.0M) display = true; break;
                    }
                    if (!xs.CustomEncoderOptions.Contains("--aq-strength "))
                        if (display)
                            sb.Append("--aq-strength " + xs.AQstrength.ToString(ci) + " ");
                }
                else
                {
                    if (!xs.CustomEncoderOptions.Contains("--aq-mode "))
                        if (xs.x264PresetLevel != x264Settings.x264PresetLevelModes.ultrafast && xs.x264Tuning != 4)
                            sb.Append("--aq-mode 0 ");
                }
            }

            // custom matrices 
            if (xs.QuantizerMatrixType > 0)
            {
                switch (xs.QuantizerMatrixType)
                {
                    case 1: if (!xs.CustomEncoderOptions.Contains("--cqm ")) sb.Append("--cqm \"jvt\" "); break;
                    case 2: if (!xs.CustomEncoderOptions.Contains("--cqmfile")) sb.Append("--cqmfile \"" + xs.QuantizerMatrix + "\" "); break;
                }
            }

            

            
            ///<summary>
            /// x264 Analysis Tab Settings
            /// </summary>

            // Disable Chroma Motion Estimation
            if (!xs.CustomEncoderOptions.Contains("--no-chroma-me"))    
                if (!xs.ChromaME)
                    sb.Append("--no-chroma-me ");

            // Motion Estimation Range
            if (!xs.CustomEncoderOptions.Contains("--merange "))   
            {
                if ((xs.x264PresetLevel <= x264Settings.x264PresetLevelModes.slower && xs.MERange != 16) ||
                    (xs.x264PresetLevel >= x264Settings.x264PresetLevelModes.veryslow && xs.MERange != 24))
                    sb.Append("--merange " + xs.MERange + " ");
            }

            // ME Type
            if (!xs.CustomEncoderOptions.Contains("--me "))
            {
                display = false;
                switch (xs.x264PresetLevel)
                {
                    case x264Settings.x264PresetLevelModes.ultrafast:
                    case x264Settings.x264PresetLevelModes.superfast:   if (xs.METype != 0) display = true; break;
                    case x264Settings.x264PresetLevelModes.veryfast: 
                    case x264Settings.x264PresetLevelModes.faster:
                    case x264Settings.x264PresetLevelModes.fast:
                    case x264Settings.x264PresetLevelModes.medium:      if (xs.METype != 1) display = true; break;
                    case x264Settings.x264PresetLevelModes.slow: 
                    case x264Settings.x264PresetLevelModes.slower:
                    case x264Settings.x264PresetLevelModes.veryslow:    if (xs.METype != 2) display = true; break;
                    case x264Settings.x264PresetLevelModes.placebo:     if (xs.METype != 4) display = true; break;
                }

                if (display)
                {
                    switch (xs.METype)
                    {
                        case 0: sb.Append("--me dia "); break;
                        case 1: sb.Append("--me hex "); break;
                        case 2: sb.Append("--me umh "); break;
                        case 3: sb.Append("--me esa "); break;
                        case 4: sb.Append("--me tesa "); break;
                    }
                }

            }

            if (!xs.CustomEncoderOptions.Contains("--direct "))
            {
                display = false;
                if (xs.x264PresetLevel > x264Settings.x264PresetLevelModes.medium)
                {
                    if (xs.BframePredictionMode != 3)
                        display = true;
                }
                else if (xs.BframePredictionMode != 1)
                    display = true;
                
                if (display)
                {
                    switch (xs.BframePredictionMode)
                    {
                        case 0: sb.Append("--direct none "); break;
                        case 1: sb.Append("--direct spatial "); break;
                        case 2: sb.Append("--direct temporal "); break;
                        case 3: sb.Append("--direct auto "); break;
                    }
                }
            }

            if (!xs.CustomEncoderOptions.Contains("--nr "))
                if (xs.NoiseReduction > 0)
                    sb.Append("--nr " + xs.NoiseReduction + " ");
      

            // subpel refinement
            if (!xs.CustomEncoderOptions.Contains("--subme "))
            {
                display = false;
                switch (xs.x264PresetLevel)
                {
                    case x264Settings.x264PresetLevelModes.ultrafast:   if (xs.SubPelRefinement != 0) display = true; break;
                    case x264Settings.x264PresetLevelModes.superfast:   if (xs.SubPelRefinement != 1) display = true; break;
                    case x264Settings.x264PresetLevelModes.veryfast:    if (xs.SubPelRefinement != 2) display = true; break;
                    case x264Settings.x264PresetLevelModes.faster:      if (xs.SubPelRefinement != 4) display = true; break;
                    case x264Settings.x264PresetLevelModes.fast:        if (xs.SubPelRefinement != 6) display = true; break;
                    case x264Settings.x264PresetLevelModes.medium:      if (xs.SubPelRefinement != 7) display = true; break;
                    case x264Settings.x264PresetLevelModes.slow:        if (xs.SubPelRefinement != 8) display = true; break;
                    case x264Settings.x264PresetLevelModes.slower:      if (xs.SubPelRefinement != 9) display = true; break;
                    case x264Settings.x264PresetLevelModes.veryslow:    if (xs.SubPelRefinement != 10) display = true; break;
                    case x264Settings.x264PresetLevelModes.placebo:     if (xs.SubPelRefinement != 10) display = true; break;
                }
                if (display)
                    sb.Append("--subme " + (xs.SubPelRefinement) + " ");
            }

            // macroblock types
            if (!xs.CustomEncoderOptions.Contains("--partitions "))
            {
                bool bExpectedP8x8mv = true;
                bool bExpectedB8x8mv = true;
                bool bExpectedI4x4mv = true;
                bool bExpectedI8x8mv = true;
                bool bExpectedP4x4mv = true;

                switch (xs.x264PresetLevel) 
                {
                    case x264Settings.x264PresetLevelModes.ultrafast: bExpectedP8x8mv = false; bExpectedB8x8mv = false; bExpectedI4x4mv = false; 
                            bExpectedI8x8mv = false; bExpectedP4x4mv = false; break;
                    case x264Settings.x264PresetLevelModes.superfast: bExpectedP8x8mv = false; bExpectedB8x8mv = false; bExpectedP4x4mv = false; break;
                    case x264Settings.x264PresetLevelModes.veryfast:
                    case x264Settings.x264PresetLevelModes.faster:
                    case x264Settings.x264PresetLevelModes.fast:
                    case x264Settings.x264PresetLevelModes.medium:
                    case x264Settings.x264PresetLevelModes.slow: bExpectedP4x4mv = false; break;
                }
                if (xs.x264Tuning == 7 && bExpectedP8x8mv)
                    bExpectedP4x4mv = true;

                if (bExpectedP8x8mv != xs.P8x8mv || bExpectedB8x8mv != xs.B8x8mv 
                    || bExpectedI4x4mv != xs.I4x4mv || bExpectedI8x8mv != xs.I8x8mv 
                    || bExpectedP4x4mv != xs.P4x4mv)
                {
                    if (xs.P8x8mv || xs.B8x8mv || xs.I4x4mv || xs.I8x8mv || xs.P4x4mv)
                    {
                        sb.Append("--partitions ");
                        if (xs.I4x4mv && xs.I8x8mv && xs.P4x4mv && xs.P8x8mv && xs.B8x8mv)
                            sb.Append("all ");
                        else
                        {
                            if (xs.P8x8mv) // default is checked
                                sb.Append("p8x8,");
                            if (xs.B8x8mv) // default is checked
                                sb.Append("b8x8,");
                            if (xs.I4x4mv) // default is checked
                                sb.Append("i4x4,");
                            if (xs.P4x4mv) // default is unchecked
                                sb.Append("p4x4,");
                            if (xs.I8x8mv) // default is checked
                                sb.Append("i8x8");
                            if (sb.ToString().EndsWith(","))
                                sb.Remove(sb.Length - 1, 1);
                        }

                        if (!sb.ToString().EndsWith(" "))
                            sb.Append(" ");
                    }
                    else
                        sb.Append("--partitions none ");
                }
            }

            if (!xs.CustomEncoderOptions.Contains("--no-8x8dct"))
                if (!xs.AdaptiveDCT)
                    if (xs.Profile > 0 && xs.x264PresetLevel > x264Settings.x264PresetLevelModes.ultrafast)
                        sb.Append("--no-8x8dct ");

            // Trellis
            if (!xs.CustomEncoderOptions.Contains("--trellis ") && xs.Cabac)
            {
                display = false;
                switch (xs.x264PresetLevel)
                {
                    case x264Settings.x264PresetLevelModes.ultrafast: 
                    case x264Settings.x264PresetLevelModes.superfast:
                    case x264Settings.x264PresetLevelModes.veryfast:    if (xs.X264Trellis != 0) display = true; break;
                    case x264Settings.x264PresetLevelModes.faster: 
                    case x264Settings.x264PresetLevelModes.fast: 
                    case x264Settings.x264PresetLevelModes.medium:
                    case x264Settings.x264PresetLevelModes.slow:        if (xs.X264Trellis != 1) display = true; break;
                    case x264Settings.x264PresetLevelModes.slower: 
                    case x264Settings.x264PresetLevelModes.veryslow:
                    case x264Settings.x264PresetLevelModes.placebo:     if (xs.X264Trellis != 2) display = true; break;
                }
                if (display)
                    sb.Append("--trellis " + xs.X264Trellis + " ");
            }

            if (!xs.CustomEncoderOptions.Contains("--psy-rd "))
            {
                if (xs.SubPelRefinement > 5)
                {
                    display = false;
                    switch (xs.x264Tuning)
                    {
                        case 1: if ((xs.PsyRDO != 1.0M) && (xs.PsyTrellis != 0.15M)) display = true; break;
                        case 2: if ((xs.PsyRDO != 0.4M) && (xs.PsyTrellis != 0.0M)) display = true; break;
                        case 3: if ((xs.PsyRDO != 1.0M) && (xs.PsyTrellis != 0.25M)) display = true; break;
                        case 7: if ((xs.PsyRDO != 1.0M) && (xs.PsyTrellis != 0.2M)) display = true; break;
                        default: if ((xs.PsyRDO != 1.0M) || (xs.PsyTrellis != 0.0M)) display = true; break;
                    }

                    if (display)
                        sb.Append("--psy-rd " + xs.PsyRDO.ToString(ci) + ":" + xs.PsyTrellis.ToString(ci) + " ");
                }
            }
            else
            {
                display = false;
                switch (xs.x264Tuning)
                {
                    case 1: if (xs.PsyTrellis != 0.15M) display = true; break;
                    case 3: if (xs.PsyTrellis != 0.25M) display = true; break;
                    case 7: if (xs.PsyTrellis != 0.2M) display = true; break;
                    case 0:
                    case 4:
                        {
                            if (xs.PsyTrellis != 0.0M)
                                display = true;
                        } break;
                }
                if (!xs.CustomEncoderOptions.Contains("--psy-rd 0: "))
                    if (display)
                        sb.Append("--psy-rd 0:" + xs.PsyTrellis.ToString(ci) + " ");
            }

            if (!xs.CustomEncoderOptions.Contains("--no-mixed-refs"))
                if (xs.NoMixedRefs)
                    if (xs.x264PresetLevel >= x264Settings.x264PresetLevelModes.fast)
                        sb.Append("--no-mixed-refs ");

            if (!xs.CustomEncoderOptions.Contains("--no-dct-decimate"))
                if (xs.NoDCTDecimate)
                    if (xs.x264Tuning != 3)
                        sb.Append("--no-dct-decimate ");

            if (!xs.CustomEncoderOptions.Contains("--no-fast-pskip"))
                if (xs.NoFastPSkip)
                    if (xs.x264PresetLevel != x264Settings.x264PresetLevelModes.placebo)
                        sb.Append("--no-fast-pskip ");

            if (!xs.CustomEncoderOptions.Contains("--no-psy"))
                if (xs.NoPsy && (xs.x264Tuning != 4 && xs.x264Tuning != 5))
                    sb.Append("--no-psy ");

            if (!xs.CustomEncoderOptions.Contains("--aud"))
                if (xs.X264Aud)
                    sb.Append("--aud ");

            if (!xs.CustomEncoderOptions.Contains("--nal-hrd"))
                if (xs.X264Nalhrd)
                    sb.Append("--nal-hrd vbr ");

            

            
            ///<summary>
            /// x264 Misc Tab Settings
            /// </summary>

            // QPFile
            if (!xs.CustomEncoderOptions.Contains("-qpfile "))
                if (xs.UseQPFile)
                    if (xs.EncodingMode == 0 ||
                        xs.EncodingMode == 1 ||
                        xs.EncodingMode == 2 ||
                        xs.EncodingMode == 5 ||
                        xs.EncodingMode == 9)
                        sb.Append("--qpfile " + "\"" + xs.QPFile + "\" ");

            if (!xs.CustomEncoderOptions.Contains("--psnr"))
                if (xs.PSNRCalculation)
                    sb.Append("--psnr ");

            if (!xs.CustomEncoderOptions.Contains("--ssim"))
                if (xs.SSIMCalculation)
                    sb.Append("--ssim ");

            if (!xs.CustomEncoderOptions.Contains("--fullrange on"))    
                if (xs.fullRange)
                    sb.Append("--fullrange on ");

            if (!xs.CustomEncoderOptions.Equals("")) // add custom encoder options
                sb.Append(xs.CustomEncoderOptions + " ");

            

            
            if (zones != null && zones.Length > 0 && xs.CreditsQuantizer >= 1.0M)
            {
                sb.Append("--zones ");
                foreach (Zone zone in zones)
                {
                    sb.Append(zone.startFrame + "," + zone.endFrame + ",");
                    if (zone.mode == ZONEMODE.Quantizer)
                    {
                        sb.Append("q=");
                        sb.Append(zone.modifier + "/");
                    }
                    if (zone.mode == ZONEMODE.Weight)
                    {
                        sb.Append("b=");
                        double mod = (double)zone.modifier / 100.0;
                        sb.Append(mod.ToString(ci) + "/");
                    }
                }
                sb.Remove(sb.Length - 1, 1);
                sb.Append(" ");
            }
            

            
            if (!xs.CustomEncoderOptions.Contains("--sar "))
            {
                if (d.HasValue)
                {
                    Sar s = d.Value.ToSar(hres, vres);
                    sb.Append("--sar " + s.X + ":" + s.Y + " ");
                }
            }
            

            
            
            //add the rest of the commandline regarding the output
            if (xs.EncodingMode == 2 || xs.EncodingMode == 5)
                sb.Append("--output NUL ");
            else
                sb.Append("--output " + "\"" + output + "\" ");
            sb.Append("\"" + input + "\" ");
            

            return sb.ToString();
        }

        protected override string Commandline
        {
            get {
                return genCommandline(job.Input, job.Output, job.DAR, hres, vres, job.Settings as x264Settings, job.Zones);
            }
        }
    }
}
