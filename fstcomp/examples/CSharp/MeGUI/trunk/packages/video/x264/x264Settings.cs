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

namespace MeGUI
{
	/// <summary>
	/// Summary description for x264Settings.
	/// </summary>
	[Serializable]
	public class x264Settings: VideoCodecSettings
	{
        public static string ID = "x264";

        public enum x264PresetLevelModes : int 
        { 
            ultrafast = 0,
            superfast = 1,
            veryfast = 2,
            faster = 3,
            fast = 4,
            medium = 5,
            slow = 6,
            slower = 7,
            veryslow = 8,
            placebo = 9
        }

        public override void setAdjustedNbThreads(int nbThreads)
        {
            base.setAdjustedNbThreads(0);
        }

        public override void FixFileNames(System.Collections.Generic.Dictionary<string, string> substitutionTable)
        {
            base.FixFileNames(substitutionTable);
            if (QuantizerMatrixType == 2) // CQM
            {
                if (substitutionTable.ContainsKey(QuantizerMatrix))
                    QuantizerMatrix = substitutionTable[QuantizerMatrix];
            }
        }
        public override string[] RequiredFiles
        {
            get
            {
                List<string> list = new List<string>(base.RequiredFiles);
                if (QuantizerMatrixType == 2) // Custom profile
                    list.Add(QuantizerMatrix);
                return list.ToArray();
            }
        }
        int NewadaptiveBFrames, nbRefFrames, alphaDeblock, betaDeblock, subPelRefinement, maxQuantDelta, tempQuantBlur, 
			bframePredictionMode, vbvBufferSize, vbvMaxBitrate, meType, meRange, minGOPSize, macroBlockOptions,
            quantizerMatrixType, x264Trellis, noiseReduction, deadZoneInter, deadZoneIntra, AQMode, profile, level,
            lookahead, slicesnb, maxSliceSyzeBytes, maxSliceSyzeMBs, bFramePyramid, weightedPPrediction, tune;
		decimal ipFactor, pbFactor, chromaQPOffset, vbvInitialBuffer, bitrateVariance, quantCompression, 
			tempComplexityBlur, tempQuanBlurCC, scdSensitivity, bframeBias, quantizerCrf, AQStrength, psyRDO, psyTrellis;
		bool deblock, cabac, p4x4mv, p8x8mv, b8x8mv, i4x4mv, i8x8mv, weightedBPrediction, encodeInterlaced,
			chromaME, adaptiveDCT, noMixedRefs, noFastPSkip, psnrCalc, noDctDecimate, ssimCalc, useQPFile, 
            FullRange, advSet, noMBTree, threadInput, noPsy, scenecut, x264Nalhrd, x264Aud, x264SlowFirstpass;
		string quantizerMatrix, qpfile;
        x264PresetLevelModes preset;
		#region constructor
        /// <summary>
		/// default constructor, initializes codec default values
		/// </summary>
		public x264Settings():base(ID, VideoEncoderType.X264)
		{
            preset = x264PresetLevelModes.medium;
            tune = 0;
            deadZoneInter = 21;
            deadZoneIntra = 11;
            encodeInterlaced = false;
			noFastPSkip = false;
            ssimCalc = false;
            psnrCalc = false;
			EncodingMode = 9;
			BitrateQuantizer = 23;
			KeyframeInterval = 250;
			nbRefFrames = 3;
			noMixedRefs = false;
			NbBframes = 3;
			deblock = true;
			alphaDeblock = 0;
			betaDeblock = 0;
			cabac = true;
			weightedBPrediction = true;
            weightedPPrediction = 2;
			NewadaptiveBFrames = 1;
			bFramePyramid = 2;
			subPelRefinement = 7;
			psyRDO = new decimal(1.0);
            psyTrellis = new decimal(0.0);
            macroBlockOptions = 3;
            chromaME = true;
			p8x8mv = true;
			b8x8mv = true;
			p4x4mv = false;
			i4x4mv = true;
			i8x8mv = false;
			MinQuantizer = 10;
			MaxQuantizer = 51;
			maxQuantDelta = 4;
			CreditsQuantizer = new decimal(40);
			ipFactor = new decimal(1.4);
			pbFactor = new decimal(1.3);
			chromaQPOffset = new decimal(0.0);
			vbvBufferSize = 0;
			vbvMaxBitrate = 0;
			vbvInitialBuffer = new decimal(0.9);
			bitrateVariance = 1;
			quantCompression = new decimal(0.6);
			tempComplexityBlur = 20;
			tempQuanBlurCC = new decimal(0.5);
			bframePredictionMode = 1;
			scdSensitivity = new decimal(40);
			bframeBias = new decimal(0);
			meType = 1;
			meRange = 16;
			NbThreads = 0;
			minGOPSize = 25;
			adaptiveDCT = true;
			quantizerMatrix = "";
			quantizerMatrixType = 0; // none
			x264Trellis = 1;
            base.MaxNumberOfPasses = 3;
            AQMode = 1;
            AQStrength = new decimal(1.0);
            useQPFile = false;
            qpfile = "";
            FullRange = false;
            advSet = false;
            lookahead = 40;
            noMBTree = true;
            threadInput = true;
            noPsy = false;
            scenecut = true;
            slicesnb = 0;
            maxSliceSyzeBytes = 0;
            maxSliceSyzeMBs = 0;
            x264Nalhrd = false;
            x264Aud = false;
            profile = 3; // Autoguess. High if using default options.
            level = 15;
            x264SlowFirstpass = false;
		}
		#endregion
		#region properties

#warning Deprecated since 0.3.4.9, delete block after 0.3.6
        public int x264Preset
        {
            get { return 99; }
            set
            {
                if (value == 99)
                    return;
                // needs to be assigned to the new preset system (+superfast)
                if (value > 0)
                    value++;
                preset = (x264PresetLevelModes)value;
            }
        }
#warning Deprecated since 0.3.4.14, delete block after 0.3.6
        public string Turbo
        {
            get { return "migrated"; }
            set 
            {
                if (value.Equals("migrated"))
                    return;
                if (value.Equals("false"))
                    x264SlowFirstpass = true;
                if (value.Equals("true"))
                    x264SlowFirstpass = false;
            }
        }
        public x264PresetLevelModes x264PresetLevel
        {
            get { return preset; }
            set { preset = value; }
        }
        public int x264Tuning
        {
            get { return tune; }
            set { tune = value; }
        }
        public decimal QuantizerCRF
        {
            get { return quantizerCrf; }
            set { quantizerCrf = value; }
        }
        public bool EncodeInterlaced
        {
            get { return encodeInterlaced; }
            set { encodeInterlaced = value; }
        }
        public bool NoDCTDecimate
        {
            get { return noDctDecimate; }
            set { noDctDecimate = value; }
        }

        public bool PSNRCalculation
        {
            get { return psnrCalc; }
            set { psnrCalc = value; }
        }
		public bool NoFastPSkip
		{
			get { return noFastPSkip; }
			set { noFastPSkip = value; }
		}
		public int NoiseReduction
        {
            get { return noiseReduction; }
            set { noiseReduction = value; }
        }
        public bool NoMixedRefs
		{
			get { return noMixedRefs; }
			set { noMixedRefs = value; }
		}
		public int X264Trellis
		{
			get { return x264Trellis; }
			set { x264Trellis = value; }
		}
		public int NbRefFrames
		{
			get { return nbRefFrames; }
			set { nbRefFrames = value; }
		}
		public int AlphaDeblock
		{
			get { return alphaDeblock; }
			set { alphaDeblock = value; }
		}
		public int BetaDeblock
		{
			get { return betaDeblock; }
			set { betaDeblock = value; }
		}
		public int SubPelRefinement
		{
			get { return subPelRefinement; }
			set { subPelRefinement = value; }
		}
		public int MaxQuantDelta
		{
			get { return maxQuantDelta; }
			set { maxQuantDelta = value; }
		}
		public int TempQuantBlur
		{
			get { return tempQuantBlur; }
			set { tempQuantBlur = value; }
		}
		public int BframePredictionMode
		{
			get { return bframePredictionMode; }
			set { bframePredictionMode = value; }
		}
		public int VBVBufferSize
		{
			get { return vbvBufferSize; }
			set { vbvBufferSize = value; }
		}
		public int VBVMaxBitrate
		{
			get { return vbvMaxBitrate; }
			set { vbvMaxBitrate = value; }
		}
		public int METype
		{
			get { return meType; }
			set { meType = value; }
		}
		public int MERange
		{
			get { return meRange; }
			set { meRange = value; }
		}
		public int MinGOPSize
		{
			get { return minGOPSize; }
			set { minGOPSize = value; }
		}
		public decimal IPFactor
		{
			get { return ipFactor; }
			set { ipFactor = value; }
		}
		public decimal PBFactor
		{
			get { return pbFactor; }
			set { pbFactor = value; }
		}
		public decimal ChromaQPOffset
		{
			get { return chromaQPOffset; }
			set { chromaQPOffset = value; }
		}
		public decimal VBVInitialBuffer
		{
			get { return vbvInitialBuffer; }
			set { vbvInitialBuffer = value; }
		}
		public decimal BitrateVariance
		{
			get { return bitrateVariance; }
			set { bitrateVariance = value; }
		}
		public decimal QuantCompression
		{
			get { return quantCompression; }
			set { quantCompression = value; }
		}
		public decimal TempComplexityBlur
		{
			get { return tempComplexityBlur; }
			set { tempComplexityBlur = value; }
		}
		public decimal TempQuanBlurCC
		{
			get { return tempQuanBlurCC; }
			set { tempQuanBlurCC = value; }
		}
		public decimal SCDSensitivity
		{
			get { return scdSensitivity; }
			set { scdSensitivity = value; }
		}
		public decimal BframeBias
		{
			get { return bframeBias; }
			set { bframeBias = value; }
		}
        public decimal PsyRDO
        {
            get { return psyRDO; }
            set { psyRDO = value; }
        }
        public decimal PsyTrellis
        {
            get { return psyTrellis; }
            set { psyTrellis = value; }
        }
		public bool Deblock
		{
			get { return deblock; }
			set { deblock = value; }
		}
		public bool Cabac
		{
			get { return cabac; }
			set { cabac = value; }
		}
        public bool UseQPFile
        {
            get { return useQPFile; }
            set { useQPFile = value; }
        }
		public bool WeightedBPrediction
		{
			get { return weightedBPrediction; }
			set { weightedBPrediction = value; }
		}
        public int WeightedPPrediction
        {
            get { return weightedPPrediction; }
            set { weightedPPrediction = value; }
        }
		public int NewAdaptiveBFrames
		{
			get { return NewadaptiveBFrames; }
			set { NewadaptiveBFrames = value; }
		}
		public int x264BFramePyramid
		{
			get { return bFramePyramid; }
			set { bFramePyramid = value; }
		}
        public bool ChromaME
		{
			get { return chromaME; }
			set { chromaME = value; }
		}
        public int MacroBlockOptions
        {
            get { return macroBlockOptions; }
            set { macroBlockOptions = value; }
        }
        public bool P8x8mv
		{
			get { return p8x8mv; }
			set { p8x8mv = value; }
		}
		public bool B8x8mv
		{
			get { return b8x8mv; }
			set { b8x8mv = value; }
		}
		public bool I4x4mv
		{
			get { return i4x4mv; }
			set { i4x4mv = value; }
		}
		public bool I8x8mv
		{
			get { return i8x8mv; }
			set { i8x8mv = value; }
		}
		public bool P4x4mv
		{
			get { return p4x4mv; }
			set { p4x4mv = value; }
		}
		public bool AdaptiveDCT
		{
			get { return adaptiveDCT; }
			set { adaptiveDCT = value; }
		}
        public bool SSIMCalculation
        {
            get { return ssimCalc; }
            set { ssimCalc = value; }
        }
		public string QuantizerMatrix
		{
			get { return quantizerMatrix; }
			set { quantizerMatrix = value; }
		}
		public int QuantizerMatrixType
		{
			get { return quantizerMatrixType; }
			set { quantizerMatrixType = value; }
		}
        public int DeadZoneInter
        {
            get { return deadZoneInter; }
            set { deadZoneInter = value; }
        }
        public int DeadZoneIntra
        {
            get { return deadZoneIntra; }
            set { deadZoneIntra = value; }
        }
        public int AQmode
        {
            get { return AQMode; }
            set { AQMode = value; }
        }
        public decimal AQstrength
        {
            get { return AQStrength; }
            set { AQStrength = value; }
        }
        public string QPFile
        {
            get { return qpfile; }
            set { qpfile = value; }
        }
        public bool fullRange
        {
            get { return FullRange; }
            set { FullRange = value; }
        }
        public bool x264AdvancedSettings
        {
            get { return advSet; }
            set { advSet = value; }
        }
        public int Lookahead
        {
            get { return lookahead; }
            set { lookahead = value; }
        }
        public bool NoMBTree
        {
            get { return noMBTree; }
            set { noMBTree = value; }
        }
        public bool ThreadInput
        {
            get { return threadInput; }
            set { threadInput = value; }
        }
        public bool NoPsy
        {
            get { return noPsy; }
            set { noPsy = value; }
        }
        public bool Scenecut
        {
            get { return scenecut; }
            set { scenecut = value; }
        }
        public bool X264Nalhrd
        {
            get { return x264Nalhrd; }
            set { x264Nalhrd = value; }
        }
        public bool X264Aud
        {
            get { return x264Aud; }
            set { x264Aud = value; }
        }
        public bool X264SlowFirstpass
        {
            get { return x264SlowFirstpass; }
            set { x264SlowFirstpass = value; }
        }
        public int SlicesNb
        {
            get { return slicesnb; }
            set { slicesnb = value; }
        }
        public int MaxSliceSyzeBytes
        {
            get { return maxSliceSyzeBytes; }
            set { maxSliceSyzeBytes = value; }
        }
        public int MaxSliceSyzeMBs
        {
            get { return maxSliceSyzeMBs; }
            set { maxSliceSyzeMBs = value; }
        }
        public int Profile
        {
            get { return profile; }
            set { profile = value; }
        }
        public int Level
        {
            get { return level; }
            set { level = value; }
        }
        #endregion
        public override bool UsesSAR
        {
            get { return true; }
        }
        /// <summary>
        ///  Handles assessment of whether the encoding options vary between two x264Settings instances
        /// The following are excluded from the comparison:
        /// BitrateQuantizer
        /// CreditsQuantizer
        /// Logfile
        /// NbThreads
        /// SARX
        /// SARY
        /// Zones
        /// </summary>
        /// <param name="otherSettings"></param>
        /// <returns>true if the settings differ</returns>
        public bool IsAltered(x264Settings otherSettings)
        {
            if (
                this.NewAdaptiveBFrames != otherSettings.NewAdaptiveBFrames ||
                this.AdaptiveDCT != otherSettings.AdaptiveDCT ||
                this.AlphaDeblock != otherSettings.AlphaDeblock ||
                this.NoFastPSkip != otherSettings.NoFastPSkip ||
                this.B8x8mv != otherSettings.B8x8mv ||
                this.BetaDeblock != otherSettings.BetaDeblock ||
                this.BframeBias != otherSettings.BframeBias ||
                this.BframePredictionMode != otherSettings.BframePredictionMode ||
                this.x264BFramePyramid != otherSettings.x264BFramePyramid ||
                this.BitrateVariance != otherSettings.BitrateVariance ||
                this.PsyRDO != otherSettings.PsyRDO ||
                this.PsyTrellis != otherSettings.PsyTrellis ||
                this.Cabac != otherSettings.Cabac ||
                this.ChromaME != otherSettings.ChromaME ||
                this.ChromaQPOffset != otherSettings.ChromaQPOffset ||
                this.CustomEncoderOptions != otherSettings.CustomEncoderOptions ||
                this.Deblock != otherSettings.Deblock ||
                this.EncodingMode != otherSettings.EncodingMode ||
                this.I4x4mv != otherSettings.I4x4mv ||
                this.I8x8mv != otherSettings.I8x8mv ||
                this.IPFactor != otherSettings.IPFactor ||
                this.KeyframeInterval != otherSettings.KeyframeInterval ||
                this.Level != otherSettings.Level ||
                this.MaxQuantDelta != otherSettings.MaxQuantDelta ||
                this.MaxQuantizer != otherSettings.MaxQuantizer ||
                this.MERange != otherSettings.MERange ||
                this.METype != otherSettings.METype ||
                this.MinGOPSize != otherSettings.MinGOPSize ||
                this.MinQuantizer != otherSettings.MinQuantizer ||
                this.NoMixedRefs != otherSettings.NoMixedRefs ||
                this.NbBframes != otherSettings.NbBframes ||
                this.NbRefFrames != otherSettings.NbRefFrames ||
                this.noiseReduction != otherSettings.noiseReduction ||
                this.P4x4mv != otherSettings.P4x4mv ||
                this.P8x8mv != otherSettings.P8x8mv ||
                this.PBFactor != otherSettings.PBFactor ||
                this.Profile != otherSettings.Profile ||
                this.QPel != otherSettings.QPel ||
                this.QuantCompression != otherSettings.QuantCompression ||
                this.QuantizerMatrix != otherSettings.QuantizerMatrix ||
                this.QuantizerMatrixType != otherSettings.QuantizerMatrixType ||
                this.SCDSensitivity != otherSettings.SCDSensitivity ||
                this.SubPelRefinement != otherSettings.SubPelRefinement ||
                this.TempComplexityBlur != otherSettings.TempComplexityBlur ||
                this.TempQuanBlurCC != otherSettings.TempQuanBlurCC ||
                this.TempQuantBlur != otherSettings.TempQuantBlur ||
                this.Trellis != otherSettings.Trellis ||
                this.x264SlowFirstpass != otherSettings.x264SlowFirstpass ||
                this.V4MV != otherSettings.V4MV ||
                this.VBVBufferSize != otherSettings.VBVBufferSize ||
                this.VBVInitialBuffer != otherSettings.VBVInitialBuffer ||
                this.VBVMaxBitrate != otherSettings.VBVMaxBitrate ||
                this.WeightedBPrediction != otherSettings.WeightedBPrediction ||
                this.WeightedPPrediction != otherSettings.WeightedPPrediction ||
                this.X264Trellis != otherSettings.X264Trellis ||
                this.AQmode != otherSettings.AQmode ||
                this.AQstrength != otherSettings.AQstrength ||
                this.UseQPFile != otherSettings.UseQPFile ||
                this.fullRange != otherSettings.fullRange ||
                this.MacroBlockOptions != otherSettings.MacroBlockOptions ||
                this.x264PresetLevel != otherSettings.x264PresetLevel ||
                this.x264Tuning != otherSettings.x264Tuning ||
                this.x264AdvancedSettings != otherSettings.x264AdvancedSettings ||
                this.Lookahead != otherSettings.Lookahead ||
                this.NoMBTree != otherSettings.NoMBTree ||
                this.ThreadInput != otherSettings.ThreadInput ||
                this.NoPsy != otherSettings.NoPsy ||
                this.Scenecut != otherSettings.Scenecut ||
                this.SlicesNb != otherSettings.SlicesNb ||
                this.X264Nalhrd != otherSettings.X264Nalhrd ||
                this.X264Aud != otherSettings.X264Aud ||
                this.MaxSliceSyzeBytes != otherSettings.MaxSliceSyzeBytes ||
                this.MaxSliceSyzeMBs != otherSettings.MaxSliceSyzeMBs
                )
                return true;
            else
                return false;
        }

        public void doTriStateAdjustment()
        {
            switch (Profile)
            {
                case 0:
                    Cabac = false;
                    NbBframes = 0;
                    NewAdaptiveBFrames = 0;
                    x264BFramePyramid = 0;
                    I8x8mv = false;
                    AdaptiveDCT = false;
                    BframeBias = 0;
                    BframePredictionMode = 1; // default
                    QuantizerMatrixType = 0; // no matrix
                    QuantizerMatrix = "";
                    WeightedPPrediction = 0;
                    break;
                case 1:
                    x264BFramePyramid = 2;
                    I8x8mv = false;
                    AdaptiveDCT = false;
                    QuantizerMatrixType = 0; // no matrix
                    QuantizerMatrix = "";
                    WeightedPPrediction = 0;
                    break;
                case 2:
                    x264BFramePyramid = 2;
                    WeightedPPrediction = 1;
                    break;
            }
            if (EncodingMode != 2 && EncodingMode != 5)
                x264SlowFirstpass = false;
            if (NbBframes == 0)
            {
                NewAdaptiveBFrames = 0;
                WeightedBPrediction = false;
            }
            if (!Cabac) // trellis requires CABAC
                X264Trellis = 0;
            if (!P8x8mv) // p8x8 requires p4x4
                P4x4mv = false;
        }
	}
}
