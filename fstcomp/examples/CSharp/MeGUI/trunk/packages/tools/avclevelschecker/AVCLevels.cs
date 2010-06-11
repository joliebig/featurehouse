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
using System.Windows.Forms;

namespace MeGUI
{
    public class AVCLevelTool : MeGUI.core.plugins.interfaces.ITool
    {

        #region ITool Members

        public string Name
        {
            get { return "AVC Levels Checker"; }
        }

        public void Run(MainForm info)
        {
            if (info.Video.VideoInput.Equals(""))
                MessageBox.Show("You first need to load an AviSynth script", "No video configured",
                    MessageBoxButtons.OK, MessageBoxIcon.Warning);
            else
            {
                int compliantLevel = 15;
                x264Settings currentX264Settings = (x264Settings)MainForm.Instance.Profiles.GetCurrentSettings("x264");
                bool succ = info.JobUtil.validateAVCLevel(info.Video.VideoInput, currentX264Settings, out compliantLevel);
                if (succ)
                    MessageBox.Show("This file matches the criteria for the level chosen", "Video validated",
                        MessageBoxButtons.OK, MessageBoxIcon.Information);
                else
                {
                    if (compliantLevel == -1)
                        MessageBox.Show("Unable to open video", "Test failed", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    else
                    {
                        AVCLevels al = new AVCLevels();
                        string[] levels = al.getLevels();
                        string levelRequired = levels[compliantLevel];
                        string message = "This video source cannot be encoded to comply with the chosen level.\n"
                            + "You need at least " + levelRequired + " for this source. Do you want\n"
                            + "to increase the level automatically now?";
                        DialogResult dr = MessageBox.Show(message, "Test failed", MessageBoxButtons.YesNo,
                            MessageBoxIcon.Question);
                        if (dr == DialogResult.Yes)
                            currentX264Settings.Level = compliantLevel;
                    }
                }
            }
        }

        public Shortcut[] Shortcuts
        {
            get { return new Shortcut[] { Shortcut.CtrlL }; }
        }

        #endregion

        #region IIDable Members

        public string ID
        {
            get { return "avc_level_validater"; }
        }

        #endregion
    }
    /// <summary>
	/// Summary description for AVCLevels.
    /// akupenguin http://forum.doom9.org/showthread.php?p=730001#post730001
    /// These are the properties listed in the levels tables in the standard, and how they should limit x264 settings:
    /// MaxMBPS >= width*height*fps. (w&h measured in macroblocks, i.e. pixels/16 round up in each dimension)
    /// MaxFS >= width*height
    /// sqrt(MaxFS*8) >= width
    /// sqrt(MaxFS*8) >= height
    /// MaxDPB >= (bytes in a frame) * min(16, ref)
    /// MaxBR >= vbv_maxrate. It isn't strictly required since we don't write the VCL HRD parameters, but this satisfies the intent.
    /// MaxCPB >= vbv_bufsize. Likewise.
    /// MaxVmvR >= max_mv_range. (Not exposed in the cli, I'll add it if people care.)
    /// MaxMvsPer2Mb, MinLumaBiPredSize, direct_8x8_inference_flag : are not enforced by x264. The only way to ensure compliance is to disable p4x4 at level>=3.1, or at level>=3 w/ B-frames.
    /// MinCR : is not enforced by x264. Won't ever be an issue unless you use lossless.
    /// SliceRate : I don't know what this limits.
	/// </summary>
	public class AVCLevels
    {
        #region internal logic and calculation routines for level verification
        /// <summary>
        /// Check functions to verify elements of the level
        /// </summary>
        /// <param name="level"></param>
        /// <param name="settings"></param>
        /// <returns>true if the settings are compliant with the level</returns>
        private bool checkP4x4Enabled(int level, x264Settings settings)
        {
            //if (level != 15 && (level > 7 || (level == 7 && settings.NbBframes != 0)))
            //    return false;
            //else
                return true;
        }
        private bool checkP4x4(int level, x264Settings settings)
        {
            if (!checkP4x4Enabled(level, settings))
                if (settings.P4x4mv)
                    return false;
            return true;
        }
        private double pictureBufferSize(x264Settings settings, double bytesInUncompressedFrame)
        {
            double decodedPictureBufferSizeTestValue = 0;
            if (settings != null)
                decodedPictureBufferSizeTestValue = bytesInUncompressedFrame * Math.Min(16, settings.NbRefFrames);
            return decodedPictureBufferSizeTestValue;
        }
        private bool checkMaxDPB(int level, x264Settings settings, double bytesInUncompressedFrame)
        {
            if (pictureBufferSize(settings, bytesInUncompressedFrame) > this.getMaxDPB(level))
                return false;
            else
                return true;
        }
        private int macroblocks(int res)
        {
            int blocks;
            if (res % 16 == 0)
                blocks = res / 16;
            else
            {
                int remainder;
                blocks = Math.DivRem(res, 16, out remainder);
                blocks++;
            }
            return blocks;
        }
        private double maxFS(int hRes, int vRes)
        {
            int horizontalBlocks, verticalBlocks;
            if (hRes % 16 == 0)
                horizontalBlocks = hRes / 16;
            else
            {
                int remainder;
                horizontalBlocks = Math.DivRem(hRes, 16, out remainder);
                horizontalBlocks++;
            }
            if (vRes % 16 == 0)
                verticalBlocks = vRes / 16;
            else
            {
                int remainder;
                verticalBlocks = Math.DivRem(vRes, 16, out remainder);
                verticalBlocks++;
            }
            return (double)horizontalBlocks * (double)verticalBlocks;
        }
        private int maxBPS(int hres, int vres, double framerate)
        {
            return (int)(maxFS(hres, vres) * framerate);
        }
        #endregion
        #region public calculation utilities
        public double bytesPerFrame(int hres, int vres)
        {
            return hres * vres * 1.5;
        }
        #endregion
        #region constructor
        public AVCLevels()
		{
			//
			// TODO: Add constructor logic here
			//
        }
        #endregion
        #region public look-up routines
        public string[] getLevels()
		{
			return new string[] {"Level 1", "Level 1.1", "Level 1.2", "Level 1.3", "Level 2", "Level 2.1",
									"Level 2.2", "Level 3", "Level 3.1", "Level 3.2", "Level 4", "Level 4.1",
									"Level 4.2", "Level 5", "Level 5.1", "Unrestricted/Autoguess"};
		}

        public static string[] getCLILevelNames()
        {
            return CLILevelNames;
        }

        public static readonly string[] CLILevelNames = new string[] { "1", "1.1", "1.2", "1.3",  "2", "2.1", "2.2",   "3", "3.1", "3.2",   "4",  "4.1",  "4.2",    "5", "5.1" };
        public static readonly int[] MainProfileMaxBRs = new int[]   {  64,   192,   384,   768, 2000,  4000,  4000, 10000, 14000, 20000, 20000,  50000,  50000, 135000, 240000 };
        // all bitrates and cbps are multiplied by 1.25 in high profile
        public static readonly int[] HighProfileMaxBRs = new int[]   { 80,   240,  480,  960, 2500, 5000, 5000, 12500, 17500, 25000, 25000, 62500, 62500, 168750, 300000 };
		/// <summary>
		/// gets the MaxBR value corresponding to a given AVC Level
		/// </summary>
		/// <param name="level">the level</param>
		/// <returns>the MaxBR in kbit/s</returns>
		public int getMaxBR(int level, bool isHighProfile)
		{
            if (level >= HighProfileMaxBRs.Length) return HighProfileMaxBRs[HighProfileMaxBRs.Length - 1];
            if (isHighProfile)
                return HighProfileMaxBRs[level];
            else
                return MainProfileMaxBRs[level];
        }
		/// <summary>
		/// gets the max cbp rate in bytes for a given level
		/// </summary>
		/// <param name="level">the level</param>
		/// <returns>the MaxCBP in bits</returns>
		public int getMaxCBP(int level, bool isHighProfile)
		{
            if (isHighProfile) // all bitrates and cbps are multiplied by 1.25 in high profile
            {
                switch (level)
			    {
				    case 0: // level 1
					    return 210;
					    
				    case 1: // level 1.1
					    return 625;
					    
				    case 2: // level 1.2
					    return 1250;
					    
				    case 3: // level 1.3
					    return 2500;
					    
				    case 4: // level 2
					    return 2500;
					    
				    case 5: // level 2.1
					    return 5000;
					    
				    case 6: // level 2.2
					    return 5000;
					    
				    case 7: // level 3
					    return 12500;
					    
				    case 8: // level 3.1
					    return 17500;
					    
				    case 9: // level 3.2
					    return 25000;
					    
				    case 10: // level 4
					    return 31250;
					    
				    case 11: // level 4.1
					    return 78125;
					    
				    case 12: // level 4.2
					    return 78125;
					    
				    case 13: // level 5
					    return 168750;
					    
				    case 14: // level 5.1
					    return 300000;
                }

            }
            else
            {
                switch (level)
			    {
				    case 0: // level 1
					    return 175;
					    
				    case 1: // level 1.1
					    return 500;
					    
				    case 2: // level 1.2
					    return 1000;
					    
				    case 3: // level 1.3
					    return 2000;
					    
				    case 4: // level 2
					    return 2000;
					    
				    case 5: // level 2.1
					    return 4000;
					    
				    case 6: // level 2.2
					    return 4000;
					    
				    case 7: // level 3
					    return 10000;
					    
				    case 8: // level 3.1
					    return 14000;
					    
				    case 9: // level 3.2
					    return 20000;
					    
				    case 10: // level 4
					    return 25000;
					    
				    case 11: // level 4.1
					    return 62500;
					    
				    case 12: // level 4.2
					    return 62500;
					    
				    case 13: // level 5
					    return 135000;
					    
				    case 14: // level 5.1
					    return 240000;

			    }
            }
            if (isHighProfile)
                return 300000;
            else
                return 240000;
		}
		/// <summary>
		/// gets the Maximum macroblock rate given a level
		/// </summary>
		/// <param name="level">the level</param>
		/// <returns>the macroblock rate</returns>
		public int getMaxMBPS(int level)
		{
			switch (level)
			{
				case 0: // level 1
					return 1485;
					
				case 1: // level 1.1
					return 3000;
					
				case 2: // level 1.2
					return 6000;
					
				case 3: // level 1.3
					return 11880;
					
				case 4: // level 2
					return 11880;
					
				case 5: // level 2.1
					return 19800;
					
				case 6: // level 2.2
					return 20250;
					
				case 7: // level 3
					return 40500;
					
				case 8: // level 3.1
					return 108000;
					
				case 9: // level 3.2
					return 216000;
					
				case 10: // level 4
					return 245760;
					
				case 11: // level 4.1
					return 245760;
					
				case 12: // level 4.2
					return 491520;
					
				case 13: // level 5
					return 589824;
					
				case 14: // level 5.1
					return 983040;
					
			}
			return 983040;
		}
		/// <summary>
		/// gets the maximum framesize given a level
		/// </summary>
		/// <param name="level">the level</param>
		/// <returns>the maximum framesize in number of macroblocks
		/// (1MB = 16x16)</returns>
		public int getMaxFS(int level)
		{
			switch (level)
			{
				case 0: // level 1
					return 99;
					
				case 1: // level 1.1
					return 396;
					
				case 2: // level 1.2
					return 396;
					
				case 3: // level 1.3
					return 396;
					
				case 4: // level 2
					return 396;
					
				case 5: // level 2.1
					return 792;
					
				case 6: // level 2.2
					return 1620;
					
				case 7: // level 3
					return 1620;
					
				case 8: // level 3.1
					return 3600;
					
				case 9: // level 3.2
					return 5120;
					
				case 10: // level 4
					return 8192;
					
				case 11: // level 4.1
					return 8192;
					
				case 12: // level 4.2
					return 8192;
					
				case 13: // level 5
					return 22080;
					
				case 14: // level 5.1
					return 36864;	
			}
			return 36864;
		}
		/// <summary>
		/// gets the maximum picture decoded buffer for the given level 
		/// </summary>
		/// <param name="level">the level</param>
		/// <returns>the size of the decoding buffer in bytes</returns>
		public double getMaxDPB(int level)
		{
			double maxDPB = 69120;
			switch (level)
			{
				case 0: // level 1
					maxDPB = 148.5;
					break;
				case 1: // level 1.1
					maxDPB =  337.5;
					break;
				case 2: // level 1.2
					maxDPB =  891;
					break;
				case 3: // level 1.3
					maxDPB =  891;
					break;
				case 4: // level 2
					maxDPB =  891;
					break;
				case 5: // level 2.1
					maxDPB = 1782;
					break;
				case 6: // level 2.2
					maxDPB = 3037.5;
					break;
				case 7: // level 3
					maxDPB = 3037.5;
					break;
				case 8: // level 3.1
					maxDPB = 6750;
					break;
				case 9: // level 3.2
					maxDPB = 7680;
					break;
				case 10: // level 4
					maxDPB = 12288;
					break;
				case 11: // level 4.1
					maxDPB = 12288;
					break;
				case 12: // level 4.2
					maxDPB = 12288;
					break;
				case 13: // level 5
					maxDPB = 41310;
					break;
				case 14: // level 5.1
					maxDPB = 69120;
					break;
			}
			return maxDPB * 1024;
        }
        #endregion
        #region verify and enforce
        /// <summary>
        /// Verifies a group of x264Settings against an AVC Level 
        /// </summary>
        /// <param name="settings">the x264Settings to test</param>
        /// <param name="level">the level</param>
        /// <param name="bytesInUncompressedFrame">Number of bytes in an uncompressed frame</param>
        /// <returns>   0 if the settings are compliant with the level
        ///             1 if (level > 3 || level = 3 AND Bframes > 0)
        ///             2 if maxDPB violated
        ///             3 if vbv_maxrate violated
        ///             4 if vbv_bufsize violated</returns>
        public int Verifyx264Settings(x264Settings settings, int level, double bytesInUncompressedFrame)
        {

            if (!this.checkP4x4(level, settings))
                return 1;

            if (!this.checkMaxDPB(level, settings, bytesInUncompressedFrame))
                return 2;

            if (settings.VBVMaxBitrate > this.getMaxBR(level, settings.Profile == 2))
                return 3;

            if (settings.VBVBufferSize > this.getMaxCBP(level, settings.Profile == 2))
                return 4;

            return 0;
        }
        /// <summary>
        /// Checks a collection of x264Settings and modifies them if needed to fit within the level constraints.
        /// </summary>
        /// <param name="level">the level to enforce</param>
        /// <param name="inputSettings">the collection of x264Settings to check</param>
        /// <param name="frameSize">the size of the decoded video frame in bytes</param>
        /// <returns>A compliant set of x264Settings</returns>
        public x264Settings EnforceSettings(int level, x264Settings inputSettings, double frameSize, out AVCLevelEnforcementReturn enforcement)
        {
            x264Settings enforcedSettings = (x264Settings) inputSettings.Clone();
            enforcement = new AVCLevelEnforcementReturn();
            enforcement.Altered = false;
            enforcement.EnableP4x4mv = true;
            enforcement.EnableVBVBufferSize = true;
            enforcement.EnableVBVMaxRate = true;
            enforcement.Panic = false;
            enforcement.PanicString = "";

            if (!checkP4x4(level, inputSettings))
            {
                enforcement.Altered = true;
                enforcedSettings.P4x4mv = false;
            }
            if (checkP4x4Enabled(level, inputSettings))
                enforcement.EnableP4x4mv = true;
            else
                enforcement.EnableP4x4mv = false;

            // step through various options to enforce the max decoded picture buffer size
            while (!this.checkMaxDPB(level,enforcedSettings, frameSize))
            {
                if (enforcedSettings.NbRefFrames > 1)
                {
                    enforcement.Altered = true;
                    enforcedSettings.NbRefFrames -= 1; // try reducing the number of reference frames
                }
                else
                {
                    enforcement.Panic = true;
                    enforcement.PanicString = "Can't force settings to conform to level (the frame size is too large)";
                    // reset output settings to original and set level to unrestrained
                    enforcedSettings = (x264Settings)inputSettings.Clone();
                    enforcedSettings.Level = 15;
                    return enforcedSettings;
                }   
            }

            // Disallow independent specification of MaxBitrate and MaxBufferSize unless Unrestrained
            if (level < 15)
            {
                enforcement.EnableVBVMaxRate = false;
                enforcedSettings.VBVMaxBitrate = -1;
                enforcement.EnableVBVBufferSize = false;
                enforcedSettings.VBVBufferSize = -1;
            }
            else
            {
                enforcement.EnableVBVMaxRate = true;
                enforcement.EnableVBVBufferSize = true;
            }

            return enforcedSettings;

        }
        /// <summary>
        /// validates a source against a given AVC level taking into account the source properties and the x264 settings
		/// <param name="bytesPerFrame">bytesize of a single frame</param>
		/// <param name="FS">frame area in pixels</param>
		/// <param name="MBPS">macroblocks per second</param>
        /// <param name="settings">the codec config to test</param>
		/// <param name="compliantLevel">the first avc level that can be used to encode this source</param>
		/// <returns>whether or not the current level is okay, if false and compliantLevel is -1, 
		/// the source could not be read</returns>
        public bool validateAVCLevel( int hRes, int vRes, double framerate, x264Settings settings, out int compliantLevel)
        {
            settings = (x264Settings)settings.Clone(); //Otherwise this sets it to the lowest compliant level anyway.
            const int unrestricted = 15; // maybe this should be set as a global constant
            compliantLevel = unrestricted;
            if (settings.Level == unrestricted) // 15 = unrestricted
                return true;

            int FrameSize = (int)maxFS(hRes, vRes);
            int MBPS = maxBPS(hRes, vRes, framerate);
            int hBlocks = macroblocks(hRes);
            int vBlocks = macroblocks(vRes);
            double bufferSize = pictureBufferSize(settings, bytesPerFrame(hRes, vRes));
            int allowableBPS = this.getMaxMBPS(settings.Level);
            int allowableFS = this.getMaxFS(settings.Level);
            double dimensionRestriction = Math.Ceiling(Math.Sqrt((double)(allowableFS)*8));
            double allowableDPB = this.getMaxDPB(settings.Level);

            if (allowableBPS >= MBPS && allowableFS >= FrameSize && allowableDPB >= bufferSize 
                && dimensionRestriction >= hBlocks && dimensionRestriction >= vBlocks)
                return true;
            else
            {
                while (settings.Level < unrestricted && (allowableBPS < MBPS || allowableFS < FrameSize || 
                    allowableDPB < bufferSize || dimensionRestriction < hBlocks || dimensionRestriction < vBlocks))
                {
                    settings.Level = settings.Level + 1;
                    allowableBPS = this.getMaxMBPS(settings.Level);
                    allowableFS = this.getMaxFS(settings.Level);
                    dimensionRestriction = Math.Ceiling(Math.Sqrt((double)(allowableFS)*8));
                    allowableDPB = this.getMaxDPB(settings.Level);
                }
                compliantLevel = settings.Level;
                return false;
            }
        }
        #endregion
    }
    #region Return structure for AVC level Enforcement
    public class AVCLevelEnforcementReturn
    {
        bool enableP4x4mv, enableVBVBufferSize, enableVBVMaxRate, altered;
        bool panic; // Panic! Something failed and the level was reset to unrestrained
        string panicString; // Description of the error that caused the panic

        public AVCLevelEnforcementReturn()
        {
            enableP4x4mv = true;
            enableVBVBufferSize = true;
            enableVBVMaxRate = true;
            altered = false;
        }
        public bool EnableP4x4mv
        {
            get {return enableP4x4mv;}
            set { enableP4x4mv = value; }
        }
        public bool EnableVBVBufferSize
        {
            get {return enableVBVBufferSize;}
            set { enableVBVBufferSize = value; }
        }
        public bool EnableVBVMaxRate
        {
            get {return enableVBVMaxRate;}
            set { enableVBVMaxRate = value; }
        }
        public bool Altered
        {
            get { return altered; }
            set { altered = value; }
        }
        public bool Panic
        {
            get { return panic; }
            set { panic = value; }
        }
        public string PanicString
        {
            get { return panicString; }
            set { panicString = value; }
        }
    }
    #endregion
}
