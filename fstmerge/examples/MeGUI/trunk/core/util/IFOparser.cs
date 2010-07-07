// ****************************************************************************
// 
// Copyright (C) 2009  Kurtnoise (kurtnoise@free.fr)
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

namespace MeGUI.core.util
{
    public sealed class IFOparser
    {
        /// <summary>
        /// Determine the IFO file that contains the menu: although it often is the largest
        /// IFO, this is not always the case, especially with elaborate DVDs with many extras.
        /// Therefore, look for the largest VOBs, and determine the IFO based on that.
        /// </summary>
        /// <param name="inputPath">Path that contains the DVD</param>
        /// <returns>Filename of the IFO that contains the movie</returns>
        public static string DetermineMovieIFO(string inputPath)
        {
            // The first 7 characters are the same for each VOB set, e.g.
            // VTS_24_0.VOB, VTS_24_1.VOB etc.
            string[] vobFiles = Directory.GetFiles(inputPath, "vts*.vob");
            if (vobFiles.Length == 0) return null;            

            // Look for the largest VOB set
            string vtsNameCurrent;
            string vtsNamePrevious = Path.GetFileName(vobFiles[0]).Substring(0, 7);
            long vtsSizeLargest = 0;
            long vtsSize = 0;
            string vtsNumber = "01";
            foreach (string file in vobFiles)
            {
                vtsNameCurrent = Path.GetFileName(file).Substring(0, 7);
                if (vtsNameCurrent.Equals(vtsNamePrevious))
                    vtsSize += new FileInfo(file).Length;
                else
                {
                    if (vtsSize > vtsSizeLargest)
                    {
                        vtsSizeLargest = vtsSize;
                        vtsNumber = vtsNamePrevious.Substring(4, 2);
                    }
                    vtsNamePrevious = vtsNameCurrent;
                    vtsSize = new FileInfo(file).Length;
                }
            }
            // Check whether the last one isn't the largest
            if (vtsSize > vtsSizeLargest)
                vtsNumber = vtsNamePrevious.Substring(4, 2);

            string ifoFile = inputPath + Path.DirectorySeparatorChar + "VTS_" + vtsNumber + "_0.IFO";
            // Name of largest VOB set is the name of the IFO, so we can now create the IFO file
            return ifoFile;
        }

        public static byte[] GetFileBlock(string strFile, long pos, int count)
        {
            using (FileStream stream = new FileStream(strFile, FileMode.Open, FileAccess.Read, FileShare.Read))
            {
                byte[] buf = new byte[count];
                stream.Seek(pos, SeekOrigin.Begin);
                if (stream.Read(buf, 0, count) != count)
                    return buf;
                return buf;
            }
        }

        public static short ToInt16(byte[] bytes) { return (short)((bytes[0] << 8) + bytes[1]); }
        public static uint ToInt32(byte[] bytes) { return (uint)((bytes[0] << 24) + (bytes[1] << 16) + (bytes[2] << 8) + bytes[3]); }
        public static short ToShort(byte[] bytes) { return ToInt16(bytes); }
        public static long ToFilePosition(byte[] bytes) { return ToInt32(bytes) * 0x800L; }

        public static long GetTotalFrames(TimeSpan time, int fps)
        {
            return (long)Math.Round(fps * time.TotalSeconds);
        }

        static string TwoLong(int val) { return string.Format("{0:D2}", val); }

        static int AsHex(int val)
        {
            int ret;
            int.TryParse(string.Format("{0:X2}", val), out ret);
            return ret;
        }

        internal static short? GetFrames(byte val)
        {
            int byte0_high = val >> 4;
            int byte0_low = val & 0x0F;
            if (byte0_high > 11)
                return (short)(((byte0_high - 12) * 10) + byte0_low);
            if ((byte0_high <= 3) || (byte0_high >= 8))
                return null;
            return (short)(((byte0_high - 4) * 10) + byte0_low);
        }

        internal static int GetFrames(TimeSpan time, int fps)
        {
            return (int)Math.Round(fps * time.Milliseconds / 1000.0);
        }
        internal static long GetPCGIP_Position(string ifoFile)
        {
            return ToFilePosition(GetFileBlock(ifoFile, 0xCC, 4));
        }

        internal static int GetProgramChains(string ifoFile, long pcgitPosition)
        {
            return ToInt16(GetFileBlock(ifoFile, pcgitPosition, 2));
        }

        internal static uint GetChainOffset(string ifoFile, long pcgitPosition, int programChain)
        {
            return ToInt32(GetFileBlock(ifoFile, (pcgitPosition + (8 * programChain)) + 4, 4));
        }

        internal static int GetNumberOfPrograms(string ifoFile, long pcgitPosition, uint chainOffset)
        {
            return GetFileBlock(ifoFile, (pcgitPosition + chainOffset) + 2, 1)[0];
        }

        internal static TimeSpan? ReadTimeSpan(string ifoFile, long pcgitPosition, uint chainOffset, out double fps)
        {
            return ReadTimeSpan(GetFileBlock(ifoFile, (pcgitPosition + chainOffset) + 4, 4), out fps);
        }

        internal static TimeSpan? ReadTimeSpan(byte[] playbackBytes, out double fps)
        {
            short? frames = GetFrames(playbackBytes[3]);
            int fpsMask = playbackBytes[3] >> 6;
            fps = fpsMask == 0x01 ? 25D : fpsMask == 0x03 ? (30D / 1.001D) : 0;
            if (frames == null)
                return null;

            try
            {
                int hours = AsHex(playbackBytes[0]);
                int minutes = AsHex(playbackBytes[1]);
                int seconds = AsHex(playbackBytes[2]);
                TimeSpan ret = new TimeSpan(hours, minutes, seconds);
                if (fps != 0)
                    ret = ret.Add(TimeSpan.FromSeconds((double)frames / fps));
                return ret;
            }
            catch { return null; }
        }

        /// <summary>
        /// get Audio Language from the IFO file
        /// </summary>
        /// <param name="fileName">name of the IFO file</param>
        /// <param name="count">the audio stream number</param>
        /// <returns>Language as String</returns>
        public static string getAudioLanguage(string FileName, int count)
        {
            FileStream fs = new FileStream(FileName, FileMode.Open, FileAccess.Read);
            BinaryReader br = new BinaryReader(fs);
            Stream sr = br.BaseStream;

            // go to audio stream number
            sr.Seek(0x203, SeekOrigin.Begin);
            byte a = br.ReadByte();
            sr.Seek(2, SeekOrigin.Current);
            if (count > 0) sr.Seek(8*count, SeekOrigin.Current);
            byte[] buff = new byte[2];
            br.Read(buff, 0, 2);
            string ShortLangCode = String.Format("{0}{1}", (char)buff[0], (char)buff[1]);
            string audioLang = LanguageSelectionContainer.Short2FullLanguageName(ShortLangCode);
            fs.Close();
            return audioLang;
        }

        /// <summary>
        /// get several Audio Informations from the IFO file
        /// </summary>
        /// <param name="fileName">name of the IFO file</param>
        /// <param name="verbose">to have complete infos or not</param>
        /// <returns>several infos as String</returns>
        public static AudioTrackInfo[] GetAudioInfos(string FileName, bool verbose)
        {
            FileStream fs = new FileStream(FileName, FileMode.Open, FileAccess.Read);
            BinaryReader br = new BinaryReader(fs);
            Stream sr = br.BaseStream;

            // go to audio stream number
            sr.Seek(0x203, SeekOrigin.Begin);

            byte a = br.ReadByte();
            if (a > 8)
                a = 8; // force the max #. According to the specs 8 is the max value for audio streams.

            AudioTrackInfo[] ati = new AudioTrackInfo[a];

            for (int i = 0; i < a; i++)
            {
                ati[i] = new AudioTrackInfo();
                byte[] array = new byte[2];
                fs.Read(array, 0, 2);
                string cm = GetAudioCodingMode(array);
                string sp = GetAudioSamplingRate(array);
                int ch = (int)((GetAudioChannels(array)) + 1);
                byte trackID = 0xBD;
                StringBuilder ad = new StringBuilder();

                switch (cm)
                {
                    case "AC3":  trackID = (byte)(0x80 + i); break;
                    case "LPCM": trackID = (byte)(0xA0 + i); break;
                    case "DTS":  trackID = (byte)(0x88 + i); break;
                    case "Mpeg-1":
                    case "Mpeg-2 Ext": trackID = (byte)(0xC0 + i); break;
                }

                if (verbose)
                {
                    string mce = "Not Present";
                    if (GetAudioMultichannelExt(array))
                        mce = "Present";
                    string lt = GetAudioLanguageType(array);
                    string ap = GetAudioApplicationMode(array);
                    string aq = GetAudioQuantization(array);

                    ad.AppendFormat("Track# : {0:00} - [{1:X}]", i, trackID, Environment.NewLine); 
                    ad.AppendFormat("Coding Mode            : {0}", cm, Environment.NewLine);
                    ad.AppendFormat("Multichannel Extension : {0}", mce, Environment.NewLine);
                    ad.AppendFormat("Language Type          : {0}", lt, Environment.NewLine);
                    ad.AppendFormat("Application Mode       : {0}", ap, Environment.NewLine);
                    ad.AppendFormat("Quantization           : {0}", aq, Environment.NewLine);
                    ad.AppendFormat("Sampling Rate          : {0}", sp, Environment.NewLine);
                    ad.AppendFormat("Channels               : {0}", ch, Environment.NewLine);
                }

                byte[] buff = new byte[2];
                br.Read(buff, 0, 2);
                string ShortLangCode = String.Format("{0}{1}", (char)buff[0], (char)buff[1]);

                sr.Seek(1, SeekOrigin.Current);
                byte[] l = new byte[1];
                fs.Read(l, 0, 1);
                string lce = GetAudioLanguageCodeExt(l);

                ati[i].TrackID = trackID;
                ati[i].NbChannels = ch + " channels";
                ati[i].SamplingRate = sp;
                ati[i].Type = cm;
                ati[i].Language = LanguageSelectionContainer.Short2FullLanguageName(ShortLangCode);
                if (verbose)
                {
                    ad.AppendFormat("Language              : {0} - {1}", LanguageSelectionContainer.Short2FullLanguageName(ShortLangCode), ShortLangCode, Environment.NewLine);
                    ad.AppendFormat("Language Extension    : {0}", lce, Environment.NewLine);
                    ad.AppendLine();
                    ati[i].Description = ad.ToString();
                }
                else
                {
                    ati[i].Description = String.Format("[{0:X}] - {1} - {2}ch / {3} / {4}", trackID, cm, ch, sp, LanguageSelectionContainer.Short2FullLanguageName(ShortLangCode));
                }

                // go to the next audio stream
                sr.Seek(2, SeekOrigin.Current);
            }

            fs.Close();

            return ati;
        }

        /// <summary>
        /// get the Audio Coding Mode from the Audio Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Coding Mode as String</returns>
        public static string GetAudioCodingMode(byte[] bytes)
        {
            byte b = (byte)((0xE0 & bytes[0]) >> 5);
            string codingMode = "";

            switch (b)
            {
                case 0: codingMode = "AC3"; break;
                case 1: codingMode = "Unknown"; break;
                case 2: codingMode = "Mpeg-1"; break;
                case 3: codingMode = "Mpeg-2 Ext"; break;
                case 4: codingMode = "LPCM"; break;
                case 5: codingMode = "Unknown"; break;
                case 6: codingMode = "DTS"; break;
                case 7: codingMode = "SDDS"; break;
            }
            return codingMode;
        }

        /// <summary>
        /// get the Multichannel Extension flag from the Audio Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Multichannel Extension as Bool</returns>
        public static bool GetAudioMultichannelExt(byte[] bytes)
        {
            byte b = (byte)((0x20 & bytes[0]) >> 4);

            if (b == 0) return false;
            else return true;
        }

        /// <summary>
        /// get the Audio Language Type from the Audio Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Language Type as String</returns>
        public static string GetAudioLanguageType(byte[] bytes)
        {
            byte b = (byte)((0x0C & bytes[0]) >> 2);
            string LanguageType = "Present";

            if (b == 0) LanguageType = "Not Present";
            return LanguageType;
        }

        /// <summary>
        /// get the Audio Application Mode from the Audio Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Application Mode as String</returns>
        public static string GetAudioApplicationMode(byte[] bytes)
        {
            byte b = (byte)(0x03 & bytes[0]);
            string ApplicationMode = "";
            
            switch (b)
            {
                case 0: ApplicationMode = "Unspecified"; break;
                case 1: ApplicationMode = "Karaoke"; break;
                case 2: ApplicationMode = "Surround"; break;
            }
            return ApplicationMode;
        }

        /// <summary>
        /// get the Audio Quantization from the Audio Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Quantization as String</returns>
        public static string GetAudioQuantization(byte[] bytes)
        {
            byte b = (byte)((0xC0 & bytes[1]) >> 6);
            string quantization = "16 Bits";

            switch (b)
            {
                case 0: quantization = "16 Bits"; break;
                case 1: quantization = "20 Bits"; break;
                case 2: quantization = "24 Bits"; break;
            }
            return quantization;
        }

        /// <summary>
        /// get the Audio Sampling Rate from the Audio Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Coding Mode as String</returns>
        public static string GetAudioSamplingRate(byte[] bytes)
        {
            byte b = (byte)((0x30 & bytes[1]) >> 4);
            string samplingRate = "";

            if (b == 0) samplingRate = "48 KHz";
            return samplingRate;
        }

        /// <summary>
        /// get the Audio Channels Number from the Audio Stream
        /// Specs specifies this value as Channels - 1
        /// That's why we must add +1 when we grab this info
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Channels as Byte</returns>
        public static byte GetAudioChannels(byte[] bytes)
        {
            byte b = (byte)(0x07 & bytes[1]);
            return b;
        }

        /// <summary>
        /// get the Language Code Extension from the Audio Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Language Code Extension as String</returns>
        public static string GetAudioLanguageCodeExt(byte[] bytes)
        {
            byte b = (byte)(0xFF & bytes[0]);
            string lce = "";
            switch (b)
            {
                case 0: lce = " Unspecified"; break;
                case 1: lce = " Normal"; break;
                case 2: lce = " For Visually Impaired"; break;
                case 3: lce = " Director's Comments"; break;
                case 4: lce = " Alternate Director's Comments"; break;
            }
            return lce;
        }


        /// <summary>
        /// get several Subtitles Informations from the IFO file
        /// </summary>
        /// <param name="fileName">name of the IFO file</param>
        /// <returns>several infos as String</returns>       
        public static string[] GetSubtitlesStreamsInfos(string FileName)
        {
            byte[] buff = new byte[2];
            byte s = 0;
            string[] subdesc = new string[s];

            try
            {
                FileStream fs = new FileStream(FileName, FileMode.Open, FileAccess.Read);
                BinaryReader br = new BinaryReader(fs);
                Stream sr = br.BaseStream;

                // go to the substream #1
                sr.Seek(0x255, SeekOrigin.Begin);

                s = br.ReadByte();
                if (s > 32)
                    s = 32; // force the max #. According to the specs 32 is the max value for subtitles streams.

                subdesc = new string[s];

                // go to the Language Code
                sr.Seek(2, SeekOrigin.Current);

                for (int i = 0; i < s; i++)
                {
                    // Presence (1 bit), Coding Mode (1bit), Short Language Code (2bits), Language Extension (1bit), Sub Picture Caption Type (1bit)
                    br.Read(buff, 0, 2);
                    string ShortLangCode = String.Format("{0}{1}", (char)buff[0], (char)buff[1]);

                    subdesc[i] = "[" + String.Format("{0:00}", i) + "]  - " + LanguageSelectionContainer.Short2FullLanguageName(ShortLangCode);

                    // Go to Code Extension
                    sr.Seek(1, SeekOrigin.Current);
                    buff[0] = br.ReadByte();

                    switch (buff[0] & 0x0F)
                    {
                        // from http://dvd.sourceforge.net/dvdinfo/sprm.html
                        case 1: subdesc[i] += " (Caption/Normal Size Char)"; break;
                        case 2: subdesc[i] += " (Caption/Large Size Char)"; break;
                        case 3: subdesc[i] += " (Caption For Children)"; break;
                        case 5: subdesc[i] += " (Closed Caption/Normal Size Char)"; break;
                        case 6: subdesc[i] += " (Closed Caption/Large Size Char)"; break;
                        case 7: subdesc[i] += " (Closed Caption For Children)"; break;
                        case 9: subdesc[i] += " (Forced Caption)"; break;
                        case 13: subdesc[i] += " (Director Comments/Normal Size Char)"; break;
                        case 14: subdesc[i] += " (Director Comments/Large Size Char)"; break;
                        case 15: subdesc[i] += " (Director Comments for Children)"; break;
                    }

                    if (buff[0] == 0) buff[0] = 1;

                    // go to the next sub stream
                    sr.Seek(2, SeekOrigin.Current);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString());
            }
           return subdesc;
        }

        /// <summary>
        /// get several Video Informations from the IFO file
        /// </summary>
        /// <param name="fileName">name of the IFO file</param>
        /// <param name="verbose">to have complete infos or not</param>
        /// <returns>several infos as String</returns>
        public static string GetVideoInfos(string FileName, bool verbose)
        {
            FileStream fs = new FileStream(FileName, FileMode.Open, FileAccess.Read);
            BinaryReader br = new BinaryReader(fs);
            Stream sr = br.BaseStream;

            sr.Seek(0x200, SeekOrigin.Begin);
            byte[] array = new byte[2];
            fs.Read(array, 0, 2);
            fs.Close();

            string videodesc = "";
            string cm = GetVideoCodingMode(array);
            string std = GetVideoStandard(array);
            string ar = GetVideoAR(array);
            string resolution = GetVideoResolution(array);
            string letterboxed = GetVideoLetterboxed(array);
            string stdType = GetVideoStandardType(array);

            if (verbose)
            {
                StringBuilder vd = new StringBuilder();
                string aps = "Not allowed";
                string alb = "Not allowed";
                if (GetVideoAutoPanScan(array))
                    aps = "allowed";
                if (GetVideoAutoLetterbox(array))
                    alb = "allowed";

                vd.AppendFormat("Coding Mode    : {0}", cm, Environment.NewLine);
                vd.AppendFormat("Standard       : {0}", std, Environment.NewLine);
                vd.AppendFormat("Aspect Ratio   : {0}", ar, Environment.NewLine);
                vd.AppendFormat("Resolution     : {0}", resolution, Environment.NewLine);
                vd.AppendFormat("Letterboxed    : {0}", letterboxed, Environment.NewLine);
                vd.AppendFormat("Standard Type  : {0}", stdType, Environment.NewLine);
                vd.AppendFormat("Auto Pan&Scan  : {0}", aps, Environment.NewLine);
                vd.AppendFormat("Auto Letterbox : {0}", alb, Environment.NewLine);
                videodesc = vd.ToString();
            }
            else
            {
                videodesc = string.Format("{0} / {1} / {2} / {3} / {4} / {5}", cm, std, ar, resolution, letterboxed, stdType);
            }

            return videodesc;
        }

        /// <summary>
        /// get the Video Coding Mode from the Video Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Coding Mode as String</returns>
        public static string GetVideoCodingMode(byte[] bytes)
        {
            byte b = (byte)((0xC0 & bytes[0]) >> 6);
            string codingMode = "";

            switch (b)
            {
                case 0: codingMode = "Mpeg-1"; break;
                case 1: codingMode = "Mpeg-2"; break;
            }
            return codingMode;
        }

        /// <summary>
        /// get the Standard used from the Video Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>PAL or NTSC</returns>
        public static string GetVideoStandard(byte[] bytes)
        {
            byte b = (byte)((0x30 & bytes[0]) >> 4);
            string standard = "";

            switch (b)
            {
                case 0: standard = "NTSC"; break;
                case 1: standard = "PAL"; break;
            }
            return standard;
        }

        /// <summary>
        /// get the Video Aspect Ratio from the Video Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>AR as String</returns>
        public static string GetVideoAR(byte[] bytes)
        {
            byte b = (byte)((0x0C & bytes[0]) >> 2);
            string ar = "";

            switch (b)
            {
                case 0: ar = "4:3"; break;
                case 1:
                case 2: ar = "Reserved"; break; 
                case 3: ar = "16:9"; break;        
            }
            return ar;
        }

        /// <summary>
        /// get the Automatic Pan&Scan flag from the Video Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Automatic Pan&Scan as Bool</returns>
        public static bool GetVideoAutoPanScan(byte[] bytes)
        {
            byte b = (byte)((0x02 & bytes[0]) >> 1);
            if (b == 1) return false;
            else return true;
        }

        /// <summary>
        /// get the Automatic Letterboxing flag from the Video Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Automatic Letterboxing as Bool</returns>
        public static bool GetVideoAutoLetterbox(byte[] bytes)
        {
            byte b = (byte)(0x01 & bytes[0]);
            if (b == 1) return false;
            else return true;
        }

        /// <summary>
        /// get the Resolution from the Video Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Resolution as String</returns>
        public static string GetVideoResolution(byte[] bytes)
        {
            byte b = (byte)((0x30 & bytes[0]) >> 4); // Standard
            byte c = (byte)((0x38 & bytes[1]) >> 3); // Resolution
            string res = "";

            switch (b)
            {
                case 0: // NTSC
                    {
                        switch (c)
                        {
                            case 0: res = "720x480"; break;
                            case 1: res = "704x480"; break;
                            case 2: res = "352x480"; break;
                            case 3: res = "352x240"; break;
                        }
                    }; break;
                case 1: // PAL
                    {
                        switch (c)
                        {
                            case 0: res = "720x576"; break;
                            case 1: res = "704x576"; break;
                            case 2: res = "352x576"; break;
                            case 3: res = "352x288"; break;
                        }
                    }; break;
            }
            return res;
        }

        /// <summary>
        /// get the Letterboxed Info from the Video Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Letterboxed as String</returns>
        public static string GetVideoLetterboxed(byte[] bytes)
        {
            byte b = (byte)((4 & bytes[1]));
            string Letterboxed = "";

            if (b > 0)
                Letterboxed = "Not Letterboxed";
            else
                Letterboxed = "Letterboxed";

            return Letterboxed;
        }

        /// <summary>
        /// get the Video Type from the Video Stream
        /// </summary>
        /// <param name="bytes">array of bytes</param>
        /// <returns>Camera/Film as String</returns>
        public static string GetVideoStandardType(byte[] bytes)
        {
            byte b = (byte)((0x30 & bytes[0]) >> 4); // Standard
            byte c = (byte)(1 & bytes[1]); // type
            string StandardType = "";

            if (b == 1) // PAL
            {
                if (c > 0) StandardType = "Camera"; 
                else StandardType = "Film";
            }
            return StandardType;
        }

        /// <summary>
        /// get number of PGCs
        /// </summary>
        /// <param name="fileName">name of the IFO file</param>
        /// <returns>number of PGS as unsigned integer</returns>
        public static uint getPGCnb(string FileName)
        {
            FileStream fs = new FileStream(FileName, FileMode.Open, FileAccess.Read);
            BinaryReader br = new BinaryReader(fs);
            Stream sr = br.BaseStream;

            sr.Seek(0xCC, SeekOrigin.Begin);
            uint buf = ReadUInt32(br);									// Read PGC offset
            sr.Seek(2048 * buf + 0x1, SeekOrigin.Begin);			// Move to beginning of PGC
            long VTS_PGCITI_start_position = sr.Position - 1;
            byte nPGCs = br.ReadByte();									// Number of PGCs

            return nPGCs;
        }

        private static uint ReadUInt32(BinaryReader br)
        {
            uint val = (
                ((uint)br.ReadByte()) << 24 |
                ((uint)br.ReadByte()) << 16 |
                ((uint)br.ReadByte()) << 8 |
                ((uint)br.ReadByte()));
            return val;
        }
    }
}
