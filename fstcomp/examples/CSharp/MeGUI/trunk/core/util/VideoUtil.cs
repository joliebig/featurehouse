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

using Utils.MessageBoxExLib;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Text;
using System.Threading;
using System.Windows.Forms;

using MeGUI.core.details;
using MeGUI.core.gui;
using MeGUI.core.util;

using MediaInfoWrapper;

namespace MeGUI
{
	/// <summary>
	/// VideoUtil is used to perform various video related tasks, namely autocropping, 
	/// auto resizing
	/// </summary>
	public class VideoUtil
    {

        private MainForm mainForm;
		private JobUtil jobUtil;
		public VideoUtil(MainForm mainForm)
		{
			this.mainForm = mainForm;
			jobUtil = new JobUtil(mainForm);
        }

		#region finding source information
		/// <summary>
		/// gets the dvd decrypter generated stream information file
		/// </summary>
		/// <param name="fileName">name of the first vob to be loaded</param>
		/// <returns>full name of the info file or an empty string if no file was found</returns>
		public static string getInfoFileName(string fileName)
		{
			int pgc = 1;
			string path = Path.GetDirectoryName(fileName);
			string name = Path.GetFileName(fileName);
			string vts = name.Substring(0, 6);
			string infoFile = "";
			string[] files = Directory.GetFiles(path, vts + "*.txt");
			foreach (string file in files)
			{
				if (file.IndexOf("Stream Information") != -1) // we found our file
				{
                    int index = file.IndexOf("_PGC_");
                    if (index != -1) // PGC number is in the filename
                    {
                        string pgcString = file.Substring(index + 5, 2);
                        try
                        {
                            pgc = Int32.Parse(pgcString);
                        }
                        catch (Exception)
                        {
                        }
                    }
					infoFile = file;
					break;
				}
			}
            // pgc is parsed, but unused. Might be useful later
            return infoFile;
		}

		/// <summary>
		/// gets the dvd decrypter generated chapter file
		/// </summary>
		/// <param name="fileName">name of the first vob to be loaded</param>
		/// <returns>full name of the chapter file or an empty string if no file was found</returns>
		public static string getChapterFile(string fileName)
		{
			string path = Path.GetDirectoryName(fileName);
			string name = Path.GetFileName(fileName);
			string vts = name.Substring(0, 6);
			string chapterFile = "";
            string[] files = Directory.GetFiles(path, vts + "*Chapter Information*");
			foreach (string file in files)
			{
				if (file.ToLower().EndsWith(".txt") || file.ToLower().EndsWith(".qpf"))
                {
					chapterFile = file;
					break;
				}                   
			}
			return chapterFile;
		}
		/// <summary>
		/// gets information about a video source based on its DVD Decrypter generated info file
		/// </summary>
		/// <param name="infoFile">the info file to be analyzed</param>
		/// <param name="audioTracks">the audio tracks found</param>
		/// <param name="aspectRatio">the aspect ratio of the video</param>
        public void getSourceInfo(string infoFile, out List<AudioTrackInfo> audioTracks, out List<SubtitleInfo> subtitles,
			out Dar? aspectRatio, out int maxHorizontalResolution)
		{
			StreamReader sr = null;
            audioTracks = new List<AudioTrackInfo>();
            subtitles = new List<SubtitleInfo>();
            aspectRatio = null;
            maxHorizontalResolution = 5000;
			try
			{
				sr = new StreamReader(infoFile, System.Text.Encoding.Default);
                string line = ""; int LineCount = 0;
				while ((line = sr.ReadLine()) != null)
				{
					if (line.IndexOf("Video") != -1)
					{
						char[] separator = {'/'};
						string[] split = line.Split(separator, 1000);
                        string resolution = split[1];
                        resolution = resolution.Substring(1, resolution.IndexOf('x')-1);
                        maxHorizontalResolution = Int32.Parse(resolution);
						string ar = split[2].Substring(1, split[2].Length - 2);

                        aspectRatio = Dar.A1x1;
                        if (split[1].Contains("PAL"))
                        {
                            if (ar.Equals("16:9"))
                                aspectRatio = Dar.ITU16x9PAL;
                            else if (ar.Equals("4:3"))
                                aspectRatio = Dar.ITU4x3PAL;
                        }
                        else if (split[1].Contains("NTSC"))
                        {
                            if (ar.Equals("16:9"))
                                aspectRatio = Dar.ITU16x9NTSC;
                            else if (ar.Equals("4:3"))
                                aspectRatio = Dar.ITU4x3NTSC;
                        }
					}
					else if (line.IndexOf("Audio") != -1)
					{
						char[] separator = {'/'};
						string[] split = line.Split(separator, 1000); 
						AudioTrackInfo ati = new AudioTrackInfo();
						ati.Type = split[0].Substring(split[0].LastIndexOf("-") + 1).Trim();
						ati.NbChannels = split[1].Trim();
                        ati.TrackInfo = new TrackInfo(split[4].Trim(), null);
                        audioTracks.Add(ati);
					}
                    else if (line.IndexOf("Subtitle") != -1)
                    {
                        char[] separator = { '-' };
                        string[] split = line.Split(separator, 1000);
                        string language = split[2].Trim();
                        SubtitleInfo si = new SubtitleInfo(language, LineCount);
                        LineCount++; // must be there coz vobsub index begins to zero...                        
                        subtitles.Add(si);
                    }
				}
			}
			catch (Exception i)
			{
				MessageBox.Show("The following error ocurred when parsing the info file " + infoFile + "\r\n" + i.Message, "Error parsing info file", MessageBoxButtons.OK);
                audioTracks.Clear();
			}
			finally
			{
				if (sr != null)
				{
					try 
					{
						sr.Close();
					}
					catch (IOException i)
					{
						Trace.WriteLine("IO Exception when closing StreamReader in FileIndexerWindow: " + i.Message);
					}
				}
			}
		}

        /// <summary>
        /// gets information about a video source using MediaInfo
        /// </summary>
        /// <param name="infoFile">the info file to be analyzed</param>
        /// <param name="audioTracks">the audio tracks found</param>
        /// <param name="maxHorizontalResolution">the width of the video</param>
        public void getSourceMediaInfo(string fileName, out List<AudioTrackInfo> audioTracks, out int maxHorizontalResolution, out Dar? dar)
        {
            MediaInfo info;
            audioTracks = new List<AudioTrackInfo>();
            maxHorizontalResolution = 5000;
            dar = Dar.A1x1;
            try
            {
                info = new MediaInfo(fileName);
                maxHorizontalResolution = Int32.Parse(info.Video[0].Width);

                if (info.Video[0].Width == "720" && (info.Video[0].Height == "576" || info.Video[0].Height == "480"))
                {
                    if (info.Video[0].Height == "576")
                    {
                        if (info.Video[0].AspectRatioString.Equals("16:9"))
                            dar = Dar.ITU16x9PAL;
                        else if (info.Video[0].AspectRatioString.Equals("4:3"))
                            dar = Dar.ITU4x3PAL;
                        else
                            dar = new Dar(ulong.Parse(info.Video[0].Width), ulong.Parse(info.Video[0].Height));
                    }
                    else
                    {
                        if (info.Video[0].AspectRatioString.Equals("16:9"))
                            dar = Dar.ITU16x9NTSC;
                        else if (info.Video[0].AspectRatioString.Equals("4:3"))
                            dar = Dar.ITU4x3NTSC;
                        else
                            dar = new Dar(ulong.Parse(info.Video[0].Width), ulong.Parse(info.Video[0].Height));
                    }
                }
                else
                {
                    dar = new Dar(ulong.Parse(info.Video[0].Width), ulong.Parse(info.Video[0].Height));
                }

                for (int counter = 0; counter < info.Audio.Count; counter++)
                {
                    MediaInfoWrapper.AudioTrack atrack = info.Audio[counter];
                    AudioTrackInfo ati = new AudioTrackInfo();
                    // DGIndex expects audio index not ID for TS
                    ati.ContainerType = info.General[0].Format;
                    ati.Index = counter;
                    if (info.General[0].Format == "CDXA/MPEG-PS")
                        // MediaInfo doesn't give TrackID for VCD, specs indicate only MP1L2 is supported
                        ati.TrackID = (0xC0 + counter);
                    else if (atrack.ID != "0" && atrack.ID != "")
                        ati.TrackID = Int32.Parse(atrack.ID);
                    else
                        // MediaInfo failed to get ID try guessing based on codec
                        switch (atrack.Format.Substring(0,3))
                        {
                            case "AC3": ati.TrackID = (0x80 + counter); break;
                            case "PCM": ati.TrackID = (0xA0 + counter); break;
                            case "MPE": // MPEG-1 Layer 1/2/3
                            case "MPA": ati.TrackID = (0xC0 + counter); break;
                            case "DTS": ati.TrackID = (0x88 + counter); break;
                        }
                    if (atrack.FormatProfile != "") // some tunings to have a more useful info instead of a typical audio Format
                    {
                        switch (atrack.FormatProfile)
                        {   
                            case "Dolby Digital": ati.Type = "AC-3"; break;
                            case "HRA": ati.Type = "DTS-HD High Resolution"; break;
                            case "Layer 1": ati.Type = "MPA"; break;
                            case "Layer 2": ati.Type = "MP2"; break;
                            case "Layer 3": ati.Type = "MP3"; break;
                            case "LC": ati.Type = "AAC"; break;
                            case "MA": ati.Type = "DTS-HD Master Audio"; break;
                            case "TrueHD": ati.Type = "TrueHD"; break;
                        }
                    }
                    else ati.Type = atrack.Format;
                    ati.NbChannels = atrack.ChannelsString;
                    ati.SamplingRate = atrack.SamplingRateString;
                    if (atrack.LanguageString == "") // to retrieve Language 
                    {
                        if (Path.GetExtension(fileName.ToLower()) == ".vob")
                        {
                            string ifoFile;
                            string fileNameNoPath = Path.GetFileName(fileName);

                            // Languages are not present in VOB, so we check the main IFO
                            if (fileNameNoPath.Substring(0, 4) == "VTS_")
                                 ifoFile = fileName.Substring(0, fileName.LastIndexOf("_")) + "_0.IFO";
                            else ifoFile = Path.ChangeExtension(fileName, ".IFO");

                            if (File.Exists(ifoFile))
                                atrack.LanguageString = IFOparser.getAudioLanguage(ifoFile, counter);
                        }
                    }
                    ati.TrackInfo = new TrackInfo(atrack.LanguageString, null);
                    audioTracks.Add(ati);
                    if (info.General[0].Format == "MPEG-TS")
                        break;  // DGIndex only supports first audio stream with TS files
                }
            }
            catch (Exception i)
            {
                MessageBox.Show("The following error ocurred when trying to get Media info for file " + fileName + "\r\n" + i.Message, "Error parsing mediainfo data", MessageBoxButtons.OK);
                audioTracks.Clear();
            }
        }

        /// <summary>
        /// gets chapters from IFO file and save them as Ogg Text File
        /// </summary>
        /// <param name="fileName"></param>
        public static void getChaptersFromIFO(string fileName, bool qpfile)
        {
            if (Path.GetExtension(fileName.ToLower()) == ".vob")
            {
                string ifoFile;
                string fileNameNoPath = Path.GetFileName(fileName);

                // we check the main IFO
                if (fileNameNoPath.Substring(0, 4) == "VTS_")
                    ifoFile = fileName.Substring(0, fileName.LastIndexOf("_")) + "_0.IFO";
                else ifoFile = Path.ChangeExtension(fileName, ".IFO");

                if (File.Exists(ifoFile))
                {
                    ChapterInfo pgc;
                    ChapterExtractor ex = new IfoExtractor();
                    pgc = ex.GetStreams(ifoFile)[0];
                    if (Drives.ableToWriteOnThisDrive(Path.GetPathRoot(ifoFile)))
                    {
                        if (qpfile)
                            pgc.SaveQpfile(Path.GetDirectoryName(ifoFile) + "\\" + fileNameNoPath.Substring(0, 6) + " - Chapter Information.qpf");

                        // save always this format - some users want it for the mux
                        pgc.SaveText(Path.GetDirectoryName(ifoFile) + "\\" + fileNameNoPath.Substring(0, 6) + " - Chapter Information - OGG.txt");
                    }
                    else
                        MessageBox.Show("MeGUI cannot write on the disc " + Path.GetPathRoot(ifoFile) + " \n" +
                                        "Please, select another output path to save the chapters file...", "Configuration Incomplete", MessageBoxButtons.OK, MessageBoxIcon.Warning);

                }
            }            
        }

        /// <summary>
        /// gets Timeline from Chapters Text file (formally as Ogg Format)
        /// </summary>
        /// <param name="fileName">the file read</param>
        /// <returns>chapters Timeline as string</returns>
        public static string getChapterTimeLine(string fileName)
        {
            long count = 0;
            string line;
            string chap = "=";
                
            using (StreamReader r = new StreamReader(fileName))
            {
                while ((line = r.ReadLine()) != null)
                {
                    count++;
                    if (count % 2 != 0) // odd line
                    {
                        if (count >= 2)
                            chap += ";";
                        chap += line.Substring(line.IndexOf("=") + 1, 12);
                    }
                }
            }
            return chap;
        }

        /// <summary>
        /// gets ID from a first video stream using MediaInfo
        /// </summary>
        /// <param name="infoFile">the file to be analyzed</param>
        /// <returns>the video track ID found</returns>
        public static int getIDFromFirstVideoStream(string fileName)
        {
            MediaInfo info;
            int TrackID = 0;
            try
            {
                info = new MediaInfo(fileName);
                if (info.Video.Count > 0)
                {
                    MediaInfoWrapper.VideoTrack vtrack = info.Video[0];
                    TrackID = Int32.Parse(vtrack.ID);
                }
            }
            catch (Exception i)
            {
                MessageBox.Show("The following error ocurred when trying to get Media info for file " + fileName + "\r\n" + i.Message, "Error parsing mediainfo data", MessageBoxButtons.OK);
            }
            return TrackID;
        }

        /// <summary>
        /// gets Video Codec and Container Format using MediaInfo
        /// </summary>
        /// <param name="strFileName">the file to be analyzed</param>
        /// <param name="strVideoCodec">the Video Codec</param>
        /// <param name="strContainerFormat">the Container Format</param>
        /// <param name="audioTracks">the audio tracks</param>
        /// <returns>true if successful</returns>
        public static bool getMediaInformation(string strFileName, out string strVideoCodec, out string strVideoScanType, out string strContainerFormat, out List<AudioTrackInfo> audioTracks)
        {
            MediaInfo info;
    
            strVideoCodec = "";
            strVideoScanType = "";
            strContainerFormat = "";
            audioTracks = new List<AudioTrackInfo>();

            try
            {
                info = new MediaInfo(strFileName);
                if (info.Video.Count > 0)
                {
                    MediaInfoWrapper.VideoTrack vtrack = info.Video[0];
                    strVideoCodec = vtrack.CodecString;
                    strVideoScanType = vtrack.ScanTypeString;
                }
                MediaInfoWrapper.GeneralTrack gtrack = info.General[0];
                strContainerFormat = gtrack.Format;
                
                for (int counter = 0; counter < info.Audio.Count; counter++)
                {
                    MediaInfoWrapper.AudioTrack atrack = info.Audio[counter];
                    AudioTrackInfo ati = new AudioTrackInfo();
                    // DGIndex expects audio index not ID for TS
                    ati.ContainerType = info.General[0].Format;
                    ati.Index = counter;
                    if (info.General[0].Format == "CDXA/MPEG-PS")
                        // MediaInfo doesn't give TrackID for VCD, specs indicate only MP1L2 is supported
                        ati.TrackID = (0xC0 + counter);
                    else if (atrack.ID != "0" && atrack.ID != "")
                        ati.TrackID = Int32.Parse(atrack.ID);
                    else
                        // MediaInfo failed to get ID try guessing based on codec
                        switch (atrack.Format.Substring(0, 3))
                        {
                            case "AC3": ati.TrackID = (0x80 + counter); break;
                            case "PCM": ati.TrackID = (0xA0 + counter); break;
                            case "MPE": // MPEG-1 Layer 1/2/3
                            case "MPA": ati.TrackID = (0xC0 + counter); break;
                            case "DTS": ati.TrackID = (0x88 + counter); break;
                        }
                    if (atrack.FormatProfile != "") // some tunings to have a more useful info instead of a typical audio Format
                    {
                        switch (atrack.FormatProfile)
                        {
                            case "Dolby Digital": ati.Type = "AC-3"; break;
                            case "HRA": ati.Type = "DTS-HD High Resolution"; break;
                            case "Layer 1": ati.Type = "MPA"; break;
                            case "Layer 2": ati.Type = "MP2"; break;
                            case "Layer 3": ati.Type = "MP3"; break;
                            case "LC": ati.Type = "AAC"; break;
                            case "MA": ati.Type = "DTS-HD Master Audio"; break;
                            case "TrueHD": ati.Type = "TrueHD"; break;
                        }
                    }
                    else ati.Type = atrack.Format;
                    ati.NbChannels = atrack.ChannelsString;
                    ati.SamplingRate = atrack.SamplingRateString;
                    if (atrack.LanguageString == "") // to retrieve Language 
                    {
                        if (Path.GetExtension(strFileName.ToLower()) == ".vob")
                        {
                            string ifoFile;
                            string fileNameNoPath = Path.GetFileName(strFileName);

                            // Languages are not present in VOB, so we check the main IFO
                            if (fileNameNoPath.Substring(0, 4) == "VTS_")
                                ifoFile = strFileName.Substring(0, strFileName.LastIndexOf("_")) + "_0.IFO";
                            else ifoFile = Path.ChangeExtension(strFileName, ".IFO");

                            if (File.Exists(ifoFile))
                                atrack.LanguageString = IFOparser.getAudioLanguage(ifoFile, counter);
                        }
                    }
                    ati.TrackInfo = new TrackInfo(atrack.LanguageString, null);
                    audioTracks.Add(ati);
                }

                info.Dispose();
                return true;
            }
            catch (Exception i)
            {
                MessageBox.Show("The following error ocurred when trying to get Media info for file " + strFileName + "\r\n" + i.Message, "Error parsing mediainfo data", MessageBoxButtons.OK);
                return false;
            }
        }

        /// detect AVC stream from a file using MediaInfo
        /// </summary>
        /// <param name="infoFile">the file to be analyzed</param>
        /// <returns>AVC stream found whether or not</returns>
        public static bool detecAVCStreamFromFile(string fileName)
        {
            MediaInfo info;
            bool avcS = false;
            try
            {
                info = new MediaInfo(fileName);
                if (info.Video.Count > 0)
                {
                    MediaInfoWrapper.VideoTrack vtrack = info.Video[0];
                    string format = vtrack.Format;
                    if (format == "AVC")
                        avcS = true;
                }
            }
            catch (Exception i)
            {
                MessageBox.Show("The following error ocurred when trying to get Media info for file " + fileName + "\r\n" + i.Message, "Error parsing mediainfo data", MessageBoxButtons.OK);
            }
            return avcS;
        }

        /// gets ID from audio stream using MediaInfo
        /// </summary>
        /// <param name="infoFile">the file to be analyzed</param>
        /// <param name="count">the counter</param>
        /// <returns>the audio track ID found</returns>
        public static int getIDFromAudioStream(string fileName)
        {
            MediaInfo info;
            int TrackID = 0;
            try
            {
                info = new MediaInfo(fileName);
                if (info.Audio.Count > 0)
                {
                    MediaInfoWrapper.AudioTrack atrack = info.Audio[0];
                    TrackID = Int32.Parse(atrack.ID);
                }
            }
            catch (Exception i)
            {
                MessageBox.Show("The following error ocurred when trying to get Media info for file " + fileName + "\r\n" + i.Message, "Error parsing mediainfo data", MessageBoxButtons.OK);
            }
            return TrackID;
        }

        public static int getIDFromSubStream(string fileName)
        {
            MediaInfo info;
            int TrackID = 0;
            try
            {
                info = new MediaInfo(fileName);
                if (info.Text.Count > 0)
                {
                    MediaInfoWrapper.TextTrack strack = info.Text[0];
                    TrackID = Int32.Parse(strack.ID);
                }
            }
            catch (Exception i)
            {
                MessageBox.Show("The following error ocurred when trying to get Media info for file " + fileName + "\r\n" + i.Message, "Error parsing mediainfo data", MessageBoxButtons.OK);
            }
            return TrackID;
        }

        /// <summary>
        /// gets Audio Streams Number from input file using MediaInfo
        /// </summary>
        /// <param name="fileName">input</param>
        /// <returns>nb of audio streams found</returns>
        public static int getAudioStreamsNb(string fileName)
        {
            MediaInfo info;
            int nb = 0;
            try
            {
                info = new MediaInfo(fileName);
                nb = info.Audio.Count;
            }
            catch (Exception i)
            {
                MessageBox.Show("The following error ocurred when trying to get Media info for file " + fileName + "\r\n" + i.Message, "Error parsing mediainfo data", MessageBoxButtons.OK);
            }
            return nb;
        }

        /// gets SBR flag from AAC streams using MediaInfo
        /// </summary>
        /// <param name="infoFile">the file to be analyzed</param>
        /// <param name="count">the counter</param>
        /// <returns>the flag found</returns>
        public static int getSBRFlagFromAACStream(string fileName)
        {
            MediaInfo info;
            int flag = 0;
            try
            {
                info = new MediaInfo(fileName);
                if (info.Audio.Count > 0)
                {
                    MediaInfoWrapper.AudioTrack atrack = info.Audio[0];
                    if (atrack.Format == "AAC")
                    {
                        if (atrack.FormatSettingsSBR == "Yes")
                             flag = 1;
                        if (atrack.SamplingRate == "24000")
                        {
                            if ((atrack.Channels == "2") || (atrack.Channels == "1")) // workaround for raw aac
                                flag = 1;
                        }
                    }
                }
            }
            catch (Exception i)
            {
                MessageBox.Show("The following error ocurred when trying to get Media info for file " + fileName + "\r\n" + i.Message, "Error parsing mediainfo data", MessageBoxButtons.OK);
            }
            return flag;
        }


        /// <summary>
        /// gets basic information about a video source based on its DGindex generated log file
        /// </summary>
        /// <param name="logFile">the log file to be analyzed</param>
        /// <param name="audioTrackIDs">the audio tracks IDs found</param>
        public void getDGindexLogInfo(string logFile, out List<AudioTrackInfo> audioTrackIDs)
        {
            StreamReader sr = null;
            audioTrackIDs = new List<AudioTrackInfo>();
            try
            {
                sr = new StreamReader(logFile, System.Text.Encoding.Default);
                string line = "";
                while ((line = sr.ReadLine()) != null)
                {
                    if (line.IndexOf("Audio Stream") != -1)
                    {
                        char[] separator = { ':' };
                        string[] split = line.Split(separator, 1000);
                        AudioTrackInfo ati = new AudioTrackInfo();
                        ati.TrackIDx = split[1];
                        ati.Type = split[2].Trim().Substring(0, 3);
                        audioTrackIDs.Add(ati);
                    }
                }
            }
            catch
                (Exception i)
            {
                MessageBox.Show("The following error ocurred when parsing the log file " + logFile + "\r\n" + i.Message, "Error parsing log file", MessageBoxButtons.OK);
                audioTrackIDs.Clear();
            }
        }

        public static List<string> setDeviceTypes(string outputFormat)
        {
            List<string> deviceList = new List<string>();
            switch (outputFormat)
            {
                case ".mp4": deviceList.AddRange(new string[] { "iPhone", "iPod", "ISMA", "PSP" }); break;
                case ".m2ts": deviceList.AddRange(new string[] { "AVCHD", "Blu-ray" }); break;
            }

            return deviceList;
        }

        /// <summary>
        /// Manage CUVIDServer from DGIndexNV tools package
        /// </summary>
        public static bool manageCUVIDServer()
        {
            MainForm.Instance.DialogManager.runCUVIDServer();
            return true;
        }

        public static bool findDGSource(string FileName)
        {
            using (StreamReader sr = new StreamReader(FileName))
            {
                string line = string.Empty;
                while ((line = sr.ReadLine()) != null)
                {
                    if (line.ToLower().Contains("dgsource"))
                    {
                        return true;
                    }
                }
            }
            return false;
        }

		#endregion
		#region dgindex preprocessing
		/// <summary>
		/// opens a video source and fills out the track selector dropdowns
		/// </summary>
		/// <param name="fileName">the video input file</param>
		/// <param name="track1">combobox for audio track selection</param>
		/// <param name="track2">combobox for audio track selection</param>
		/// <param name="ar">aspect ratio of the video</param>
		/// <param name="trackIDs">an arraylist that will contain the track IDs of the source if found</param>
		/// <returns>true if a source info file has been found, false if not</returns>
        public bool openVideoSource(string fileName, out List<AudioTrackInfo> audioTracks, out List<SubtitleInfo> subtitles, 
            out Dar? ar, out int maxHorizontalResolution)
		{
            audioTracks = new List<AudioTrackInfo>();
            subtitles = new List<SubtitleInfo>();
            string infoFile = VideoUtil.getInfoFileName(fileName);
			bool putDummyTracks = true; // indicates whether audio tracks have been found or not
			ar = null;
            maxHorizontalResolution = 5000;

            getSourceMediaInfo(fileName, out audioTracks, out maxHorizontalResolution, out ar);
            ar = null; // muss noch weg!
            if ((audioTracks.Count > 0) || (subtitles.Count > 0))
                putDummyTracks = false;

			if (putDummyTracks)
			{                
                for (int i = 1; i <= 8; i++)
                {
                    audioTracks.Add(new AudioTrackInfo("Track " + i, "", "", i));
                }

                subtitles.Clear();
                for (int i = 1; i <= 32; i++)
                {
                     subtitles.Add(new SubtitleInfo("Track " + i, i));
                }
			}
			return putDummyTracks;
		}
		#endregion
		#region dgindex postprocessing
		/// <summary>
		/// gets all demuxed audio files from a given dgindex project
		/// starts with the first file and returns the desired number of files
		/// </summary>
        /// <param name="audioTrackIDs">list of audio TrackIDs</param>
		/// <param name="projectName">the name of the dgindex project</param>
		/// <param name="cutoff">maximum number of results to be returned</param>
		/// <returns>an array of string of filenames</returns>
        public Dictionary<int, string> getAllDemuxedAudio(List<AudioTrackInfo> audioTracks, string projectName, int cutoff)
        {
		    Dictionary<int, string> audioFiles = new Dictionary<int, string>();
            for (int counter = 0; counter < audioTracks.Count; counter++)
            {
                string trackNumber = audioTracks[counter].ContainerType == "MPEG-TS" ? " PID " : " T";
                trackNumber += audioTracks[counter].TrackIDx + "*";
                string [] files = Directory.GetFiles(Path.GetDirectoryName(projectName),
				        Path.GetFileNameWithoutExtension(projectName) + trackNumber);
                foreach (string file in files)
                {
                    if ( file.EndsWith(".ac3") ||
                         file.EndsWith(".mp3") ||
                         file.EndsWith(".mp2") ||
                         file.EndsWith(".mp1") ||
                         file.EndsWith(".mpa") ||
                         file.EndsWith(".dts") ||
                         file.EndsWith(".wav") ||
                         file.EndsWith(".aac")) // It is the right track
					{
                        audioFiles.Add(audioTracks[counter].TrackID, file);
                        break;
					}
				}
			}
            return audioFiles;
		}

        public Dictionary<int, string> getAllDemuxedSubtitles(List<SubtitleInfo> subTracks, string projectName)
        {
            Dictionary<int, string> subFiles = new Dictionary<int, string>();
            for (int counter = 0; counter < subTracks.Count; counter++)
            {
                string[] files = Directory.GetFiles(Path.GetDirectoryName(projectName),
                        Path.GetFileNameWithoutExtension(projectName));
                foreach (string file in files)
                {
                    if (file.EndsWith(".idx") ||
                        file.EndsWith(".srt") ||
                        file.EndsWith(".ssa") ||
                        file.EndsWith(".ass")) // It is the right track
                    {
                        subFiles.Add(subTracks[counter].Index, file);
                        break;
                    }
                }
            }
            return subFiles;
        }

		#endregion
		#region automated job generation
		/// <summary>
		/// ensures that video and audio don't have the same filenames which would lead to severe problems
		/// </summary>
		/// <param name="videoOutput">name of the encoded video file</param>
		/// <param name="muxedOutput">name of the final output</param>
		/// <param name="aStreams">all encodable audio streams</param>
		/// <param name="audio">all muxable audio streams</param>
		/// <returns>the info to be added to the log</returns>
		public LogItem eliminatedDuplicateFilenames(ref string videoOutput, ref string muxedOutput, AudioJob[] aStreams)
		{
            LogItem log = new LogItem("Eliminating duplicate filenames");
            videoOutput = Path.GetFullPath(videoOutput);
            muxedOutput = Path.GetFullPath(muxedOutput);

            log.LogValue("Video output file", videoOutput);
            if (File.Exists(videoOutput))
            {
                int counter = 0;
                string directoryname = Path.GetDirectoryName(videoOutput);
                string filename = Path.GetFileNameWithoutExtension(videoOutput);
                string extension = Path.GetExtension(videoOutput);

                while (File.Exists(videoOutput))
                {
                    videoOutput = Path.Combine(directoryname,
                        filename + "_" + counter + extension);
                    counter++;
                }

                log.LogValue("File already exists. New video output filename", videoOutput);
            }

            log.LogValue("Muxed output file", muxedOutput);
            if (File.Exists(muxedOutput) || muxedOutput == videoOutput)
            {
                int counter = 0;
                string directoryname = Path.GetDirectoryName(muxedOutput);
                string filename = Path.GetFileNameWithoutExtension(muxedOutput);
                string extension = Path.GetExtension(muxedOutput);

                while (File.Exists(muxedOutput) || muxedOutput == videoOutput)
                {
                    muxedOutput = Path.Combine(directoryname,
                        filename + "_" + counter + extension);
                    counter++;
                }

                log.LogValue("File already exists. New muxed output filename", muxedOutput);
            }

			for (int i = 0; i < aStreams.Length; i++)
			{
				string name = Path.GetFullPath(aStreams[i].Output);
                log.LogValue("Encodable audio stream " + i, name);
				if (name.Equals(videoOutput) || name.Equals(muxedOutput)) // audio will be overwritten -> no good
				{
					name = Path.Combine(Path.GetDirectoryName(name), Path.GetFileNameWithoutExtension(name) + i.ToString() + Path.GetExtension(name));
					aStreams[i].Output = name;
                    log.LogValue("Stream has the same name as video stream. New audio stream output", name);
				}
			}
            return log;

		}
        #endregion

        #region source checking
        public string checkVideo(string avsFile)
        {
            return checkVideo(avsFile, true);
        }
        
        private string checkVideo(string avsFile, bool tryToFix)
        {
            try
            {
                using (AvsFile avi = AvsFile.OpenScriptFile(avsFile))
                {
                    if (avi.Clip.OriginalColorspace != AviSynthColorspace.YV12)
                    {
                        if (tryToFix && !isConvertedToYV12(avsFile))
                        {
                            bool convert = mainForm.DialogManager.addConvertToYV12(avi.Clip.OriginalColorspace.ToString());
                            if (convert)
                            {
                                if (appendConvertToYV12(avsFile))
                                {
                                    string sResult = checkVideo(avsFile, false); // Check everything again, to see if it is all fixed now
                                    if (sResult == null)
                                    {
                                        MessageBox.Show("Successfully converted to YV12.");
                                        return null;
                                    }
                                    else
                                    {
                                        return sResult;
                                    }
                                }
                            }
                            return "You didn't want me to append ConvertToYV12(). You'll have to fix the colorspace problem yourself.";
                        }
                        return string.Format("AviSynth clip is in {0} not in YV12, even though ConvertToYV12() has been appended.", avi.Clip.OriginalColorspace.ToString());
                    }

                    VideoCodecSettings settings = GetCurrentVideoSettings();

                    if (settings != null && settings.SettingsID != "x264") // mod16 restriction
                    {
                        if (avi.Clip.VideoHeight % 16 != 0 ||
                            avi.Clip.VideoWidth % 16 != 0)
                            return string.Format("AviSynth clip doesn't have mod16 dimensions:\r\nWidth: {0}\r\nHeight:{1}\r\n" +
                                "This could cause problems with some encoders,\r\n" +
                                "and will also result in a loss of compressibility.\r\n" +
                                "I suggest you resize to a mod16 resolution.", avi.Clip.VideoWidth, avi.Clip.VideoHeight);
                    }
                }
            }
            catch (Exception e)
            {
                return "Error in AviSynth script:\r\n" + e.Message;
            }
            return null;
        }

        private bool appendConvertToYV12(string file)
        {
            try
            {
                StreamWriter avsOut = new StreamWriter(file, true);
                avsOut.Write("\r\nConvertToYV12()");
                avsOut.Close();
            }
            catch (IOException)
            {
                return false; 
            }
            return true;
        }

        private bool isConvertedToYV12(string file)
        {
            try
            {
                String strLastLine = "", line = "";
                using (StreamReader reader = new StreamReader(file))
                {
                    while ((line = reader.ReadLine()) != null)
                    {
                        if (!String.IsNullOrEmpty(line))
                            strLastLine = line;
                    }
                }
                if (strLastLine.ToLower().Equals("converttoyv12()"))
                    return true;
                else
                    return false;
            }
            catch
            {
                return false;
            }
        }

        delegate VideoCodecSettings CurrentSettingsDelegate();
        private VideoCodecSettings GetCurrentVideoSettings()
        {
            if (mainForm.InvokeRequired)
                return (VideoCodecSettings)mainForm.Invoke(new CurrentSettingsDelegate(GetCurrentVideoSettings));
            else
                return mainForm.Video.CurrentSettings;
        }
        #endregion

        #region new stuff
        public JobChain GenerateJobSeries(VideoStream video, string muxedOutput, AudioJob[] audioStreams,
            MuxStream[] subtitles, string chapters, FileSize? desiredSize, FileSize? splitSize, ContainerType container, bool prerender, MuxStream[] muxOnlyAudio, LogItem log, string deviceType)
        {
            if (desiredSize.HasValue)
            {
                if (video.Settings.EncodingMode != 4 && video.Settings.EncodingMode != 8) // no automated 2/3 pass
                {
                    if (this.mainForm.Settings.NbPasses == 2)
                        video.Settings.EncodingMode = 4; // automated 2 pass
                    else if (video.Settings.MaxNumberOfPasses == 3)
                        video.Settings.EncodingMode = 8;
                }
            }

            fixFileNameExtensions(video, audioStreams, container);
            string videoOutput = video.Output;
            log.Add(eliminatedDuplicateFilenames(ref videoOutput, ref muxedOutput, audioStreams));
            video.Output = videoOutput;

            JobChain vjobs = jobUtil.prepareVideoJob(video.Input, video.Output, video.Settings, video.DAR, prerender, true, null);

            if (vjobs == null) return null;
            /* Here, we guess the types of the files based on extension.
             * This is guaranteed to work with MeGUI-encoded files, because
             * the extension will always be recognised. For non-MeGUI files,
             * we can only ever hope.*/
            List<MuxStream> allAudioToMux = new List<MuxStream>();
            List<MuxableType> allInputAudioTypes = new List<MuxableType>();
            foreach (MuxStream muxStream in muxOnlyAudio)
            {
                if (VideoUtil.guessAudioMuxableType(muxStream.path, true) != null)
                {
                    allInputAudioTypes.Add(VideoUtil.guessAudioMuxableType(muxStream.path, true));
                    allAudioToMux.Add(muxStream);
                }
            }

            foreach (AudioJob stream in audioStreams)
            {
                allAudioToMux.Add(stream.ToMuxStream());
                allInputAudioTypes.Add(stream.ToMuxableType());
            }


            List<MuxableType> allInputSubtitleTypes = new List<MuxableType>();
            foreach (MuxStream muxStream in subtitles)
                if (VideoUtil.guessSubtitleType(muxStream.path) != null)
                    allInputSubtitleTypes.Add(new MuxableType(VideoUtil.guessSubtitleType(muxStream.path), null));

            MuxableType chapterInputType = null;
            if (!String.IsNullOrEmpty(chapters))
            {
                ChapterType type = VideoUtil.guessChapterType(chapters);
                if (type != null)
                    chapterInputType = new MuxableType(type, null);
            }

            MuxableType deviceOutputType = null;
            if (!String.IsNullOrEmpty(deviceType))
            {
                DeviceType type = VideoUtil.guessDeviceType(deviceType);
                if (type != null)
                    deviceOutputType = new MuxableType(type, null);
            }

            List<string> inputsToDelete = new List<string>();
            inputsToDelete.Add(video.Output);
            inputsToDelete.AddRange(Array.ConvertAll<AudioJob, string>(audioStreams, delegate(AudioJob a) { return a.Output; }));

            JobChain muxJobs = this.jobUtil.GenerateMuxJobs(video, video.Framerate, allAudioToMux.ToArray(), allInputAudioTypes.ToArray(),
                subtitles, allInputSubtitleTypes.ToArray(), chapters, chapterInputType, container, muxedOutput, splitSize, inputsToDelete, deviceType, deviceOutputType);

            if (desiredSize.HasValue)
            {
                BitrateCalculationInfo b = new BitrateCalculationInfo();
                
                List<string> audiofiles = new List<string>();
                foreach (MuxStream s in allAudioToMux)
                    audiofiles.Add(s.path);
                b.AudioFiles = audiofiles;

                b.Container = container;
                b.VideoJobs = new List<TaggedJob>(vjobs.Jobs);
                b.DesiredSize = desiredSize.Value;
                ((VideoJob)vjobs.Jobs[0].Job).BitrateCalculationInfo = b;
            }

            return 
                new SequentialChain(
                    new ParallelChain((Job[])audioStreams),
                    new SequentialChain(vjobs),
                    new SequentialChain(muxJobs));
        }

        private void fixFileNameExtensions(VideoStream video, AudioJob[] audioStreams, ContainerType container)
        {
            AudioEncoderType[] audioCodecs = new AudioEncoderType[audioStreams.Length];
            for (int i = 0; i < audioStreams.Length; i++)
            {
                audioCodecs[i] = audioStreams[i].Settings.EncoderType;
            }
            MuxPath path = mainForm.MuxProvider.GetMuxPath(video.Settings.EncoderType, audioCodecs, container);
            if (path == null)
                return;
            List<AudioType> audioTypes = new List<AudioType>();
            foreach (MuxableType type in path.InitialInputTypes)
            {
                if (type.outputType is VideoType)
                {
                    // see http://forum.doom9.org/showthread.php?p=1243370#post1243370
                    if ((mainForm.Settings.ForceRawAVCExtension) && (video.Settings.EncoderType == VideoEncoderType.X264))
                         video.Output = Path.ChangeExtension(video.Output, ".264");
                    else video.Output = Path.ChangeExtension(video.Output, type.outputType.Extension);
                    video.VideoType = type;
                }
                if (type.outputType is AudioType)
                {
                    audioTypes.Add((AudioType)type.outputType);
                }
            }
            AudioEncoderProvider aProvider = new AudioEncoderProvider();
            for (int i = 0; i < audioStreams.Length; i++)
            {
                AudioType[] types = aProvider.GetSupportedOutput(audioStreams[i].Settings.EncoderType);
                foreach (AudioType type in types)
                {
                    if (audioTypes.Contains(type))
                    {
                        audioStreams[i].Output = Path.ChangeExtension(audioStreams[i].Output,
                            type.Extension);
                        break;
                    }
                }
            }
        }

        public static SubtitleType guessSubtitleType(string p)
        {
            foreach (SubtitleType type in ContainerManager.SubtitleTypes.Values)
            {
                if (Path.GetExtension(p.ToLower()) == "." + type.Extension)
                    return type;
            }
            return null;
        }

        public static VideoType guessVideoType(string p)
        {
            foreach (VideoType type in ContainerManager.VideoTypes.Values)
            {
                if (Path.GetExtension(p.ToLower()) == "." + type.Extension)
                    return type;
            }
            return null;
        }
 
        public static AudioType guessAudioType(string p)
        {
            foreach (AudioType type in ContainerManager.AudioTypes.Values)
            {
                if (Path.GetExtension(p.ToLower()) == "." + type.Extension)
                    return type;
            }
            return null;
        }

        public static ChapterType guessChapterType(string p)
        {
            foreach (ChapterType type in ContainerManager.ChapterTypes.Values)
            {
                if (Path.GetExtension(p.ToLower()) == "." + type.Extension)
                    return type;
            }
            return null;
        }

        public static DeviceType guessDeviceType(string p)
        {
            foreach (DeviceType type in ContainerManager.DeviceTypes.Values)
            {
                if (p == type.Extension)
                    return type;
            }
            return null;
        }

        public static MuxableType guessVideoMuxableType(string p, bool useMediaInfo)
        {
            if (string.IsNullOrEmpty(p))
                return null;
            if (useMediaInfo)
            {
                MediaInfoFile info = new MediaInfoFile(p);
                if (info.Info.HasVideo)
                    return new MuxableType(info.VideoType, info.VCodec);
                // otherwise we may as well try the other route too
            }
            VideoType vType = guessVideoType(p);
            if (vType != null)
            {
                if (vType.SupportedCodecs.Length == 1)
                    return new MuxableType(vType, vType.SupportedCodecs[0]);
                else
                    return new MuxableType(vType, null);
            }
            return null;
        }

        public static MuxableType guessAudioMuxableType(string p, bool useMediaInfo)
        {
            if (string.IsNullOrEmpty(p))
                return null;
            if (useMediaInfo)
            {
                MediaInfoFile info = new MediaInfoFile(p);
                if (info.AudioType != null)
                    return new MuxableType(info.AudioType, info.ACodecs[0]);
            }
            AudioType aType = guessAudioType(p);
            if (aType != null)
            {
                if (aType.SupportedCodecs.Length == 1)
                    return new MuxableType(aType, aType.SupportedCodecs[0]);
                else
                    return new MuxableType(aType, null);
            }
            return null;
        }
        #endregion

        public static string createSimpleAvisynthScript(string filename)
        {
            PossibleSources sourceType = PossibleSources.directShow;
            if (filename.ToLower().EndsWith(".vdr"))
                sourceType = PossibleSources.vdr;
            string outputFile = filename + ".avs";
            if (File.Exists(outputFile))
            {
                DialogResult response = MessageBox.Show("The file, '" + outputFile + "' already exists.\r\n Do you want to overwrite it?",
                    "File already exists", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                if (response == DialogResult.No)
                    return null;
            }
            try
            {
                StreamWriter output = new StreamWriter(outputFile);
                output.WriteLine(
                    ScriptServer.GetInputLine(filename, false, sourceType, false, false, false, -1, false));
                output.Close();
            }
            catch (IOException)
            {
                return null;
            }
            return outputFile;
        }

        public static string convertChaptersTextFileTox264QPFile(string filename, double framerate)
        {
            StreamWriter sw = null;
            string qpfile = "";
            if (File.Exists(filename))
            {
                StreamReader sr = null;
                string line = null;
                qpfile = Path.ChangeExtension(filename, ".qpf");
                sw = new StreamWriter(qpfile, false, System.Text.Encoding.Default);
                try
                {
                    sr = new StreamReader(filename);
                    Chapter chap = new Chapter();
                    while ((line = sr.ReadLine()) != null)
                    {
                        if (line.IndexOf("NAME") == -1) // chapter time
                        {
                            string tc = line.Substring(line.IndexOf("=") + 1);
                            chap.timecode = tc;
                            int chapTime = Util.getTimeCode(chap.timecode);
                            int frameNumber = Util.convertTimecodeToFrameNumber(chapTime, framerate);
                            sw.WriteLine(frameNumber.ToString() + " I -1");
                        }
                    }

                }
                catch (Exception f)
                {
                    MessageBox.Show(f.Message);
                }
                finally
                {
                    if (sw != null)
                    {
                        try
                        {
                            sw.Close();
                        }
                        catch (Exception f)
                        {
                            MessageBox.Show(f.Message);
                        }
                    }
                }                
            }
            return qpfile;
        }

        public static string GetNameForNth(int n)
        {
            switch (n)
            {
                case 0:
                    return "zeroth";
                case 1:
                    return "first";
                case 2:
                    return "second";
                case 3:
                    return "third";
                case 4:
                    return "fourth";
                case 5:
                    return "fifth";
            }
            string number = n.ToString();
            if (number.EndsWith("1"))
                return number + "st";
            if (number.EndsWith("2"))
                return number + "nd";
            if (number.EndsWith("3"))
                return number + "rd";
            return number + "th";
        }

        public static string GenerateCombinedFilter(OutputFileType[] types)
        {
            StringBuilder initialFilterName = new StringBuilder();
            StringBuilder initialFilter = new StringBuilder();
            StringBuilder allSmallFilters = new StringBuilder();
            initialFilterName.Append("All supported files (");
            foreach (OutputFileType type in types)
            {
                initialFilter.Append(type.OutputFilter);
                initialFilter.Append(";");
                initialFilterName.Append(type.OutputFilter);
                initialFilterName.Append(", ");
                allSmallFilters.Append(type.OutputFilterString);
                allSmallFilters.Append("|");
            }

            string initialFilterTrimmed = initialFilterName.ToString().TrimEnd(' ', ',') + ")|" +
                initialFilter.ToString();

            if (types.Length > 1)
                return initialFilterTrimmed + "|" + allSmallFilters.ToString().TrimEnd('|');
            else
                return allSmallFilters.ToString().TrimEnd('|');
        }

        public static void getAvisynthVersion(out string FileVersion, out bool PropExists)
        {
            string systempath = Environment.GetFolderPath(Environment.SpecialFolder.System);

            if (File.Exists(systempath + "\\avisynth.dll"))
            {
                FileVersionInfo FileProperties = FileVersionInfo.GetVersionInfo(systempath + "\\avisynth.dll");
                FileVersion = FileProperties.FileVersion;
                PropExists = true;
            }
            else
            {
                // on x64, try the SysWOW64 folder
                string syswow64path = Environment.GetFolderPath(Environment.SpecialFolder.System).Substring(0, 11) + "\\SysWOW64";
                if (Directory.Exists(syswow64path))
                {
                    if (File.Exists(syswow64path + "\\avisynth.dll"))
                    {
                        FileVersionInfo FileProperties = FileVersionInfo.GetVersionInfo(syswow64path + "\\avisynth.dll");
                        FileVersion = FileProperties.FileVersion;
                        PropExists = true;
                    }
                    else
                    {
                        FileVersion = string.Empty;
                        PropExists = false;
                    }
                }
                else
                {
                    FileVersion = string.Empty;
                    PropExists = false;
                }
            }
        }
    }
	#region helper structs
	/// <summary>
	/// helper structure for cropping
	/// holds the crop values for all 4 edges of a frame
	/// </summary>
	[LogByMembers]
    public sealed class CropValues
	{
		public int left, top, right, bottom;
        public CropValues Clone()
        {
            return (CropValues)this.MemberwiseClone();
        }
	}
    public class AudioTrackInfo
    {
        private string nbChannels, type, samplingRate, containerType, description;
        private int index, trackID;
        public AudioTrackInfo() :this(null, null, null, 0)
        {
        }
        public AudioTrackInfo(string language, string nbChannels, string type, int trackID)
        {
            TrackInfo = new TrackInfo(language, null);
            this.nbChannels = nbChannels;
            this.type = type;
            this.trackID = trackID;
        }

        public string Language
        {
            get
            {
                if (TrackInfo == null) return null;
                return TrackInfo.Language;
            }
            set
            {
                if (TrackInfo == null)
                    TrackInfo = new TrackInfo();
                TrackInfo.Language = value;
            }
        }

        public TrackInfo TrackInfo;
        public string TrackIDx
        {
            get { return containerType == "MPEG-TS" ? trackID.ToString("x3") : trackID.ToString("x"); }
            set { trackID = Int32.Parse(value, System.Globalization.NumberStyles.HexNumber); }
        }
        public int TrackID
        {
            get { return trackID; }
            set { trackID = value; }
        }
        public string DgIndexID
        {
            get { return containerType == "MPEG-TS" ? index.ToString() : TrackIDx; }
        }
        public string ContainerType
        {
            get { return containerType; }
            set { containerType = value; }
        }
        public int Index
        {
            get { return index; }
            set { index = value; }
        }
        public string Description
        {
            get { return description; }
            set { description = value; }
        }
        public string Type
        {
            get { return type; }
            set { type = value; }
        }

        public string NbChannels
        {
            get { return nbChannels; }
            set { nbChannels = value; }
        }

        public string SamplingRate
        {
            get { return samplingRate; }
            set { samplingRate = value; }
        }

        public override string ToString()
        {
            string fullString = "[" + TrackIDx + "] - " + this.type;
            if (!string.IsNullOrEmpty(nbChannels))
            {
                fullString += " - " + this.nbChannels;
            }
            if (!string.IsNullOrEmpty(samplingRate))
            {
                fullString += " / " + samplingRate;
            }
            if (!string.IsNullOrEmpty(TrackInfo.Language))
            {
                fullString += " / " + TrackInfo.Language;
            }
            return fullString.Trim();
        }
    }
    public class SubtitleInfo
    {
        private string name;
        private int index;
        public SubtitleInfo(string name, int index)
        {
            this.name = name;
            this.index = index;
        }
        public string Name
        {
            get { return name; }
            set { name = value; }
        }
        public int Index
        {
            get { return index; }
            set { index = value; }
        }
        public override string ToString()
        {
            string fullString = "[" + this.index.ToString("D2") + "] - " + this.name;
            return fullString.Trim();
        }
    }
	#endregion
}
