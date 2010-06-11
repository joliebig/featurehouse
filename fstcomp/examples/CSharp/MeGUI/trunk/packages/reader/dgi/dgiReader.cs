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
using System.Drawing;
using System.IO;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using System.Text.RegularExpressions;

using MeGUI.core.util;

namespace MeGUI
{
    public class dgiFileFactory : IMediaFileFactory
    {

        #region IMediaFileFactory Members

        public IMediaFile Open(string file)
        {
            return new dgiFile(file);
        }

        public int HandleLevel(string file)
        {
            if (file.ToLower().EndsWith(".dgi"))
                return 12;
            return -1;
        }

        #endregion

        #region IIDable Members

        public string ID
        {
            get { return "dgi"; }
        }

        #endregion
    }

    /// <summary>
    /// Summary description for dgiReader.
    /// </summary>
    public class dgiFile : IMediaFile
    {
        private AvsFile reader;
        private string fileName;
        private MediaFileInfo info;

        /// <summary>
        /// initializes the dgi reader
        /// </summary>
        /// <param name="fileName">the DGNVIndex project file that this reader will process</param>
        public dgiFile(string fileName)
        {
            this.fileName = fileName;

            string strScript = "";
            string strPath = Path.GetDirectoryName(MainForm.Instance.Settings.DgnvIndexPath);
            if (MainForm.Instance.Settings.UseCUVIDserver == true)
            {
                string strDLL = Path.Combine(strPath, "DGDecodeNV.dll");
                strScript = "LoadPlugin(\"" + strDLL + "\")\r\nDGSource(\"" + this.fileName + "\"";
            }
            else
            {
                string strDLL = Path.Combine(strPath, "DGMultiDecodeNV.dll");
                strScript = "LoadPlugin(\"" + strDLL + "\")\r\nDGMultiSource(\"" + this.fileName + "\"";
            }
            if (MainForm.Instance.Settings.AutoForceFilm &&
                        MainForm.Instance.Settings.ForceFilmThreshold <= (decimal)dgiFile.GetFilmPercent(this.fileName))
                strScript += ",fieldop=1)";
            else
                strScript += ",fieldop=0)";

            reader = AvsFile.ParseScript(strScript);

            this.readFileProperties();
        }

        private static readonly Regex r =
            new Regex("[0-9.]+(?=% FILM)");

        public static double GetFilmPercent(string file)
        {
            double filmPercentage = -1.0;

            if (String.IsNullOrEmpty(file))
                return filmPercentage;

            using (StreamReader sr = new StreamReader(file))
            {
                string line = sr.ReadLine();
                while ((line = sr.ReadLine()) != null)
                    if (r.IsMatch(line))
                        filmPercentage = double.Parse(r.Match(line).Value,
                            System.Globalization.CultureInfo.InvariantCulture);
            }
            return filmPercentage;
        }

        /// <summary>
        /// reads the dgi file, which is essentially a text file
        /// </summary>
        private void readFileProperties()
        {
            info = reader.Info.Clone();
            Dar dar = Dar.A1x1;
            using (StreamReader sr = new StreamReader(fileName))
            {
                string line = null;
                int iLineCount = 0;
                while ((line = sr.ReadLine()) != null)
                {
                    if (iLineCount == 3)
                    {
                        string strSourceFile = line.Substring(0, line.LastIndexOf(" "));
                        if (File.Exists(strSourceFile))
                        {
                            int unused2;
                            System.Collections.Generic.List<AudioTrackInfo> unused1;
                            Dar? dar1;
                            VideoUtil vUtil = new VideoUtil(MainForm.Instance);
                            vUtil.getSourceMediaInfo(strSourceFile, out unused1, out unused2, out dar1);
                            dar = (Dar)dar1;
                        }
                        else
                        {
                            dar = new Dar(reader.Info.Width, reader.Info.Height);
                        }
                        break;
                    }
                    iLineCount++;
                }
            }
            info.DAR = dar;
        }
        #region properties
        public MediaFileInfo Info
        {
            get { return info; }
        }
        #endregion

        #region IMediaFile Members

        public bool CanReadVideo
        {
            get { return reader.CanReadVideo; }
        }

        public bool CanReadAudio
        {
            get { return false; }
        }

        public IVideoReader GetVideoReader()
        {
            return reader.GetVideoReader();
        }

        public IAudioReader GetAudioReader(int track)
        {
            throw new Exception("The method or operation is not implemented.");
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            reader.Dispose();
        }

        #endregion
    }
}

