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
using System.ComponentModel;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;

using ICSharpCode.SharpZipLib.Zip;

using MeGUI.core.util;


namespace MeGUI
{
    class UpdateCacher
    {
        public static void flushOldCachedFilesAsync(List<string> urls)
        {
            string updateCache = MainForm.Instance.Settings.MeGUIUpdateCache;
            if (string.IsNullOrEmpty(updateCache)
                || !Directory.Exists(updateCache))
                return;

            DirectoryInfo fi = new DirectoryInfo(updateCache);
            FileInfo[] files = fi.GetFiles();

            for (int i = 0; i < urls.Count; ++i)
            {
                urls[i] = urls[i].ToLower();
            }

            foreach (FileInfo f in files)
            {
                if (urls.IndexOf(f.Name.ToLower()) < 0)
                {
                    if (DateTime.Now - f.LastWriteTime > new TimeSpan(7, 0, 0, 0, 0))
                        f.Delete();
                }
            }
        }

        private static void ensureSensibleCacheFolderExists()
        {
            FileUtil.ensureDirectoryExists(MainForm.Instance.Settings.MeGUIUpdateCache);
        }

        public static UpdateWindow.ErrorState DownloadFile(string url, Uri serverAddress,
            out Stream str, DownloadProgressChangedEventHandler wc_DownloadProgressChanged)
        {
            ensureSensibleCacheFolderExists();
            UpdateWindow.ErrorState er = UpdateWindow.ErrorState.Successful;
            string updateCache = MainForm.Instance.Settings.MeGUIUpdateCache;

            string localFilename = Path.Combine(updateCache, url);
            FileInfo finfo = new FileInfo(localFilename);
            if (File.Exists(localFilename) && (finfo.Length == 0))
            {
                try
                {
                    finfo.Delete();
                }
                catch (IOException) { }
            }
            else if (File.Exists(localFilename))
            {
                // check the zip file
                if (localFilename.ToLower().EndsWith(".zip"))
                {
                    try
                    {
                        ZipFile zipFile = new ZipFile(localFilename);
                        if (zipFile.TestArchive(true) == false)
                        {
                            try
                            {
                                finfo.Delete();
                            }
                            catch (IOException) { }
                        }
                        else
                        {
                            goto gotLocalFile;
                        }
                    }
                    catch
                    {
                        try
                        {
                            finfo.Delete();
                        }
                        catch (IOException) { }
                    }
                }
                else
                {
                    goto gotLocalFile;
                }
            }

            WebClient wc = new WebClient();

            // check for proxy authentication...
            if (MainForm.Instance.Settings.UseHttpProxy == true)
            {
                WebProxy wprox = null;
                ICredentials icred = null;

                if (MainForm.Instance.Settings.HttpProxyUid != null)
                {
                    icred = new NetworkCredential(MainForm.Instance.Settings.HttpProxyUid, MainForm.Instance.Settings.HttpProxyPwd);
                }

                wprox = new WebProxy(MainForm.Instance.Settings.HttpProxyAddress + ":" + MainForm.Instance.Settings.HttpProxyPort, true, null, icred);

                WebRequest.DefaultWebProxy = wprox;
                wc.Proxy = wprox;
            }
            else
            {
                wc.Proxy = null;
            }

            ManualResetEvent mre = new ManualResetEvent(false);
            wc.DownloadFileCompleted += delegate(object sender, AsyncCompletedEventArgs e)
            {
                if (e.Error != null)
                    er = UpdateWindow.ErrorState.CouldNotDownloadFile;

                mre.Set();
            };

            wc.DownloadProgressChanged += wc_DownloadProgressChanged;

            wc.DownloadFileAsync(new Uri(serverAddress, url), localFilename);
            mre.WaitOne();

        gotLocalFile:
            try
            {
                str = File.OpenRead(localFilename);
            }
            catch (IOException)
            {
                str = null;
                return UpdateWindow.ErrorState.CouldNotDownloadFile;
            }

            return er;
        }

        public static void FlushFile(string p)
        {
            string localFilename = Path.Combine(MainForm.Instance.Settings.MeGUIUpdateCache, p);
            try
            {
                File.Delete(localFilename);
            }
            catch (IOException) { }
        }
    }
}