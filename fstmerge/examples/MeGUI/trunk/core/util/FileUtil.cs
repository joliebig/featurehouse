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
using System.IO;
using System.Text;
using System.Windows.Forms;

using ICSharpCode.SharpZipLib.Zip;

namespace MeGUI.core.util
{
    delegate bool FileExists(string filename);

    class FileUtil
    {
        public static DirectoryInfo CreateTempDirectory()
        {
            while (true)
            {
                string file = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());

                if (!File.Exists(file) && !Directory.Exists(file))
                {
                    MainForm.Instance.DeleteOnClosing(file);
                    return Directory.CreateDirectory(file);
                }
            }
        }

        public static void CreateZipFile(string path, string filename)
        {
            using (ZipOutputStream outputFile = new ZipOutputStream(File.OpenWrite(filename)))
            {
                foreach (string file in FileUtil.AllFiles(path))
                {
                    ZipEntry newEntry = new ZipEntry(file.Substring(path.Length).TrimStart('\\', '/'));
                    outputFile.PutNextEntry(newEntry);
                    FileStream input = File.OpenRead(file);
                    FileUtil.copyData(input, outputFile);
                    input.Close();
                }
            }
        }

        public static void ExtractZipFile(string file, string extractFolder)
        {
            ExtractZipFile(File.OpenRead(file), extractFolder);
        }

        public static void ExtractZipFile(Stream s, string extractFolder)
        {
            using (ZipFile inputFile = new ZipFile(s))
            {
                foreach (ZipEntry entry in inputFile)
                {
                    string pathname = Path.Combine(extractFolder, entry.Name);
                    if (entry.IsDirectory)
                    {
                        Directory.CreateDirectory(pathname);
                    }
                    else // entry.isFile
                    {
                        System.Diagnostics.Debug.Assert(entry.IsFile);
                        FileUtil.ensureDirectoryExists(Path.GetDirectoryName(pathname));
                        Stream outputStream = File.OpenWrite(pathname);
                        FileUtil.copyData(inputFile.GetInputStream(entry), outputStream);
                        outputStream.Close();
                        File.SetLastWriteTime(pathname, entry.DateTime);
                    }
                }
            }
        }

        public static void DeleteDirectoryIfExists(string p, bool recursive)
        {
            if (Directory.Exists(p))
                Directory.Delete(p, recursive);
        }


        public static DirectoryInfo ensureDirectoryExists(string p)
        {
            if (Directory.Exists(p)) return new DirectoryInfo(p);
            if (string.IsNullOrEmpty(p)) throw new IOException("Can't create directory");
            ensureDirectoryExists(Path.GetDirectoryName(p));
            System.Threading.Thread.Sleep(100);
            return Directory.CreateDirectory(p);
        }
        /// <summary>
        /// Generates a filename not in the list
        /// </summary>
        /// <param name="original"></param>
        /// <param name="filenames"></param>
        /// <returns></returns>
        public static string getUniqueFilename(string original, List<string> filenames)
        {
            return getUniqueFilename(original, new FileExists(delegate(string test)
            {
                return filenames.Contains(test);
            }));
        }

        /// <summary>
        /// Generates a unique filename by adding numbers to the filename.
        /// </summary>
        /// <param name="original"></param>
        /// <param name="fileExists"></param>
        /// <returns></returns>
        public static string getUniqueFilename(string original, FileExists fileExists)
        {
            if (!fileExists(original)) return original;
            string prefix = Path.Combine(Path.GetDirectoryName(original),
                Path.GetFileNameWithoutExtension(original)) + "_";
            string suffix = Path.GetExtension(original);
            for (int i = 0; true; i++)
            {
                string filename = prefix + i + suffix;
                if (!fileExists(filename)) return filename;
            }
        }

        public static List<string> AllFiles(string folder)
        {
            List<string> list = new List<string>();
            AddFiles(folder, list);
            return list;
        }

        private static void AddFiles(string folder, List<string> list)
        {
            list.AddRange(Directory.GetFiles(folder));
            foreach (string subFolder in Directory.GetDirectories(folder))
                AddFiles(subFolder, list);
        }

        private const int BUFFER_SIZE = 2 * 1024 * 1024; // 2 MBs
        public static void copyData(Stream input, Stream output)
        {
            int count = -1;
            byte[] data = new byte[BUFFER_SIZE];
            while ((count = input.Read(data, 0, BUFFER_SIZE)) > 0)
            {
                output.Write(data, 0, count);
            }
        }

        /// <summary>
        /// Returns the full path and filename, but without the extension
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        public static string GetPathWithoutExtension(string path)
        {
            return Path.Combine(Path.GetDirectoryName(path), Path.GetFileNameWithoutExtension(path));
        }

        /// <summary>
        /// Returns TimeSpan value formatted
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        public static string ToShortString(TimeSpan ts)
        {
            string time;
            time = ts.Hours.ToString("00");
            time = time + ":" + ts.Minutes.ToString("00");
            time = time + ":" + ts.Seconds.ToString("00");
            time = time + "." + ts.Milliseconds.ToString("000");
            return time;
        }

        /// <summary>
        /// Adds extra to the filename, modifying the filename but keeping the extension and folder the same.
        /// </summary>
        /// <param name="filename"></param>
        /// <param name="extra"></param>
        /// <returns></returns>
        public static string AddToFileName(string filename, string extra)
        {
            return Path.Combine(
                Path.GetDirectoryName(filename),
                Path.GetFileNameWithoutExtension(filename) + extra + Path.GetExtension(filename));
        }

        /// <summary>
        /// Returns true if the filename matches the filter specified. The format
        /// of the filter is the same as that of a FileDialog.
        /// </summary>
        /// <param name="filter"></param>
        /// <param name="filename"></param>
        /// <returns></returns>
        public static bool MatchesFilter(string filter, string filename)
        {
            if (string.IsNullOrEmpty(filter))
                return true;

            filter = filter.ToLower();
            filename = Path.GetFileName(filename).ToLower();
            string[] filters = filter.Split('|');
            
            for (int i = 1; i < filters.Length; i += 2)
            {
                string[] iFilters = filters[i].Split(';');
                foreach (string f in iFilters)
                {
                    if (f.IndexOf('*') > -1)
                    {
                        if (!f.StartsWith("*."))
                            throw new Exception("Invalid filter format");

                        if (f == "*.*" && filename.IndexOf('.') > -1)
                            return true;

                        string extension = f.Substring(1);
                        if (filename.EndsWith(extension))
                            return true;
                    }
                    else if (f == filename)
                        return true;
                    else return false;

                }
            }

            return false;
        }

        /// <summary>
        /// Copy File
        /// </summary>
        /// <param name"sourcePath">Path of the Source file</param>
        /// <param name"targetPath">Path of the Target File</param>
        /// <param name"targetName">Name of the Target file</param>
        /// <param name="overwrite"></param>
        public static void CopyFile(string sourcePath, string targetPath, string targetName, bool overwrite)
        {
            if (Directory.Exists(sourcePath))
            {
                string[] files =Directory.GetFiles(sourcePath);

                foreach (string s in files)
                {
                    // Use static Path methods to extract only the file name from the path.
                    string fileName = Path.GetFileName(s);
                    if (fileName == targetName)
                    {
                        string destFile = Path.Combine(targetPath, fileName);
                        File.Copy(s, destFile, overwrite);
                    }
                }
            }
            else
            {
                Console.WriteLine("Source path does not exist!");
            }
        }

        /// <summary>
        /// Backup File
        /// </summary>
        /// <param name"sourcePath">Path of the Source file</param>
        /// <param name="overwrite"></param>
        public static void BackupFile(string sourcePath, bool overwrite)
        {
            try
            {
                if (File.Exists(sourcePath))
                {
                    String targetPath = sourcePath.Replace(System.Windows.Forms.Application.StartupPath, System.Windows.Forms.Application.StartupPath + @"\backup");
                    if (File.Exists(targetPath))
                        File.Delete(targetPath);

                    FileUtil.ensureDirectoryExists(Path.GetDirectoryName(targetPath));

                    File.Move(sourcePath,targetPath);
                }
            } catch (Exception ex)
            {
                MessageBox.Show("Error while moving file: \n" + sourcePath + "\n" + ex.Message, "Error moving file", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        /// <summary>
        /// Checks if a directory is writable
        /// </summary>
        /// <param name"strPath">Path of the Source file</param>
        public static bool IsDirWriteable(string strPath)
        {
            try
            {
                // Does the root directory exists
                if (!Directory.Exists(strPath))
                    return false;

                // Create a new file name
                string newFileName = System.IO.Path.GetRandomFileName();

                // Combine the new file name with the path
                string newPath = System.IO.Path.Combine(strPath, newFileName);

                // Create & delete the file
                if (!System.IO.File.Exists(newPath))
                {
                    System.IO.FileStream fs = System.IO.File.Create(newPath);
                    fs.Close();
                    System.IO.File.Delete(newPath);
                    return true; 
                }

                return false;
            }
            catch
            {
                return false;
            }
        }
    }
}
