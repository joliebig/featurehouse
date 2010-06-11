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
using System.Data;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using System.Xml;

using MeGUI.core.util;

namespace MeGUI.core.gui
{
    public partial class ProfileExporter : MeGUI.core.gui.ProfilePorter
    {
        private MainForm mainForm;
        private DirectoryInfo tempFolder;
        private XmlDocument ContextHelp = new XmlDocument();

        public ProfileExporter(MainForm mainForm)
        {
            InitializeComponent();
            this.mainForm = mainForm;

            Profiles = mainForm.Profiles.AllProfiles;
        }

        private List<string> getRequiredFiles(List<Profile> ps)
        {
            List<string> files = new List<string>();

            foreach (Profile p in ps)
                files.AddRange(p.BaseSettings.RequiredFiles);

            return Util.Unique(files);
        }



        private void export_Click(object sender, EventArgs e)
        {
            Util.CatchExceptionsAndTellUser("An error occurred when saving the file", delegate
            {
                try
                {
                    string filename = askForFilename();

                    tempFolder = FileUtil.CreateTempDirectory();

                    List<Profile> profs = SelectedAndRequiredProfiles;
                    Dictionary<string, string> subTable =
                        copyExtraFilesToFolder(getRequiredFiles(profs),
                        Path.Combine(tempFolder.FullName, "extra"));

                    subTable = turnValuesToZippedStyleName(subTable);

                    fixFileNames(profs, subTable);

                    ProfileManager.WriteProfiles(tempFolder.FullName, profs);

                    FileUtil.CreateZipFile(tempFolder.FullName, filename);

                    DialogResult = DialogResult.OK;
                    MessageBox.Show("Completed successfully", "Export completed successfully", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
                catch (CancelledException)
                {
                    DialogResult = DialogResult.Cancel;
                }
            });
        }

        private Dictionary<string, string> turnValuesToZippedStyleName(Dictionary<string, string> subTable)
        {
            Dictionary<string, string> newTable = new Dictionary<string, string>();
            foreach (string key in subTable.Keys)
                newTable[key] = getZippedExtraFileName(subTable[key]);
            return newTable;
        }

        private static string askForFilename()
        {
            SaveFileDialog outputFilesChooser = new SaveFileDialog();
            outputFilesChooser.Title = "Choose your output file";
            outputFilesChooser.Filter = "Zip archives|*.zip";
            if (outputFilesChooser.ShowDialog() != DialogResult.OK)
                throw new CancelledException();

            return outputFilesChooser.FileName;
        }

        private string SelectHelpText(string node)
        {
            StringBuilder HelpText = new StringBuilder(64);

            string xpath = "/ContextHelp/Form[@name='PresetExporter']/" + node;
            XmlNodeList nl = ContextHelp.SelectNodes(xpath); // Return the details for the specified node

            if (nl.Count == 1) // if it finds the required HelpText, count should be 1
            {
                HelpText.AppendLine(nl[0]["Basic"].InnerText);
                HelpText.AppendLine();
            }
            else // If count isn't 1, then theres no valid data.
                HelpText.Append("Error: No data available");

            return (HelpText.ToString());
        }

        private void SetToolTips()
        {
            PresetExporterToolTip.SetToolTip(this.profileList, SelectHelpText("presetList"));
        }

        private void ProfileExporter_Shown(object sender, EventArgs e)
        {
            try
            {
                string p = System.IO.Path.Combine(Application.StartupPath, "Data");
                p = System.IO.Path.Combine(p, "ContextHelp.xml");
                ContextHelp.Load(p);
                SetToolTips();
            }
            catch
            {
                MessageBox.Show("The ContextHelp.xml file could not be found. Please check in the 'Data' directory to see if it exists. Help tooltips will not be available.", "File Not Found", MessageBoxButtons.OK);
            }
        }

        private void checkAllToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ToolStripMenuItem ts = (ToolStripMenuItem)sender;
            for (int i = 0; i < profileList.Items.Count; i++)
            {
                profileList.SetItemChecked(i, true);
            } 
        }

        private void checkNoneToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ToolStripMenuItem ts = (ToolStripMenuItem)sender;
            for (int i = 0; i < profileList.Items.Count; i++)
            {
                profileList.SetItemChecked(i, false);
            } 
        }
    }
}


