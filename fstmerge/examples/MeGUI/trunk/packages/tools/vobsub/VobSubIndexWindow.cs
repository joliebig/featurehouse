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

using MeGUI.core.plugins.interfaces;
using MeGUI.core.util;

namespace MeGUI
{
    public partial class VobSubIndexWindow : Form
    {
        
        private bool dialogMode = false;
        private bool configured = false;
        private MainForm mainForm;
        private VideoUtil vUtil;
        private JobUtil jobUtil;
        
        
        public VobSubIndexWindow(MainForm mainForm)
        {
            InitializeComponent();
            this.mainForm = mainForm;
            this.vUtil = new VideoUtil(mainForm);
            this.jobUtil = new JobUtil(mainForm);
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            base.OnClosing(e);
        }
        
        
        private void queueButton_Click(object sender, EventArgs e)
        {
            if (Drives.ableToWriteOnThisDrive(Path.GetPathRoot(output.Filename)))
            {
                if (configured)
                {
                    if (!dialogMode)
                    {
                        SubtitleIndexJob job = generateJob();
                        mainForm.Jobs.addJobsToQueue(job);
                        if (this.closeOnQueue.Checked)
                            this.Close();
                    }
                }
                else
                    MessageBox.Show("You must select an Input and Output file to continue",
                        "Configuration incomplete", MessageBoxButtons.OK);
            }
            else
                MessageBox.Show("MeGUI cannot write on " + Path.GetPathRoot(output.Filename) +
                                ". Please, select another output path.", "Configuration Incomplete", MessageBoxButtons.OK);
        }
        
        private void openVideo(string fileName)
        {
            input.Filename = fileName;
            subtitleTracks.Items.Clear();
            uint nbPGC = IFOparser.getPGCnb(fileName);
            pgc.Maximum = nbPGC;
            subtitleTracks.Items.AddRange(IFOparser.GetSubtitlesStreamsInfos(input.Filename));
            demuxSelectedTracks.Checked = !keepAllTracks.Checked;
        }
        private void checkIndexIO()
        {
            configured = (!input.Filename.Equals("") && !output.Filename.Equals(""));
            if (configured && dialogMode)
                queueButton.DialogResult = DialogResult.OK;
            else
                queueButton.DialogResult = DialogResult.None;
        }

        private SubtitleIndexJob generateJob()
        {
            List<int> trackIDs = new List<int>();
            foreach (string s in subtitleTracks.CheckedItems)
            {
                trackIDs.Add(Int32.Parse(s.Substring(1,2)));
            }
            return new SubtitleIndexJob(input.Filename, output.Filename, keepAllTracks.Checked, trackIDs, (int)pgc.Value);
        }
        public void setConfig(string input, string output, bool indexAllTracks, List<int> trackIDs, int pgc)
        {
            this.dialogMode = true;
            queueButton.Text = "Update";
            this.input.Filename = input;
            openVideo(input);
            this.output.Filename = output;
            checkIndexIO();
            if (indexAllTracks)
                keepAllTracks.Checked = true;
            else
            {
                demuxSelectedTracks.Checked = true;
                int index = 0;
                List<int> checkedItems = new List<int>();
                foreach (object item in subtitleTracks.Items)
                {
                    SubtitleInfo si = (SubtitleInfo)item;
                    if (trackIDs.Contains(si.Index))
                        checkedItems.Add(index);
                    index++;
                }
                foreach (int idx in checkedItems)
                {
                    subtitleTracks.SetItemChecked(idx, true);
                }
            }
            this.pgc.Value = pgc;
        }
        /// <summary>
        /// gets the index job created from the current configuration
        /// </summary>
        public SubtitleIndexJob Job
        {
            get { return generateJob(); }
        }

        private void input_FileSelected(FileBar sender, FileBarEventArgs args)
        {
            openVideo(input.Filename);
            output.Filename = Path.ChangeExtension(input.Filename, ".idx");
            checkIndexIO();
        }

        private void output_FileSelected(FileBar sender, FileBarEventArgs args)
        {
            checkIndexIO();
        }
    }

    public class VobSubTool : ITool
    {

        

        public string Name
        {
            get { return "VobSubber"; }
        }

        /// <summary>
        /// launches the vobsub indexer
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void Run(MainForm info)
        {
            new VobSubIndexWindow(info).Show();
        }

        public Shortcut[] Shortcuts
        {
            get { return new Shortcut[] { Shortcut.CtrlN }; }
        }

        

        

        public string ID
        {
            get { return "VobSubber"; }
        }

        
    }
}