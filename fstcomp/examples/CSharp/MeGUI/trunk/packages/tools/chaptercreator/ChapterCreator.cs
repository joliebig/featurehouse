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
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Windows.Forms;

using MeGUI.core.util;

namespace MeGUI
{

	/// <summary>
	/// Summary description for ChapterCreator.
	/// </summary>
	public class ChapterCreator : System.Windows.Forms.Form
	{

		private Chapter[] chapters;
		private string videoInput;
		private VideoPlayer player;
		private int introEndFrame = 0, creditsStartFrame = 0;
        private MainForm mainForm;
        private ChapterInfo pgc;
        private int intIndex;

		private System.Windows.Forms.GroupBox chaptersGroupbox;
		private System.Windows.Forms.Button addZoneButton;
        private System.Windows.Forms.Button clearZonesButton;
		private System.Windows.Forms.Button showVideoButton;
		private System.Windows.Forms.Button removeZoneButton;
		private System.Windows.Forms.ColumnHeader timecodeColumn;
		private System.Windows.Forms.ColumnHeader nameColumn;
		private System.Windows.Forms.Label startTimeLabel;
		private System.Windows.Forms.Label chapterNameLabel;
		private System.Windows.Forms.Button saveButton;
		private System.Windows.Forms.TextBox startTime;
		private System.Windows.Forms.TextBox chapterName;
		private System.Windows.Forms.ListView chapterListView;
		private System.Windows.Forms.OpenFileDialog openFileDialog;
		private System.Windows.Forms.SaveFileDialog saveFileDialog;
        private System.Windows.Forms.GroupBox gbInput;
        private System.Windows.Forms.RadioButton rbFromFile;
        private System.Windows.Forms.RadioButton rbFromDisk;
        private System.Windows.Forms.Button btInput;
        private System.Windows.Forms.TextBox input;
        private MeGUI.core.gui.HelpButton helpButton1;
        private System.Windows.Forms.CheckBox closeOnQueue; 
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
		#region start / stop
		public ChapterCreator(MainForm mainForm)
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
            intIndex = 0;
			chapters = new Chapter[0];
            this.mainForm = mainForm;
            pgc = new ChapterInfo()
            {
                Chapters = new List<Chapter>(),
                FramesPerSecond = 25.0,
                LangCode = string.Empty
            };
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if(components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}
		#endregion
		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ChapterCreator));
            this.chaptersGroupbox = new System.Windows.Forms.GroupBox();
            this.chapterName = new System.Windows.Forms.TextBox();
            this.chapterNameLabel = new System.Windows.Forms.Label();
            this.chapterListView = new System.Windows.Forms.ListView();
            this.timecodeColumn = new System.Windows.Forms.ColumnHeader();
            this.nameColumn = new System.Windows.Forms.ColumnHeader();
            this.startTime = new System.Windows.Forms.TextBox();
            this.startTimeLabel = new System.Windows.Forms.Label();
            this.addZoneButton = new System.Windows.Forms.Button();
            this.clearZonesButton = new System.Windows.Forms.Button();
            this.showVideoButton = new System.Windows.Forms.Button();
            this.removeZoneButton = new System.Windows.Forms.Button();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.saveFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.gbInput = new System.Windows.Forms.GroupBox();
            this.btInput = new System.Windows.Forms.Button();
            this.input = new System.Windows.Forms.TextBox();
            this.rbFromFile = new System.Windows.Forms.RadioButton();
            this.rbFromDisk = new System.Windows.Forms.RadioButton();
            this.saveButton = new System.Windows.Forms.Button();
            this.helpButton1 = new MeGUI.core.gui.HelpButton();
            this.closeOnQueue = new System.Windows.Forms.CheckBox();
            this.chaptersGroupbox.SuspendLayout();
            this.gbInput.SuspendLayout();
            this.SuspendLayout();
            // 
            // chaptersGroupbox
            // 
            this.chaptersGroupbox.Controls.Add(this.chapterName);
            this.chaptersGroupbox.Controls.Add(this.chapterNameLabel);
            this.chaptersGroupbox.Controls.Add(this.chapterListView);
            this.chaptersGroupbox.Controls.Add(this.startTime);
            this.chaptersGroupbox.Controls.Add(this.startTimeLabel);
            this.chaptersGroupbox.Controls.Add(this.addZoneButton);
            this.chaptersGroupbox.Controls.Add(this.clearZonesButton);
            this.chaptersGroupbox.Controls.Add(this.showVideoButton);
            this.chaptersGroupbox.Controls.Add(this.removeZoneButton);
            this.chaptersGroupbox.Location = new System.Drawing.Point(4, 86);
            this.chaptersGroupbox.Name = "chaptersGroupbox";
            this.chaptersGroupbox.Size = new System.Drawing.Size(458, 336);
            this.chaptersGroupbox.TabIndex = 23;
            this.chaptersGroupbox.TabStop = false;
            this.chaptersGroupbox.Text = "Chapters";
            // 
            // chapterName
            // 
            this.chapterName.Location = new System.Drawing.Point(75, 305);
            this.chapterName.Name = "chapterName";
            this.chapterName.Size = new System.Drawing.Size(306, 21);
            this.chapterName.TabIndex = 38;
            this.chapterName.Text = "Chapter 01";
            this.chapterName.TextChanged += new System.EventHandler(this.chapterName_TextChanged);
            // 
            // chapterNameLabel
            // 
            this.chapterNameLabel.Location = new System.Drawing.Point(13, 308);
            this.chapterNameLabel.Name = "chapterNameLabel";
            this.chapterNameLabel.Size = new System.Drawing.Size(56, 17);
            this.chapterNameLabel.TabIndex = 37;
            this.chapterNameLabel.Text = "Name :";
            // 
            // chapterListView
            // 
            this.chapterListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.timecodeColumn,
            this.nameColumn});
            this.chapterListView.FullRowSelect = true;
            this.chapterListView.HideSelection = false;
            this.chapterListView.Location = new System.Drawing.Point(16, 24);
            this.chapterListView.Name = "chapterListView";
            this.chapterListView.Size = new System.Drawing.Size(365, 240);
            this.chapterListView.TabIndex = 36;
            this.chapterListView.UseCompatibleStateImageBehavior = false;
            this.chapterListView.View = System.Windows.Forms.View.Details;
            this.chapterListView.SelectedIndexChanged += new System.EventHandler(this.chapterListView_SelectedIndexChanged);
            // 
            // timecodeColumn
            // 
            this.timecodeColumn.Text = "Timecode";
            this.timecodeColumn.Width = 100;
            // 
            // nameColumn
            // 
            this.nameColumn.Text = "Name";
            this.nameColumn.Width = 250;
            // 
            // startTime
            // 
            this.startTime.Location = new System.Drawing.Point(75, 274);
            this.startTime.Name = "startTime";
            this.startTime.Size = new System.Drawing.Size(306, 21);
            this.startTime.TabIndex = 23;
            this.startTime.Text = "00:00:00.000";
            // 
            // startTimeLabel
            // 
            this.startTimeLabel.Location = new System.Drawing.Point(13, 277);
            this.startTimeLabel.Name = "startTimeLabel";
            this.startTimeLabel.Size = new System.Drawing.Size(64, 16);
            this.startTimeLabel.TabIndex = 24;
            this.startTimeLabel.Text = "Start Time :";
            // 
            // addZoneButton
            // 
            this.addZoneButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.addZoneButton.Location = new System.Drawing.Point(392, 24);
            this.addZoneButton.Name = "addZoneButton";
            this.addZoneButton.Size = new System.Drawing.Size(55, 23);
            this.addZoneButton.TabIndex = 33;
            this.addZoneButton.Text = "&Add";
            this.addZoneButton.Click += new System.EventHandler(this.addZoneButton_Click);
            // 
            // clearZonesButton
            // 
            this.clearZonesButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.clearZonesButton.Location = new System.Drawing.Point(392, 82);
            this.clearZonesButton.Name = "clearZonesButton";
            this.clearZonesButton.Size = new System.Drawing.Size(55, 23);
            this.clearZonesButton.TabIndex = 29;
            this.clearZonesButton.Text = "&Clear";
            this.clearZonesButton.Click += new System.EventHandler(this.clearZonesButton_Click);
            // 
            // showVideoButton
            // 
            this.showVideoButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.showVideoButton.Enabled = false;
            this.showVideoButton.Location = new System.Drawing.Point(392, 111);
            this.showVideoButton.Name = "showVideoButton";
            this.showVideoButton.Size = new System.Drawing.Size(55, 23);
            this.showVideoButton.TabIndex = 34;
            this.showVideoButton.Text = "&Preview";
            this.showVideoButton.Click += new System.EventHandler(this.showVideoButton_Click);
            // 
            // removeZoneButton
            // 
            this.removeZoneButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.removeZoneButton.Location = new System.Drawing.Point(392, 53);
            this.removeZoneButton.Name = "removeZoneButton";
            this.removeZoneButton.Size = new System.Drawing.Size(55, 23);
            this.removeZoneButton.TabIndex = 32;
            this.removeZoneButton.Text = "&Remove";
            this.removeZoneButton.Click += new System.EventHandler(this.removeZoneButton_Click);
            // 
            // openFileDialog
            // 
            this.openFileDialog.DefaultExt = "txt";
            this.openFileDialog.Filter = "Blu-ray Playlist Files (*.mpls)|*.mpls|IFO Files (*.ifo)|*.ifo|Chapter Files (*.t" +
                "xt)|*.txt|All Files supported (*.ifo;*.txt;*.mpls)|*.ifo;*.mpls;*.txt";
            this.openFileDialog.FilterIndex = 4;
            // 
            // saveFileDialog
            // 
            this.saveFileDialog.DefaultExt = "txt";
            this.saveFileDialog.Filter = "x264 qp Files (.qpf)|*.qpf|Chapter Files (*.txt)|*.txt|Matroska Chapters files (*" +
                ".xml)|*.xml|All supported Files (*.qpf;*.txt;*.xml)|*.qpf;*.txt;*.xml";
            this.saveFileDialog.FilterIndex = 4;
            //
            // gbInput
            // 
            this.gbInput.Controls.Add(this.btInput);
            this.gbInput.Controls.Add(this.input);
            this.gbInput.Controls.Add(this.rbFromFile);
            this.gbInput.Controls.Add(this.rbFromDisk);
            this.gbInput.Location = new System.Drawing.Point(4, 4);
            this.gbInput.Name = "gbInput";
            this.gbInput.Size = new System.Drawing.Size(458, 76);
            this.gbInput.TabIndex = 24;
            this.gbInput.TabStop = false;
            this.gbInput.Text = "Input";
            // 
            // btInput
            // 
            this.btInput.Location = new System.Drawing.Point(392, 20);
            this.btInput.Name = "btInput";
            this.btInput.Size = new System.Drawing.Size(55, 23);
            this.btInput.TabIndex = 10;
            this.btInput.Text = "...";
            this.btInput.UseVisualStyleBackColor = true;
            this.btInput.Click += new System.EventHandler(this.btInput_Click);
            // 
            // input
            // 
            this.input.Location = new System.Drawing.Point(18, 20);
            this.input.Name = "input";
            this.input.ReadOnly = true;
            this.input.Size = new System.Drawing.Size(363, 21);
            this.input.TabIndex = 9;
            // 
            // rbFromFile
            // 
            this.rbFromFile.AutoSize = true;
            this.rbFromFile.Checked = true;
            this.rbFromFile.Location = new System.Drawing.Point(124, 45);
            this.rbFromFile.Name = "rbFromFile";
            this.rbFromFile.Size = new System.Drawing.Size(68, 17);
            this.rbFromFile.TabIndex = 8;
            this.rbFromFile.TabStop = true;
            this.rbFromFile.Text = "From File";
            this.rbFromFile.UseVisualStyleBackColor = true;
            // 
            // rbFromDisk
            // 
            this.rbFromDisk.AutoSize = true;
            this.rbFromDisk.Location = new System.Drawing.Point(25, 45);
            this.rbFromDisk.Name = "rbFromDisk";
            this.rbFromDisk.Size = new System.Drawing.Size(71, 17);
            this.rbFromDisk.TabIndex = 7;
            this.rbFromDisk.Text = "From Disk";
            this.rbFromDisk.UseVisualStyleBackColor = true;
            // 
            // saveButton
            // 
            this.saveButton.Location = new System.Drawing.Point(396, 428);
            this.saveButton.Name = "saveButton";
            this.saveButton.Size = new System.Drawing.Size(55, 23);
            this.saveButton.TabIndex = 41;
            this.saveButton.Text = "&Save";
            this.saveButton.Click += new System.EventHandler(this.saveButton_Click);
            // 
            // helpButton1
            // 
            this.helpButton1.ArticleName = "Chapter creator";
            this.helpButton1.AutoSize = true;
            this.helpButton1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.helpButton1.Location = new System.Drawing.Point(12, 428);
            this.helpButton1.Name = "helpButton1";
            this.helpButton1.Size = new System.Drawing.Size(55, 23);
            this.helpButton1.TabIndex = 42;
            // 
            // closeOnQueue
            // 
            this.closeOnQueue.Checked = true;
            this.closeOnQueue.CheckState = System.Windows.Forms.CheckState.Checked;
            this.closeOnQueue.Location = new System.Drawing.Point(313, 428);
            this.closeOnQueue.Name = "closeOnQueue";
            this.closeOnQueue.Size = new System.Drawing.Size(72, 24);
            this.closeOnQueue.TabIndex = 43;
            this.closeOnQueue.Text = "and close";
            // 
            // ChapterCreator
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.ClientSize = new System.Drawing.Size(468, 458);
            this.Controls.Add(this.closeOnQueue);
            this.Controls.Add(this.helpButton1);
            this.Controls.Add(this.saveButton);
            this.Controls.Add(this.gbInput);
            this.Controls.Add(this.chaptersGroupbox);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "ChapterCreator";
            this.ShowInTaskbar = false;
            this.Text = "MeGUI - Chapter Creator";
            this.Load += new System.EventHandler(this.ChapterCreator_Load);
            this.chaptersGroupbox.ResumeLayout(false);
            this.chaptersGroupbox.PerformLayout();
            this.gbInput.ResumeLayout(false);
            this.gbInput.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();
		}
		#endregion
		#region helper methods
		/// <summary>
		/// shows an array of chapters in the GUI
		/// </summary>
		/// <param name="chaps">the chapters to be shown</param>
		private void showChapters(Chapter[] chaps)
		{
			this.chapterListView.Items.Clear();
			foreach (Chapter chap in chaps)
			{
				ListViewItem item = new ListViewItem(new string[] {chap.timecode, chap.name});
				item.Tag = chap;
				chapterListView.Items.Add(item);
                if (item.Index % 2 != 0)
                    item.BackColor = Color.White;
                else
                    item.BackColor = Color.WhiteSmoke;
			}
		}

        private void FreshChapterView()
        {
            this.Cursor = Cursors.WaitCursor;
            try
            {
                this.chapterListView.Items.Clear();
                //fill list
                foreach (Chapter c in pgc.Chapters)
                {
                    ListViewItem item = new ListViewItem(new string[] { c.Time.ToString(), c.Name });
                    chapterListView.Items.Add(item);
                    if (item.Index % 2 != 0)
                        item.BackColor = Color.White;
                    else
                        item.BackColor = Color.WhiteSmoke;
                }
            }
            catch (Exception ex) { MessageBox.Show(ex.Message); }
            finally { this.Cursor = Cursors.Default; }
        }

        private void updateTimeLine()
        {
            for (int i = 0; i < chapterListView.Items.Count; i++)
            {
                if (chapterListView.Items[i].SubItems[0].Text.Length == 8)
                    chapterListView.Items[i].SubItems[0].Text = chapterListView.Items[i].SubItems[0].Text + ".000";
                else
                    chapterListView.Items[i].SubItems[0].Text = chapterListView.Items[i].SubItems[0].Text.Substring(0, 12);
            }
        }
		#endregion
		#region buttons
		private void removeZoneButton_Click(object sender, System.EventArgs e)
		{
            if (chapterListView.Items.Count < 1 || pgc.Chapters.Count < 1) return;
            intIndex = chapterListView.SelectedIndices[0];
            pgc.Chapters.Remove(pgc.Chapters[intIndex]);
            if (intIndex != 0) intIndex--;
            FreshChapterView();
            updateTimeLine();
		}

		private void clearZonesButton_Click(object sender, System.EventArgs e)
		{
			this.chapterListView.Items.Clear();
			this.chapters = new Chapter[0];
		}

		private void chapterListView_SelectedIndexChanged(object sender, System.EventArgs e)
		{
            if (chapterListView.Items.Count < 1) return;

            chapterName.TextChanged -= new System.EventHandler(this.chapterName_TextChanged);
            startTime.TextChanged -= new System.EventHandler(this.chapterName_TextChanged);            
            ListView lv = (ListView)sender;

            if (lv.SelectedItems.Count == 1) intIndex = lv.SelectedItems[0].Index;
            if (pgc.Chapters.Count > 0)
            {
                this.startTime.Text = FileUtil.ToShortString(pgc.Chapters[intIndex].Time);
                this.chapterName.Text = pgc.Chapters[intIndex].Name;
            }

            chapterName.TextChanged += new System.EventHandler(this.chapterName_TextChanged);
            startTime.TextChanged += new System.EventHandler(this.chapterName_TextChanged); 
		}

		private void addZoneButton_Click(object sender, System.EventArgs e)
		{
            Chapter c;
            if (chapterListView.Items.Count != 0)
                 intIndex = chapterListView.Items.Count;
            else intIndex = 0;
            TimeSpan ts = new TimeSpan(0);
            try
            {//try to get a valid time input					
                 ts = TimeSpan.Parse(startTime.Text);
            }
            catch (Exception parse)
            { //invalid time input
                startTime.Focus();
                startTime.SelectAll();
                MessageBox.Show("Cannot parse the timecode you have entered.\nIt must be given in the hh:mm:ss.ccc format"
                                + Environment.NewLine + parse.Message, "Incorrect timecode", MessageBoxButtons.OK, MessageBoxIcon.Stop);
                return;
            }
            //create a new chapter
            c = new Chapter() { Time = ts, Name = chapterName.Text };
            pgc.Chapters.Insert(intIndex, c);
            FreshChapterView();
            updateTimeLine();
		}
		#endregion
		#region saving files
		private void saveButton_Click(object sender, System.EventArgs e)
		{
			if (this.saveFileDialog.ShowDialog() == DialogResult.OK)
			{
                string ext = Path.GetExtension(saveFileDialog.FileName).ToLower();
                if (Drives.ableToWriteOnThisDrive(Path.GetPathRoot(saveFileDialog.FileName)))
                {
                    if (ext == ".qpf")
                        pgc.SaveQpfile(saveFileDialog.FileName);
                    else if (ext == ".xml")
                        pgc.SaveXml(saveFileDialog.FileName);
                    else
                        pgc.SaveText(saveFileDialog.FileName);
                }
                else
                    MessageBox.Show("MeGUI cannot write on the disc " + Path.GetPathRoot(saveFileDialog.FileName) + "\n" +
                "Please, select another output path to save your project...", "Configuration Incomplete", MessageBoxButtons.OK, MessageBoxIcon.Warning);
			}

            if (this.closeOnQueue.Checked)
                this.Close();
		}
		#endregion

        private void btInput_Click(object sender, EventArgs e)
        {
            if (rbFromFile.Checked)
            {
                openFileDialog.Filter = "IFO Files (*.ifo)|*.ifo|MPLS Files (*.mpls)|*.mpls|Text Files (*.txt)|*.txt|All Files supported (*.ifo,*.mpls,*.txt)|*.ifo;*.mpls;*.txt";
                openFileDialog.FilterIndex = 4;

               if (this.openFileDialog.ShowDialog() == DialogResult.OK)
                {
                    input.Text = openFileDialog.FileName;

                    if (input.Text.ToLower().EndsWith("ifo"))
                    {
                        ChapterExtractor ex = new IfoExtractor();
                        pgc = ex.GetStreams(input.Text)[0];
                        FreshChapterView();
                        updateTimeLine();
                    }
                    else if (input.Text.ToLower().EndsWith("mpls"))
                    {
                        ChapterExtractor ex = new MplsExtractor();
                        pgc = ex.GetStreams(input.Text)[0];
                        FreshChapterView();
                        updateTimeLine();
                    }
                    else
                    {
                        ChapterExtractor ex = new TextExtractor();
                        pgc = ex.GetStreams(input.Text)[0];
                        FreshChapterView();
                        updateTimeLine();
                    }
                }
            }
            else
            {
                using (FolderBrowserDialog d = new FolderBrowserDialog())
                {
                    d.ShowNewFolderButton = false;
                    d.Description = "Select DVD, BluRay disc, or folder.";
                    if (d.ShowDialog() == DialogResult.OK)
                    {
                        input.Text = d.SelectedPath;                       
                        try
                        {
                            ChapterExtractor ex =
                              Directory.Exists(Path.Combine(input.Text, "VIDEO_TS")) ?
                              new DvdExtractor() as ChapterExtractor :
                              Directory.Exists(Path.Combine(Path.Combine(input.Text, "BDMV"), "PLAYLIST")) ?
                              new BlurayExtractor() as ChapterExtractor :
                              null;

                            if (ex == null)
                                throw new Exception("The location was not detected as DVD, or Blu-Ray.");


                            using (frmStreamSelect frm = new frmStreamSelect(ex))
                            {
                                if (ex is DvdExtractor)
                                    frm.Text = "Select your PGC";
                                else
                                    frm.Text = "Select your Playlist";
                                ex.GetStreams(input.Text);
                                if (frm.ShowDialog(this) == DialogResult.OK)
                                {
                                    pgc = frm.ProgramChain;
                                    if (pgc.FramesPerSecond == 0) pgc.FramesPerSecond = 25.0;
                                    if (pgc.LangCode == null) pgc.LangCode = "und";
                                }
                            }
                            FreshChapterView();
                            updateTimeLine();
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show(ex.Message);
                        }
                    }
                }
            }


            if (chapterListView.Items.Count != 0)
                chapterListView.Items[0].Selected = true;
        }

		private void showVideoButton_Click(object sender, System.EventArgs e)
		{
			if (!this.videoInput.Equals(""))
			{
				if (player == null)
				{
					player = new VideoPlayer();
					bool videoLoaded = player.loadVideo(mainForm, videoInput, PREVIEWTYPE.CHAPTERS, false);
					if (videoLoaded)
					{
						player.Closed += new EventHandler(player_Closed);
						player.ChapterSet += new ChapterSetCallback(player_ChapterSet);
						if (introEndFrame > 0)
							player.IntroEnd = this.introEndFrame;
						if (creditsStartFrame > 0)
							player.CreditsStart = this.creditsStartFrame;
						player.Show();
					}
					else
						return;
				}
				if (chapterListView.SelectedItems.Count == 1) // a zone has been selected, show that zone
				{
					Chapter chap = (Chapter)chapterListView.SelectedItems[0].Tag;
					int time = Util.getTimeCode(chap.timecode);
					double framerate = player.Framerate;
                    int frameNumber = Util.convertTimecodeToFrameNumber(time, framerate);
					player.CurrentFrame = frameNumber;

				}
				else // no chapter has been selected.. but if start time is configured, show the frame in the preview
				{
					if (!startTime.Text.Equals(""))
					{
						int time = Util.getTimeCode(startTime.Text);
						double framerate = player.Framerate;
                        int frameNumber = Util.convertTimecodeToFrameNumber(time, framerate);
						player.CurrentFrame = frameNumber;
					}
				}
			}
			else
				MessageBox.Show("Please configure video input first", "No video input found", MessageBoxButtons.OK, MessageBoxIcon.Stop);
		}

		#region properties
		/// <summary>
		/// sets the video input to be used for a zone preview
		/// </summary>
		public string VideoInput
		{
			set 
			{
				this.videoInput = value;
				showVideoButton.Enabled = true;
			}
		}
		/// <summary>
		/// gets / sets the start frame of the credits
		/// </summary>
		public int CreditsStartFrame
		{
			get {return this.creditsStartFrame;}
			set {creditsStartFrame = value;}
		}
		/// <summary>
		/// gets / sets the end frame of the intro
		/// </summary>
		public int IntroEndFrame
		{
			get {return this.introEndFrame;}
			set {introEndFrame = value;}
		}
		#endregion
		private void player_Closed(object sender, EventArgs e)
		{
			player = null;
		}

		private void player_ChapterSet(int frameNumber)
		{
            startTime.Text = Util.converFrameNumberToTimecode(frameNumber, player.Framerate);
            if (chapterListView.Items.Count < 9)
                 chapterName.Text = "Chapter 0" + Convert.ToString(chapterListView.Items.Count+1);
            else chapterName.Text = "Chapter " + Convert.ToString(chapterListView.Items.Count+1);
			addZoneButton_Click(null, null);
		}

        private void ChapterCreator_Load(object sender, EventArgs e)
        {

            if (VistaStuff.IsVistaOrNot)
            {
                VistaStuff.SetWindowTheme(chapterListView.Handle, "explorer", null);
            }
        }

        private void chapterName_TextChanged(object sender, EventArgs e)
        {
            try
            {
                intIndex = chapterListView.SelectedIndices[0];
                pgc.Chapters[intIndex] = new Chapter()
                {
                    Time = TimeSpan.Parse(startTime.Text),
                    Name = chapterName.Text
                };
                chapterListView.SelectedItems[0].SubItems[0].Text = startTime.Text;
                chapterListView.SelectedItems[0].SubItems[1].Text = chapterName.Text;
            }
            catch (Exception parse)
            { //invalid time input
                startTime.Focus();
                startTime.SelectAll();
                MessageBox.Show("Cannot parse the timecode you have entered.\nIt must be given in the hh:mm:ss.ccc format"
                                + Environment.NewLine + parse.Message, "Incorrect timecode", MessageBoxButtons.OK, MessageBoxIcon.Stop);
                return;
            }
        }
	}
	public struct Chapter
	{
		public string timecode;
        public TimeSpan StartTime;
		public string name;

        //add-on
        public TimeSpan Time { get; set; }
        public string Name { get; set; }

        //public string Lang { get; set; }
        public override string ToString()
        {
            return Time.ToString() + ": " + Name;
        }
	}

    public class ChapterCreatorTool : MeGUI.core.plugins.interfaces.ITool
    {

        #region ITool Members

        public string Name
        {
            get { return "Chapter Creator"; }
        }

        public void Run(MainForm info)
        {
            ChapterCreator cc = new ChapterCreator(info);
            cc.VideoInput = info.Video.Info.VideoInput;
            cc.CreditsStartFrame = info.Video.Info.CreditsStartFrame;
            cc.IntroEndFrame = info.Video.Info.IntroEndFrame;
            cc.Show();
        }

        public Shortcut[] Shortcuts
        {
            get { return new Shortcut[] { Shortcut.CtrlH }; }
        }

        #endregion

        #region IIDable Members

        public string ID
        {
            get { return "chapter_creator"; }
        }

        #endregion
    }
}
