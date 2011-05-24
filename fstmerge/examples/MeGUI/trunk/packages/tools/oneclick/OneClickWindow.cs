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
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Text;
using System.Threading;
using System.Windows.Forms;

using MeGUI.core.details;
using MeGUI.core.details.video;
using MeGUI.core.gui;
using MeGUI.core.plugins.interfaces;
using MeGUI.core.util;
using MeGUI.packages.tools.oneclick;

namespace MeGUI
{

//    public class OneClickPostProcessor 
    public partial class OneClickWindow : Form
    {
        List<FileSCBox> audioTrack;
        List<Label> trackLabel;
        List<AudioConfigControl> audioConfigControl;

        
        void ProfileChanged(object sender, EventArgs e)
        {
            if (VideoSettings.EncoderType.ID == "x264")
                usechaptersmarks.Enabled = true;
            else
            {
                usechaptersmarks.Enabled = false;
                usechaptersmarks.Checked = usechaptersmarks.Enabled;
            }            
            updatePossibleContainers();
        }

        
        private void initOneClickHandler()
        {
            oneclickProfile.Manager = mainForm.Profiles;
        }

        private void initTabs()
        {
            audioTrack = new List<FileSCBox>();
            audioTrack.Add(audioTrack1);
            audioTrack.Add(audioTrack2);

            trackLabel = new List<Label>();
            trackLabel.Add(track1Label);
            trackLabel.Add(track2Label);

            audioConfigControl = new List<AudioConfigControl>();
            audioConfigControl.Add(audio1);
            audioConfigControl.Add(audio2);
        }

        void OneClickProfileChanged(object sender, EventArgs e)
        {
            this.Settings = (OneClickSettings)oneclickProfile.SelectedProfile.BaseSettings;
        } 
        
        
        private VideoCodecSettings VideoSettings
        {
            get { return (VideoCodecSettings)videoProfile.SelectedProfile.BaseSettings; }
        }
        
        
        private void initAudioHandler()
        {
            audio1.initHandler();
            audio2.initHandler();
        }
        
        

        
        /// <summary>
        /// whether we ignore the restrictions on container output type set by the profile
        /// </summary>
        private bool ignoreRestrictions = false;
        
        /// <summary>
        /// the restrictions from above: the only containers we may use
        /// </summary>
        private ContainerType[] acceptableContainerTypes;
        
        private VideoUtil vUtil;
        private MainForm mainForm;
        private MuxProvider muxProvider;
        
        /// <summary>
        /// whether the user has selected an output filename
        /// </summary>
        private bool outputChosen = false;

        
        
        
        public OneClickWindow(MainForm mainForm, JobUtil jobUtil, VideoEncoderProvider vProv, AudioEncoderProvider aProv)
        {
            this.mainForm = mainForm;
            vUtil = new VideoUtil(mainForm);
            this.muxProvider = mainForm.MuxProvider;
            acceptableContainerTypes = muxProvider.GetSupportedContainers().ToArray();

            InitializeComponent();

            initTabs();

            videoProfile.Manager = mainForm.Profiles;
            initAudioHandler();
            avsProfile.Manager = mainForm.Profiles;
            initOneClickHandler();

            audioTrack1.StandardItems = audioTrack2.StandardItems = new object[] { "None" };
            audioTrack1.SelectedIndex = audioTrack2.SelectedIndex = 0;

            //if containerFormat has not yet been set by the oneclick profile, add supported containers
            if (containerFormat.Items.Count == 0)
            {
                containerFormat.Items.AddRange(muxProvider.GetSupportedContainers().ToArray());
                this.containerFormat.SelectedIndex = 0;
            }

            //add device type
            if (devicetype.Items.Count == 0)
            {
                devicetype.Items.Add("Standard");
                devicetype.Items.AddRange(muxProvider.GetSupportedDevices().ToArray());
                this.devicetype.SelectedIndex = 0;
            }

            showAdvancedOptions_CheckedChanged(null, null);
        }
        

        
        private void showAdvancedOptions_CheckedChanged(object sender, EventArgs e)
        {
            if (showAdvancedOptions.Checked)
            {
                if (!tabControl1.TabPages.Contains(tabPage2))
                    tabControl1.TabPages.Add(tabPage2);
                if (!tabControl1.TabPages.Contains(encoderConfigTab))
                    tabControl1.TabPages.Add(encoderConfigTab);
            }
            else
            {
                if (tabControl1.TabPages.Contains(tabPage2))
                    tabControl1.TabPages.Remove(tabPage2);
                if (tabControl1.TabPages.Contains(encoderConfigTab))
                    tabControl1.TabPages.Remove(encoderConfigTab);
            }
        }
        private void containerFormat_SelectedIndexChanged_1(object sender, EventArgs e)
        {
            if (this.containerFormat.Text != "MP4")
                this.devicetype.Enabled = false;
            else this.devicetype.Enabled = true;
            updateFilename();
        }

        private void updateFilename()
        {
            if (!outputChosen)
            {
                output.Filename = Path.Combine(workingDirectory.Filename, workingName.Text + "." +
                    ((ContainerType)containerFormat.SelectedItem).Extension);
                outputChosen = false;
            }
            else
            {
                output.Filename = Path.ChangeExtension(output.Filename, ((ContainerType)containerFormat.SelectedItem).Extension);
            }
            output.Filter = ((ContainerType)containerFormat.SelectedItem).OutputFilterString;
        }

        private void input_FileSelected(FileBar sender, FileBarEventArgs args)
        {
            openInput(input.Filename);
        }

        private void output_FileSelected(FileBar sender, FileBarEventArgs args)
        {
            outputChosen = true;
        }

        private void workingDirectory_FileSelected(FileBar sender, FileBarEventArgs args)
        {
            updateFilename();
        }

        private void workingName_TextChanged(object sender, EventArgs e)
        {
            updateFilename();
        }
        
        public void openInput(string fileName)
        {
            input.Filename = fileName;
            Dar? ar;
            int maxHorizontalResolution;
            List<AudioTrackInfo> audioTracks;
            List<SubtitleInfo> subtitles;
            vUtil.openVideoSource(fileName, out audioTracks, out subtitles, out ar, out maxHorizontalResolution);

            List<object> trackNames = new List<object>();
            trackNames.Add("None");
            foreach (object o in audioTracks)
                trackNames.Add(o);

            foreach (FileSCBox b in audioTrack)
                b.StandardItems = trackNames.ToArray();

            foreach (AudioTrackInfo ati in audioTracks)
            {
                if (ati.Language.ToLower().Equals(mainForm.Settings.DefaultLanguage1.ToLower()) &&
                    audioTrack1.SelectedIndex == 0)
                {
                    audioTrack1.SelectedObject = ati;
                    continue;
                }
                if (ati.Language.ToLower().Equals(mainForm.Settings.DefaultLanguage2.ToLower()) &&
                    audioTrack2.SelectedIndex == 0)
                {
                    audioTrack2.SelectedObject = ati;
                    continue;
                }
            }

            // If nothing matches DefaultLanguage select 1st track
            if (audioTrack1.SelectedIndex == 0)
            {
                audioTrack1.SelectedObject = audioTracks[0];
            }
  
            horizontalResolution.Maximum = maxHorizontalResolution;
            
            string chapterFile = VideoUtil.getChapterFile(fileName);
            if (File.Exists(chapterFile))
                this.chapterFile.Filename = chapterFile;
            if (string.IsNullOrEmpty(this.chapterFile.Filename))
            {
                bool useqpFile = false;
                if (usechaptersmarks.Checked && usechaptersmarks.Enabled)
                    useqpFile = true;
                VideoUtil.getChaptersFromIFO(fileName, useqpFile);
                chapterFile = VideoUtil.getChapterFile(fileName);
                if (File.Exists(chapterFile))
                    this.chapterFile.Filename = chapterFile;
            }

            if (string.IsNullOrEmpty(workingDirectory.Filename = mainForm.Settings.DefaultOutputDir))
                workingDirectory.Filename = Path.GetDirectoryName(fileName);

            workingName.Text = PrettyFormatting.ExtractWorkingName(fileName);
            this.updateFilename();
            this.ar.Value = ar;            

            if (VideoSettings.EncoderType.ID == "x264" && this.chapterFile.Filename != null)
                this.usechaptersmarks.Enabled = true; 
        }

        private bool beingCalled;
        private void updatePossibleContainers()
        {
            // Since everything calls everything else, this is just a safeguard to make sure we don't infinitely recurse
            if (beingCalled)
                return;
            beingCalled = true;

            List<AudioEncoderType> audioCodecs = new List<AudioEncoderType>();
            List<MuxableType> dictatedOutputTypes = new List<MuxableType>();

            for (int i = 0; i < audioConfigControl.Count; ++i)
            {
                if (audioTrack[i].SelectedIndex == 0) // "None"
                    continue;

                if (audioConfigControl[i].Settings != null && !audioConfigControl[i].DontEncode)
                    audioCodecs.Add(audioConfigControl[i].Settings.EncoderType);

                else if (audioConfigControl[i].DontEncode)
                {
                    string typeString;

                    if (audioTrack[i].SelectedSCItem.IsStandard)
                    {
                        AudioTrackInfo ati = (AudioTrackInfo)audioTrack[i].SelectedObject;
                        typeString = "file." + ati.Type;
                    }
                    else
                    {
                        typeString = audioTrack[i].SelectedText;
                    }

                    if (VideoUtil.guessAudioType(typeString) != null)
                        dictatedOutputTypes.Add(VideoUtil.guessAudioMuxableType(typeString, false));
                }
            }

            List<ContainerType> tempSupportedOutputTypes = this.muxProvider.GetSupportedContainers(
                VideoSettings.EncoderType, audioCodecs.ToArray(), dictatedOutputTypes.ToArray());

            List<ContainerType> supportedOutputTypes = new List<ContainerType>();

            foreach (ContainerType c in acceptableContainerTypes)
            {
                if (tempSupportedOutputTypes.Contains(c))
                    supportedOutputTypes.Add(c);
            }
            
            if (supportedOutputTypes.Count == 0)
            {
                if (tempSupportedOutputTypes.Count > 0 && !ignoreRestrictions)
                {
                    string message = string.Format(
                    "No container type could be found that matches the list of acceptable types " +
                    "in your chosen one click profile. {0}" +
                    "Your restrictions are now being ignored.", Environment.NewLine);
                    MessageBox.Show(message, "Filetype restrictions too restrictive", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                    ignoreRestrictions = true;
                }
                if (ignoreRestrictions) supportedOutputTypes = tempSupportedOutputTypes;
                if (tempSupportedOutputTypes.Count == 0)
                    MessageBox.Show("No container type could be found for your current settings. Please modify the codecs you use", "No container found", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            if (supportedOutputTypes.Count > 0)
            {
                this.containerFormat.Items.Clear();
                this.containerFormat.Items.AddRange(supportedOutputTypes.ToArray());
                this.containerFormat.SelectedIndex = 0;
                this.output.Filename = Path.ChangeExtension(output.Filename, (this.containerFormat.SelectedItem as ContainerType).Extension);
            }
            beingCalled = false;
        }

        private OneClickSettings Settings
        {
            set
            {
                OneClickSettings settings = value;

                foreach (AudioConfigControl a in audioConfigControl)
                    a.SelectProfileNameOrWarn(settings.AudioProfileName);

                videoProfile.SetProfileNameOrWarn(settings.VideoProfileName);
                avsProfile.SetProfileNameOrWarn(settings.AvsProfileName);

                List<ContainerType> temp = new List<ContainerType>();
                List<ContainerType> allContainerTypes = muxProvider.GetSupportedContainers();
                foreach (string s in settings.ContainerCandidates)
                {
                    ContainerType ct = allContainerTypes.Find(new Predicate<ContainerType>(delegate(ContainerType t) { return t.ToString() == s; }));
                    if (ct != null)
                        temp.Add(ct);
                }
                acceptableContainerTypes = temp.ToArray();

                ignoreRestrictions = false;

                foreach (AudioConfigControl a in audioConfigControl)
                    a.DontEncode = settings.DontEncodeAudio;
                
                // bools
                signalAR.Checked = settings.SignalAR;
                autoDeint.Checked = settings.AutomaticDeinterlacing;
                autoCrop.Checked = settings.AutoCrop;
                keepInputResolution.Checked = settings.KeepInputResolution;
                addPrerenderJob.Checked = settings.PrerenderVideo;
                if (usechaptersmarks.Enabled)
                    usechaptersmarks.Checked = settings.UseChaptersMarks;

                splitting.Value = settings.SplitSize;
                optionalTargetSizeBox1.Value = settings.Filesize;
                horizontalResolution.Value = settings.OutputResolution;

                // device type
                devicetype.Text = settings.DeviceOutputType;

                // Clean up after those settings were set
                updatePossibleContainers();
                containerFormat_SelectedIndexChanged_1(null, null);
            }
        }

        private void goButton_Click(object sender, EventArgs e)
        {
            if (Drives.ableToWriteOnThisDrive(Path.GetPathRoot(output.Filename)) || // check whether the output path is read-only
                Drives.ableToWriteOnThisDrive(Path.GetPathRoot(workingName.Text)))
            {
                if ((verifyAudioSettings() == null)
                    && (VideoSettings != null)
                    && !string.IsNullOrEmpty(input.Filename)
                    && !string.IsNullOrEmpty(workingName.Text))
                {
                    FileSize? desiredSize = optionalTargetSizeBox1.Value;

                    List<AudioJob> aJobs = new List<AudioJob>();
                    List<MuxStream> muxOnlyAudio = new List<MuxStream>();
                    List<AudioTrackInfo> audioTracks = new List<AudioTrackInfo>();
                    for (int i = 0; i < audioConfigControl.Count; ++i)
                    {
                        if (audioTrack[i].SelectedIndex == 0) // "None"
                            continue;

                        string aInput;
                        TrackInfo info = null;
                        int delay = audioConfigControl[i].Delay;
                        if (audioTrack[i].SelectedSCItem.IsStandard)
                        {
                            AudioTrackInfo a = (AudioTrackInfo)audioTrack[i].SelectedObject;
                            audioTracks.Add(a);
                            aInput = "::" + a.TrackID + "::";
                            info = a.TrackInfo;
                        }
                        else
                            aInput = audioTrack[i].SelectedText;

                        if (audioConfigControl[i].DontEncode)
                            muxOnlyAudio.Add(new MuxStream(aInput, info, delay));
                        else
                            aJobs.Add(new AudioJob(aInput, null, null, audioConfigControl[i].Settings, delay));
                    }

                    string d2vName = Path.Combine(workingDirectory.Filename, workingName.Text + ".d2v");

                    DGIndexPostprocessingProperties dpp = new DGIndexPostprocessingProperties();
                    dpp.DAR = ar.Value;
                    dpp.DirectMuxAudio = muxOnlyAudio.ToArray();
                    dpp.AudioJobs = aJobs.ToArray();
                    dpp.AutoDeinterlace = autoDeint.Checked;
                    dpp.AvsSettings = (AviSynthSettings)avsProfile.SelectedProfile.BaseSettings;
                    dpp.ChapterFile = chapterFile.Filename;
                    dpp.Container = (ContainerType)containerFormat.SelectedItem;
                    dpp.FinalOutput = output.Filename;
                    dpp.HorizontalOutputResolution = (int)horizontalResolution.Value;
                    dpp.OutputSize = desiredSize;
                    dpp.SignalAR = signalAR.Checked;
                    dpp.AutoCrop = autoCrop.Checked;
                    dpp.KeepInputResolution = keepInputResolution.Checked;
                    dpp.PrerenderJob = addPrerenderJob.Checked;
                    dpp.Splitting = splitting.Value;
                    dpp.DeviceOutputType = devicetype.Text;
                    dpp.UseChaptersMarks = usechaptersmarks.Checked;
                    dpp.VideoSettings = VideoSettings.Clone();
                    D2VIndexJob job = new D2VIndexJob(input.Filename, d2vName, 1, audioTracks, dpp, false, false);
                    mainForm.Jobs.addJobsToQueue(job);
                    if (this.openOnQueue.Checked)
                    {
                        if (!string.IsNullOrEmpty(this.chapterFile.Filename))
                            this.chapterFile.Filename = string.Empty; // clean up  
                        input.PerformClick();
                    }
                    else
                        this.Close();
                }
                else
                    MessageBox.Show("You must select audio and video profile, output name and working directory to continue",
                        "Incomplete configuration", MessageBoxButtons.OK, MessageBoxIcon.Stop);
            }
            else MessageBox.Show("MeGUI cannot write on the disc " + Path.GetPathRoot(output.Filename) + " \n" +
                                 "Please, select another output path to save your project...", "Configuration Incomplete", MessageBoxButtons.OK, MessageBoxIcon.Warning);

        }

        

        

        private string verifyAudioSettings()
        {
            for (int i = 0; i < audioTrack.Count; ++i)
            {
                if (audioTrack[i].SelectedSCItem.IsStandard)
                    continue;

                string r = MainForm.verifyInputFile(audioTrack[i].SelectedText);
                if (r != null) return r;
            }
            return null;
        }
        

        public struct PartialAudioStream
        {
            public string input;
            public string language;
            public bool useExternalInput;
            public bool dontEncode;
            public int trackNumber;
            public AudioCodecSettings settings;
        }

        private void AddTrack()
        {
            FileSCBox b = new FileSCBox();
            b.Filter = audioTrack1.Filter;
            b.Size = audioTrack1.Size;
            b.StandardItems = audioTrack1.StandardItems;
            b.SelectedIndex = 0;
            b.Anchor = audioTrack1.Anchor;
            b.SelectionChanged += new StringChanged(this.audioTrack1_SelectionChanged);
            
            int delta_y = audioTrack2.Location.Y - audioTrack1.Location.Y;
            b.Location = new Point(audioTrack1.Location.X, audioTrack[audioTrack.Count - 1].Location.Y + delta_y);

            Label l = new Label();
            l.Text = "Track " + (audioTrack.Count + 1);
            l.AutoSize = true;
            l.Location = new Point(track1Label.Location.X, trackLabel[trackLabel.Count - 1].Location.Y + delta_y);

            AudioConfigControl a = new AudioConfigControl();
            a.Dock = DockStyle.Fill;
            a.Location = audio1.Location;
            a.Size = audio1.Size;
            a.initHandler();
            a.SomethingChanged += new EventHandler(audio1_SomethingChanged);

            TabPage t = new TabPage("Audio track " + (audioTrack.Count + 1));
            t.UseVisualStyleBackColor = trackTabPage1.UseVisualStyleBackColor;
            t.Padding = trackTabPage1.Padding;
            t.Size = trackTabPage1.Size;
            t.Controls.Add(a);
            tabControl2.TabPages.Add(t);
            
            panel1.SuspendLayout();
            panel1.Controls.Add(l);
            panel1.Controls.Add(b);
            panel1.ResumeLayout();

            trackLabel.Add(l);
            audioTrack.Add(b);
            audioConfigControl.Add(a);
        }

        private void RemoveTrack()
        {
            panel1.SuspendLayout();
            panel1.Controls.Remove(trackLabel[trackLabel.Count - 1]);
            panel1.Controls.Remove(audioTrack[audioTrack.Count - 1]);
            panel1.ResumeLayout();
            
            tabControl2.TabPages.RemoveAt(tabControl2.TabPages.Count - 1);
            trackLabel.RemoveAt(trackLabel.Count - 1);
            audioTrack.RemoveAt(audioTrack.Count - 1);
        }

        private void audioTrack1_SelectionChanged(object sender, string val)
        {
            int i = audioTrack.IndexOf((FileSCBox)sender);
            Debug.Assert(i >= 0 && i < audioTrack.Count);
            
            FileSCBox track = audioTrack[i];
            if (!track.SelectedSCItem.IsStandard)
                audioConfigControl[i].openAudioFile((string)track.SelectedObject);
            audioConfigControl[i].DelayEnabled = !track.SelectedSCItem.IsStandard;
        }
        
        private void audio1_SomethingChanged(object sender, EventArgs e)
        {
            updatePossibleContainers();
        }

        private void addTrackToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AddTrack();
        }

        private void contextMenuStrip1_Opening(object sender, CancelEventArgs e)
        {
            removeTrackToolStripMenuItem.Enabled = (audioTrack.Count > 1);
        }

        private void removeTrackToolStripMenuItem_Click(object sender, EventArgs e)
        {
            RemoveTrack();
        }

        private void keepInputResolution_CheckedChanged(object sender, EventArgs e)
        {
            if (keepInputResolution.Checked)
            {
                horizontalResolution.Enabled = false;
                autoCrop.Checked = false;
                autoCrop.Enabled = false;
                signalAR.Enabled = false;
                signalAR.Checked = false;
            }
            else
            {
                horizontalResolution.Enabled = true;
                autoCrop.Checked = true;
                autoCrop.Enabled = true;
                signalAR.Enabled = true;
            }
        }
    }
    public class OneClickTool : MeGUI.core.plugins.interfaces.ITool
    {

        

        public string Name
        {
            get { return "One Click Encoder"; }
        }

        public void Run(MainForm info)
        {
            OneClickWindow ocmt = new OneClickWindow(info, info.JobUtil, info.Video.VideoEncoderProvider,
                new AudioEncoderProvider());
            ocmt.Show();
        }

        public Shortcut[] Shortcuts
        {
            get { return new Shortcut[] { Shortcut.CtrlF1 }; }
        }

        

        

        public string ID
        {
            get { return "one_click"; }
        }

        

    }
    public class OneClickPostProcessor
    {
        
        private static LogItem postprocess(MainForm mainForm, Job job)
        {
            if (!(job is D2VIndexJob))
                return null;
            D2VIndexJob ijob = (D2VIndexJob)job;
            if (ijob.PostprocessingProperties == null)
                return null;
            OneClickPostProcessor p = new OneClickPostProcessor(mainForm, ijob);
            return p.postprocess();
        }
        public static JobPostProcessor PostProcessor = new JobPostProcessor(postprocess, "OneClick_postprocessor");

        
        private MainForm mainForm;
        Dictionary<int, string> audioFiles;
        private JobUtil jobUtil;
        private VideoUtil vUtil;
        private D2VIndexJob job;
        private AVCLevels al = new AVCLevels();
        private bool finished = false;
        private bool interlaced = false;
        private DeinterlaceFilter[] filters;
        private LogItem log = new LogItem("OneClick postprocessor", ImageType.Information);
        string qpfile = string.Empty;
        
        public OneClickPostProcessor(MainForm mainForm, D2VIndexJob ijob)
        {
            this.job = ijob;
            this.mainForm = mainForm;
            this.jobUtil = mainForm.JobUtil;
            this.vUtil = new VideoUtil(mainForm);
        }

        public LogItem postprocess()
        {
            audioFiles = vUtil.getAllDemuxedAudio(job.AudioTracks, job.Output, 8);

            fillInAudioInformation();

            log.LogValue("Desired size", job.PostprocessingProperties.OutputSize);
            log.LogValue("Split size", job.PostprocessingProperties.Splitting);

            VideoCodecSettings videoSettings = job.PostprocessingProperties.VideoSettings;

            string videoOutput = Path.Combine(Path.GetDirectoryName(job.Output),
                Path.GetFileNameWithoutExtension(job.Output) + "_Video");
            string muxedOutput = job.PostprocessingProperties.FinalOutput;

            //Open the video
            Dar? dar;
            string videoInput = openVideo(job.Output, job.PostprocessingProperties.DAR, 
                job.PostprocessingProperties.HorizontalOutputResolution, job.PostprocessingProperties.SignalAR, log,
                job.PostprocessingProperties.AvsSettings, job.PostprocessingProperties.AutoDeinterlace, videoSettings, out dar,
                job.PostprocessingProperties.AutoCrop, job.PostprocessingProperties.KeepInputResolution,
                job.PostprocessingProperties.UseChaptersMarks);

            VideoStream myVideo = new VideoStream();
            ulong length;
            double framerate;        
            JobUtil.getInputProperties(out length, out framerate, videoInput);
            myVideo.Input = videoInput;
            myVideo.Output = videoOutput;
            myVideo.NumberOfFrames = length;
            myVideo.Framerate = (decimal)framerate;
            myVideo.DAR = dar;
            myVideo.VideoType = new MuxableType((new VideoEncoderProvider().GetSupportedOutput(videoSettings.EncoderType))[0], videoSettings.Codec);
            myVideo.Settings = videoSettings;
            List<string> intermediateFiles = new List<string>();
            intermediateFiles.Add(videoInput);
            intermediateFiles.Add(job.Output);
            intermediateFiles.AddRange(audioFiles.Values);
            if (!string.IsNullOrEmpty(qpfile))
                intermediateFiles.Add(qpfile);

            if (!string.IsNullOrEmpty(videoInput))
            {
                //Create empty subtitles for muxing (subtitles not supported in one click mode)
                MuxStream[] subtitles = new MuxStream[0];
                JobChain c = vUtil.GenerateJobSeries(myVideo, muxedOutput, job.PostprocessingProperties.AudioJobs, subtitles,
                    job.PostprocessingProperties.ChapterFile, job.PostprocessingProperties.OutputSize,
                    job.PostprocessingProperties.Splitting, job.PostprocessingProperties.Container,
                    job.PostprocessingProperties.PrerenderJob, job.PostprocessingProperties.DirectMuxAudio, log, job.PostprocessingProperties.DeviceOutputType);
                if (c == null)
                {
                    log.Warn("Job creation aborted");
                    return log;
                }

                c = CleanupJob.AddAfter(c, intermediateFiles);
                mainForm.Jobs.addJobsWithDependencies(c);
            }
            return log;
        }

        private void fillInAudioInformation()
        {
            foreach (MuxStream m in job.PostprocessingProperties.DirectMuxAudio)
                m.path = convertTrackNumberToFile(m.path, ref m.delay);

            foreach (AudioJob a in job.PostprocessingProperties.AudioJobs)
            {
                a.Input = convertTrackNumberToFile(a.Input, ref a.Delay);
                if (string.IsNullOrEmpty(a.Output))
                    a.Output = FileUtil.AddToFileName(a.Input, "_audio");
            }
        }

        /// <summary>
        /// if input is a track number (of the form, "::&lt;number&gt;::")
        /// then it returns the file path of that track number. Otherwise,
        /// it returns the string only
        /// </summary>
        /// <param name="input"></param>
        /// <returns></returns>
        private string convertTrackNumberToFile(string input, ref int delay)
        {
            if (input.StartsWith("::") && input.EndsWith("::") && input.Length > 4)
            {
                string sub = input.Substring(2, input.Length - 4);
                try
                {
                    int t = int.Parse(sub);
                    string s = audioFiles[t];
                    delay = PrettyFormatting.getDelay(s) ?? 0;
                    return s;
                }
                catch (Exception)
                {
                    log.Warn(string.Format("Couldn't find audio file for track {0}. Skipping track.", input));
                    return null;
                }
            }

            return input;
        }
        /// <summary>
        /// opens a dgindex script
        /// if the file can be properly opened, auto-cropping is performed, then depending on the AR settings
        /// the proper resolution for automatic resizing, taking into account the derived cropping values
        /// is calculated, and finally the avisynth script is written and its name returned
        /// </summary>
        /// <param name="path">dgindex script</param>
        /// <param name="aspectRatio">aspect ratio selection to be used</param>
        /// <param name="customDAR">custom display aspect ratio for this source</param>
        /// <param name="horizontalResolution">desired horizontal resolution of the output</param>
        /// <param name="settings">the codec settings (used only for x264)</param>
        /// <param name="sarX">pixel aspect ratio X</param>
        /// <param name="sarY">pixel aspect ratio Y</param>
        /// <param name="height">the final height of the video</param>
        /// <param name="signalAR">whether or not ar signalling is to be used for the output 
        /// (depending on this parameter, resizing changes to match the source AR)</param>
        /// <param name="autoCrop">whether or not autoCrop is used for the input</param>
        /// <returns>the name of the AviSynth script created, empty of there was an error</returns>
        private string openVideo(string path, Dar? AR, int horizontalResolution,
            bool signalAR, LogItem log, AviSynthSettings avsSettings, bool autoDeint,
            VideoCodecSettings settings, out Dar? dar, bool autoCrop, bool keepInputResolution, bool useChaptersMarks)
        {
            dar = null;
            CropValues final = new CropValues();
            Dar customDAR;

            IMediaFile d2v = new d2vFile(path);
            IVideoReader reader = d2v.GetVideoReader();
            if (reader.FrameCount < 1)
            {
                log.Error("DGDecode reported 0 frames in this file. This is a fatal error. Please recreate the DGIndex project");
                return "";
            }

            if (!keepInputResolution)
            {
                //Autocrop
                final = Autocrop.autocrop(reader);

                if (signalAR)
                {
                    if (avsSettings.Mod16Method == mod16Method.overcrop)
                        ScriptServer.overcrop(ref final);
                    else if (avsSettings.Mod16Method == mod16Method.mod4Horizontal)
                        ScriptServer.cropMod4Horizontal(ref final);
                    else if (avsSettings.Mod16Method == mod16Method.undercrop)
                        ScriptServer.undercrop(ref final);
                }

                if (autoCrop)
                {
                    bool error = (final.left == -1);
                    if (!error)
                        log.LogValue("Autocrop values", final);
                    else
                    {
                        log.Error("Autocrop failed, aborting now");
                        return "";
                    }
                }
            }

            log.LogValue("Auto-detect aspect ratio now", AR == null);
            //Check if AR needs to be autodetected now
            if (AR == null) // it does
            {
                customDAR = d2v.Info.DAR;
                if (customDAR.ar > 0)
                    log.LogValue("Aspect ratio", customDAR);
                else
                {
                    customDAR = Dar.ITU16x9PAL;
                    log.Warn(string.Format("No aspect ratio found, defaulting to {0}.", customDAR));
                }
            }
            else 
                customDAR = AR.Value;

            if (keepInputResolution)
            {
                horizontalResolution = (int)d2v.Info.Width;
                dar = customDAR;
            }
            else
            {
                // Minimise upsizing
                int sourceHorizontalResolution = (int)d2v.Info.Width - final.right - final.left;
                if (autoCrop)
                    sourceHorizontalResolution = (int)d2v.Info.Width;

                if (horizontalResolution > sourceHorizontalResolution)
                {
                    if (avsSettings.Mod16Method == mod16Method.resize)
                        while (horizontalResolution > sourceHorizontalResolution + 16)
                            horizontalResolution -= 16;
                    else
                        horizontalResolution = sourceHorizontalResolution;
                }
            }

            //Suggest a resolution (taken from AvisynthWindow.suggestResolution_CheckedChanged)
            int scriptVerticalResolution = 0;
            if (keepInputResolution)
            {
                scriptVerticalResolution = (int)d2v.Info.Height;
                log.LogValue("Output resolution", horizontalResolution + "x" + scriptVerticalResolution);
            }
            else
            {
                scriptVerticalResolution = Resolution.suggestResolution(d2v.Info.Height, d2v.Info.Width, (double)customDAR.ar,
                final, horizontalResolution, signalAR, mainForm.Settings.AcceptableAspectErrorPercent, out dar);
                log.LogValue("Output resolution", horizontalResolution + "x" + scriptVerticalResolution);
                if (settings != null && settings is x264Settings) // verify that the video corresponds to the chosen avc level, if not, change the resolution until it does fit
                {
                    x264Settings xs = (x264Settings)settings;
                    if (xs.Level != 15)
                    {
                        AVCLevels al = new AVCLevels();
                        log.LogValue("AVC level", al.getLevels()[xs.Level]);

                        int compliantLevel = 15;
                        while (!this.al.validateAVCLevel(horizontalResolution, scriptVerticalResolution, d2v.Info.FPS, xs, out compliantLevel))
                        { // resolution not profile compliant, reduce horizontal resolution by 16, get the new vertical resolution and try again
                            string levelName = al.getLevels()[xs.Level];
                            horizontalResolution -= 16;
                            scriptVerticalResolution = Resolution.suggestResolution(d2v.Info.Height, d2v.Info.Width, (double)customDAR.ar,
                                final, horizontalResolution, signalAR, mainForm.Settings.AcceptableAspectErrorPercent, out dar);
                        }
                        log.LogValue("Resolution adjusted for AVC Level", horizontalResolution + "x" + scriptVerticalResolution);
                    }
                    if (useChaptersMarks)
                    {
                        qpfile = job.PostprocessingProperties.ChapterFile;
                        if ((Path.GetExtension(qpfile).ToLower()) == ".txt")
                            qpfile = VideoUtil.convertChaptersTextFileTox264QPFile(job.PostprocessingProperties.ChapterFile, d2v.Info.FPS);
                        if (File.Exists(qpfile))
                        {
                            xs.UseQPFile = true;
                            xs.QPFile = qpfile;
                        }
                    }
                }
            }

            //Generate the avs script based on the template
            string inputLine = "#input";
            string deinterlaceLines = "#deinterlace";
            string denoiseLines = "#denoise";
            string cropLine = "#crop";
            string resizeLine = "#resize";

            inputLine = ScriptServer.GetInputLine(path, false, PossibleSources.d2v,
                false, false, false, 0, false);

            log.LogValue("Automatic deinterlacing", autoDeint);
            if (autoDeint)
            {
                string d2vPath = path;
                SourceDetector sd = new SourceDetector(inputLine, d2vPath, false,
                    mainForm.Settings.SourceDetectorSettings,
                    new UpdateSourceDetectionStatus(analyseUpdate),
                    new FinishedAnalysis(finishedAnalysis));
                finished = false;
                sd.analyse();
                waitTillAnalyseFinished();
                deinterlaceLines = filters[0].Script;
                log.LogValue("Deinterlacing used", deinterlaceLines);
            }

            inputLine = ScriptServer.GetInputLine(path, interlaced, PossibleSources.d2v, avsSettings.ColourCorrect, avsSettings.MPEG2Deblock, false, 0, false);

            if (autoCrop)
                cropLine = ScriptServer.GetCropLine(true, final);
            else 
                cropLine = ScriptServer.GetCropLine(false, final);

            denoiseLines = ScriptServer.GetDenoiseLines(avsSettings.Denoise, (DenoiseFilterType)avsSettings.DenoiseMethod);

            if (!keepInputResolution)
                resizeLine = ScriptServer.GetResizeLine(!signalAR || avsSettings.Mod16Method == mod16Method.resize, horizontalResolution, scriptVerticalResolution, (ResizeFilterType)avsSettings.ResizeMethod);

            string newScript = ScriptServer.CreateScriptFromTemplate(avsSettings.Template, inputLine, cropLine, resizeLine, denoiseLines, deinterlaceLines);
                        
            if (dar.HasValue)
                newScript = string.Format("global MeGUI_darx = {0}\r\nglobal MeGUI_dary = {1}\r\n{2}", dar.Value.X, dar.Value.Y, newScript);

            log.LogValue("Generated Avisynth script", newScript);
            try
            {
                StreamWriter sw = new StreamWriter(Path.ChangeExtension(path, ".avs"),false, System.Text.Encoding.Default);
                sw.Write(newScript);
                sw.Close();
            }
            catch (IOException i)
            {
                log.LogValue("Error saving AviSynth script", i, ImageType.Error);
                return "";
            }
            return Path.ChangeExtension(path, ".avs");
        }
        
        public void finishedAnalysis(SourceInfo info, bool error, string errorMessage)
        {
            if (error)
            {
                MessageBox.Show(errorMessage, "Source detection failed", MessageBoxButtons.OK, MessageBoxIcon.Error);
                filters = new DeinterlaceFilter[] {
                    new DeinterlaceFilter("Error", "#An error occurred in source detection. Doing no processing")};
            }
            else if (info.sourceType == SourceType.NOT_ENOUGH_SECTIONS)
            {
                MessageBox.Show("Could not find enough useful sections to determine source type for " + job.Input, "Source detection failed", MessageBoxButtons.OK, MessageBoxIcon.Error);
                filters = new DeinterlaceFilter[] {
                    new DeinterlaceFilter("Error", "#Not enough useful sections for source detection. Doing no processing")};
            }
            else
                this.filters = ScriptServer.GetDeinterlacers(info).ToArray();
            interlaced = (info.sourceType != SourceType.PROGRESSIVE);
            finished = true;
        }

        public void analyseUpdate(int amountDone, int total)
        { /*Do nothing*/ }

        private void waitTillAnalyseFinished()
        {
            while (!finished)
            {
                Thread.Sleep(500);
            }
        }
    }
}
