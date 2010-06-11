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

#if UNUSED
using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using System.Text;
using System.Diagnostics;
using System.Threading;

namespace MeGUI.core.details
{
    public class MeGUIInfo
    {
        #region variable declaration
        private MainForm mainForm;
        private bool restart = false;
        private Dictionary<string, CommandlineUpgradeData> filesToReplace = new Dictionary<string, CommandlineUpgradeData>();
        private DialogManager dialogManager;
        private string path; // path the program was started from
        private int jobNr = 1; // number of jobs in the queue
        private bool isEncoding = false, queueEncoding = false, paused = false; // encoding status and whether or not we're in queue encoding mode
        private Dictionary<string, Job> jobs; //storage for all the jobs and profiles known to the system
        private List<Job> skipJobs; // contains jobs to be skipped (chained with a previously errored out job)
        private IJobProcessor currentProcessor;
        private CommandLineGenerator gen; // class that generates commandlines
        private ProgressWindow pw; // window that shows the encoding progress
        private VideoPlayer player; // window that shows a preview of the video
        public StringBuilder logBuilder; // made public so that system jobs can write to it
        private int creditsStartFrame = -1, introEndFrame = -1;
        private int parX = -1, parY = -1;
        private MediaFileFactory mediaFileFactory;
        private PackageSystem packageSystem;
        private JobUtil jobUtil;
        private int lastSelectedAudioTrackNumber = 0;
        private MuxProvider muxProvider = new MuxProvider();
        private VideoEncoderProvider videoEncoderProvider;
        private AudioEncoderProvider audioEncoderProvider;
        private MeGUISettings settings;
        private ProfileManager profileManager;
        private BitrateCalculator calc;
        private AudioStream[] audioStreams;

        private CodecManager codecs;
        #endregion
        #region start and end
        public void handleCommandline(CommandlineParser parser)
        {
            foreach (string file in parser.failedUpgrades)
                System.Windows.Forms.MessageBox.Show("Failed to upgrade '" + file + "'.", "Upgrade failed", MessageBoxButtons.OK, MessageBoxIcon.Error);
#warning turn the UpdateWindow here into an ITool
            if (parser.upgradeData.Count > 0)
            {
                UpdateWindow update = new UpdateWindow(this, Settings);
                foreach (string file in parser.upgradeData.Keys)
                    update.UpdateVersionNumber(file, parser.upgradeData[file]);
                update.SaveSettings();
            }
        }

        /// <summary>
        /// default constructor
        /// initializes all the GUI components, initializes the internal objects and makes a default selection for all the GUI dropdowns
        /// In addition, all the jobs and profiles are being loaded from the harddisk
        /// </summary>
        public MeGUIInfo()
        {
            this.codecs = new CodecManager();
            this.gen = new CommandLineGenerator();
            this.path = System.Windows.Forms.Application.StartupPath;
            this.jobs = new Dictionary<string, Job>();
            this.skipJobs = new List<Job>();
            this.logBuilder = new StringBuilder();
            this.jobUtil = new JobUtil(this);
            this.settings = new MeGUISettings();
            this.calc = new BitrateCalculator();
            audioStreams = new AudioStream[2];
            audioStreams[0].path = "";
            audioStreams[0].output = "";
            audioStreams[0].settings = null;
            audioStreams[1].path = "";
            audioStreams[1].output = "";
            audioStreams[1].settings = null;
            this.videoEncoderProvider = new VideoEncoderProvider();
            this.audioEncoderProvider = new AudioEncoderProvider();
            this.profileManager = new ProfileManager(this.path);
            this.profileManager.LoadProfiles(videoProfile, audioProfile);
            this.loadSettings();
            this.loadJobs();
            this.dialogManager = new DialogManager(this);
#warning refactor menus
            int index = menuItem1.MenuItems.Count;
            foreach (IMuxing muxer in PackageSystem.MuxerProviders.Values)
            {
                MenuItem newMenuItem = new MenuItem();
                newMenuItem.Text = muxer.Name;
                newMenuItem.Tag = muxer;
                newMenuItem.Index = index;
                index++;
                menuItem1.MenuItems.Add(newMenuItem);
                newMenuItem.Click += new System.EventHandler(this.mnuMuxer_Click);
            }
            index = mnuTools.MenuItems.Count;
            foreach (ITool tool in PackageSystem.Tools.Values)
            {
                MenuItem newMenuItem = new MenuItem();
                newMenuItem.Text = tool.Name;
                newMenuItem.Tag = tool;
                newMenuItem.Index = index;
                index++;
                mnuTools.MenuItems.Add(newMenuItem);
                newMenuItem.Click += new System.EventHandler(this.mnuTool_Click);
            }
            //MessageBox.Show(String.Join("|", this.GetType().Assembly.GetManifestResourceNames()));
            using (TextReader r = new StreamReader(this.GetType().Assembly.GetManifestResourceStream("MeGUI.Changelog.txt")))
            {
                mainForm.Changelog.Text = r.ReadToEnd();
            }
        }

        private static Mutex mySingleInstanceMutex = new Mutex(true, "MeGUI_D9D0C224154B489784998BF97B9C9414");

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main(string[] args)
        {
            System.Windows.Forms.Application.EnableVisualStyles();
            if (!mySingleInstanceMutex.WaitOne(0, false))
            {
                if (DialogResult.Yes != MessageBox.Show("Running MeGUI instance detected!\n\rThere's not really much point in running multiple copies of MeGUI, and it can cause problems.\n\rDo You still want to run yet another MeGUI instance?", "Running MeGUI instance detected", MessageBoxButtons.YesNo, MessageBoxIcon.Warning))
                    return;
            }
            Application.ThreadException += new System.Threading.ThreadExceptionEventHandler(Application_ThreadException);
            Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException);
            CommandlineParser parser = new CommandlineParser();
            parser.Parse(args);
            MeGUIInfo info = new MeGUIInfo();
            MainForm mainForm = new MainForm();
            mainForm.AttachInfo(info);
            info.handleCommandline(parser);
            if (parser.start)
                Application.Run(mainForm);
        }

        static void Application_ThreadException(object sender, System.Threading.ThreadExceptionEventArgs e)
        {
            MessageBox.Show("MeGUI encountered a fatal error and may not be able to proceed. Reason: " + e.Exception.Message
                + " Source of exception: " + e.Exception.Source + " stacktrace: " + e.Exception.StackTrace, "Fatal error",
                MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
        private void runRestarter()
        {
            if (filesToReplace.Keys.Count == 0)
                return;
            Process proc = new Process();
            ProcessStartInfo pstart = new ProcessStartInfo();
            pstart.FileName = Path.Combine(Application.StartupPath, "updatecopier.exe");
            foreach (string file in filesToReplace.Keys)
            {
                pstart.Arguments += string.Format("--component \"{0}\" \"{1}\" ", file, filesToReplace[file].newVersion);
                for (int i = 0; i < filesToReplace[file].filename.Count; i++)
                {
                    pstart.Arguments += string.Format("\"{0}\" \"{1}\" ",
                       filesToReplace[file].filename[i],
                       filesToReplace[file].tempFilename[i]);
                }
            }
            if (restart)
                pstart.Arguments += "--restart ";
            else
                pstart.Arguments += "--app ";
            pstart.Arguments += "\"" + Application.ExecutablePath + "\"";

            pstart.CreateNoWindow = true;
            pstart.UseShellExecute = false;
            proc.StartInfo = pstart;
            if (!proc.Start())
                MessageBox.Show("Couldn't run updater.", "Couldn't run updater.", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
        #endregion
        #region properties
        public PackageSystem PackageSystem
        {
            get { return packageSystem; }
        }
        public bool Restart
        {
            get { return restart; }
            set { restart = value; }
        }
        public DialogManager DialogManager
        {
            get { return dialogManager; }
        }
        /// <summary>
        /// gets / sets if we are in queue encoding mode
        /// </summary>
        public bool QueueEncoding
        {
            get { return queueEncoding; }
            set { queueEncoding = value; }
        }
        public MeGUISettings Settings
        {
            get { return settings; }
        }

        /// <summary>
        /// gets the path from where MeGUI was launched
        /// </summary>
        public string MeGUIPath
        {
            get { return this.path; }
        }
        /// <summary>
        /// gets  / sets the currently selected audiostream
        /// </summary>
        public ProfileManager Profiles
        {
            get { return profileManager; }
        }
        #endregion
    }
}
#endif