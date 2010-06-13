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
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.Xml.Serialization;

using MeGUI.core.details;
using MeGUI.core.gui;
using MeGUI.core.plugins.interfaces;
using MeGUI.core.util;
using MeGUI.packages.tools.besplitter;
using MeGUI.packages.tools.cutter;
using MeGUI.packages.tools.hdbdextractor;

namespace MeGUI
{
    public delegate void UpdateGUIStatusCallback(StatusUpdate su); // catches the UpdateGUI events fired from the encoder
    public enum FileType
    {
        VIDEOINPUT, AUDIOINPUT, DGINDEX, FFMSINDEX, OTHERVIDEO, ZIPPED_PROFILES, NONE
    };
    public enum ProcessingStatus
    {
        DOINGNOTHING, RUNNING, PAUSED, STOPPING
    }

    /// <summary>
    /// Form1 is the main GUI of the program
    /// it contains all the elements required to start encoding and contains the application intelligence as well.
    /// </summary>
    public class MainForm : System.Windows.Forms.Form
    {
        // This instance is to be used by the serializers that can't be passed a MainForm as a parameter
        public static MainForm Instance;

        
        private List<string> filesToDeleteOnClosing = new List<string>();
        private System.Windows.Forms.TabPage inputTab;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.MainMenu mainMenu1;
        private System.Windows.Forms.MenuItem mnuFile;
        private System.Windows.Forms.MenuItem mnuFileExit;
        private System.Windows.Forms.MenuItem mnuTools;
        private System.Windows.Forms.OpenFileDialog openFileDialog;
        private System.Windows.Forms.SaveFileDialog saveFileDialog;
        private System.Windows.Forms.MenuItem mnuView;
        private System.Windows.Forms.MenuItem progressMenu;
        private MenuItem mnuViewMinimizeToTray;
        private NotifyIcon trayIcon;
        private System.Windows.Forms.MenuItem mnuMuxers;
        private MenuItem mnuFileOpen;
        private ContextMenuStrip trayMenu;
        private ToolStripMenuItem openMeGUIToolStripMenuItem;
        private ToolStripSeparator toolStripSeparator1;
        private ToolStripMenuItem exitMeGUIToolStripMenuItem;
        private MenuItem mnuFileImport;
        private MenuItem mnuFileExport;
        private MenuItem mnuToolsAdaptiveMuxer;
        
        private AudioEncodingComponent audioEncodingComponent1;
        private VideoEncodingComponent videoEncodingComponent1;
        private TabPage tabPage2;
        private JobControl jobControl1;
        private MenuItem mnuHelp;
        private MenuItem mnuChangelog;
        private MenuItem menuItem1;
        private MenuItem createNewWorker;
        private MenuItem menuItem6;
        private MenuItem workersMenu;
        private MenuItem showAllWorkers;
        private MenuItem hideAllWorkers;
        private MenuItem separator;
        private MenuItem menuItem2;
        private MenuItem viewSummary;
        private MenuItem showAllProgressWindows;
        private MenuItem hideAllProgressWindows;
        private MenuItem separator2;
        private MenuItem menuItem7;

        private List<Form> allForms = new List<Form>();
        private MenuItem menuItem3;
        private MenuItem mnuDoc;
        private MenuItem mnuWebsite;
        private MenuItem mnuHome;
        private MenuItem mnuForum;
        private MenuItem mnuBugTracker;
        private MenuItem mnuFeaturesReq;
        private MenuItem mnuOptions;
        private MenuItem mnuOptionsSettings;
        private TabPage logTab;
        private LogTree logTree1;
        private SplitContainer splitContainer1;
        private FlowLayoutPanel flowLayoutPanel1;
        private FlowLayoutPanel flowLayoutPanel2;
        private Button autoEncodeButton;
        private Button resetButton;
        private Button OneClickEncButton;
        private Button helpButton1;
        private SplitContainer splitContainer2;
        private MenuItem mnutoolsD2VCreator;
        private List<Form> formsToReopen = new List<Form>();

        public bool IsHiddenMode { get { return trayIcon.Visible; } }

        public void RegisterForm(Form f)
        {
        }

        public void DeleteOnClosing(string file)
        {
            filesToDeleteOnClosing.Add(file);
        }

        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;
        
        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.inputTab = new System.Windows.Forms.TabPage();
            this.splitContainer2 = new System.Windows.Forms.SplitContainer();
            this.videoEncodingComponent1 = new MeGUI.VideoEncodingComponent();
            this.audioEncodingComponent1 = new MeGUI.AudioEncodingComponent();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.flowLayoutPanel1 = new System.Windows.Forms.FlowLayoutPanel();
            this.resetButton = new System.Windows.Forms.Button();
            this.helpButton1 = new System.Windows.Forms.Button();
            this.flowLayoutPanel2 = new System.Windows.Forms.FlowLayoutPanel();
            this.autoEncodeButton = new System.Windows.Forms.Button();
            this.OneClickEncButton = new System.Windows.Forms.Button();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.jobControl1 = new MeGUI.core.details.JobControl();
            this.logTab = new System.Windows.Forms.TabPage();
            this.logTree1 = new MeGUI.core.gui.LogTree();
            this.mnuMuxers = new System.Windows.Forms.MenuItem();
            this.mnuToolsAdaptiveMuxer = new System.Windows.Forms.MenuItem();
            this.mainMenu1 = new System.Windows.Forms.MainMenu(this.components);
            this.mnuFile = new System.Windows.Forms.MenuItem();
            this.mnuFileOpen = new System.Windows.Forms.MenuItem();
            this.mnuFileImport = new System.Windows.Forms.MenuItem();
            this.mnuFileExport = new System.Windows.Forms.MenuItem();
            this.mnuFileExit = new System.Windows.Forms.MenuItem();
            this.mnuView = new System.Windows.Forms.MenuItem();
            this.progressMenu = new System.Windows.Forms.MenuItem();
            this.showAllProgressWindows = new System.Windows.Forms.MenuItem();
            this.hideAllProgressWindows = new System.Windows.Forms.MenuItem();
            this.separator2 = new System.Windows.Forms.MenuItem();
            this.menuItem7 = new System.Windows.Forms.MenuItem();
            this.mnuViewMinimizeToTray = new System.Windows.Forms.MenuItem();
            this.menuItem1 = new System.Windows.Forms.MenuItem();
            this.createNewWorker = new System.Windows.Forms.MenuItem();
            this.menuItem6 = new System.Windows.Forms.MenuItem();
            this.workersMenu = new System.Windows.Forms.MenuItem();
            this.showAllWorkers = new System.Windows.Forms.MenuItem();
            this.hideAllWorkers = new System.Windows.Forms.MenuItem();
            this.separator = new System.Windows.Forms.MenuItem();
            this.menuItem2 = new System.Windows.Forms.MenuItem();
            this.menuItem3 = new System.Windows.Forms.MenuItem();
            this.viewSummary = new System.Windows.Forms.MenuItem();
            this.mnuTools = new System.Windows.Forms.MenuItem();
            this.mnutoolsD2VCreator = new System.Windows.Forms.MenuItem();
            this.mnuOptions = new System.Windows.Forms.MenuItem();
            this.mnuOptionsSettings = new System.Windows.Forms.MenuItem();
            this.mnuHelp = new System.Windows.Forms.MenuItem();
            this.mnuChangelog = new System.Windows.Forms.MenuItem();
            this.mnuDoc = new System.Windows.Forms.MenuItem();
            this.mnuWebsite = new System.Windows.Forms.MenuItem();
            this.mnuHome = new System.Windows.Forms.MenuItem();
            this.mnuForum = new System.Windows.Forms.MenuItem();
            this.mnuBugTracker = new System.Windows.Forms.MenuItem();
            this.mnuFeaturesReq = new System.Windows.Forms.MenuItem();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.saveFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.trayIcon = new System.Windows.Forms.NotifyIcon(this.components);
            this.trayMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.openMeGUIToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.exitMeGUIToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.tabControl1.SuspendLayout();
            this.inputTab.SuspendLayout();
            this.splitContainer2.Panel1.SuspendLayout();
            this.splitContainer2.Panel2.SuspendLayout();
            this.splitContainer2.SuspendLayout();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.flowLayoutPanel1.SuspendLayout();
            this.flowLayoutPanel2.SuspendLayout();
            this.tabPage2.SuspendLayout();
            this.logTab.SuspendLayout();
            this.trayMenu.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.inputTab);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Controls.Add(this.logTab);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(508, 499);
            this.tabControl1.TabIndex = 0;
            // 
            // inputTab
            // 
            this.inputTab.BackColor = System.Drawing.Color.Transparent;
            this.inputTab.Controls.Add(this.splitContainer2);
            this.inputTab.Controls.Add(this.splitContainer1);
            this.inputTab.Location = new System.Drawing.Point(4, 22);
            this.inputTab.Name = "inputTab";
            this.inputTab.Size = new System.Drawing.Size(500, 473);
            this.inputTab.TabIndex = 0;
            this.inputTab.Text = "Input";
            this.inputTab.UseVisualStyleBackColor = true;
            // 
            // splitContainer2
            // 
            this.splitContainer2.BackColor = System.Drawing.SystemColors.ControlLight;
            this.splitContainer2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer2.Location = new System.Drawing.Point(0, 0);
            this.splitContainer2.Name = "splitContainer2";
            this.splitContainer2.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer2.Panel1
            // 
            this.splitContainer2.Panel1.Controls.Add(this.videoEncodingComponent1);
            // 
            // splitContainer2.Panel2
            // 
            this.splitContainer2.Panel2.Controls.Add(this.audioEncodingComponent1);
            this.splitContainer2.Size = new System.Drawing.Size(500, 441);
            this.splitContainer2.SplitterDistance = 168;
            this.splitContainer2.TabIndex = 4;
            // 
            // videoEncodingComponent1
            // 
            this.videoEncodingComponent1.BackColor = System.Drawing.SystemColors.Control;
            this.videoEncodingComponent1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.videoEncodingComponent1.FileType = "";
            this.videoEncodingComponent1.Location = new System.Drawing.Point(0, 0);
            this.videoEncodingComponent1.MinimumSize = new System.Drawing.Size(500, 168);
            this.videoEncodingComponent1.Name = "videoEncodingComponent1";
            this.videoEncodingComponent1.PrerenderJob = false;
            this.videoEncodingComponent1.Size = new System.Drawing.Size(500, 168);
            this.videoEncodingComponent1.TabIndex = 0;
            this.videoEncodingComponent1.VideoInput = "";
            this.videoEncodingComponent1.VideoOutput = "";
            // 
            // audioEncodingComponent1
            // 
            this.audioEncodingComponent1.AutoScroll = true;
            this.audioEncodingComponent1.BackColor = System.Drawing.SystemColors.Control;
            this.audioEncodingComponent1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.audioEncodingComponent1.Location = new System.Drawing.Point(0, 0);
            this.audioEncodingComponent1.MinimumSize = new System.Drawing.Size(400, 192);
            this.audioEncodingComponent1.Name = "audioEncodingComponent1";
            this.audioEncodingComponent1.Size = new System.Drawing.Size(500, 269);
            this.audioEncodingComponent1.TabIndex = 1;
            // 
            // splitContainer1
            // 
            this.splitContainer1.BackColor = System.Drawing.SystemColors.Control;
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.splitContainer1.Location = new System.Drawing.Point(0, 441);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.flowLayoutPanel1);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.flowLayoutPanel2);
            this.splitContainer1.Size = new System.Drawing.Size(500, 32);
            this.splitContainer1.SplitterDistance = 244;
            this.splitContainer1.TabIndex = 3;
            // 
            // flowLayoutPanel1
            // 
            this.flowLayoutPanel1.BackColor = System.Drawing.SystemColors.Control;
            this.flowLayoutPanel1.Controls.Add(this.resetButton);
            this.flowLayoutPanel1.Controls.Add(this.helpButton1);
            this.flowLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.flowLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.flowLayoutPanel1.Name = "flowLayoutPanel1";
            this.flowLayoutPanel1.Size = new System.Drawing.Size(244, 32);
            this.flowLayoutPanel1.TabIndex = 0;
            // 
            // resetButton
            // 
            this.resetButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.resetButton.AutoSize = true;
            this.resetButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.resetButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.resetButton.Location = new System.Drawing.Point(3, 3);
            this.resetButton.Name = "resetButton";
            this.resetButton.Size = new System.Drawing.Size(45, 23);
            this.resetButton.TabIndex = 4;
            this.resetButton.Text = "Reset";
            this.resetButton.Click += new System.EventHandler(this.resetButton_Click);
            // 
            // helpButton1
            // 
            this.helpButton1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.helpButton1.AutoSize = true;
            this.helpButton1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.helpButton1.Location = new System.Drawing.Point(54, 3);
            this.helpButton1.Name = "helpButton1";
            this.helpButton1.Size = new System.Drawing.Size(38, 23);
            this.helpButton1.TabIndex = 5;
            this.helpButton1.Text = "Help";
            this.helpButton1.Click += new System.EventHandler(this.HelpButton_Click);
            // 
            // flowLayoutPanel2
            // 
            this.flowLayoutPanel2.AutoSize = true;
            this.flowLayoutPanel2.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.flowLayoutPanel2.BackColor = System.Drawing.SystemColors.Control;
            this.flowLayoutPanel2.Controls.Add(this.autoEncodeButton);
            this.flowLayoutPanel2.Controls.Add(this.OneClickEncButton);
            this.flowLayoutPanel2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.flowLayoutPanel2.FlowDirection = System.Windows.Forms.FlowDirection.RightToLeft;
            this.flowLayoutPanel2.Location = new System.Drawing.Point(0, 0);
            this.flowLayoutPanel2.Name = "flowLayoutPanel2";
            this.flowLayoutPanel2.Size = new System.Drawing.Size(252, 32);
            this.flowLayoutPanel2.TabIndex = 5;
            // 
            // autoEncodeButton
            // 
            this.autoEncodeButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.autoEncodeButton.AutoSize = true;
            this.autoEncodeButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.autoEncodeButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.autoEncodeButton.Location = new System.Drawing.Point(174, 3);
            this.autoEncodeButton.Name = "autoEncodeButton";
            this.autoEncodeButton.Size = new System.Drawing.Size(75, 23);
            this.autoEncodeButton.TabIndex = 2;
            this.autoEncodeButton.Text = "AutoEncode";
            this.autoEncodeButton.Click += new System.EventHandler(this.autoEncodeButton_Click);
            // 
            // OneClickEncButton
            // 
            this.OneClickEncButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.OneClickEncButton.AutoSize = true;
            this.OneClickEncButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.OneClickEncButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.OneClickEncButton.Location = new System.Drawing.Point(106, 3);
            this.OneClickEncButton.Name = "OneClickEncButton";
            this.OneClickEncButton.Size = new System.Drawing.Size(62, 23);
            this.OneClickEncButton.TabIndex = 3;
            this.OneClickEncButton.Text = "One-Click";
            this.OneClickEncButton.Click += new System.EventHandler(this.OneClickEncButton_Click);
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.jobControl1);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Size = new System.Drawing.Size(500, 473);
            this.tabPage2.TabIndex = 12;
            this.tabPage2.Text = "Queue";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // jobControl1
            // 
            this.jobControl1.BackColor = System.Drawing.SystemColors.Control;
            this.jobControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.jobControl1.Location = new System.Drawing.Point(0, 0);
            this.jobControl1.Name = "jobControl1";
            this.jobControl1.Size = new System.Drawing.Size(192, 74);
            this.jobControl1.TabIndex = 0;
            // 
            // logTab
            // 
            this.logTab.Controls.Add(this.logTree1);
            this.logTab.Location = new System.Drawing.Point(4, 22);
            this.logTab.Name = "logTab";
            this.logTab.Size = new System.Drawing.Size(500, 473);
            this.logTab.TabIndex = 13;
            this.logTab.Text = "Log";
            this.logTab.UseVisualStyleBackColor = true;
            // 
            // logTree1
            // 
            this.logTree1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.logTree1.Location = new System.Drawing.Point(0, 0);
            this.logTree1.Name = "logTree1";
            this.logTree1.Size = new System.Drawing.Size(192, 74);
            this.logTree1.TabIndex = 0;
            // 
            // mnuMuxers
            // 
            this.mnuMuxers.Index = 0;
            this.mnuMuxers.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.mnuToolsAdaptiveMuxer});
            this.mnuMuxers.Text = "Muxer";
            // 
            // mnuToolsAdaptiveMuxer
            // 
            this.mnuToolsAdaptiveMuxer.Index = 0;
            this.mnuToolsAdaptiveMuxer.Text = "Adaptive Muxer";
            this.mnuToolsAdaptiveMuxer.Click += new System.EventHandler(this.mnuToolsAdaptiveMuxer_Click);
            // 
            // mainMenu1
            // 
            this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.mnuFile,
            this.mnuView,
            this.menuItem1,
            this.mnuTools,
            this.mnuOptions,
            this.mnuHelp});
            // 
            // mnuFile
            // 
            this.mnuFile.Index = 0;
            this.mnuFile.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.mnuFileOpen,
            this.mnuFileImport,
            this.mnuFileExport,
            this.mnuFileExit});
            this.mnuFile.Text = "&File";
            // 
            // mnuFileOpen
            // 
            this.mnuFileOpen.Index = 0;
            this.mnuFileOpen.Shortcut = System.Windows.Forms.Shortcut.CtrlO;
            this.mnuFileOpen.Text = "&Open";
            this.mnuFileOpen.Click += new System.EventHandler(this.mnuFileOpen_Click);
            // 
            // mnuFileImport
            // 
            this.mnuFileImport.Index = 1;
            this.mnuFileImport.Shortcut = System.Windows.Forms.Shortcut.CtrlI;
            this.mnuFileImport.Text = "&Import Presets";
            this.mnuFileImport.Click += new System.EventHandler(this.mnuFileImport_Click);
            // 
            // mnuFileExport
            // 
            this.mnuFileExport.Index = 2;
            this.mnuFileExport.Shortcut = System.Windows.Forms.Shortcut.CtrlE;
            this.mnuFileExport.Text = "&Export Presets";
            this.mnuFileExport.Click += new System.EventHandler(this.mnuFileExport_Click);
            // 
            // mnuFileExit
            // 
            this.mnuFileExit.Index = 3;
            this.mnuFileExit.Shortcut = System.Windows.Forms.Shortcut.CtrlX;
            this.mnuFileExit.Text = "E&xit";
            this.mnuFileExit.Click += new System.EventHandler(this.mnuFileExit_Click);
            // 
            // mnuView
            // 
            this.mnuView.Index = 1;
            this.mnuView.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.progressMenu,
            this.mnuViewMinimizeToTray});
            this.mnuView.Text = "&View";
            this.mnuView.Popup += new System.EventHandler(this.mnuView_Popup);
            // 
            // progressMenu
            // 
            this.progressMenu.Index = 0;
            this.progressMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.showAllProgressWindows,
            this.hideAllProgressWindows,
            this.separator2,
            this.menuItem7});
            this.progressMenu.Text = "&Process Status";
            // 
            // showAllProgressWindows
            // 
            this.showAllProgressWindows.Index = 0;
            this.showAllProgressWindows.Text = "Show all";
            this.showAllProgressWindows.Click += new System.EventHandler(this.showAllProgressWindows_Click);
            // 
            // hideAllProgressWindows
            // 
            this.hideAllProgressWindows.Index = 1;
            this.hideAllProgressWindows.Text = "Hide all";
            this.hideAllProgressWindows.Click += new System.EventHandler(this.hideAllProgressWindows_Click);
            // 
            // separator2
            // 
            this.separator2.Index = 2;
            this.separator2.Text = "-";
            // 
            // menuItem7
            // 
            this.menuItem7.Index = 3;
            this.menuItem7.Text = "(List of progress windows goes here)";
            // 
            // mnuViewMinimizeToTray
            // 
            this.mnuViewMinimizeToTray.Index = 1;
            this.mnuViewMinimizeToTray.Shortcut = System.Windows.Forms.Shortcut.CtrlM;
            this.mnuViewMinimizeToTray.Text = "&Minimize to Tray";
            this.mnuViewMinimizeToTray.Click += new System.EventHandler(this.mnuViewMinimizeToTray_Click);
            // 
            // menuItem1
            // 
            this.menuItem1.Index = 2;
            this.menuItem1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.createNewWorker,
            this.menuItem6,
            this.workersMenu,
            this.viewSummary});
            this.menuItem1.Text = "Workers";
            this.menuItem1.Popup += new System.EventHandler(this.showAllWorkers_Popup);
            // 
            // createNewWorker
            // 
            this.createNewWorker.Index = 0;
            this.createNewWorker.Text = "Create new worker";
            this.createNewWorker.Click += new System.EventHandler(this.createNewWorker_Click);
            // 
            // menuItem6
            // 
            this.menuItem6.Index = 1;
            this.menuItem6.Text = "-";
            // 
            // workersMenu
            // 
            this.workersMenu.Index = 2;
            this.workersMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.showAllWorkers,
            this.hideAllWorkers,
            this.separator,
            this.menuItem2,
            this.menuItem3});
            this.workersMenu.Text = "Workers";
            // 
            // showAllWorkers
            // 
            this.showAllWorkers.Index = 0;
            this.showAllWorkers.Text = "Show all";
            this.showAllWorkers.Click += new System.EventHandler(this.showAllWorkers_Click);
            // 
            // hideAllWorkers
            // 
            this.hideAllWorkers.Index = 1;
            this.hideAllWorkers.Text = "Hide all";
            this.hideAllWorkers.Click += new System.EventHandler(this.hideAllWorkers_Click);
            // 
            // separator
            // 
            this.separator.Index = 2;
            this.separator.Text = "-";
            // 
            // menuItem2
            // 
            this.menuItem2.Index = 3;
            this.menuItem2.Text = "(List of workers goes here)";
            // 
            // menuItem3
            // 
            this.menuItem3.Index = 4;
            this.menuItem3.Text = "ao";
            // 
            // viewSummary
            // 
            this.viewSummary.Index = 3;
            this.viewSummary.Text = "Worker summary";
            this.viewSummary.Click += new System.EventHandler(this.viewSummary_Click);
            // 
            // mnuTools
            // 
            this.mnuTools.Index = 3;
            this.mnuTools.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.mnuMuxers,
            this.mnutoolsD2VCreator});
            this.mnuTools.Shortcut = System.Windows.Forms.Shortcut.CtrlT;
            this.mnuTools.Text = "&Tools";
            // 
            // mnutoolsD2VCreator
            // 
            this.mnutoolsD2VCreator.Index = 1;
            this.mnutoolsD2VCreator.Shortcut = System.Windows.Forms.Shortcut.CtrlF2;
            this.mnutoolsD2VCreator.Text = "File Indexer";
            this.mnutoolsD2VCreator.Click += new System.EventHandler(this.menuItem5_Click);
            // 
            // mnuOptions
            // 
            this.mnuOptions.Index = 4;
            this.mnuOptions.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.mnuOptionsSettings});
            this.mnuOptions.Text = "&Options";
            // 
            // mnuOptionsSettings
            // 
            this.mnuOptionsSettings.Index = 0;
            this.mnuOptionsSettings.Shortcut = System.Windows.Forms.Shortcut.CtrlS;
            this.mnuOptionsSettings.Text = "Settings";
            this.mnuOptionsSettings.Click += new System.EventHandler(this.mnuOptionsSettings_Click);
            // 
            // mnuHelp
            // 
            this.mnuHelp.Index = 5;
            this.mnuHelp.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.mnuChangelog,
            this.mnuDoc,
            this.mnuWebsite});
            this.mnuHelp.Text = "&Help";
            // 
            // mnuChangelog
            // 
            this.mnuChangelog.Index = 0;
            this.mnuChangelog.Text = "Changelog";
            this.mnuChangelog.Click += new System.EventHandler(this.mnuChangelog_Click);
            // 
            // mnuDoc
            // 
            this.mnuDoc.Index = 1;
            this.mnuDoc.Text = "Wiki - User Guides";
            this.mnuDoc.Click += new System.EventHandler(this.mnuDoc_Click);
            // 
            // mnuWebsite
            // 
            this.mnuWebsite.Index = 2;
            this.mnuWebsite.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.mnuHome,
            this.mnuForum,
            this.mnuBugTracker,
            this.mnuFeaturesReq});
            this.mnuWebsite.Text = "Website";
            // 
            // mnuHome
            // 
            this.mnuHome.Index = 0;
            this.mnuHome.Text = "Homepage";
            this.mnuHome.Click += new System.EventHandler(this.mnuHome_Click);
            // 
            // mnuForum
            // 
            this.mnuForum.Index = 1;
            this.mnuForum.Text = "Forum";
            this.mnuForum.Click += new System.EventHandler(this.mnuForum_Click);
            // 
            // mnuBugTracker
            // 
            this.mnuBugTracker.Index = 2;
            this.mnuBugTracker.Text = "Bugs Tracker";
            this.mnuBugTracker.Click += new System.EventHandler(this.mnuBugTracker_Click);
            // 
            // mnuFeaturesReq
            // 
            this.mnuFeaturesReq.Index = 3;
            this.mnuFeaturesReq.Text = "Feature Requests";
            this.mnuFeaturesReq.Click += new System.EventHandler(this.mnuFeaturesReq_Click);
            // 
            // trayIcon
            // 
            this.trayIcon.BalloonTipText = "meGUI is still working...";
            this.trayIcon.BalloonTipTitle = "meGUI";
            this.trayIcon.ContextMenuStrip = this.trayMenu;
            this.trayIcon.Text = "MeGUI";
            this.trayIcon.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.trayIcon_MouseDoubleClick);
            // 
            // trayMenu
            // 
            this.trayMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.openMeGUIToolStripMenuItem,
            this.toolStripSeparator1,
            this.exitMeGUIToolStripMenuItem});
            this.trayMenu.Name = "trayMenu";
            this.trayMenu.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional;
            this.trayMenu.Size = new System.Drawing.Size(143, 54);
            // 
            // openMeGUIToolStripMenuItem
            // 
            this.openMeGUIToolStripMenuItem.Name = "openMeGUIToolStripMenuItem";
            this.openMeGUIToolStripMenuItem.Size = new System.Drawing.Size(142, 22);
            this.openMeGUIToolStripMenuItem.Text = "Open MeGUI";
            this.openMeGUIToolStripMenuItem.Click += new System.EventHandler(this.openMeGUIToolStripMenuItem_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(139, 6);
            // 
            // exitMeGUIToolStripMenuItem
            // 
            this.exitMeGUIToolStripMenuItem.Name = "exitMeGUIToolStripMenuItem";
            this.exitMeGUIToolStripMenuItem.Size = new System.Drawing.Size(142, 22);
            this.exitMeGUIToolStripMenuItem.Text = "Exit MeGUI";
            this.exitMeGUIToolStripMenuItem.Click += new System.EventHandler(this.exitMeGUIToolStripMenuItem_Click);
            // 
            // MainForm
            // 
            this.AllowDrop = true;
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(508, 499);
            this.Controls.Add(this.tabControl1);
            this.DataBindings.Add(new System.Windows.Forms.Binding("Size", global::MeGUI.Properties.Settings.Default, "MainFormSize", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.DataBindings.Add(new System.Windows.Forms.Binding("Location", global::MeGUI.Properties.Settings.Default, "MainFormLocation", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Location = global::MeGUI.Properties.Settings.Default.MainFormLocation;
            this.Menu = this.mainMenu1;
            this.MinimumSize = new System.Drawing.Size(524, 537);
            this.Name = "MainForm";
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.MainForm_FormClosed);
            this.Load += new System.EventHandler(this.MeGUI_Load);
            this.Shown += new System.EventHandler(this.MainForm_Shown);
            this.Move += new System.EventHandler(this.MainForm_Move);
            this.tabControl1.ResumeLayout(false);
            this.inputTab.ResumeLayout(false);
            this.splitContainer2.Panel1.ResumeLayout(false);
            this.splitContainer2.Panel2.ResumeLayout(false);
            this.splitContainer2.ResumeLayout(false);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.Panel2.PerformLayout();
            this.splitContainer1.ResumeLayout(false);
            this.flowLayoutPanel1.ResumeLayout(false);
            this.flowLayoutPanel1.PerformLayout();
            this.flowLayoutPanel2.ResumeLayout(false);
            this.flowLayoutPanel2.PerformLayout();
            this.tabPage2.ResumeLayout(false);
            this.logTab.ResumeLayout(false);
            this.trayMenu.ResumeLayout(false);
            this.ResumeLayout(false);

        }
        
        /// <summary>
        /// launches the megui wiki in the default browser
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void mnuGuide_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("http://mewiki.project357.com/wiki/Main_Page");
        }
        /// <summary>
        /// launches the encoder gui forum in the default browser
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void menuItem2_Click(object sender, EventArgs e)
        {
            
        }
        /// <summary>
        /// shows the changelog dialog window
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void mnuChangelog_Click(object sender, EventArgs e)
        {
            using (Changelog cl = new Changelog())
            {
                cl.ShowDialog();
            }
        }

        public MainForm()
        {
            System.Reflection.Assembly a = System.Reflection.Assembly.GetExecutingAssembly();
            Version appVersion = a.GetName().Version;
            string appVersionString = appVersion.ToString();

            if (Properties.Settings.Default.ApplicationVersion != appVersion.ToString())
            {
                Properties.Settings.Default.Upgrade();
                CustomUserSettings.Default.Upgrade();
                Properties.Settings.Default.ApplicationVersion = appVersionString;
            }

            Instance = this;
            constructMeGUIInfo();
            InitializeComponent();
            System.Reflection.Assembly myAssembly = this.GetType().Assembly;
            string name = this.GetType().Namespace + ".";

			name = "";

            string[] resources = myAssembly.GetManifestResourceNames();
            this.trayIcon.Icon = new Icon(myAssembly.GetManifestResourceStream(name + "App.ico"));
            this.Icon = trayIcon.Icon;
            this.TitleText = Application.ProductName + " " + Application.ProductVersion;

            this.TitleText += " x64";

            if (MainForm.Instance.Settings.AutoUpdate == true && MainForm.Instance.Settings.AutoUpdateServerSubList == 1)
                this.TitleText += " DEVELOPMENT UPDATE SERVER";
            setGUIInfo();
            Jobs.showAfterEncodingStatus(Settings);
            this.videoEncodingComponent1.FileType = MainForm.Instance.Settings.MainFileFormat;
        }

        
        public JobControl Jobs
        {
            get { return jobControl1; }
        }
        public bool ProcessStatusChecked
        {
            get { return progressMenu.Checked; }
            set { progressMenu.Checked = value; }
        }
        public VideoEncodingComponent Video
        {
            get { return videoEncodingComponent1; }
        }
        public AudioEncodingComponent Audio
        {
            get { return audioEncodingComponent1; }
        }
       
        /// <summary>
        /// initializes all the dropdown elements in the GUI to their default values
        /// </summary>

        /// <summary>
        /// handles the GUI closing event
        /// saves all jobs, stops the currently active job and saves all profiles as well
        /// </summary>
        /// <param name="e"></param>
        protected override void OnClosing(CancelEventArgs e)
        {
            if (jobControl1.IsAnyWorkerEncoding)
            {
                DialogResult dr = MessageBox.Show("Are you sure you want to quit?", "Job in progress", MessageBoxButtons.YesNo, MessageBoxIcon.Warning);
                if (dr == DialogResult.No)
                    e.Cancel = true; // abort closing
                else
                    jobControl1.AbortAll();
            }
            if (!e.Cancel)
            {
                CloseSilent();
            }
            base.OnClosing(e);
        }
        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }
        
        private void resetButton_Click(object sender, System.EventArgs e)
        {
            videoEncodingComponent1.Reset();
            audioEncodingComponent1.Reset();
        }
        
        
        private void autoEncodeButton_Click(object sender, System.EventArgs e)
        {
            RunTool("AutoEncode");
        }

        private void RunTool(string p)
        {
            try
            {
                ITool tool = PackageSystem.Tools[p];
                tool.Run(this);
            }
            catch (KeyNotFoundException)
            {
                MessageBox.Show("Required tool, '" + p + "', not found.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
        
        
        
        /// <summary>
        /// Test whether a filename is suitable for writing to
        /// </summary>
        /// <param name="filename"></param>
        /// <returns>Error message if problem, null if ok</returns>
        public static string verifyOutputFile(string filename)
        {
            try
            {
                filename = Path.GetFullPath(filename);  // this will throw ArgumentException if invalid
                if (File.Exists(filename))
                {
                    FileStream fs = File.OpenWrite(filename);  // this will throw if we'll have problems writing
                    fs.Close();
                }
                else
                {
                    FileStream fs = File.Create(filename);  // this will throw if we'll have problems writing
                    fs.Close();
                    File.Delete(filename);
                }
            }
            catch (Exception e)
            {
                return e.Message;
            }
            return null;
        }

        /// <summary>
        /// Test whether a filename is suitable for reading from
        /// </summary>
        /// <param name="filename"></param>
        /// <returns>Error message if problem, null if ok</returns>
        public static string verifyInputFile(string filename)
        {
            try
            {
                filename = Path.GetFullPath(filename);  // this will throw ArgumentException if invalid
                FileStream fs = File.OpenRead(filename);  // this will throw if we'll have problems reading
                fs.Close();
            }
            catch (Exception e)
            {
                return e.Message;
            }
            return null;
        }
        
        
        
        /// <summary>
        /// saves the global GUI settings to settings.xml
        /// </summary>
        public void saveSettings()
        {
            XmlSerializer ser = null;
            string fileName = this.path + @"\settings.xml";
            using (Stream s = File.Open(fileName, System.IO.FileMode.Create, System.IO.FileAccess.Write))
            {
                try
                {
                    ser = new XmlSerializer(typeof(MeGUISettings));
                    ser.Serialize(s, this.settings);
                }
                catch (Exception e)
                {
                    Console.Write(e.Message);
                }
            }
        }
        /// <summary>
        /// loads the global settings
        /// </summary>
        public void loadSettings()
        {
            string fileName = Path.Combine(path, "settings.xml");
            if (File.Exists(fileName))
            {
                XmlSerializer ser = null;
                using (Stream s = File.OpenRead(fileName))
                {
                    ser = new XmlSerializer(typeof(MeGUISettings));
                    try
                    {
                        this.settings = (MeGUISettings)ser.Deserialize(s);
                    }
                    catch (Exception e)
                    {
                        MessageBox.Show("Settings could not be loaded.", "Error loading profile", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        Console.Write(e.Message);
                    }
                }
            }
        }

        
        
        
        
        

        public JobUtil JobUtil
        {
            get { return jobUtil; }
        }

        public string TitleText
        {
            get
            {
                return this.Text;
            }
            set
            {
                this.Text = value;
                trayIcon.Text = value;
            }
        }

        /// <summary>
        /// shuts down the PC if the shutdown option is set
        /// also saves all profiles, jobs and the log as MeGUI is killed
        /// via the shutdown so the appropriate methods in the OnClosing are not called
        /// </summary>
        public void runAfterEncodingCommands()
        {
            if (Jobs.CurrentAfterEncoding == AfterEncoding.DoNothing) return;
            this.profileManager.SaveProfiles();
            this.saveSettings();
            jobControl1.saveJobs();
            this.saveLog();

            if (Jobs.CurrentAfterEncoding == AfterEncoding.Shutdown)
            {
                using (CountdownWindow countdown = new CountdownWindow(30))
                {
                    if (countdown.ShowDialog() == DialogResult.OK)
                    {
                        bool succ = Shutdown.shutdown();
                        if (!succ)
                            Log.LogEvent("Tried and failed to shut down system");
                        else
                            Log.LogEvent("Shutdown initiated");
                    }
                    else
                        Log.LogEvent("User aborted shutdown");

                }
            }
            else
            {
                string filename = MeGUIPath + @"\after_encoding.bat";
                try
                {
                    using (StreamWriter s = new StreamWriter(File.OpenWrite(filename)))
                    {
                        s.WriteLine(settings.AfterEncodingCommand);
                    }
                    ProcessStartInfo psi = new ProcessStartInfo(filename);
                    psi.CreateNoWindow = true;
                    psi.UseShellExecute = false;
                    Process p = new Process();
                    p.StartInfo = psi;
                    p.Start();
                }
                catch (IOException e) { MessageBox.Show("Error when attempting to run command: " + e.Message, "Run command failed", MessageBoxButtons.OK, MessageBoxIcon.Error); }
            }
        }

        public LogItem Log
        {
            get { return logTree1.Log; }
        }


        /// <summary>
        /// saves the whole content of the log into a logfile
        /// </summary>
        public void saveLog()
        {
            string text = Log.ToString();
            try
            {
                string logDirectory = path + @"\logs";
                FileUtil.ensureDirectoryExists(logDirectory);
                string fileName = logDirectory + @"\logfile-" + DateTime.Now.ToString("yy'-'MM'-'dd'_'HH'-'mm'-'ss") + ".log";
                File.WriteAllText(fileName, text);
            }
            catch (Exception e)
            {
                Console.Write(e.Message);
            }
        }
        private void exitMeGUIToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Close();
        }
        /// <summary>
        /// returns the profile manager to whomever might require it
        /// </summary>
        public ProfileManager Profiles
        {
            get
            {
                return this.profileManager;
            }
        }
        
        
        private void mnuFileOpen_Click(object sender, EventArgs e)
        {
            openFileDialog.Filter = "AviSynth Scripts (*.avs)|*.avs|" +
                "Audio Files (*.ac3, *.mp2, *.mpa, *.wav)|*.ac3;*.mp2;*.mpa;*.wav|" +
                "MPEG-2 Files (*.vob, *.mpg, *.mpeg, *.m2t*, *.m2v, *.mpv, *.tp, *.ts, *.trp, *.pva, *.vro)|" +
                "*.vob;*.mpg;*.mpeg;*.m2t*;*.m2v;*.mpv;*.tp;*.ts;*.trp;*.pva;*.vro|" +
                "Other Video Files (*.d2v, *.avi, *.mp4, *.mkv, *.rmvb)|*.d2v;*.avi;*.mp4;*.mkv;*.rmvb|" +
                "All supported encodable files|" +
                "*.avs;*.ac3;*.mp2;*.mpa;*.wav;*.vob;*.mpg;*.mpeg;*.m2t*;*.m2v;*.mpv;*.tp;*.ts;*.trp;*.pva;*.vro;*.d2v;*.avi;*.mp4;*.mkv;*.rmvb|" +
                "All files|*.*";
            openFileDialog.Title = "Select your input file";
            if (openFileDialog.ShowDialog() == DialogResult.OK)
                openFile(openFileDialog.FileName);
        }
        private void mnuViewMinimizeToTray_Click(object sender, EventArgs e)
        {
            formsToReopen.Clear();
            this.Visible = false;
            trayIcon.Visible = true;
        }

        private void mnuFileExit_Click(object sender, System.EventArgs e)
        {
            this.Close();
        }

        private void mnuToolsSettings_Click(object sender, System.EventArgs e)
        {
            using (SettingsForm sform = new SettingsForm())
            {
                sform.Settings = this.settings;
                if (sform.ShowDialog() == DialogResult.OK)
                {
                    this.settings = sform.Settings;
                    this.saveSettings();
                    Jobs.showAfterEncodingStatus(settings);
                }
            }
        }
        private void mnuTool_Click(object sender, System.EventArgs e)
        {
            if ((!(sender is System.Windows.Forms.MenuItem)) || (!((sender as MenuItem).Tag is ITool)))
                return;
            ((ITool)(sender as MenuItem).Tag).Run(this);
        }

        private void mnuOptions_Click(object sender, System.EventArgs e)
        {
            if ((!(sender is System.Windows.Forms.MenuItem)) || (!((sender as MenuItem).Tag is IOption)))
                return;
            ((IOption)(sender as MenuItem).Tag).Run(this);
        }

        private void mnuMuxer_Click(object sender, System.EventArgs e)
        {
            if ((!(sender is System.Windows.Forms.MenuItem)) || (!((sender as MenuItem).Tag is IMuxing)))
                return;
            using (MuxWindow mw = new MuxWindow((IMuxing)((sender as MenuItem).Tag),this))
            {
                if (mw.ShowDialog() == DialogResult.OK)
                {
                    MuxJob job = mw.Job;
                    Jobs.addJobsToQueue(job);
                }
            }
        }

        private void mnuView_Popup(object sender, System.EventArgs e)
        {
            List<Pair<string, bool> > workers = Jobs.ListProgressWindows();
            progressMenu.MenuItems.Clear();
            progressMenu.MenuItems.Add(showAllProgressWindows);
            progressMenu.MenuItems.Add(hideAllProgressWindows);
            progressMenu.MenuItems.Add(separator2);

            foreach (Pair<string, bool> p in workers)
            {
                MenuItem i = new MenuItem(p.fst);
                i.Checked = p.snd;
                i.Click += new EventHandler(mnuProgress_Click);
                progressMenu.MenuItems.Add(i);
            }

            if (workers.Count == 0)
            {
                MenuItem i = new MenuItem("(No progress windows to show)");
                i.Enabled = false;
                progressMenu.MenuItems.Add(i);
            }
        }

        void mnuProgress_Click(object sender, EventArgs e)
        {
            MenuItem i = (MenuItem)sender;
            if (i.Checked)
                Jobs.HideProgressWindow(i.Text);
            else
                Jobs.ShowProgressWindow(i.Text);
        }
        private void mnuViewProcessStatus_Click(object sender, System.EventArgs e)
        {
        }

        


        public MeGUISettings Settings
        {
            get { return settings; }
        }

        public MediaFileFactory MediaFileFactory
        {
            get { return mediaFileFactory; }
        }
        
        private void trayIcon_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            // Activate the form.
            this.Show(); this.Activate();

            if (progressMenu.Checked)
                Jobs.ShowAllProcessWindows();
            trayIcon.Visible = false;            
        }
        private void openMeGUIToolStripMenuItem_Click(object sender, EventArgs e)
        {
            trayIcon.Visible = false;
            this.Visible = true;
        }

        
        
        public void openOtherVideoFile(string fileName)
        {
            AviSynthWindow asw = new AviSynthWindow(this, fileName);
            asw.OpenScript += new OpenScriptCallback(Video.openVideoFile);
            asw.Show();
        }
        public void openDGIndexFile(string fileName)
        {
            if (DialogManager.useOneClick())
                openOneClickFile(fileName);
            else
                openD2VCreatorFile(fileName);
        }
        public void openOneClickFile(string fileName)
        {
            OneClickWindow ocmt = new OneClickWindow(this, jobUtil, videoEncodingComponent1.VideoEncoderProvider, new AudioEncoderProvider());
            ocmt.openInput(fileName);
            ocmt.ShowDialog();
        }
        public void openD2VCreatorFile(string fileName)
        {
            FileIndexerWindow mpegInput = new FileIndexerWindow(this);
            mpegInput.setConfig(fileName, null, 2, true, true, true, false);
            mpegInput.ShowDialog();
        }
        private FileType getFileType(string fileName)
        {
            switch (Path.GetExtension(fileName.ToLower()))
            {
                case ".avs":
                    return FileType.VIDEOINPUT;
                case ".aac":
                case ".ac3":
                case ".aif":
                case ".au":
                case ".caf":
                case ".bwf":
                case ".dtsma":
                case ".dtshd":               
                case ".dts":
                case ".mp2":
                case ".mp3": 
                case ".mpa":
                case ".wav":
                case ".w64":
                case ".eac3":
                case ".ddp":
                   return FileType.AUDIOINPUT;
                case ".vob":
                case ".mpg":
                case ".mpeg":
                case ".m2v":
                case ".mpv":
                case ".tp":
                case ".ts":
                case ".trp":
                case ".pva":
                case ".vro":
                    return FileType.DGINDEX;
                case ".mkv":
                    return FileType.FFMSINDEX;
                case ".zip":
                    return FileType.ZIPPED_PROFILES;

                default:
                    return FileType.OTHERVIDEO;
            }
        }
        public void openFile(string file)
        {
            switch (getFileType(file))
            {
                case FileType.VIDEOINPUT:
                    Video.openVideoFile(file);
                    break;
                case FileType.AUDIOINPUT:
                    audioEncodingComponent1.openAudioFile(file);
                    break;
                case FileType.DGINDEX:
                    openDGIndexFile(file);
                    break;
                case FileType.FFMSINDEX:
                    openDGIndexFile(file);
                    break;
                case FileType.OTHERVIDEO:
                    openOtherVideoFile(file);
                    audioEncodingComponent1.openAudioFile(file); // for Non-MPEG OneClick fudge
                    break;

                case FileType.ZIPPED_PROFILES:
                    importProfiles(file);
                    break;
            }
        }

        private void importProfiles(string file)
        {
            new ProfileImporter(this, file).ShowDialog();
        }
        
        
        private void MeGUI_DragDrop(object sender, DragEventArgs e)
        {
            string[] files = (string[])e.Data.GetData(DataFormats.FileDrop, false);
            Invoke(new MethodInvoker(delegate
            {
                openFile(files[0]);
            }));
            this.tabControl1.SelectedIndex = 0;
        }

        private void MeGUI_DragEnter(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.None;
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                string[] files = (string[])e.Data.GetData(DataFormats.FileDrop, false);
                if (files.Length > 0)
                    e.Effect = DragDropEffects.All;
            }
        }
        
        
        public void importProfiles(Stream data)
        {
            Util.ThreadSafeRun(this, delegate
            {
                ProfileImporter importer = new ProfileImporter(this, data);
                importer.Show();
                while (importer.Visible == true)    // wait until the profiles have been imported
                {
                    Application.DoEvents();
                    System.Threading.Thread.Sleep(1000);
                }
            });
        }

        private void mnuFileImport_Click(object sender, EventArgs e)
        {
            try
            {
                new ProfileImporter(this).ShowDialog();
            }
            catch (CancelledException) { }
        }

        private void mnuFileExport_Click(object sender, EventArgs e)
        {
            new ProfileExporter(this).ShowDialog();
        }
        

        private void mnuToolsAdaptiveMuxer_Click(object sender, EventArgs e)
        {
            AdaptiveMuxWindow amw = new AdaptiveMuxWindow(this);
            if (amw.ShowDialog() == DialogResult.OK)
                Jobs.addJobsWithDependencies(amw.Jobs);
        }

        private void MeGUI_Load(object sender, EventArgs e)
        {
            RegisterForm(this);
            //DriveInfo[] allDrives = DriveInfo.GetDrives();      

            //i = Log.Info("Hardware");
            //i.LogValue("CPU ", string.Format("{0}", OSInfo.GetMOStuff("Win32_Processor")));
            /*
                        foreach (DriveInfo d in allDrives)   //OSInfo.GetMOStuff("Win32_OperatingSystem")
                        {
                            if ((d.DriveType == DriveType.Fixed) && (d.DriveFormat != null)) 
                            {
                                try
                                {
                                    long freespace = long.Parse(d.AvailableFreeSpace.ToString()) / 1073741824;
                                    long totalsize = long.Parse(d.TotalSize.ToString()) / 1073741824;

                                    if (d.VolumeLabel.ToString() == "")
                                        d.VolumeLabel = "Local Disk";

                                    i.LogValue("HDD ", string.Format("{0} ({1})  -  {2} Go free of {3} Go", d.VolumeLabel, d.Name, Convert.ToString(freespace), Convert.ToString(totalsize)));
                                }
                                catch (Exception)
                                {
                                    MessageBox.Show("MeGUI cannot access to the disk " + d.Name);
                                }
                            }
                        }
            */
            //Log.LogValue("Settings", Settings, ImageType.Information);
        }

        private void beginUpdateCheck()
        {

            string strLocalUpdateXML = Path.Combine(Path.GetDirectoryName(System.Windows.Forms.Application.ExecutablePath), "upgrade.xml");


            string strLocalUpdateXML = Path.Combine(Path.GetDirectoryName(System.Windows.Forms.Application.ExecutablePath), "upgrade_x64.xml");

            UpdateWindow update = new UpdateWindow(this, this.Settings);
            update.GetUpdateData(true);
            if (update.HasUpdatableFiles()) // If there are updated files, display the window
            {
                if (File.Exists(strLocalUpdateXML))
                {
                    update.Visible = true;
                    update.StartAutoUpdate();
                    while (update.Visible == true)
                    {
                        Application.DoEvents();
                        System.Threading.Thread.Sleep(1000);
                    }
                }
                else
                {
                    if (MessageBox.Show("There are updated files available. Do you wish to update to the latest versions?",
                        "Updates Available", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
                        update.ShowDialog();
                }
            }
            else
            {
                if (File.Exists(strLocalUpdateXML))
                    File.Delete(strLocalUpdateXML);
            }
        }

        private void mnuUpdate_Click(object sender, EventArgs e)
        {
            UpdateWindow update = new UpdateWindow(this, this.Settings);
            update.ShowDialog();
        }

        internal void AddFileToReplace(string iUpgradeableName, string tempFilename, string filename, string newVersion)
        {
            CommandlineUpgradeData data = new CommandlineUpgradeData();
            data.filename.Add(filename);
            data.tempFilename.Add(tempFilename);
            data.newVersion = newVersion;
            if (filesToReplace.ContainsKey(iUpgradeableName))
            {
                filesToReplace[iUpgradeableName].tempFilename.Add(tempFilename);
                filesToReplace[iUpgradeableName].filename.Add(filename);
                return;
            }
            filesToReplace.Add(iUpgradeableName, data);
        }

        internal void CloseSilent()
        {
            this.profileManager.SaveProfiles();
            this.saveSettings();
            this.saveApplicationSettings();
            jobControl1.saveJobs();
            this.saveLog();
            deleteFiles();
            this.runRestarter();
            Application.ApplicationExit += new EventHandler(Application_ApplicationExit);
        }

        void Application_ApplicationExit(object sender, EventArgs e)
        {
            MeGUI.Properties.Settings.Default.Save();
            MeGUI.core.util.CustomUserSettings.Default.Save();
        }

        private void saveApplicationSettings()
        {
        }

        private void deleteFiles()
        {
            foreach (string file in filesToDeleteOnClosing)
            {
                try
                {
                    FileUtil.DeleteDirectoryIfExists(file, true);
                    if (File.Exists(file)) File.Delete(file);
                }
                catch { }
            }
        }

        public void OpenVideoFile(string p)
        {
            if (p.ToLower().EndsWith(".avs"))
                Video.openVideoFile(p);
            else
            {

                string newFileName = VideoUtil.createSimpleAvisynthScript(p);
                if (newFileName != null)
                    Video.openVideoFile(newFileName);
            }
        }
        
        
        private bool restart = false;
        private Dictionary<string, CommandlineUpgradeData> filesToReplace = new Dictionary<string, CommandlineUpgradeData>();
        private DialogManager dialogManager;
        private string path; // path the program was started from
        private MediaFileFactory mediaFileFactory;
        private PackageSystem packageSystem = new PackageSystem();
        private JobUtil jobUtil;
        private MuxProvider muxProvider;
        private MeGUISettings settings = new MeGUISettings();
        private ProfileManager profileManager;

        //public MeGUISettings Settings
        //{
        //    get { return settings; }
        //}

        public MuxProvider MuxProvider
        {
            get { return muxProvider; }
        }

        private CodecManager codecs;
        
        
        public void handleCommandline(CommandlineParser parser)
        {
            foreach (string file in parser.failedUpgrades)
                System.Windows.Forms.MessageBox.Show("Failed to upgrade '" + file + "'.", "Upgrade failed", MessageBoxButtons.OK, MessageBoxIcon.Error);

            if (parser.upgradeData.Count > 0)
            {
                UpdateWindow update = new UpdateWindow(this, Settings);
                foreach (string file in parser.upgradeData.Keys)
                    update.UpdateVersionNumber(file, parser.upgradeData[file]);
                update.SaveSettings();
            }
        }

        public void setGUIInfo()
        {
            fillMenus();
            jobControl1.MainForm = this;
            jobControl1.loadJobs();

        }

        /// <summary>
        /// default constructor
        /// initializes all the GUI components, initializes the internal objects and makes a default selection for all the GUI dropdowns
        /// In addition, all the jobs and profiles are being loaded from the harddisk
        /// </summary>
        public void constructMeGUIInfo()
        {
            muxProvider = new MuxProvider(this);
            this.codecs = new CodecManager();
            this.path = System.Windows.Forms.Application.StartupPath;
            this.jobUtil = new JobUtil(this);
            this.settings = new MeGUISettings();
            addPackages();
            this.profileManager = new ProfileManager(this.path);
            this.profileManager.LoadProfiles();
            this.mediaFileFactory = new MediaFileFactory(this);
            this.loadSettings();
            this.dialogManager = new DialogManager(this);
        }

        private void fillMenus()
        {
            // Fill the muxing menu
            mnuMuxers.MenuItems.Clear();
            mnuMuxers.MenuItems.Add(mnuToolsAdaptiveMuxer);
            int index = 1;
            foreach (IMuxing muxer in PackageSystem.MuxerProviders.Values)
            {
                MenuItem newMenuItem = new MenuItem();
                newMenuItem.Text = muxer.Name;
                newMenuItem.Tag = muxer;
                newMenuItem.Index = index;
                index++;
                mnuMuxers.MenuItems.Add(newMenuItem);
                newMenuItem.Click += new System.EventHandler(this.mnuMuxer_Click);
            }
            
            // Fill the tools menu
            mnuTools.MenuItems.Clear();
            List<MenuItem> toolsItems = new List<MenuItem>();
            List<Shortcut> usedShortcuts = new List<Shortcut>();
            toolsItems.Add(mnutoolsD2VCreator);
            toolsItems.Add(mnuMuxers);
            usedShortcuts.Add(mnuMuxers.Shortcut);
            
            foreach (ITool tool in PackageSystem.Tools.Values)
            {
                if (tool.Name != "File Indexer")
                {
                    MenuItem newMenuItem = new MenuItem();
                    newMenuItem.Text = tool.Name;
                    newMenuItem.Tag = tool;
                    newMenuItem.Click += new System.EventHandler(this.mnuTool_Click);
                    bool shortcutAttempted = false;
                    foreach (Shortcut s in tool.Shortcuts)
                    {
                        shortcutAttempted = true;
                        Debug.Assert(s != Shortcut.None);
                        if (!usedShortcuts.Contains(s))
                        {
                            usedShortcuts.Add(s);
                            newMenuItem.Shortcut = s;
                            break;
                        }
                    }

                    if (shortcutAttempted && newMenuItem.Shortcut == Shortcut.None)
                        Log.Warn("Shortcut for '" + tool.Name + "' is already used. No shortcut selected.");
                    toolsItems.Add(newMenuItem);
                }
            }

            toolsItems.Sort(new Comparison<MenuItem>(delegate(MenuItem a, MenuItem b) { return (a.Text.CompareTo(b.Text)); }));
            index = 0;
            foreach (MenuItem m in toolsItems)
            {
                m.Index = index;
                index++;
                mnuTools.MenuItems.Add(m);
            }

            // Fill the Options Menu
            mnuOptions.MenuItems.Clear();
            List<MenuItem> optionsItems = new List<MenuItem>();
            List<Shortcut> usedShortcuts2 = new List<Shortcut>();
            optionsItems.Add(mnuOptionsSettings);
            usedShortcuts2.Add(mnuOptionsSettings.Shortcut);
            foreach (IOption option in PackageSystem.Options.Values)
            {
                MenuItem newMenuItem = new MenuItem();
                newMenuItem.Text = option.Name;
                newMenuItem.Tag = option;
                newMenuItem.Click += new System.EventHandler(this.mnuOptions_Click);
                bool shortcutAttempted = false;
                foreach (Shortcut s in option.Shortcuts)
                {
                    shortcutAttempted = true;
                    Debug.Assert(s != Shortcut.None);
                    if (!usedShortcuts.Contains(s))
                    {
                        usedShortcuts2.Add(s);
                        newMenuItem.Shortcut = s;
                        break;
                    }
                }
                if (shortcutAttempted && newMenuItem.Shortcut == Shortcut.None)
                    Log.Warn("Shortcut for '" + option.Name + "' is already used. No shortcut selected.");
                optionsItems.Add(newMenuItem);
            }

            optionsItems.Sort(new Comparison<MenuItem>(delegate(MenuItem a, MenuItem b) { return (a.Text.CompareTo(b.Text)); }));
            index = 0;
            foreach (MenuItem m in optionsItems)
            {
                m.Index = index;
                index++;
                mnuOptions.MenuItems.Add(m);
            }
        }

        private void addPackages()
        {
            PackageSystem.JobProcessors.Register(AviSynthAudioEncoder.Factory);

            PackageSystem.JobProcessors.Register(mencoderEncoder.Factory);
            PackageSystem.JobProcessors.Register(x264Encoder.Factory);
            PackageSystem.JobProcessors.Register(XviDEncoder.Factory);

            PackageSystem.JobProcessors.Register(MkvMergeMuxer.Factory);
            PackageSystem.JobProcessors.Register(MP4BoxMuxer.Factory);
            PackageSystem.JobProcessors.Register(AMGMuxer.Factory);
            PackageSystem.JobProcessors.Register(tsMuxeR.Factory);
            PackageSystem.JobProcessors.Register(CleanupJobRunner.Factory);

            PackageSystem.JobProcessors.Register(AviSynthProcessor.Factory);
            PackageSystem.JobProcessors.Register(D2VIndexer.Factory);
            PackageSystem.JobProcessors.Register(DGAIndexer.Factory);
            PackageSystem.JobProcessors.Register(DGIIndexer.Factory);
            PackageSystem.JobProcessors.Register(FFMSIndexer.Factory);
            PackageSystem.JobProcessors.Register(VobSubIndexer.Factory);
            PackageSystem.JobProcessors.Register(Joiner.Factory);
            PackageSystem.JobProcessors.Register(MeGUI.packages.tools.besplitter.Splitter.Factory);
            PackageSystem.JobProcessors.Register(HDStreamExtractorIndexer.Factory);
            PackageSystem.MuxerProviders.Register(new AVIMuxGUIMuxerProvider());
            PackageSystem.MuxerProviders.Register(new TSMuxerProvider());            
            PackageSystem.MuxerProviders.Register(new MKVMergeMuxerProvider());
            PackageSystem.MuxerProviders.Register(new MP4BoxMuxerProvider());
            PackageSystem.Tools.Register(new CutterTool());
            PackageSystem.Tools.Register(new AviSynthWindowTool());
            PackageSystem.Tools.Register(new AutoEncodeTool());
            PackageSystem.Tools.Register(new CQMEditorTool());
            PackageSystem.Tools.Register(new CalculatorTool());
            PackageSystem.Tools.Register(new ChapterCreatorTool());
            PackageSystem.Options.Register(new UpdateOptions());
            PackageSystem.Tools.Register(new BesplitterTool());
            PackageSystem.Tools.Register(new OneClickTool());
            PackageSystem.Tools.Register(new D2VCreatorTool());
            PackageSystem.Tools.Register(new AVCLevelTool());
            PackageSystem.Tools.Register(new VobSubTool());
            PackageSystem.Tools.Register(new HdBdExtractorTool());
            PackageSystem.MediaFileTypes.Register(new AvsFileFactory());
            PackageSystem.MediaFileTypes.Register(new d2vFileFactory());
            PackageSystem.MediaFileTypes.Register(new dgaFileFactory());
            PackageSystem.MediaFileTypes.Register(new dgiFileFactory());
            PackageSystem.MediaFileTypes.Register(new ffmsFileFactory());
            PackageSystem.MediaFileTypes.Register(new MediaInfoFileFactory());
            PackageSystem.JobPreProcessors.Register(BitrateCalculatorPreProcessor.CalculationProcessor);
            PackageSystem.JobPostProcessors.Register(OneClickPostProcessor.PostProcessor);
            PackageSystem.JobPostProcessors.Register(d2vIndexJobPostProcessor.PostProcessor);
            PackageSystem.JobPostProcessors.Register(dgaIndexJobPostProcessor.PostProcessor);
            PackageSystem.JobPostProcessors.Register(dgiIndexJobPostProcessor.PostProcessor);
            PackageSystem.JobPostProcessors.Register(ffmsIndexJobPostProcessor.PostProcessor);
            PackageSystem.JobPostProcessors.Register(CleanupJobRunner.DeleteIntermediateFilesPostProcessor);
            PackageSystem.JobConfigurers.Register(MuxWindow.Configurer);
            PackageSystem.JobConfigurers.Register(AudioEncodingWindow.Configurer);
        }

        private static Mutex mySingleInstanceMutex = new Mutex(true, "MeGUI_D9D0C224154B489784998BF97B9C9414");

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main(string[] args)
        {
            // Parse for the need to run the program elevated
            Boolean bRunElevated = false, bForceAdmin = false;
            foreach (string strParam in args)
            {
                if (strParam.Equals("-elevate"))
                    bRunElevated = true;
                else if (strParam.Equals("-forceadmin"))
                    bForceAdmin = true;
            }

            // Check if the program can write to the program and avisynth plugin dir
            if (FileUtil.IsDirWriteable(Path.GetDirectoryName(Application.ExecutablePath)) == false)
                bForceAdmin = true;

            // If needed run as elevated process
            if (bForceAdmin && !bRunElevated)
            {
                try
                {
                    Process p = new Process();
                    p.StartInfo.FileName = Application.ExecutablePath;
                    p.StartInfo.Arguments = "-elevate";
                    p.StartInfo.Verb = "runas";
                    p.Start();
                    return;
                }
                catch
                {
                }
            }

            System.Windows.Forms.Application.EnableVisualStyles();

            if (!mySingleInstanceMutex.WaitOne(0, false))
            {
                if (DialogResult.Yes != MessageBox.Show("Running MeGUI instance detected!\n\rThere's not really much point in running multiple copies of MeGUI, and it can cause problems.\n\rDo You still want to run yet another MeGUI instance?", "Running MeGUI instance detected", MessageBoxButtons.YesNo, MessageBoxIcon.Warning))
                    return;
            }

            Application.ThreadException += new System.Threading.ThreadExceptionEventHandler(Application_ThreadException);
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(CurrentDomain_UnhandledException);
            CommandlineParser parser = new CommandlineParser();
            parser.Parse(args);
            MainForm mainForm = new MainForm();
            mainForm.handleCommandline(parser);
            if (parser.start)
                Application.Run(mainForm);
        }

        static void CurrentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            HandleUnhandledException((Exception)e.ExceptionObject);
        }

        static void HandleUnhandledException(Exception e)
        {
            LogItem i = MainForm.Instance.Log.Error("Unhandled error");
            i.LogValue("Exception message", e.Message);
            i.LogValue("Stacktrace", e.StackTrace);
            i.LogValue("Inner exception", e.InnerException);
            foreach (DictionaryEntry info in e.Data)
                i.LogValue(info.Key.ToString(), info.Value);

            MessageBox.Show("MeGUI encountered a fatal error and may not be able to proceed. Reason: " + e.Message
                , "Fatal error", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }

        static void Application_ThreadException(object sender, System.Threading.ThreadExceptionEventArgs e)
        {
            HandleUnhandledException(e.Exception);
        }

        private void runRestarter()
        {
            if (filesToReplace.Keys.Count == 0 /*&& otherFilesToInstall.Count == 0*/)
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
                pstart.Arguments += "--restart";
            else
                pstart.Arguments += "--no-restart";

            // Check if the program can write to the program dir
            if (FileUtil.IsDirWriteable(Path.GetDirectoryName(Application.ExecutablePath)) == true)
            {
                pstart.CreateNoWindow = true;
                pstart.UseShellExecute = false;
            }
            else
            {
                // Need admin permissions
                proc.StartInfo.Verb = "runas";
                pstart.UseShellExecute = true;
            }

            proc.StartInfo = pstart;
            try
            {
                if (!proc.Start())
                    MessageBox.Show("Couldn't run updater.", "Couldn't run updater.", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            catch
            {
                MessageBox.Show("Couldn't run updater.", "Couldn't run updater.", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
        
        
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
        /// gets the path from where MeGUI was launched
        /// </summary>
        public string MeGUIPath
        {
            get { return this.path; }
        }
        


        internal void ClosePlayer()
        {
            videoEncodingComponent1.ClosePlayer();
        }

        internal void hidePlayer()
        {
            videoEncodingComponent1.hidePlayer();
        }

        internal void showPlayer()
        {
            videoEncodingComponent1.showPlayer();
        }


        private void showAllWorkers_Click(object sender, EventArgs e)
        {
            Jobs.ShowAllWorkers();
        }

        private void hideAllWorkers_Click(object sender, EventArgs e)
        {
            Jobs.HideAllWorkers();
        }

        private void showAllWorkers_Popup(object sender, EventArgs e)
        {
            viewSummary.Checked = Jobs.SummaryVisible;
            
            List<Pair<string, bool> > workers = Jobs.ListWorkers();
            workersMenu.MenuItems.Clear();
            workersMenu.MenuItems.Add(showAllWorkers);
            workersMenu.MenuItems.Add(hideAllWorkers);
            workersMenu.MenuItems.Add(separator);

            foreach (Pair<string, bool> p in workers)
            {
                MenuItem i = new MenuItem(p.fst);
                i.Checked = p.snd;
                i.Click += new EventHandler(mnuWorker_Click);
                workersMenu.MenuItems.Add(i);
            }

            if (workers.Count == 0)
            {
                MenuItem i = new MenuItem("(No workers to show)");
                i.Enabled = false;
                workersMenu.MenuItems.Add(i);
            }
        }



        void mnuWorker_Click(object sender1, EventArgs e)
        {
            MenuItem sender = (MenuItem) sender1;
            Jobs.SetWorkerVisible(sender.Text, !sender.Checked);
        }

        private void viewSummary_Click(object sender, EventArgs e)
        {
            if (viewSummary.Checked)
            {
                viewSummary.Checked = false;
                Jobs.HideSummary();
            }
            else
            {
                viewSummary.Checked = true;
                Jobs.ShowSummary();
            }
        }

        private void createNewWorker_Click(object sender, EventArgs e)
        {
            Jobs.RequestNewWorker();
        }

        private void showAllProgressWindows_Click(object sender, EventArgs e)
        {
            Jobs.ShowAllProcessWindows();
        }

        private void hideAllProgressWindows_Click(object sender, EventArgs e)
        {
            Jobs.HideAllProcessWindows();
        }

        private void MainForm_FormClosed(object sender, FormClosedEventArgs e)
        {
            DialogManager.stopCUVIDServer(); // close CUVIDServer from DGIndexNV tools if it is running
        }
        private void mnuForum_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("http://forum.doom9.org/forumdisplay.php?f=78");
        }

        private void mnuHome_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("http://sourceforge.net/projects/megui");
        }

        private void mnuBugTracker_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("http://sourceforge.net/tracker/?group_id=156112&atid=798476");
        }

        private void mnuFeaturesReq_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("http://sourceforge.net/tracker/?group_id=156112&atid=798479");
        }

        private void mnuDoc_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("http://mewiki.project357.com/wiki/Main_Page");
        }

        private void mnuOptionsSettings_Click(object sender, EventArgs e)
        {
            using (SettingsForm sform = new SettingsForm())
            {
                sform.Settings = this.settings;
                if (sform.ShowDialog() == DialogResult.OK)
                {
                    this.settings = sform.Settings;
                    this.saveSettings();
                    Jobs.showAfterEncodingStatus(settings);
                }
            }
        }

        private void MainForm_Shown(object sender, EventArgs e)
        {            
            LogItem i = Log.Info("Versions");

            i.LogValue("MeGUI Version ", Application.ProductVersion);


            i.LogValue("MeGUI Version ", Application.ProductVersion + " x64");

            i.LogValue("OS ", string.Format("{0}{1} ({2}.{3}.{4}.{5})", OSInfo.GetOSName(), OSInfo.GetOSServicePack(), OSInfo.OSMajorVersion, OSInfo.OSMinorVersion, OSInfo.OSRevisionVersion, OSInfo.OSBuildVersion));
            i.LogValue("Latest .Net Framework installed ", string.Format("{0}", OSInfo.DotNetVersionFormated(OSInfo.FormatDotNetVersion())));

            string avisynthversion = string.Empty;
            bool PropExists = false;
            VideoUtil.getAvisynthVersion(out avisynthversion, out PropExists);

            if (!PropExists)
            {
                if (AskToDownloadAvisynth() == true)
                    System.Diagnostics.Process.Start("http://www.avisynth.org");
            }
            
            if (PropExists)
            { // launch the updater only if avisynth is found to avoid installation issues
                if (settings.AutoUpdate)
                {
                    // Need a seperate thread to run the updater to stop internet lookups from freezing the app.
                    Thread updateCheck = new Thread(new ThreadStart(beginUpdateCheck));
                    updateCheck.IsBackground = true;
                    updateCheck.Start();
                }

                if (!string.IsNullOrEmpty(avisynthversion))
                    i.LogValue("Avisynth Version ", avisynthversion.Replace(", ", ".").ToString());
            }
        }

        private bool AskToDownloadAvisynth()
        {
            if (MessageBox.Show("MeGUI cannot find Avisynth on your system. Without this, it won't run properly. May I ask you to install it first ?\n", "Warning",
                                     MessageBoxButtons.YesNo, MessageBoxIcon.Warning) == DialogResult.Yes)
                return true;
            else
                return false;
        }

        private void OneClickEncButton_Click(object sender, EventArgs e)
        {
            RunTool("one_click");
        }

        private void HelpButton_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start("http://mewiki.project357.com/wiki/Main_Page");
        }

        private void menuItem5_Click(object sender, EventArgs e)
        {
            FileIndexerWindow d2vc = new FileIndexerWindow(this);
            d2vc.ShowDialog();
        }

        private void MainForm_Move(object sender, EventArgs e)
        {
            if (this.WindowState != FormWindowState.Minimized && this.Visible == true)
            {
                global::MeGUI.Properties.Settings.Default.MainFormSize = this.Size;
                global::MeGUI.Properties.Settings.Default.MainFormLocation = this.Location;
                global::MeGUI.Properties.Settings.Default.Save();
            }
        }
    }
    public class CommandlineUpgradeData
    {
        public List<string> filename = new List<string>();
        public List<string> tempFilename = new List<string>();
        public string newVersion;
    }
}
        
