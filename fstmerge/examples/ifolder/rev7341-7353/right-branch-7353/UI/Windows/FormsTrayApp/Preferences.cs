

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;
using System.Diagnostics;
using System.Xml;
using System.Net;
using Microsoft.Win32;

using Novell.iFolder.Web;
using Novell.iFolderCom;
using Novell.Win32Util;
using Novell.Wizard;
using Simias.Client;
using Simias.Client.Authentication;
using Simias.Client.Event;

namespace Novell.FormsTrayApp
{



    public class Preferences : System.Windows.Forms.Form
    {

        System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(Preferences));
        private const decimal defaultMinimumSeconds = 300;
        private const decimal maximumSeconds = int.MaxValue;
        private const decimal maximumMinutes = (decimal)(maximumSeconds / 60);
        private const decimal maximumHours = (decimal)(maximumMinutes / 60);
        private const decimal maximumDays = (decimal)(maximumHours / 24);
        private const decimal minmumSecondsAllowed = 5;
        private const string startiFolderinTray = "StartiFolderinTray";
        private const string iFolderRun = "DisableAutoStart";
        private const string notifyShareDisabled = "NotifyShareDisable";
        private const string notifyCollisionDisabled = "NotifyCollisionDisabled";
        private const string notifyJoinDisabled = "NotifyJoinDisabled";

        private const string hideSyncWindowPopup = "hideSyncWindowPopup";
        private const string hidePolicynotification = "hidePolicynotification";

        public static readonly string iFolderKey = @"SOFTWARE\Novell\iFolder";
        private const string preferencesX = "PreferencesX";
        private const string preferencesY = "PreferencesY";
        private const string notifyPolicyQuotaVoilation = "PolicyQuotaVoilation";
        private const string notifyFilePermissionVoilation = "FilePermissionVoilation";
        private const string notifyDiskFullFailure = "DiskFullFailure";
        private const string notifyPolicyTypeVoilation = "PolicyTypeVoilation";
        private const string notifyPolicySizeVoilation = "PolicySizeVoilation";
        private const string notifyIOPermissionFailure = "IOPermissionFailure";
        private const string notifyPathLongFailure = "PathLongFailure";
        private decimal minimumSyncInterval;
        private decimal minimumSeconds;
        private iFolderWebService ifWebService;
        private SimiasWebService simiasWebService;
        private bool shutdown = false;
        private Domain currentDefaultDomain = null;
        private Domain selectedDomain = null;
        private ListViewItem newAccountLvi = null;
        private bool processing = false;
        private bool successful;
        private bool updatePassword = false;
        private bool updateEnabled = false;
        private bool updateHost = false;
        private bool initialPositionSet = false;
        private System.Windows.Forms.NumericUpDown defaultInterval;
        private System.Windows.Forms.CheckBox displayConfirmation;
        private System.Windows.Forms.CheckBox displayTrayIcon;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.CheckBox autoSync;
        private System.Windows.Forms.CheckBox autoStart;
        private System.Windows.Forms.Button apply;
        private System.Windows.Forms.Button cancel;
        private System.Windows.Forms.CheckBox notifyShared;
        private System.Windows.Forms.CheckBox notifyCollisions;
        private System.Windows.Forms.CheckBox notifyJoins;
        private System.Windows.Forms.Button addAccount;
        private System.Windows.Forms.Button removeAccount;
        private System.Windows.Forms.Button details;
        private System.Windows.Forms.ListView accounts;
        private System.Windows.Forms.ColumnHeader columnHeader2;
        private System.Windows.Forms.ColumnHeader columnHeader3;
        private System.Windows.Forms.Button ok;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.TabPage tabGeneral;
        private System.Windows.Forms.TabPage tabAccounts;
        private System.Windows.Forms.TabPage tabMigrate;
        private System.Windows.Forms.Timer timer1;
        private System.Windows.Forms.ColumnHeader columnHeader1;
        private System.Windows.Forms.HelpProvider helpProvider1;
        private System.Windows.Forms.ComboBox timeUnit;
        private System.Windows.Forms.Label label1;
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.ListView listView1;
        private System.Windows.Forms.ColumnHeader columnHeader4;
        private System.Windows.Forms.ColumnHeader columnHeader5;
        private System.Windows.Forms.Button btnMigrate;
        private Manager simiasManager;
        private System.Windows.Forms.Button btnHelp;
        public Novell.FormsTrayApp.GlobalProperties parent;
        public string str;
        private System.Windows.Forms.CheckBox startInTrayIcon;
        private System.Windows.Forms.CheckBox hideSyncLog;
        private TabPage Settings;
        private ListBox policylistbox;
        private CheckBox notifyCheckbox;
        private System.Windows.Forms.CheckBox hidenotification;


        enum policyVoilation
        {
            QuotaVoliation,
            FileSizeVoilation,
            FileTypeVoilation,
            DiskFullVoilation,
            PremissionUnavailable,
            LongPath
        };

        enum preferenceTab
        {
            General = 0,
            Accounts = 1,
            Settings = 2
        };






        public Preferences(iFolderWebService ifolderWebService, SimiasWebService simiasWebService, Manager simiasManager)
        {



            InitializeComponent();

            defaultInterval.TextChanged += new EventHandler(defaultInterval_ValueChanged);

            ifWebService = ifolderWebService;
            this.simiasWebService = simiasWebService;
            this.simiasManager = simiasManager;







            {


            }

            int delta = calculateSize(label1, 0);

            if (delta > 0)
            {
                groupBox1.Height += 8 * (int)Math.Ceiling((float)delta / (float)label1.Width);
            }

            this.StartPosition = FormStartPosition.CenterScreen;
        }




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






        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Preferences));
            this.defaultInterval = new System.Windows.Forms.NumericUpDown();
            this.displayConfirmation = new System.Windows.Forms.CheckBox();
            this.displayTrayIcon = new System.Windows.Forms.CheckBox();
            this.startInTrayIcon = new System.Windows.Forms.CheckBox();
            this.hideSyncLog = new System.Windows.Forms.CheckBox();
            this.hidenotification = new System.Windows.Forms.CheckBox();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabGeneral = new System.Windows.Forms.TabPage();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.notifyCollisions = new System.Windows.Forms.CheckBox();
            this.notifyShared = new System.Windows.Forms.CheckBox();
            this.notifyJoins = new System.Windows.Forms.CheckBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.label1 = new System.Windows.Forms.Label();
            this.timeUnit = new System.Windows.Forms.ComboBox();
            this.autoSync = new System.Windows.Forms.CheckBox();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.autoStart = new System.Windows.Forms.CheckBox();
            this.tabAccounts = new System.Windows.Forms.TabPage();
            this.accounts = new System.Windows.Forms.ListView();
            this.columnHeader3 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
            this.details = new System.Windows.Forms.Button();
            this.removeAccount = new System.Windows.Forms.Button();
            this.addAccount = new System.Windows.Forms.Button();
            this.Settings = new System.Windows.Forms.TabPage();
            this.notifyCheckbox = new System.Windows.Forms.CheckBox();
            this.policylistbox = new System.Windows.Forms.ListBox();
            this.tabMigrate = new System.Windows.Forms.TabPage();
            this.btnMigrate = new System.Windows.Forms.Button();
            this.listView1 = new System.Windows.Forms.ListView();
            this.columnHeader4 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader5 = new System.Windows.Forms.ColumnHeader();
            this.cancel = new System.Windows.Forms.Button();
            this.apply = new System.Windows.Forms.Button();
            this.ok = new System.Windows.Forms.Button();
            this.timer1 = new System.Windows.Forms.Timer(this.components);
            this.helpProvider1 = new System.Windows.Forms.HelpProvider();
            this.btnHelp = new System.Windows.Forms.Button();
            ((System.ComponentModel.ISupportInitialize)(this.defaultInterval)).BeginInit();
            this.tabControl1.SuspendLayout();
            this.tabGeneral.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.tabAccounts.SuspendLayout();
            this.Settings.SuspendLayout();
            this.tabMigrate.SuspendLayout();
            this.SuspendLayout();



            resources.ApplyResources(this.defaultInterval, "defaultInterval");
            this.helpProvider1.SetHelpString(this.defaultInterval, resources.GetString("defaultInterval.HelpString"));
            this.defaultInterval.Increment = new decimal(new int[] {
            5,
            0,
            0,
            0});
            this.defaultInterval.Maximum = new decimal(new int[] {
            2147483647,
            0,
            0,
            0});
            this.defaultInterval.Name = "defaultInterval";
            this.helpProvider1.SetShowHelp(this.defaultInterval, ((bool)(resources.GetObject("defaultInterval.ShowHelp"))));
            this.defaultInterval.Value = new decimal(new int[] {
            60,
            0,
            0,
            0});
            this.defaultInterval.KeyDown += new System.Windows.Forms.KeyEventHandler(this.defaultInterval_KeyDown);



            resources.ApplyResources(this.displayConfirmation, "displayConfirmation");
            this.helpProvider1.SetHelpString(this.displayConfirmation, resources.GetString("displayConfirmation.HelpString"));
            this.displayConfirmation.Name = "displayConfirmation";
            this.helpProvider1.SetShowHelp(this.displayConfirmation, ((bool)(resources.GetObject("displayConfirmation.ShowHelp"))));
            this.displayConfirmation.CheckedChanged += new System.EventHandler(this.displayConfirmation_CheckedChanged);



            resources.ApplyResources(this.displayTrayIcon, "displayTrayIcon");
            this.helpProvider1.SetHelpString(this.displayTrayIcon, resources.GetString("displayTrayIcon.HelpString"));
            this.displayTrayIcon.Name = "displayTrayIcon";
            this.helpProvider1.SetShowHelp(this.displayTrayIcon, ((bool)(resources.GetObject("displayTrayIcon.ShowHelp"))));
            this.displayTrayIcon.CheckedChanged += new System.EventHandler(this.displayTrayIcon_CheckedChanged);



            resources.ApplyResources(this.startInTrayIcon, "startInTrayIcon");
            this.helpProvider1.SetHelpString(this.startInTrayIcon, resources.GetString("startInTrayIcon.HelpString"));
            this.startInTrayIcon.Name = "startInTrayIcon";
            this.helpProvider1.SetShowHelp(this.startInTrayIcon, ((bool)(resources.GetObject("startInTrayIcon.ShowHelp"))));
            this.startInTrayIcon.CheckedChanged += new System.EventHandler(this.startInTrayIcon_CheckedChanged);



            resources.ApplyResources(this.hideSyncLog, "hideSyncLog");
            this.helpProvider1.SetHelpString(this.hideSyncLog, resources.GetString("hideSyncLog.HelpString"));
            this.hideSyncLog.Name = "hideSyncLog";
            this.helpProvider1.SetShowHelp(this.hideSyncLog, ((bool)(resources.GetObject("hideSyncLog.ShowHelp"))));
            this.hideSyncLog.CheckedChanged += new System.EventHandler(this.hideSyncLog_CheckedChanged);



            resources.ApplyResources(this.hidenotification, "hidenotification");
            this.helpProvider1.SetHelpString(this.hidenotification, resources.GetString("hidenotification.HelpString"));
            this.hidenotification.Name = "hidenotification";
            this.helpProvider1.SetShowHelp(this.hidenotification, ((bool)(resources.GetObject("hidenotification.ShowHelp"))));
            this.hidenotification.CheckedChanged += new System.EventHandler(this.hidenotification_CheckedChanged);



            resources.ApplyResources(this.tabControl1, "tabControl1");
            this.tabControl1.Controls.Add(this.tabGeneral);
            this.tabControl1.Controls.Add(this.tabAccounts);
            this.tabControl1.Controls.Add(this.Settings);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.helpProvider1.SetShowHelp(this.tabControl1, ((bool)(resources.GetObject("tabControl1.ShowHelp"))));
            this.tabControl1.SelectedIndexChanged += new System.EventHandler(this.tabControl1_SelectedIndexChanged);



            this.tabGeneral.Controls.Add(this.groupBox4);
            this.tabGeneral.Controls.Add(this.groupBox1);
            this.tabGeneral.Controls.Add(this.groupBox3);
            resources.ApplyResources(this.tabGeneral, "tabGeneral");
            this.tabGeneral.Name = "tabGeneral";
            this.helpProvider1.SetShowHelp(this.tabGeneral, ((bool)(resources.GetObject("tabGeneral.ShowHelp"))));
            this.tabGeneral.UseVisualStyleBackColor = true;



            resources.ApplyResources(this.groupBox4, "groupBox4");
            this.groupBox4.Controls.Add(this.notifyCollisions);
            this.groupBox4.Controls.Add(this.notifyShared);
            this.groupBox4.Controls.Add(this.notifyJoins);
            this.groupBox4.Controls.Add(this.hidenotification);
            this.groupBox4.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.groupBox4.Name = "groupBox4";
            this.helpProvider1.SetShowHelp(this.groupBox4, ((bool)(resources.GetObject("groupBox4.ShowHelp"))));
            this.groupBox4.TabStop = false;



            resources.ApplyResources(this.notifyCollisions, "notifyCollisions");
            this.helpProvider1.SetHelpString(this.notifyCollisions, resources.GetString("notifyCollisions.HelpString"));
            this.notifyCollisions.Name = "notifyCollisions";
            this.helpProvider1.SetShowHelp(this.notifyCollisions, ((bool)(resources.GetObject("notifyCollisions.ShowHelp"))));
            this.notifyCollisions.CheckedChanged += new System.EventHandler(this.notifyCollisions_CheckedChanged);



            resources.ApplyResources(this.notifyShared, "notifyShared");
            this.helpProvider1.SetHelpString(this.notifyShared, resources.GetString("notifyShared.HelpString"));
            this.notifyShared.Name = "notifyShared";
            this.helpProvider1.SetShowHelp(this.notifyShared, ((bool)(resources.GetObject("notifyShared.ShowHelp"))));
            this.notifyShared.CheckedChanged += new System.EventHandler(this.notifyShared_CheckedChanged);



            resources.ApplyResources(this.notifyJoins, "notifyJoins");
            this.helpProvider1.SetHelpString(this.notifyJoins, resources.GetString("notifyJoins.HelpString"));
            this.notifyJoins.Name = "notifyJoins";
            this.helpProvider1.SetShowHelp(this.notifyJoins, ((bool)(resources.GetObject("notifyJoins.ShowHelp"))));
            this.notifyJoins.CheckedChanged += new System.EventHandler(this.notifyJoins_CheckedChanged);



            resources.ApplyResources(this.groupBox1, "groupBox1");
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.timeUnit);
            this.groupBox1.Controls.Add(this.defaultInterval);
            this.groupBox1.Controls.Add(this.autoSync);
            this.groupBox1.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.groupBox1.Name = "groupBox1";
            this.helpProvider1.SetShowHelp(this.groupBox1, ((bool)(resources.GetObject("groupBox1.ShowHelp"))));
            this.groupBox1.TabStop = false;



            resources.ApplyResources(this.label1, "label1");
            this.label1.Name = "label1";
            this.helpProvider1.SetShowHelp(this.label1, ((bool)(resources.GetObject("label1.ShowHelp"))));



            resources.ApplyResources(this.timeUnit, "timeUnit");
            this.timeUnit.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.timeUnit.Name = "timeUnit";
            this.helpProvider1.SetShowHelp(this.timeUnit, ((bool)(resources.GetObject("timeUnit.ShowHelp"))));
            this.timeUnit.SelectedIndexChanged += new System.EventHandler(this.timeUnit_SelectedIndexChanged);



            this.autoSync.Checked = true;
            this.autoSync.CheckState = System.Windows.Forms.CheckState.Checked;
            resources.ApplyResources(this.autoSync, "autoSync");
            this.helpProvider1.SetHelpString(this.autoSync, resources.GetString("autoSync.HelpString"));
            this.autoSync.Name = "autoSync";
            this.helpProvider1.SetShowHelp(this.autoSync, ((bool)(resources.GetObject("autoSync.ShowHelp"))));
            this.autoSync.CheckedChanged += new System.EventHandler(this.autoSync_CheckedChanged);



            resources.ApplyResources(this.groupBox3, "groupBox3");
            this.groupBox3.Controls.Add(this.autoStart);
            this.groupBox3.Controls.Add(this.displayConfirmation);
            this.groupBox3.Controls.Add(this.displayTrayIcon);
            this.groupBox3.Controls.Add(this.startInTrayIcon);
            this.groupBox3.Controls.Add(this.hideSyncLog);
            this.groupBox3.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.groupBox3.Name = "groupBox3";
            this.helpProvider1.SetShowHelp(this.groupBox3, ((bool)(resources.GetObject("groupBox3.ShowHelp"))));
            this.groupBox3.TabStop = false;



            resources.ApplyResources(this.autoStart, "autoStart");
            this.helpProvider1.SetHelpString(this.autoStart, resources.GetString("autoStart.HelpString"));
            this.autoStart.Name = "autoStart";
            this.helpProvider1.SetShowHelp(this.autoStart, ((bool)(resources.GetObject("autoStart.ShowHelp"))));
            this.autoStart.CheckedChanged += new System.EventHandler(this.autoStart_CheckedChanged);



            this.tabAccounts.Controls.Add(this.accounts);
            this.tabAccounts.Controls.Add(this.details);
            this.tabAccounts.Controls.Add(this.removeAccount);
            this.tabAccounts.Controls.Add(this.addAccount);
            resources.ApplyResources(this.tabAccounts, "tabAccounts");
            this.tabAccounts.Name = "tabAccounts";
            this.helpProvider1.SetShowHelp(this.tabAccounts, ((bool)(resources.GetObject("tabAccounts.ShowHelp"))));
            this.tabAccounts.UseVisualStyleBackColor = true;



            resources.ApplyResources(this.accounts, "accounts");
            this.accounts.CheckBoxes = true;
            this.accounts.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader3,
            this.columnHeader2,
            this.columnHeader1});
            this.accounts.FullRowSelect = true;
            this.helpProvider1.SetHelpString(this.accounts, resources.GetString("accounts.HelpString"));
            this.accounts.HideSelection = false;
            this.accounts.MultiSelect = false;
            this.accounts.Name = "accounts";
            this.helpProvider1.SetShowHelp(this.accounts, ((bool)(resources.GetObject("accounts.ShowHelp"))));
            this.accounts.UseCompatibleStateImageBehavior = false;
            this.accounts.View = System.Windows.Forms.View.Details;
            this.accounts.SelectedIndexChanged += new System.EventHandler(this.accounts_SelectedIndexChanged);
            this.accounts.DoubleClick += new System.EventHandler(this.details_Click);
            this.accounts.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(this.accounts_ItemCheck);



            resources.ApplyResources(this.columnHeader3, "columnHeader3");



            resources.ApplyResources(this.columnHeader2, "columnHeader2");



            resources.ApplyResources(this.columnHeader1, "columnHeader1");



            resources.ApplyResources(this.details, "details");
            this.helpProvider1.SetHelpString(this.details, resources.GetString("details.HelpString"));
            this.details.Name = "details";
            this.helpProvider1.SetShowHelp(this.details, ((bool)(resources.GetObject("details.ShowHelp"))));
            this.details.Click += new System.EventHandler(this.details_Click);



            resources.ApplyResources(this.removeAccount, "removeAccount");
            this.helpProvider1.SetHelpString(this.removeAccount, resources.GetString("removeAccount.HelpString"));
            this.removeAccount.Name = "removeAccount";
            this.helpProvider1.SetShowHelp(this.removeAccount, ((bool)(resources.GetObject("removeAccount.ShowHelp"))));
            this.removeAccount.Click += new System.EventHandler(this.removeAccount_Click);



            resources.ApplyResources(this.addAccount, "addAccount");
            this.helpProvider1.SetHelpString(this.addAccount, resources.GetString("addAccount.HelpString"));
            this.addAccount.Name = "addAccount";
            this.helpProvider1.SetShowHelp(this.addAccount, ((bool)(resources.GetObject("addAccount.ShowHelp"))));
            this.addAccount.Click += new System.EventHandler(this.addAccount_Click);



            this.Settings.BackColor = System.Drawing.SystemColors.Control;
            this.Settings.Controls.Add(this.notifyCheckbox);
            this.Settings.Controls.Add(this.policylistbox);
            resources.ApplyResources(this.Settings, "Settings");
            this.Settings.Name = "Settings";



            resources.ApplyResources(this.notifyCheckbox, "notifyCheckbox");
            this.notifyCheckbox.Name = "notifyCheckbox";
            this.notifyCheckbox.UseVisualStyleBackColor = true;



            this.policylistbox.FormattingEnabled = true;
            this.policylistbox.Items.AddRange(new object[] {
            resources.GetString("policylistbox.Items"),
            resources.GetString("policylistbox.Items1"),
            resources.GetString("policylistbox.Items2"),
            resources.GetString("policylistbox.Items3"),
            resources.GetString("policylistbox.Items4"),
            resources.GetString("policylistbox.Items5")});
            resources.ApplyResources(this.policylistbox, "policylistbox");
            this.policylistbox.Name = "policylistbox";
            this.policylistbox.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended;
            this.policylistbox.SelectedIndexChanged += new System.EventHandler(this.policylistbox_SelectedIndexChanged);



            this.tabMigrate.Controls.Add(this.btnMigrate);
            this.tabMigrate.Controls.Add(this.listView1);
            resources.ApplyResources(this.tabMigrate, "tabMigrate");
            this.tabMigrate.Name = "tabMigrate";
            this.helpProvider1.SetShowHelp(this.tabMigrate, ((bool)(resources.GetObject("tabMigrate.ShowHelp"))));



            resources.ApplyResources(this.btnMigrate, "btnMigrate");
            this.btnMigrate.Name = "btnMigrate";
            this.helpProvider1.SetShowHelp(this.btnMigrate, ((bool)(resources.GetObject("btnMigrate.ShowHelp"))));
            this.btnMigrate.Click += new System.EventHandler(this.btnMigrate_Click);



            resources.ApplyResources(this.listView1, "listView1");
            this.listView1.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader4,
            this.columnHeader5});
            this.listView1.FullRowSelect = true;
            this.listView1.HideSelection = false;
            this.listView1.Name = "listView1";
            this.helpProvider1.SetShowHelp(this.listView1, ((bool)(resources.GetObject("listView1.ShowHelp"))));
            this.listView1.UseCompatibleStateImageBehavior = false;
            this.listView1.View = System.Windows.Forms.View.Details;



            resources.ApplyResources(this.columnHeader4, "columnHeader4");



            resources.ApplyResources(this.columnHeader5, "columnHeader5");



            resources.ApplyResources(this.cancel, "cancel");
            this.cancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancel.Name = "cancel";
            this.helpProvider1.SetShowHelp(this.cancel, ((bool)(resources.GetObject("cancel.ShowHelp"))));
            this.cancel.Click += new System.EventHandler(this.cancel_Click);



            resources.ApplyResources(this.apply, "apply");
            this.apply.Name = "apply";
            this.helpProvider1.SetShowHelp(this.apply, ((bool)(resources.GetObject("apply.ShowHelp"))));
            this.apply.Click += new System.EventHandler(this.apply_Click);



            resources.ApplyResources(this.ok, "ok");
            this.ok.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.ok.Name = "ok";
            this.helpProvider1.SetShowHelp(this.ok, ((bool)(resources.GetObject("ok.ShowHelp"))));
            this.ok.Click += new System.EventHandler(this.ok_Click);



            this.timer1.Interval = 10;
            this.timer1.Tick += new System.EventHandler(this.timer1_Tick);



            resources.ApplyResources(this.btnHelp, "btnHelp");
            this.btnHelp.Name = "btnHelp";
            this.helpProvider1.SetShowHelp(this.btnHelp, ((bool)(resources.GetObject("btnHelp.ShowHelp"))));
            this.btnHelp.Click += new System.EventHandler(this.btnHelp_Click);



            this.AcceptButton = this.ok;
            resources.ApplyResources(this, "$this");
            this.BackColor = System.Drawing.Color.Gainsboro;
            this.CancelButton = this.cancel;
            this.Controls.Add(this.ok);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.apply);
            this.Controls.Add(this.cancel);
            this.Controls.Add(this.btnHelp);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.HelpButton = true;
            this.KeyPreview = true;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "Preferences";
            this.helpProvider1.SetShowHelp(this, ((bool)(resources.GetObject("$this.ShowHelp"))));
            this.Load += new System.EventHandler(this.Preferences_Load);
            this.VisibleChanged += new System.EventHandler(this.Preferences_VisibleChanged);
            this.Closing += new System.ComponentModel.CancelEventHandler(this.Preferences_Closing);
            this.Move += new System.EventHandler(this.Preferences_Move);
            ((System.ComponentModel.ISupportInitialize)(this.defaultInterval)).EndInit();
            this.tabControl1.ResumeLayout(false);
            this.tabGeneral.ResumeLayout(false);
            this.groupBox4.ResumeLayout(false);
            this.groupBox1.ResumeLayout(false);
            this.groupBox3.ResumeLayout(false);
            this.tabAccounts.ResumeLayout(false);
            this.Settings.ResumeLayout(false);
            this.Settings.PerformLayout();
            this.tabMigrate.ResumeLayout(false);
            this.ResumeLayout(false);

        }






        public static bool NotifyShareEnabled
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(notifyShareDisabled, 0);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {

                    regKey.DeleteValue(notifyShareDisabled, false);
                }
                else
                {

                    regKey.SetValue(notifyShareDisabled, 1);
                }
            }
        }




        public static bool NotifyCollisionEnabled
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(notifyCollisionDisabled, 0);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {

                    regKey.DeleteValue(notifyCollisionDisabled, false);
                }
                else
                {

                    regKey.SetValue(notifyCollisionDisabled, 1);
                }
            }
        }




        public static bool NotifyJoinEnabled
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(notifyJoinDisabled, 0);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {

                    regKey.DeleteValue(notifyJoinDisabled, false);
                }
                else
                {

                    regKey.SetValue(notifyJoinDisabled, 1);
                }
            }
        }




        public static bool HideiFolderInTray
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(startiFolderinTray, 1);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {



                    regKey.SetValue(startiFolderinTray, 0);
                }
                else
                {

                    regKey.SetValue(startiFolderinTray, 1);
                }
            }
        }


        public static bool HideSyncLogWindow
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(hideSyncWindowPopup, 1);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {

                    regKey.SetValue(hideSyncWindowPopup, 0);
                }
                else
                {

                    regKey.SetValue(hideSyncWindowPopup, 1);
                }
            }
        }




        public static bool HidePolicyVoilationNotification
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(hidePolicynotification, 1);

                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {

                    regKey.SetValue(hidePolicynotification, 0);
                }
                else
                {


                    regKey.SetValue(hidePolicynotification, 1);
                }
            }
        }


        public static bool NotifyPolicyQouta
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(notifyPolicyQuotaVoilation, 1);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {



                    regKey.SetValue(notifyPolicyQuotaVoilation, 0);
                }
                else
                {

                    regKey.SetValue(notifyPolicyQuotaVoilation, 1);
                }
            }
        }


        public static bool NotifyFilePermission
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(notifyFilePermissionVoilation, 1);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {



                    regKey.SetValue(notifyFilePermissionVoilation, 0);
                }
                else
                {

                    regKey.SetValue(notifyFilePermissionVoilation, 1);
                }
            }
        }


        public static bool NotifyDiskFull
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(notifyDiskFullFailure, 1);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {



                    regKey.SetValue(notifyDiskFullFailure, 0);
                }
                else
                {

                    regKey.SetValue(notifyDiskFullFailure, 1);
                }
            }
        }


        public static bool NotifyPolicyType
        {

            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(notifyPolicyTypeVoilation, 1);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {



                    regKey.SetValue(notifyPolicyTypeVoilation, 0);
                }
                else
                {

                    regKey.SetValue(notifyPolicyTypeVoilation, 1);
                }
            }
        }


        public static bool NotifyPolicySize
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(notifyPolicySizeVoilation, 1);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {



                    regKey.SetValue(notifyPolicySizeVoilation, 0);
                }
                else
                {

                    regKey.SetValue(notifyPolicySizeVoilation, 1);
                }
            }
        }


        public static bool NotifyIOPermission
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(notifyIOPermissionFailure, 1);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {



                    regKey.SetValue(notifyIOPermissionFailure, 0);
                }
                else
                {

                    regKey.SetValue(notifyIOPermissionFailure, 1);
                }
            }
        }


        public static bool NotifyPathLong
        {
            get
            {
                int notify;
                try
                {

                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);


                    notify = (int)regKey.GetValue(notifyPathLongFailure, 1);
                }
                catch
                {
                    return true;
                }

                return (notify == 0);
            }
            set
            {

                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);

                if (value)
                {



                    regKey.SetValue(notifyPathLongFailure, 0);
                }
                else
                {

                    regKey.SetValue(notifyPathLongFailure, 1);
                }
            }
        }


        public iFolderWebService ifolderWebService
        {
            set
            {
                this.ifWebService = value;
            }
        }

        public SimiasWebService Simws
        {
            set
            {
                this.simiasWebService = value;
            }
        }

        public Manager simManager
        {
            set
            {
                this.simiasManager = value;
            }
        }
        static public bool IsRunEnabled()
        {
            int run;
            try
            {
                RegistryKey regKey = Registry.CurrentUser.OpenSubKey(iFolderKey);
                run = (int)regKey.GetValue(iFolderRun, 0);
            }
            catch
            {
                return true;
            }
            return (run == 0);
        }
        public void SelectAccounts(bool createAccount)
        {
            tabControl1.SelectedTab = tabAccounts;
            if (createAccount)
            {
                addAccount_Click(this, new EventArgs());
            }
        }
        public void SelectGeneral()
        {
            tabControl1.SelectedTab = tabGeneral;
        }
        public void AddDomainToList(DomainInformation domainInfo)
        {
            Domain domain = null;
            foreach (ListViewItem lvi in accounts.Items)
            {
                Domain d = (Domain)lvi.Tag;
                if (d.ID.Equals(domainInfo.ID))
                {
                    domain = d;
                    break;
                }
            }
            if (domain == null)
            {
                domain = new Domain(domainInfo);
                if (domainInfo.IsDefault)
                {
                    if ((currentDefaultDomain != null) && !currentDefaultDomain.ID.Equals(domainInfo.ID))
                    {
                        currentDefaultDomain.DomainInfo.IsDefault = false;
                    }
                    currentDefaultDomain = domain;
                    if (ChangeDefaultDomain != null)
                    {
                        ChangeDefaultDomain(this, new DomainConnectEventArgs(currentDefaultDomain.DomainInfo));
                    }
                }
                ListViewItem lvi = new ListViewItem(
                    new string[] { string.Empty, domain.Name,
          domainInfo.MemberName });
                lvi.Checked = domainInfo.Authenticated;
                lvi.Tag = domain;
                lvi.Selected = domainInfo.IsDefault;
                accounts.Items.Add(lvi);
            }
        }
        public string GetDomainName(string poBoxID)
        {
            string name = string.Empty;
            foreach (ListViewItem lvi in accounts.Items)
            {
                Domain d = (Domain)lvi.Tag;
                if (d.DomainInfo.POBoxID != null && d.DomainInfo.POBoxID.Equals(poBoxID))
                {
                    name = d.Name;
                    break;
                }
            }
            return name;
        }
        public bool IsCurrentUser(string userID)
        {
            bool result = false;
            foreach (ListViewItem lvi in accounts.Items)
            {
                Domain d = (Domain)lvi.Tag;
                if (d.DomainInfo.MemberUserID.Equals(userID))
                {
                    result = true;
                    break;
                }
            }
            return result;
        }
        public bool IsPOBox(string poBoxID)
        {
            bool result = false;
            return result;
        }
        public void RemoveDomainFromList(DomainInformation domainInfo, string defaultDomainID)
        {
            ListViewItem lvitem = null;
            Domain defaultDomain = null;
            try
            {
                foreach (ListViewItem lvi in accounts.Items)
                {
                    Domain d = (Domain)lvi.Tag;
                    if (d.ID.Equals(domainInfo.ID))
                    {
                        lvitem = lvi;
                    }
                    else if ((defaultDomainID != null) && d.ID.Equals(defaultDomainID))
                    {
                        defaultDomain = d;
                    }
                }
                if (lvitem != null)
                {
                    lvitem.Remove();
                }
                if (defaultDomain != null)
                {
                    if ((currentDefaultDomain != null) && !currentDefaultDomain.ID.Equals(defaultDomainID))
                    {
                        currentDefaultDomain.DomainInfo.IsDefault = false;
                    }
                    currentDefaultDomain = defaultDomain;
                }
            }
            catch { }
        }
        public void SetProxyForDomain(string hostUrl, bool unknownScheme)
        {
            UriBuilder ubHost = new UriBuilder(hostUrl);
            if (unknownScheme)
            {
                ubHost.Scheme = Uri.UriSchemeHttp;
                ubHost.Port = 80;
                SetProxyForDomain(ubHost.Uri.ToString(), false);
                ubHost.Scheme = Uri.UriSchemeHttps;
                ubHost.Port = 443;
                SetProxyForDomain(ubHost.Uri.ToString(), false);
            }
            else
            {
                IWebProxy iwp = GlobalProxySelection.Select;
                if (!iwp.IsBypassed(ubHost.Uri))
                {
                    string proxyUser = null;
                    string proxyPassword = null;
                    Uri proxyUri = iwp.GetProxy(ubHost.Uri);
                    if (iwp.Credentials != null)
                    {
                        NetworkCredential netCred = iwp.Credentials.GetCredential(proxyUri, "Basic");
                        if (netCred != null)
                        {
                            proxyUser = netCred.UserName;
                            proxyPassword = netCred.Password;
                        }
                    }
                    simiasWebService.SetProxyAddress(ubHost.Uri.ToString(), proxyUri.ToString(), proxyUser, proxyPassword);
                }
            }
        }
        public bool MachineShutdown()
        {
            return this.shutdown;
        }
        public void UpdateDomainStatus(Domain domain)
        {
            foreach (ListViewItem lvi in accounts.Items)
            {
                Domain d = (Domain)lvi.Tag;
                if (d.ID.Equals(domain.ID))
                {
                    lvi.Tag = domain;
                    lvi.Checked = domain.DomainInfo.Authenticated;
                    break;
                }
            }
            if (UpdateDomain != null)
            {
                UpdateDomain(this, new DomainConnectEventArgs(domain.DomainInfo));
            }
        }
        public void AddMigrationDetails()
        {
            string iFolderRegistryKey = @"Software\Novell iFolder";
            RegistryKey iFolderKey = Registry.LocalMachine.OpenSubKey(iFolderRegistryKey);
            string[] AllKeys = new string[iFolderKey.SubKeyCount];
            string User;
            AllKeys = iFolderKey.GetSubKeyNames();
            this.listView1.Items.Clear();
            for (int i = 0; i < AllKeys.Length; i++)
            {
                ListViewItem lvi;
                User = iFolderRegistryKey + "\\" + AllKeys[i];
                RegistryKey UserKey = Registry.LocalMachine.OpenSubKey(User);
                if (UserKey.GetValue("FolderPath") != null)
                {
                    lvi = new ListViewItem(new string[] { AllKeys[i], (string)UserKey.GetValue("FolderPath") });
                    listView1.Items.Add(lvi);
                    lvi.Selected = true;
                }
                UserKey.Close();
            }
            iFolderKey.Close();
        }
        private int calculateSize(Control control, int delta)
        {
            int size;
            Graphics g = control.CreateGraphics();
            try
            {
                SizeF textSize = g.MeasureString(control.Text, control.Font);
                size = (int)Math.Ceiling(textSize.Width) - control.Width;
            }
            finally
            {
                g.Dispose();
            }
            return (int)Math.Max(delta, size);
        }
        private void displaySyncInterval(int syncInterval)
        {
            autoSync.Checked = syncInterval != System.Threading.Timeout.Infinite;
            string units = resourceManager.GetString("seconds");
            decimal displayValue = autoSync.Checked ?
                iFolderAdvanced.ConvertSecondsToTimeUnit(syncInterval, out units) : minimumSeconds;
            switch (units)
            {
                case "seconds":
                    timeUnit.SelectedIndex = 0;
                    break;
                case "minutes":
                    timeUnit.SelectedIndex = 1;
                    break;
                case "hours":
                    timeUnit.SelectedIndex = 2;
                    break;
                case "days":
                    timeUnit.SelectedIndex = 3;
                    break;
            }
            try
            {
                defaultInterval.Value = displayValue;
            }
            catch (ArgumentOutOfRangeException ae)
            {
                defaultInterval.Value = syncInterval;
                timeUnit.SelectedIndex = 0;
            }
            catch (Exception e)
            {
                throw e;
            }
        }
        private bool login(int itemIndex)
        {
            bool result = false;
            ListViewItem lvi = accounts.Items[itemIndex];
            Domain domain = (Domain)lvi.Tag;
            result = loginToDomain(domain);
            if (result)
                lvi.Tag = domain;
            return result;
        }
        public bool loginToDomain(Domain domain)
        {
            bool result = false;
            if (domain != null)
            {
                Connecting connecting = new Connecting(this.ifWebService, simiasWebService, simiasManager, domain.DomainInfo);
                if (connecting.ShowDialog() == DialogResult.OK)
                {
                    result = true;
                }
                if (!result)
                {
                    ServerInfo serverInfo = new ServerInfo(this.ifWebService, simiasManager, domain.DomainInfo, connecting.Password);
                    serverInfo.ShowDialog();
                    result = serverInfo.DomainInfo.Authenticated;
                    serverInfo.Dispose();
                }
                connecting.Dispose();
            }
            if (result)
            {
                domain.DomainInfo.Authenticated = true;
                FormsTrayApp.globalProp().updateifListViewDomainStatus(domain.DomainInfo.ID, true);
                FormsTrayApp.globalProp().AddDomainToUIList(domain.DomainInfo);
                FormsTrayApp.globalProp().UpdateiFolderStatus(domain.DomainInfo.Authenticated, domain.DomainInfo.ID);
            }
            return result;
        }
        private bool logout(int itemIndex)
        {
            bool result = false;
            ListViewItem lvi = accounts.Items[itemIndex];
            Domain domain = (Domain)lvi.Tag;
            result = logoutFromDomain(domain);
            if(result)
                lvi.Tag = domain;
            return result;
        }
        public bool logoutFromDomain(Domain domain)
        {
            bool result = false;
            if (domain != null)
            {
                DomainAuthentication domainAuth = new DomainAuthentication("iFolder", domain.ID, null);
                Status authStatus = domainAuth.Logout(simiasManager.WebServiceUri, simiasManager.DataPath);
                if (authStatus != null && authStatus.statusCode == StatusCodes.Success)
                {
                    result = true;
                    domain.DomainInfo.Authenticated = false;
                    if (this.simiasWebService.GetRememberOption(domain.ID) == false)
                    {
                        this.simiasWebService.StorePassPhrase(domain.ID, "", CredentialType.None, false);
                    }
                    FormsTrayApp.globalProp().updateifListViewDomainStatus(domain.DomainInfo.ID, false);
                    (FormsTrayApp.globalProp()).RemoveDomainFromUIList(domain.DomainInfo.ID, null);
                    FormsTrayApp.globalProp().UpdateiFolderStatus(domain.DomainInfo.Authenticated, domain.DomainInfo.ID);
                }
            }
            return result;
        }
        private bool processChanges()
        {
            bool result = true;
            Cursor.Current = Cursors.WaitCursor;
            if (autoStart.Checked != IsRunEnabled())
            {
                setAutoRunValue(!autoStart.Checked);
            }
            NotifyShareEnabled = notifyShared.Checked;
            NotifyCollisionEnabled = notifyCollisions.Checked;
            NotifyJoinEnabled = notifyJoins.Checked;
            HideiFolderInTray = startInTrayIcon.Checked;
            HideSyncLogWindow = hideSyncLog.Checked;
            HidePolicyVoilationNotification = hidenotification.Checked;
            iFolderComponent.DisplayConfirmationEnabled = displayConfirmation.Checked;
            iFolderComponent.DisplayTrayIconEnabled = !(displayTrayIcon.Checked);
            if (displayTrayIcon.Checked)
                FormsTrayApp.SetTrayIconStatus(false);
            else
                FormsTrayApp.SetTrayIconStatus(true);
            try
            {
                ListBox.SelectedIndexCollection selectedIndex = policylistbox.SelectedIndices;
                if (notifyCheckbox.Checked)
                {
                    NotifyPolicyQouta = false;
                    NotifyPolicySize = false;
                    NotifyPolicyType = false;
                    NotifyDiskFull = false;
                    NotifyIOPermission = false;
                    NotifyPathLong = false;
                    foreach (int index in selectedIndex)
                    {
                        switch (index)
                        {
                            case (int)policyVoilation.QuotaVoliation:
                                NotifyPolicyQouta = true;
                                break;
                            case (int)policyVoilation.FileSizeVoilation:
                                NotifyPolicySize = true;
                                break;
                            case (int)policyVoilation.FileTypeVoilation:
                                NotifyPolicyType = true;
                                break;
                            case (int)policyVoilation.DiskFullVoilation:
                                NotifyDiskFull = true;
                                break;
                            case (int)policyVoilation.PremissionUnavailable:
                                NotifyIOPermission = true;
                                break;
                            case (int)policyVoilation.LongPath:
                                NotifyPathLong = true;
                                break;
                            default:
                                FormsTrayApp.log.Debug("invalid index");
                                break;
                        }
                    }
                }
                decimal syncValueInSeconds;
                if (((string)timeUnit.SelectedItem).Equals(resourceManager.GetString("days")))
                {
                    syncValueInSeconds = defaultInterval.Value * 86400;
                }
                else if (((string)timeUnit.SelectedItem).Equals(resourceManager.GetString("hours")))
                {
                    syncValueInSeconds = defaultInterval.Value * 3600;
                }
                else if (((string)timeUnit.SelectedItem).Equals(resourceManager.GetString("minutes")))
                {
                    syncValueInSeconds = defaultInterval.Value * 60;
                }
                else
                {
                    syncValueInSeconds = defaultInterval.Value;
                }
                int currentInterval = ifWebService.GetDefaultSyncInterval();
                if ((!syncValueInSeconds.Equals((decimal)currentInterval)) ||
                    (autoSync.Checked != (currentInterval != System.Threading.Timeout.Infinite)))
                {
                    try
                    {
                        ifWebService.SetDefaultSyncInterval(autoSync.Checked ? (int)syncValueInSeconds : System.Threading.Timeout.Infinite);
                        if (autoSync.Checked)
                        {
                            displaySyncInterval((int)syncValueInSeconds);
                        }
                    }
                    catch (Exception ex)
                    {
                        result = false;
                        Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("saveSyncError"), resourceManager.GetString("PreferencesErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                        mmb.ShowDialog();
                        mmb.Dispose();
                    }
                }
            }
            catch (Exception ex)
            {
                result = false;
                Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("readSyncError"), resourceManager.GetString("PreferencesErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                mmb.ShowDialog();
                mmb.Dispose();
            }
            Cursor.Current = Cursors.Default;
            return result;
        }
        private void resizeButton(Button button)
        {
            Graphics g = button.CreateGraphics();
            try
            {
                Point p = button.Location;
                int width = button.Width;
                SizeF size = g.MeasureString(button.Text, button.Font);
                button.Width = (int)Math.Ceiling(size.Width) + 20;
                if ((button.Anchor & AnchorStyles.Right) == AnchorStyles.Right)
                {
                    button.Left = p.X - (button.Width - width);
                }
            }
            finally
            {
                g.Dispose();
            }
        }
        private void setAutoRunValue(bool disable)
        {
            RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);
            if (disable)
            {
                regKey.SetValue(iFolderRun, 1);
            }
            else
            {
                regKey.DeleteValue(iFolderRun, false);
            }
        }
        public delegate void EnterpriseConnectDelegate(object sender, DomainConnectEventArgs e);
        public event EnterpriseConnectDelegate EnterpriseConnect;
        public delegate void ChangeDefaultDomainDelegate(object sender, DomainConnectEventArgs e);
        public event ChangeDefaultDomainDelegate ChangeDefaultDomain;
        public delegate void RemoveDomainDelegate(object sender, DomainRemoveEventArgs e);
        public event RemoveDomainDelegate RemoveDomain;
        public delegate void ShutdownTrayAppDelegate(object sender, EventArgs e);
        public event ShutdownTrayAppDelegate ShutdownTrayApp;
        public delegate void UpdateDomainDelegate(object sender, DomainConnectEventArgs e);
        public event UpdateDomainDelegate UpdateDomain;
        public delegate void DisplayiFolderDialogDelegate(object sender, EventArgs e);
        public event DisplayiFolderDialogDelegate DisplayiFolderDialog;
        private void accountWizard_EnterpriseConnect(object sender, DomainConnectEventArgs e)
        {
            AddDomainToList(e.DomainInfo);
            if (EnterpriseConnect != null)
            {
                EnterpriseConnect(this, new DomainConnectEventArgs(e.DomainInfo));
            }
        }
        private void Preferences_Load(object sender, System.EventArgs e)
        {
            string helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"preferences.html");
            if (!File.Exists(helpFile))
            {
                helpFile = Path.Combine(Application.StartupPath, @"help\en\preferences.html");
            }
            if (File.Exists(helpFile))
            {
                helpProvider1.HelpNamespace = helpFile;
            }
            try
            {
                this.Icon = new Icon(Path.Combine(Application.StartupPath, @"res\ifolder_16.ico"));
            }
            catch { }
            if (Environment.OSVersion.Version.Major > 4
                & Environment.OSVersion.Version.Minor > 0
                & System.IO.File.Exists(Application.ExecutablePath + ".manifest"))
            {
            }
            minimumSyncInterval = 1;
            timeUnit.Items.Add(resourceManager.GetString("seconds"));
            timeUnit.Items.Add(resourceManager.GetString("minutes"));
            timeUnit.Items.Add(resourceManager.GetString("hours"));
            timeUnit.Items.Add(resourceManager.GetString("days"));
        }
        private void Preferences_VisibleChanged(object sender, System.EventArgs e)
        {
            if (this.Visible)
            {
                accounts.Items.Clear();
                successful = true;
                DomainInformation[] domains;
                try
                {
                    domains = simiasWebService.GetDomains(true);
                    foreach (DomainInformation di in domains)
                    {
                        AddDomainToList(di);
                    }
                }
                catch (Exception ex)
                {
                    Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("readAccountsError"), resourceManager.GetString("accountErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                    mmb.ShowDialog();
                    mmb.Dispose();
                }
                apply.Enabled = false;
                autoStart.Checked = IsRunEnabled();
                notifyShared.Checked = NotifyShareEnabled;
                notifyCollisions.Checked = NotifyCollisionEnabled;
                notifyJoins.Checked = NotifyJoinEnabled;
                startInTrayIcon.Checked = HideiFolderInTray;
                hideSyncLog.Checked = HideSyncLogWindow;
                hidenotification.Checked = HidePolicyVoilationNotification;
                displayConfirmation.Checked = iFolderComponent.DisplayConfirmationEnabled;
                displayTrayIcon.Checked = !(iFolderComponent.DisplayTrayIconEnabled);
                notifyCheckbox.Checked = false;
                policylistbox.ClearSelected();
                if (NotifyPolicyQouta)
                    policylistbox.SelectedIndex = (int)policyVoilation.QuotaVoliation;
                if (NotifyPolicySize)
                    policylistbox.SelectedIndex = (int)policyVoilation.FileSizeVoilation;
                if (NotifyPolicyType)
                    policylistbox.SelectedIndex = (int)policyVoilation.FileTypeVoilation;
                if (NotifyPathLong)
                    policylistbox.SelectedIndex = (int)policyVoilation.LongPath;
                if (NotifyIOPermission)
                    policylistbox.SelectedIndex = (int)policyVoilation.PremissionUnavailable;
                if (NotifyDiskFull)
                    policylistbox.SelectedIndex = (int)policyVoilation.DiskFullVoilation;
                try
                {
                    int syncInterval = ifWebService.GetDefaultSyncInterval();
                    minimumSeconds = (!syncInterval.Equals(System.Threading.Timeout.Infinite) &&
                        (syncInterval < (int)defaultMinimumSeconds)) ? (decimal)syncInterval : defaultMinimumSeconds;
                    displaySyncInterval(syncInterval);
                }
                catch (Exception ex)
                {
                    Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("readSyncError"), resourceManager.GetString("PreferencesErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                    mmb.ShowDialog();
                    mmb.Dispose();
                }
                Activate();
            }
        }
        private void Preferences_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            if (!successful && DialogResult.Equals(DialogResult.OK))
            {
                e.Cancel = true;
            }
            else if (!shutdown)
            {
                e.Cancel = true;
                currentDefaultDomain = null;
                newAccountLvi = null;
                addAccount.Enabled = true;
                details.Enabled = removeAccount.Enabled = false;
                updatePassword = updateEnabled = updateHost = false;
                Hide();
            }
        }
        private void ok_Click(object sender, System.EventArgs e)
        {
            verifyInterval();
            successful = processChanges();
            Close();
        }
        private void displayTrayIcon_CheckedChanged(object sender, System.EventArgs e)
        {
            if (displayTrayIcon.Focused)
            {
                apply.Enabled = true;
            }
        }
        private void startInTrayIcon_CheckedChanged(object sender, System.EventArgs e)
        {
            if (startInTrayIcon.Focused)
            {
                apply.Enabled = true;
            }
        }
        private void hidenotification_CheckedChanged(object sender, System.EventArgs e)
        {
            if (hidenotification.Focused)
            {
                apply.Enabled = true;
            }
        }
        private void hideSyncLog_CheckedChanged(object sender, System.EventArgs e)
        {
            if (hideSyncLog.Focused)
            {
                apply.Enabled = true;
            }
        }
        private void apply_Click(object sender, System.EventArgs e)
        {
            verifyInterval();
            if (processChanges())
            {
                apply.Enabled = false;
            }
        }
        private void cancel_Click(object sender, System.EventArgs e)
        {
            Close();
        }
        private void autoStart_CheckedChanged(object sender, System.EventArgs e)
        {
            if (autoStart.Focused)
            {
                apply.Enabled = true;
            }
        }
        private void displayConfirmation_CheckedChanged(object sender, System.EventArgs e)
        {
            if (displayConfirmation.Focused)
            {
                apply.Enabled = true;
            }
        }
        private void autoSync_CheckedChanged(object sender, System.EventArgs e)
        {
            if (autoSync.Focused)
            {
                apply.Enabled = true;
            }
            defaultInterval.Enabled = timeUnit.Enabled = label1.Enabled = autoSync.Checked;
        }
        private void notifyShared_CheckedChanged(object sender, System.EventArgs e)
        {
            if (notifyShared.Focused)
            {
                apply.Enabled = true;
            }
        }
        private void notifyCollisions_CheckedChanged(object sender, System.EventArgs e)
        {
            if (notifyCollisions.Focused)
            {
                apply.Enabled = true;
            }
        }
        private void notifyJoins_CheckedChanged(object sender, System.EventArgs e)
        {
            if (notifyJoins.Focused)
            {
                apply.Enabled = true;
            }
        }
        private void defaultInterval_ValueChanged(object sender, System.EventArgs e)
        {
            if (defaultInterval.Focused)
            {
                try
                {
                    if (!defaultInterval.Text.Equals(string.Empty))
                    {
                        defaultInterval.Value = decimal.Parse(defaultInterval.Text);
                    }
                }
                catch
                {
                    defaultInterval.Value = minimumSyncInterval;
                }
                if (defaultInterval.Value < minimumSyncInterval)
                {
                    defaultInterval.Value = minimumSyncInterval;
                }
                apply.Enabled = true;
            }
        }
        private void defaultInterval_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
        {
            if (((e.KeyCode & Keys.F) == Keys.F) &&
                ((e.Modifiers & Keys.Shift) == Keys.Shift) &&
                ((e.Modifiers & Keys.Control) == Keys.Control))
            {
                defaultInterval.Minimum = minimumSyncInterval = 0;
            }
            else
            {
                timeUnit_SelectedIndexChanged(this, new EventArgs());
            }
        }
        private void timeUnit_SelectedIndexChanged(object sender, System.EventArgs e)
        {
            if (timeUnit.Focused)
            {
                apply.Enabled = true;
            }
            if (((string)timeUnit.SelectedItem).Equals(resourceManager.GetString("seconds")))
            {
                defaultInterval.Maximum = maximumSeconds;
                defaultInterval.Increment = 1;
            }
            else if (((string)timeUnit.SelectedItem).Equals(resourceManager.GetString("minutes")))
            {
                minimumSyncInterval = 1;
                defaultInterval.Increment = 1;
                defaultInterval.Maximum = maximumMinutes;
            }
            else if (((string)timeUnit.SelectedItem).Equals(resourceManager.GetString("hours")))
            {
                minimumSyncInterval = defaultInterval.Increment = 1;
                defaultInterval.Maximum = maximumHours;
            }
            else if (((string)timeUnit.SelectedItem).Equals(resourceManager.GetString("days")))
            {
                minimumSyncInterval = defaultInterval.Increment = 1;
                defaultInterval.Maximum = maximumDays;
            }
            defaultInterval.Minimum = minimumSyncInterval;
        }
        private void addAccount_Click(object sender, System.EventArgs e)
        {
            AccountWizard accountWizard = new AccountWizard(ifWebService, simiasWebService, simiasManager, accounts.Items.Count == 0, this, (GlobalProperties)FormsTrayApp.globalProp());
            accountWizard.EnterpriseConnect += new Novell.Wizard.AccountWizard.EnterpriseConnectDelegate(accountWizard_EnterpriseConnect);
            if (accountWizard.ShowDialog() == DialogResult.OK)
            {
                if (DisplayiFolderDialog != null)
                {
                    DisplayiFolderDialog(this, new EventArgs());
                }
            }
            accountWizard.Dispose();
            this.Focus();
        }
        private void removeAccount_Click(object sender, System.EventArgs e)
        {
            ListViewItem lvi = accounts.SelectedItems[0];
            Domain domain = (Domain)lvi.Tag;
            if (domain == null)
            {
                newAccountLvi = null;
                lvi.Remove();
                updatePassword = updateEnabled = updateHost = false;
                addAccount.Enabled = true;
            }
            else
            {
                RemoveAccount removeAccount = new RemoveAccount(domain.DomainInfo);
                if (removeAccount.ShowDialog() == DialogResult.Yes)
                {
                    try
                    {
                        simiasWebService.LeaveDomain(domain.ID, !removeAccount.RemoveAll);
                        lvi.Remove();
                        string defaultDomainID = null;
                        if (domain.Equals(currentDefaultDomain))
                        {
                            defaultDomainID = simiasWebService.GetDefaultDomainID();
                        }
                        if (RemoveDomain != null)
                        {
                            RemoveDomain(this, new DomainRemoveEventArgs(domain.DomainInfo, defaultDomainID));
                        }
                        if (defaultDomainID != null)
                        {
                            foreach (ListViewItem item in accounts.Items)
                            {
                                Domain d = (Domain)item.Tag;
                                if (d.ID.Equals(defaultDomainID))
                                {
                                    currentDefaultDomain = d;
                                    break;
                                }
                            }
                        }
                        updatePassword = updateEnabled = updateHost = false;
                    }
                    catch (Exception ex)
                    {
                        MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("removeAccountError"), resourceManager.GetString("accountErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                        mmb.ShowDialog();
                        mmb.Dispose();
                    }
                }
                DomainInformation[] domains;
                System.Threading.Thread.Sleep(2000);
                domains = this.simiasWebService.GetDomains(false);
                if (domains.Length.Equals(0))
                {
                    if (((GlobalProperties)FormsTrayApp.globalProp()).Visible)
                        ((GlobalProperties)FormsTrayApp.globalProp()).Hide();
                }
                removeAccount.Dispose();
            }
        }
        private void timer1_Tick(object sender, System.EventArgs e)
        {
        }
        private void accounts_ItemCheck(object sender, System.Windows.Forms.ItemCheckEventArgs e)
        {
            if (accounts.Focused)
            {
                if (e.CurrentValue == CheckState.Checked)
                {
                    if (!logout(e.Index))
                    {
                        e.NewValue = CheckState.Checked;
                    }
                    else
                    {
                    }
                }
                else
                {
                    if (!login(e.Index))
                    {
                        e.NewValue = CheckState.Unchecked;
                    }
                    else
                    {
                    }
                }
            }
        }
        private void accounts_SelectedIndexChanged(object sender, System.EventArgs e)
        {
            if ((accounts.SelectedItems.Count == 1) &&
                (accounts.Items.Count > 0))
            {
                ListViewItem lvi = accounts.SelectedItems[0];
                if (lvi != null)
                {
                    removeAccount.Enabled = details.Enabled = true;
                }
            }
            else
            {
                removeAccount.Enabled = details.Enabled = false;
            }
        }
        private void details_Click(object sender, System.EventArgs e)
        {
            ListViewItem lvi = accounts.SelectedItems[0];
            Domain domain = (Domain)lvi.Tag;
            if (domain != null)
            {
                ServerDetails serverDetails = new ServerDetails(this.simiasWebService, this.ifWebService, domain);
                if (serverDetails.ShowDialog() == DialogResult.OK)
                {
                    if (serverDetails.EnableChanged || serverDetails.AddressChanged)
                    {
                    }
                    if (serverDetails.DefaultChanged)
                    {
                        if (currentDefaultDomain != null)
                        {
                            currentDefaultDomain.DomainInfo.IsDefault = false;
                        }
                        currentDefaultDomain = domain;
                        if (ChangeDefaultDomain != null)
                        {
                            ChangeDefaultDomain(this, new DomainConnectEventArgs(currentDefaultDomain.DomainInfo));
                        }
                    }
                }
                serverDetails.Dispose();
            }
        }
        private void tabControl1_SelectedIndexChanged(object sender, System.EventArgs e)
        {
            string helpFile;
            switch (tabControl1.SelectedIndex)
            {
                case 0:
                    helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"preferences.html");
                    if (!File.Exists(helpFile))
                    {
                        helpFile = Path.Combine(Application.StartupPath, @"help\en\preferences.html");
                    }
                    if (File.Exists(helpFile))
                    {
                        helpProvider1.HelpNamespace = helpFile;
                    }
                    break;
                case 1:
                    helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"accounts.html");
                    if (!File.Exists(helpFile))
                    {
                        helpFile = Path.Combine(Application.StartupPath, @"help\en\accounts.html");
                    }
                    if (File.Exists(helpFile))
                    {
                        helpProvider1.HelpNamespace = helpFile;
                    }
                    break;
                case 2:
                    break;
            }
        }
        private void Preferences_Move(object sender, System.EventArgs e)
        {
            if (initialPositionSet)
            {
                try
                {
                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);
                    regKey.SetValue(preferencesX, Location.X);
                    regKey.SetValue(preferencesY, Location.Y);
                }
                catch { }
            }
            else
            {
                try
                {
                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);
                    int x = (int)regKey.GetValue(preferencesX);
                    int y = (int)regKey.GetValue(preferencesY);
                    Point point = new Point(x, y);
                    if (SystemInformation.VirtualScreen.Contains(point))
                    {
                        this.Location = point;
                    }
                }
                catch { }
                initialPositionSet = true;
            }
        }
        private const int WM_QUERYENDSESSION = 0x0011;
        protected override void WndProc(ref Message m)
        {
            switch (m.Msg)
            {
                case WM_QUERYENDSESSION:
                    {
                        this.shutdown = true;
                        break;
                    }
            }
            base.WndProc(ref m);
        }
        private void btnMigrate_Click(object sender, EventArgs e)
        {
            ListViewItem lvi = this.listView1.SelectedItems[0];
        }
        private void btnHelp_Click(object sender, System.EventArgs e)
        {
            string helpFile = null;
            switch(this.tabControl1.SelectedIndex)
            {
                case (int)preferenceTab.General:
                    helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"preferences.html");
                    break;
                case (int)preferenceTab.Accounts:
                    helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"accounts.html");
                    break;
                case (int)preferenceTab.Settings:
                    helpFile = Path.Combine(Path.Combine(Path.Combine(Application.StartupPath, "help"), iFolderAdvanced.GetLanguageDirectory()), @"settings.html");
                    break;
                default:
                    break;
            }
            new iFolderComponent().ShowHelp(Application.StartupPath, helpFile);
        }
        private void verifyInterval()
        {
            if ((defaultInterval.Value < minmumSecondsAllowed) && ((string)timeUnit.SelectedItem).Equals(resourceManager.GetString("seconds")))
            {
                defaultInterval.Value = minmumSecondsAllowed;
                MessageBox.Show(str,"Synchronization Interval Limit!", 0, MessageBoxIcon.Information);
            }
        }
        private void policylistbox_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (!displayTrayIcon.Focused)
            {
                apply.Enabled = true;
            }
        }
    }
}
