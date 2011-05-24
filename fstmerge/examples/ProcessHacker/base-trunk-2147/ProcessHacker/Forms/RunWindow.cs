

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker
{
    public partial class RunWindow : Form
    {
        private int _pid = -1;

        public RunWindow()
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            textSessionID.Text = Program.CurrentSessionId.ToString();
            comboType.SelectedItem = "Interactive";

            if (Program.ElevationType == TokenElevationType.Limited)
                buttonOK.SetShieldIcon(true);

            List<string> users = new List<string>();

            users.Add("NT AUTHORITY\\SYSTEM");
            users.Add("NT AUTHORITY\\LOCAL SERVICE");
            users.Add("NT AUTHORITY\\NETWORK SERVICE");

            try
            {
                using (var phandle = new LsaPolicyHandle(LsaPolicyAccess.ViewLocalInformation))
                {
                    foreach (var sid in phandle.GetAccounts())
                        if (sid.NameUse == SidNameUse.User)
                            users.Add(sid.GetFullName(true));
                }
            }
            catch
            { }

            users.Sort();

            comboUsername.Items.AddRange(users.ToArray());
        }

        public void UsePID(int PID)
        {
            _pid = PID;

            try
            {
                comboUsername.Text = Program.ProcessProvider.Dictionary[PID].Username;
            }
            catch
            {
                _pid = -1;
                return;
            }

            comboUsername.Enabled = false;
            comboType.Enabled = false;
            textPassword.Enabled = false;
        }

        private void RunWindow_Load(object sender, EventArgs e)
        {
            if (_pid == -1)
            {
                comboUsername.Text = Properties.Settings.Default.RunAsUsername;
            }

            textCmdLine.Text = Properties.Settings.Default.RunAsCommand;
            textCmdLine.Select();
        }

        private void RunWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            Properties.Settings.Default.RunAsCommand = textCmdLine.Text;
            Properties.Settings.Default.RunAsUsername = comboUsername.Text;
        }

        private void buttonBrowse_Click(object sender, EventArgs e)
        {
            OpenFileDialog ofd = new OpenFileDialog();

            try
            {
                ofd.FileName = textCmdLine.Text;
            }
            catch
            { }

            ofd.CheckFileExists = true;
            ofd.CheckPathExists = true;
            ofd.Multiselect = false;

            if (ofd.ShowDialog() == DialogResult.OK)
                textCmdLine.Text = ofd.FileName;
        }

        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void buttonOK_Click(object sender, EventArgs e)
        {
            this.Cursor = Cursors.WaitCursor;
            Application.DoEvents();

            try
            {
                System.Diagnostics.ProcessStartInfo info = new System.Diagnostics.ProcessStartInfo();

                info.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
                info.FileName = Application.StartupPath + "\\Assistant.exe";
                info.Arguments = "-w";

                System.Diagnostics.Process.Start(info);
            }
            catch
            { }

            try
            {
                string binPath;
                string mailslotName;
                bool omitUserAndType = false;

                if (_pid != -1)
                    omitUserAndType = true;

                mailslotName = "ProcessHackerAssistant" + Utils.CreateRandomString(8);
                binPath = "\"" + Application.StartupPath + "\\Assistant.exe\" " +
                    (omitUserAndType ? "" :
                    ("-u \"" + comboUsername.Text + "\" -t " + comboType.SelectedItem.ToString().ToLower() + " ")) +
                    (_pid != -1 ? ("-P " + _pid.ToString() + " ") : "") + "-p \"" +
                    textPassword.Text.Replace("\"", "\\\"") + "\" -s " + textSessionID.Text + " -c \"" +
                    textCmdLine.Text.Replace("\"", "\\\"") + "\" -E " + mailslotName;

                if (Program.ElevationType == TokenElevationType.Limited)
                {
                    var result = Program.StartProcessHackerAdminWait(
                        "-e -type processhacker -action runas -obj \"" + binPath.Replace("\"", "\\\"") +
                        "\" -mailslot " + mailslotName +
                        " -hwnd " + this.Handle.ToString(), this.Handle, 5000);

                    if (result == WaitResult.Object0)
                        this.Close();
                }
                else
                {
                    string serviceName = Utils.CreateRandomString(8);

                    using (var manager = new ServiceManagerHandle(ScManagerAccess.CreateService))
                    {
                        using (var service = manager.CreateService(
                            serviceName,
                            serviceName + " (Process Hacker Assistant)",
                            ServiceType.Win32OwnProcess,
                            ServiceStartType.DemandStart,
                            ServiceErrorControl.Ignore,
                            binPath,
                            "",
                            "LocalSystem",
                            null))
                        {

                            using (var mhandle = MailslotHandle.Create(
                                FileAccess.GenericRead, @"\Device\Mailslot\" + mailslotName, 0, 5000)
                                )
                            {
                                try { service.Start(); }
                                catch { }
                                service.Delete();

                                Win32Error errorCode = (Win32Error)mhandle.Read(4).ToInt32();

                                if (errorCode != Win32Error.Success)
                                    throw new WindowsException(errorCode);
                            }
                        }
                    }

                    this.Close();
                }
            }
            catch (Exception ex)
            {
                PhUtils.ShowException("Unable to start the program", ex);
            }

            this.Cursor = Cursors.Default;
        }

        private bool isServiceUser()
        {
            if (comboUsername.Text.ToUpper() == "NT AUTHORITY\\SYSTEM" ||
                comboUsername.Text.ToUpper() == "NT AUTHORITY\\LOCAL SERVICE" ||
                comboUsername.Text.ToUpper() == "NT AUTHORITY\\NETWORK SERVICE")
                return true;
            else
                return false;
        }

        private void comboUsername_TextChanged(object sender, EventArgs e)
        {
            if (_pid == -1)
            {
                if (isServiceUser())
                {
                    textPassword.Enabled = false;
                    comboType.SelectedItem = "Service";


                    if (comboUsername.Text.ToUpper() == "NT AUTHORITY\\SYSTEM" &&
                        OSVersion.IsBelowOrEqual(WindowsVersion.XP))
                        comboType.SelectedItem = "NewCredentials";
                }
                else
                {
                    textPassword.Enabled = true;
                    comboType.SelectedItem = "Interactive";
                }
            }
        }

        private void buttonSessions_Click(object sender, EventArgs e)
        {
            ContextMenu menu = new ContextMenu();

            foreach (var session in TerminalServerHandle.GetCurrent().GetSessions())
            {
                MenuItem item = new MenuItem();

                string userName = session.DomainName + "\\" + session.UserName;
                string displayName = session.SessionId.ToString();

                if (!string.IsNullOrEmpty(session.Name))
                    displayName += ": " + session.Name + (userName != "\\" ? (" (" + userName + ")") : "");
                else if (userName != "\\")
                    displayName += ": " + userName;

                item.Text = displayName;
                item.Tag = session.SessionId;
                item.Click += new EventHandler(item_Click);

                menu.MenuItems.Add(item);
            }

            menu.Show(buttonSessions, new Point(buttonSessions.Width, 0));
        }

        private void item_Click(object sender, EventArgs e)
        {
            textSessionID.Text = ((MenuItem)sender).Tag.ToString();
        }
    }
}
