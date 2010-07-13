

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Net;
using System.Windows.Forms;
using Aga.Controls.Tree;
using ProcessHacker.Components;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.UI;

namespace ProcessHacker.Common
{
    public static class PhUtils
    {
        public static string[] DangerousNames =
        {
            "csrss.exe", "dwm.exe", "logonui.exe", "lsass.exe", "lsm.exe", "services.exe",
            "smss.exe", "wininit.exe", "winlogon.exe"
        };





        public static void AddShortcuts(this ListView lv)
        {
            lv.AddShortcuts(null);
        }






        public static void AddShortcuts(this ListView lv, RetrieveVirtualItemEventHandler retrieveVirtualItem)
        {
            lv.KeyDown +=
                (sender, e) =>
                {
                    if (e.Control && e.KeyCode == Keys.A)
                    {
                        if (retrieveVirtualItem != null)
                        {
                            for (int i = 0; i < lv.VirtualListSize; i++)
                                if (!lv.SelectedIndices.Contains(i))
                                    lv.SelectedIndices.Add(i);
                        }
                        else
                        {
                            lv.Items.SelectAll();
                        }
                    }

                    if (e.Control && e.KeyCode == Keys.C)
                    {
                        GenericViewMenu.ListViewCopy(lv, -1, retrieveVirtualItem);
                    }
                };
        }






        public static bool IsDangerousPid(int pid)
        {
            if (pid == 4)
                return true;

            try
            {
                using (var phandle = new ProcessHandle(pid, OSVersion.MinProcessQueryInfoAccess))
                {
                    foreach (string s in DangerousNames)
                    {
                        if ((Environment.SystemDirectory + "\\" + s).Equals(
                            FileUtils.GetFileName(FileUtils.GetFileName(phandle.GetImageFileName())),
                            StringComparison.InvariantCultureIgnoreCase))
                        {
                            return true;
                        }
                    }
                }
            }
            catch
            { }

            return false;
        }
        private static string FormatException(string operation, Exception ex)
        {
            if (!string.IsNullOrEmpty(operation))
                return operation + ": " + ex.Message;
            else
                return ex.Message;
        }
        public static string FormatPriorityClass(ProcessPriorityClass priorityClass)
        {
            switch (priorityClass)
            {
                case ProcessPriorityClass.AboveNormal:
                    return "Above Normal";
                case ProcessPriorityClass.BelowNormal:
                    return "Below Normal";
                case ProcessPriorityClass.High:
                    return "High";
                case ProcessPriorityClass.Idle:
                    return "Idle";
                case ProcessPriorityClass.Normal:
                    return "Normal";
                case ProcessPriorityClass.RealTime:
                    return "Realtime";
                case ProcessPriorityClass.Unknown:
                default:
                    return "";
            }
        }
        public static Color GetForeColor(Color backColor)
        {
            if (backColor.GetBrightness() > 0.4)
                return Color.Black;
            else
                return Color.White;
        }
        public static string GetIntegrity(this TokenHandle tokenHandle, out int integrityLevel)
        {
            var groups = tokenHandle.GetGroups();
            string integrity = null;
            integrityLevel = 0;
            for (int i = 0; i < groups.Length; i++)
            {
                if ((groups[i].Attributes & SidAttributes.IntegrityEnabled) != 0)
                {
                    integrity = groups[i].GetFullName(false).Replace(" Mandatory Level", "");
                    if (integrity == "Untrusted")
                        integrityLevel = 0;
                    else if (integrity == "Low")
                        integrityLevel = 1;
                    else if (integrity == "Medium")
                        integrityLevel = 2;
                    else if (integrity == "High")
                        integrityLevel = 3;
                    else if (integrity == "System")
                        integrityLevel = 4;
                    else if (integrity == "Installer")
                        integrityLevel = 5;
                }
                groups[i].Dispose();
            }
            return integrity;
        }
        public static bool IsEmpty(this IPEndPoint endPoint)
        {
            return endPoint.Address.GetAddressBytes().IsEmpty() && endPoint.Port == 0;
        }
        public static void IsNetworkError(string url)
        {
            if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
            {
                IntPtr ndfhandle = IntPtr.Zero;
                HResult ndfCreate = Win32.NdfCreateWebIncident(url, ref ndfhandle);
                ndfCreate.ThrowIf();
                Win32.NdfExecuteDiagnosis(ndfhandle, IntPtr.Zero);
                Win32.NdfCloseIncident(ndfhandle);
            }
        }
        public static void IsNetworkError(string url, IntPtr hwnd)
        {
            if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
            {
                IntPtr ndfhandle = IntPtr.Zero;
                HResult ndfCreate = Win32.NdfCreateWebIncident(url, ref ndfhandle);
                ndfCreate.ThrowIf();
                Win32.NdfExecuteDiagnosis(ndfhandle, hwnd);
                Win32.NdfCloseIncident(ndfhandle);
            }
        }
        public static bool IsInternetAddressReachable(string url)
        {
            return Win32.InternetCheckConnection(url, 1, 0);
        }
        public static bool IsInternetConnected
        {
            get
            {
                try
                {
                    System.Net.IPHostEntry entry = System.Net.Dns.GetHostEntry("www.msftncsi.com");
                    return true;
                }
                catch
                { return false; }
            }
        }
        public static bool IsInternetConnectedEx
        {
            get { return Win32.InternetCheckConnection("http://www.msftncsi.com", 1, 0); }
        }
        public static void OpenKeyInRegedit(string keyName)
        {
            OpenKeyInRegedit(null, keyName);
        }
        public static void OpenKeyInRegedit(IWin32Window window, string keyName)
        {
            string lastKey = keyName;
            if (lastKey.ToLowerInvariant().StartsWith("hkcu"))
                lastKey = "HKEY_CURRENT_USER" + lastKey.Substring(4);
            else if (lastKey.ToLowerInvariant().StartsWith("hku"))
                lastKey = "HKEY_USERS" + lastKey.Substring(3);
            else if (lastKey.ToLowerInvariant().StartsWith("hkcr"))
                lastKey = "HKEY_CLASSES_ROOT" + lastKey.Substring(4);
            else if (lastKey.ToLowerInvariant().StartsWith("hklm"))
                lastKey = "HKEY_LOCAL_MACHINE" + lastKey.Substring(4);
            using (var regeditKey =
                Microsoft.Win32.Registry.CurrentUser.CreateSubKey(
                    @"Software\Microsoft\Windows\CurrentVersion\Applets\Regedit",
                    Microsoft.Win32.RegistryKeyPermissionCheck.ReadWriteSubTree
                    ))
            {
                if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
                    regeditKey.SetValue("LastKey", "Computer\\" + lastKey);
                else
                    regeditKey.SetValue("LastKey", lastKey);
            }
            if (OSVersion.HasUac && Program.ElevationType == TokenElevationType.Limited)
            {
                Program.StartProgramAdmin(
                    Environment.SystemDirectory + "\\..\\regedit.exe",
                    "",
                    null,
                    ShowWindowType.Normal,
                    window != null ? window.Handle : IntPtr.Zero
                    );
            }
            else
            {
                System.Diagnostics.Process.Start(Environment.SystemDirectory + "\\..\\regedit.exe");
            }
        }
        public static void SelectAll(this IEnumerable<TreeNodeAdv> nodes)
        {
            foreach (TreeNodeAdv node in nodes)
                node.IsSelected = true;
        }
        private static void SetShieldIconInternal(Button button, bool show)
        {
            Win32.SendMessage(button.Handle,
                WindowMessage.BcmSetShield, 0, show ? 1 : 0);
        }
        public static void SetShieldIcon(this Button button, bool visible)
        {
            SetShieldIconInternal(button, visible);
        }
        public static void SetTheme(this Control control, string theme)
        {
            Win32.SetWindowTheme(control.Handle, theme, null);
        }
        public static bool ShowConfirmMessage(string verb, string obj, string message, bool warning)
        {
            verb = verb.ToLower();
            string verbCaps = char.ToUpper(verb[0]) + verb.Substring(1);
            string action = verb + " " + obj;
            if (OSVersion.HasTaskDialogs)
            {
                TaskDialog td = new TaskDialog();
                td.WindowTitle = "Process Hacker";
                td.MainIcon = warning ? TaskDialogIcon.Warning : TaskDialogIcon.None;
                td.MainInstruction = "Do you want to " + action + "?";
                if (!string.IsNullOrEmpty(message))
                    td.Content = message + " Are you sure you want to continue?";
                td.Buttons = new TaskDialogButton[]
                {
                    new TaskDialogButton((int)DialogResult.Yes, verbCaps),
                    new TaskDialogButton((int)DialogResult.No, "Cancel")
                };
                td.DefaultButton = (int)DialogResult.No;
                return td.Show(Form.ActiveForm) == (int)DialogResult.Yes;
            }
            else
            {
                return MessageBox.Show(
                    message + " Are you sure you want to " + action + "?",
                    "Process Hacker",
                    MessageBoxButtons.YesNo,
                    MessageBoxIcon.Warning
                    ) == DialogResult.Yes;
            }
        }
        public static bool ShowContinueMessage(string operation, Exception ex)
        {
            return MessageBox.Show(
                FormatException(operation, ex),
                "Process Hacker",
                MessageBoxButtons.OKCancel,
                MessageBoxIcon.Error
                ) == DialogResult.OK;
        }
        public static void ShowError(string message)
        {
            MessageBox.Show(Form.ActiveForm, message, "Process Hacker", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
        public static void ShowException(string operation, Exception ex)
        {
            MessageBox.Show(Form.ActiveForm, FormatException(operation, ex), "Process Hacker", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
        public static void ShowInformation(string message)
        {
            MessageBox.Show(Form.ActiveForm, message, "Process Hacker", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }
        public static void ShowWarning(string message)
        {
            MessageBox.Show(Form.ActiveForm, message, "Process Hacker", MessageBoxButtons.OK, MessageBoxIcon.Warning);
        }
    }
}
