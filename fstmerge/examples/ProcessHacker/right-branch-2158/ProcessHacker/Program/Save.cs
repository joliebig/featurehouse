

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Aga.Controls.Tree;
using Aga.Controls.Tree.NodeControls;
using ProcessHacker.Common;
using ProcessHacker.Native;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker
{
    internal static class Save
    {
        private const int TabSize = 8;

        public static void SaveToFile()
        {
            SaveFileDialog sfd = new SaveFileDialog();


            sfd.Filter = "Text Files (*.txt;*.log)|*.txt;*.log|Comma-separated values (*.csv)|*.csv|All Files (*.*)|*.*";

            if (Program.HackerWindow.SelectedPid == -1)
            {
                sfd.FileName = "Process List.txt";
            }
            else
            {
                string processName = Windows.GetProcessName(Program.HackerWindow.SelectedPid);

                if (processName != null)
                    sfd.FileName = processName + ".txt";
                else
                    sfd.FileName = "Process Info.txt";
            }

            if (sfd.ShowDialog() == DialogResult.OK)
            {
                FileInfo fi = new FileInfo(sfd.FileName);
                string ext = fi.Extension.ToLower();

                try
                {
                    using (StreamWriter sw = new StreamWriter(fi.FullName))
                    {
                        Program.HackerWindow.ProcessTree.Tree.ExpandAll();

                        if (ext == ".htm" || ext == ".html")
                        {

                        }
                        else if (ext == ".csv")
                        {
                            sw.Write(GetProcessTreeText(false));
                        }
                        else
                        {
                            sw.Write(GetEnvironmentInfo());
                            sw.WriteLine();
                            sw.Write(GetProcessTreeText(true));
                            sw.WriteLine();

                            if (Program.HackerWindow.SelectedPid != -1)
                            {
                                sw.Write(GetProcessDetailsText(Program.HackerWindow.SelectedPid));
                                sw.WriteLine();
                            }
                        }
                    }
                }
                catch (IOException ex)
                {
                    PhUtils.ShowException("Unable to save the process list", ex);
                }
            }
        }

        private static string GetEnvironmentInfo()
        {
            StringBuilder sb = new StringBuilder();

            sb.AppendLine("Process Hacker version " + Application.ProductVersion);
            sb.AppendLine(Environment.OSVersion.ToString() + " (" + OSVersion.BitsString + ")");

            return sb.ToString();
        }

        private static string GetProcessTreeText(bool tabs)
        {



            StringBuilder sb = new StringBuilder();

            int items = Program.HackerWindow.ProcessTree.Tree.ItemCount + 1;

            int columns = 0;

            Dictionary<TreeColumn, int> columnIndexMap = new Dictionary<TreeColumn, int>();

            string[][] str = new string[items][];




            foreach (TreeColumn column in Program.HackerWindow.ProcessTree.Tree.Columns)
            {
                if (column.IsVisible && column.Header != "CPU History" && column.Header != "I/O History")
                {
                    columnIndexMap[column] = columns;
                    columns++;
                }
            }




            for (int i = 0; i < items; i++)
                str[i] = new string[columns];


            foreach (var column in Program.HackerWindow.ProcessTree.Tree.Columns)
            {
                if (columnIndexMap.ContainsKey(column))
                    str[0][columnIndexMap[column]] = column.Header;
            }


            {
                int i = 0;


                foreach (var node in Program.HackerWindow.ProcessTree.Tree.AllNodes)
                {


                    foreach (var control in Program.HackerWindow.ProcessTree.Tree.NodeControls)
                    {

                        if (!control.ParentColumn.IsVisible || !(control is BaseTextControl))
                            continue;


                        string text = (control as BaseTextControl).GetLabel(node);

                        int columnIndex = columnIndexMap[control.ParentColumn];


                        str[i + 1][columnIndex] =

                            (columnIndex == 0 ? (new string(' ', (node.Level - 1) * 2)) : "") +
                            (text != null ? text : "");
                    }

                    i++;
                }
            }




            int[] tabCount = new int[columns];

            for (int i = 0; i < items; i++)
            {
                for (int j = 0; j < columns; j++)
                {
                    int newCount = str[i][j].Length / TabSize;


                    if (newCount > tabCount[j])
                        tabCount[j] = newCount;
                }
            }




            for (int i = 0; i < items; i++)
            {
                for (int j = 0; j < columns; j++)
                {
                    if (tabs)
                    {

                        sb.Append(str[i][j]);

                        sb.Append('\t', tabCount[j] - str[i][j].Length / TabSize + 1);
                    }
                    else
                    {

                        sb.Append("\"");
                        sb.Append(str[i][j].Replace("\"", "\\\""));
                        sb.Append("\"");


                        if (j != columns - 1)
                            sb.Append(",");
                    }
                }

                sb.AppendLine();
            }

            return sb.ToString();
        }

        private static string GetProcessDetailsText(int pid)
        {



            StringBuilder sb = new StringBuilder();

            sb.AppendLine("Process PID " + pid.ToString() + ":");
            sb.AppendLine();

            try
            {
                using (var phandle = new ProcessHandle(pid, ProcessAccess.QueryLimitedInformation))
                {
                    var fileName = phandle.GetImageFileName();

                    sb.AppendLine("Native file name: " + fileName);
                    fileName = FileUtils.GetFileName(fileName);
                    sb.AppendLine("DOS file name: " + fileName);

                    try
                    {
                        var fileInfo = FileVersionInfo.GetVersionInfo(fileName);

                        sb.AppendLine("Description: " + fileInfo.FileDescription);
                        sb.AppendLine("Company: " + fileInfo.CompanyName);
                        sb.AppendLine("Version: " + fileInfo.FileVersion);
                    }
                    catch (Exception ex2)
                    {
                        sb.AppendLine("Version info section failed! " + ex2.Message);
                    }

                    sb.AppendLine("Started: " + phandle.GetCreateTime().ToString());

                    var memoryInfo = phandle.GetMemoryStatistics();

                    sb.AppendLine("WS: " + Utils.FormatSize(memoryInfo.WorkingSetSize));
                    sb.AppendLine("Pagefile usage: " + Utils.FormatSize(memoryInfo.PagefileUsage));
                }
            }
            catch (Exception ex)
            {
                sb.AppendLine("Basic info section failed! " + ex.Message);
            }

            try
            {
                using (var phandle = new ProcessHandle(pid, ProcessAccess.QueryLimitedInformation | ProcessAccess.VmRead))
                {
                    var commandLine = phandle.GetCommandLine();
                    var currentDirectory = phandle.GetPebString(PebOffset.CurrentDirectoryPath);

                    sb.AppendLine("Command line: " + commandLine);
                    sb.AppendLine("Current directory: " + currentDirectory);
                }
            }
            catch (Exception ex)
            {
                sb.AppendLine("PEB info section failed! " + ex.Message);
            }

            sb.AppendLine();
            sb.AppendLine("Modules:");
            sb.AppendLine();

            try
            {
                using (var phandle = new ProcessHandle(pid, ProcessAccess.QueryLimitedInformation | ProcessAccess.VmRead))
                {
                    foreach (var module in phandle.GetModules())
                    {
                        sb.AppendLine(module.FileName);
                        sb.Append("    [0x" + module.BaseAddress.ToInt32().ToString("x") + ", ");
                        sb.AppendLine(Utils.FormatSize(module.Size) + "] ");
                        sb.AppendLine("    Flags: " + module.Flags.ToString());

                        try
                        {
                            var fileInfo = FileVersionInfo.GetVersionInfo(module.FileName);

                            sb.AppendLine("    Description: " + fileInfo.FileDescription);
                            sb.AppendLine("    Company: " + fileInfo.CompanyName);
                            sb.AppendLine("    Version: " + fileInfo.FileVersion);
                        }
                        catch (Exception ex2)
                        {
                            sb.AppendLine("    Version info failed! " + ex2.Message);
                        }

                        sb.AppendLine();
                    }
                }
            }
            catch (Exception ex)
            {
                sb.AppendLine("Modules section failed! " + ex.Message);
            }

            sb.AppendLine("Token:");
            sb.AppendLine();

            try
            {
                using (var phandle = new ProcessHandle(pid, ProcessAccess.QueryLimitedInformation))
                using (var thandle = phandle.GetToken(TokenAccess.Query))
                {
                    sb.AppendLine("User: " + thandle.GetUser().GetFullName(true));
                    sb.AppendLine("Owner: " + thandle.GetOwner().GetFullName(true));
                    sb.AppendLine("Primary group: " + thandle.GetPrimaryGroup().GetFullName(true));

                    foreach (var group in thandle.GetGroups())
                    {
                        sb.AppendLine("Group " + group.GetFullName(true));
                    }

                    foreach (var privilege in thandle.GetPrivileges())
                    {
                        sb.AppendLine("Privilege " + privilege.Name + ": " + privilege.Attributes.ToString());
                    }
                }
            }
            catch (Exception ex)
            {
                sb.AppendLine("Token section failed! " + ex.Message);
            }

            sb.AppendLine();
            sb.AppendLine("Environment:");
            sb.AppendLine();

            try
            {
                using (var phandle = new ProcessHandle(pid, ProcessAccess.QueryInformation | ProcessAccess.VmRead))
                {
                    var vars = phandle.GetEnvironmentVariables();

                    foreach (var kvp in vars)
                    {
                        sb.AppendLine(kvp.Key + " = " + kvp.Value);
                    }
                }
            }
            catch (Exception ex)
            {
                sb.AppendLine("Environment section failed! " + ex.Message);
            }

            sb.AppendLine();
            sb.AppendLine("Handles:");
            sb.AppendLine();

            try
            {
                using (var phandle = new ProcessHandle(pid, ProcessAccess.DupHandle))
                {
                    var handles = Windows.GetHandles();

                    foreach (var handle in handles)
                    {
                        if (handle.ProcessId != pid)
                            continue;

                        sb.Append("[0x" + handle.Handle.ToString("x") + ", ");

                        try
                        {
                            var info = handle.GetHandleInfo(phandle);

                            sb.Append(info.TypeName + "] ");
                            sb.AppendLine(!string.IsNullOrEmpty(info.BestName) ? info.BestName : "(no name)");
                        }
                        catch (Exception ex2)
                        {
                            sb.Append(handle.ObjectTypeNumber.ToString() + "] ");
                            sb.AppendLine("Error: " + ex2.Message);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                sb.AppendLine("Handles section failed! " + ex.Message);
            }

            return sb.ToString();
        }
    }
}
