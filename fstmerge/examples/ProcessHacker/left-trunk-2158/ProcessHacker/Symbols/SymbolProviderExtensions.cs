

using System;
using System.IO;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.Components;
using ProcessHacker.Native.Objects;

namespace ProcessHacker.Native.Symbols
{
    public static class SymbolProviderExtensions
    {
        public static void ShowWarning(IWin32Window window, bool force)
        {
            if (Properties.Settings.Default.DbgHelpWarningShown && !force)
                return;

            try
            {
                var modules = ProcessHandle.GetCurrent().GetModules();

                foreach (var module in modules)
                {
                    if (module.FileName.ToLowerInvariant().EndsWith("dbghelp.dll"))
                    {
                        if (!File.Exists(Path.GetDirectoryName(module.FileName) + "\\symsrv.dll"))
                        {
                            if (!force)
                                Properties.Settings.Default.DbgHelpWarningShown = true;

                            if (OSVersion.HasTaskDialogs)
                            {
                                TaskDialog td = new TaskDialog();
                                bool verificationChecked;

                                td.CommonButtons = TaskDialogCommonButtons.Ok;
                                td.WindowTitle = "Process Hacker";
                                td.MainIcon = TaskDialogIcon.Warning;
                                td.MainInstruction = "Microsoft Symbol Server not supported";
                                td.Content = "The Microsoft Symbol Server is not supported by your version of dbghelp.dll " +
                                    "or could not be loaded. " +
                                    "To ensure you have the latest version of dbghelp.dll, download " +
                                    "<a href=\"dbghelp\">Debugging " +
                                    "Tools for Windows</a> and configure Process Hacker to " +
                                    "use its version of dbghelp.dll. If you have the latest version of dbghelp.dll, " +
                                    "ensure that symsrv.dll resides in the same directory as dbghelp.dll.";
                                td.EnableHyperlinks = true;
                                td.Callback = (taskDialog, args, callbackData) =>
                                {
                                    if (args.Notification == TaskDialogNotification.HyperlinkClicked)
                                    {
                                        try
                                        {
                                            System.Diagnostics.Process.Start(
                                                "http://www.microsoft.com/whdc/devtools/debugging/default.mspx");
                                        }
                                        catch (Exception ex)
                                        {
                                            MessageBox.Show("Could not open the hyperlink: " + ex.ToString(),
                                                "Process Hacker", MessageBoxButtons.OK, MessageBoxIcon.Error);
                                        }

                                        return true;
                                    }

                                    return false;
                                };
                                td.VerificationText = force ? null : "Do not display this warning again";
                                td.VerificationFlagChecked = true;

                                td.Show(window, out verificationChecked);

                                if (!force)
                                    Properties.Settings.Default.DbgHelpWarningShown = verificationChecked;
                            }
                            else
                            {
                                MessageBox.Show(window, "The Microsoft Symbol Server is not supported by your version of dbghelp.dll " +
                                    "or could not be loaded. To ensure you have the latest version of dbghelp.dll, download " +
                                    "Debugging Tools for Windows and configure Process Hacker to use its version of dbghelp.dll. " +
                                    "If you have the latest version of dbghelp.dll, ensure that symsrv.dll resides in the same " +
                                    "directory as dbghelp.dll.", "Process Hacker", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                            }
                        }

                        break;
                    }
                }
            }
            catch (Exception ex)
            {
                Logging.Log(ex);
            }
        }

        public static void LoadKernelModules(this SymbolProvider symbols)
        {

            symbols.PreloadModules = true;


            foreach (var module in Windows.GetKernelModules())
            {
                try
                {
                    symbols.LoadModule(module.FileName, module.BaseAddress);
                }
                catch (Exception ex)
                {
                    Logging.Log(ex);
                }
            }
        }

        public static void LoadProcessModules(this SymbolProvider symbols, ProcessHandle phandle)
        {
            foreach (var module in phandle.GetModules())
            {
                try
                {
                    symbols.LoadModule(module.FileName, module.BaseAddress, module.Size);
                }
                catch (Exception ex)
                {
                    Logging.Log(ex);
                }
            }
        }

        public static void LoadProcessWow64Modules(this SymbolProvider symbols, int pid)
        {
            using (var buffer = new ProcessHacker.Native.Debugging.DebugBuffer())
            {
                buffer.Query(pid, ProcessHacker.Native.Api.RtlQueryProcessDebugFlags.Modules32);

                foreach (var module in buffer.GetModules())
                {
                    try
                    {
                        symbols.LoadModule(module.FileName, module.BaseAddress, module.Size);
                    }
                    catch (Exception ex)
                    {
                        Logging.Log(ex);
                    }
                }
            }
        }
    }
}
