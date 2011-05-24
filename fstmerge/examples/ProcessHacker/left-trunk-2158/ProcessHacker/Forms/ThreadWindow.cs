

using System;
using System.Reflection;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.Native;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;
using ProcessHacker.Native.Symbols;
using ProcessHacker.UI;

namespace ProcessHacker
{
    public partial class ThreadWindow : Form
    {
        private int _pid;
        private int _tid;
        private ProcessHandle _phandle;
        private bool _processHandleOwned = true;
        private ThreadHandle _thandle;
        private SymbolProvider _symbols;

        public const string DisplayFormat = "0x{0:x}";

        public string Id
        {
            get { return _pid + "-" + _tid; }
        }

        public ThreadWindow(int PID, int TID, SymbolProvider symbols, ProcessHandle processHandle)
        {
            InitializeComponent();
            this.AddEscapeToClose();
            this.SetTopMost();

            listViewCallStack_SelectedIndexChanged(null, null);

            _pid = PID;
            _tid = TID;
            _symbols = symbols;

            this.Text = Program.ProcessProvider.Dictionary[_pid].Name + " (PID " + _pid.ToString() +
                ") - Thread " + _tid.ToString();

            PropertyInfo property = typeof(ListView).GetProperty("DoubleBuffered",
                BindingFlags.NonPublic | BindingFlags.Instance);

            property.SetValue(listViewCallStack, true, null);

            listViewCallStack.ContextMenu = listViewCallStack.GetCopyMenu();

            try
            {
                if (processHandle != null)
                {
                    _phandle = processHandle;
                    _processHandleOwned = false;
                }
                else
                {
                    try
                    {
                        _phandle = new ProcessHandle(_pid,
                            ProcessAccess.QueryInformation | ProcessAccess.VmRead
                            );
                    }
                    catch
                    {
                        if (KProcessHacker.Instance != null)
                        {
                            _phandle = new ProcessHandle(_pid, Program.MinProcessReadMemoryRights);
                        }
                        else
                        {
                            throw;
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                PhUtils.ShowException("Unable to open the process", ex);

                this.Close();

                return;
            }

            try
            {
                try
                {
                    _thandle = new ThreadHandle(_tid, ThreadAccess.GetContext | ThreadAccess.SuspendResume);
                }
                catch
                {
                    if (KProcessHacker.Instance != null)
                    {
                        _thandle = new ThreadHandle(_tid,
                            Program.MinThreadQueryRights | ThreadAccess.SuspendResume
                            );
                    }
                    else
                    {
                        throw;
                    }
                }
            }
            catch (Exception ex)
            {
                PhUtils.ShowException("Unable to open the thread", ex);

                this.Close();

                return;
            }
        }

        private void ThreadWindow_Load(object sender, EventArgs e)
        {
            listViewCallStack.SetTheme("explorer");
            listViewCallStack.AddShortcuts();

            this.Size = Properties.Settings.Default.ThreadWindowSize;
            ColumnSettings.LoadSettings(Properties.Settings.Default.CallStackColumns, listViewCallStack);

            this.WalkCallStack();
        }

        private void ThreadWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            Properties.Settings.Default.ThreadWindowSize = this.Size;
            Properties.Settings.Default.CallStackColumns = ColumnSettings.SaveSettings(listViewCallStack);
            _symbols = null;
        }

        private void WalkCallStack()
        {
            try
            {

                listViewCallStack.BeginUpdate();
                listViewCallStack.Items.Clear();

                bool suspended;

                try
                {
                    _thandle.Suspend();
                    suspended = true;
                }
                catch
                {
                    suspended = false;
                }

                try
                {

                    if (KProcessHacker.Instance != null)
                    {
                        this.WalkKernelStack();
                    }





                    _thandle.WalkStack(_phandle, this.WalkStackCallback);

                    if (IntPtr.Size == 8 && _phandle.IsWow64())
                    {
                        _thandle.WalkStack(_phandle, this.WalkStackCallback, OSArch.I386);
                    }
                }
                finally
                {
                    if (suspended)
                        _thandle.Resume();
                }
            }
            catch (Exception ex)
            {
                Logging.Log(ex);
            }
            finally
            {
                listViewCallStack.EndUpdate();
            }
        }

        private void WalkKernelStack()
        {
            try
            {
                IntPtr[] frames = _thandle.CaptureKernelStack(1);

                foreach (IntPtr frame in frames)
                {
                    ulong address = frame.ToUInt64();

                    try
                    {
                        ListViewItem newItem = listViewCallStack.Items.Add(new ListViewItem(
                            new string[]
                                {
                                    Utils.FormatAddress(address),
                                    _symbols.GetSymbolFromAddress(address)
                                }));

                        newItem.Tag = address;
                    }
                    catch (Exception ex2)
                    {
                        Logging.Log(ex2);
                    }
                }
            }
            catch (Exception ex)
            {
                Logging.Log(ex);
            }
        }

        private bool WalkStackCallback(ThreadStackFrame stackFrame)
        {
            ulong address = stackFrame.PcAddress.ToUInt64();


            if (_pid == 4)
            {
                if (OSVersion.WindowsVersion == WindowsVersion.XP && address == 0xffffffff)
                    return true;
            }

            try
            {
                ListViewItem newItem = listViewCallStack.Items.Add(new ListViewItem(
                    new string[]
                    {
                        Utils.FormatAddress(address),
                        _symbols.GetSymbolFromAddress(address)
                    }));

                newItem.Tag = address;

                try
                {
                    if (stackFrame.Params.Length > 0)
                        newItem.ToolTipText = "Parameters: ";

                    foreach (IntPtr arg in stackFrame.Params)
                        newItem.ToolTipText += Utils.FormatAddress(arg) + ", ";

                    if (newItem.ToolTipText.EndsWith(", "))
                        newItem.ToolTipText = newItem.ToolTipText.Remove(newItem.ToolTipText.Length - 2);

                    try
                    {
                        string fileAndLine = _symbols.GetLineFromAddress(address);

                        if (fileAndLine != null)
                            newItem.ToolTipText += "\nFile: " + fileAndLine;
                    }
                    catch
                    { }
                }
                catch (Exception ex2)
                {
                    Logging.Log(ex2);
                }
            }
            catch (Exception ex)
            {
                Logging.Log(ex);

                ListViewItem newItem = listViewCallStack.Items.Add(new ListViewItem(new string[] {
                            Utils.FormatAddress(address),
                            "???"
                        }));

                newItem.Tag = address;
            }

            return true;
        }

        private void buttonWalk_Click(object sender, EventArgs e)
        {
            this.WalkCallStack();
        }

        private void listViewCallStack_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (listViewCallStack.SelectedItems.Count == 1)
            {
                string fileName;

                _symbols.GetSymbolFromAddress((ulong)listViewCallStack.SelectedItems[0].Tag, out fileName);
                fileModule.Text = fileName;
                fileModule.Enabled = true;
            }
            else
            {
                fileModule.Text = "";
                fileModule.Enabled = false;
            }
        }
    }
}
