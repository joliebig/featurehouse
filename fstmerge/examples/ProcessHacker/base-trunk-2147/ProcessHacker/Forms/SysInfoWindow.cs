

using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using ProcessHacker.Common;
using ProcessHacker.Components;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Symbols;

namespace ProcessHacker
{
    public partial class SysInfoWindow : Form
    {
        private static IntPtr _mmSizeOfPagedPoolInBytes;
        private static IntPtr _mmMaximumNonPagedPoolInBytes;

        private bool _isFirstPaint = true;
        private Components.Plotter[] _cpuPlotters;
        private uint _noOfCPUs = Program.ProcessProvider.System.NumberOfProcessors;
        private uint _pages = (uint)Program.ProcessProvider.System.NumberOfPhysicalPages;
        private uint _pageSize = (uint)Program.ProcessProvider.System.PageSize;

        public SysInfoWindow()
        {
            InitializeComponent();
            this.AddEscapeToClose();

            this.Size = Properties.Settings.Default.SysInfoWindowSize;
            this.Location = Utils.FitRectangle(new Rectangle(
                Properties.Settings.Default.SysInfoWindowLocation, this.Size), this).Location;


            if (
                _mmSizeOfPagedPoolInBytes == IntPtr.Zero &&
                KProcessHacker.Instance != null
                )
            {
                WorkQueue.GlobalQueueWorkItemTag(new Action(() =>
                    {
                        try
                        {
                            SymbolProvider symbols = new SymbolProvider();

                            symbols.LoadModule(Windows.KernelFileName, Windows.KernelBase);
                            _mmSizeOfPagedPoolInBytes =
                                symbols.GetSymbolFromName("MmSizeOfPagedPoolInBytes").Address.ToIntPtr();
                            _mmMaximumNonPagedPoolInBytes =
                                symbols.GetSymbolFromName("MmMaximumNonPagedPoolInBytes").Address.ToIntPtr();
                        }
                        catch
                        { }
                    }), "load-mm-addresses");
            }
        }

        private void SysInfoWindow_Paint(object sender, PaintEventArgs e)
        {
            if (_isFirstPaint)
            {
                this.LoadStage1();
            }

            _isFirstPaint = false;
        }

        private void LoadStage1()
        {

            indicatorPhysical.Maximum = (int)_pages;


            indicatorCpu.Color1 = Properties.Settings.Default.PlotterCPUKernelColor;
            indicatorCpu.Color2 = Properties.Settings.Default.PlotterCPUUserColor;
            indicatorIO.Color1 = Properties.Settings.Default.PlotterIOROColor;
            indicatorPhysical.Color1 = Properties.Settings.Default.PlotterMemoryWSColor;



            plotterCPU.Data1 = Program.ProcessProvider.FloatHistory["Kernel"];
            plotterCPU.Data2 = Program.ProcessProvider.FloatHistory["User"];
            plotterCPU.GetToolTip = i =>
                Program.ProcessProvider.MostCpuHistory[i] + "\n" +
                ((plotterCPU.Data1[i] + plotterCPU.Data2[i]) * 100).ToString("N2") +
                "% (K " + (plotterCPU.Data1[i] * 100).ToString("N2") +
                "%, U " + (plotterCPU.Data2[i] * 100).ToString("N2") + "%)" + "\n" +
                Program.ProcessProvider.TimeHistory[i].ToString();
            plotterIO.LongData1 = Program.ProcessProvider.LongHistory[SystemStats.IoReadOther];
            plotterIO.LongData2 = Program.ProcessProvider.LongHistory[SystemStats.IoWrite];
            plotterIO.GetToolTip = i =>
                Program.ProcessProvider.MostIoHistory[i] + "\n" +
                "R+O: " + Utils.FormatSize(plotterIO.LongData1[i]) + "\n" +
                "W: " + Utils.FormatSize(plotterIO.LongData2[i]) + "\n" +
                Program.ProcessProvider.TimeHistory[i].ToString();
            plotterMemory.LongData1 = Program.ProcessProvider.LongHistory[SystemStats.Commit];
            plotterMemory.LongData2 = Program.ProcessProvider.LongHistory[SystemStats.PhysicalMemory];
            plotterMemory.GetToolTip = i =>
                "Commit: " + Utils.FormatSize(plotterMemory.LongData1[i]) + "\n" +
                "Phys. Memory: " + Utils.FormatSize(plotterMemory.LongData2[i]) + "\n" +
                Program.ProcessProvider.TimeHistory[i].ToString();


            _cpuPlotters = new Plotter[_noOfCPUs];
            tableCPUs.ColumnCount = (int)_noOfCPUs;
            tableCPUs.ColumnStyles.Clear();
            tableCPUs.Dock = DockStyle.Fill;

            for (int i = 0; i < _cpuPlotters.Length; i++)
            {
                Plotter plotter;

                tableCPUs.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 1.0f / _noOfCPUs));
                _cpuPlotters[i] = plotter = new ProcessHacker.Components.Plotter();
                plotter.BackColor = Color.Black;
                plotter.Dock = DockStyle.Fill;
                plotter.Margin = new Padding(i == 0 ? 0 : 3, 0, 0, 0);
                plotter.UseSecondLine = true;
                plotter.Data1 = Program.ProcessProvider.FloatHistory[i.ToString() + " Kernel"];
                plotter.Data2 = Program.ProcessProvider.FloatHistory[i.ToString() + " User"];
                plotter.GetToolTip = j =>
                    Program.ProcessProvider.MostCpuHistory[j] + "\n" +
                    ((plotter.Data1[j] + plotter.Data2[j]) * 100).ToString("N2") +
                    "% (K " + (plotter.Data1[j] * 100).ToString("N2") +
                    "%, U " + (plotter.Data2[j] * 100).ToString("N2") + "%)" + "\n" +
                    Program.ProcessProvider.TimeHistory[j].ToString();
                tableCPUs.Controls.Add(plotter, i, 0);
            }

            tableCPUs.Visible = true;
            tableCPUs.Visible = false;
            checkShowOneGraphPerCPU.Checked = Properties.Settings.Default.ShowOneGraphPerCPU;

            if (_noOfCPUs == 1)
                checkShowOneGraphPerCPU.Enabled = false;

            this.UpdateGraphs();
            this.UpdateInfo();

            Program.ProcessProvider.Updated +=
                new ProcessSystemProvider.ProviderUpdateOnce(ProcessProvider_Updated);



            this.SetTopMost();
        }

        private void SysInfoWindow_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (this.WindowState == FormWindowState.Normal)
            {
                Properties.Settings.Default.SysInfoWindowLocation = this.Location;
                Properties.Settings.Default.SysInfoWindowSize = this.Size;
            }

            Program.ProcessProvider.Updated -=
                new ProcessSystemProvider.ProviderUpdateOnce(ProcessProvider_Updated);
            Properties.Settings.Default.ShowOneGraphPerCPU = checkShowOneGraphPerCPU.Checked;
        }

        private void UpdateGraphs()
        {

            indicatorCpu.Data1 = (int)(Program.ProcessProvider.CurrentCpuKernelUsage * indicatorCpu.Maximum);
            indicatorCpu.Data2 = (int)(Program.ProcessProvider.CurrentCpuUserUsage * indicatorCpu.Maximum);
            indicatorCpu.TextValue = (Program.ProcessProvider.CurrentCpuUsage * 100).ToString("F2") + "%";


            int count = plotterIO.Width / plotterIO.EffectiveMoveStep;
            long maxRO = Program.ProcessProvider.LongHistory[SystemStats.IoReadOther].Take(count).Max();
            long maxW = Program.ProcessProvider.LongHistory[SystemStats.IoWrite].Take(count).Max();
            if(maxRO>maxW)
                indicatorIO.Maximum = maxRO;
            else
                indicatorIO.Maximum = maxW;
            indicatorIO.Data1 = Program.ProcessProvider.LongHistory[SystemStats.IoReadOther][0];
            indicatorIO.TextValue = Utils.FormatSize(Program.ProcessProvider.LongHistory[SystemStats.IoReadOther][0]);


            plotterIO.LongData1 = Program.ProcessProvider.LongHistory[SystemStats.IoReadOther];
            plotterIO.LongData2 = Program.ProcessProvider.LongHistory[SystemStats.IoWrite];

            plotterCPU.LineColor1 = Properties.Settings.Default.PlotterCPUKernelColor;
            plotterCPU.LineColor2 = Properties.Settings.Default.PlotterCPUUserColor;
            plotterIO.LineColor1 = Properties.Settings.Default.PlotterIOROColor;
            plotterIO.LineColor2 = Properties.Settings.Default.PlotterIOWColor;
            plotterMemory.LineColor1 = Properties.Settings.Default.PlotterMemoryPrivateColor;
            plotterMemory.LineColor2 = Properties.Settings.Default.PlotterMemoryWSColor;

            for (int i = 0; i < _cpuPlotters.Length; i++)
            {
                _cpuPlotters[i].LineColor1 = Properties.Settings.Default.PlotterCPUKernelColor;
                _cpuPlotters[i].LineColor2 = Properties.Settings.Default.PlotterCPUUserColor;
                _cpuPlotters[i].Text = ((_cpuPlotters[i].Data1[0] + _cpuPlotters[i].Data2[0]) * 100).ToString("F2") +
                    "% (K: " + (_cpuPlotters[i].Data1[0] * 100).ToString("F2") +
                    "%, U: " + (_cpuPlotters[i].Data2[0] * 100).ToString("F2") + "%)";
                _cpuPlotters[i].MoveGrid();
                _cpuPlotters[i].Draw();
            }

            plotterCPU.Text = ((plotterCPU.Data1[0] + plotterCPU.Data2[0]) * 100).ToString("F2") +
                "% (K: " + (plotterCPU.Data1[0] * 100).ToString("F2") +
                "%, U: " + (plotterCPU.Data2[0] * 100).ToString("F2") + "%)";


            plotterIO.Text = "R+O: " + Utils.FormatSize(plotterIO.LongData1[0]) +
                ", W: " + Utils.FormatSize(plotterIO.LongData2[0]);


            plotterMemory.Text = "Commit: " + Utils.FormatSize(plotterMemory.LongData1[0]) +
                ", Phys. Mem: " + Utils.FormatSize(plotterMemory.LongData2[0]);

            plotterCPU.MoveGrid();
            plotterCPU.Draw();
            plotterIO.MoveGrid();
            plotterIO.Draw();
            plotterMemory.MoveGrid();
            plotterMemory.Draw();
        }

        private unsafe void GetPoolLimits(out int paged, out int nonPaged)
        {
            int pagedLocal, nonPagedLocal;
            int retLength;


            KProcessHacker.Instance.KphReadVirtualMemoryUnsafe(
                ProcessHacker.Native.Objects.ProcessHandle.GetCurrent(),
                _mmSizeOfPagedPoolInBytes.ToInt32(),
                &pagedLocal,
                sizeof(int),
                out retLength
                );
            KProcessHacker.Instance.KphReadVirtualMemoryUnsafe(
                ProcessHacker.Native.Objects.ProcessHandle.GetCurrent(),
                _mmMaximumNonPagedPoolInBytes.ToInt32(),
                &nonPagedLocal,
                sizeof(int),
                out retLength
                );

            paged = pagedLocal;
            nonPaged = nonPagedLocal;
        }

        private void UpdateInfo()
        {
            var perfInfo = Program.ProcessProvider.Performance;
            var info = new PerformanceInformation();

            Win32.GetPerformanceInfo(out info, System.Runtime.InteropServices.Marshal.SizeOf(info));

            SystemCacheInformation cacheInfo;
            int retLen;

            Win32.NtQuerySystemInformation(SystemInformationClass.SystemFileCacheInformation,
                out cacheInfo, Marshal.SizeOf(typeof(SystemCacheInformation)), out retLen);


            labelTotalsProcesses.Text = ((ulong)info.ProcessCount).ToString("N0");
            labelTotalsThreads.Text = ((ulong)info.ThreadCount).ToString("N0");
            labelTotalsHandles.Text = ((ulong)info.HandlesCount).ToString("N0");
            labelTotalsUptime.Text = Utils.FormatLongTimeSpan(Windows.GetUptime());


            labelCCC.Text = Utils.FormatSize((ulong)perfInfo.CommittedPages * _pageSize);
            labelCCP.Text = Utils.FormatSize((ulong)perfInfo.PeakCommitment * _pageSize);
            labelCCL.Text = Utils.FormatSize((ulong)perfInfo.CommitLimit * _pageSize);


            string physMemText = Utils.FormatSize((ulong)(_pages - perfInfo.AvailablePages) * _pageSize);

            labelPMC.Text = physMemText;
            labelPSC.Text = Utils.FormatSize((ulong)info.SystemCache * _pageSize);
            labelPMT.Text = Utils.FormatSize((ulong)_pages * _pageSize);



            indicatorPhysical.Data1 = _pages - perfInfo.AvailablePages;
            indicatorPhysical.TextValue = physMemText;


            labelCacheCurrent.Text = Utils.FormatSize(cacheInfo.SystemCacheWsSize);
            labelCachePeak.Text = Utils.FormatSize(cacheInfo.SystemCacheWsPeakSize);
            labelCacheMinimum.Text = Utils.FormatSize((ulong)cacheInfo.SystemCacheWsMinimum * _pageSize);
            labelCacheMaximum.Text = Utils.FormatSize((ulong)cacheInfo.SystemCacheWsMaximum * _pageSize);


            labelKPPPU.Text = Utils.FormatSize((ulong)perfInfo.ResidentPagedPoolPage * _pageSize);
            labelKPPVU.Text = Utils.FormatSize((ulong)perfInfo.PagedPoolPages * _pageSize);
            labelKPPA.Text = ((ulong)perfInfo.PagedPoolAllocs).ToString("N0");
            labelKPPF.Text = ((ulong)perfInfo.PagedPoolFrees).ToString("N0");
            labelKPNPU.Text = Utils.FormatSize((ulong)perfInfo.NonPagedPoolPages * _pageSize);
            labelKPNPA.Text = ((ulong)perfInfo.NonPagedPoolAllocs).ToString("N0");
            labelKPNPF.Text = ((ulong)perfInfo.NonPagedPoolFrees).ToString("N0");


            long pagedLimit = 0;
            long nonPagedLimit = 0;

            if (
                _mmSizeOfPagedPoolInBytes != IntPtr.Zero &&
                _mmMaximumNonPagedPoolInBytes != IntPtr.Zero &&
                KProcessHacker.Instance != null
                )
            {
                try
                {
                    int pl, npl;

                    this.GetPoolLimits(out pl, out npl);
                    pagedLimit = pl;
                    nonPagedLimit = npl;
                }
                catch
                { }
            }

            if (pagedLimit != 0)
                labelKPPL.Text = Utils.FormatSize(pagedLimit);
            else if (KProcessHacker.Instance == null)
                labelKPPL.Text = "no driver";
            else
                labelKPPL.Text = "no symbols";

            if (nonPagedLimit != 0)
                labelKPNPL.Text = Utils.FormatSize(nonPagedLimit);
            else if (KProcessHacker.Instance == null)
                labelKPNPL.Text = "no driver";
            else
                labelKPNPL.Text = "no symbols";


            labelPFTotal.Text = ((ulong)perfInfo.PageFaultCount).ToString("N0");
            labelPFCOW.Text = ((ulong)perfInfo.CopyOnWriteCount).ToString("N0");
            labelPFTrans.Text = ((ulong)perfInfo.TransitionCount).ToString("N0");
            labelPFCacheTrans.Text = ((ulong)perfInfo.CacheTransitionCount).ToString("N0");
            labelPFDZ.Text = ((ulong)perfInfo.CacheTransitionCount).ToString("N0");
            labelPFCache.Text = ((ulong)cacheInfo.SystemCacheWsFaults).ToString("N0");


            labelIOR.Text = ((ulong)perfInfo.IoReadOperationCount).ToString("N0");
            labelIORB.Text = Utils.FormatSize(perfInfo.IoReadTransferCount);
            labelIOW.Text = ((ulong)perfInfo.IoWriteOperationCount).ToString("N0");
            labelIOWB.Text = Utils.FormatSize(perfInfo.IoWriteTransferCount);
            labelIOO.Text = ((ulong)perfInfo.IoOtherOperationCount).ToString("N0");
            labelIOOB.Text = Utils.FormatSize(perfInfo.IoOtherTransferCount);


            labelCPUContextSwitches.Text = ((ulong)perfInfo.ContextSwitches).ToString("N0");
            labelCPUInterrupts.Text = ((ulong)Program.ProcessProvider.ProcessorPerf.InterruptCount).ToString("N0");
            labelCPUSystemCalls.Text = ((ulong)perfInfo.SystemCalls).ToString("N0");
        }

        private void ProcessProvider_Updated()
        {
            this.BeginInvoke(new MethodInvoker(delegate
            {
                this.UpdateGraphs();
                this.UpdateInfo();
            }));
        }

        private void checkShowOneGraphPerCPU_CheckedChanged(object sender, EventArgs e)
        {
            if (checkShowOneGraphPerCPU.Checked)
            {
                tableCPUs.Visible = true;
            }
            else
            {
                tableCPUs.Visible = false;
            }
        }

        private void checkAlwaysOnTop_CheckedChanged(object sender, EventArgs e)
        {
            this.TopMost = checkAlwaysOnTop.Checked;
        }
    }
}
