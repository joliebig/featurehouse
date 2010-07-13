using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using ProcessHacker.Native.Api;
using System.Diagnostics;
using System.Timers;
using System.Collections;
using System.Net.NetworkInformation;
using ProcessHacker.Common;

namespace ProcessHacker
{
    public partial class NetInfoWindow : Form
    {
        public NetInfoWindow()
        {
            InitializeComponent();
        }

        public HistoryManager<string, float> FloatHistory { get { return _floatHistory; } }

        private HistoryManager<string, float> _floatHistory = new HistoryManager<string, float>();

        private void NetInfoWindow_Load(object sender, EventArgs e)
        {
            _floatHistory.Add("up");
            _floatHistory.Add("down");
            plotter1.Data1 = FloatHistory["up"];
            plotter1.Data2 = FloatHistory["down"];

            this._monitor = new NetworkMonitor();
            this._monitor.StopMonitoring();
            this._monitor.StartMonitoring();
        }

        private ProcessHacker.NetworkMonitor _monitor;

        private void getStats()
        {
            MibTcpStats mtcp = Win32.GetTcpStats();
            label1.Text = String.Format("ActiveOpens: {0}", mtcp.ActiveOpens);
            label2.Text = String.Format("AttemptFails: {0}", mtcp.AttemptFails);
            label3.Text = String.Format("CurrEstab: {0}", mtcp.CurrEstab);
            label4.Text = String.Format("EstabResets: {0}", mtcp.EstabResets);
            label5.Text = String.Format("InErrs: {0}", mtcp.InErrs);
            label6.Text = String.Format("InSegs: {0}", mtcp.InSegs);
            label7.Text = String.Format("MaxConn: {0}", mtcp.MaxConn);
            label8.Text = String.Format("NumConns: {0}", mtcp.NumConns);
            label9.Text = String.Format("OutRsts: {0}", mtcp.OutRsts);
            label10.Text = String.Format("OutSegs: {0}", mtcp.OutSegs);
            label11.Text = String.Format("PassiveOpens: {0}", mtcp.PassiveOpens);
            label12.Text = String.Format("RetransSegs: {0}", mtcp.RetransSegs);
            label13.Text = String.Format("RtoAlgorithm: {0}", mtcp.RtoAlgorithm);
            label14.Text = String.Format("RtoMax: {0}", mtcp.RtoMax);
            label15.Text = String.Format("RtoMin: {0}", mtcp.RtoMin);

            MibUdpStats mudp = Win32.GetUdpStats();
            label16.Text = String.Format("InDatagrams: {0}", mudp.InDatagrams);
            label17.Text = String.Format("InErrors: {0}", mudp.InErrors);
            label18.Text = String.Format("NoPorts: {0}", mudp.NoPorts);
            label19.Text = String.Format("NumAddrs: {0}", mudp.NumAddrs);
            label20.Text = String.Format("OutDatagrams: {0}", mudp.OutDatagrams);
        }

        private void timer1_Tick(object sender, EventArgs e)
        {
            getStats();

            foreach (NetworkAdapter i in _monitor.Adapters)
            {
                try
                {
                    int down = unchecked((int)Convert.ToInt32(Math.Round(i.DownloadSpeedKbps, 0)));
                    int up = unchecked((int)Convert.ToInt32(Math.Round(i.DownloadSpeedKbps, 0)));

                    this.label25.Text = up.ToString();
                    this.label26.Text = down.ToString();

                    plotter1.Data1 = FloatHistory["up"];
                    plotter1.Data2 = FloatHistory["down"];
                    _floatHistory.Update("up", (float)i.UploadSpeedKbps);
                    _floatHistory.Update("down", (float)i.DownloadSpeedKbps);
                    plotter1.MoveGrid();
                    plotter1.Draw();

                    this.label21.Text = String.Format("U: {0:n}kbps", i.UploadSpeedKbps);
                    this.label22.Text = String.Format("D: {0:n}kbps", i.DownloadSpeedKbps);

                    NetworkInformation nic = new NetworkInformation();

                    this.label23.Text = "TSen: " + nic.BytesSent(0).ToString();
                    this.label24.Text = "TRec: " + nic.BytesReceived(0).ToString();

                }
                catch (Exception)
                {
                }
            }
        }
    }





public class NetworkAdapter
{







 internal NetworkAdapter(string name)
 {
  this.name = name;
 }

 private long dlSpeed, ulSpeed;
 private long dlValue, ulValue;
 private long dlValueOld, ulValueOld;

 internal string name;
 internal PerformanceCounter dlCounter, ulCounter;




 internal void init()
 {

  this.dlValueOld = this.dlCounter.NextSample().RawValue;
  this.ulValueOld = this.ulCounter.NextSample().RawValue;
 }





 internal void refresh()
 {
  this.dlValue = this.dlCounter.NextSample().RawValue;
  this.ulValue = this.ulCounter.NextSample().RawValue;


  this.dlSpeed = this.dlValue - this.dlValueOld;
  this.ulSpeed = this.ulValue - this.ulValueOld;

  this.dlValueOld = this.dlValue;
  this.ulValueOld = this.ulValue;
 }





 public override string ToString()
 {
  return this.name;
 }




 public string Name
 {
  get
  {
   return this.name;
  }
 }




 public long DownloadSpeed
 {
  get
  {
   return this.dlSpeed;
  }
 }




 public long UploadSpeed
 {
  get
  {
   return this.ulSpeed;
  }
 }




 public double DownloadSpeedKbps
 {
  get
  {
   return this.dlSpeed/1*(double)1024;
  }
 }




 public double UploadSpeedKbps
 {
  get
  {
   return this.ulSpeed/1*(double)1024;
  }
 }
}

public class NetworkInformation
{
        private static NetworkInterface[] NIC;

        public NetworkInformation()
        {
            NIC = NetworkInterface.GetAllNetworkInterfaces();
        }

        public string BytesReceived(int index)
        {
            return NIC[index].GetIPv4Statistics().BytesReceived.ToString();
        }

        public string BytesSent(int index)
        {
            return NIC[index].GetIPv4Statistics().BytesSent.ToString();
        }

        public string IncomingPacketsDiscarded(int index)
        {
            return NIC[index].GetIPv4Statistics().IncomingPacketsDiscarded.ToString();
        }

        public string IncomingPacketsWithErrors(int index)
        {
            return NIC[index].GetIPv4Statistics().IncomingPacketsWithErrors.ToString();
        }

        public string Description(int index)
        {
            return NIC[index].Description;
        }

        public string Speed(int index)
        {
            return NIC[index].Speed.ToString();
        }


}




public class NetworkMonitor
{
    private System.Timers.Timer timer;
    private ArrayList adapters;
    private ArrayList monitoredAdapters;




    public NetworkMonitor()
    {
        this.adapters = new ArrayList();
        this.monitoredAdapters = new ArrayList();
        this.EnumerateNetworkAdapters();

        this.timer = new System.Timers.Timer(1000);
        this.timer.Elapsed += new ElapsedEventHandler(this.timer_Elapsed);
    }




    private void EnumerateNetworkAdapters()
    {
        PerformanceCounterCategory category = new PerformanceCounterCategory("Network Interface");

        foreach (string name in category.GetInstanceNames())
        {

            if (name == "MS TCP Loopback interface")
            { continue; }

            NetworkAdapter adapter = new NetworkAdapter(name);
            adapter.dlCounter = new PerformanceCounter("Network Interface", "Bytes Received/sec", name);
            adapter.ulCounter = new PerformanceCounter("Network Interface", "Bytes Sent/sec", name);
            this.adapters.Add(adapter);
        }
    }

    private void timer_Elapsed(object sender, ElapsedEventArgs e)
    {
        foreach (NetworkAdapter adapter in this.monitoredAdapters)
        { adapter.refresh(); }
    }




    public NetworkAdapter[] Adapters
    {
        get
        {
            return (NetworkAdapter[])this.adapters.ToArray(typeof(NetworkAdapter));
        }
    }




    public void StartMonitoring()
    {
        if (this.adapters.Count > 0)
        {
            foreach (NetworkAdapter adapter in this.adapters)
                if (!this.monitoredAdapters.Contains(adapter))
                {
                    this.monitoredAdapters.Add(adapter);
                    adapter.init();
                }

            this.timer.Enabled = true;
        }
    }





    public void StartMonitoring(NetworkAdapter adapter)
    {
        if (!this.monitoredAdapters.Contains(adapter))
        {
            this.monitoredAdapters.Add(adapter);
            adapter.init();
        }
        this.timer.Enabled = true;
    }




    public void StopMonitoring()
    {
        this.monitoredAdapters.Clear();
        this.timer.Enabled = false;
    }





    public void StopMonitoring(NetworkAdapter adapter)
    {
        if (this.monitoredAdapters.Contains(adapter))
        { this.monitoredAdapters.Remove(adapter); }
        if (this.monitoredAdapters.Count == 0)
        { this.timer.Enabled = false; }
    }
}
}
