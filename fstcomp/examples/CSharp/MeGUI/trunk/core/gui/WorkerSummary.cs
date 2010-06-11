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
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using MeGUI.core.util;
using MeGUI.core.details;

namespace MeGUI.core.gui
{
    public partial class WorkerSummary : Form
    {
        public WorkerSummary(JobControl jobs)
        {
            InitializeComponent();
            panel1.Controls.Clear(); // they're just there for the designer
            panel1.Dock = DockStyle.Top;
            int width = panel1.Width;
            panel1.Dock = DockStyle.None;
            panel1.Height = 0;
            panel1.Width = width;
            panel1.Location = new Point(0, 0);
            panel1.Anchor = AnchorStyles.Top | AnchorStyles.Left | AnchorStyles.Right;
            this.jobs = jobs;
        }

        JobControl jobs;
        Dictionary<string, IndividualWorkerSummary> displays = new Dictionary<string,IndividualWorkerSummary>();

        public void Rename(string workerName, string newName)
        {
            IndividualWorkerSummary i = displays[workerName];
            displays.Remove(workerName);
            displays[newName] = i;
        }

        public void RefreshInfo()
        {
            if (InvokeRequired)
            {
                Invoke(new MethodInvoker(RefreshInfo));
                return;
            }
            if (!Visible) return;
            foreach (IndividualWorkerSummary i in displays.Values)
                i.RefreshInfo();
        }

        public void Add(JobWorker w)
        {
            IndividualWorkerSummary i = new IndividualWorkerSummary();
            i.Worker = w;
            i.Dock = DockStyle.Bottom;
            panel1.Controls.Add(i);
            displays[w.Name] = i;
            RefreshInfo();
        }

        public void Remove(string name)
        {
            Util.ThreadSafeRun(panel1, delegate { panel1.Controls.Remove(displays[name]); });
            displays.Remove(name);
            RefreshInfo();
        }

        private void WorkerSummary_VisibleChanged(object sender, EventArgs e)
        {
            if (Visible)
                RefreshInfo();
        }

        internal void RefreshInfo(string name)
        {
            if (InvokeRequired)
            {
                Invoke(new MethodInvoker(delegate { RefreshInfo(name); }));
                return;
            }
            if (Visible && displays.ContainsKey(name))
                displays[name].RefreshInfo();
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            e.Cancel = true;
            Hide();
        }

        private void newWorkerToolStripMenuItem_Click(object sender, EventArgs e)
        {
            jobs.RequestNewWorker();
        }

    }
}