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
using System.IO;
using System.Text;
using System.Windows.Forms;

using MeGUI.core.util;

namespace MeGUI.core.gui
{
    public partial class LogTree : UserControl
    {
        public LogTree()
        {
            InitializeComponent();

            ImageList i = new ImageList();
            i.Images.Add(System.Drawing.SystemIcons.Error);
            i.Images.Add(System.Drawing.SystemIcons.Warning);
            i.Images.Add(System.Drawing.SystemIcons.Information);
            treeView.ImageList = i;

            Log.SubItemAdded += delegate(object sender, EventArgs<LogItem> args)
            {
                Util.ThreadSafeRun(treeView, delegate { treeView.Nodes.Add(register(args.Data)); });
            };
        }


        public readonly LogItem Log = new LogItem("Log", ImageType.NoImage);


        private TreeNode register(LogItem log)
        {
            List<TreeNode> subNodes = log.SubEvents.ConvertAll<TreeNode>(delegate(LogItem e)
            {
                return register(e);
            });

            TreeNode node = new TreeNode(log.Text, (int)log.Type, (int)log.Type, subNodes.ToArray());
            node.Tag = log;

            log.SubItemAdded += delegate(object sender, EventArgs<LogItem> args)
            {
                Util.ThreadSafeRun(treeView, delegate { node.Nodes.Add(register(args.Data)); });
            };

            log.TypeChanged += delegate(object sender, EventArgs<ImageType> args)
            {
                Util.ThreadSafeRun(treeView, delegate { node.SelectedImageIndex = node.ImageIndex = (int)args.Data; });
            };
            log.Expanded += delegate(object sender, EventArgs e)
            {
                Util.ThreadSafeRun(treeView, delegate { node.Expand(); });
            };
            log.Collapsed += delegate(object sender, EventArgs e)
            {
                Util.ThreadSafeRun(treeView, delegate { node.Collapse(); });
            };

            return node;
        }

        private void ofIndividualNodeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            show(selectedLogItem, false);
        }

        private void ofBranchToolStripMenuItem_Click(object sender, EventArgs e)
        {
            show(selectedLogItem, true);
        }

        private void editLog_Click(object sender, EventArgs e)
        {
            show(Log, true);
        }

        private LogItem selectedLogItem
        {
            get
            {
                if (treeView.SelectedNode == null)
                    return null;

                return (treeView.SelectedNode.Tag as LogItem);
            }
        }

        private void show(LogItem l, bool subnodes)
        {
            if (l == null)
                return;

            TextViewer t = new TextViewer();
            t.Contents = l.ToString(subnodes);
            t.Wrap = false;
            t.ShowDialog();
        }

        private void saveLog_Click(object sender, EventArgs e)
        {
            save(Log);
        }

        private void saveBranch_Click(object sender, EventArgs e)
        {
            LogItem i = selectedLogItem;
            if (i == null)
            {
                MessageBox.Show("No log branch selected", "Can't save file", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }
            save(i);
        }

        private void save(LogItem i)
        {
            if (saveDialog.ShowDialog() != DialogResult.OK)
                return;

            try
            {
                File.WriteAllText(saveDialog.FileName, i.ToString());
                MessageBox.Show("File saved successfully", "Success", MessageBoxButtons.OK, MessageBoxIcon.None);
            }
            catch (IOException ie)
            {
                MessageBox.Show("Error saving file: " + ie.Message, "Error saving file", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

        }

       private void expandOrCollapseAll(LogItem i, bool expand)
        {
            if (expand)
                i.Expand();
            else
                i.Collapse();

            foreach (LogItem i2 in i.SubEvents)
                expandOrCollapseAll(i2, expand);
        }

        private void expandAll(LogItem i) { expandOrCollapseAll(i, true); }
        private void collapseAll(LogItem i) { expandOrCollapseAll(i, false); }

        private void expandLog_Click(object sender, EventArgs e)
        {
            expandAll(Log);
        }

        private void expandBranch_Click(object sender, EventArgs e)
        {
            expandAll(selectedLogItem);
        }

        private void collapseLog_Click(object sender, EventArgs e)
        {
            collapseAll(Log);
        }

        private void collapseBranch_Click(object sender, EventArgs e)
        {
            collapseAll(selectedLogItem);
        }

        private void LogTree_Load(object sender, EventArgs e)
        {

            if (VistaStuff.IsVistaOrNot)
            {
                VistaStuff.SetWindowTheme(treeView.Handle, "explorer", null);
            }
        }
    }
}
