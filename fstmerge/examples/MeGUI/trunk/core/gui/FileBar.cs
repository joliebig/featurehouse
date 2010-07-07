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

namespace MeGUI
{

    public delegate void FileBarEventHandler(FileBar sender, FileBarEventArgs args);
    
    public partial class FileBar : UserControl
    {
        private string oldName;
        public FileBar()
        {
            InitializeComponent();
            DragDropUtil.RegisterSingleFileDragDrop(filename, setFilename, delegate() { return Filter; });
        }

        private bool saveMode = false;
        public bool SaveMode
        {
            get { return saveMode; }
            set { saveMode = value; filename.AllowDrop = !value; }
        }

        public bool ReadOnly
        {
            get { return filename.ReadOnly; }
            set { filename.ReadOnly = value; }
        }

        private string title;

        public string Title
        {
            get { return title; }
            set { title = value; }
        }

        NotifyCounter raiseEvent = new NotifyCounter();

        public string Filename
        {
            get { return filename.Text; }
            set
            {
                using (IDisposable wrapper = raiseEvent.Wrap())
                {
                    filename.Text = value;
                    oldName = value;
                }
            }
        }

        private bool folderMode;

        public bool FolderMode
        {
            get { return folderMode; }
            set { folderMode = value; }
        }

        private string filter;

        public string Filter
        {
            get { return filter; }
            set { filter = value; }
        }

        private int filterIndex;

        public int FilterIndex
        {
            get { return filterIndex; }
            set { filterIndex = value; }
        }

        public event FileBarEventHandler FileSelected;

        private void openButton_Click(object sender, EventArgs e)
        {
            if (folderMode)
            {
                FolderBrowserDialog dialog = new FolderBrowserDialog();
                if (dialog.ShowDialog() == DialogResult.OK)
                    setFilename(dialog.SelectedPath);
            }
            else
            {
                FileDialog dialog = saveMode ?
                    (FileDialog)new SaveFileDialog() :
                    (FileDialog)new OpenFileDialog();

                dialog.Filter = filter;
                dialog.FilterIndex = filterIndex;
                dialog.Title = title;
                if (!string.IsNullOrEmpty (Filename))
                {
                    dialog.InitialDirectory = Path.GetDirectoryName (Filename);
                    dialog.FileName = Path.GetFileName (Filename);
                }
                if (dialog.ShowDialog() == DialogResult.OK)
                    setFilename(dialog.FileName);
            }
        }

        public void PerformClick()
        {
            openButton.PerformClick();
        }

        private void setFilename(string filename)
        {
            oldName = this.filename.Text;
            using (IDisposable a = raiseEvent.Wrap())
            {
                this.filename.Text = filename;
            }
            triggerEvent();
        }

        private void triggerEvent()
        {
            if (raiseEvent.Ready && FileSelected != null) FileSelected(this, new FileBarEventArgs(oldName, filename.Text));
            oldName = filename.Text;
        }

        private void filename_TextChanged(object sender, EventArgs e)
        {
            setFilename(filename.Text);
            triggerEvent();
        }
    }
    public class FileBarEventArgs : EventArgs
    {
        public readonly string OldFileName;
        public readonly string NewFileName;
        public FileBarEventArgs(string oldName, string newName)
            : base()
        {
            OldFileName = oldName;
            NewFileName = newName;
        }
    }

}
