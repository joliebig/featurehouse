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
using System.Diagnostics;
using System.Text;
using System.Windows.Forms;

using MeGUI.core.util;

namespace MeGUI.core.gui
{
    public class TargetSizeSCBox : StandardAndCustomComboBox
    {
        public static readonly Named<FileSize>[] PredefinedFilesizes = new Named<FileSize>[] {
            new Named<FileSize>("1/4 CD  (175MB)", new FileSize(Unit.MB, 175)),
            new Named<FileSize>("1/2 CD  (350MB)", new FileSize(Unit.MB, 350)),
            new Named<FileSize>("CD  (700MB)", new FileSize(Unit.MB, 700)),
            new Named<FileSize>("2 CDs  (1400MB)", new FileSize(Unit.MB, 1400)),
            new Named<FileSize>("3 CDs  (2100MB)", new FileSize(Unit.MB, 2100)),
            new Named<FileSize>("1/5 DVD  (896MB)", new FileSize(Unit.MB, 896)),
            new Named<FileSize>("1/4 DVD  (1120MB)", new FileSize(Unit.MB, 1120)),
            new Named<FileSize>("1/3 DVD  (1492MB)", new FileSize(Unit.MB, 1492)),
            new Named<FileSize>("1/2 DVD  (2240MB)", new FileSize(Unit.MB, 2240)),
            new Named<FileSize>("DVD or BD-5  (4480MB)", new FileSize(Unit.MB, 4480)),
            new Named<FileSize>("1½ DVD  (6720MB)", new FileSize(Unit.MB, 6720)),
            new Named<FileSize>("DVD-DL or BD-9 (8145MB)", new FileSize(Unit.MB, 8145)),
            new Named<FileSize>("BD  (23450MB)", new FileSize(Unit.MB, 23450)),
            new Named<FileSize>("BD-DL  (46900MB)", new FileSize(Unit.MB, 46900)) };

        protected override void Dispose(bool disposing)
        {
            CustomUserSettings.Default.CustomSizes = CustomSizes;
            base.Dispose(disposing);
        }

        private string nullString;
        /// <summary>
        /// String to display which represents "null" filesize. If NullString is set to null, then
        /// there is no option not to select a filesize.
        /// </summary>
        public string NullString
        {
            get { return nullString; }
            set { nullString = value; fillStandard(); }
        }

        private FileSize minSize = FileSize.MinNonZero;

        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public FileSize MinimumFileSize
        {
            get { return minSize; }
            set { minSize = value; }
        }

        private FileSize? maxSize = null;
        
        [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public FileSize? MaximumFileSize
        {
            get { return maxSize; }
            set { maxSize = value; }
        }

        public TargetSizeSCBox() : base("Clear user-selected sizes...", "Select size...")
        {
            base.Getter = new Getter<object>(getter);
            CustomSizes = CustomUserSettings.Default.CustomSizes;
        }

        private void fillStandard()
        {
            List<object> objects = new List<object>();
            if (!string.IsNullOrEmpty(NullString))
                objects.Add(NullString);
            objects.AddRange(TargetSizeSCBox.PredefinedFilesizes);
            base.StandardItems = objects.ToArray();

        }

        FileSizeDialog ofd = new FileSizeDialog();

        private object getter()
        {
            ofd.Value = Value ?? new FileSize(Unit.MB, 700);
            if (ofd.ShowDialog() == DialogResult.OK)
            {
                if (ofd.Value >= minSize &&
                   maxSize == null || ofd.Value <= maxSize)
                    return ofd.Value;
                else
                    MessageBox.Show(genRestrictions(), "Invalid filesize", MessageBoxButtons.OK, MessageBoxIcon.Warning);
            }
            return null;
        }

        public FileSize[] CustomSizes
        {
            get
            {
                return Util.CastAll<FileSize>(CustomItems);
            }
            set
            {
                CustomItems = Util.CastAll<FileSize, object>(value);
            }
        }


        private string genRestrictions()
        {
            if (maxSize.HasValue)
                return string.Format("Filesize must be between {0} and {1}.", minSize, maxSize);
            else
                return string.Format("Filesize must be at least {0}.", minSize);
        }

        /// <summary>
        /// Gets / sets the target, or null if the user doesn't care about filesize
        /// </summary>
        [Browsable(false)]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public FileSize? Value
        {
            get
            {
                object o = base.SelectedObject;
                if (o.Equals(NullString))
                    return null;
                if (o is Named<FileSize>)
                    return ((Named<FileSize>)o).Data;
                else
                    return (FileSize)o;
            }

            set
            {
                if (value.HasValue)
                    base.SelectedObject = value.Value;
                else
                    base.SelectedObject = NullString;
            }
        }

        /// <summary>
        /// Gets / sets the target, or null if the user doesn't care about filesize
        /// </summary>
        [Browsable(false)]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public FileSize CertainValue
        {
            get { return Value.Value; }
            set { Value = value; }
        }

    }
}
