

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using Eraser.Manager;
using Eraser.Util;
using Eraser.Util.ExtensionMethods;
using System.IO;

namespace Eraser
{
 public partial class TaskDataSelectionForm : Form
 {
  private class DriveItem
  {
   public override string ToString()
   {
    return Label;
   }

   public string Drive;
   public string Label;
   public Icon Icon;
  }

  public TaskDataSelectionForm()
  {

   InitializeComponent();
   Theming.ApplyTheme(this);
   file.Checked = true;


   foreach (VolumeInfo volume in VolumeInfo.Volumes)
   {
    DriveType driveType = volume.VolumeType;
    if (driveType != DriveType.Unknown &&
     driveType != DriveType.NoRootDirectory &&
     driveType != DriveType.CDRom &&
     driveType != DriveType.Network)
    {

     if (!volume.IsMounted)
      continue;

     DriveItem item = new DriveItem();
     string volumePath = volume.MountPoints[0];
     DirectoryInfo root = new DirectoryInfo(volumePath);

     item.Drive = volumePath;
     item.Label = root.GetDescription();
     item.Icon = root.GetIcon();
     unusedDisk.Items.Add(item);
    }
   }

   if (unusedDisk.Items.Count != 0)
    unusedDisk.SelectedIndex = 0;


   Dictionary<Guid, ErasureMethod> methods = ErasureMethodManager.Items;
   this.method.Items.Add(ErasureMethodManager.Default);
   foreach (ErasureMethod method in methods.Values)
    this.method.Items.Add(method);
   if (this.method.Items.Count != 0)
    this.method.SelectedIndex = 0;
  }






  public ErasureTarget Target
  {
   get
   {
    ErasureTarget result = null;
    if (file.Checked)
    {
     FileTarget fileTask = new FileTarget();
     result = fileTask;

     fileTask.Path = filePath.Text;
    }
    else if (folder.Checked)
    {
     FolderTarget folderTask = new FolderTarget();
     result = folderTask;

     folderTask.Path = folderPath.Text;
     folderTask.IncludeMask = folderInclude.Text;
     folderTask.ExcludeMask = folderExclude.Text;
     folderTask.DeleteIfEmpty = folderDelete.Checked;
    }
    else if (unused.Checked)
    {
     UnusedSpaceTarget unusedSpaceTask = new UnusedSpaceTarget();
     result = unusedSpaceTask;

     unusedSpaceTask.Drive = ((DriveItem)unusedDisk.SelectedItem).Drive;
     unusedSpaceTask.EraseClusterTips = unusedClusterTips.Checked;
    }
    else
    {
     RecycleBinTarget recycleBinTask = new RecycleBinTarget();
     result = recycleBinTask;
    }

    result.Method = (ErasureMethod)this.method.SelectedItem;
    return result;
   }
   set
   {

    if (value.MethodDefined)
    {
     foreach (object item in method.Items)
      if (((ErasureMethod)item).Guid == value.Method.Guid)
       method.SelectedItem = item;
    }
    else
     method.SelectedIndex = 0;


    FileTarget fileTarget = value as FileTarget;
    FolderTarget folderTarget = value as FolderTarget;
    UnusedSpaceTarget unusedSpaceTarget = value as UnusedSpaceTarget;

    if (fileTarget != null)
    {
     file.Checked = true;
     filePath.Text = fileTarget.Path;
    }
    else if (folderTarget != null)
    {
     folder.Checked = true;

     folderPath.Text = folderTarget.Path;
     folderInclude.Text = folderTarget.IncludeMask;
     folderExclude.Text = folderTarget.ExcludeMask;
     folderDelete.Checked = folderTarget.DeleteIfEmpty;
    }
    else if (unusedSpaceTarget != null)
    {
     unused.Checked = true;
     foreach (object item in unusedDisk.Items)
      if (((DriveItem)item).Drive == unusedSpaceTarget.Drive)
       unusedDisk.SelectedItem = item;
     unusedClusterTips.Checked = unusedSpaceTarget.EraseClusterTips;
    }
    else if (value is RecycleBinTarget)
    {
     recycleBin.Checked = true;
    }
    else
     throw new ArgumentException("Unknown erasure target.");
   }
  }

  private void method_SelectedIndexChanged(object sender, EventArgs e)
  {
   if (!(method.SelectedItem is UnusedSpaceErasureMethod) &&
    method.SelectedItem != ErasureMethodManager.Default)
   {
    if (unused.Checked)
    {
     file.Checked = true;
     errorProvider.SetError(unused, S._("The erasure method selected does " +
      "not support unused disk space erasures."));
    }
    unused.Enabled = false;
   }
   else if (!unused.Enabled)
   {
    unused.Enabled = true;
    errorProvider.Clear();
   }
  }

  private void data_CheckedChanged(object sender, EventArgs e)
  {
   filePath.Enabled = fileBrowse.Enabled = file.Checked;
   folderPath.Enabled = folderBrowse.Enabled = folderIncludeLbl.Enabled =
    folderInclude.Enabled = folderExcludeLbl.Enabled = folderExclude.Enabled =
    folderDelete.Enabled = folder.Checked;
   unusedDisk.Enabled = unusedClusterTips.Enabled = unused.Checked;
   errorProvider.Clear();
  }

  private void fileBrowse_Click(object sender, EventArgs e)
  {
   fileDialog.FileName = filePath.Text;
   if (fileDialog.ShowDialog() == DialogResult.OK)
    filePath.Text = fileDialog.FileName;
  }

  private void folderBrowse_Click(object sender, EventArgs e)
  {
   try
   {
    folderDialog.SelectedPath = folderPath.Text;
    if (folderDialog.ShowDialog() == DialogResult.OK)
     folderPath.Text = folderDialog.SelectedPath;
   }
   catch (NotSupportedException)
   {
    MessageBox.Show(this, S._("The path you selected is invalid."), S._("Eraser"),
     MessageBoxButtons.OK, MessageBoxIcon.Error, MessageBoxDefaultButton.Button1,
     S.IsRightToLeft(this) ?
      MessageBoxOptions.RtlReading | MessageBoxOptions.RightAlign : 0);
   }
  }

  private void unusedDisk_DrawItem(object sender, DrawItemEventArgs e)
  {
   if (e.Index == -1)
    return;

   Graphics g = e.Graphics;
   DriveItem item = (DriveItem)unusedDisk.Items[e.Index];
   Color textColour = e.ForeColor;
   PointF textPos = e.Bounds.Location;
   textPos.X += item.Icon.Width + 4;
   textPos.Y += 2;


   if ((e.State & DrawItemState.Disabled) == 0)
    e.DrawBackground();
   else
   {
    g.FillRectangle(new SolidBrush(SystemColors.ButtonFace), e.Bounds);
    textColour = SystemColors.GrayText;
   }

   g.DrawIcon(item.Icon, e.Bounds.X + 2, e.Bounds.Y);
   g.DrawString(item.Label, e.Font, new SolidBrush(textColour), textPos);
   if ((e.State & DrawItemState.Focus) != 0)
    e.DrawFocusRectangle();
  }

  private void ok_Click(object sender, EventArgs e)
  {
   if (file.Checked && filePath.Text.Length == 0)
    errorProvider.SetError(filePath, S._("Invalid file path"));
   else if (folder.Checked && folderPath.Text.Length == 0)
    errorProvider.SetError(folderPath, S._("Invalid folder path"));
   else
   {
    errorProvider.Clear();
    DialogResult = DialogResult.OK;
    Close();
   }
  }
 }
}
