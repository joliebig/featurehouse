

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Eraser.Util;
using System.Diagnostics;
using System.Globalization;

namespace Eraser
{
 public partial class BlackBoxMainForm : Form
 {
  public BlackBoxMainForm()
  {
   InitializeComponent();
   Theming.ApplyTheme(this);

   ReportsLv.BeginUpdate();
   foreach (BlackBoxReport report in BlackBox.GetDumps())
   {
    if (report.Submitted)
     continue;

    ListViewItem item = ReportsLv.Items.Add(report.Timestamp.ToString(
     "F", CultureInfo.CurrentCulture));
    if (report.StackTrace.Count != 0)
     item.SubItems.Add(report.StackTrace[0].ExceptionType);
    item.Tag = report;
    item.Checked = true;
   }
   ReportsLv.EndUpdate();
  }

  private void ReportsLv_ItemActivate(object sender, EventArgs e)
  {
   Process.Start((ReportsLv.SelectedItems[0].Tag as BlackBoxReport).Path);
  }

  private void SubmitBtn_Click(object sender, EventArgs e)
  {
   List<BlackBoxReport> reports = new List<BlackBoxReport>();
   foreach (ListViewItem item in ReportsLv.Items)
    if (item.Checked)
     reports.Add((BlackBoxReport)item.Tag);
    else
     ((BlackBoxReport)item.Tag).Delete();

   if (reports.Count != 0)
   {
    BlackBoxUploadForm form = new BlackBoxUploadForm(reports);
    form.Show();
   }

   Close();
  }

  private void PostponeBtn_Click(object sender, EventArgs e)
  {
   Close();
  }




  private BlackBox BlackBox = BlackBox.Get();
 }
}
