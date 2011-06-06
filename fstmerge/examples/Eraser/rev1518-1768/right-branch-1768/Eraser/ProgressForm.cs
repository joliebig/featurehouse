using System;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Eraser.Manager;
using Eraser.Util;
using System.Globalization;
namespace Eraser
{
 public partial class ProgressForm : Form
 {
  private Task task;
  private DateTime lastUpdate;
  public ProgressForm(Task task)
  {
   InitializeComponent();
   Theming.ApplyTheme(this);
   this.task = task;
   this.lastUpdate = DateTime.Now;
   this.ActiveControl = hide;
   jobTitle.Text = task.UIText;
   task.ProgressChanged += task_ProgressChanged;
   task.TaskFinished += task_TaskFinished;
   if (task.Progress.CurrentStep != null)
    UpdateProgress((SteppedProgressManager)task.Progress.CurrentStep.Progress,
     new ProgressChangedEventArgs(task.Progress.CurrentStep.Progress, null));
  }
  private void ProgressForm_FormClosed(object sender, FormClosedEventArgs e)
  {
   task.ProgressChanged -= task_ProgressChanged;
   task.TaskFinished -= task_TaskFinished;
  }
  private void task_ProgressChanged(object sender, ProgressChangedEventArgs e)
  {
   if (IsDisposed || !IsHandleCreated)
    return;
   if (InvokeRequired)
   {
    if (DateTime.Now - lastUpdate < new TimeSpan(0, 0, 0, 0, 300))
     return;
    lastUpdate = DateTime.Now;
    Invoke((EventHandler<ProgressChangedEventArgs>)task_ProgressChanged, sender, e);
    return;
   }
   ErasureTarget target = sender as ErasureTarget;
   if (target == null)
    return;
   SteppedProgressManager progress = target.Progress as SteppedProgressManager;
   if (progress == null)
    return;
   UpdateProgress(progress, e);
  }
  private void task_TaskFinished(object sender, EventArgs e)
  {
   if (IsDisposed || !IsHandleCreated)
    return;
   if (InvokeRequired)
   {
    Invoke((EventHandler)task_TaskFinished, sender, e);
    return;
   }
   Task task = (Task)sender;
   timeLeft.Text = item.Text = pass.Text = string.Empty;
   overallProgressLbl.Text = S._("Total: {0,2:#0.00%}", 1.0);
   overallProgress.Value = overallProgress.Maximum;
   itemProgressLbl.Text = "100%";
   itemProgress.Style = ProgressBarStyle.Continuous;
   itemProgress.Value = itemProgress.Maximum;
   LogLevel highestLevel = LogLevel.Information;
   LogEntryCollection entries = task.Log.LastSessionEntries;
   foreach (LogEntry log in entries)
    if (log.Level > highestLevel)
     highestLevel = log.Level;
   switch (highestLevel)
   {
    case LogLevel.Warning:
     status.Text = S._("Completed with warnings");
     break;
    case LogLevel.Error:
     status.Text = S._("Completed with errors");
     break;
    case LogLevel.Fatal:
     status.Text = S._("Not completed");
     break;
    default:
     status.Text = S._("Completed");
     break;
   }
   hide.Enabled = false;
   stop.Text = S._("Close");
  }
  private void hide_Click(object sender, EventArgs e)
  {
   Close();
  }
  private void stop_Click(object sender, EventArgs e)
  {
   if (task.Executing)
    task.Cancel();
   Close();
  }
  private void UpdateProgress(SteppedProgressManager targetProgress, ProgressChangedEventArgs e)
  {
   TaskProgressChangedEventArgs e2 = (TaskProgressChangedEventArgs)e.UserState;
   status.Text = targetProgress.CurrentStep.Name;
   if (e2 != null)
   {
    item.Text = WrapItemName(e2.ItemName);
    pass.Text = e2.ItemTotalPasses != 0 ?
     S._("{0} out of {1}", e2.ItemPass, e2.ItemTotalPasses) :
     e2.ItemPass.ToString(CultureInfo.CurrentCulture);
   }
   if (targetProgress.TimeLeft >= TimeSpan.Zero)
    timeLeft.Text = S._("About {0} left", RoundToSeconds(targetProgress.TimeLeft));
   else
    timeLeft.Text = S._("Unknown");
   if (targetProgress.Progress >= 0.0f)
   {
    itemProgress.Style = ProgressBarStyle.Continuous;
    itemProgress.Value = (int)(targetProgress.Progress * 1000);
    itemProgressLbl.Text = targetProgress.Progress.ToString("#0%",
     CultureInfo.CurrentCulture);
   }
   else
   {
    itemProgress.Style = ProgressBarStyle.Marquee;
    itemProgressLbl.Text = string.Empty;
   }
   overallProgress.Value = (int)(task.Progress.Progress * 1000);
   overallProgressLbl.Text = S._("Total: {0,2:#0.00%}", task.Progress.Progress);
  }
  private string WrapItemName(string itemName)
  {
   StringBuilder result = new StringBuilder(itemName.Length);
   using (Graphics g = item.CreateGraphics())
   {
    while (itemName.Length > 0)
    {
     int chars = 0;
     int lines = 0;
     g.MeasureString(itemName, item.Font, new SizeF(item.Width - 2, 15),
      StringFormat.GenericDefault, out chars, out lines);
     result.AppendLine(itemName.Substring(0, chars));
     itemName = itemName.Remove(0, chars);
    }
   }
   return result.ToString();
  }
  private static TimeSpan RoundToSeconds(TimeSpan span)
  {
   return new TimeSpan(span.Ticks - span.Ticks % TimeSpan.TicksPerSecond);
  }
 }
}
