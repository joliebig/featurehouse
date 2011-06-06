using System;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Eraser.Manager;
using Eraser.Util;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Diagnostics;
using System.IO;
using System.Runtime.Serialization;
using System.ComponentModel;
using ProgressChangedEventArgs = Eraser.Manager.ProgressChangedEventArgs;
namespace Eraser
{
 internal partial class SchedulerPanel : Eraser.BasePanel
 {
  public SchedulerPanel()
  {
   InitializeComponent();
   UXThemeApi.UpdateControlTheme(schedulerDefaultMenu);
   CreateHandle();
   ExecutorTasksCollection tasks = Program.eraserClient.Tasks;
   foreach (Task task in tasks)
    DisplayTask(task);
   Program.eraserClient.TaskAdded += TaskAdded;
   Program.eraserClient.TaskDeleted += TaskDeleted;
  }
  private void DisplayTask(Task task)
  {
   ListViewItem item = scheduler.Items.Add(task.UIText);
   item.SubItems.Add(string.Empty);
   item.SubItems.Add(string.Empty);
   item.Tag = task;
   task.TaskStarted += task_TaskStarted;
   task.ProgressChanged += task_ProgressChanged;
   task.TaskFinished += task_TaskFinished;
   UpdateTask(item);
  }
  private void UpdateTask(ListViewItem item)
  {
   Task task = (Task)item.Tag;
   item.Text = task.UIText;
   if (task.Queued)
   {
    item.SubItems[1].Text = S._("Queued for execution");
    item.SubItems[2].Text = string.Empty;
   }
   else if (task.Schedule is RecurringSchedule)
    item.SubItems[1].Text = ((task.Schedule as RecurringSchedule).NextRun.
     ToString("F", CultureInfo.CurrentCulture));
   else if (task.Schedule == Schedule.RunNow || task.Schedule == Schedule.RunManually)
    item.SubItems[1].Text = S._("Not queued");
   else
    item.SubItems[1].Text = task.Schedule.UIText;
   CategorizeTask(task, item);
  }
  private void CategorizeTask(Task task)
  {
   CategorizeTask(task, GetTaskItem(task));
  }
  private void CategorizeTask(Task task, ListViewItem item)
  {
   if (task.Schedule == Schedule.RunNow || task.Schedule == Schedule.RunManually)
    item.Group = scheduler.Groups["manual"];
   else if (task.Schedule == Schedule.RunOnRestart)
    item.Group = scheduler.Groups["restart"];
   else
    item.Group = scheduler.Groups["recurring"];
  }
  private void TaskAdded(object sender, TaskEventArgs e)
  {
   if (InvokeRequired)
   {
    Invoke(new EventHandler<TaskEventArgs>(TaskAdded), sender, e);
    return;
   }
   MainForm parent = (MainForm)FindForm();
   if (parent != null && (parent.WindowState == FormWindowState.Minimized || !parent.Visible))
   {
    parent.ShowNotificationBalloon(S._("New task added"), S._("{0} " +
     "has just been added to the list of tasks.", e.Task.UIText),
     ToolTipIcon.Info);
   }
   DisplayTask(e.Task);
  }
  private void DeleteSelectedTasks()
  {
   if (MessageBox.Show(this, S._("Are you sure you want to delete the selected tasks?"),
       S._("Eraser"), MessageBoxButtons.YesNo, MessageBoxIcon.Question,
       MessageBoxDefaultButton.Button1,
       S.IsRightToLeft(this) ? MessageBoxOptions.RtlReading : 0) != DialogResult.Yes)
   {
    return;
   }
   foreach (ListViewItem item in scheduler.SelectedItems)
   {
    Task task = (Task)item.Tag;
    if (!task.Executing)
     Program.eraserClient.Tasks.Remove(task);
   }
  }
  private void TaskDeleted(object sender, TaskEventArgs e)
  {
   if (InvokeRequired)
   {
    Invoke(new EventHandler<TaskEventArgs>(TaskDeleted), sender, e);
    return;
   }
   foreach (ListViewItem item in scheduler.Items)
    if (((Task)item.Tag) == e.Task)
    {
     scheduler.Items.Remove(item);
     break;
    }
   PositionProgressBar();
  }
  void task_TaskStarted(object sender, TaskEventArgs e)
  {
   if (scheduler.InvokeRequired)
   {
    Invoke(new EventHandler<TaskEventArgs>(task_TaskStarted), sender, e);
    return;
   }
   ListViewItem item = GetTaskItem(e.Task);
   item.SubItems[1].Text = S._("Running...");
   schedulerProgress.Tag = item;
   schedulerProgress.Visible = true;
   schedulerProgress.Value = 0;
   PositionProgressBar();
  }
  void task_ProgressChanged(object sender, ProgressChangedEventArgs e)
  {
   if (scheduler.InvokeRequired)
   {
    Invoke((EventHandler<ProgressChangedEventArgs>)task_ProgressChanged, sender, e);
    return;
   }
   ErasureTarget target = (ErasureTarget)sender;
   schedulerProgress.Value = (int)(target.Task.Progress.Progress * 1000.0);
  }
  void task_TaskFinished(object sender, TaskEventArgs e)
  {
   if (InvokeRequired)
   {
    Invoke(new EventHandler<TaskEventArgs>(task_TaskFinished), sender, e);
    return;
   }
   ListViewItem item = GetTaskItem(e.Task);
   if (item == null)
    return;
   if (schedulerProgress.Tag != null && schedulerProgress.Tag == item)
   {
    schedulerProgress.Tag = null;
    schedulerProgress.Visible = false;
   }
   LogLevel highestLevel = LogLevel.Information;
   LogEntryCollection logs = e.Task.Log.LastSessionEntries;
   foreach (LogEntry log in logs)
    if (log.Level > highestLevel)
     highestLevel = log.Level;
   MainForm parent = (MainForm)FindForm();
   if (parent == null)
    throw new InvalidOperationException();
   if (parent.WindowState == FormWindowState.Minimized || !parent.Visible)
   {
    string message = null;
    ToolTipIcon icon = ToolTipIcon.None;
    switch (highestLevel)
    {
     case LogLevel.Warning:
      message = S._("The task {0} has completed with warnings.", e.Task.UIText);
      icon = ToolTipIcon.Warning;
      break;
     case LogLevel.Error:
      message = S._("The task {0} has completed with errors.", e.Task.UIText);
      icon = ToolTipIcon.Error;
      break;
     case LogLevel.Fatal:
      message = S._("The task {0} did not complete.", e.Task.UIText);
      icon = ToolTipIcon.Error;
      break;
     default:
      message = S._("The task {0} has completed.", e.Task.UIText);
      icon = ToolTipIcon.Info;
      break;
    }
    parent.ShowNotificationBalloon(S._("Task executed"), message,
     icon);
   }
   if (EraserSettings.Get().ClearCompletedTasks &&
    (e.Task.Schedule == Schedule.RunNow) && highestLevel < LogLevel.Warning)
   {
    Program.eraserClient.Tasks.Remove(e.Task);
   }
   else
   {
    switch (highestLevel)
    {
     case LogLevel.Warning:
      item.SubItems[2].Text = S._("Completed with warnings");
      break;
     case LogLevel.Error:
      item.SubItems[2].Text = S._("Completed with errors");
      break;
     case LogLevel.Fatal:
      item.SubItems[2].Text = S._("Not completed");
      break;
     default:
      item.SubItems[2].Text = S._("Completed");
      break;
    }
    CategorizeTask(e.Task, item);
    UpdateTask(item);
   }
  }
  private void scheduler_KeyDown(object sender, KeyEventArgs e)
  {
   if (e.KeyCode == Keys.Delete)
    DeleteSelectedTasks();
  }
  private void scheduler_ItemActivate(object sender, EventArgs e)
  {
   if (scheduler.SelectedItems.Count == 0)
    return;
   ListViewItem item = scheduler.SelectedItems[0];
   if (((Task)item.Tag).Executing)
    using (ProgressForm form = new ProgressForm((Task)item.Tag))
     form.ShowDialog();
   else
    editTaskToolStripMenuItem_Click(sender, e);
  }
  private void scheduler_DragEnter(object sender, DragEventArgs e)
  {
   string descriptionMessage = string.Empty;
   string descriptionInsert = string.Empty;
   const string descrptionPlaceholder = "%1";
   if (!e.Data.GetDataPresent(DataFormats.FileDrop))
    e.Effect = DragDropEffects.None;
   else
   {
    string[] files = (string[])e.Data.GetData(DataFormats.FileDrop, false);
    bool isTaskList = true;
    foreach (string file in files)
    {
     if (descriptionInsert.Length < 259 &&
      (descriptionInsert.Length < 3 || descriptionInsert.Substring(descriptionInsert.Length - 3) != "..."))
     {
      string append = string.Format(CultureInfo.InvariantCulture, "{0}, ",
       Path.GetFileNameWithoutExtension(file));
      if (descriptionInsert.Length + append.Length > 259)
      {
       descriptionInsert += ".....";
      }
      else
      {
       descriptionInsert += append;
      }
     }
     if (Path.GetExtension(file) != ".ersx")
      isTaskList = false;
    }
    descriptionInsert = descriptionInsert.Remove(descriptionInsert.Length - 2);
    if (isTaskList)
    {
     e.Effect = DragDropEffects.Copy;
     descriptionMessage = S._("Import tasks from {0}", descrptionPlaceholder);
    }
    else
    {
     e.Effect = DragDropEffects.Move;
     descriptionMessage = S._("Erase {0}", descrptionPlaceholder);
    }
   }
   DropTargetHelper.DragEnter(this, e.Data, new Point(e.X, e.Y), e.Effect,
    descriptionMessage, descriptionInsert);
  }
  private void scheduler_DragLeave(object sender, EventArgs e)
  {
   DropTargetHelper.DragLeave(this);
  }
  private void scheduler_DragOver(object sender, DragEventArgs e)
  {
   DropTargetHelper.DragOver(new Point(e.X, e.Y), e.Effect);
  }
  private void scheduler_DragDrop(object sender, DragEventArgs e)
  {
   if (!e.Data.GetDataPresent(DataFormats.FileDrop))
    e.Effect = DragDropEffects.None;
   else
   {
    string[] files = (string[])e.Data.GetData(DataFormats.FileDrop, false);
    if (e.Effect == DragDropEffects.Copy)
    {
     foreach (string file in files)
      using (FileStream stream = new FileStream(file, FileMode.Open,
       FileAccess.Read, FileShare.Read))
      {
       try
       {
        Program.eraserClient.Tasks.LoadFromStream(stream);
       }
       catch (SerializationException ex)
       {
        MessageBox.Show(S._("Could not import task list from {0}. The error " +
         "returned was: {1}", file, ex.Message), S._("Eraser"),
         MessageBoxButtons.OK, MessageBoxIcon.Error,
         MessageBoxDefaultButton.Button1,
         S.IsRightToLeft(null) ? MessageBoxOptions.RtlReading : 0);
       }
      }
    }
    else if (e.Effect == DragDropEffects.Move)
    {
     Task task = new Task();
     foreach (string file in files)
     {
      FileSystemObjectTarget target;
      if (Directory.Exists(file))
       target = new FolderTarget();
      else
       target = new FileTarget();
      target.Path = file;
      task.Targets.Add(target);
     }
     Program.eraserClient.Tasks.Add(task);
    }
   }
   DropTargetHelper.Drop(e.Data, new Point(e.X, e.Y), e.Effect);
  }
  private void schedulerMenu_Opening(object sender, CancelEventArgs e)
  {
   if (scheduler.SelectedItems.Count == 0)
   {
    schedulerDefaultMenu.Show(schedulerMenu.Left, schedulerMenu.Top);
    e.Cancel = true;
    return;
   }
   bool aTaskNotQueued = false;
   bool aTaskExecuting = false;
   foreach (ListViewItem item in scheduler.SelectedItems)
   {
    Task task = (Task)item.Tag;
    aTaskNotQueued = aTaskNotQueued || (!task.Queued && !task.Executing);
    aTaskExecuting = aTaskExecuting || task.Executing;
   }
   runNowToolStripMenuItem.Enabled = aTaskNotQueued;
   cancelTaskToolStripMenuItem.Enabled = aTaskExecuting;
   editTaskToolStripMenuItem.Enabled = scheduler.SelectedItems.Count == 1 &&
    !((Task)scheduler.SelectedItems[0].Tag).Executing &&
    !((Task)scheduler.SelectedItems[0].Tag).Queued;
   deleteTaskToolStripMenuItem.Enabled = !aTaskExecuting;
  }
  private void newTaskToolStripMenuItem_Click(object sender, EventArgs e)
  {
   using (TaskPropertiesForm form = new TaskPropertiesForm())
   {
    if (form.ShowDialog() == DialogResult.OK)
    {
     Task task = form.Task;
     Program.eraserClient.Tasks.Add(task);
    }
   }
  }
  private void runNowToolStripMenuItem_Click(object sender, EventArgs e)
  {
   foreach (ListViewItem item in scheduler.SelectedItems)
   {
    Task task = (Task)item.Tag;
    if (!task.Executing && !task.Queued)
    {
     Program.eraserClient.QueueTask(task);
     item.SubItems[1].Text = S._("Queued for execution");
    }
   }
  }
  private void cancelTaskToolStripMenuItem_Click(object sender, EventArgs e)
  {
   foreach (ListViewItem item in scheduler.SelectedItems)
   {
    Task task = (Task)item.Tag;
    if (task.Executing || task.Queued)
    {
     task.Cancel();
     item.SubItems[1].Text = string.Empty;
    }
   }
  }
  private void viewTaskLogToolStripMenuItem_Click(object sender, EventArgs e)
  {
   if (scheduler.SelectedItems.Count != 1)
    return;
   ListViewItem item = scheduler.SelectedItems[0];
   using (LogForm form = new LogForm((Task)item.Tag))
    form.ShowDialog();
  }
  private void editTaskToolStripMenuItem_Click(object sender, EventArgs e)
  {
   if (scheduler.SelectedItems.Count != 1 ||
    ((Task)scheduler.SelectedItems[0].Tag).Executing ||
    ((Task)scheduler.SelectedItems[0].Tag).Queued)
   {
    return;
   }
   ListViewItem item = scheduler.SelectedItems[0];
   Task task = (Task)item.Tag;
   if (task.Executing)
    return;
   using (TaskPropertiesForm form = new TaskPropertiesForm())
   {
    form.Task = task;
    if (form.ShowDialog() == DialogResult.OK)
    {
     task = form.Task;
     UpdateTask(item);
    }
   }
  }
  private void deleteTaskToolStripMenuItem_Click(object sender, EventArgs e)
  {
   DeleteSelectedTasks();
  }
  private ListViewItem GetTaskItem(Task task)
  {
   foreach (ListViewItem item in scheduler.Items)
    if (item.Tag == task)
     return item;
   return null;
  }
  private void PositionProgressBar()
  {
   if (schedulerProgress.Tag == null)
    return;
   Rectangle rect = ((ListViewItem)schedulerProgress.Tag).SubItems[2].Bounds;
   schedulerProgress.Location = rect.Location;
   schedulerProgress.Size = rect.Size;
  }
  private void scheduler_DrawSubItem(object sender, DrawListViewSubItemEventArgs e)
  {
   e.DrawDefault = true;
   if (schedulerProgress.Tag != null)
    PositionProgressBar();
  }
  private void scheduler_DrawColumnHeader(object sender, DrawListViewColumnHeaderEventArgs e)
  {
   e.DrawDefault = true;
  }
 }
}
