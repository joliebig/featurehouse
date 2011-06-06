using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Globalization;
using Eraser.Manager;
using Eraser.Util;
namespace Eraser
{
 public partial class TaskPropertiesForm : Form
 {
  public TaskPropertiesForm()
  {
   InitializeComponent();
   Theming.ApplyTheme(this);
   scheduleTime.CustomFormat = DateTimeFormatInfo.CurrentInfo.ShortTimePattern;
   typeManual.Checked = true;
   scheduleDaily.Checked = true;
  }
  public Task Task
  {
   get { UpdateTaskFromUI(); return task; }
   set { task = value; UpdateUIFromTask(); }
  }
  private void UpdateTaskFromUI()
  {
   task.Name = name.Text;
   if (typeManual.Checked)
   {
    task.Schedule = Schedule.RunManually;
   }
   else if (typeImmediate.Checked)
   {
    task.Schedule = Schedule.RunNow;
   }
   else if (typeRestart.Checked)
   {
    task.Schedule = Schedule.RunOnRestart;
   }
   else if (typeRecurring.Checked)
   {
    RecurringSchedule schedule = new RecurringSchedule();
    task.Schedule = schedule;
    schedule.ExecutionTime = new DateTime(1, 1, 1, scheduleTime.Value.Hour,
     scheduleTime.Value.Minute, scheduleTime.Value.Second);
    if (scheduleDaily.Checked)
    {
     if (scheduleDailyByDay.Checked)
     {
      schedule.ScheduleType = RecurringScheduleUnit.Daily;
      schedule.Frequency = (int)scheduleDailyByDayFreq.Value;
     }
     else
     {
      schedule.ScheduleType = RecurringScheduleUnit.Weekdays;
     }
    }
    else if (scheduleWeekly.Checked)
    {
     schedule.ScheduleType = RecurringScheduleUnit.Weekly;
     schedule.Frequency = (int)scheduleWeeklyFreq.Value;
     DaysOfWeek weeklySchedule = 0;
     if (scheduleWeeklyMonday.Checked)
      weeklySchedule |= DaysOfWeek.Monday;
     if (scheduleWeeklyTuesday.Checked)
      weeklySchedule |= DaysOfWeek.Tuesday;
     if (scheduleWeeklyWednesday.Checked)
      weeklySchedule |= DaysOfWeek.Wednesday;
     if (scheduleWeeklyThursday.Checked)
      weeklySchedule |= DaysOfWeek.Thursday;
     if (scheduleWeeklyFriday.Checked)
      weeklySchedule |= DaysOfWeek.Friday;
     if (scheduleWeeklySaturday.Checked)
      weeklySchedule |= DaysOfWeek.Saturday;
     if (scheduleWeeklySunday.Checked)
      weeklySchedule |= DaysOfWeek.Sunday;
     schedule.WeeklySchedule = weeklySchedule;
    }
    else if (scheduleMonthly.Checked)
    {
     schedule.ScheduleType = RecurringScheduleUnit.Monthly;
     schedule.Frequency = (int)scheduleMonthlyFreq.Value;
     schedule.MonthlySchedule = (int)scheduleMonthlyDayNumber.Value;
    }
    else
     throw new ArgumentException("No such scheduling method.");
   }
  }
  private void UpdateUIFromTask()
  {
   name.Text = task.Name;
   foreach (ErasureTarget target in task.Targets)
   {
    ListViewItem item = data.Items.Add(target.UIText);
    item.SubItems.Add(target.MethodDefined ? target.Method.Name : S._("(default)"));
    item.Tag = target;
   }
   if (task.Schedule == Schedule.RunManually)
   {
    typeManual.Checked = true;
   }
   else if (task.Schedule == Schedule.RunNow)
   {
    typeImmediate.Checked = true;
   }
   else if (task.Schedule == Schedule.RunOnRestart)
   {
    typeRestart.Checked = true;
   }
   else
   {
    typeRecurring.Checked = true;
    RecurringSchedule schedule = (RecurringSchedule)task.Schedule;
    scheduleTime.Value = scheduleTime.MinDate.Add(schedule.ExecutionTime.TimeOfDay);
    switch (schedule.ScheduleType)
    {
     case RecurringScheduleUnit.Daily:
      scheduleDailyByDay.Checked = true;
      scheduleDailyByDayFreq.Value = schedule.Frequency;
      break;
     case RecurringScheduleUnit.Weekdays:
      scheduleDailyByWeekday.Checked = true;
      break;
     case RecurringScheduleUnit.Weekly:
      scheduleWeeklyFreq.Value = schedule.Frequency;
      scheduleWeekly.Checked = true;
      scheduleWeeklyMonday.Checked =
       (schedule.WeeklySchedule & DaysOfWeek.Monday) != 0;
      scheduleWeeklyTuesday.Checked =
       (schedule.WeeklySchedule & DaysOfWeek.Tuesday) != 0;
      scheduleWeeklyWednesday.Checked =
       (schedule.WeeklySchedule & DaysOfWeek.Wednesday) != 0;
      scheduleWeeklyThursday.Checked =
       (schedule.WeeklySchedule & DaysOfWeek.Thursday) != 0;
      scheduleWeeklyFriday.Checked =
       (schedule.WeeklySchedule & DaysOfWeek.Friday) != 0;
      scheduleWeeklySaturday.Checked =
       (schedule.WeeklySchedule & DaysOfWeek.Saturday) != 0;
      scheduleWeeklySunday.Checked =
       (schedule.WeeklySchedule & DaysOfWeek.Sunday) != 0;
      break;
     case RecurringScheduleUnit.Monthly:
      scheduleMonthly.Checked = true;
      scheduleMonthlyFreq.Value = schedule.Frequency;
      scheduleMonthlyDayNumber.Value = schedule.MonthlySchedule;
      break;
     default:
      throw new ArgumentException("Unknown schedule type.");
    }
   }
  }
  private void dataAdd_Click(object sender, EventArgs e)
  {
   using (TaskDataSelectionForm form = new TaskDataSelectionForm())
   {
    if (form.ShowDialog() == DialogResult.OK)
    {
     ErasureTarget target = form.Target;
     ListViewItem item = data.Items.Add(target.UIText);
     item.SubItems.Add(target.MethodDefined ? target.Method.Name : S._("(default)"));
     item.Tag = target;
     task.Targets.Add(target);
     errorProvider.Clear();
    }
   }
  }
  private void data_ItemActivate(object sender, EventArgs e)
  {
   using (TaskDataSelectionForm form = new TaskDataSelectionForm())
   {
    ListViewItem item = data.SelectedItems[0];
    form.Target = task.Targets[item.Index];
    if (form.ShowDialog() == DialogResult.OK)
    {
     ErasureTarget target = form.Target;
     task.Targets[item.Index] = target;
     item.Text = target.UIText;
     item.SubItems[1].Text = target.MethodDefined ? target.Method.Name : S._("(default)");
    }
   }
  }
  private void dataContextMenuStrip_Opening(object sender, CancelEventArgs e)
  {
   if (data.SelectedIndices.Count == 0)
   {
    e.Cancel = true;
    return;
   }
  }
  private void deleteDataToolStripMenuItem_Click(object sender, EventArgs e)
  {
   if (data.SelectedIndices.Count == 0)
    return;
   foreach (ListViewItem obj in data.SelectedItems)
   {
    task.Targets.Remove((ErasureTarget)obj.Tag);
    data.Items.Remove(obj);
   }
  }
  private void taskType_CheckedChanged(object sender, EventArgs e)
  {
   scheduleTimeLbl.Enabled = scheduleTime.Enabled = schedulePattern.Enabled =
    scheduleDaily.Enabled = scheduleWeekly.Enabled =
    scheduleMonthly.Enabled = typeRecurring.Checked;
   nonRecurringPanel.Visible = !typeRecurring.Checked;
   scheduleSpan_CheckedChanged(sender, e);
  }
  private void scheduleSpan_CheckedChanged(object sender, EventArgs e)
  {
   scheduleDailyByDay.Enabled = scheduleDailyByDayLbl.Enabled =
    scheduleDailyByWeekday.Enabled = scheduleDaily.Checked &&
    typeRecurring.Checked;
   scheduleWeeklyLbl.Enabled = scheduleWeeklyFreq.Enabled =
    scheduleWeeklyFreqLbl.Enabled = scheduleWeeklyMonday.Enabled =
    scheduleWeeklyTuesday.Enabled = scheduleWeeklyWednesday.Enabled =
    scheduleWeeklyThursday.Enabled = scheduleWeeklyFriday.Enabled =
    scheduleWeeklySaturday.Enabled = scheduleWeeklySunday.Enabled =
    scheduleWeekly.Checked && typeRecurring.Checked;
   scheduleMonthlyLbl.Enabled = scheduleMonthlyDayNumber.Enabled =
    scheduleMonthlyEveryLbl.Enabled = scheduleMonthlyFreq.Enabled =
    scheduleMonthlyMonthLbl.Enabled = scheduleMonthly.Checked &&
    typeRecurring.Checked;
   scheduleDailySpan_CheckedChanged(sender, e);
  }
  private void scheduleDailySpan_CheckedChanged(object sender, EventArgs e)
  {
   scheduleDailyByDayFreq.Enabled = scheduleDailyByDay.Checked &&
    scheduleDaily.Checked && typeRecurring.Checked;
  }
  private void ok_Click(object sender, EventArgs e)
  {
   if (data.Items.Count == 0)
   {
    errorProvider.SetIconPadding(data, -16);
    errorProvider.SetIconAlignment(data, ErrorIconAlignment.BottomRight);
    errorProvider.SetError(data, S._("The task has no data to erase."));
    container.SelectedIndex = 0;
    return;
   }
   else if (typeRecurring.Checked && scheduleWeekly.Checked)
   {
    if (!scheduleWeeklyMonday.Checked && !scheduleWeeklyTuesday.Checked &&
     !scheduleWeeklyWednesday.Checked && !scheduleWeeklyThursday.Checked &&
     !scheduleWeeklyFriday.Checked && !scheduleWeeklySaturday.Checked &&
     !scheduleWeeklySunday.Checked)
    {
     errorProvider.SetIconPadding(scheduleWeeklyDays, -16);
     errorProvider.SetError(scheduleWeeklyDays, S._("The task needs to run " +
      "on at least one day a week"));
     container.SelectedIndex = 1;
     return;
    }
   }
   errorProvider.Clear();
   DialogResult = DialogResult.OK;
   Close();
  }
  private Task task = new Task();
 }
}
