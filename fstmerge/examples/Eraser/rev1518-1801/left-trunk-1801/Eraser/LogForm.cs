using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Eraser.Manager;
using System.Globalization;
using Eraser.Util;
using System.IO;
namespace Eraser
{
 public partial class LogForm : Form
 {
  public LogForm(Task task)
  {
   InitializeComponent();
   UXThemeApi.UpdateControlTheme(this);
   Text = string.Format(CultureInfo.InvariantCulture, "{0} - {1}", Text, task.UIText);
   foreach (DateTime session in task.Log.Entries.Keys)
    filterSessionCombobox.Items.Add(session);
   if (task.Log.Entries.Keys.Count != 0)
    filterSessionCombobox.SelectedIndex = filterSessionCombobox.Items.Count - 1;
   filterFilterTypeCombobox.SelectedIndex = 0;
   filterSeverityCombobox.SelectedIndex = 0;
   this.task = task;
   RefreshMessages();
   EnableButtons();
   task.Log.Logged += task_Logged;
   task.Log.NewSession += task_NewSession;
  }
  private void LogForm_FormClosed(object sender, FormClosedEventArgs e)
  {
   task.Log.Logged -= task_Logged;
  }
  private void filter_Changed(object sender, EventArgs e)
  {
   RefreshMessages();
  }
  private void task_NewSession(object sender, EventArgs e)
  {
   if (IsDisposed || !IsHandleCreated)
    return;
   if (InvokeRequired)
   {
    Invoke(new EventHandler<EventArgs>(task_NewSession), sender, e);
    return;
   }
   filterSessionCombobox.Items.Add(task.Log.LastSession);
  }
  private void task_Logged(object sender, LogEventArgs e)
  {
   if (IsDisposed || !IsHandleCreated)
    return;
   if (InvokeRequired)
   {
    Invoke(new EventHandler<LogEventArgs>(task_Logged), sender, e);
    return;
   }
   if (filterSessionCombobox.SelectedItem == null ||
    (DateTime)filterSessionCombobox.SelectedItem != task.Log.LastSession ||
    !MeetsCriteria(e.LogEntry))
   {
    return;
   }
   entryCache.Add(e.LogEntry);
   ++log.VirtualListSize;
   EnableButtons();
  }
  private void log_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
  {
   LogEntry entry = entryCache[e.ItemIndex];
   e.Item = new ListViewItem(entry.Timestamp.ToString("F", CultureInfo.CurrentCulture));
   e.Item.SubItems.Add(entry.Level.ToString());
   e.Item.SubItems.Add(entry.Message);
   switch (entry.Level)
   {
    case LogLevel.Fatal:
     e.Item.ForeColor = Color.Red;
     break;
    case LogLevel.Error:
     e.Item.ForeColor = Color.OrangeRed;
     break;
    case LogLevel.Warning:
     e.Item.ForeColor = Color.Orange;
     break;
   }
  }
  private void log_ItemSelectionChanged(object sender, ListViewItemSelectionChangedEventArgs e)
  {
   if (e.IsSelected)
   {
    if (!selectedEntries.ContainsKey(e.ItemIndex))
     selectedEntries.Add(e.ItemIndex, null);
   }
   else
   {
    selectedEntries.Remove(e.ItemIndex);
   }
  }
  private void log_VirtualItemsSelectionRangeChanged(object sender, ListViewVirtualItemsSelectionRangeChangedEventArgs e)
  {
   for (int i = e.StartIndex; i <= e.EndIndex; ++i)
   {
    if (e.IsSelected)
    {
     if (!selectedEntries.ContainsKey(i))
      selectedEntries.Add(i, null);
    }
    else
    {
     selectedEntries.Remove(i);
    }
   }
  }
  private void log_ItemActivate(object sender, EventArgs e)
  {
   if (selectedEntries.Count < 1)
    return;
   int currentEntryIndex = 0;
   LogEntry selectedEntry = new LogEntry();
   foreach (LogEntry entry in task.Log.Entries[(DateTime)filterSessionCombobox.SelectedItem])
   {
    if (!MeetsCriteria(entry))
     continue;
    if (!selectedEntries.ContainsKey(currentEntryIndex++))
     continue;
    selectedEntry = entry;
    break;
   }
   MessageBoxIcon icon = MessageBoxIcon.None;
   switch (selectedEntry.Level)
   {
    case LogLevel.Information:
     icon = MessageBoxIcon.Information;
     break;
    case LogLevel.Warning:
     icon = MessageBoxIcon.Warning;
     break;
    case LogLevel.Error:
    case LogLevel.Fatal:
     icon = MessageBoxIcon.Error;
     break;
   }
   MessageBox.Show(this, selectedEntry.Message,
    selectedEntry.Timestamp.ToString("F", CultureInfo.CurrentCulture),
    MessageBoxButtons.OK, icon);
  }
  private void logContextMenuStrip_Opening(object sender, CancelEventArgs e)
  {
   copySelectedEntriesToolStripMenuItem.Enabled = selectedEntries.Count != 0;
  }
  private void copySelectedEntriesToolStripMenuItem_Click(object sender, EventArgs e)
  {
   if (selectedEntries.Count == 0)
    return;
   StringBuilder csvText = new StringBuilder();
   StringBuilder rawText = new StringBuilder();
   LogSessionDictionary logEntries = task.Log.Entries;
   DateTime sessionTime = (DateTime)filterSessionCombobox.SelectedItem;
   csvText.AppendLine(S._("Session: {0:F}", sessionTime));
   rawText.AppendLine(S._("Session: {0:F}", sessionTime));
   int currentEntryIndex = 0;
   foreach (LogEntry entry in logEntries[sessionTime])
   {
    if (!MeetsCriteria(entry))
     continue;
    if (!selectedEntries.ContainsKey(currentEntryIndex++))
     continue;
    string timeStamp = entry.Timestamp.ToString("F", CultureInfo.CurrentCulture);
    string message = entry.Message;
    csvText.AppendFormat("\"{0}\",\"{1}\",\"{2}\"\n",
     timeStamp.Replace("\"", "\"\""), entry.Level.ToString(),
     message.Replace("\"", "\"\""));
    rawText.AppendFormat("{0}	{1}	{2}\n", timeStamp, entry.Level.ToString(),
     message);
   }
   if (csvText.Length > 0 || rawText.Length > 0)
   {
    DataObject tableText = new DataObject();
    tableText.SetText(rawText.ToString());
    byte[] bytes = Encoding.UTF8.GetBytes(csvText.ToString());
    MemoryStream tableStream = new MemoryStream(bytes);
    tableText.SetData(DataFormats.CommaSeparatedValue, tableStream);
    Clipboard.SetDataObject(tableText, true);
   }
  }
  private void clear_Click(object sender, EventArgs e)
  {
   task.Log.Clear();
   filterSessionCombobox.Items.Clear();
   log.VirtualListSize = 0;
   selectedEntries.Clear();
   entryCache.Clear();
   EnableButtons();
  }
  private void close_Click(object sender, EventArgs e)
  {
   Close();
  }
  private bool MeetsCriteria(LogEntry entry)
  {
   switch (filterFilterTypeCombobox.SelectedIndex)
   {
    case 0:
     if (entry.Level < (LogLevel)filterSeverityCombobox.SelectedIndex)
      return false;
     break;
    case 1:
     if (entry.Level != (LogLevel)filterSeverityCombobox.SelectedIndex)
      return false;
     break;
    case 2:
     if (entry.Level > (LogLevel)filterSeverityCombobox.SelectedIndex)
      return false;
     break;
   }
   return true;
  }
  private void RefreshMessages()
  {
   if (task == null)
    return;
   Application.UseWaitCursor = true;
   LogSessionDictionary log = task.Log.Entries;
   entryCache.Clear();
   selectedEntries.Clear();
   foreach (DateTime sessionTime in log.Keys)
   {
    if (filterSessionCombobox.SelectedItem == null ||
     sessionTime != (DateTime)filterSessionCombobox.SelectedItem)
     continue;
    foreach (LogEntry entry in log[sessionTime])
    {
     if (MeetsCriteria(entry))
      entryCache.Add(entry);
    }
   }
   this.log.VirtualListSize = entryCache.Count;
   this.log.Refresh();
   EnableButtons();
   Application.UseWaitCursor = false;
  }
  private void EnableButtons()
  {
   clear.Enabled = task.Log.Entries.Count > 0;
  }
  private Task task;
  private List<LogEntry> entryCache = new List<LogEntry>();
  private SortedList<int, object> selectedEntries = new SortedList<int, object>();
 }
}
