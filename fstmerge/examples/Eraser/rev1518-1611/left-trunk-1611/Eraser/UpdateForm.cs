using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Net;
using System.Reflection;
using System.IO;
using System.Xml;
using Eraser.Util;
using System.Net.Cache;
using System.Net.Mime;
using System.Globalization;
namespace Eraser
{
 public partial class UpdateForm : Form
 {
  public UpdateForm()
  {
   InitializeComponent();
   UXThemeApi.UpdateControlTheme(this);
   updateListDownloader.RunWorkerAsync();
  }
  private void UpdateForm_FormClosing(object sender, FormClosingEventArgs e)
  {
   if (updateListDownloader.IsBusy || downloader.IsBusy || installer.IsBusy)
   {
    updateListDownloader.CancelAsync();
    downloader.CancelAsync();
    installer.CancelAsync();
    e.Cancel = true;
   }
  }
  private void cancelBtn_Click(object sender, EventArgs e)
  {
   Close();
  }
  private void updateListDownloader_DoWork(object sender, DoWorkEventArgs e)
  {
   try
   {
    updates.OnProgressEvent += updateListDownloader_ProgressChanged;
    updates.DownloadUpdateList();
   }
   finally
   {
    updates.OnProgressEvent -= updateListDownloader_ProgressChanged;
   }
  }
  private void updateListDownloader_ProgressChanged(object sender, ProgressEventArgs e)
  {
   if (InvokeRequired)
   {
    if (updateListDownloader.CancellationPending)
     throw new OperationCanceledException();
    Invoke(new EventHandler<ProgressEventArgs>(
     updateListDownloader_ProgressChanged), sender, e);
    return;
   }
   progressPb.Style = ProgressBarStyle.Continuous;
   progressPb.Value = (int)(e.OverallProgressPercentage * 100);
   progressProgressLbl.Text = e.Message;
   if (progressPb.Value == 100)
    progressProgressLbl.Text = S._("Processing update list...");
  }
  private void updateListDownloader_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
  {
   if (e.Error != null)
   {
    if (!(e.Error is OperationCanceledException))
     MessageBox.Show(this, e.Error.Message, S._("Eraser"), MessageBoxButtons.OK,
      MessageBoxIcon.Error, MessageBoxDefaultButton.Button1,
      S.IsRightToLeft(this) ? MessageBoxOptions.RtlReading : 0);
    Close();
    return;
   }
   progressPanel.Visible = false;
   updatesPanel.Visible = true;
   Dictionary<string, Mirror>.Enumerator iter = updates.Mirrors.GetEnumerator();
   while (iter.MoveNext())
    updatesMirrorCmb.Items.Add(iter.Current.Value);
   updatesMirrorCmb.SelectedIndex = 0;
   Dictionary<string, string> updateCategories = new Dictionary<string, string>();
   updateCategories.Add("update", S._("Updates"));
   updateCategories.Add("plugin", S._("Plugins"));
   List<string> compatibleArchs = new List<string>();
   {
    compatibleArchs.Add("any");
    switch (KernelApi.ProcessorArchitecture)
    {
     case ProcessorArchitecture.Amd64:
      compatibleArchs.Add("x64");
      break;
     case ProcessorArchitecture.IA64:
      compatibleArchs.Add("ia64");
      break;
     case ProcessorArchitecture.X86:
      compatibleArchs.Add("x86");
      break;
    }
   }
   foreach (string key in updates.Categories)
   {
    ListViewGroup group = new ListViewGroup(updateCategories.ContainsKey(key) ?
     updateCategories[key] : key);
    updatesLv.Groups.Add(group);
    foreach (UpdateInfo update in updates.Updates[key])
    {
     if (compatibleArchs.IndexOf(update.Architecture) == -1)
      continue;
     ListViewItem item = new ListViewItem(update.Name);
     item.SubItems.Add(update.Version.ToString());
     item.SubItems.Add(update.Publisher);
     item.SubItems.Add(Util.File.GetHumanReadableFilesize(update.FileSize));
     item.Tag = update;
     item.Group = group;
     item.Checked = true;
     updatesLv.Items.Add(item);
     uiUpdates.Add(update, new UpdateData(update, item));
    }
   }
   updatesBtn.Enabled = updatesLv.Items.Count > 0;
   if (updatesLv.Items.Count == 0)
   {
    MessageBox.Show(this, S._("There are no new updates or plugins available for " +
     "Eraser."), S._("Eraser"), MessageBoxButtons.OK, MessageBoxIcon.Information,
     MessageBoxDefaultButton.Button1,
     S.IsRightToLeft(this) ? MessageBoxOptions.RtlReading : 0);
    Close();
   }
  }
  private void updatesLv_ItemChecked(object sender, ItemCheckedEventArgs e)
  {
   if (selectedUpdates == -1 || updatesCount != updatesLv.Items.Count)
   {
    updatesCount = updatesLv.Items.Count;
    selectedUpdates = 0;
    foreach (ListViewItem item in updatesLv.Items)
     if (item.Checked)
      ++selectedUpdates;
   }
   else
    selectedUpdates += e.Item.Checked ? 1 : -1;
   updatesBtn.Text = selectedUpdates == 0 ? S._("Close") : S._("Install");
  }
  private void updatesBtn_Click(object sender, EventArgs e)
  {
   updatesPanel.Visible = false;
   downloadingPnl.Visible = true;
   List<UpdateInfo> updatesToInstall = new List<UpdateInfo>();
   updates.SelectedMirror = (Mirror)updatesMirrorCmb.SelectedItem;
   foreach (ListViewItem item in updatesLv.Items)
    if (item.Checked)
    {
     item.Remove();
     item.SubItems.RemoveAt(1);
     item.SubItems.RemoveAt(1);
     downloadingLv.Items.Add(item);
     updatesToInstall.Add((UpdateInfo)item.Tag);
    }
    else
     uiUpdates.Remove((UpdateInfo)item.Tag);
   if (updatesToInstall.Count > 0)
    downloader.RunWorkerAsync(updatesToInstall);
   else
    Close();
  }
  private void downloader_DoWork(object sender, DoWorkEventArgs e)
  {
   try
   {
    updates.OnProgressEvent += downloader_ProgressChanged;
    object downloadedUpdates = updates.DownloadUpdates((List<UpdateInfo>)e.Argument);
    e.Result = downloadedUpdates;
   }
   finally
   {
    updates.OnProgressEvent -= downloader_ProgressChanged;
   }
  }
  private void downloader_ProgressChanged(object sender, ProgressEventArgs e)
  {
   if (InvokeRequired)
   {
    if (updateListDownloader.CancellationPending)
     throw new OperationCanceledException();
    Invoke(new EventHandler<ProgressEventArgs>(downloader_ProgressChanged),
     sender, e);
    return;
   }
   UpdateData update = uiUpdates[(UpdateInfo)e.UserState];
   if (e is ProgressErrorEventArgs)
   {
    update.Error = ((ProgressErrorEventArgs)e).Exception;
    update.LVItem.ImageIndex = 3;
    update.LVItem.SubItems[1].Text = S._("Error");
    update.LVItem.ToolTipText = update.Error.Message;
   }
   else
   {
    if (e.ProgressPercentage >= 1.0f)
    {
     update.LVItem.ImageIndex = -1;
     update.LVItem.SubItems[1].Text = S._("Downloaded");
    }
    else
    {
     update.amountDownloaded = (long)(e.ProgressPercentage * update.Update.FileSize);
     update.LVItem.ImageIndex = 0;
     update.LVItem.SubItems[1].Text = Util.File.GetHumanReadableFilesize(
      update.Update.FileSize - update.amountDownloaded);
    }
   }
   downloadingItemLbl.Text = e.Message;
   downloadingItemPb.Value = (int)(e.ProgressPercentage * 100);
   downloadingOverallPb.Value = (int)(e.OverallProgressPercentage * 100);
   long amountToDownload = 0;
   foreach (UpdateData upd in uiUpdates.Values)
    amountToDownload += upd.Update.FileSize - upd.amountDownloaded;
   downloadingOverallLbl.Text = S._("Overall progress: {0} left",
    Util.File.GetHumanReadableFilesize(amountToDownload));
  }
  private void downloader_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
  {
   if (e.Error != null)
   {
    if (!(e.Error is OperationCanceledException))
     MessageBox.Show(this, e.Error.Message, S._("Eraser"),
      MessageBoxButtons.OK, MessageBoxIcon.Error,
      MessageBoxDefaultButton.Button1,
      S.IsRightToLeft(this) ? MessageBoxOptions.RtlReading : 0);
    Close();
    return;
   }
   downloadingPnl.Visible = false;
   installingPnl.Visible = true;
   foreach (ListViewItem item in downloadingLv.Items)
   {
    item.Remove();
    installingLv.Items.Add(item);
    UpdateData update = uiUpdates[(UpdateInfo)item.Tag];
    if (update.Error == null)
     item.SubItems[1].Text = string.Empty;
    else
     item.SubItems[1].Text = S._("Error: {0}", update.Error.Message);
   }
   installer.RunWorkerAsync(e.Result);
  }
  private void installer_DoWork(object sender, DoWorkEventArgs e)
  {
   try
   {
    updates.OnProgressEvent += installer_ProgressChanged;
    updates.InstallUpdates(e.Argument);
   }
   finally
   {
    updates.OnProgressEvent -= installer_ProgressChanged;
   }
  }
  private void installer_ProgressChanged(object sender, ProgressChangedEventArgs e)
  {
   if (InvokeRequired)
   {
    if (updateListDownloader.CancellationPending)
     throw new OperationCanceledException();
    Invoke(new EventHandler<ProgressEventArgs>(installer_ProgressChanged),
     sender, e);
    return;
   }
   UpdateData update = uiUpdates[(UpdateInfo)e.UserState];
   if (e is ProgressErrorEventArgs)
   {
    update.Error = ((ProgressErrorEventArgs)e).Exception;
    update.LVItem.ImageIndex = 3;
    update.LVItem.SubItems[1].Text = S._("Error: {0}", update.Error.Message);
   }
   else
    switch (update.LVItem.ImageIndex)
    {
     case -1:
      update.LVItem.ImageIndex = 1;
      break;
     case 1:
      update.LVItem.ImageIndex = 2;
      break;
    }
  }
  private void installer_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
  {
   if (e.Error is OperationCanceledException)
    Close();
   installingPnl.UseWaitCursor = false;
  }
  UpdateManager updates = new UpdateManager();
  Dictionary<UpdateInfo, UpdateData> uiUpdates = new Dictionary<UpdateInfo, UpdateData>();
  private class UpdateData
  {
   public UpdateData(UpdateInfo update, ListViewItem item)
   {
    Update = update;
    LVItem = item;
   }
   public UpdateInfo Update;
   public ListViewItem LVItem;
   public long amountDownloaded;
   public Exception Error;
  }
  private int selectedUpdates = -1;
  private int updatesCount = -1;
 }
 public class UpdateManager
 {
  public UpdateManager()
  {
   Updates = new UpdateCategoriesDictionary();
  }
  public void DownloadUpdateList()
  {
   WebRequest.DefaultCachePolicy = new HttpRequestCachePolicy(
    HttpRequestCacheLevel.Refresh);
   HttpWebRequest req = (HttpWebRequest)
    WebRequest.Create(new Uri("http://eraser.heidi.ie/scripts/updates?" +
     "action=listupdates&version=" +
     Assembly.GetExecutingAssembly().GetName().Version.ToString()));
   using (WebResponse response = req.GetResponse())
   using (Stream responseStream = response.GetResponseStream())
   using (MemoryStream memoryStream = new MemoryStream())
   {
    Manager.ProgressManager progress = new Manager.ProgressManager();
    progress.Total = response.ContentLength;
    int lastRead = 0;
    byte[] buffer = new byte[16384];
    while ((lastRead = responseStream.Read(buffer, 0, buffer.Length)) != 0)
    {
     memoryStream.Write(buffer, 0, lastRead);
     progress.Completed = memoryStream.Position;
     OnProgress(new ProgressEventArgs(progress.Progress, progress.Progress, null,
      S._("{0} of {1} downloaded",
       Util.File.GetHumanReadableFilesize(progress.Completed),
       Util.File.GetHumanReadableFilesize(progress.Total))));
    }
    memoryStream.Position = 0;
    ParseUpdateList(memoryStream);
   }
  }
  private void ParseUpdateList(Stream strm)
  {
   Updates.Clear();
   mirrors.Clear();
   XmlReader rdr = XmlReader.Create(strm);
   rdr.ReadToFollowing("updateList");
   XmlReader categories = rdr.ReadSubtree();
   bool cont = categories.ReadToDescendant("mirrors");
   while (cont)
   {
    if (categories.NodeType == XmlNodeType.Element)
    {
     if (categories.Name == "mirrors")
     {
      Dictionary<string, string> mirrorsList =
       ParseMirror(categories.ReadSubtree());
      Dictionary<string, string>.Enumerator e = mirrorsList.GetEnumerator();
      while (e.MoveNext())
       this.mirrors.Add(e.Current.Key,
        new Mirror(e.Current.Value, e.Current.Key));
     }
     else
      Updates.Add(categories.Name, ParseUpdateCategory(categories.ReadSubtree()));
    }
    cont = categories.Read();
   }
  }
  private static Dictionary<string, string> ParseMirror(XmlReader rdr)
  {
   Dictionary<string, string> result = new Dictionary<string,string>();
   if (!rdr.ReadToDescendant("mirror"))
    return result;
   do
   {
    if (rdr.NodeType != XmlNodeType.Element || rdr.Name != "mirror")
     continue;
    string location = rdr.GetAttribute("location");
    result.Add(rdr.ReadElementContentAsString(), location);
   }
   while (rdr.ReadToNextSibling("mirror"));
   return result;
  }
  private static UpdateCollection ParseUpdateCategory(XmlReader rdr)
  {
   UpdateCollection result = new UpdateCollection();
   if (!rdr.ReadToDescendant("item"))
    return result;
   do
   {
    if (rdr.Name != "item")
     continue;
    UpdateInfo update = new UpdateInfo();
    update.Name = rdr.GetAttribute("name");
    update.Version = new Version(rdr.GetAttribute("version"));
    update.Publisher = rdr.GetAttribute("publisher");
    update.Architecture = rdr.GetAttribute("architecture");
    update.FileSize = Convert.ToInt64(rdr.GetAttribute("filesize"),
     CultureInfo.InvariantCulture);
    update.Link = rdr.ReadElementContentAsString();
    result.Add(update);
   }
   while (rdr.ReadToNextSibling("item"));
   return result;
  }
  public object DownloadUpdates(ICollection<UpdateInfo> downloadQueue)
  {
   DirectoryInfo tempDir = new DirectoryInfo(Path.GetTempPath());
   tempDir = tempDir.CreateSubdirectory("eraser" + Environment.TickCount.ToString(
    CultureInfo.InvariantCulture));
   int currUpdate = 0;
   Dictionary<string, UpdateInfo> tempFilesMap = new Dictionary<string, UpdateInfo>();
   Manager.SteppedProgressManager progress = new Manager.SteppedProgressManager();
   foreach (UpdateInfo update in downloadQueue)
   {
    try
    {
     Manager.ProgressManager step = new Eraser.Manager.ProgressManager();
     progress.Steps.Add(new Manager.SteppedProgressManager.Step(
      step, 1.0f / downloadQueue.Count));
     Uri reqUri = null;
     if (Uri.IsWellFormedUriString(update.Link, UriKind.Absolute))
      reqUri = new Uri(update.Link);
     else
      reqUri = new Uri(new Uri(SelectedMirror.Link), new Uri(update.Link));
     HttpWebRequest req = (HttpWebRequest)WebRequest.Create(reqUri);
     using (WebResponse resp = req.GetResponse())
     {
      ContentDisposition contentDisposition = null;
      foreach (string header in resp.Headers.AllKeys)
       if (header.ToLowerInvariant() == "content-disposition")
        contentDisposition = new ContentDisposition(resp.Headers[header]);
      string tempFilePath = Path.Combine(
       tempDir.FullName, string.Format(CultureInfo.InvariantCulture, "{0}-{1}",
       ++currUpdate,
       contentDisposition == null ?
        Path.GetFileName(reqUri.GetComponents(UriComponents.Path,
        UriFormat.Unescaped)) : contentDisposition.FileName));
      using (Stream responseStream = resp.GetResponseStream())
      using (FileStream fileStream = new FileStream(tempFilePath, FileMode.CreateNew))
      {
       step.Total = resp.ContentLength;
       int lastRead = 0;
       byte[] buffer = new byte[16384];
       while ((lastRead = responseStream.Read(buffer, 0, buffer.Length)) != 0)
       {
        fileStream.Write(buffer, 0, lastRead);
        step.Completed = fileStream.Position;
        OnProgress(new ProgressEventArgs(step.Progress, progress.Progress,
         update, S._("Downloading: {0}", update.Name)));
       }
      }
      tempFilesMap.Add(tempFilePath, update);
      step.Completed = step.Total;
      OnProgress(new ProgressEventArgs(step.Progress, progress.Progress,
       update, S._("Downloaded: {0}", update.Name)));
     }
    }
    catch (Exception e)
    {
     OnProgress(new ProgressErrorEventArgs(new ProgressEventArgs(1.0f,
      (float)currUpdate / downloadQueue.Count, update,
       S._("Error downloading {0}: {1}", update.Name, e.Message)),
      e));
    }
   }
   return tempFilesMap;
  }
  public void InstallUpdates(object value)
  {
   Manager.ProgressManager progress = new Manager.ProgressManager();
   Dictionary<string, UpdateInfo> tempFiles = (Dictionary<string, UpdateInfo>)value;
   Dictionary<string, UpdateInfo>.KeyCollection files = tempFiles.Keys;
   try
   {
    progress.Total = files.Count;
    foreach (string path in files)
    {
     UpdateInfo item = tempFiles[path];
     ++progress.Completed;
     OnProgress(new ProgressEventArgs(0.0f, progress.Progress,
      item, S._("Installing {0}", item.Name)));
     System.Diagnostics.ProcessStartInfo info = new System.Diagnostics.ProcessStartInfo();
     info.FileName = path;
     info.UseShellExecute = true;
     System.Diagnostics.Process process = System.Diagnostics.Process.Start(info);
     process.WaitForExit(Int32.MaxValue);
     if (process.ExitCode == 0)
      OnProgress(new ProgressEventArgs(1.0f, progress.Progress,
       item, S._("Installed {0}", item.Name)));
     else
      OnProgress(new ProgressErrorEventArgs(new ProgressEventArgs(1.0f,
       progress.Progress, item, S._("Error installing {0}", item.Name)),
       new ApplicationException(S._("The installer exited with an error code {0}",
        process.ExitCode))));
    }
   }
   finally
   {
    foreach (string file in files)
    {
     DirectoryInfo tempDir = null;
     {
      FileInfo info = new FileInfo(file);
      tempDir = info.Directory;
     }
     tempDir.Delete(true);
     break;
    }
   }
  }
  public EventHandler<ProgressEventArgs> OnProgressEvent { get; set; }
  private void OnProgress(ProgressEventArgs arg)
  {
   if (OnProgressEvent != null)
    OnProgressEvent(this, arg);
  }
  public Dictionary<string, Mirror> Mirrors
  {
   get
   {
    return mirrors;
   }
  }
  public Mirror SelectedMirror
  {
   get
   {
    if (selectedMirror.Link.Length == 0)
    {
     Dictionary<string, Mirror>.Enumerator iter = mirrors.GetEnumerator();
     if (iter.MoveNext())
      return iter.Current.Value;
    }
    return selectedMirror;
   }
   set
   {
    foreach (Mirror mirror in Mirrors.Values)
     if (mirror.Equals(value))
     {
      selectedMirror = value;
      return;
     }
    throw new ArgumentException(S._("Unknown mirror selected."));
   }
  }
  public ICollection<string> Categories
  {
   get
   {
    return Updates.Keys;
   }
  }
  public UpdateCategoriesDictionary Updates { get; private set; }
  private Dictionary<string, Mirror> mirrors =
   new Dictionary<string, Mirror>();
  private Mirror selectedMirror;
 }
 public class UpdateCategoriesDictionary : IDictionary<string, UpdateCollection>,
  ICollection<KeyValuePair<string, UpdateCollection> >,
  IEnumerable<KeyValuePair<string, UpdateCollection> >
 {
  public void Add(string key, UpdateCollection value)
  {
   dictionary.Add(key, value);
  }
  public bool ContainsKey(string key)
  {
   return dictionary.ContainsKey(key);
  }
  public ICollection<string> Keys
  {
   get { return dictionary.Keys; }
  }
  public bool Remove(string key)
  {
   return dictionary.Remove(key);
  }
  public bool TryGetValue(string key, out UpdateCollection value)
  {
   return dictionary.TryGetValue(key, out value);
  }
  public ICollection<UpdateCollection> Values
  {
   get { return dictionary.Values; }
  }
  public UpdateCollection this[string key]
  {
   get
   {
    return dictionary[key];
   }
   set
   {
    dictionary[key] = value;
   }
  }
  public void Add(KeyValuePair<string, UpdateCollection> item)
  {
   dictionary.Add(item.Key, item.Value);
  }
  public void Clear()
  {
   dictionary.Clear();
  }
  public bool Contains(KeyValuePair<string, UpdateCollection> item)
  {
   return dictionary.ContainsKey(item.Key) && dictionary[item.Key] == item.Value;
  }
  public void CopyTo(KeyValuePair<string, UpdateCollection>[] array, int arrayIndex)
  {
   throw new NotImplementedException();
  }
  public int Count
  {
   get { return dictionary.Count; }
  }
  public bool IsReadOnly
  {
   get { return true; }
  }
  public bool Remove(KeyValuePair<string, UpdateCollection> item)
  {
   return dictionary.Remove(item.Key);
  }
  public IEnumerator<KeyValuePair<string, UpdateCollection> > GetEnumerator()
  {
   return dictionary.GetEnumerator();
  }
  System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
  {
   return GetEnumerator();
  }
  private Dictionary<string, UpdateCollection> dictionary =
   new Dictionary<string, UpdateCollection>();
 }
 public class UpdateCollection : IList<UpdateInfo>, ICollection<UpdateInfo>,
  IEnumerable<UpdateInfo>
 {
  public int IndexOf(UpdateInfo item)
  {
   return list.IndexOf(item);
  }
  public void Insert(int index, UpdateInfo item)
  {
   list.Insert(index, item);
  }
  public void RemoveAt(int index)
  {
   list.RemoveAt(index);
  }
  public UpdateInfo this[int index]
  {
   get
   {
    return list[index];
   }
   set
   {
    list[index] = value;
   }
  }
  public void Add(UpdateInfo item)
  {
   list.Add(item);
  }
  public void Clear()
  {
   list.Clear();
  }
  public bool Contains(UpdateInfo item)
  {
   return list.Contains(item);
  }
  public void CopyTo(UpdateInfo[] array, int arrayIndex)
  {
   list.CopyTo(array, arrayIndex);
  }
  public int Count
  {
   get { return list.Count; }
  }
  public bool IsReadOnly
  {
   get { return true; }
  }
  public bool Remove(UpdateInfo item)
  {
   return list.Remove(item);
  }
  public IEnumerator<UpdateInfo> GetEnumerator()
  {
   return list.GetEnumerator();
  }
  System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
  {
   return list.GetEnumerator();
  }
  private List<UpdateInfo> list = new List<UpdateInfo>();
 }
 public struct Mirror
 {
  public Mirror(string location, string link)
   : this()
  {
   Location = location;
   Link = link;
  }
  public string Location { get; set; }
  public string Link { get; set; }
  public override string ToString()
  {
   return Location;
  }
  public override bool Equals(object obj)
  {
   if (!(obj is Mirror))
    return false;
   return Equals((Mirror)obj);
  }
  public bool Equals(Mirror other)
  {
   return Link == other.Link;
  }
  public static bool operator ==(Mirror mirror1, Mirror mirror2)
  {
   return mirror1.Equals(mirror2);
  }
  public static bool operator !=(Mirror mirror1, Mirror mirror2)
  {
   return !mirror1.Equals(mirror2);
  }
  public override int GetHashCode()
  {
   return Link.GetHashCode();
  }
 }
 public struct UpdateInfo
 {
  public string Name { get; set; }
  public Version Version { get; set; }
  public string Publisher { get; set; }
  public string Architecture { get; set; }
  public long FileSize { get; set; }
  public string Link { get; set; }
  public override bool Equals(object obj)
  {
   if (!(obj is UpdateInfo))
    return false;
   return Equals((UpdateInfo)obj);
  }
  public bool Equals(UpdateInfo other)
  {
   return Link == other.Link;
  }
  public static bool operator ==(UpdateInfo update1, UpdateInfo update2)
  {
   return update1.Equals(update2);
  }
  public static bool operator !=(UpdateInfo update1, UpdateInfo update2)
  {
   return !update1.Equals(update2);
  }
  public override int GetHashCode()
  {
   return Link.GetHashCode();
  }
 }
 public class ProgressEventArgs : ProgressChangedEventArgs
 {
  public ProgressEventArgs(float progressPercentage, float overallPercentage,
   object userState, string message)
   : base((int)(progressPercentage * 100), userState)
  {
   ProgressPercentage = progressPercentage;
   OverallProgressPercentage = overallPercentage;
   Message = message;
  }
  public new float ProgressPercentage { get; private set; }
  public float OverallProgressPercentage { get; private set; }
  public string Message { get; private set; }
 }
 public class ProgressErrorEventArgs : ProgressEventArgs
 {
  public ProgressErrorEventArgs(ProgressEventArgs e, Exception ex)
   : base(e.ProgressPercentage, e.OverallProgressPercentage, e.UserState, e.Message)
  {
   Exception = ex;
  }
  public Exception Exception { get; private set; }
 }
}
