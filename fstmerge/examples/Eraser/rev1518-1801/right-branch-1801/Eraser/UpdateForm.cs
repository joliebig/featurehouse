using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Diagnostics;
using System.Reflection;
using System.IO;
using System.Linq;
using System.Xml;
using System.Net;
using System.Net.Cache;
using System.Net.Mime;
using System.Globalization;
using Eraser.Util;
using DoWorkEventArgs = System.ComponentModel.DoWorkEventArgs;
using RunWorkerCompletedEventArgs = System.ComponentModel.RunWorkerCompletedEventArgs;
namespace Eraser
{
 public partial class UpdateForm : Form
 {
  public UpdateForm()
  {
   InitializeComponent();
   Theming.ApplyTheme(this);
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
   e.Result = DownloadManager.GetDownloads(updateListDownloader_ProgressChanged);
  }
  private void updateListDownloader_ProgressChanged(object sender, ProgressChangedEventArgs e)
  {
   if (InvokeRequired)
   {
    if (updateListDownloader.CancellationPending)
     throw new OperationCanceledException();
    Invoke((EventHandler<ProgressChangedEventArgs>)updateListDownloader_ProgressChanged,
     sender, e);
    return;
   }
   progressPb.Style = ProgressBarStyle.Continuous;
   progressPb.Value = (int)(e.Progress.Progress * 100);
   progressProgressLbl.Text = e.UserState as string;
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
      Localisation.IsRightToLeft(this) ?
       MessageBoxOptions.RtlReading | MessageBoxOptions.RightAlign : 0);
    Close();
    return;
   }
   progressPanel.Visible = false;
   updatesPanel.Visible = true;
   IList<DownloadInfo> downloads = (IList<DownloadInfo>)e.Result;
   updatesLv.Groups.Add(DownloadType.Update.ToString(), S._("Updates"));
   updatesLv.Groups.Add(DownloadType.Plugin.ToString(), S._("Plugins"));
   updatesLv.Groups.Add(DownloadType.Build.ToString(), S._("Nightly builds"));
   List<string> architectures = new List<string>();
   {
    architectures.Add("any");
    switch (SystemInfo.ProcessorArchitecture)
    {
     case ProcessorArchitecture.Amd64:
      architectures.Add("x64");
      break;
     case ProcessorArchitecture.IA64:
      architectures.Add("ia64");
      break;
     case ProcessorArchitecture.X86:
      architectures.Add("x86");
      break;
    }
   }
   foreach (DownloadInfo download in downloads)
   {
    if (architectures.IndexOf(download.Architecture) == -1)
     continue;
    ListViewGroup group = updatesLv.Groups[download.Type.ToString()];
    ListViewItem item = new ListViewItem(download.Name);
    item.SubItems.Add(download.Version.ToString());
    item.SubItems.Add(download.Publisher);
    item.SubItems.Add(FileSize.ToString(download.FileSize));
    item.Tag = download;
    item.Group = group;
    item.Checked = true;
    updatesLv.Items.Add(item);
   }
   updatesBtn.Enabled = updatesLv.Items.Count > 0;
   if (updatesLv.Items.Count == 0)
   {
    MessageBox.Show(this, S._("There are no new updates or plugins available for " +
     "Eraser."), S._("Eraser"), MessageBoxButtons.OK, MessageBoxIcon.Information,
     MessageBoxDefaultButton.Button1,
     Localisation.IsRightToLeft(this) ?
      MessageBoxOptions.RtlReading | MessageBoxOptions.RightAlign : 0);
    Close();
   }
  }
  private void updatesLv_ItemChecked(object sender, ItemCheckedEventArgs e)
  {
   updatesBtn.Text = updatesLv.CheckedIndices.Count == 0 ? S._("Close") : S._("Install");
  }
  private void updatesBtn_Click(object sender, EventArgs e)
  {
   updatesPanel.Visible = false;
   downloadingPnl.Visible = true;
   List<DownloadInfo> updatesToInstall = new List<DownloadInfo>();
   foreach (ListViewItem item in updatesLv.CheckedItems)
   {
    item.Remove();
    item.SubItems.RemoveAt(1);
    item.SubItems.RemoveAt(1);
    downloadingLv.Items.Add(item);
    DownloadInfo download = (DownloadInfo)item.Tag;
    updatesToInstall.Add(download);
    DownloadItems.Add(download, new DownloadUIInfo(download, item));
   }
   if (updatesToInstall.Count > 0)
    downloader.RunWorkerAsync(updatesToInstall);
   else
    Close();
  }
  private void downloader_DoWork(object sender, DoWorkEventArgs e)
  {
   List<DownloadInfo> downloads = (List<DownloadInfo>)e.Argument;
   SteppedProgressManager overallProgress = new SteppedProgressManager();
   long totalDownloadSize = downloads.Sum(delegate(DownloadInfo download)
    {
     return download.FileSize;
    });
   foreach (DownloadInfo download in downloads)
   {
    ProgressManagerBase downloadProgress = null;
    ProgressChangedEventHandler localHandler =
     delegate(object sender2, ProgressChangedEventArgs e2)
     {
      DownloadInfo downloadInfo = (DownloadInfo)sender2;
      if (downloadProgress == null)
      {
       downloadProgress = e2.Progress;
       overallProgress.Steps.Add(new SteppedProgressManagerStep(
        e2.Progress, download.FileSize / (float)totalDownloadSize));
      }
      downloader_ProgressChanged(sender2,
       new ProgressChangedEventArgs(overallProgress, e2.UserState));
     };
    download.Download(localHandler);
   }
   e.Result = e.Argument;
  }
  private void downloader_ProgressChanged(object sender, ProgressChangedEventArgs e)
  {
   if (InvokeRequired)
   {
    if (updateListDownloader.CancellationPending)
     throw new OperationCanceledException();
    Invoke((EventHandler<ProgressChangedEventArgs>)downloader_ProgressChanged,
     sender, e);
    return;
   }
   DownloadInfo download = (DownloadInfo)sender;
   DownloadUIInfo downloadUIInfo = DownloadItems[download];
   SteppedProgressManager overallProgress = (SteppedProgressManager)e.Progress;
   if (e.UserState is Exception)
   {
    downloadUIInfo.ListViewItem.ImageIndex = 3;
    downloadUIInfo.ListViewItem.SubItems[1].Text = S._("Error");
    downloadUIInfo.ListViewItem.ToolTipText = ((Exception)e.UserState).Message;
   }
   else
   {
    if (overallProgress.CurrentStep.Progress.Progress >= 1.0f)
    {
     downloadUIInfo.ListViewItem.ImageIndex = -1;
     downloadUIInfo.ListViewItem.SubItems[1].Text = S._("Downloaded");
    }
    else
    {
     downloadUIInfo.Downloaded = (long)
      (overallProgress.CurrentStep.Progress.Progress * download.FileSize);
     downloadUIInfo.ListViewItem.ImageIndex = 0;
     downloadUIInfo.ListViewItem.SubItems[1].Text = FileSize.ToString(download.FileSize -
      downloadUIInfo.Downloaded);
    }
   }
   downloadingItemLbl.Text = S._("Downloading: {0}", download.Name);
   downloadingItemPb.Value = (int)(overallProgress.CurrentStep.Progress.Progress * 100);
   downloadingOverallPb.Value = (int)(overallProgress.Progress * 100);
   downloadingOverallLbl.Text = S._("Overall progress: {0} left",
    FileSize.ToString(DownloadItems.Values.Sum(delegate(DownloadUIInfo item)
     {
      return item.Download.FileSize - item.Downloaded;
     }
   )));
  }
  private void downloader_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
  {
   if (e.Error != null)
   {
    if (!(e.Error is OperationCanceledException))
     MessageBox.Show(this, e.Error.Message, S._("Eraser"),
      MessageBoxButtons.OK, MessageBoxIcon.Error,
      MessageBoxDefaultButton.Button1,
      Localisation.IsRightToLeft(this) ?
       MessageBoxOptions.RtlReading | MessageBoxOptions.RightAlign : 0);
    Close();
    return;
   }
   downloadingPnl.Visible = false;
   installingPnl.Visible = true;
   foreach (DownloadUIInfo download in DownloadItems.Values)
   {
    download.ListViewItem.Remove();
    installingLv.Items.Add(download.ListViewItem);
    if (download.Error == null)
     download.ListViewItem.SubItems[1].Text = string.Empty;
    else
     download.ListViewItem.SubItems[1].Text = S._("Error: {0}", download.Error.Message);
   }
   installer.RunWorkerAsync(e.Result);
  }
  private void installer_DoWork(object sender, DoWorkEventArgs e)
  {
   List<DownloadInfo> downloads = (List<DownloadInfo>)e.Argument;
   ProgressManager progress = new ProgressManager();
   progress.Total = downloads.Count;
   foreach (DownloadInfo download in downloads)
   {
    ++progress.Completed;
    try
    {
     installer_ProgressChanged(download,
      new ProgressChangedEventArgs(progress, null));
     download.Install();
     installer_ProgressChanged(download,
      new ProgressChangedEventArgs(progress, null));
    }
    catch (Exception ex)
    {
     installer_ProgressChanged(download,
      new ProgressChangedEventArgs(progress, ex));
    }
   }
   e.Result = e.Argument;
  }
  private void installer_ProgressChanged(object sender, ProgressChangedEventArgs e)
  {
   if (InvokeRequired)
   {
    if (updateListDownloader.CancellationPending)
     throw new OperationCanceledException();
    Invoke((EventHandler<ProgressChangedEventArgs>)installer_ProgressChanged,
     sender, e);
    return;
   }
   DownloadInfo download = (DownloadInfo)sender;
   DownloadUIInfo downloadUIInfo = DownloadItems[download];
   if (e.UserState is Exception)
   {
    downloadUIInfo.Error = (Exception)e.UserState;
    downloadUIInfo.ListViewItem.ImageIndex = 3;
    downloadUIInfo.ListViewItem.SubItems[1].Text =
     S._("Error: {0}", downloadUIInfo.Error.Message);
   }
   else
   {
    switch (downloadUIInfo.ListViewItem.ImageIndex)
    {
     case -1:
      downloadUIInfo.ListViewItem.SubItems[1].Text =
       S._("Installing {0}", download.Name);
      downloadUIInfo.ListViewItem.ImageIndex = 1;
      break;
     case 1:
      downloadUIInfo.ListViewItem.SubItems[1].Text =
       S._("Installed {0}", download.Name);
      downloadUIInfo.ListViewItem.ImageIndex = 2;
      break;
    }
   }
  }
  private void installer_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
  {
   if (e.Error is OperationCanceledException)
    Close();
   installingPnl.UseWaitCursor = false;
  }
  private class DownloadUIInfo
  {
   public DownloadUIInfo(DownloadInfo download, ListViewItem item)
   {
    Download = download;
    ListViewItem = item;
   }
   public DownloadInfo Download { get; private set; }
   public ListViewItem ListViewItem { get; private set; }
   public long Downloaded { get; set; }
   public Exception Error { get; set; }
  }
  private Dictionary<DownloadInfo, DownloadUIInfo> DownloadItems =
   new Dictionary<DownloadInfo, DownloadUIInfo>();
 }
 public static class DownloadManager
 {
  public static IList<DownloadInfo> GetDownloads(Eraser.Util.ProgressChangedEventHandler handler)
  {
   WebRequest.DefaultCachePolicy = new HttpRequestCachePolicy(
    HttpRequestCacheLevel.Revalidate);
   HttpWebRequest request = (HttpWebRequest)WebRequest.Create(
    new Uri("http://eraser.heidi.ie/scripts/updates?action=listupdates&version=6.1.0.0" ));
   using (HttpWebResponse response = (HttpWebResponse)request.GetResponse())
   using (Stream responseStream = response.GetResponseStream())
   using (MemoryStream memoryStream = new MemoryStream())
   {
    Util.ProgressManager progress = new Util.ProgressManager();
    progress.Total = response.ContentLength;
    int lastRead = 0;
    byte[] buffer = new byte[16384];
    while ((lastRead = responseStream.Read(buffer, 0, buffer.Length)) != 0)
    {
     memoryStream.Write(buffer, 0, lastRead);
     progress.Completed = memoryStream.Position;
     if (handler != null)
      handler(null, new Eraser.Util.ProgressChangedEventArgs(progress,
       S._("{0} of {1} downloaded", FileSize.ToString(progress.Completed),
        FileSize.ToString(progress.Total))));
    }
    memoryStream.Position = 0;
    return ParseDownloadList(memoryStream).AsReadOnly();
   }
  }
  private static List<DownloadInfo> ParseDownloadList(Stream strm)
  {
   XmlReader reader = XmlReader.Create(strm);
   reader.ReadToFollowing("updateList");
   bool cont = reader.Read();
   while (reader.NodeType != XmlNodeType.Element)
    cont = reader.Read();
   if (reader.NodeType != XmlNodeType.Element)
    return new List<DownloadInfo>();
   List<DownloadInfo> result = new List<DownloadInfo>();
   do
   {
    if (reader.NodeType == XmlNodeType.Element)
    {
     result.AddRange(ParseDownloadCategory(reader.Name, reader.ReadSubtree()));
    }
    cont = reader.Read();
   }
   while (cont);
   return result;
  }
  private static List<DownloadInfo> ParseDownloadCategory(string category, XmlReader rdr)
  {
   List<DownloadInfo> result = new List<DownloadInfo>();
   if (!rdr.ReadToDescendant("item"))
    return result;
   do
   {
    if (rdr.Name != "item")
     continue;
    result.Add(new DownloadInfo(rdr.GetAttribute("name"),
     (DownloadType)Enum.Parse(typeof(DownloadType), category, true),
     new Version(rdr.GetAttribute("version")), rdr.GetAttribute("publisher"),
     rdr.GetAttribute("architecture"), Convert.ToInt64(rdr.GetAttribute("filesize")),
     new Uri(rdr.ReadElementContentAsString())));
   }
   while (rdr.ReadToNextSibling("item"));
   return result;
  }
 }
 public enum DownloadType
 {
  Unknown,
  Update,
  Plugin,
  Build
 }
 public class DownloadInfo
 {
  internal DownloadInfo(string name, DownloadType type, Version version,
   string publisher, string architecture, long fileSize, Uri link)
  {
   Name = name;
   Type = type;
   Version = version;
   Publisher = publisher;
   Architecture = architecture;
   FileSize = fileSize;
   Link = link;
  }
  public string Name { get; private set; }
  public DownloadType Type { get; private set; }
  public Version Version { get; private set; }
  public string Publisher { get; private set; }
  public string Architecture { get; private set; }
  public long FileSize { get; private set; }
  public Uri Link { get; private set; }
  public void Download(Eraser.Util.ProgressChangedEventHandler handler)
  {
   if (DownloadedFile != null && DownloadedFile.Length > 0)
    throw new InvalidOperationException("The Download method cannot be called " +
     "before the Download method has been called.");
   lock (TempPathLock)
   {
    if (TempPath == null)
    {
     TempPath = new DirectoryInfo(Path.GetTempPath());
     TempPath = TempPath.CreateSubdirectory("eraser" + Environment.TickCount.ToString(
      CultureInfo.InvariantCulture));
    }
   }
   ProgressManager progress = new ProgressManager();
   try
   {
    HttpWebRequest request = (HttpWebRequest)WebRequest.Create(Link);
    using (HttpWebResponse response = (HttpWebResponse)request.GetResponse())
    {
     progress.Total = response.ContentLength;
     ContentDisposition contentDisposition = null;
     foreach (string header in response.Headers.AllKeys)
      if (header.ToUpperInvariant() == "CONTENT-DISPOSITION")
       contentDisposition = new ContentDisposition(response.Headers[header]);
     DownloadedFile = new FileInfo(Path.Combine(
      TempPath.FullName, string.Format(CultureInfo.InvariantCulture,
       "{0:00}-{1}", ++DownloadFileIndex, contentDisposition == null ?
        Path.GetFileName(Link.GetComponents(UriComponents.Path, UriFormat.Unescaped)) :
        contentDisposition.FileName)));
     using (Stream responseStream = response.GetResponseStream())
     using (FileStream fileStream = DownloadedFile.OpenWrite())
     {
      int lastRead = 0;
      byte[] buffer = new byte[16384];
      while ((lastRead = responseStream.Read(buffer, 0, buffer.Length)) != 0)
      {
       fileStream.Write(buffer, 0, lastRead);
       progress.Completed = fileStream.Position;
       if (handler != null)
        handler(this, new ProgressChangedEventArgs(progress, null));
      }
     }
     progress.MarkComplete();
     if (handler != null)
      handler(this, new ProgressChangedEventArgs(progress, null));
    }
   }
   catch (Exception e)
   {
    if (handler != null)
     handler(this, new ProgressChangedEventArgs(progress, e));
   }
  }
  public void Install()
  {
   if (DownloadedFile == null || !DownloadedFile.Exists || DownloadedFile.Length == 0)
    throw new InvalidOperationException("The Install method cannot be called " +
     "before the Download method has been called.");
   ProcessStartInfo info = new ProcessStartInfo();
   info.FileName = DownloadedFile.FullName;
   info.UseShellExecute = true;
   Process process = Process.Start(info);
   process.WaitForExit(Int32.MaxValue);
  }
  private static object TempPathLock = new object();
  private static DirectoryInfo TempPath;
  private static int DownloadFileIndex;
  private FileInfo DownloadedFile;
 }
}
