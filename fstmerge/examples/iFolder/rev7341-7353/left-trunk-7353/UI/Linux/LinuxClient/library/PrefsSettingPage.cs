using System;
using System.IO;
using System.Collections;
using Gtk;
using Simias.Client.Event;
using Simias.Client;
using Simias.Client.Authentication;
using Novell.iFolder.Events;
using Novell.iFolder.Controller;
using Novell.iFolder.DomainProvider;
namespace Novell.iFolder
{
 public class PrefsSettingPage: VBox
 {
  private Gtk.CheckButton NotifyCheckButton;
  private Gtk.TreeView PolicyTreeView;
  private Gtk.ListStore PolicyTreeStore;
  private string[] policyVoilation = {
                        "When quota policy is violated",
                        "When file size policy is violated",
                        "When file exclusion policy is violated",
                        "When disk is full",
                        "When required permissions are unavailable",
                        "When file path exceeds optimal limit"
   };
  private enum policyTypes{
     QuotaViolation,
   FileSizeViolation,
   FileExclusionViolation,
   DiskFull,
   PermissionUnavailable,
   ExceedsPathSize
   };
  public PrefsSettingPage( Gtk.Window topWindow )
   : base()
  {
                        InitializeWidgets();
                        this.Realized += new EventHandler(OnRealizeWidget);
  }
  ~PrefsSettingPage()
  {
  }
  private void InitializeWidgets()
  {
   this.Spacing = Util.SectionSpacing;
                        this.BorderWidth = Util.DefaultBorderWidth;
   NotifyCheckButton = new CheckButton(Util.GS("Display _Notification"));
                        this.PackStart(NotifyCheckButton, false, false, 0);
                        ScrolledWindow sw = new ScrolledWindow();
                        sw.ShadowType = Gtk.ShadowType.None;
                        PolicyTreeView = new TreeView();
                        sw.Add(PolicyTreeView);
                        PolicyTreeView.HeadersVisible = false;
   PolicyTreeView.Selection.Mode = SelectionMode.Multiple;
                        PolicyTreeStore = new ListStore(typeof(string));
                        PolicyTreeView.Model = PolicyTreeStore;
   this.PackStart(sw, false, false, 0);
                        CellRendererText logcr = new CellRendererText();
                        logcr.Xpad = 10;
                        PolicyTreeView.AppendColumn(Util.GS("Log"), logcr, "text", 0);
  }
                private void OnRealizeWidget(object o, EventArgs args)
                {
                        PopulateWidgets();
                }
                private void PopulateWidgets()
  {
   TreePath path = null;
   string indexToSelect = null;
   AddMessage();
   if((bool)ClientConfig.Get(ClientConfig.KEY_SHOW_QUOTA_VIOLATION))
   {
    indexToSelect = ((int)policyTypes.QuotaViolation).ToString();
    path = new TreePath(indexToSelect);
    PolicyTreeView.Selection.SelectPath(path);
   }
   if((bool)ClientConfig.Get(ClientConfig.KEY_SHOW_FILE_SIZE_VOILATION))
   {
    indexToSelect = ((int)policyTypes.FileSizeViolation).ToString();
    path = new TreePath(indexToSelect);
    PolicyTreeView.Selection.SelectPath(path);
   }
   if((bool)ClientConfig.Get(ClientConfig.KEY_SHOW_EXCLUSION_VOILATION))
   {
    indexToSelect = ((int)policyTypes.FileExclusionViolation).ToString();
    path = new TreePath(indexToSelect);
    PolicyTreeView.Selection.SelectPath(path);
   }
   if((bool)ClientConfig.Get(ClientConfig.KEY_SHOW_DISK_FULL))
   {
    indexToSelect = ((int)policyTypes.DiskFull).ToString();
    path = new TreePath(indexToSelect);
    PolicyTreeView.Selection.SelectPath(path);
   }
   if((bool)ClientConfig.Get(ClientConfig.KEY_SHOW_PERMISSION_UNAVAILABLE))
   {
    indexToSelect = ((int)policyTypes.PermissionUnavailable).ToString();
    path = new TreePath(indexToSelect);
    PolicyTreeView.Selection.SelectPath(path);
   }
   if((bool)ClientConfig.Get(ClientConfig.KEY_SHOW_EXCEEDS_PATH_SIZE))
   {
    indexToSelect = ((int)policyTypes.ExceedsPathSize).ToString();
    path = new TreePath(indexToSelect);
    PolicyTreeView.Selection.SelectPath(path);
   }
  }
  public void GetSelectedRow()
  {
   string rowNumber = null;
   int index = 0;
   if(!NotifyCheckButton.Active)
   {
    return;
   }
   ClientConfig.Set(ClientConfig.KEY_SHOW_QUOTA_VIOLATION , false);
   ClientConfig.Set(ClientConfig.KEY_SHOW_FILE_SIZE_VOILATION , false);
   ClientConfig.Set(ClientConfig.KEY_SHOW_EXCLUSION_VOILATION , false);
   ClientConfig.Set(ClientConfig.KEY_SHOW_DISK_FULL , false);
   ClientConfig.Set(ClientConfig.KEY_SHOW_PERMISSION_UNAVAILABLE , false);
   ClientConfig.Set(ClientConfig.KEY_SHOW_EXCEEDS_PATH_SIZE , false);
     Gtk.TreePath[] paths = PolicyTreeView.Selection.GetSelectedRows ();
                        for (int n=0; n<paths.Length; n++) {
    rowNumber = paths[n].ToString();
    index = int.Parse(rowNumber);
    switch(index)
    {
     case (int)policyTypes.QuotaViolation:
      ClientConfig.Set(ClientConfig.KEY_SHOW_QUOTA_VIOLATION , true);
     break;
     case (int)policyTypes.FileSizeViolation:
      ClientConfig.Set(ClientConfig.KEY_SHOW_FILE_SIZE_VOILATION , true);
     break;
     case (int)policyTypes.FileExclusionViolation:
      ClientConfig.Set(ClientConfig.KEY_SHOW_EXCLUSION_VOILATION , true);
     break;
     case (int)policyTypes.DiskFull:
      ClientConfig.Set(ClientConfig.KEY_SHOW_DISK_FULL , true);
     break;
     case (int)policyTypes.PermissionUnavailable:
      ClientConfig.Set(ClientConfig.KEY_SHOW_PERMISSION_UNAVAILABLE , true);
     break;
     case (int)policyTypes.ExceedsPathSize:
      ClientConfig.Set(ClientConfig.KEY_SHOW_EXCEEDS_PATH_SIZE , true);
     break;
     default:
     break;
    }
                        }
  }
                private void AddMessage()
                {
   foreach(string str in policyVoilation)
   {
    PolicyTreeStore.AppendValues(string.Format( "{0}", Util.GS(str) ) );
   }
                }
 }
}
