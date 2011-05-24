

using Gtk;
using System;

namespace Novell.iFolder
{
 public class iFolderAcceptDialog : Dialog
 {
  private iFolderWeb ifolder;
  private string initialPath;
  private Label previewPath;
  FileChooserWidget fileChooserWidget;


  public new string Path
  {
   get
   {
    return fileChooserWidget.CurrentFolder;

   }
  }

  public iFolderAcceptDialog(iFolderWeb ifolder, string initialPath) : base()
  {
   this.Title =
    string.Format(Util.GS("Download \"{0}\"..."), ifolder.Name);
   this.SetDefaultSize (600, 500);

   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder24.png"));

   this.ifolder = ifolder;
   this.initialPath = initialPath;


   VBox dialogBox = new VBox();
   dialogBox.Spacing = 10;
   dialogBox.BorderWidth = 10;
   dialogBox.Homogeneous = false;
   this.VBox.PackStart(dialogBox, true, true, 0);
   VBox detailBox = new VBox();
   dialogBox.PackStart(detailBox, false, false, 0);
   Label l = new Label(Util.GS("Details:"));
   l.Xalign = 0;
   detailBox.PackStart(l, false, false, 0);
   TextView tv = new TextView();
   tv.LeftMargin = 10;
   tv.RightMargin = 10;
   tv.Editable = false;
   tv.CursorVisible = false;
   TextBuffer buffer = tv.Buffer;
   buffer.Text = string.Format(Util.GS("Name: {0}\nShared by: {1}\nAccess: {2}"), ifolder.Name, ifolder.Owner, GetDisplayRights(ifolder.CurrentUserRights));
   ScrolledWindow sw = new ScrolledWindow();
   sw.ShadowType = Gtk.ShadowType.EtchedIn;
   sw.Add(tv);
   detailBox.PackStart(sw, false, false, 0);
   l = new Label(Util.GS("The iFolder will be downloaded into this folder:"));
   l.LineWrap = false;
   l.Xalign = 0; l.Yalign = 1;
   dialogBox.PackStart(l, false, false, 0);
   previewPath = new Label();
   previewPath.Xalign = 0; previewPath.Yalign = 0;
   previewPath.Wrap = true;
   dialogBox.PackStart(previewPath, false, false, 0);
   fileChooserWidget =
    new FileChooserWidget(FileChooserAction.SelectFolder, "");
   fileChooserWidget.SelectMultiple = false;
   fileChooserWidget.LocalOnly = true;
   fileChooserWidget.CurrentName = ifolder.Name;
   if (this.initialPath != null && this.initialPath.Length > 0)
    fileChooserWidget.SetCurrentFolder(this.initialPath);
   fileChooserWidget.SelectionChanged +=
    new EventHandler(OnFileChooserSelectionChanged);
   dialogBox.PackStart(fileChooserWidget, true, true, 0);
   this.VBox.ShowAll();
   this.AddButton(Stock.Cancel, ResponseType.Cancel);
   this.AddButton(Stock.Ok, ResponseType.Ok);
   if (this.initialPath != null && this.initialPath.Length > 0)
    this.SetResponseSensitive(ResponseType.Ok, true);
   else
    this.SetResponseSensitive(ResponseType.Ok, false);
  }
  private void OnFileChooserSelectionChanged(object sender, EventArgs args)
  {
   previewPath.Text = System.IO.Path.Combine(fileChooserWidget.Filename, ifolder.Name);
  }
  private string GetDisplayRights(string rights)
  {
   if(rights == "ReadWrite")
    return Util.GS("Read/Write");
   else if(rights == "Admin")
    return Util.GS("Full Control");
   else if(rights == "ReadOnly")
    return Util.GS("Read Only");
   else
    return Util.GS("Unknown");
  }
 }
}
