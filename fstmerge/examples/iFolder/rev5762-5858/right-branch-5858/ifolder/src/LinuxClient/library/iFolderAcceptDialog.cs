using Gtk;
using System;
namespace Novell.iFolder
{
 public class iFolderAcceptDialog : FileChooserDialog
 {
  private iFolderWeb ifolder;
  private string initialPath;
  private Entry nameEntry;
  public new string Path
  {
   get
   {
    return this.CurrentFolder;
   }
  }
  public iFolderAcceptDialog(iFolderWeb ifolder, string initialPath)
    : base("", "", null, FileChooserAction.SelectFolder, Stock.Cancel, ResponseType.Cancel)
        {
   this.Title =
    string.Format(Util.GS("Download \"{0}\" to..."), ifolder.Name);
         this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder24.png"));
         this.ifolder = ifolder;
         this.initialPath = initialPath;
         this.SelectMultiple = false;
         this.LocalOnly = true;
         this.CurrentName = ifolder.Name;
         if (this.initialPath != null && this.initialPath.Length > 0)
          this.SetCurrentFolder(this.initialPath);
   this.AddButton(Util.GS("_Download"), ResponseType.Ok);
        }
 }
}
