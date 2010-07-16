

using Gtk;
using System;

namespace Novell.iFolder
{



 public class iFolderAcceptDialog : FileChooserDialog
 {
  private string initialPath;
  private bool merge;




  public new string Path
  {
   get
   {
    return this.Filename;
   }
  }




        public iFolderAcceptDialog(iFolderWeb ifolder, string initialPath) : this(ifolder, initialPath, false)
  {
  }







  public iFolderAcceptDialog(iFolderWeb ifolder, string initialPath, bool merge)
    : base("", "", null, FileChooserAction.SelectFolder, Stock.Cancel, ResponseType.Cancel)
        {
         if( !merge)
         {
    this.Title =
     string.Format(Util.GS("Download \"{0}\" to..."), ifolder.Name);
   }
   else
   {
    this.Title =
     string.Format(Util.GS("Merge \"{0}\" to..."), ifolder.Name);
   }
         this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));

         this.initialPath = initialPath;

         this.SelectMultiple = false;
         this.LocalOnly = true;


         if (this.initialPath != null && this.initialPath.Length > 0)
          this.SetCurrentFolder(this.initialPath);

         if( !merge)
    this.AddButton(Util.GS("_Download"), ResponseType.Ok);
   else
    this.AddButton(Util.GS("_Merge"), ResponseType.Ok);
        }
 }
}
