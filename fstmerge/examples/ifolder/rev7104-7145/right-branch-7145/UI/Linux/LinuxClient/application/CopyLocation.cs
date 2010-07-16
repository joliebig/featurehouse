

using Gtk;
using System;

namespace Novell.iFolder
{



 public class CopyLocation : FileChooserDialog
 {
  private DomainInformation[] domains;
  private ComboBox domainComboBox;

  private uint keyReleasedTimeoutID;




  public string iFolderPath
  {
   get
   {
    return this.Filename;
   }
   set
   {
    this.SetCurrentFolder(value);
   }
  }




  public string DomainID
  {
   get
   {
    int activeIndex = domainComboBox.Active;
    if (activeIndex >= 0)
     return domains[activeIndex].ID;
    else
     return "0";
   }
  }





  public CopyLocation(Gtk.Window parentWindow, string initialPath)
    : base("Choose folder", Util.GS("Select the folder to Download..."), parentWindow, FileChooserAction.SelectFolder, Stock.Cancel, ResponseType.Cancel,
                Stock.Ok, ResponseType.Ok)
  {
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));


    this.SetCurrentFolder(initialPath);
   this.SetFilename(initialPath);

   this.SetResponseSensitive(ResponseType.Ok, false);
  }

  protected override void OnSelectionChanged()
  {

   this.SetResponseSensitive(ResponseType.Ok, true);
  }

  protected override bool OnKeyReleaseEvent(Gdk.EventKey evnt)
  {



   if (keyReleasedTimeoutID != 0)
   {
    GLib.Source.Remove(keyReleasedTimeoutID);
    keyReleasedTimeoutID = 0;
   }

   return true;
  }

 }
}
