

using Gtk;
using System;

namespace Novell.iFolder
{



 public class MigrateLocation : FileChooserDialog
 {
  private DomainInformation[] domains;
  private ComboBox domainComboBox;

  private CheckButton Encryption;
  private CheckButton SSL;
  private string initialPath;
  private string copyDir;
  iFolderWebService ifws;
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





  public MigrateLocation(Gtk.Window parentWindow, string initialPath, iFolderWebService ifws)
    : base("", Util.GS("Select a folder..."), parentWindow, FileChooserAction.SelectFolder, Stock.Cancel, ResponseType.Cancel,
                Stock.Ok, ResponseType.Ok)
  {
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));

   this.initialPath = System.IO.Directory.GetCurrentDirectory();

   this.copyDir = initialPath;
   this.ifws = ifws;

   keyReleasedTimeoutID = 0;

   if (this.initialPath != null && this.initialPath.Length > 0)
    this.SetCurrentFolder(this.initialPath);


   this.SetResponseSensitive(ResponseType.Ok, false);
  }




  protected override void OnSelectionChanged()
  {
   string currentPath = this.Filename;
   if( copyDir != null)
    currentPath += "/"+copyDir;

   try
   {
    if (ifws.CanBeiFolder(currentPath))
     this.SetResponseSensitive(ResponseType.Ok, true);
    else
     this.SetResponseSensitive(ResponseType.Ok, false);
   }
   catch (Exception e)
   {
    this.SetResponseSensitive(ResponseType.Ok, false);
   }
  }

  protected override bool OnKeyReleaseEvent(Gdk.EventKey evnt)
  {



   if (keyReleasedTimeoutID != 0)
   {
    GLib.Source.Remove(keyReleasedTimeoutID);
    keyReleasedTimeoutID = 0;
   }




   keyReleasedTimeoutID =
    GLib.Timeout.Add(100,
         new GLib.TimeoutHandler(CheckEnableOkButton));

   return true;
  }





  private bool CheckEnableOkButton()
  {
   try
   {
    string currentPath = this.Filename;
    if (ifws.CanBeiFolder(currentPath))
    {
     this.SetResponseSensitive(ResponseType.Ok, true);
     return false;
    }
   }
   catch{}
   this.SetResponseSensitive(ResponseType.Ok, false);
   return false;
  }
 }
}
