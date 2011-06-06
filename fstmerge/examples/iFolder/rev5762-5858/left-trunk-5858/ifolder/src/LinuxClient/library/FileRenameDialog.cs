using Gtk;
using System;
namespace Novell.iFolder
{
 public class FileRenameDialog : Dialog
 {
  private Entry nameEntry;
  private string fileName;
  public string FileName
  {
   get
   {
    return nameEntry.Text;
   }
   set
   {
    fileName = value;
    nameEntry.Text = value;
   }
  }
  public FileRenameDialog(Gtk.Window parent) : base()
  {
   this.TransientFor = parent;
   SetupDialog();
  }
  private void SetupDialog()
  {
   this.Title = Util.GS("Rename file");
   this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder24.png"));
   this.HasSeparator = false;
   this.SetDefaultSize (450, 100);
   this.Resizable = false;
   this.Modal = true;
   this.DefaultResponse = ResponseType.Ok;
   HBox hbox = new HBox();
   Label nameLabel = new Label(Util.GS("File name:"));
   hbox.PackStart(nameLabel, false, false, 0);
   nameEntry = new Entry();
   nameEntry.Changed += new EventHandler(OnNameChanged);
   nameEntry.ActivatesDefault = true;
   hbox.PackStart(nameEntry, true, true, 0);
   this.VBox.PackStart(hbox, true, true, 0);
   hbox.ShowAll();
   this.AddButton(Stock.Cancel, ResponseType.Cancel);
   this.AddButton(Stock.Ok, ResponseType.Ok);
   this.SetResponseSensitive(ResponseType.Ok, false);
   this.DefaultResponse = ResponseType.Ok;
  }
  private void OnNameChanged(object obj, EventArgs args)
  {
   bool enableOK = false;
   if( (nameEntry.Text.Length > 0) &&
    (nameEntry.Text != fileName) )
    enableOK = true;
   else
    enableOK = false;
   this.SetResponseSensitive(ResponseType.Ok, enableOK);
  }
 }
}
