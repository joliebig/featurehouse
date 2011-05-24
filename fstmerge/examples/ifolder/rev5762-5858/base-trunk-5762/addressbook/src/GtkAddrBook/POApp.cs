

using System;

using Gtk;
using Gnome;
using Gdk;
using GtkSharp;
using Novell.AddressBook.UI.gtk;

namespace POBoxViewer
{
 public class POBoxViewerApp
 {
  public static void Main (string[] args)
  {
   Gnome.Program program =
    new Program("pobox-viewer", "0.10.0", Modules.UI, args);

   POBoxViewer poBox = new POBoxViewer();
   poBox.ViewerClosed += new EventHandler(on_po_closed);
   poBox.ShowAll();
   program.Run();
  }

  public static void on_po_closed(object o, EventArgs args)
  {
   Application.Quit();
  }
 }
}
