using System;
using Gtk;
using Gnome;
using Gdk;
using GtkSharp;
using Novell.AddressBook.UI.gtk;
namespace ContactBrowser
{
 public class ContactBrowserApp
 {
  public static void Main (string[] args)
  {
   Gnome.Program program =
    new Program("contact-browser", "0.10.0", Modules.UI, args);
   ContactBrowser cb = new ContactBrowser();
   cb.AddrBookClosed += new EventHandler(on_cb_closed);
   cb.ShowAll();
   program.Run();
  }
  public static void on_cb_closed(object o, EventArgs args)
  {
   Application.Quit();
  }
 }
}
