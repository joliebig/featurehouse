using System;
using Novell.AddressBook.UI.gtk;
using Gtk;
using Gdk;
using Gnome;
using Glade;
using GtkSharp;
using GLib;
namespace Novell.iFolder
{
 public class InviteApp
 {
  public static void Main (string[] args)
  {
   Gnome.Program program =
    new Program("invitation-assistant", "0.10.0", Modules.UI, args);
   InvitationAssistant inviteAss;
   if(args.Length > 0)
    inviteAss = new InvitationAssistant(args[0]);
   else
    inviteAss = new InvitationAssistant();
   inviteAss.AssistantClosed += new EventHandler(on_assistant_closed);
   inviteAss.ShowAll();
   program.Run();
  }
  public static void on_assistant_closed(object o, EventArgs args)
  {
   Application.Quit();
  }
 }
}
