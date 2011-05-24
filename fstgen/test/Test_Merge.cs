

using System;
using System.Net;
using Gtk;
using Gnome;
using Novell.iFolder;
using Simias.Client;

namespace Novell.iFolder.Nautilus
{
 public class NautilusiFolder
 {
  public static int Main (string[] args)
  {

   if (args.Length == 0)
    return 0;


   Gnome.Program program =
    new Program ("Nautilus-Extension-UI", "0.1.0", Modules.UI, args);


   Util.InitCatalog();

   switch (args [0]) {
    case "share":
     return showShareDialog (args);
    case "properties":
     return showPropertiesDialog (args);
    case "help":
     return showHelp (args);
   }

   program.Run ();
   return 0;
  }

  public static void on_dialog_closed (object o, EventArgs args)
  {
   Application.Quit ();
  }

  private static int showShareDialog (string[] args)
  {
   if (args.Length < 2) {
    System.Console.Write ("ERROR: iFolder ID not specified\n");
    return -1;
   }

   Manager manager = new Manager();
   manager.Start();

   iFolderPropertiesDialog propsDialog;
   propsDialog = new iFolderPropertiesDialog (args [1], manager);
   propsDialog.CurrentPage = 1;
   propsDialog.Run ();
   propsDialog.Hide ();
   propsDialog.Destroy ();

   manager.Stop();

   return 0;
  }

  private static int showPropertiesDialog (string[] args)
  {
   if (args.Length < 2) {
    System.Console.Write ("ERROR: iFolder ID not specified\n");
    return -1;
   }

   Manager manager = new Manager();
   manager.Start();

   iFolderPropertiesDialog propsDialog;
   propsDialog = new iFolderPropertiesDialog (args [1], manager);
   propsDialog.CurrentPage = 0;
   propsDialog.Run ();
   propsDialog.Hide ();
   propsDialog.Destroy ();

   manager.Stop();

   return 0;
  }

  private static int showHelp (string[] args)
  {
   Util.ShowHelp("front.html", null);
   return 0;
  }
 }
}
