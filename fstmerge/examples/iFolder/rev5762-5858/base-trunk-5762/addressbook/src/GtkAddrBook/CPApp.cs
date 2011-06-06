using System;
using Simias.Storage;
using Gtk;
using Gnome;
using Gdk;
using GtkSharp;
using Novell.AddressBook.UI.gtk;
namespace CollectionPropertiesViewer
{
 public class CollectionPropertiesApp
 {
  public static void Main (string[] args)
  {
   Gnome.Program program =
    new Program("collection-properties", "0.10.0", Modules.UI, args);
   Store store = Store.GetStore();
   if(args.Length < 1)
   {
    Console.WriteLine("Usage: ColPropViewer [collectionID]");
    Console.WriteLine("       where collectionID is:");
    foreach(ShallowNode sn in store)
    {
     Collection col = store.GetCollectionByID(sn.ID);
     Console.WriteLine("{0} : {1}", col.Name, col.ID);
    }
   }
   else
   {
    Collection col = store.GetCollectionByID(args[0]);
    if(col != null)
    {
     CollectionProperties cp = new CollectionProperties();
     cp.Collection = col;
     cp.Closed += new EventHandler(on_cp_closed);
     cp.Show();
     program.Run();
    }
   }
  }
  public static void on_cp_closed(object o, EventArgs args)
  {
   Application.Quit();
  }
 }
}
