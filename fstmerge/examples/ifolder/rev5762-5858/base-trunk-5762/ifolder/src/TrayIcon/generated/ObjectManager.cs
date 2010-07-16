


namespace GtkSharp.TrayIcon {

 public class ObjectManager {

  static bool initialized = false;

  public static void Initialize ()
  {
   if (initialized)
    return;

   initialized = true;
   GLib.ObjectManager.RegisterType("EggTrayIcon", "Egg.TrayIcon,TrayIcon");
  }
 }
}
