

using System;
using Gtk;
using System.Runtime.InteropServices;

namespace Novell.iFolder
{



 public class CompatFileChooserDialog
 {





        public enum Action
  {
   Open,
   Save,
   SelectFolder
  }







  public CompatFileChooserDialog (string title, Gtk.Window parent,
          Action action)
  {

   string gtkResults = Gtk.Global.CheckVersion(2,4,0);
   if (gtkResults == null || gtkResults.Length == 0)
   {


    gtkResults = Gtk.Global.CheckVersion(2,8,0);
    if (gtkResults == null || gtkResults.Length == 0)
     use_file_chooser = false;
    else
     use_file_chooser = true;
   }
   else
   {
    use_file_chooser = false;
   }






   if (use_file_chooser)
    create_with_file_chooser (title, parent, action);
   else
    create_with_file_selection (title, parent, action);
  }




  public bool SelectMultiple
  {
   get
   {
    if (use_file_chooser)
     return gtk_file_chooser_get_select_multiple(chooser.Handle);
    else
     return filesel.SelectMultiple;
   }

   set
   {
    if (use_file_chooser)
     gtk_file_chooser_set_select_multiple(chooser.Handle, value);
    else
     filesel.SelectMultiple = value;
   }
  }




  public string Filename
  {
   get
   {
    if (use_file_chooser)
     return gtk_file_chooser_get_filename (chooser.Handle);
    else
     return filesel.Filename;
   }

   set
   {
    if (use_file_chooser)




     gtk_file_chooser_set_filename (chooser.Handle, value);
    else
     filesel.Filename = value;
   }
  }




  public string CurrentFolder
  {
   get
   {
    if (use_file_chooser)
     return gtk_file_chooser_get_current_folder (chooser.Handle);
    else
     return null;
   }
   set
   {
    if (use_file_chooser)
     gtk_file_chooser_set_current_folder(chooser.Handle, value);
   }
  }

  public string[] Selections
  {
   get
   {
    if (use_file_chooser)
    {
     IntPtr ptr = gtk_file_chooser_get_filenames(chooser.Handle);
     if (ptr == IntPtr.Zero)
      return null;

     GLib.SList slist = new GLib.SList (ptr, typeof (string));

     string [] paths = new string [slist.Count];
     for (int i = 0; i < slist.Count; i ++)
      paths [i] = (string) slist [i];

     return paths;
    } else
     return filesel.Selections;
   }
  }

  public void Destroy ()
  {
   if (use_file_chooser)
   {

   }
   else
    filesel.Destroy ();
  }


  public void Hide()
  {
   if (use_file_chooser)
    chooser.Hide();
   else
    filesel.Hide();
  }




        public int Run ()
  {
   int response;

   if (use_file_chooser)
    response = chooser.Run ();
   else
    response = filesel.Run ();

   return response;
  }



  private bool use_file_chooser;

  private Gtk.FileSelection filesel;
  private Gtk.Dialog chooser;

  private void create_with_file_chooser (string title, Gtk.Window parent,
            Action action)
  {
   int a = 0;
   string stock = Gtk.Stock.Open;


   switch (action)
   {
    case Action.Open:
     a = 0;
     stock = Gtk.Stock.Open;
     break;

    case Action.Save:
     a = 1;
     stock = Gtk.Stock.Save;
     break;

    case Action.SelectFolder:
     a = 2;
     stock = Gtk.Stock.Open;
     break;
   }

   IntPtr ptr = gtk_file_chooser_dialog_new (title,
     parent != null ? parent.Handle : IntPtr.Zero,
     a,
     IntPtr.Zero);

   chooser = GLib.Object.GetObject (ptr, false) as Gtk.Dialog;

   chooser.AddButton (Gtk.Stock.Cancel, Gtk.ResponseType.Cancel);


   chooser.AddButton (stock, Gtk.ResponseType.Ok);
   chooser.DefaultResponse = Gtk.ResponseType.Ok;
  }

  private void create_with_file_selection (string title,
        Gtk.Window parent, Action action)
  {
   filesel = new Gtk.FileSelection (title);
   filesel.TransientFor = parent;


   switch (action)
   {
    case Action.Open:
     filesel.ShowFileops = false;
     break;

    case Action.Save:
     filesel.FileopDelFile.Hide ();
     filesel.FileopRenFile.Hide ();
     break;

    case Action.SelectFolder:
     filesel.FileList.Parent.Hide ();
     filesel.SelectionEntry.Hide ();
     filesel.FileopDelFile.Hide ();
     filesel.FileopRenFile.Hide ();
     break;
   }
  }



  [DllImport("libgtk-x11-2.0.so.0")]

   extern static IntPtr gtk_file_chooser_dialog_new (string title,
      IntPtr parent, int action, IntPtr varargs);

  [DllImport("libgtk-x11-2.0.so.0")]

   extern static string gtk_file_chooser_get_filename (IntPtr handle);

  [DllImport("libgtk-x11-2.0.so.0")]

   extern static int gtk_file_chooser_set_filename (IntPtr handle,
      string filename);

  [DllImport("libgtk-x11-2.0.so.0")]

   extern static IntPtr gtk_file_chooser_get_filenames (IntPtr handle);

  [DllImport("libgtk-x11-2.0.so.0")]

   extern static bool gtk_file_chooser_get_select_multiple (
      IntPtr handle);

  [DllImport("libgtk-x11-2.0.so.0")]

   extern static void gtk_file_chooser_set_select_multiple (
      IntPtr handle, bool multi);

  [DllImport("libgtk-x11-2.0.so.0")]

   extern static bool gtk_file_chooser_set_current_folder (
      IntPtr handle, string filename);

  [DllImport("libgtk-x11-2.0.so.0")]

   extern static string gtk_file_chooser_get_current_folder (
      IntPtr handle);
 }
}
