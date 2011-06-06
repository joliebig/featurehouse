using System;
using System.Drawing;
using System.Collections;
using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;
namespace Novell.AddressBook.UI.gtk
{
 public class OptionChangedEventArgs : EventArgs
 {
  private readonly int index;
  public OptionChangedEventArgs(int index)
  {
   this.index = index;
  }
  public int OptionIndex
  {
   get {return index;}
  }
 }
 public delegate void OptionChangedEventHandler(object sender,
   OptionChangedEventArgs e);
 public class OptionButton : Button
 {
  internal Gtk.Menu menu = null;
  internal Gtk.Label label = null;
  internal System.Collections.ArrayList optionArray = null;
  internal int curIndex;
  static GLib.GType gtype = GLib.GType.Invalid;
  public event OptionChangedEventHandler OptionChanged;
  public OptionButton(string buttonLabel) : base (GType)
  {
   HBox hbox = new HBox(false, 0);
   label = new Label(buttonLabel);
   hbox.PackStart(label, false, false, 5);
   Alignment al = new Alignment(0,0,0,0);
   hbox.PackStart(al, true, true, 0);
   Arrow ba = new Arrow(ArrowType.Down, ShadowType.None);
   ba.Xalign = (float)1;
   ba.Yalign = (float).5;
   hbox.PackStart(ba, false, false, 5);
   Add(hbox);
   curIndex = 0;
  }
  public static new GLib.GType GType
  {
   get
   {
    if(gtype == GLib.GType.Invalid)
     gtype = RegisterGType(typeof (OptionButton));
    return gtype;
   }
  }
  public new string Label
  {
   get
   {
    return(this.label.Text);
   }
   set
   {
    label.Text = value;
   }
  }
  public int Index
  {
   get
   {
    return(this.curIndex);
   }
   set
   {
    if( (optionArray != null) &&
      (value >= 0) && (value < optionArray.Count) )
    {
     MenuItem mItem = (MenuItem)optionArray[value];
     label.Text = ((Label)mItem.Child).Text;
     curIndex = value;
    }
   }
  }
  public void AddOption(string menuOption)
  {
   if(menu == null)
    menu = new Menu();
   if(optionArray == null)
    optionArray = new System.Collections.ArrayList();
   MenuItem newItem = new MenuItem(menuOption);
   menu.Append(newItem);
   newItem.Activated +=
    new EventHandler(handle_menu_item);
   optionArray.Add(newItem);
  }
  internal void handle_menu_item(object o, EventArgs args)
  {
   MenuItem mItem = (Gtk.MenuItem)o;
   curIndex = optionArray.IndexOf(mItem);
   label.Text = ((Label)mItem.Child).Text;
   if(OptionChanged != null)
    OptionChanged(this, new OptionChangedEventArgs(curIndex));
  }
  protected override bool OnButtonReleaseEvent(Gdk.EventButton evnt)
  {
   base.OnButtonReleaseEvent(evnt);
   if(menu != null)
   {
    menu.ShowAll();
    menu.Popup(null, null,
     new MenuPositionFunc(PositionMenu),
     IntPtr.Zero, 0, Gtk.Global.CurrentEventTime);
   }
   return true;
  }
  internal void PositionMenu(Menu menu, out int x, out int y,
    out bool push_in)
  {
   GdkWindow.GetOrigin(out x, out y);
   x += Allocation.X;
   y += Allocation.Y;
   push_in = false;
  }
 }
}
