


using System;
using Gtk;

namespace Novell.iFolder
{

public class iFolderWaitDialog : Dialog
{
 private ButtonSet buttonSet;

 public enum ButtonSet : int
 {
  Cancel,
  None
 }


 public iFolderWaitDialog(Window parent,
        Widget waitingWidget,
        ButtonSet buttonSet,
        string title,
        string statement,
        string secondaryStatement)
  : base()
 {
  Init(parent, waitingWidget, buttonSet, title, statement, secondaryStatement);
 }

 internal void Init(Gtk.Window parent,
        Widget waitingWidget,
        ButtonSet buttonSet,
        string title,
        string statement,
        string secondaryStatement)
 {
  this.Title = title;
  this.HasSeparator = false;

  this.Resizable = false;
  this.Modal = true;
  if(parent != null)
   this.TransientFor = parent;

  this.buttonSet = buttonSet;

  HBox h = new HBox();
  h.BorderWidth = 10;
  h.Spacing = 10;

  if (waitingWidget != null)
  {
   h.PackStart(waitingWidget, false, false, 0);
  }

  VBox v = new VBox();
  v.Spacing = 10;
  Label l = new Label();
  l.LineWrap = true;
  l.UseMarkup = true;
  l.Selectable = false;
  l.CanFocus = false;
  l.Xalign = 0; l.Yalign = 0;
  l.Markup = "<span weight=\"bold\" size=\"larger\">" + statement + "</span>";
  v.PackStart(l);

  l = new Label(secondaryStatement);
  l.LineWrap = true;
  l.Selectable = false;
  l.CanFocus = false;
  l.Xalign = 0; l.Yalign = 0;
  v.PackStart(l, true, true, 8);

  h.PackEnd(v);
  h.ShowAll();

  this.VBox.Add(h);

  switch(buttonSet)
  {
   case ButtonSet.Cancel:
    this.AddButton(Stock.Cancel, ResponseType.Cancel);
    break;
   case ButtonSet.None:
   default:
    break;
  }
 }

 protected override bool OnDeleteEvent(Gdk.Event evnt)
 {
  if (buttonSet == ButtonSet.None)
   return true;
  else
   return false;
 }

 protected override bool OnDestroyEvent(Gdk.Event evnt)
 {
  if (buttonSet == ButtonSet.None)
   return true;
  else
   return false;
 }
}
}
