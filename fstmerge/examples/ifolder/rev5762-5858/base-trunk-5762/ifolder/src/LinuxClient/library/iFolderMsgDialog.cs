


using System;
using Gtk;

namespace Novell.iFolder
{

public class iFolderMsgDialog : Dialog
{

 private Expander detailsExpander;
 private ScrolledWindow showDetailsScrolledWindow;

 public enum DialogType : int
 {
  Error = 1,
  Info,
  Question,
  Warning
 }

 public enum ButtonSet : int
 {
  Ok = 1,
  OkCancel,
  YesNo,
  None
 }


 public iFolderMsgDialog( Gtk.Window parent,
        DialogType type,
        ButtonSet buttonSet,
        string title,
        string statement,
        string secondaryStatement)
  : base()
 {
  Init(parent, type, buttonSet, title, statement, secondaryStatement, null);
 }






 public iFolderMsgDialog( Gtk.Window parent,
        DialogType type,
        ButtonSet buttonSet,
        string title,
        string statement,
        string secondaryStatement,
        string details)
  : base()
 {
  Init(parent, type, buttonSet, title, statement, secondaryStatement, details);
 }

 internal void Init(Gtk.Window parent,
        DialogType type,
        ButtonSet buttonSet,
        string title,
        string statement,
        string secondaryStatement,
        string details)
 {
  this.Title = title;
  this.HasSeparator = false;

  this.Resizable = false;
  this.Modal = true;
  if(parent != null)
   this.TransientFor = parent;

  HBox h = new HBox();
  h.BorderWidth = 10;
  h.Spacing = 10;

  Image i = new Image();
  switch(type)
  {
   case DialogType.Error:
    i.SetFromStock(Gtk.Stock.DialogError, IconSize.Dialog);
    break;
   case DialogType.Question:
    i.SetFromStock(Gtk.Stock.DialogQuestion, IconSize.Dialog);
    break;
   case DialogType.Warning:
    i.SetFromStock(Gtk.Stock.DialogWarning, IconSize.Dialog);
    break;
   default:
   case DialogType.Info:
    i.SetFromStock(Gtk.Stock.DialogInfo, IconSize.Dialog);
    break;
  }
  i.SetAlignment(0.5F, 0);
  h.PackStart(i, false, false, 0);

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

  if (details != null)
  {
   detailsExpander = new Expander(Util.GS("_Details"));
   v.PackStart(detailsExpander, false, false, 0);
   TextView textView = new TextView();
   textView.Editable = false;
   textView.WrapMode = WrapMode.Char;
   TextBuffer textBuffer = textView.Buffer;
   textBuffer.Text = details;
   showDetailsScrolledWindow = new ScrolledWindow();
   detailsExpander.Add(showDetailsScrolledWindow);
   showDetailsScrolledWindow.AddWithViewport(textView);
   showDetailsScrolledWindow.Visible = false;
  }
  h.PackEnd(v);
  h.ShowAll();
  this.VBox.Add(h);
  switch(buttonSet)
  {
   default:
   case ButtonSet.Ok:
    this.AddButton(Stock.Ok, ResponseType.Ok);
    break;
   case ButtonSet.OkCancel:
    this.AddButton(Stock.Cancel, ResponseType.Cancel);
    this.AddButton(Stock.Ok, ResponseType.Ok);
    break;
   case ButtonSet.YesNo:
    this.AddButton(Stock.No, ResponseType.No);
    this.AddButton(Stock.Yes, ResponseType.Yes);
    break;
  }
 }
}
}
