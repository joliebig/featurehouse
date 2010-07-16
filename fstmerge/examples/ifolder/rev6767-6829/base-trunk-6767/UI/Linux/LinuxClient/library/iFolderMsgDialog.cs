

using System;
using Gtk;

namespace Novell.iFolder
{




public class iFolderMsgDialog : Dialog
{

 private Image dialogImage;
 private Expander detailsExpander;
 private ScrolledWindow showDetailsScrolledWindow;
 private VBox extraWidgetVBox;
 private Widget extraWidget;




 public Image Image
 {
  get
  {
   return dialogImage;
  }
 }




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
  AcceptDeny,
  None
 }




    public Widget ExtraWidget
 {
  get
  {
   return extraWidget;
  }
  set
  {
   if (extraWidget != null)
   {
    extraWidgetVBox.Remove(extraWidget);
    extraWidget.Destroy();
    extraWidget = null;
   }

   if (value == null)
   {
    extraWidgetVBox.Hide();
   }
   else
   {
    extraWidget = value;
    extraWidgetVBox.PackStart(extraWidget, false, false, 0);
    extraWidgetVBox.Show();
    extraWidget.Show();
   }
  }
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
  this.Icon = new Gdk.Pixbuf(Util.ImagesPath("ifolder16.png"));
  this.Resizable = false;
  this.Modal = true;
  if(parent != null)
  {
   this.TransientFor = parent;
   this.WindowPosition = WindowPosition.CenterOnParent;
  }
  else
   this.WindowPosition = WindowPosition.Center;
  HBox h = new HBox();
  h.BorderWidth = 10;
  h.Spacing = 10;
  dialogImage = new Image();
  switch(type)
  {
   case DialogType.Error:
    dialogImage.SetFromStock(Gtk.Stock.DialogError, IconSize.Dialog);
    break;
   case DialogType.Question:
    dialogImage.SetFromStock(Gtk.Stock.DialogQuestion, IconSize.Dialog);
    break;
   case DialogType.Warning:
    dialogImage.SetFromStock(Gtk.Stock.DialogWarning, IconSize.Dialog);
    break;
   default:
   case DialogType.Info:
    dialogImage.SetFromStock(Gtk.Stock.DialogInfo, IconSize.Dialog);
    break;
  }
  dialogImage.SetAlignment(0.5F, 0);
  h.PackStart(dialogImage, false, false, 0);
  VBox v = new VBox();
  v.Spacing = 10;
  Label l = new Label();
  l.LineWrap = true;
  l.UseMarkup = true;
  l.Selectable = false;
  l.CanFocus = false;
  l.Xalign = 0; l.Yalign = 0;
  l.Markup = "<span weight=\"bold\" size=\"larger\">" + GLib.Markup.EscapeText(statement) + "</span>";
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
  extraWidgetVBox = new VBox(false, 0);
  v.PackStart(extraWidgetVBox, false, false, 0);
  extraWidgetVBox.NoShowAll = true;
  extraWidget = null;
  h.PackEnd(v);
  h.ShowAll();
  this.VBox.Add(h);
  Widget defaultButton;
  switch(buttonSet)
  {
   default:
   case ButtonSet.Ok:
    defaultButton = this.AddButton(Stock.Ok, ResponseType.Ok);
    break;
   case ButtonSet.OkCancel:
    this.AddButton(Stock.Cancel, ResponseType.Cancel);
    defaultButton = this.AddButton(Stock.Ok, ResponseType.Ok);
    break;
   case ButtonSet.YesNo:
    this.AddButton(Util.GS("No"), ResponseType.No);
    defaultButton = this.AddButton(Util.GS("Yes"), ResponseType.Yes);
    break;
   case ButtonSet.AcceptDeny:
    this.AddButton(Util.GS("Deny"), ResponseType.No);
    defaultButton = this.AddButton(Util.GS("Accept"), ResponseType.Yes);
    break;
  }
  defaultButton.CanDefault = true;
  defaultButton.GrabFocus();
  defaultButton.GrabDefault();
 }
}
}
