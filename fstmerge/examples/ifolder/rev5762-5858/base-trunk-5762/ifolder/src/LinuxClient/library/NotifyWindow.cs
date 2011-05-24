


using Gtk;
using Gdk;
using System;
using System.Collections;
using System.Runtime.InteropServices;
using System.Xml;

namespace Novell.iFolder
{
 public class NotifyWindow : Gtk.Window
 {
  private Widget parentWidget;
  private Pixbuf background = null;
  private Pixbuf activebackground = null;
  private Pixbuf inactivebackground = null;
  private Gdk.Color activeBackgroundColor;
  private Gdk.Color inactiveBackgroundColor;
  private LinkTextView detailsTextView;
  private uint closeWindowTimeoutID;
  private bool isSelected = false;
  private int wbsize = 16;
  private int messageTextWidth = 300;
  private uint timeout;





  public event LinkClickedEventHandler LinkClicked;
  public NotifyWindow(Gtk.Widget parent, string message, string details,
       Gtk.MessageType messageType, uint timeout)
   : base(Gtk.WindowType.Popup)
   {
   this.AppPaintable = true;
   parentWidget = parent;
   this.timeout = timeout;
   activeBackgroundColor = new Gdk.Color(249, 253, 202);
   inactiveBackgroundColor = new Gdk.Color(255, 255, 255);
   Gtk.HBox outBox = new HBox();
   this.Add(outBox);
   outBox.BorderWidth = (uint)wbsize;
   Gtk.VBox closeBox = new VBox();
   closeBox.BorderWidth = 3;
   outBox.PackEnd(closeBox, true, true, 0);
   EventBox eBox = new EventBox();
   eBox.ButtonPressEvent +=
    new ButtonPressEventHandler(OnCloseEvent);
   Gtk.Image closeImg = new Gtk.Image();
   closeImg.SetFromStock(Gtk.Stock.Close,
           IconSize.Menu);
   eBox.Add(closeImg);
   closeBox.PackStart(eBox, false, false, 0);
   Label padder = new Label("");
   outBox.PackStart(padder, false, false, 5);
   Gtk.VBox vbox = new VBox();
   outBox.PackStart(vbox, true, true, 0);
   vbox.BorderWidth = 10;
   Gtk.HBox hbox = new HBox();
   hbox.Spacing = 5;
   vbox.PackStart(hbox, false, false, 0);
   VBox iconVBox = new VBox();
   hbox.PackStart(iconVBox, false, false, 0);
   Gtk.Image msgImage = new Gtk.Image();
   switch(messageType)
   {
    case Gtk.MessageType.Info:
     msgImage.SetFromStock(Gtk.Stock.DialogInfo,
           IconSize.Button);
     break;
    case Gtk.MessageType.Warning:
     msgImage.SetFromStock(Gtk.Stock.DialogWarning,
           IconSize.Button);
     break;
    case Gtk.MessageType.Question:
     msgImage.SetFromStock(Gtk.Stock.DialogQuestion,
           IconSize.Button);
     break;
    case Gtk.MessageType.Error:
     msgImage.SetFromStock(Gtk.Stock.DialogError,
           IconSize.Button);
     break;
   }
   iconVBox.PackStart(msgImage, false, false, 0);
   VBox messageVBox = new VBox();
   hbox.PackStart(messageVBox, true, true, 0);
   Label l = new Label();
   l.Markup = "<span size=\"small\" weight=\"bold\">" + message + "</span>";
   l.LineWrap = false;
   l.UseMarkup = true;
   l.Selectable = false;
   l.Xalign = 0;
   l.Yalign = 0;
   l.LineWrap = true;
   l.Wrap = true;
   l.WidthRequest = messageTextWidth;
   messageVBox.PackStart(l, false, true, 0);
   detailsTextView = new LinkTextView(details);
   detailsTextView.Editable = false;
   detailsTextView.CursorVisible = false;
   detailsTextView.WrapMode = WrapMode.Word;
   detailsTextView.SizeAllocate(new Gdk.Rectangle(0, 0, messageTextWidth, 600));
   detailsTextView.LinkClicked +=
    new LinkClickedEventHandler(OnLinkClicked);
   messageVBox.PackStart(detailsTextView, false, false, 3);
   Label spacer = new Label();
   spacer.UseMarkup = true;
   spacer.Markup = "<span size=\"xx-small\"> </span>";
   messageVBox.PackEnd(spacer, false, false, 0);
   closeWindowTimeoutID = 0;
  }
  private void OnLinkClicked(object sender, LinkClickedEventArgs args)
  {
   if (LinkClicked != null)
    LinkClicked(this, args);
  }
  protected override void OnShown()
  {
   base.OnShown();
   if(closeWindowTimeoutID == 0 && timeout > 0)
   {
    closeWindowTimeoutID = GLib.Timeout.Add(timeout, new GLib.TimeoutHandler(
      HideWindowCallback));
   }
  }
  private bool HideWindowCallback()
  {
   this.Hide();
   this.Destroy();
   return false;
  }
  private void OnCloseEvent(object obj, ButtonPressEventArgs args)
  {
   this.Hide();
   this.Destroy();
  }
  protected override void OnSizeAllocated(Gdk.Rectangle sized)
  {
   base.OnSizeAllocated(sized);
   if(background == null)
   {
    Gdk.Pixmap mask;
    RenderBubbles(this.RootWindow, sized, out activebackground,
       out inactivebackground, out mask);
    background = inactivebackground;
    if(mask != null)
     this.ShapeCombineMask(mask, 0, 0);
    else
     Console.WriteLine("Novell.iFolder.NotifyWindow: mask was null");
   }
  }
  private void RenderBubbles(Gdk.Window win, Gdk.Rectangle size,
      out Pixbuf pbactive, out Pixbuf pbinactive,
      out Gdk.Pixmap pbpm)
  {
   int pmHeight, pmWidth;
   Gdk.Pixmap daPixmap;
   Gdk.Pixmap daOtherPixmap;
   pmHeight = size.Height - (wbsize * 2);
   pmWidth = size.Width - (wbsize * 2);
   Gdk.GC gc = new Gdk.GC(win);
   Gdk.Pixmap pm = new Pixmap(win, size.Width, size.Height, -1);
   gc.RgbFgColor = new Gdk.Color(255, 255, 255);
   pm.DrawRectangle(gc, true, 0, 0, size.Width, size.Height);
   gc.RgbFgColor = new Gdk.Color(249, 253, 202);
   Gdk.Point[] roundedSquare = CalculateRect(wbsize, wbsize,
          pmWidth, pmHeight);
   pm.DrawPolygon(gc, true, roundedSquare);
   Gdk.Point[] roundedborder = CalculateRect(wbsize, wbsize,
            pmWidth - 1, pmHeight - 1);
   gc.RgbFgColor = new Gdk.Color(0, 0, 0);
   pm.DrawPolygon(gc, false, roundedborder);
   Gdk.Point[] balloonptr = CalcPointerMoveWindow( size.Width,
             size.Height );
   gc.RgbFgColor = new Gdk.Color(249, 253, 202);
   pm.DrawPolygon(gc, true, balloonptr);
   gc.RgbFgColor = new Gdk.Color(0, 0, 0);
   pm.DrawLine(gc, balloonptr[0].X, balloonptr[0].Y-1, balloonptr[1].X,
       balloonptr[1].Y);
   pm.DrawLine(gc, balloonptr[1].X, balloonptr[1].Y, balloonptr[2].X,
       balloonptr[2].Y-1);
   Gdk.Pixbuf pb = new Pixbuf(Gdk.Colorspace.Rgb, false,
      8, size.Width, size.Height);
   pb = Pixbuf.FromDrawable( pm, pm.Colormap, 0, 0, 0, 0,
      size.Width, size.Height);
   pb = pb.AddAlpha(true, 255, 255,255);
   RenderPixmapAndMask(pb, out daPixmap, out daOtherPixmap, 2);
   pbactive = pb;
   pbpm = daOtherPixmap;
   gc.RgbFgColor = new Gdk.Color(255, 255, 255);
   pm.DrawRectangle(gc, true, 0, 0, size.Width, size.Height);
   gc.RgbFgColor = new Gdk.Color(0, 0, 0);
   pm.DrawPolygon(gc, false, roundedborder);
   gc.RgbFgColor = new Gdk.Color(255, 255, 255);
   pm.DrawPolygon(gc, true, balloonptr);
   gc.RgbFgColor = new Gdk.Color(0, 0, 0);
   pm.DrawLine(gc, balloonptr[0].X, balloonptr[0].Y-1, balloonptr[1].X,
       balloonptr[1].Y);
   pm.DrawLine(gc, balloonptr[1].X, balloonptr[1].Y, balloonptr[2].X,
       balloonptr[2].Y - 1);
   pb = Pixbuf.FromDrawable( pm,
      pm.Colormap, 0, 0, 0, 0, size.Width, size.Height);
   pbinactive = pb;
  }
  protected Gdk.Point[] CalcPointerMoveWindow( int width, int height )
  {
   int parentX, parentY, parentWidth, parentHeight, parentDepth;
   int midParentX, midParentY, posX, posY;
   int ptsize = wbsize;
   bool drawRight, drawDown;
   parentWidget.GdkWindow.GetGeometry(out parentX, out parentY,
            out parentWidth,
            out parentHeight,
            out parentDepth);
   parentWidget.GdkWindow.GetOrigin(out parentX, out parentY);
   midParentX = parentX + (parentWidth / 2);
   midParentY = parentY + (parentHeight / 2);
   if(parentX >= (this.Screen.Width / 2) )
   {
    drawRight = false;
    posX = midParentX - width;
   }
   else
   {
    drawRight = true;
    posX = midParentX;
   }
   if(parentY >= (this.Screen.Height / 2) )
   {
    drawDown = false;
    posY = midParentY - height;
   }
   else
   {
    drawDown = true;
    posY = midParentY;
   }
   Move(posX, posY);
   ArrayList list = new ArrayList();
   if(drawRight)
   {
    if(drawDown)
    {
     list.Add(new Point( (wbsize),
          (wbsize + ptsize) ));
     list.Add(new Point( 0, 0 ));
     list.Add(new Point( (wbsize + ptsize),
          (wbsize) ));
    }
    else
    {
     list.Add(new Point( (wbsize + ptsize),
          (height - wbsize) ));
     list.Add(new Point( 0, height ));
     list.Add(new Point( (wbsize),
          (height - wbsize - ptsize) ));
    }
   }
   else
   {
    if(drawDown)
    {
     list.Add(new Point( (width - wbsize - ptsize),
          (wbsize) ));
     list.Add(new Point( width, 0 ));
     list.Add(new Point( (width - wbsize),
          (wbsize + ptsize) ));
    }
    else
    {
     list.Add(new Point( (width - wbsize - ptsize),
          (height - wbsize) ));
     list.Add(new Point( width, height ));
     list.Add(new Point( (width - wbsize),
          (height - wbsize - ptsize) ));
    }
   }
   return (Gdk.Point[]) (list.ToArray(typeof(Gdk.Point)));
  }
  protected Gdk.Point[] CalculateRect( int xorg, int yorg,
            int height, int width)
  {
   ArrayList list = new ArrayList();
   list.Add(new Point(xorg, yorg + 4));
   list.Add(new Point(xorg + 1, yorg + 4));
   list.Add(new Point(xorg + 1, yorg + 2));
   list.Add(new Point(xorg + 2, yorg + 2));
   list.Add(new Point(xorg + 2, yorg + 1));
   list.Add(new Point(xorg + 4, yorg + 1));
   list.Add(new Point(xorg + 4, yorg));
   list.Add(new Point( (xorg + height) - 4, yorg));
   list.Add(new Point( (xorg + height) - 4, yorg + 1));
   list.Add(new Point( (xorg + height) - 2, yorg + 1));
   list.Add(new Point( (xorg + height) - 2, yorg + 2));
   list.Add(new Point( (xorg + height) - 1, yorg + 2));
   list.Add(new Point( (xorg + height) - 1, yorg + 4));
   list.Add(new Point( (xorg + height), yorg + 4));
   list.Add(new Point( (xorg + height), (yorg + width) - 4));
   list.Add(new Point( (xorg + height) - 1, (yorg + width) - 4));
   list.Add(new Point( (xorg + height) - 1, (yorg + width) - 2));
   list.Add(new Point( (xorg + height) - 2, (yorg + width) - 2));
   list.Add(new Point( (xorg + height) - 2, (yorg + width) - 1));
   list.Add(new Point( (xorg + height) - 4, (yorg + width) - 1));
   list.Add(new Point( (xorg + height) - 4, (yorg + width)));
   list.Add(new Point( xorg + 4, (yorg + width)));
   list.Add(new Point( xorg + 4, (yorg + width) - 1));
   list.Add(new Point( xorg + 2, (yorg + width) - 1));
   list.Add(new Point( xorg + 2, (yorg + width) - 2));
   list.Add(new Point( xorg + 1, (yorg + width) - 2));
   list.Add(new Point( xorg + 1, (yorg + width) - 4));
   list.Add(new Point( xorg, (yorg + width) - 4));
   return (Gdk.Point[]) (list.ToArray(typeof(Gdk.Point)));
  }
  protected override bool OnExposeEvent(Gdk.EventExpose args)
  {
   if(isSelected)
    background = activebackground;
   else
    background = inactivebackground;
   GdkWindow.DrawPixbuf(Style.BackgroundGC(State), background, 0, 0,
    0, 0, background.Width, background.Height,
    Gdk.RgbDither.None, 0, 0);
   return base.OnExposeEvent(args);
  }
  protected override bool OnEnterNotifyEvent(Gdk.EventCrossing evnt)
  {
   if(evnt.Detail == Gdk.NotifyType.Inferior)
    return false;
   if(closeWindowTimeoutID != 0)
   {
    GLib.Source.Remove(closeWindowTimeoutID);
    closeWindowTimeoutID = 0;
   }
   isSelected = true;
   QueueDraw();
   detailsTextView.ModifyBase(StateType.Normal, activeBackgroundColor);
   return false;
  }
  protected override bool OnLeaveNotifyEvent(Gdk.EventCrossing evnt)
  {
   if(evnt.Detail == Gdk.NotifyType.Inferior)
    return false;
   isSelected = false;
   QueueDraw();
   detailsTextView.ModifyBase(StateType.Normal, inactiveBackgroundColor);
   if(closeWindowTimeoutID != 0)
   {
    GLib.Source.Remove(closeWindowTimeoutID);
    closeWindowTimeoutID = 0;
   }
   if (timeout > 0)
   {
    closeWindowTimeoutID = GLib.Timeout.Add(timeout, new GLib.TimeoutHandler(
       HideWindowCallback));
   }
   return false;
  }
  [DllImport("libgtk-x11-2.0.so.0")]
  static extern IntPtr gdk_pixmap_create_from_xpm(IntPtr drawable,
   out IntPtr mask, ref Gdk.Color transparent_color, string filename);
  public static Gdk.Pixmap CreateFromXpm(Gdk.Drawable drawable,
   out Gdk.Pixmap mask, Gdk.Color transparent_color, string filename)
  {
   IntPtr mask_handle;
   IntPtr raw_ret = gdk_pixmap_create_from_xpm(drawable.Handle,
    out mask_handle, ref transparent_color, filename);
   Gdk.Pixmap ret;
   if (raw_ret == IntPtr.Zero)
    ret = null;
   else
    ret = new Gdk.Pixmap(raw_ret);
   mask = new Gdk.Pixmap(mask_handle);
   return ret;
  }
  [DllImport("libgtk-x11-2.0.so.0")]
  static extern void gdk_pixbuf_render_pixmap_and_mask(IntPtr raw,
    out IntPtr pixmap_return, out IntPtr mask_return,
    int alpha_threshold);
  public void RenderPixmapAndMask(Gdk.Pixbuf pixbuf,
    out Gdk.Pixmap pixmap_return, out Gdk.Pixmap mask_return,
    int alpha_threshold)
  {
   IntPtr pm_handle;
   IntPtr bm_handle;
   gdk_pixbuf_render_pixmap_and_mask(pixbuf.Handle, out pm_handle,
     out bm_handle, alpha_threshold);
   pixmap_return = new Gdk.Pixmap(pm_handle);
   mask_return = new Gdk.Pixmap(bm_handle);
  }
 }
 public class LinkTextView : TextView
 {
  private int currentCursor;
  private Gdk.Cursor handCursor;
  private bool hoveringOverLink;
  public event LinkClickedEventHandler LinkClicked;
  public LinkTextView(string linkText) : base()
  {
   currentCursor = -1;
   handCursor = new Gdk.Cursor (Gdk.CursorType.Hand2);
   hoveringOverLink = false;
   string xmlLinkText = "<message>" + linkText + "</message>";
   XmlDocument linkTextDom = new XmlDocument();
   linkTextDom.LoadXml(xmlLinkText);
   TextTagTable textTagTable = CreateTextTagTable(linkTextDom);
   TextBuffer textBuffer = new TextBuffer(textTagTable);
   FormatTextBuffer(textBuffer, linkTextDom.DocumentElement);
   this.Buffer = textBuffer;
  }
  private TextTagTable CreateTextTagTable(XmlNode topLevelNode)
  {
   TextTagTable textTagTable = new TextTagTable();
   TextTag smallFontTag = new TextTag("small-font");
   smallFontTag.Scale = Pango.Scale.Small;
   textTagTable.Add(smallFontTag);
   XmlNodeList linkNodes = topLevelNode.SelectNodes("//a");
   if (linkNodes != null)
   {
    foreach(XmlNode linkNode in linkNodes)
    {
     XmlAttribute href = linkNode.Attributes["href"];
     if (href != null)
     {
      string textTagName = href.Value;
      if (textTagTable.Lookup(textTagName) != null)
       continue;
      TextTag textTag = new TextTag(textTagName);
      textTag.Underline = Pango.Underline.Single;
      textTag.Foreground = "blue";
      textTag.TextEvent +=
       new TextEventHandler(OnTextEvent);
      textTagTable.Add(textTag);
     }
    }
   }
   return textTagTable;
  }
  private void FormatTextBuffer(TextBuffer textBuffer, XmlNode linkTextNode)
  {
   XmlNodeList childNodes = linkTextNode.ChildNodes;
   foreach(XmlNode childNode in childNodes)
   {
    if (childNode.Name.Equals("a"))
    {
     XmlAttribute href = childNode.Attributes["href"];
     if (href != null)
     {
      string textTagName = href.Value;
      TextTag textTag = textBuffer.TagTable.Lookup(textTagName);
      if (textTag != null)
      {
       TextMark startTagMark = textBuffer.CreateMark(textTagName, textBuffer.EndIter, true);
       textBuffer.InsertAtCursor(childNode.InnerText);
       TextIter startTagIter = textBuffer.GetIterAtMark(startTagMark);
       TextIter endTagIter = textBuffer.EndIter;
       textBuffer.ApplyTag(textTag, startTagIter, endTagIter);
      }
     }
     else
      textBuffer.InsertAtCursor(childNode.InnerText);
    }
    else
    {
     textBuffer.InsertAtCursor(childNode.InnerText);
    }
   }
   textBuffer.ApplyTag("small-font", textBuffer.StartIter, textBuffer.EndIter);
  }
  private void OnTextEvent(object sender, TextEventArgs args)
  {
   if (LinkClicked != null && args.Event.Type == EventType.ButtonPress)
   {
    TextTag textTag = (TextTag)sender;
    string linkID = textTag.Name;
    LinkClicked(this, new LinkClickedEventArgs(linkID));
   }
  }
  protected override bool OnMotionNotifyEvent(Gdk.EventMotion eventMotion)
  {
   int x, y;
   Gdk.ModifierType state;
   this.WindowToBufferCoords(TextWindowType.Widget,
           (int) eventMotion.X,
           (int) eventMotion.Y,
           out x, out y);
   SetTextViewCursorIfAppropriate(x, y);
   this.GdkWindow.GetPointer(out x, out y, out state);
   return false;
  }
  private void SetTextViewCursorIfAppropriate(int x, int y)
  {
   bool hovering = false;
   TextIter iter = this.GetIterAtLocation(x, y);
   foreach(TextTag tag in iter.Tags)
   {
    if (tag.Name != null && !tag.Name.Equals("small-font"))
    {
     hovering = true;
     break;
    }
   }
   if (hovering != hoveringOverLink)
   {
    Gdk.Window window = this.GetWindow(Gtk.TextWindowType.Text);
    hoveringOverLink = hovering;
    if (hoveringOverLink)
    {
     window.Cursor = handCursor;
     currentCursor = 1;
    }
    else
    {
     window.Cursor = null;
     currentCursor = 0;
    }
   }
   if (!hoveringOverLink && currentCursor != 0)
   {
    Gdk.Window window = this.GetWindow(Gtk.TextWindowType.Text);
    window.Cursor = null;
    currentCursor = 0;
   }
  }
  protected override bool OnButtonPressEvent(Gdk.EventButton eventButton)
  {
   return false;
  }
  protected override bool OnButtonReleaseEvent(Gdk.EventButton eventButton)
  {
   return false;
  }
 }
 public delegate void LinkClickedEventHandler(object sender, LinkClickedEventArgs args);
 public class LinkClickedEventArgs : EventArgs
 {
  private string linkID;
  public LinkClickedEventArgs(string linkID)
  {
   this.linkID = linkID;
  }
  public string LinkID
  {
   get{ return this.linkID; }
  }
 }
}
