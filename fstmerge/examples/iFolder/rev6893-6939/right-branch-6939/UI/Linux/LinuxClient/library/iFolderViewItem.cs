using System;
using System.Collections;
using Gtk;
using Novell.iFolder.Controller;
namespace Novell.iFolder
{
 public class iFolderViewItem : EventBox
 {
  private iFolderHolder holder;
  private iFolderViewGroup group;
  private TreeIter iter;
  private int maxWidth;
  private int widthForLabels;
  private bool bSelected;
  private static Gdk.Pixbuf OKPixbuf = null;
  private static Gdk.Pixbuf OKPixbufSpotlight = null;
  private static Gdk.Pixbuf EncryptedOKPixbuf = null;
  private static Gdk.Pixbuf EncryptedOKPixbufSpotlight = null;
  private static Gdk.Pixbuf SharedOKPixbuf = null;
  private static Gdk.Pixbuf SharedOKPixbufSpotlight = null;
  private static Gdk.Pixbuf WaitPixbuf = null;
  private static Gdk.Pixbuf WaitPixbufSpotlight = null;
  private static Gdk.Pixbuf SubscriptionPixbuf = null;
  private static Gdk.Pixbuf SubscriptionPixbufSpotlight = null;
  private static Gdk.Pixbuf SyncPixbuf = null;
  private static Gdk.Pixbuf SyncPixbufSpotlight = null;
  private static Gdk.Pixbuf WarningPixbuf = null;
  private static Gdk.Pixbuf WarningPixbufSpotlight = null;
  private static Gdk.Pixbuf ErrorPixbuf = null;
  private static Gdk.Pixbuf ErrorPixbufSpotlight = null;
  private Gdk.Pixbuf normalPixbuf;
  private Gdk.Pixbuf spotlightPixbuf;
  private Table table;
  private Image image;
  private Label nameLabel;
  private Label locationLabel;
  private Label statusLabel;
  private bool bMouseIsHovering;
  private string currentName;
  private string currentLocation;
  private string currentStatus;
  public event EventHandler LeftClicked;
  public event EventHandler RightClicked;
  public event EventHandler DoubleClicked;
  public iFolderHolder Holder
  {
   get{ return holder; }
   set
   {
    holder = value;
   }
  }
  public iFolderViewGroup Group
  {
   get{ return group; }
  }
        public TreeIter TreeIter
  {
   get{ return iter; }
  }
  public bool Selected
  {
   get{ return bSelected; }
   set
   {
    bSelected = value;
    if (bSelected)
     this.State = StateType.Selected;
    else
     this.State = StateType.Normal;
   }
  }
        public iFolderViewItem(iFolderHolder holder, iFolderViewGroup group, TreeIter iter, int maxWidth)
  {
   this.holder = holder;
   this.group = group;
   this.iter = iter;
   this.maxWidth = maxWidth;
   this.CanFocus = true;
   this.bSelected = false;
   this.bMouseIsHovering = false;
   this.ModifyBg(StateType.Normal, this.Style.Base(StateType.Normal));
   this.ModifyBase(StateType.Normal, this.Style.Base(StateType.Normal));
   currentName = "";
   currentLocation = "";
   currentStatus = "";
   LoadImages();
   SetPixbufs();
   this.Add(CreateWidgets());
   this.WidthRequest = this.maxWidth;
   this.Realized +=
    new EventHandler(OnWidgetRealized);
   TargetEntry[] targets =
    new TargetEntry[]
    {
     new TargetEntry ("text/ifolder-id", 0, (uint)iFolderWindow.DragTargetType.iFolderID)
    };
   this.DragDataGet += new DragDataGetHandler(HandleDragDataGet);
   Drag.SourceSet(this, Gdk.ModifierType.Button1Mask, targets, Gdk.DragAction.Move);
  }
  public static Gdk.Pixbuf CreateEmblemedPixbuf(Gdk.Pixbuf icon, Gdk.Pixbuf emblem)
  {
   if (icon == null || emblem == null) return null;
   if (icon.Width <= emblem.Width || icon.Height <= emblem.Height) return null;
   Gdk.Pixbuf dest = new Gdk.Pixbuf(icon.Colorspace, true, icon.BitsPerSample, icon.Width, icon.Height);
   dest.Fill(0x00000000);
   try
   {
    icon.Composite(dest,
        0,
        0,
        dest.Width,
        dest.Height,
        0,
        0,
        1.0,
        1.0,
        Gdk.InterpType.Bilinear,
        255);
    emblem.Composite(dest,
        icon.Width - emblem.Width - 1,
        icon.Height - emblem.Height - 1,
        emblem.Width,
        emblem.Height,
        icon.Width - emblem.Width - 1,
        icon.Height - emblem.Height - 1,
        1.0,
        1.0,
        Gdk.InterpType.Bilinear,
        255);
   }
   catch(Exception e)
   {
    dest = null;
   }
   return dest;
  }
  public void LoadImages()
  {
   lock(typeof(iFolderViewItem))
   {
    if (OKPixbuf == null)
    {
     OKPixbuf = new Gdk.Pixbuf(Util.ImagesPath("ifolder48.png"));
     OKPixbufSpotlight =
      Util.EelCreateSpotlightPixbuf(OKPixbuf);
    }
    if (EncryptedOKPixbuf == null)
    {
     EncryptedOKPixbuf = new Gdk.Pixbuf(Util.ImagesPath("encrypt-ilock-48.png"));
     EncryptedOKPixbufSpotlight =
      Util.EelCreateSpotlightPixbuf(EncryptedOKPixbuf);
    }
    if (SharedOKPixbuf == null)
    {
     SharedOKPixbuf = new Gdk.Pixbuf(Util.ImagesPath("ifolder_user_48.png"));
     SharedOKPixbufSpotlight =
      Util.EelCreateSpotlightPixbuf(SharedOKPixbuf);
    }
    if (SubscriptionPixbuf == null)
    {
     SubscriptionPixbuf =
      new Gdk.Pixbuf(Util.ImagesPath("ifolder-download48.png"));
     SubscriptionPixbufSpotlight =
      Util.EelCreateSpotlightPixbuf(SubscriptionPixbuf);
    }
    if (WarningPixbuf == null)
    {
     WarningPixbuf =
      new Gdk.Pixbuf(Util.ImagesPath("ifolder-warning48.png"));
     WarningPixbufSpotlight =
      Util.EelCreateSpotlightPixbuf(WarningPixbuf);
    }
    if (ErrorPixbuf == null)
    {
     ErrorPixbuf =
      new Gdk.Pixbuf(Util.ImagesPath("ifolder-error48.png"));
     ErrorPixbufSpotlight =
      Util.EelCreateSpotlightPixbuf(ErrorPixbuf);
    }
    if (SyncPixbuf == null)
    {
     SyncPixbuf =
      new Gdk.Pixbuf(Util.ImagesPath("ifolder-sync48.png"));
     SyncPixbufSpotlight =
      Util.EelCreateSpotlightPixbuf(SyncPixbuf);
    }
    if (WaitPixbuf == null)
    {
     WaitPixbuf =
      new Gdk.Pixbuf(Util.ImagesPath("ifolder-waiting48.png"));
     WaitPixbufSpotlight =
      Util.EelCreateSpotlightPixbuf(WaitPixbuf);
    }
   }
  }
  private void OnWidgetRealized(object o, EventArgs args)
  {
   Refresh();
  }
  public void Refresh()
  {
   SetPixbufs();
   if (bMouseIsHovering)
   {
    if (image.Pixbuf != spotlightPixbuf)
     image.Pixbuf = spotlightPixbuf;
   }
   else
   {
    if (image.Pixbuf != normalPixbuf)
     image.Pixbuf = normalPixbuf;
   }
   UpdateNameLabel();
   UpdateLocationLabel();
   UpdateStatusLabel();
  }
  private Widget CreateWidgets()
  {
   table = new Table(1, 2, false);
   table.ColumnSpacing = 12;
   table.BorderWidth = 4;
   image = new Image(normalPixbuf);
   table.Attach(image,
       0, 1,
       0, 1,
       AttachOptions.Shrink,
       0, 0, 0);
   VBox vbox = new VBox(false, 0);
   table.Attach(vbox,
       1, 2,
       0, 1,
       AttachOptions.Expand | AttachOptions.Fill,
       0, 0, 0);
   widthForLabels = (int)maxWidth - (int)normalPixbuf.Width
        - (int)table.ColumnSpacing - (int)(table.BorderWidth * 2);
   nameLabel = new Label("<span size=\"large\"></span>");
   vbox.PackStart(nameLabel, false, false, 0);
   nameLabel.UseMarkup = true;
   nameLabel.UseUnderline = false;
   nameLabel.Xalign = 0;
   Requisition req = nameLabel.SizeRequest();
   nameLabel.SetSizeRequest(widthForLabels, -1);
   Util.GtkLabelSetMaxWidthChars(nameLabel, widthForLabels);
   Util.GtkLabelSetEllipsize(nameLabel, true);
   locationLabel = new Label("<span size=\"small\"></span>");
   vbox.PackStart(locationLabel, false, false, 0);
   locationLabel.UseMarkup = true;
   locationLabel.UseUnderline = false;
   locationLabel.ModifyFg(StateType.Normal, this.Style.Base(StateType.Active));
   locationLabel.Xalign = 0;
   req = locationLabel.SizeRequest();
   locationLabel.SetSizeRequest(widthForLabels, req.Height);
   Util.GtkLabelSetMaxWidthChars(locationLabel, widthForLabels);
   Util.GtkLabelSetEllipsize(locationLabel, true);
   statusLabel = new Label("<span size=\"small\"></span>");
   vbox.PackStart(statusLabel, false, false, 0);
   statusLabel.UseMarkup = true;
   statusLabel.UseUnderline = false;
   statusLabel.ModifyFg(StateType.Normal, this.Style.Base(StateType.Active));
   statusLabel.Xalign = 0;
   req = statusLabel.SizeRequest();
   statusLabel.SetSizeRequest(widthForLabels, req.Height);
   Util.GtkLabelSetMaxWidthChars(statusLabel, widthForLabels);
   Util.GtkLabelSetEllipsize(statusLabel, true);
   return table;
  }
  protected override bool OnEnterNotifyEvent(Gdk.EventCrossing evnt)
  {
   bMouseIsHovering = true;
   image.Pixbuf = spotlightPixbuf;
   locationLabel.ModifyFg(StateType.Normal, this.Style.Foreground(StateType.Normal));
   statusLabel.ModifyFg(StateType.Normal, this.Style.Foreground(StateType.Normal));
   return false;
  }
  protected override bool OnLeaveNotifyEvent(Gdk.EventCrossing evnt)
  {
   bMouseIsHovering = false;
   image.Pixbuf = normalPixbuf;
   locationLabel.ModifyFg(StateType.Normal, this.Style.Base(StateType.Active));
   statusLabel.ModifyFg(StateType.Normal, this.Style.Base(StateType.Active));
   return false;
  }
  protected override bool OnButtonPressEvent(Gdk.EventButton evnt)
  {
   switch(evnt.Button)
   {
    case 1:
     if (LeftClicked != null)
      LeftClicked(this, EventArgs.Empty);
     if (evnt.Type == Gdk.EventType.TwoButtonPress)
     {
      if (DoubleClicked != null)
       DoubleClicked(this, EventArgs.Empty);
     }
     break;
    case 3:
     if (RightClicked != null)
      RightClicked(this, EventArgs.Empty);
     break;
    default:
     break;
   }
   return false;
  }
  private void SetPixbufs()
  {
   Gdk.Pixbuf newNormalPixbuf = null;
   Gdk.Pixbuf newSpotlightPixbuf = null;
   if (holder.iFolder.IsSubscription)
   {
     newNormalPixbuf = SubscriptionPixbuf;
     newSpotlightPixbuf = SubscriptionPixbufSpotlight;
   }
   else
   {
    if (holder.State == iFolderState.Synchronizing ||
     holder.State == iFolderState.SynchronizingLocal)
    {
     newNormalPixbuf = SyncPixbuf;
     newSpotlightPixbuf = SyncPixbufSpotlight;
    }
    else
    {
     if (holder.iFolder.HasConflicts)
     {
      newNormalPixbuf = WarningPixbuf;
      newSpotlightPixbuf = WarningPixbufSpotlight;
     }
     else
     {
      switch (holder.State)
      {
       case iFolderState.Disconnected:
         newNormalPixbuf = WarningPixbuf;
         newSpotlightPixbuf = WarningPixbufSpotlight;
        break;
       case iFolderState.NoPassphrase:
        newNormalPixbuf = WarningPixbuf;
        newSpotlightPixbuf = WarningPixbufSpotlight;
        break;
       case iFolderState.FailedSync:
        newNormalPixbuf = ErrorPixbuf;
        newSpotlightPixbuf = ErrorPixbufSpotlight;
        break;
       case iFolderState.Initial:
        newNormalPixbuf = WaitPixbuf;
        newSpotlightPixbuf = WaitPixbufSpotlight;
        break;
       case iFolderState.Normal:
       default:
        if (holder.ObjectsToSync > 0)
        {
         newNormalPixbuf = WaitPixbuf;
         newSpotlightPixbuf = WaitPixbufSpotlight;
        }
        else
        {
         if( holder.iFolder.encryptionAlgorithm == null || holder.iFolder.encryptionAlgorithm == "")
         {
          if( holder.iFolder.shared == true)
          {
           newNormalPixbuf = SharedOKPixbuf;
           newSpotlightPixbuf = SharedOKPixbufSpotlight;
          }
          else
          {
           newNormalPixbuf = OKPixbuf;
           newSpotlightPixbuf = OKPixbufSpotlight;
          }
         }
         else
         {
          newNormalPixbuf = EncryptedOKPixbuf;
          newSpotlightPixbuf = EncryptedOKPixbufSpotlight;
         }
        }
        break;
      }
     }
    }
   }
   if (newNormalPixbuf != null && normalPixbuf != newNormalPixbuf)
    normalPixbuf = newNormalPixbuf;
   if (newSpotlightPixbuf != null && spotlightPixbuf != newSpotlightPixbuf)
    spotlightPixbuf = newSpotlightPixbuf;
  }
  private void UpdateNameLabel()
  {
   string text = holder.iFolder.Name;
   if (text == null || text.Length == 0)
    text = Util.GS("Unknown");
   if (currentName != text)
   {
    currentName = text;
    string potentialMarkup =
     string.Format("<span size=\"large\">{0}</span>",
      GLib.Markup.EscapeText(text));
    nameLabel.Markup = potentialMarkup;
   }
  }
  private void UpdateLocationLabel()
  {
   string text = null;
   if (holder.iFolder.IsSubscription)
    text = holder.iFolder.Owner;
   else
    text = holder.iFolder.UnManagedPath;
   if (text == null || text.Length == 0)
    text = Util.GS("Unknown");
   if (currentLocation != text)
   {
    string potentialMarkup;
    if (holder.iFolder.IsSubscription)
     potentialMarkup =
      string.Format("<span size=\"medium\">{0}: {1}</span>",
           Util.GS("Owner"),
           GLib.Markup.EscapeText(text));
    else
     potentialMarkup =
      string.Format("<span size=\"medium\">{0}</span>",
           GLib.Markup.EscapeText(text));
    locationLabel.Markup = potentialMarkup;
   }
  }
  private void UpdateStatusLabel()
  {
   string text = null;
   if (holder.iFolder.IsSubscription)
    text = "";
   else
    text = holder.StateString;
   if (text == null || text.Length == 0)
    text = Util.GS("Unknown");
   if (currentStatus != text)
   {
    currentStatus = text;
    string potentialMarkup;
    if (holder.iFolder.IsSubscription)
     potentialMarkup = "";
    else
     potentialMarkup =
      string.Format("<span size=\"medium\">{0}</span>",
            GLib.Markup.EscapeText(text));
    statusLabel.Markup = potentialMarkup;
   }
  }
  private void HandleDragDataGet(object o, DragDataGetArgs args)
  {
   if (holder == null || holder.iFolder == null) return;
   string ifolderID = this.holder.iFolder.ID;
   if (ifolderID == null) return;
   switch (args.Info)
   {
    case (uint) iFolderWindow.DragTargetType.iFolderID:
     Byte[] data = System.Text.Encoding.UTF8.GetBytes(ifolderID);
     Gdk.Atom[] targets = args.Context.Targets;
     args.SelectionData.Set(targets[0], 8, data, data.Length);
     break;
   }
  }
 }
}
