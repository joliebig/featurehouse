


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

  private static Gdk.Pixbuf OKFolder = null;
  private static Gdk.Pixbuf OKFolderSpotlight = null;

  private static Gdk.Pixbuf MyAvailableFolder = null;
  private static Gdk.Pixbuf MyAvailableFolderSpotlight = null;

  private static Gdk.Pixbuf SharedAvailableFolder = null;
  private static Gdk.Pixbuf SharedAvailableFolderSpotlight = null;

  private static Gdk.Pixbuf ConflictFolder = null;
  private static Gdk.Pixbuf ConflictFolderSpotlight = null;

  private static Gdk.Pixbuf SyncFolder = null;
  private static Gdk.Pixbuf SyncFolderSpotlight = null;

  private static Gdk.Pixbuf SyncWaitFolder = null;
  private static Gdk.Pixbuf SyncWaitFolderSpotlight = null;

  private static bool registeredForThemeChangeEvent = false;

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

  private DomainController domainController;




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

   domainController = DomainController.GetDomainController();

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
   if (OKFolder == null)
   {
    OKFolder =
     Util.LoadIcon("gnome-fs-directory", 48);
    if (OKFolder == null)
     OKFolder =
      new Gdk.Pixbuf(
       Util.ImagesPath("ok-folder64.png"));
    Gdk.Pixbuf okEmblem =
     new Gdk.Pixbuf(Util.ImagesPath("emblem-ok.png"));
    if (okEmblem == null || OKFolder == null)
     Console.WriteLine("     =========== HUH?");
    else
    {
     Gdk.Pixbuf okFolder = CreateEmblemedPixbuf(OKFolder, okEmblem);
     if (okFolder != null)
      OKFolder = okFolder;
    }
    if (OKFolder == null)
     Console.WriteLine("******* GRRRR ********");
   }
   if (OKFolderSpotlight == null)
   {
    OKFolderSpotlight = OKFolder.Copy();
   }
   if (MyAvailableFolder == null)
   {
    MyAvailableFolder =
     Util.LoadIcon("gnome-fs-share", 48);
    if (MyAvailableFolder == null)
    {
     MyAvailableFolder =
      new Gdk.Pixbuf(
       Util.ImagesPath("my-available-folder64.png"));
    }
   }
   if (MyAvailableFolderSpotlight == null)
   {
    MyAvailableFolderSpotlight =
     Util.LoadIcon("gnome-fs-share", 48);
    if (MyAvailableFolderSpotlight == null)
    {
     MyAvailableFolderSpotlight =
      new Gdk.Pixbuf(
       Util.ImagesPath("my-available-folder-spotlight64.png"));
    }
   }
   if (SharedAvailableFolder == null)
   {
    SharedAvailableFolder =
     Util.LoadIcon("gnome-fs-share", 48);
    if (SharedAvailableFolder == null)
     SharedAvailableFolder =
      new Gdk.Pixbuf(
       Util.ImagesPath("shared-available-folder64.png"));
    Gdk.Pixbuf emblem =
     new Gdk.Pixbuf(Util.ImagesPath("emblem-shared.png"));
    if (emblem == null || SharedAvailableFolder == null)
     Console.WriteLine("     =========== HUH?");
    else
    {
     Gdk.Pixbuf tmpPixbuf = CreateEmblemedPixbuf(SharedAvailableFolder, emblem);
     if (tmpPixbuf != null)
      SharedAvailableFolder = tmpPixbuf;
    }
   }
   if (SharedAvailableFolderSpotlight == null)
   {
    SharedAvailableFolderSpotlight =
     Util.LoadIcon("gnome-fs-share", 48);
    if (SharedAvailableFolderSpotlight == null)
     SharedAvailableFolderSpotlight =
      new Gdk.Pixbuf(
       Util.ImagesPath("shared-available-folder-spotlight64.png"));
    Gdk.Pixbuf emblem =
     new Gdk.Pixbuf(Util.ImagesPath("emblem-shared.png"));
    if (emblem == null || SharedAvailableFolderSpotlight == null)
     Console.WriteLine("     =========== HUH?");
    else
    {
     Gdk.Pixbuf tmpPixbuf = CreateEmblemedPixbuf(SharedAvailableFolderSpotlight, emblem);
     if (tmpPixbuf != null)
      SharedAvailableFolderSpotlight = tmpPixbuf;
    }
   }
   if (ConflictFolder == null)
   {
    ConflictFolder =
     Util.LoadIcon("gnome-fs-directory", 48);
    if (ConflictFolder == null)
     ConflictFolder =
      new Gdk.Pixbuf(
       Util.ImagesPath("conflict-folder64.png"));
    Gdk.Pixbuf emblem =
     new Gdk.Pixbuf(Util.ImagesPath("emblem-conflict.png"));
    if (emblem == null || ConflictFolder == null)
     Console.WriteLine("     =========== HUH?");
    else
    {
     Gdk.Pixbuf tmpPixbuf = CreateEmblemedPixbuf(ConflictFolder, emblem);
     if (tmpPixbuf != null)
      ConflictFolder = tmpPixbuf;
    }
   }
   if (ConflictFolderSpotlight == null)
   {
    ConflictFolderSpotlight = ConflictFolder.Copy();
   }
   if (SyncFolder == null)
   {
    SyncFolder =
     Util.LoadIcon("gnome-fs-directory", 48);
    if (SyncFolder == null)
     SyncFolder =
      new Gdk.Pixbuf(
       Util.ImagesPath("sync-folder64.png"));
    Gdk.Pixbuf emblem =
     new Gdk.Pixbuf(Util.ImagesPath("emblem-sync.png"));
    if (emblem == null || SyncFolder == null)
     Console.WriteLine("     =========== HUH?");
    else
    {
     Gdk.Pixbuf tmpPixbuf = CreateEmblemedPixbuf(SyncFolder, emblem);
     if (tmpPixbuf != null)
      SyncFolder = tmpPixbuf;
    }
   }
   if (SyncFolderSpotlight == null)
   {
    SyncFolderSpotlight = SyncFolder.Copy();
   }
   if (SyncWaitFolder == null)
   {
    SyncWaitFolder =
     Util.LoadIcon("gnome-fs-directory", 48);
    if (SyncWaitFolder == null)
     SyncWaitFolder =
      new Gdk.Pixbuf(
       Util.ImagesPath("sync-wait-folder64.png"));
    Gdk.Pixbuf emblem =
     new Gdk.Pixbuf(Util.ImagesPath("emblem-sync-wait.png"));
    if (emblem == null || SyncWaitFolder == null)
     Console.WriteLine("     =========== HUH?");
    else
    {
     Gdk.Pixbuf tmpPixbuf = CreateEmblemedPixbuf(SyncWaitFolder, emblem);
     if (tmpPixbuf != null)
      SyncWaitFolder = tmpPixbuf;
    }
   }
   if (SyncWaitFolderSpotlight == null)
   {
    SyncWaitFolderSpotlight = SyncWaitFolder.Copy();
   }
   }
  }
  private void OnWidgetRealized(object o, EventArgs args)
  {
   Refresh();
  }
  public void Refresh()
  {
Console.WriteLine("iFolderViewItem.Refresh({0})", holder.iFolder.Name);
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
Console.WriteLine("iFolderViewItem.Refresh({0}) exiting", holder.iFolder.Name);
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
   nameLabel.SetSizeRequest(widthForLabels, req.Height);
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
    DomainInformation domain = domainController.GetDomain(holder.iFolder.DomainID);
    if (domain != null && domain.MemberUserID != holder.iFolder.OwnerID)
    {
     newNormalPixbuf = SharedAvailableFolder;
     newSpotlightPixbuf = SharedAvailableFolderSpotlight;
    }
    else
    {
     newNormalPixbuf = MyAvailableFolder;
     newSpotlightPixbuf = MyAvailableFolderSpotlight;
    }
   }
   else
   {
    if (holder.State == iFolderState.Synchronizing)
    {
     newNormalPixbuf = SyncFolder;
     newSpotlightPixbuf = SyncFolderSpotlight;
    }
    else if (holder.State == iFolderState.SynchronizingLocal)
    {
     newNormalPixbuf = SyncWaitFolder;
     newSpotlightPixbuf = SyncWaitFolderSpotlight;
    }
    else
    {
     if (holder.iFolder.HasConflicts)
     {
      newNormalPixbuf = ConflictFolder;
      newSpotlightPixbuf = ConflictFolderSpotlight;
     }
     else
     {
      switch (holder.State)
      {
       case iFolderState.Disconnected:
       case iFolderState.FailedSync:
        newNormalPixbuf = ConflictFolder;
        newSpotlightPixbuf = ConflictFolderSpotlight;
        break;
       case iFolderState.Initial:
        newNormalPixbuf = SyncWaitFolder;
        newSpotlightPixbuf = SyncWaitFolderSpotlight;
        break;
       case iFolderState.Normal:
       default:
        if (holder.ObjectsToSync > 0)
        {
         newNormalPixbuf = SyncWaitFolder;
         newSpotlightPixbuf = SyncWaitFolderSpotlight;
        }
        else
        {
         newNormalPixbuf = OKFolder;
         newSpotlightPixbuf = OKFolderSpotlight;
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
      string.Format("<span size=\"small\">{0}: {1}</span>",
           Util.GS("Owner"),
           GLib.Markup.EscapeText(text));
    else
     potentialMarkup =
      string.Format("<span size=\"small\">{0}</span>",
           GLib.Markup.EscapeText(text));
    locationLabel.Markup = potentialMarkup;
   }
  }
  private void UpdateStatusLabel()
  {
   string text = null;
   if (holder.iFolder.IsSubscription)
    text = "47";
   else
    text = holder.StateString;
   if (text == null || text.Length == 0)
    text = Util.GS("Unknown");
   if (currentStatus != text)
   {
    string potentialMarkup;
    if (holder.iFolder.IsSubscription)
     potentialMarkup =
      string.Format("<span size=\"small\">{0} {1}</span>",
           GLib.Markup.EscapeText(text),
           Util.GS("MB"));
    else
     potentialMarkup =
      string.Format("<span size=\"small\">{0}: {1}</span>",
           Util.GS("Status"),
           GLib.Markup.EscapeText(text));
    statusLabel.Markup = potentialMarkup;
   }
  }
  private void HandleDragDataGet(object o, DragDataGetArgs args)
  {
Console.WriteLine("iFolderViewItem.HandleDragDataGet()");
   if (holder == null || holder.iFolder == null) return;
   string ifolderID = this.holder.iFolder.ID;
   if (ifolderID == null) return;
Console.WriteLine("\t{0}", args.Info);
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
