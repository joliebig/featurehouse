


using System;
using System.Collections;
using Gtk;

namespace Novell.iFolder
{
 public class iFolderViewGroup : VBox
 {
  private VBox contentVBox;

  private Notebook groupNotebook;
  private VBox mainPage;
  private VBox emptyPage;
  private VBox emptySearchPage;

  private string name;
  private TreeModelFilter model;

  private Hashtable items;

  private Label nameLabel;
  private Table table;

  private iFolderViewGroupSelection selection;


  private static uint resizeTimeout = 20;
  private uint resizeTableTimeoutID;


  private static uint rebuildTimeout = 20;
  private uint rebuildTableTimeoutID;

  private static int ItemMaxWidth = 300;

  private int currentWidth;

  private bool bFirstTableBuild;

  private bool bVisibleWhenEmpty;
  private Widget emptyWidget;
  private Widget emptySearchWidget;
  private Entry searchEntry;
  private uint searchTimeoutID;

  private bool alreadyDisposed;




  private static object threadCheckLockObject = new object();
  private static int CurrentThreadID = 0;
  private static void CheckThread()
  {
   lock(threadCheckLockObject)
   {
    int newThreadID = System.Threading.Thread.CurrentThread.GetHashCode();
    if (newThreadID != CurrentThreadID)
    {
     if (CurrentThreadID == 0)
      CurrentThreadID = newThreadID;
     else
     {
      Console.WriteLine("****** WARNING: iFolderViewGroup called from different thread: {0}", newThreadID);
      Console.WriteLine("\tOld Thread: {0}", CurrentThreadID);
      Console.WriteLine("\tNew Thread: {0}", newThreadID);
      Console.WriteLine(Environment.StackTrace);
     }
    }
   }
  }

  public new string Name
  {
   get
   {
    iFolderViewGroup.CheckThread();
    return name;
   }
  }

  public TreeModelFilter Model
  {
   get
   {
    iFolderViewGroup.CheckThread();
    return model;
   }
  }

  public bool VisibleWhenEmpty
  {
   get
   {
    iFolderViewGroup.CheckThread();
    return bVisibleWhenEmpty;
   }
   set
   {
    iFolderViewGroup.CheckThread();
    bVisibleWhenEmpty = value;

    UpdateVisibility();
   }
  }

  public bool IsEmpty
  {
   get
   {
    bool bIsCurrentlyEmpty =
     model.IterNChildren() > 0 ? false : true;

    return bIsCurrentlyEmpty;
   }
  }

  public Widget EmptyWidget
  {
   get
   {
    iFolderViewGroup.CheckThread();
    return emptyWidget;
   }
   set
   {
    if (value != null)
    {
     if (emptyWidget != null)
      emptyPage.Remove(emptyWidget);

     emptyWidget = value;

     emptyPage.PackStart(emptyWidget, true, true, 0);
     emptyPage.ShowAll();

     UpdateVisibility();
    }
   }
  }

  public Widget EmptySearchWidget
  {
   get
   {
    return emptySearchWidget;
   }
   set
   {
    if (value != null)
    {
     if (emptySearchWidget != null)
      emptySearchPage.Remove(emptySearchWidget);

     emptySearchWidget = value;

     emptySearchPage.PackStart(emptySearchWidget, true, true, 0);
     emptySearchPage.ShowAll();

     UpdateVisibility();
    }
   }
  }

  public iFolderViewGroupSelection Selection
  {
   get
   {
    iFolderViewGroup.CheckThread();
    return selection;
   }
  }

  public iFolderViewItem[] Items
  {
   get
   {
    iFolderViewGroup.CheckThread();
    int i = 0;
    iFolderViewItem[] itemsA = new iFolderViewItem[items.Count];
    foreach(iFolderViewItem item in items.Values)
    {
     itemsA[i] = item;

     i++;
    }

    return itemsA;
   }
  }

  public iFolderViewGroup(string name, TreeModelFilter model, Entry searchEntry)
  {
   iFolderViewGroup.CheckThread();
   this.name = name;
   this.model = model;
   this.searchEntry = searchEntry;
   alreadyDisposed = false;

   items = new Hashtable();

   selection = new iFolderViewGroupSelection(this);

   resizeTableTimeoutID = 0;
   rebuildTableTimeoutID = 0;

   currentWidth = 1;
   bFirstTableBuild = true;

   bVisibleWhenEmpty = true;
   emptyWidget = null;
   emptySearchWidget = null;

   searchTimeoutID = 0;
   searchEntry.Changed +=
    new EventHandler(OnSearchEntryChanged);

   this.PackStart(CreateWidgets(), true, true, 0);

   this.Realized +=
    new EventHandler(OnWidgetRealized);
  }

  ~iFolderViewGroup()
  {
Console.WriteLine("\n\n***\nDestructor:iFolderViewGroup\n");
   Dispose(true);
  }

  private void Dispose(bool calledFromFinalizer)
  {
Console.WriteLine("\n\n***\niFolderViewGroup.Dispose({0})\n***\n\n", calledFromFinalizer);
   try
   {
    if (!alreadyDisposed)
    {
     alreadyDisposed = true;


     searchEntry.Changed -=
      new EventHandler(OnSearchEntryChanged);

     model.RowChanged -=
      new RowChangedHandler(OnRowChanged);
     model.RowDeleted -=
      new RowDeletedHandler(OnRowDeleted);
     model.RowInserted -=
      new RowInsertedHandler(OnRowInserted);

     if (items != null)
     {
      ArrayList itemsToRemove = new ArrayList(items.Count);
      foreach (iFolderViewItem item in items.Values)
      {
       itemsToRemove.Add(item);
      }

      foreach(iFolderViewItem item in itemsToRemove)
      {
       item.LeftClicked -=
        new EventHandler(OnItemLeftClicked);
       item.RightClicked -=
        new EventHandler(OnItemRightClicked);
       item.DoubleClicked -=
        new EventHandler(OnItemDoubleClicked);

       items.Remove(item);
       item.Destroy();
      }
     }

     if (!calledFromFinalizer)
      GC.SuppressFinalize(this);
    }
   }
   catch{}
  }

  public override void Dispose()
  {
   Dispose(false);
  }

  private Widget CreateWidgets()
  {
   iFolderViewGroup.CheckThread();

   contentVBox = new VBox(false, 0);
   contentVBox.BorderWidth = 12;

   nameLabel = new Label(
    string.Format(
     "<span size=\"xx-large\">{0}</span>",
     GLib.Markup.EscapeText(name)));
   contentVBox.PackStart(nameLabel, false, false, 0);
   nameLabel.UseMarkup = true;
   nameLabel.UseUnderline = false;
   nameLabel.ModifyFg(StateType.Normal, this.Style.Base(StateType.Selected));
   nameLabel.Xalign = 0;

   groupNotebook = new Notebook();
   contentVBox.PackStart(groupNotebook, true, true, 0);
   groupNotebook.ShowTabs = false;
   groupNotebook.ShowBorder = false;
   groupNotebook.Homogeneous = false;

   groupNotebook.AppendPage(CreateMainPage(), null);
   groupNotebook.AppendPage(CreateEmptyPage(), null);
   groupNotebook.AppendPage(CreateEmptySearchPage(), null);

   groupNotebook.Page = 0;




   model.RowChanged +=
    new RowChangedHandler(OnRowChanged);
   model.RowDeleted +=
    new RowDeletedHandler(OnRowDeleted);
   model.RowInserted +=
    new RowInsertedHandler(OnRowInserted);

   return contentVBox;
  }

  private Widget CreateMainPage()
  {
   mainPage = new VBox(false, 0);

   table = new Table(1, 1, true);
   mainPage.PackStart(table, true, true, 0);
   table.ColumnSpacing = 12;
   table.RowSpacing = 12;
   table.BorderWidth = 12;
   table.ModifyBase(StateType.Normal, this.Style.Base(StateType.Prelight));

   return mainPage;
  }

  private Widget CreateEmptyPage()
  {
   emptyPage = new VBox(false, 0);

   return emptyPage;
  }

  private Widget CreateEmptySearchPage()
  {
   emptySearchPage = new VBox(false, 0);

   return emptySearchPage;
  }

  public void OnSizeAllocated(object o, SizeAllocatedArgs args)
  {
   iFolderViewGroup.CheckThread();
   if (currentWidth != args.Allocation.Width)
   {

    bool bDelay = currentWidth == 1 ? false : true;

    currentWidth = args.Allocation.Width;

    if (bDelay)
    {
     if (resizeTableTimeoutID != 0)
     {
      GLib.Source.Remove(resizeTableTimeoutID);
      resizeTableTimeoutID = 0;
     }

     resizeTableTimeoutID = GLib.Timeout.Add(
      resizeTimeout, new GLib.TimeoutHandler(ResizeTableCallback));
    }
    else
     ResizeTable();
   }
  }

  public iFolderHolder GetiFolderAtPos(int x, int y)
  {
   iFolderViewGroup.CheckThread();
   Console.WriteLine("iFolderViewGroup.GetiFolderAtPos({0}, {1}", x, y);




   foreach(iFolderViewItem item in items.Values)
   {
    Gdk.Rectangle allocation = item.Allocation;
    Console.WriteLine("Item {0}: x={1}, y={2}, w={3}, h={4}",
          item.Holder.iFolder.Name,
          allocation.X,
          allocation.Y,
          allocation.Width,
          allocation.Height);

    int xLowerBound = allocation.X;
    int xUpperBound = allocation.X + allocation.Width;

    int yLowerBound = allocation.Y;
    int yUpperBound = allocation.Y + allocation.Height;

    if (x >= xLowerBound && x < xUpperBound
     && y >= yLowerBound && y < yUpperBound)
    {

     return item.Holder;
    }
   }

   return null;
  }

  private void OnWidgetRealized(object o, EventArgs args)
  {
   iFolderViewGroup.CheckThread();
   UpdateVisibility();
  }

  private void UpdateVisibility()
  {
   iFolderViewGroup.CheckThread();
   bool bCurrentlyEmpty = model.IterNChildren() > 0 ?
          false :
          true;
   if (bCurrentlyEmpty)
   {
    if (this.Visible != bVisibleWhenEmpty)
     this.Visible = bVisibleWhenEmpty;

    if (this.Visible)
    {
     string searchText = searchEntry.Text;
     if (searchText.Length > 0)
      groupNotebook.Page = 2;
     else
      groupNotebook.Page = 1;
    }
   }
   else
   {
    groupNotebook.Page = 0;

    if (!this.Visible)
     this.Visible = true;
   }
  }

  private void OnSearchEntryChanged(object o, EventArgs args)
  {
   if (searchTimeoutID != 0)
   {
    GLib.Source.Remove(searchTimeoutID);
    searchTimeoutID = 0;
   }

   searchTimeoutID = GLib.Timeout.Add(
    500, new GLib.TimeoutHandler(SearchCallback));
  }

  private bool SearchCallback()
  {
   UpdateVisibility();
   return false;
  }

  private void ResizeTable()
  {
   iFolderViewGroup.CheckThread();
Console.WriteLine("iFolderViewGroup.ResizeTable({0})", name);
   int numOfItems = model.IterNChildren();
Console.WriteLine("\tNum of Items: {0}", numOfItems);

   if (numOfItems > 0)
   {
    int availableWidth = currentWidth
          - (int)(contentVBox.BorderWidth * 2)
          - (int)(table.BorderWidth * 2);
Console.WriteLine("\tWidth Available: {0}", availableWidth);
    int numOfColumns = availableWidth / ItemMaxWidth;
    if (numOfColumns < 1)
     numOfColumns = 1;
    int numOfRows = numOfItems / numOfColumns;
    if ((numOfItems % numOfColumns) > 0)
     numOfRows++;


    if (!bFirstTableBuild && numOfColumns == (int)table.NColumns)
    {
Console.WriteLine("\tNumber of columns hasn't changed: {0}", numOfColumns);
     return;
    }
    else
    {
     RebuildTable();
    }
   }
  }

  public void RebuildTable()
  {
   iFolderViewGroup.CheckThread();
Console.WriteLine("iFolderViewGroup.RebuildTable({0})", name);
   int numOfItems = model.IterNChildren();
Console.WriteLine("\tNum of Items: {0}", numOfItems);

   if (numOfItems > 0)
   {
    int availableWidth = currentWidth
          - (int)(contentVBox.BorderWidth * 2)
          - (int)(table.BorderWidth * 2);
Console.WriteLine("\tWidth Available: {0}", availableWidth);
    int numOfColumns = availableWidth / ItemMaxWidth;
    if (numOfColumns < 1)
     numOfColumns = 1;
    int numOfRows = numOfItems / numOfColumns;
    if ((numOfItems % numOfColumns) > 0)
     numOfRows++;

    bFirstTableBuild = false;




    items.Clear();

    foreach (Widget w in table.Children)
    {
     table.Remove(w);
     w.Destroy();
    }

Console.WriteLine("\tResizing table to {0} rows and {1} columns", numOfRows, numOfColumns);

    table.Resize((uint)numOfRows, (uint)numOfColumns);

    int currentRow = 0;
    int currentColumn = 0;

    TreeIter iter;
    if (model.GetIterFirst(out iter))
    {
     do
     {
      iFolderHolder holder = (iFolderHolder)model.GetValue(iter, 0);
      if (holder != null)
      {
       iFolderViewItem item = new iFolderViewItem(holder, this, iter, ItemMaxWidth);
       table.Attach(item,
           (uint)currentColumn, (uint)currentColumn + 1,
           (uint)currentRow, (uint)currentRow + 1,
           AttachOptions.Shrink | AttachOptions.Fill,
           0, 0, 0);


       TreePath path = model.GetPath(iter);
       items[path.ToString()] = item;


       item.LeftClicked +=
        new EventHandler(OnItemLeftClicked);
       item.RightClicked +=
        new EventHandler(OnItemRightClicked);
       item.DoubleClicked +=
        new EventHandler(OnItemDoubleClicked);

       currentColumn = ((currentColumn + 1) % numOfColumns);
       if (currentColumn == 0)
        currentRow++;
      }
     } while (model.IterNext(ref iter));
    }

    table.ShowAll();
   }
   else
   {
    items.Clear();
    foreach(Widget w in table.Children)
    {
     table.Remove(w);
     w.Destroy();
    }
   }
  }

  private void OnRowChanged(object o, RowChangedArgs args)
  {
   iFolderViewGroup.CheckThread();
   if (args == null || args.Path == null) return;
   iFolderViewItem item = (iFolderViewItem)items[args.Path.ToString()];
   if (item != null)
   {
Console.WriteLine("iFolderViewGroup.OnRowChanged: {0}", item.Holder.iFolder.Name);
    item.Refresh();
Console.WriteLine("iFolderViewGroup.OnRowChanged: {0} exiting", item.Holder.iFolder.Name);
   }
  }

  private void OnRowDeleted(object o, RowDeletedArgs args)
  {
   iFolderViewGroup.CheckThread();
   if (args == null || args.Path == null) return;
   iFolderViewItem item = (iFolderViewItem)items[args.Path.ToString()];
   if (item != null)
   {
Console.WriteLine("iFolderViewGroup.OnRowDeleted: {0}", item.Holder.iFolder.Name);

   }

   if (rebuildTableTimeoutID != 0)
   {
    GLib.Source.Remove(rebuildTableTimeoutID);
    rebuildTableTimeoutID = 0;
   }

   rebuildTableTimeoutID = GLib.Timeout.Add(
    rebuildTimeout, new GLib.TimeoutHandler(RebuildTableCallback));

   UpdateVisibility();
  }

  private void OnRowInserted(object o, RowInsertedArgs args)
  {
   iFolderViewGroup.CheckThread();
   if (args == null) return;
Console.WriteLine("iFolderViewGroup.OnRowInserted...");
   iFolderHolder ifHolder = (iFolderHolder)model.GetValue(args.Iter, 0);
   if (ifHolder != null)
   {
Console.WriteLine("iFolderViewGroup.OnRowInserted: {0}", ifHolder.iFolder.Name);
   }
   if (rebuildTableTimeoutID != 0)
   {
    GLib.Source.Remove(rebuildTableTimeoutID);
    rebuildTableTimeoutID = 0;
   }
   rebuildTableTimeoutID = GLib.Timeout.Add(
    rebuildTimeout, new GLib.TimeoutHandler(RebuildTableCallback));
   UpdateVisibility();
  }
  private bool ResizeTableCallback()
  {
   iFolderViewGroup.CheckThread();
   ResizeTable();
   return false;
  }
  private bool RebuildTableCallback()
  {
   iFolderViewGroup.CheckThread();
   RebuildTable();
   return false;
  }
  private void OnItemLeftClicked(object o, EventArgs args)
  {
   iFolderViewGroup.CheckThread();
   iFolderViewItem item = (iFolderViewItem)o;
   selection.SelectItem(item);
  }
  private void OnItemRightClicked(object o, EventArgs args)
  {
   iFolderViewGroup.CheckThread();
   iFolderViewItem item = (iFolderViewItem)o;
   selection.SelectItem(item);
  }
  private void OnItemDoubleClicked(object o, EventArgs args)
  {
   iFolderViewGroup.CheckThread();
  }
 }
 public class iFolderViewGroupSelection
 {
  private iFolderViewGroup group;
  private SelectionMode mode;
  public iFolderViewGroup ViewGroup
  {
   get{ return group; }
  }
  public SelectionMode Mode
  {
   get{ return mode; }
   set
   {
    mode = value;
    UnselectAll();
   }
  }
  public event EventHandler SelectionChanged;
  public iFolderViewGroupSelection(iFolderViewGroup group) : base()
  {
Console.WriteLine("iFolderViewGroupSelection.iFolderViewGroupSelection");
   this.group = group;
   this.Mode = SelectionMode.Single;
  }
  public int CountSelectedRows()
  {
Console.WriteLine("iFolderViewGroupSelection.CountSelectedRows");
   int count = 0;
   foreach(iFolderViewItem item in group.Items)
   {
    if (item.Selected)
     count++;
   }
   return count;
  }
  public iFolderViewItem[] GetSelectedItems()
  {
Console.WriteLine("iFolderViewGroupSelection.GetSelectedItems");
   ArrayList arrayList = new ArrayList();
   foreach(iFolderViewItem item in group.Items)
   {
    if (item.Selected)
    {
     arrayList.Add(item);
    }
   }
   iFolderViewItem[] items =
    (iFolderViewItem[])arrayList.ToArray(typeof(iFolderViewItem));
   return items;
  }
  public void SelectAll()
  {
Console.WriteLine("iFolderViewGroupSelection.SelectAll");
   bool bSelectionChanged = false;
   foreach(iFolderViewItem item in group.Items)
   {
    if (!item.Selected)
    {
     item.Selected = true;
     bSelectionChanged = true;
    }
   }
   if (bSelectionChanged && SelectionChanged != null)
    SelectionChanged(this, EventArgs.Empty);
  }
  public void UnselectAll()
  {
Console.WriteLine("iFolderViewGroupSelection.UnselectAll");
   bool bSelectionChanged = false;
   foreach(iFolderViewItem item in group.Items)
   {
Console.WriteLine("\t{0}", item.Holder.iFolder.Name);
    if (item.Selected)
    {
     item.Selected = false;
     bSelectionChanged = true;
    }
   }
   if (bSelectionChanged && SelectionChanged != null)
    SelectionChanged(this, EventArgs.Empty);
Console.WriteLine("\tUnselectAll returning");
  }
  public void SelectItem(iFolderViewItem item)
  {
   if (item != null)
   {
    if (!item.Selected)
    {
     if (Mode == SelectionMode.Single)
     {
      iFolderViewItem oldItem;
      if (GetSelected(out oldItem))
       oldItem.Selected = false;
     }
     item.Selected = true;
     if (SelectionChanged != null)
      SelectionChanged(this, EventArgs.Empty);
    }
   }
  }
  public bool GetSelected(out iFolderViewItem itemOut)
  {
Console.WriteLine("iFolderViewGroupSelection.GetSelected");
   itemOut = null;
   foreach(iFolderViewItem item in group.Items)
   {
    if (item.Selected)
    {
     itemOut = item;
     return true;
    }
   }
   return false;
  }
  public bool GetSelected(out TreeModel modelOut, out iFolderViewItem itemOut)
  {
Console.WriteLine("iFolderViewGroupSelection.GetSelected2");
   modelOut = group.Model;
   return GetSelected(out itemOut);
  }
 }
}
