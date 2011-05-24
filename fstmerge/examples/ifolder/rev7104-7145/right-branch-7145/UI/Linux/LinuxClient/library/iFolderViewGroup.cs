

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
  private string qouta = "";
  private TreeModelFilter model;

  private Hashtable items;

  private Label nameLabel;
  public Label QoutaLabel;
  private Table table;

  private iFolderViewGroupSelection selection;


  private static uint resizeTimeout = 20;
  private uint resizeTableTimeoutID;


  private static uint rebuildTimeout = 20;
  private uint rebuildTableTimeoutID;

  private static int ItemMaxWidth = 250;

  private int currentWidth;

  private bool bFirstTableBuild;

  private bool bVisibleWhenEmpty;
  private Widget emptyWidget;
  private Widget emptySearchWidget;
  private Entry searchEntry;
  private uint searchTimeoutID;

  private bool alreadyDisposed;
  private static iFolderLog log;

  public new string Name
  {
   get
   {
    return name;
   }
  }

  public TreeModelFilter Model
  {
   get
   {
    return model;
   }
  }

  public bool VisibleWhenEmpty
  {
   get
   {
    return bVisibleWhenEmpty;
   }
   set
   {
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
    return selection;
   }
  }

  public iFolderViewItem[] Items
  {
   get
   {
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
   Dispose(true);
  }

  private void Dispose(bool calledFromFinalizer)
  {
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
   contentVBox = new VBox(false, 0);
   contentVBox.BorderWidth = 12;

   nameLabel = new Label(
    string.Format(
     "<span size=\"x-large\">{0}</span>",
     GLib.Markup.EscapeText(name)));
   contentVBox.PackStart(nameLabel, false, false, 0);
   nameLabel.UseMarkup = true;
   nameLabel.UseUnderline = false;
   nameLabel.ModifyFg(StateType.Normal, this.Style.Base(StateType.Selected));
   nameLabel.Xalign = 0;

   QoutaLabel = new Label(
    string.Format(
      "<span size=\"xx-small\">{0}</span>",
     GLib.Markup.EscapeText(qouta)));
   contentVBox.PackStart(QoutaLabel, false, false, 0);
   QoutaLabel.UseMarkup = true;
   QoutaLabel.UseUnderline = false;
   QoutaLabel.ModifyFg(StateType.Normal, this.Style.Base(StateType.Selected));
   QoutaLabel.Xalign = 0;

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



   foreach(iFolderViewItem item in items.Values)
   {
    Gdk.Rectangle allocation = item.Allocation;

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
   UpdateVisibility();
  }

  private void UpdateVisibility()
  {
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
   int numOfItems = model.IterNChildren();

   if (numOfItems > 0)
   {
    int availableWidth = currentWidth
          - (int)(contentVBox.BorderWidth * 2)
          - (int)(table.BorderWidth * 2);
    int numOfColumns = availableWidth / ItemMaxWidth;
    if (numOfColumns < 1)
     numOfColumns = 1;
    int numOfRows = numOfItems / numOfColumns;
    if ((numOfItems % numOfColumns) > 0)
     numOfRows++;


    if (!bFirstTableBuild && numOfColumns == (int)table.NColumns)
    {
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
                        if( (null == model) || (null == items) || (null == table) )
                        {
                                iFolderViewGroup.log.Info("NULL Reference Exception: Object, model is -{0}-items is-{1}-table is -{2}-",model,items,table);
                                return;
                        }

   int numOfItems = model.IterNChildren();

   if (numOfItems > 0)
   {
    int availableWidth = currentWidth
          - (int)(contentVBox.BorderWidth * 2)
          - (int)(table.BorderWidth * 2);

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
                                                        if(null == item)
                                                        {
                                                                iFolderViewGroup.log.Info("NULL Reference Exception: Object item is NULL");
                                                                return;
                                                        }

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
   if (args == null || args.Path == null) return;
   iFolderViewItem item = (iFolderViewItem)items[args.Path.ToString()];
   if (item != null)
   {
    item.Refresh();
   }
  }

  private void OnRowDeleted(object o, RowDeletedArgs args)
  {
   if (args == null || args.Path == null) return;


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
   if (args == null) return;



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
   ResizeTable();
   return false;
  }

  private bool RebuildTableCallback()
  {
   RebuildTable();
   return false;
  }

  private void OnItemLeftClicked(object o, EventArgs args)
  {
   iFolderViewItem item = (iFolderViewItem)o;
   selection.SelectItem(item);
  }

  private void OnItemRightClicked(object o, EventArgs args)
  {
   iFolderViewItem item = (iFolderViewItem)o;
   selection.SelectItem(item);
  }

  private void OnItemDoubleClicked(object o, EventArgs args)
  {
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
   this.group = group;


   this.Mode = SelectionMode.Single;
  }

  public int CountSelectedRows()
  {
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
   bool bSelectionChanged = false;

   foreach(iFolderViewItem item in group.Items)
   {
    if (item.Selected)
    {
     item.Selected = false;
     bSelectionChanged = true;
    }
   }

   if (bSelectionChanged && SelectionChanged != null)
    SelectionChanged(this, EventArgs.Empty);
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
   modelOut = group.Model;

   return GetSelected(out itemOut);
  }




 }
}
