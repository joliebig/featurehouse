using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using Simias.Client;
namespace Novell.FormsTrayApp
{
 public class TileListView : System.Windows.Forms.UserControl
 {
  private int itemsPerRow;
  private int horizontalSpacing = 5;
  private int verticleSpacing = 5;
  private int itemWidth = 280;
  private int itemHeight = 72;
  private TileListViewItemCollection items;
  private TileListViewItem selectedItem = null;
  private ImageList largeImageList;
  private Size oldSize;
  private System.ComponentModel.Container components = null;
  public TileListView()
  {
   InitializeComponent();
   items = new TileListViewItemCollection( this );
   oldSize = Size;
  }
  protected override void Dispose( bool disposing )
  {
   if( disposing )
   {
    if(components != null)
    {
     components.Dispose();
    }
   }
   base.Dispose( disposing );
  }
  private void InitializeComponent()
  {
   System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(TileListView));
   this.AccessibleDescription = resources.GetString("$this.AccessibleDescription");
   this.AccessibleName = resources.GetString("$this.AccessibleName");
   this.AutoScroll = ((bool)(resources.GetObject("$this.AutoScroll")));
   this.AutoScrollMargin = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMargin")));
   this.AutoScrollMinSize = ((System.Drawing.Size)(resources.GetObject("$this.AutoScrollMinSize")));
   this.BackColor = System.Drawing.SystemColors.Window;
   this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
   this.Enabled = ((bool)(resources.GetObject("$this.Enabled")));
   this.Font = ((System.Drawing.Font)(resources.GetObject("$this.Font")));
   this.ImeMode = ((System.Windows.Forms.ImeMode)(resources.GetObject("$this.ImeMode")));
   this.Location = ((System.Drawing.Point)(resources.GetObject("$this.Location")));
   this.Name = "TileListView";
   this.RightToLeft = ((System.Windows.Forms.RightToLeft)(resources.GetObject("$this.RightToLeft")));
   this.Size = ((System.Drawing.Size)(resources.GetObject("$this.Size")));
   this.Click += new System.EventHandler(this.TileListView_Click);
   this.SizeChanged += new System.EventHandler(this.TileListView_SizeChanged);
   this.MouseDown += new System.Windows.Forms.MouseEventHandler(this.TileListView_MouseDown);
  }
  public new event EventHandler DoubleClick;
  public delegate bool NavigateItemDelegate( object sender, NavigateItemEventArgs e );
  public event NavigateItemDelegate NavigateItem;
  public delegate void LastItemRemovedDelegate( object sender, EventArgs e);
  public event LastItemRemovedDelegate LastItemRemoved;
  public delegate void SelectedIndexChangedDelegate( object sender, EventArgs e );
  public event SelectedIndexChangedDelegate SelectedIndexChanged;
        private void TileListView_Click(object sender, System.EventArgs e)
  {
   if ( selectedItem != null )
   {
    selectedItem.Selected = false;
    selectedItem = null;
   }
   if ( SelectedIndexChanged != null )
   {
    SelectedIndexChanged( this, e );
   }
  }
  private void TileListView_DoubleClick(object sender, System.EventArgs e)
  {
   if ( DoubleClick != null )
   {
    DoubleClick( sender, e );
   }
  }
  private void TileListView_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
  {
   if ( selectedItem != null )
   {
    selectedItem.Selected = false;
    selectedItem = null;
   }
   if ( SelectedIndexChanged != null )
   {
    SelectedIndexChanged( this, new EventArgs() );
   }
  }
        private void TileListView_SizeChanged(object sender, System.EventArgs e)
  {
   if ( isReCalculateNeeded )
   {
    ReCalculateItems();
   }
   oldSize = Size;
  }
  internal void item_DoubleClick(object sender, EventArgs e)
  {
   if ( DoubleClick != null )
   {
    DoubleClick( sender, e );
   }
  }
  internal void item_Selected(object sender, EventArgs e)
  {
   TileListViewItem tlvi = (TileListViewItem)sender;
   if (selectedItem != null)
   {
    if ( !tlvi.Equals(selectedItem) )
    {
     selectedItem.Selected = false;
    }
   }
   selectedItem = tlvi;
   if ( SelectedIndexChanged != null )
   {
    SelectedIndexChanged( this, e );
   }
  }
  private void adjustHeight()
  {
   if (items.Count == 0)
   {
    if ( LastItemRemoved != null )
    {
     LastItemRemoved( this, new EventArgs() );
    }
    Height = 50;
   }
   else
   {
    int height = 0;
    height = HeightToShowAll;
    if(Height != height + 10)
     Height = height + 10;
   }
  }
  private System.Drawing.Point getItemLocation(int index)
  {
   int rowIndex = index / itemsPerRow;
   int colIndex = index - rowIndex * itemsPerRow;
   Point p = new Point(
    colIndex * ( ItemWidth + verticleSpacing ) + 10,
    rowIndex * ( ItemHeight + horizontalSpacing ) + 10 );
   return p;
  }
  public int HeightToShowAll
  {
   get
   {
    System.Drawing.Point p = getItemLocation(items.Count - 1);
    return p.Y + ItemHeight + horizontalSpacing;
   }
  }
  public int HorizontalSpacing
  {
   get
   {
    return horizontalSpacing;
   }
   set
   {
    horizontalSpacing = value;
   }
  }
  public bool isReCalculateNeeded
  {
   get
   {
    int oldItemsPerRow = (oldSize.Width - 20) / ItemWidth;
    if ( itemsPerRow == oldItemsPerRow)
    {
     return false;
    }
    else
     return true;
   }
  }
  public int ItemHeight
  {
   get
   {
    return itemHeight;
   }
   set
   {
    itemHeight = value;
   }
  }
  public TileListViewItemCollection Items
  {
   get { return items; }
  }
  public int ItemWidth
  {
   get
   {
    return itemWidth;
   }
   set
   {
    itemWidth = value;
   }
  }
  public ImageList LargeImageList
  {
   get { return largeImageList; }
   set { largeImageList = value; }
  }
  public TileListViewItem SelectedItem
  {
   get { return selectedItem; }
   set { selectedItem = value; }
  }
        public int VerticleSpacing
  {
   get
   {
    return verticleSpacing;
   }
   set
   {
    verticleSpacing = value;
   }
  }
  public bool MoveToItem( int row, int column )
  {
   bool result = false;
   bool traversingListView = false;
   if ( row == -1 )
   {
    traversingListView = true;
    row = Items.Count / itemsPerRow;
   }
   if ( column < itemsPerRow )
   {
    int index = row * itemsPerRow + column;
    if ( ( index >= Items.Count ) && traversingListView )
    {
     if ( --row >= 0 )
     {
      index = row * itemsPerRow + column;
     }
    }
    if ( index < Items.Count )
    {
     TileListViewItem tlvi = Items[ index ];
     tlvi.Selected = true;
     tlvi.Focus();
     result = true;
    }
   }
   return result;
  }
  private void getItemPosition( TileListViewItem tlvi, out int row, out int column )
  {
   int index = Items.IndexOf( tlvi );
   if ( itemsPerRow == 0 )
    itemsPerRow = 1;
   row = index / itemsPerRow;
   column = index - row * itemsPerRow;
  }
  internal void MoveDown( TileListViewItem tlvi )
  {
   int row;
   int column;
   getItemPosition( tlvi, out row, out column );
   if ( !MoveToItem( ++row, column ) && NavigateItem != null )
   {
    NavigateItem( this, new NavigateItemEventArgs( 0, column, MoveDirection.Down ) );
   }
  }
  internal void MoveLeft( TileListViewItem tlvi )
  {
   int row;
   int column;
   getItemPosition( tlvi, out row, out column );
   if ( column > 0 )
    column--;
   MoveToItem( row, column );
  }
  internal void MoveRight( TileListViewItem tlvi )
  {
   int row;
   int column;
   getItemPosition( tlvi, out row, out column );
   if ( column < itemsPerRow - 1 )
    column++;
   if ( !MoveToItem( row, column ) )
   {
    if ( NavigateItem == null ||
     !NavigateItem( this, new NavigateItemEventArgs( 0, column, MoveDirection.Right ) ) )
    {
     row--;
     if ( row >= 0 )
     {
      MoveToItem( row, column );
     }
    }
   }
  }
  internal void MoveUp( TileListViewItem tlvi )
  {
   int row;
   int column;
   getItemPosition( tlvi, out row, out column );
   row--;
   if ( row < 0 )
   {
    if ( NavigateItem != null )
    {
     NavigateItem( this, new NavigateItemEventArgs( row, column, MoveDirection.Up ) );
    }
   }
   else
   {
    MoveToItem( row, column );
   }
  }
  public void ReCalculateItems()
  {
   SuspendLayout();
   itemsPerRow = (Width - 20) / ItemWidth;
   if ( itemsPerRow == 0 )
   {
    itemsPerRow = 1;
   }
   TileListViewItem tlvi;
   for (int i = 0; i < items.Count; i++)
   {
    tlvi = items[i];
    tlvi.Location = getItemLocation(i);
   }
   adjustHeight();
   ResumeLayout();
  }
 }
}
