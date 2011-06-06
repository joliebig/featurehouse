using System;
using System.Windows.Forms;
namespace Novell.FormsTrayApp
{
 public class ItemSelectedEventArgs : EventArgs
 {
  private TileListViewItem tlvi;
  public ItemSelectedEventArgs( TileListViewItem tlvi )
  {
   this.tlvi = tlvi;
  }
  public TileListViewItem Item
  {
   get { return tlvi; }
  }
 }
}
