using System;
using System.Windows.Forms;
namespace Novell.FormsTrayApp
{
 public enum MoveDirection
 {
  Down,
  Right,
  Up
 }
 public class NavigateItemEventArgs : EventArgs
 {
  private int row;
  private int column;
  private MoveDirection direction;
  public NavigateItemEventArgs( int row, int column, MoveDirection direction )
  {
   this.row = row;
   this.column = column;
   this.direction = direction;
  }
  public int Column
  {
   get { return column; }
  }
  public MoveDirection Direction
  {
   get { return direction; }
  }
  public int Row
  {
   get { return row; }
  }
 }
}
