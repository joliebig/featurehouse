using System;
using System.Windows.Forms;
namespace Aga.Controls.Tree
{
 internal abstract class InputState
 {
  private TreeViewAdv _tree;

  public TreeViewAdv Tree
  {
   get { return _tree; }
  }

  public InputState(TreeViewAdv tree)
  {
   _tree = tree;
  }

  public abstract void KeyDown(System.Windows.Forms.KeyEventArgs args);
  public abstract void MouseDown(TreeNodeAdvMouseEventArgs args);
  public abstract void MouseUp(TreeNodeAdvMouseEventArgs args);






  public virtual bool MouseMove(MouseEventArgs args)
  {
   return false;
  }
 }
}
