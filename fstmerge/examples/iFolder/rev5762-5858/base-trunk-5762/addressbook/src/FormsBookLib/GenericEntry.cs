using System;
namespace Novell.iFolder.FormsBookLib
{
 public class GenericEntry
 {
  private bool add;
  private bool remove;
  public GenericEntry()
  {
   remove = false;
   add = false;
  }
  public bool Add
  {
   get
   {
    return add;
   }
   set
   {
    add = value;
   }
  }
  public bool Remove
  {
   get
   {
    return remove;
   }
   set
   {
    remove = value;
   }
  }
 }
}
