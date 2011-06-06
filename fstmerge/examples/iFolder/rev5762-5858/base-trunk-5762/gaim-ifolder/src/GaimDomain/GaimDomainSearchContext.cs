using System;
using System.Collections;
namespace Simias.Gaim
{
 public class GaimDomainSearchContext
 {
  private String id;
  private ArrayList members;
  private int currentIndex;
  public GaimDomainSearchContext()
  {
   id = Guid.NewGuid().ToString();
   currentIndex = 0;
  }
  public string ID
  {
   get
   {
    return id;
   }
  }
  public ArrayList Members
  {
   get
   {
    return members;
   }
   set
   {
    if (value != null)
    {
     members = value;
    }
   }
  }
  public int Count
  {
   get
   {
    if (members != null)
    {
     return members.Count;
    }
    else
     return 0;
   }
  }
  public int CurrentIndex
  {
   get
   {
    return currentIndex;
   }
   set
   {
    currentIndex = value;
   }
  }
 }
}
