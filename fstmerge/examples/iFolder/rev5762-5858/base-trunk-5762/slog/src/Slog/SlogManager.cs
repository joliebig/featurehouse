using System;
using System.Collections;
using System.IO;
using Simias;
using Simias.Storage;
namespace Novell.Collaboration
{
 public class SlogManager : IEnumerable, IEnumerator
 {
  private Store store = null;
  private IEnumerator storeEnum = null;
  internal SlogManager(Store myStore)
  {
   this.store = myStore;
  }
  public static SlogManager Connect( )
  {
   SlogManager mgr = null;
   Store store = Store.GetStore();
   if(store != null)
   {
    mgr = new SlogManager(store);
   }
   return(mgr);
  }
  public static SlogManager Connect(Configuration cConfig)
  {
   SlogManager mgr = null;
   Store store = Store.GetStore();
   if(store != null)
   {
    mgr = new SlogManager(store);
   }
   return(mgr);
  }
  public Slog CreateSlog(string name)
  {
   Slog slog = new Slog(this.store, name);
   return slog;
  }
  public Slog GetSlog(string slogID)
  {
   Slog slog = null;
   try
   {
    Collection collection = store.GetCollectionByID(slogID);
    if(collection != null)
    {
     slog = new Slog(this.store, collection);
    }
    return(slog);
   }
   catch
   {
    throw new ApplicationException("Slog not found");
   }
  }
  public IEnumerator GetEnumerator()
        {
   ICSList abList = this.store.GetCollectionsByType(typeof(Slog).Name);
   storeEnum = abList.GetEnumerator();
   return(this);
        }
  public bool MoveNext()
        {
   while(storeEnum.MoveNext())
   {
    return(true);
   }
   return(false);
        }
  public object Current
        {
            get
            {
    try
    {
     Slog slog =
      GetSlog( ((ShallowNode) storeEnum.Current).ID);
     return((object) slog);
    }
    catch{}
    return(null);
            }
        }
  public void Reset()
        {
   storeEnum.Reset();
        }
 }
}
