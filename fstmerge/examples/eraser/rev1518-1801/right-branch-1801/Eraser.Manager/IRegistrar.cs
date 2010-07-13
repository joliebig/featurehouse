

using System;
using System.Linq;
using System.Collections.Generic;
using System.Text;

namespace Eraser.Manager
{



 public interface IRegistrar<T> : IList<T> where T: IRegisterable
 {







  T this[Guid key] { get; }






  bool Contains(Guid key);






  bool Remove(Guid key);





  EventHandler<EventArgs> Registered { get; set; }





  EventHandler<EventArgs> Unregistered { get; set; }
 }




 public interface IRegisterable
 {



  Guid Guid { get; }
 }





 public class Registrar<T> : IRegistrar<T> where T: IRegisterable
 {


  public int IndexOf(T item)
  {
   lock (List)
    return List.IndexOf(item);
  }

  public void Insert(int index, T item)
  {
   lock (List)
   {
    List.Insert(index, item);
    Dictionary.Add(item.Guid, item);
   }

   if (Registered != null)
    Registered(item, EventArgs.Empty);
  }

  public void RemoveAt(int index)
  {
   T value = default(T);
   lock (List)
   {
    value = List[index];
    List.RemoveAt(index);
    Dictionary.Remove(value.Guid);
   }

   if (Unregistered != null)
    Unregistered(value, EventArgs.Empty);
  }

  public T this[int index]
  {
   get
   {
    lock (List)
     return List[index];
   }
   set
   {
    lock (List)
     List[index] = value;
   }
  }







  public void Add(T item)
  {
   lock (List)
   {
    if (Dictionary.ContainsKey(item.Guid))
     return;

    List.Add(item);
    Dictionary.Add(item.Guid, item);
   }

   if (Registered != null)
    Registered(item, EventArgs.Empty);
  }

  public void Clear()
  {
   lock (List)
   {
    if (Unregistered != null)
     List.ForEach(item => Unregistered(item, EventArgs.Empty));
    List.Clear();
    Dictionary.Clear();
   }
  }

  public bool Contains(T item)
  {
   lock (List)
    return List.Contains(item);
  }

  public void CopyTo(T[] array, int arrayIndex)
  {
   lock (List)
    List.CopyTo(array, arrayIndex);
  }

  public int Count
  {
   get
   {
    lock (List)
     return Count;
   }
  }

  public bool IsReadOnly
  {
   get { return false; }
  }

  public bool Remove(T item)
  {
   bool result = false;
   lock (List)
   {
    result = List.Remove(item);
    Dictionary.Remove(item.Guid);
   }

   if (result && Unregistered != null)
    Unregistered(item, EventArgs.Empty);
   return result;
  }





  public IEnumerator<T> GetEnumerator()
  {
   return List.GetEnumerator();
  }





  System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
  {
   return List.GetEnumerator();
  }





  public T this[Guid key]
  {
   get
   {
    lock (List)
     return Dictionary[key];
   }
  }

  public bool Contains(Guid key)
  {
   lock (List)
    return Dictionary.ContainsKey(key);
  }

  public bool Remove(Guid key)
  {
   lock (List)
   {
    if (!Dictionary.ContainsKey(key))
     return false;
    return Remove(Dictionary[key]);
   }
  }

  public EventHandler<EventArgs> Registered { get; set; }

  public EventHandler<EventArgs> Unregistered { get; set; }






  private List<T> List = new List<T>();




  private Dictionary<Guid, T> Dictionary = new Dictionary<Guid, T>();
 }
}
