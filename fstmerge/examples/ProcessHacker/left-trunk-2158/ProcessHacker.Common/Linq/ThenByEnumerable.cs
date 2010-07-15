using System;
using System.Collections.Generic;
using System.Text;

namespace System.Linq
{
 class ThenByEnumerable<TElement, TKey, TLastKey> : OrderByEnumerable<TElement, TKey>
 {
  public ThenByEnumerable (
   OrderByEnumerable<TElement, TLastKey> source,
   Func<TElement, TKey> keySelector,
   IComparer<TKey> comparer,
   bool descending)
   : base (source, keySelector, comparer, descending)
  {
  }

  public OrderByEnumerable<TElement, TLastKey> OrderedSource { get { return (OrderByEnumerable<TElement, TLastKey>) Source; } }

  internal override int CompareElements (TElement e1, TElement e2)
  {



   int result = OrderedSource.CompareElements (e1, e2);
   if (result != 0) return result;



   return base.CompareElements (e1, e2);
  }

  internal override IEnumerable<TElement> GetElementsToSort ()
  {


   return OrderedSource.GetElementsToSort();
  }
 }

}
