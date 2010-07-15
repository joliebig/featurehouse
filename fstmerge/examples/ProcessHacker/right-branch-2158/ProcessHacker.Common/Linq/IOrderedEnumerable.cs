using System;
using System.Collections.Generic;
using System.Text;

namespace System
{


 public interface IOrderedEnumerable<TElement> : IEnumerable<TElement>
 {
  IOrderedEnumerable<TElement> CreateOrderedEnumerable<TKey> (
   Func<TElement, TKey> keySelector,
   IComparer<TKey> comparer,
   bool descending);
 }
}
