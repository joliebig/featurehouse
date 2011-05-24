

using System;
using System.Collections.Generic;

namespace ProcessHacker.Common
{
    public static class LongExtensions
    {





        public static long Max(this IEnumerable<long> source)
        {
            if (source == null)
                throw new ArgumentNullException("source");

            long max = 0;
            bool afterFirst = false;

            foreach (long number in source)
            {
                if (afterFirst)
                {
                    if (number > max)
                        max = number;
                }
                else
                {
                    max = number;
                    afterFirst = true;
                }
            }

            return max;
        }
        public static IList<long> Take(this IList<long> source, int count)
        {
            if (source == null)
                throw new ArgumentNullException("source");
            if (count >= source.Count)
                return source;
            IList<long> newList = new List<long>();
            for (int i = 0; i < count; i++)
                newList.Add(source[i]);
            return newList;
        }
    }
}
