

using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text;

namespace ProcessHacker.Common
{
    public static class HistoryManager
    {
        private static int _globalMaxCount = 600;

        public static int GlobalMaxCount
        {
            get { return _globalMaxCount; }
            set { _globalMaxCount = value; }
        }
    }

    public sealed class HistoryManager<TKey, TValue>
    {
        private Dictionary<TKey, CircularBuffer<TValue> > _history;
        private Dictionary<TKey, ReadOnlyCollection<TValue> > _readOnlyCollections;
        private int _maxCount = -1;

        public HistoryManager()
        {
            _history = new Dictionary<TKey, CircularBuffer<TValue> >();
            _readOnlyCollections = new Dictionary<TKey, ReadOnlyCollection<TValue> >();
        }

        public HistoryManager(IEqualityComparer<TKey> comparer)
        {
            _history = new Dictionary<TKey, CircularBuffer<TValue> >(comparer);
            _readOnlyCollections = new Dictionary<TKey, ReadOnlyCollection<TValue> >(comparer);
        }

        public int MaxCount
        {
            get { return _maxCount; }
            set { _maxCount = value; }
        }

        public int EffectiveMaxCount
        {
            get { return _maxCount == -1 ? HistoryManager.GlobalMaxCount : _maxCount; }
        }

        public ReadOnlyCollection<TValue> this[TKey key]
        {
            get { return GetHistory(key); }
        }

        public void Add(TKey key)
        {
            _history.Add(key, new CircularBuffer<TValue>(this.EffectiveMaxCount));
        }

        public ReadOnlyCollection<TValue> GetHistory(TKey key)
        {
            if (!_readOnlyCollections.ContainsKey(key))
            {
                lock (_readOnlyCollections)
                {
                    if (!_readOnlyCollections.ContainsKey(key))
                        _readOnlyCollections.Add(key, new ReadOnlyCollection<TValue>(_history[key]));
                }
            }

            return _readOnlyCollections[key];
        }

        public void Update(TKey key, TValue value)
        {
            int maxCount = this.EffectiveMaxCount;

            if (_history[key].Size != maxCount)
                _history[key].Resize(maxCount);

            _history[key].Add(value);
        }
    }
}
