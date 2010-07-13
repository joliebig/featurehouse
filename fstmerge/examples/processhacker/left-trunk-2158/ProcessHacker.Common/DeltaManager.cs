

using System.Collections.Generic;

namespace ProcessHacker.Common
{




    public interface ISubtractor<T>
    {



        T Subtract(T v1, T v2);
    }

    public static class Subtractor
    {
        private static Int64Subtractor _int64Subtractor = new Int64Subtractor();
        private static Int32Subtractor _int32Subtractor = new Int32Subtractor();
        private static DoubleSubtractor _doubleSubtractor = new DoubleSubtractor();
        private static FloatSubtractor _floatSubtractor = new FloatSubtractor();

        public static Int64Subtractor Int64Subtractor
        {
            get { return _int64Subtractor; }
        }

        public static Int32Subtractor Int32Subtractor
        {
            get { return _int32Subtractor; }
        }

        public static DoubleSubtractor DoubleSubtractor
        {
            get { return _doubleSubtractor; }
        }

        public static FloatSubtractor FloatSubtractor
        {
            get { return _floatSubtractor; }
        }
    }




    public class Int64Subtractor : ISubtractor<long>
    {
        public long Subtract(long v1, long v2)
        {
            return v1 - v2;
        }
    }




    public class Int32Subtractor : ISubtractor<int>
    {
        public int Subtract(int v1, int v2)
        {
            return v1 - v2;
        }
    }




    public class DoubleSubtractor : ISubtractor<double>
    {
        public double Subtract(double v1, double v2)
        {
            return v1 - v2;
        }
    }




    public class FloatSubtractor : ISubtractor<float>
    {
        public float Subtract(float v1, float v2)
        {
            return v1 - v2;
        }
    }




    public sealed class DeltaManager<TKey, TValue>
    {
        private Dictionary<TKey, TValue> _values;
        private Dictionary<TKey, TValue> _deltas;
        private ISubtractor<TValue> _subtractor;





        public DeltaManager(ISubtractor<TValue> subtractor)
        {
            _subtractor = subtractor;
            _values = new Dictionary<TKey, TValue>();
            _deltas = new Dictionary<TKey, TValue>();
        }

        public DeltaManager(ISubtractor<TValue> subtractor, IEqualityComparer<TKey> comparer)
        {
            _subtractor = subtractor;
            _values = new Dictionary<TKey, TValue>(comparer);
            _deltas = new Dictionary<TKey, TValue>(comparer);
        }

        public TValue this[TKey key]
        {
            get { return _deltas[key]; }
            set { _deltas[key] = value; }
        }

        public TValue GetDelta(TKey key)
        {
            return _deltas[key];
        }

        public void Add(TKey key, TValue initialValue)
        {
            _values.Add(key, initialValue);
            _deltas.Add(key, initialValue);
        }

        public void SetDelta(TKey key, TValue value)
        {
            _deltas[key] = value;
        }

        public TValue Update(TKey key, TValue value)
        {
            _deltas[key] = _subtractor.Subtract(value, _values[key]);
            _values[key] = value;

            return _deltas[key];
        }
    }
}
