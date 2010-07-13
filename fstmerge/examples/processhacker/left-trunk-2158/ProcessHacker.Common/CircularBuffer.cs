

using System;
using System.Collections.Generic;

namespace ProcessHacker.Common
{
    public class CircularBuffer<T> : IList<T>
    {
        private int _size;
        private int _count;
        private int _index;
        private T[] _data;
        public CircularBuffer(int size)
        {
            _size = size;
            _count = 0;
            _index = 0;
            _data = new T[size];
        }
        public T this[int index]
        {
            get
            {
                return _data[(((_index + index) % _size) + _size) % _size];
            }
            set
            {
                _data[(((_index + index) % _size) + _size) % _size] = value;
            }
        }
        public int Count
        {
            get { return _count; }
        }
        public int Size
        {
            get { return _size; }
        }
        public void Add(T value)
        {
            _data[_index = (((_index - 1) % _size) + _size) % _size] = value;
            if (_count < _size)
                _count++;
        }
        public void Resize(int newSize)
        {
            if (newSize == _size)
                return;
            T[] newArray = new T[newSize];
            int tailSize = (_size - _index) % _size;
            int headSize = _count - tailSize;
            if (newSize > _size)
            {
                Array.Copy(_data, _index, newArray, 0, tailSize);
                Array.Copy(_data, 0, newArray, tailSize, headSize);
                _index = 0;
            }
            else if (newSize < _size)
            {
                if (tailSize >= newSize)
                {
                    Array.Copy(_data, _index, newArray, 0, newSize);
                    _index = 0;
                }
                else
                {
                    Array.Copy(_data, _index, newArray, 0, tailSize);
                    Array.Copy(_data, 0, newArray, tailSize, newSize - tailSize);
                    _index = 0;
                }
                if (_count > newSize)
                    _count = newSize;
            }
            _data = newArray;
            _size = newSize;
        }
        public T[] ToArray()
        {
            T[] newArray = new T[this.Count];
            this.CopyTo(newArray, 0);
            return newArray;
        }
        public int IndexOf(T item)
        {
            for (int i = 0; i < this.Count; i++)
                if (this[i].Equals(item))
                    return i;
            return -1;
        }
        public void Insert(int index, T item)
        {
            throw new NotSupportedException();
        }
        public void RemoveAt(int index)
        {
            throw new NotSupportedException();
        }
        public void Clear()
        {
            _count = 0;
        }
        public bool Contains(T item)
        {
            return this.IndexOf(item) != -1;
        }
        public void CopyTo(T[] array, int arrayIndex)
        {
            int tailSize = _size - _index;
            int headSize = _count - tailSize;
            Array.Copy(_data, _index, array, arrayIndex, tailSize);
            Array.Copy(_data, 0, array, arrayIndex + tailSize, headSize);
        }
        public bool IsReadOnly
        {
            get { return false; }
        }
        public bool Remove(T item)
        {
            throw new NotSupportedException();
        }
        public IEnumerator<T> GetEnumerator()
        {
            for (int i = 0; i < this.Count; i++)
                yield return this[i];
        }
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            for (int i = 0; i < this.Count; i++)
                yield return this[i];
        }
    }
}
