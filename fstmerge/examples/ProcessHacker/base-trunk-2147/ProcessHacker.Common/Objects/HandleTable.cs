

using System.Collections.Generic;

namespace ProcessHacker.Common.Objects
{
    public class HandleTableEntry
    {
        private int _handle;
        private IRefCounted _object;

        public int Handle
        {
            get { return _handle; }
            internal set { _handle = value; }
        }

        public IRefCounted Object
        {
            get { return _object; }
            internal set { _object = value; }
        }
    }




    public class HandleTable : HandleTable<HandleTableEntry>
    { }





    public class HandleTable<TEntry> : BaseObject
        where TEntry : HandleTableEntry, new()
    {






        public delegate bool EnumerateHandleTableDelegate(int handle, TEntry entry);

        private IdGenerator _handleGenerator = new IdGenerator(4, 4);
        private Dictionary<int, TEntry> _handles =
            new Dictionary<int, TEntry>();

        protected override void DisposeObject(bool disposing)
        {
            lock (_handles)
            {
                foreach (var entry in _handles.Values)
                    entry.Object.Dereference(disposing);
            }
        }






        public int Allocate(IRefCounted obj)
        {
            TEntry entry = new TEntry();

            return this.Allocate(obj, entry);
        }







        public int Allocate(IRefCounted obj, TEntry entry)
        {
            int handle = _handleGenerator.Pop();



            obj.Reference();

            obj.Dispose();

            entry.Handle = handle;
            entry.Object = obj;


            lock (_handles)
            {
                _handles.Add(handle, entry);
            }

            return handle;
        }






        public bool Enumerate(EnumerateHandleTableDelegate callback)
        {
            lock (_handles)
            {
                foreach (var entry in _handles.Values)
                {
                    if (callback(entry.Handle, entry))
                        return true;
                }

                return false;
            }
        }






        public bool Free(int handle)
        {
            IRefCounted obj;

            lock (_handles)
            {
                if (!_handles.ContainsKey(handle))
                    return false;


                obj = _handles[handle].Object;

                _handles.Remove(handle);
            }


            _handleGenerator.Push(handle);

            obj.Dereference();

            return true;
        }






        public TEntry LookupEntry(int handle)
        {
            lock (_handles)
            {
                if (_handles.ContainsKey(handle))
                    return _handles[handle];
                else
                    return null;
            }
        }
        public IRefCounted LookupObject(int handle)
        {
            return this.LookupEntry(handle).Object;
        }
        public T LookupObject<T>(int handle)
            where T : class, IRefCounted
        {
            return this.LookupObject(handle) as T;
        }
        public IRefCounted ReferenceByHandle(int handle)
        {
            TEntry entry;
            return this.ReferenceByHandle(handle, out entry);
        }
        public IRefCounted ReferenceByHandle(int handle, out TEntry entry)
        {
            lock (_handles)
            {
                if (_handles.ContainsKey(handle))
                {
                    IRefCounted obj = _handles[handle].Object;
                    obj.Reference();
                    entry = _handles[handle];
                    return obj;
                }
                else
                {
                    entry = null;
                    return null;
                }
            }
        }
        public T ReferenceByHandle<T>(int handle)
            where T : class, IRefCounted
        {
            TEntry entry;
            return this.ReferenceByHandle<T>(handle, out entry);
        }
        public T ReferenceByHandle<T>(int handle, out TEntry entry)
            where T : class, IRefCounted
        {
            IRefCounted obj = this.ReferenceByHandle(handle, out entry);
            if (obj == null)
                return null;
            if (obj is T)
            {
                return (T)obj;
            }
            else
            {
                obj.Dereference();
                return null;
            }
        }
    }
}
