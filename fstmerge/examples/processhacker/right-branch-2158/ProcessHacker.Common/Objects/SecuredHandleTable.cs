

using System;

namespace ProcessHacker.Common.Objects
{
    public class SecuredHandleTableEntry : HandleTableEntry
    {
        private long _grantedAccess;

        public long GrantedAccess
        {
            get { return _grantedAccess; }
            set { _grantedAccess = value; }
        }

        public bool AreAllAccessesGranted<TAccess>(TAccess access)
            where TAccess : struct
        {
            long accessLong = Convert.ToInt64(access);

            if ((_grantedAccess & accessLong) == accessLong)
                return true;
            else
                return false;
        }

        public bool AreAnyAccessesGranted<TAccess>(TAccess access)
            where TAccess : struct
        {
            long accessLong = Convert.ToInt64(access);

            if ((_grantedAccess & accessLong) != 0)
                return true;
            else
                return false;
        }
    }




    public class SecuredHandleTable : SecuredHandleTable<SecuredHandleTableEntry>
    { }





    public class SecuredHandleTable<TEntry> : HandleTable<TEntry>
        where TEntry : SecuredHandleTableEntry, new()
    {







        public int Allocate<TAccess>(IRefCounted obj, TAccess grantedAccess)
            where TAccess : struct
        {
            TEntry entry = new TEntry();

            entry.GrantedAccess = Convert.ToInt64(grantedAccess);

            return base.Allocate(obj, entry);
        }
        public IRefCounted ReferenceByHandle<TAccess>(int handle, TAccess access)
            where TAccess : struct
        {
            return this.ReferenceByHandle<TAccess>(handle, access, false);
        }
        public IRefCounted ReferenceByHandle<TAccess>(int handle, TAccess access, bool throwOnAccessDenied)
            where TAccess : struct
        {
            TEntry entry;
            IRefCounted obj;
            obj = this.ReferenceByHandle(handle, out entry);
            if (obj == null)
                return null;
            if (entry.AreAllAccessesGranted<TAccess>(access))
            {
                return obj;
            }
            else
            {
                obj.Dereference();
                if (throwOnAccessDenied)
                    throw new UnauthorizedAccessException("Access denied.");
                else
                    return null;
            }
        }
        public T ReferenceByHandle<T, TAccess>(int handle, TAccess access)
            where T : class, IRefCounted
            where TAccess : struct
        {
            return this.ReferenceByHandle<T, TAccess>(handle, access, false);
        }
        public T ReferenceByHandle<T, TAccess>(int handle, TAccess access, bool throwOnAccessDenied)
            where T : class, IRefCounted
            where TAccess : struct
        {
            IRefCounted obj = this.ReferenceByHandle<TAccess>(handle, access, throwOnAccessDenied);
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
