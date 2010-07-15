




using System;
using System.ComponentModel;
using System.Threading;

namespace ProcessHacker.Common.Objects
{
    public abstract class BaseObject : IDisposable, IRefCounted
    {
        private static int _createdCount = 0;
        private static int _freedCount = 0;
        private static int _disposedCount = 0;
        private static int _finalizedCount = 0;
        private static int _referencedCount = 0;
        private static int _dereferencedCount = 0;
        public static int CreatedCount { get { return _createdCount; } }
        public static int FreedCount { get { return _freedCount; } }
        public static int DisposedCount { get { return _disposedCount; } }
        public static int FinalizedCount { get { return _finalizedCount; } }
        public static int ReferencedCount { get { return _referencedCount; } }
        public static int DereferencedCount { get { return _dereferencedCount; } }
        public static T SwapRef<T>(ref T reference, T newObj)
            where T : class, IRefCounted
        {
            T oldObj;
            oldObj = Interlocked.Exchange<T>(ref reference, newObj);
            if (newObj != null)
                newObj.Reference();
            if (oldObj != null)
                oldObj.Dereference();
            return oldObj;
        }
        private bool _owned = true;
        private int _ownedByGc = 1;
        private int _refCount = 1;
        private volatile bool _disposed = false;
        public BaseObject()
            : this(true)
        { }
        public BaseObject(bool owned)
        {
            _owned = owned;
            if (!_owned)
            {
                GC.SuppressFinalize(this);
                _ownedByGc = 0;
                _refCount = 0;
            }
        }
        ~BaseObject()
        {
            this.Dispose(false);
        }
        public void Dispose()
        {
            this.Dispose(true);
        }
        public void Dispose(bool managed)
        {
            if (!_owned)
                return;
            Thread.BeginCriticalRegion();
            try
            {
                int oldOwnedByGc;
                oldOwnedByGc = Interlocked.CompareExchange(ref _ownedByGc, 0, 1);
                if (oldOwnedByGc == 1)
                {
                    this.Dereference(managed);
                    if (managed)
                    {
                        GC.SuppressFinalize(this);
                    }
                }
            }
            finally
            {
                Thread.EndCriticalRegion();
            }
        }
        [EditorBrowsable(EditorBrowsableState.Never)]
        public void DisposeDelayed()
        {
            DelayedReleasePool.CurrentPool.AddDispose(this);
        }
        protected abstract void DisposeObject(bool disposing);
        public bool Disposed
        {
            get { return _disposed; }
        }
        public bool Owned
        {
            get { return _owned; }
        }
        public bool OwnedByGc
        {
            get { return _ownedByGc == 1; }
        }
        public int ReferenceCount
        {
            get { return Thread.VolatileRead(ref _refCount); }
        }
        protected void DisableOwnership(bool dispose)
        {
            if (dispose)
                this.Dispose();
            GC.SuppressFinalize(this);
            _owned = false;
        }
        public int Dereference()
        {
            return this.Dereference(true);
        }
        public int Dereference(bool managed)
        {
            return this.Dereference(1, managed);
        }
        public int Dereference(int count)
        {
            return this.Dereference(count, true);
        }
        public int Dereference(int count, bool managed)
        {
            if (count == 0)
                return Interlocked.Add(ref _refCount, 0);
            if (count < 0)
                throw new ArgumentException("Cannot dereference a negative number of times.");
            Thread.BeginCriticalRegion();
            try
            {
                if (!_owned)
                    return 0;
                int newRefCount = Interlocked.Add(ref _refCount, -count);
                if (newRefCount < 0)
                    throw new InvalidOperationException("Reference count cannot be negative.");
                if (newRefCount == 0 && !_disposed)
                {
                    this.DisposeObject(managed);
                    _disposed = true;
                }
                return newRefCount;
            }
            finally
            {
                Thread.EndCriticalRegion();
            }
        }
        [EditorBrowsable(EditorBrowsableState.Never)]
        public void DereferenceDelayed()
        {
            DelayedReleasePool.CurrentPool.AddDereference(this);
        }
        public int Reference()
        {
            return this.Reference(1);
        }
        public int Reference(int count)
        {
            if (!_owned)
                return 0;
            if (count == 0)
                return Interlocked.Add(ref _refCount, 0);
            if (count < 0)
                throw new ArgumentException("Cannot reference a negative number of times.");
            return Interlocked.Add(ref _refCount, count);
        }
    }
}
