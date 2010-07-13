

using System;
using System.Collections.Generic;
using System.Threading;

namespace ProcessHacker.Common.Objects
{



    public class OutOfOrderException : Exception
    {
        public OutOfOrderException(string message)
            : base(message)
        { }
    }




    public sealed class DelayedReleasePool : BaseObject
    {



        [Flags]
        private enum DelayedReleaseFlags
        {
            Dispose = 0x1,
            Dereference = 0x2
        }




        private struct DelayedReleaseObject
        {
            private DelayedReleaseFlags _flags;
            private BaseObject _object;

            public DelayedReleaseObject(DelayedReleaseFlags flags, BaseObject obj)
            {
                _flags = flags;
                _object = obj;
            }

            public DelayedReleaseFlags Flags
            {
                get { return _flags; }
            }

            public BaseObject Object
            {
                get { return _object; }
            }
        }

        [ThreadStatic]
        private static Stack<DelayedReleasePool> _poolStack;
        [ThreadStatic]
        private static DelayedReleasePool _currentPool;




        public static DelayedReleasePool CurrentPool
        {
            get
            {
                if (_currentPool == null)
                    _currentPool = new DelayedReleasePool();

                return _currentPool;
            }
            private set { _currentPool = value; }
        }




        private static Stack<DelayedReleasePool> PoolStack
        {
            get
            {

                if (_poolStack == null)
                    _poolStack = new Stack<DelayedReleasePool>();

                return _poolStack;
            }
        }





        private static void PopPool(DelayedReleasePool pool)
        {
            if (_currentPool != pool)
                throw new OutOfOrderException(
                    "Attempted to pop a pool when it wasn't on top of the stack. " +
                    "This usually indicates that a pool was popped out-of-order."
                    );

            _currentPool = PoolStack.Pop();
        }





        private static void PushPool(DelayedReleasePool pool)
        {
            PoolStack.Push(_currentPool);
            _currentPool = pool;
        }

        private int _creatorThreadId;
        private List<DelayedReleaseObject> _objects = new List<DelayedReleaseObject>();




        public DelayedReleasePool()
        {
            _creatorThreadId = Thread.CurrentThread.ManagedThreadId;
            PushPool(this);
        }

        protected override void DisposeObject(bool disposing)
        {




            if (
                disposing &&
                _creatorThreadId == Thread.CurrentThread.ManagedThreadId
                )
                PopPool(this);

            this.Drain(disposing);
        }





        public void AddDereference(BaseObject obj)
        {
            _objects.Add(new DelayedReleaseObject(DelayedReleaseFlags.Dereference, obj));
        }





        public void AddDispose(BaseObject obj)
        {
            _objects.Add(new DelayedReleaseObject(DelayedReleaseFlags.Dispose, obj));
        }




        public void Drain()
        {
            this.Drain(true);
        }





        public void Drain(bool managed)
        {
            foreach (var obj in _objects)
            {
                if ((obj.Flags & DelayedReleaseFlags.Dispose) == DelayedReleaseFlags.Dispose)
                    obj.Object.Dispose();
                if ((obj.Flags & DelayedReleaseFlags.Dereference) == DelayedReleaseFlags.Dereference)
                    obj.Object.Dereference(managed);
            }

            _objects.Clear();
        }
    }
}
