

using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Threading
{



    public sealed class Semaphore : NativeObject<SemaphoreHandle>
    {



        public Semaphore()
            : this(null)
        { }






        public Semaphore(int initialCount, int maximumCount)
            : this(null, initialCount, maximumCount)
        { }







        public Semaphore(string name)
            : this(name, 1, 1)
        { }







        public Semaphore(string name, int initialCount, int maximumCount)
        {
            this.Handle = SemaphoreHandle.Create(
                SemaphoreAccess.All,
                name,
                ObjectFlags.OpenIf,
                null,
                initialCount,
                maximumCount
                );
        }




        public int Count
        {
            get { return this.Handle.GetBasicInformation().CurrentCount; }
        }




        public int MaximumCount
        {
            get { return this.Handle.GetBasicInformation().MaximumCount; }
        }




        public void Release()
        {
            this.Handle.Release();
        }






        public void Release(int count)
        {
            this.Handle.Release(count);
        }
    }
}
