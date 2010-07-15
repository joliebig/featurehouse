

using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Threading
{



    public sealed class EventPair : NativeObject<EventPairHandle>
    {



        public EventPair()
            : this(null)
        { }
        public EventPair(string name)
        {
            this.Handle = EventPairHandle.Create(
                EventPairAccess.All,
                name,
                ObjectFlags.OpenIf,
                null
                );
        }
        public void SetHigh()
        {
            this.Handle.SetHigh();
        }
        public WaitStatus SetHighWaitLow()
        {
            return (WaitStatus)this.Handle.SetHighWaitLow();
        }
        public void SetLow()
        {
            this.Handle.SetLow();
        }
        public WaitStatus SetLowWaitHigh()
        {
            return (WaitStatus)this.Handle.SetLowWaitHigh();
        }
        public WaitStatus WaitHigh()
        {
            return (WaitStatus)this.Handle.WaitHigh();
        }
        public WaitStatus WaitLow()
        {
            return (WaitStatus)this.Handle.WaitLow();
        }
    }
}
