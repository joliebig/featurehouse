

using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Threading
{



    public sealed class Event : NativeObject<EventHandle>
    {



        public Event()
            : this(null)
        { }
        public Event(bool autoReset, bool initialState)
            : this(null, autoReset, initialState)
        { }
        public Event(string name)
            : this(name, false, false)
        { }
        public Event(string name, bool autoReset, bool initialState)
        {
            this.Handle = EventHandle.Create(
                EventAccess.All,
                name,
                ObjectFlags.OpenIf,
                null,
                autoReset ? EventType.SynchronizationEvent : EventType.NotificationEvent,
                initialState
                );
        }
        public bool AutoReset
        {
            get
            {
                return this.Handle.GetBasicInformation().EventType ==
                    EventType.SynchronizationEvent;
            }
        }
        public bool Signaled
        {
            get { return this.Handle.GetBasicInformation().EventState != 0; }
        }
        public void Pulse()
        {
            this.Handle.Pulse();
        }
        public void Reset()
        {
            this.Handle.Reset();
        }
        public void Set()
        {
            this.Handle.Set();
        }
    }
}
