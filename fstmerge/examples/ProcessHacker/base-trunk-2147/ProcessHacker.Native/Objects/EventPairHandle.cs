

using System;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{



    public sealed class EventPairHandle : NativeHandle<EventPairAccess>
    {





        public static EventPairHandle Create(EventPairAccess access)
        {
            return Create(access, null, 0, null);
        }
        public static EventPairHandle Create(EventPairAccess access, string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;
            try
            {
                if ((status = Win32.NtCreateEventPair(out handle, access, ref oa)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }
            return new EventPairHandle(handle, true);
        }
        public static EventPairHandle FromHandle(IntPtr handle)
        {
            return new EventPairHandle(handle, false);
        }
        private EventPairHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }
        public EventPairHandle(string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, EventPairAccess access)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;
            try
            {
                if ((status = Win32.NtOpenEventPair(out handle, access, ref oa)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }
            this.Handle = handle;
        }
        public EventPairHandle(string name, EventPairAccess access)
            : this(name, 0, null, access)
        { }
        public void SetHigh()
        {
            NtStatus status;
            if ((status = Win32.NtSetHighEventPair(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public NtStatus SetHighWaitLow()
        {
            NtStatus status;
            if ((status = Win32.NtSetHighWaitLowEventPair(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return status;
        }
        public void SetLow()
        {
            NtStatus status;
            if ((status = Win32.NtSetLowEventPair(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
        public NtStatus SetLowWaitHigh()
        {
            NtStatus status;
            if ((status = Win32.NtSetLowWaitHighEventPair(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return status;
        }
        public NtStatus WaitHigh()
        {
            NtStatus status;
            if ((status = Win32.NtWaitHighEventPair(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return status;
        }
        public NtStatus WaitLow()
        {
            NtStatus status;
            if ((status = Win32.NtWaitLowEventPair(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return status;
        }
    }
}
