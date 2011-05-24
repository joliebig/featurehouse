using System;
using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public sealed class TimerHandle : NativeHandle<TimerAccess>
    {
        public static TimerHandle Create(TimerAccess access, TimerType type)
        {
            return Create(access, null, type);
        }
        public static TimerHandle Create(TimerAccess access, string name, TimerType type)
        {
            return Create(access, name, 0, null, type);
        }
        public static TimerHandle Create(TimerAccess access, string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, TimerType type)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;
            try
            {
                if ((status = Win32.NtCreateTimer(out handle, access, ref oa, type)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }
            return new TimerHandle(handle, true);
        }
        public static TimerHandle FromHandle(IntPtr handle)
        {
            return new TimerHandle(handle, false);
        }
        private TimerApcRoutine _routine;
        private TimerHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }
        public TimerHandle(string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, TimerAccess access)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;
            try
            {
                if ((status = Win32.NtOpenTimer(out handle, access, ref oa)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }
            this.Handle = handle;
        }
        public TimerHandle(string name, TimerAccess access)
            : this(name, 0, null, access)
        { }
        public bool Cancel()
        {
            NtStatus status;
            bool currentState;
            if ((status = Win32.NtCancelTimer(this, out currentState)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return currentState;
        }
        public TimerBasicInformation GetBasicInformation()
        {
            NtStatus status;
            TimerBasicInformation tbi;
            int retLength;
            if ((status = Win32.NtQueryTimer(this, TimerInformationClass.TimerBasicInformation,
                out tbi, Marshal.SizeOf(typeof(TimerBasicInformation)), out retLength)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return tbi;
        }
        public bool Set(DateTime dueTime, int period)
        {
            return this.Set(dueTime.ToFileTime(), false, null, IntPtr.Zero, period);
        }
        public bool Set(long dueTime, int period)
        {
            return this.Set(dueTime, null, period);
        }
        public bool Set(long dueTime, TimerApcRoutine routine, int period)
        {
            return this.Set(dueTime, true, routine, IntPtr.Zero, period);
        }
        public bool Set(long dueTime, bool relative, TimerApcRoutine routine, IntPtr context, int period)
        {
            return this.Set(dueTime, relative, routine, context, false, period);
        }
        public bool Set(long dueTime, bool relative, TimerApcRoutine routine, IntPtr context, bool resume, int period)
        {
            NtStatus status;
            long realDueTime = relative ? -dueTime : dueTime;
            bool previousState;
            _routine = routine;
            if ((status = Win32.NtSetTimer(
                this,
                ref realDueTime,
                routine,
                context,
                resume,
                period,
                out previousState
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return previousState;
        }
    }
}
