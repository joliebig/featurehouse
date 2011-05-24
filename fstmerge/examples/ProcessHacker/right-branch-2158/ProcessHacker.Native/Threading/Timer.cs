

using System;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Threading
{




    public delegate void TimerCallback(IntPtr context);




    public sealed class Timer : NativeObject<TimerHandle>
    {
        private TimerCallback _callback;




        public Timer()
            : this(null)
        { }
        public Timer(bool autoReset)
            : this(null, autoReset)
        { }
        public Timer(string name)
            : this(name, false)
        { }
        public Timer(string name, bool autoReset)
        {
            this.Handle = TimerHandle.Create(
                TimerAccess.All,
                name,
                ObjectFlags.OpenIf,
                null,
                autoReset ? TimerType.SynchronizationTimer : TimerType.NotificationTimer
                );
        }
        public TimeSpan RemainingTime
        {
            get { return new TimeSpan(this.Handle.GetBasicInformation().RemainingTime); }
        }
        public bool Signaled
        {
            get { return this.Handle.GetBasicInformation().TimerState; }
        }
        public void Cancel()
        {
            this.Handle.Cancel();
        }
        public void Set(int dueTime)
        {
            this.Set(dueTime, 0);
        }
        public void Set(int dueTime, int period)
        {
            this.Set(null, dueTime, period);
        }
        public void Set(TimerCallback callback, int dueTime, int period)
        {
            this.Set(callback, dueTime, period, IntPtr.Zero);
        }
        public void Set(TimerCallback callback, int dueTime, int period, IntPtr context)
        {
            TimerApcRoutine apcRoutine = (context_, lowPart, highPart) => callback(context_);
            _callback = callback;
            this.Handle.Set(
                dueTime * Win32.TimeMsTo100Ns,
                true,
                callback != null ? apcRoutine : null,
                context,
                period
                );
        }
        public void Set(DateTime dueTime)
        {
            this.Set(dueTime, 0);
        }
        public void Set(DateTime dueTime, int period)
        {
            this.Set(null, dueTime, period);
        }
        public void Set(TimerCallback callback, DateTime dueTime, int period)
        {
            this.Set(callback, dueTime, period, IntPtr.Zero);
        }
        public void Set(TimerCallback callback, DateTime dueTime, int period, IntPtr context)
        {
            TimerApcRoutine apcRoutine = (context_, lowPart, highPart) => callback(context_);
            _callback = callback;
            this.Handle.Set(
                dueTime.ToFileTime(),
                false,
                callback != null ? apcRoutine : null,
                context,
                period
                );
        }
    }
}
