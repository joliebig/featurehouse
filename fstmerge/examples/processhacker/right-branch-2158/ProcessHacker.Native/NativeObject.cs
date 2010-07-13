

using System;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;

namespace ProcessHacker.Native
{
    public class NativeObject : IDisposable
    {
        public static void WaitAll(NativeObject[] objects)
        {
            NativeHandle.WaitAll(ObjectsToISync(objects));
        }

        public static void WaitAll(NativeObject[] objects, int timeout)
        {
            NativeHandle.WaitAll(ObjectsToISync(objects), false, timeout * Win32.TimeMsTo100Ns, true);
        }

        public static void WaitAll(NativeObject[] objects, DateTime timeout)
        {
            NativeHandle.WaitAll(ObjectsToISync(objects), false, timeout.ToFileTime(), false);
        }

        public static void WaitAny(NativeObject[] objects)
        {
            NativeHandle.WaitAny(ObjectsToISync(objects));
        }

        public static void WaitAny(NativeObject[] objects, int timeout)
        {
            NativeHandle.WaitAny(ObjectsToISync(objects), false, timeout * Win32.TimeMsTo100Ns, true);
        }

        public static void WaitAny(NativeObject[] objects, DateTime timeout)
        {
            NativeHandle.WaitAny(ObjectsToISync(objects), false, timeout.ToFileTime(), false);
        }

        private static ISynchronizable[] ObjectsToISync(NativeObject[] objects)
        {
            ISynchronizable[] newArray = new ISynchronizable[objects.Length];

            for (int i = 0; i < newArray.Length; i++)
                newArray[i] = objects[i].Handle;

            return newArray;
        }

        private NativeHandle _handle;




        public void Dispose()
        {
            _handle.Dispose();
        }




        public NativeHandle Handle
        {
            get { return _handle; }
            protected set { _handle = value; }
        }





        public WaitStatus SignalAndWait(NativeObject obj)
        {
            return (WaitStatus)_handle.SignalAndWait(obj.Handle);
        }






        public WaitStatus SignalAndWait(NativeObject obj, int timeout)
        {
            return (WaitStatus)_handle.SignalAndWait(obj.Handle, false, timeout * Win32.TimeMsTo100Ns);
        }






        public WaitStatus SignalAndWait(NativeObject obj, DateTime timeout)
        {
            return (WaitStatus)_handle.SignalAndWait(obj.Handle, false, timeout.ToFileTime(), false);
        }




        public WaitStatus Wait()
        {
            return (WaitStatus)_handle.Wait();
        }





        public WaitStatus Wait(int timeout)
        {
            return (WaitStatus)_handle.Wait(timeout * Win32.TimeMsTo100Ns, true);
        }





        public WaitStatus Wait(DateTime timeout)
        {
            return (WaitStatus)_handle.Wait(timeout.ToFileTime(), false);
        }
    }

    public class NativeObject<THandle> : NativeObject
        where THandle : NativeHandle
    {
        protected new THandle Handle
        {
            get { return base.Handle as THandle; }
            set { base.Handle = value; }
        }
    }

    public enum WaitStatus : uint
    {
        Wait0 = 0x00000000,
        Wait1 = 0x00000001,
        Wait2 = 0x00000002,
        Wait3 = 0x00000003,
        Wait4 = 0x00000004,
        Wait5 = 0x00000005,
        Wait6 = 0x00000006,
        Wait7 = 0x00000007,
        Wait63 = 0x0000003f,
        Abandoned = 0x00000080,
        AbandonedWait0 = 0x00000080,
        AbandonedWait1 = 0x00000081,
        AbandonedWait2 = 0x00000082,
        AbandonedWait3 = 0x00000083,
        AbandonedWait4 = 0x00000084,
        AbandonedWait5 = 0x00000085,
        AbandonedWait6 = 0x00000086,
        AbandonedWait7 = 0x00000087,
        AbandonedWait63 = 0x000000bf,
        UserApc = 0x000000c0,
        KernelApc = 0x00000100,
        Alerted = 0x00000101,
        Timeout = 0x00000102
    }
}
