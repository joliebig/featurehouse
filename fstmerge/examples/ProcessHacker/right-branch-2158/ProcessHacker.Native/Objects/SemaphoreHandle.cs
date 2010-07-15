

using System;
using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public sealed class SemaphoreHandle : NativeHandle<SemaphoreAccess>
    {
        public static SemaphoreHandle Create(SemaphoreAccess access, int initialCount, int maximumCount)
        {
            return Create(access, null, initialCount, maximumCount);
        }

        public static SemaphoreHandle Create(SemaphoreAccess access, string name, int initialCount, int maximumCount)
        {
            return Create(access, name, 0, null, initialCount, maximumCount);
        }

        public static SemaphoreHandle Create(SemaphoreAccess access, string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, int initialCount, int maximumCount)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateSemaphore(out handle, access, ref oa,
                    initialCount, maximumCount)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new SemaphoreHandle(handle, true);
        }

        public static SemaphoreHandle FromHandle(IntPtr handle)
        {
            return new SemaphoreHandle(handle, false);
        }

        private SemaphoreHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public SemaphoreHandle(string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, SemaphoreAccess access)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtOpenSemaphore(out handle, access, ref oa)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            this.Handle = handle;
        }

        public SemaphoreHandle(string name, SemaphoreAccess access)
            : this(name, 0, null, access)
        { }

        public SemaphoreBasicInformation GetBasicInformation()
        {
            NtStatus status;
            SemaphoreBasicInformation sbi;
            int retLength;

            if ((status = Win32.NtQuerySemaphore(this, SemaphoreInformationClass.SemaphoreBasicInformation,
                out sbi, Marshal.SizeOf(typeof(SemaphoreBasicInformation)), out retLength)) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return sbi;
        }

        public int Release()
        {
            return this.Release(1);
        }

        public int Release(int count)
        {
            NtStatus status;
            int previousCount;

            if ((status = Win32.NtReleaseSemaphore(this, count, out previousCount)) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return previousCount;
        }
    }
}
