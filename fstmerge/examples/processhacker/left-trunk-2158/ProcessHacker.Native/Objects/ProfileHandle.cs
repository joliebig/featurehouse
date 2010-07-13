

using System;
using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public sealed class ProfileHandle : NativeHandle<ProfileAccess>
    {
        public static ProfileHandle Create(
            ProcessHandle processHandle,
            IntPtr rangeBase,
            uint rangeSize,
            int bucketSize,
            KProfileSource profileSource,
            IntPtr affinity
            )
        {
            NtStatus status;
            IntPtr handle;

            if (bucketSize < 2 || bucketSize > 30)
                throw new ArgumentException("Bucket size must be between 2 and 30, inclusive.");

            unchecked
            {
                uint realBucketSize = (uint)(2 << (bucketSize - 1));
                MemoryAlloc buffer = new MemoryAlloc((int)((rangeSize - 1) / realBucketSize + 1) * sizeof(int));

                if ((status = Win32.NtCreateProfile(
                    out handle,
                    processHandle ?? IntPtr.Zero,
                    rangeBase,
                    new IntPtr(rangeSize),
                    bucketSize,
                    buffer,
                    buffer.Size,
                    profileSource,
                    affinity
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);

                return new ProfileHandle(handle, true, rangeBase, rangeSize, realBucketSize, buffer);
            }
        }

        public static int GetInterval(KProfileSource profileSource)
        {
            NtStatus status;
            int interval;

            if ((status = Win32.NtQueryIntervalProfile(profileSource, out interval)) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return interval;
        }

        public static void SetInterval(KProfileSource profileSource, int interval)
        {
            NtStatus status;

            if ((status = Win32.NtSetIntervalProfile(interval, profileSource)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        private IntPtr _rangeBase;
        private uint _rangeSize;
        private uint _bucketSize;
        private MemoryAlloc _buffer;

        private ProfileHandle(
            IntPtr handle,
            bool owned,
            IntPtr rangeBase,
            uint rangeSize,
            uint bucketSize,
            MemoryAlloc buffer
            )
            : base(handle, owned)
        {
            _rangeBase = rangeBase;
            _rangeSize = rangeSize;
            _bucketSize = bucketSize;
            _buffer = buffer;
        }

        protected override void Close()
        {
            _buffer.Dispose();

            base.Close();
        }

        public int[] Collect()
        {
            int[] counters = new int[_buffer.Size / sizeof(int)];

            Marshal.Copy(_buffer, counters, 0, counters.Length);

            return counters;
        }

        public void Start()
        {
            NtStatus status;

            if ((status = Win32.NtStartProfile(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void Stop()
        {
            NtStatus status;

            if ((status = Win32.NtStopProfile(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
    }
}
