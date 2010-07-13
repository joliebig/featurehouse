

using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public sealed class IoCompletionHandle : NativeHandle<IoCompletionAccess>
    {
        public static IoCompletionHandle Create(IoCompletionAccess access)
        {
            return Create(access, 0);
        }

        public static IoCompletionHandle Create(IoCompletionAccess access, int count)
        {
            return Create(access, null, count);
        }

        public static IoCompletionHandle Create(IoCompletionAccess access, string name, int count)
        {
            return Create(access, name, 0, null, count);
        }

        public static IoCompletionHandle Create(IoCompletionAccess access, string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, int count)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateIoCompletion(out handle, access, ref oa, count)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new IoCompletionHandle(handle, true);
        }

        private IoCompletionHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public IoCompletionHandle(string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, IoCompletionAccess access)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtOpenIoCompletion(out handle, access, ref oa)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            this.Handle = handle;
        }

        public IoCompletionHandle(string name, IoCompletionAccess access)
            : this(name, 0, null, access)
        { }

        public IoStatusBlock Remove(out IntPtr keyContext, out IntPtr apcContext, long timeout)
        {
            return this.Remove(out keyContext, out apcContext, timeout, true);
        }

        public IoStatusBlock Remove(out IntPtr keyContext, out IntPtr apcContext, long timeout, bool relative)
        {
            NtStatus status;
            IoStatusBlock ioStatus;
            long realTimeout = relative ? -timeout : timeout;

            if ((status = Win32.NtRemoveIoCompletion(
                this, out keyContext, out apcContext, out ioStatus, ref realTimeout)) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return ioStatus;
        }

        public void Set(IntPtr keyContext, IntPtr apcContext, NtStatus ioStatus, IntPtr ioInformation)
        {
            NtStatus status;

            if ((status = Win32.NtSetIoCompletion(
                this, keyContext, apcContext, ioStatus, ioInformation)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
    }
}
