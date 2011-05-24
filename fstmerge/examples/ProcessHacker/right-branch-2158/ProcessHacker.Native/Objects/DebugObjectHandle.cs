

using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public sealed class DebugObjectHandle : NativeHandle<DebugObjectAccess>
    {
        public static DebugObjectHandle Create(DebugObjectAccess access, DebugObjectFlags flags)
        {
            return Create(access, null, flags);
        }

        public static DebugObjectHandle Create(DebugObjectAccess access, string name, DebugObjectFlags flags)
        {
            return Create(access, name, 0, null, flags);
        }

        public static DebugObjectHandle Create(DebugObjectAccess access, string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, DebugObjectFlags flags)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateDebugObject(
                    out handle,
                    access,
                    ref oa,
                    flags
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new DebugObjectHandle(handle, true);
        }

        public DebugObjectHandle FromHandle(IntPtr handle)
        {
            return new DebugObjectHandle(handle, false);
        }

        internal DebugObjectHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public DebugObjectHandle(ProcessHandle processHandle)
        {
            this.Handle = processHandle.GetDebugObjectHandle();


            if (this.Handle == IntPtr.Zero)
                throw new WindowsException(NtStatus.DebuggerInactive);
        }

        public void Continue(ClientId cid, NtStatus continueStatus)
        {
            NtStatus status;

            if ((status = Win32.NtDebugContinue(
                this,
                ref cid,
                continueStatus
                )) > NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void SetFlags(DebugObjectFlags flags)
        {
            unsafe
            {
                NtStatus status;
                int retLength;

                if ((status = Win32.NtSetInformationDebugObject(
                    this,
                    DebugObjectInformationClass.DebugObjectFlags,
                    new IntPtr(&flags),
                    sizeof(DebugObjectFlags),
                    out retLength
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
        }

        public IntPtr WaitForDebugEvent(bool alertable, long timeout, bool timeoutRelative)
        {

            throw new NotImplementedException();
        }
    }
}
