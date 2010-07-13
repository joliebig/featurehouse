

using System;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public sealed class SymbolicLinkHandle : NativeHandle<SymbolicLinkAccess>
    {
        public static SymbolicLinkHandle Create(SymbolicLinkAccess access, string name, string linkTarget)
        {
            return Create(access, name, 0, null, linkTarget);
        }

        public static SymbolicLinkHandle Create(SymbolicLinkAccess access, string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, string linkTarget)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                UnicodeString linkTargetString = new UnicodeString(linkTarget);

                try
                {
                    if ((status = Win32.NtCreateSymbolicLinkObject(out handle, access,
                        ref oa, ref linkTargetString)) >= NtStatus.Error)
                        Win32.ThrowLastError(status);
                }
                finally
                {
                    linkTargetString.Dispose();
                }
            }
            finally
            {
                oa.Dispose();
            }

            return new SymbolicLinkHandle(handle, true);
        }

        private SymbolicLinkHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public SymbolicLinkHandle(string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, SymbolicLinkAccess access)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtOpenSymbolicLinkObject(out handle, access, ref oa)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            this.Handle = handle;
        }

        public SymbolicLinkHandle(string name, SymbolicLinkAccess access)
            : this(name, 0, null, access)
        { }

        public string GetTarget()
        {
            NtStatus status;
            int retLength;
            UnicodeString str = new UnicodeString();

            using (var buffer = new MemoryAlloc(0x200))
            {
                str.Length = 0;
                str.MaximumLength = (ushort)buffer.Size;
                str.Buffer = buffer;

                if ((status = Win32.NtQuerySymbolicLinkObject(this, ref str, out retLength)) >= NtStatus.Error)
                {
                    buffer.Resize(retLength);
                    str.MaximumLength = (ushort)retLength;
                    str.Buffer = buffer;
                }

                if ((status = Win32.NtQuerySymbolicLinkObject(this, ref str, out retLength)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);

                return str.Read();
            }
        }
    }
}
