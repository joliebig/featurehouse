

using System;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{



    public sealed class MailslotHandle : FileHandle
    {
        public static MailslotHandle Create(FileAccess access, string fileName, int maxMessageSize, long readTimeout)
        {
            return Create(
                access,
                fileName,
                ObjectFlags.CaseInsensitive,
                null,
                0,
                maxMessageSize,
                readTimeout,
                0
                );
        }

        public static MailslotHandle Create(
            FileAccess access,
            string fileName,
            ObjectFlags objectFlags,
            FileHandle rootDirectory,
            int quota,
            int maxMessageSize,
            long readTimeout,
            FileCreateOptions createOptions
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(fileName, objectFlags, rootDirectory);
            IoStatusBlock isb;
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateMailslotFile(
                    out handle,
                    access,
                    ref oa,
                    out isb,
                    createOptions,
                    quota,
                    maxMessageSize,
                    ref readTimeout
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new MailslotHandle(handle, true);
        }

        private MailslotHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public MailslotHandle(string fileName, FileAccess access)
            : base(fileName, access)
        { }

        public MailslotHandle(string fileName, FileShareMode shareMode, FileAccess access)
            : base(fileName, shareMode, access)
        { }

        public MailslotHandle(string fileName, FileShareMode shareMode, FileCreateOptions openOptions, FileAccess access)
            : base(fileName, shareMode, openOptions, access)
        { }

        public MailslotHandle(
            string fileName,
            ObjectFlags objectFlags,
            FileHandle rootDirectory,
            FileShareMode shareMode,
            FileCreateOptions openOptions,
            FileAccess access
            )
            : base(fileName, objectFlags, rootDirectory, shareMode, openOptions, access)
        { }
    }
}
