

using System;
using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public class TmHandle : NativeHandle<TmAccess>
    {
        public static TmHandle Create(
            TmAccess access,
            string name,
            ObjectFlags objectFlags,
            DirectoryHandle rootDirectory,
            string logFileName,
            TmOptions createOptions
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                UnicodeString logFileNameStr = new UnicodeString(logFileName);

                try
                {
                    if ((status = Win32.NtCreateTransactionManager(
                        out handle,
                        access,
                        ref oa,
                        ref logFileNameStr,
                        createOptions,
                        0
                        )) >= NtStatus.Error)
                        Win32.ThrowLastError(status);
                }
                finally
                {
                    logFileNameStr.Dispose();
                }
            }
            finally
            {
                oa.Dispose();
            }

            return new TmHandle(handle, true);
        }

        public static TmHandle FromHandle(IntPtr handle)
        {
            return new TmHandle(handle, false);
        }

        private TmHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public TmHandle(string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, TmAccess access)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtOpenTransactionManager(
                    out handle,
                    access,
                    ref oa,
                    IntPtr.Zero,
                    IntPtr.Zero,
                    0
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            this.Handle = handle;
        }

        public TmBasicInformation GetBasicInformation()
        {
            NtStatus status;
            TmBasicInformation basicInfo;
            int retLength;

            if ((status = Win32.NtQueryInformationTransactionManager(
                this,
                TmInformationClass.TransactionManagerBasicInformation,
                out basicInfo,
                Marshal.SizeOf(typeof(TmBasicInformation)),
                out retLength
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return basicInfo;
        }

        public long GetLastRecoveredLsn()
        {
            NtStatus status;
            TmRecoveryInformation recoveryInfo;
            int retLength;

            if ((status = Win32.NtQueryInformationTransactionManager(
                this,
                TmInformationClass.TransactionManagerRecoveryInformation,
                out recoveryInfo,
                Marshal.SizeOf(typeof(TmRecoveryInformation)),
                out retLength
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return recoveryInfo.LastRecoveredLsn;
        }

        public string GetLogFileName()
        {
            NtStatus status;
            int retLength;

            using (var data = new MemoryAlloc(0x1000))
            {
                status = Win32.NtQueryInformationTransactionManager(
                    this,
                    TmInformationClass.TransactionManagerLogPathInformation,
                    data,
                    data.Size,
                    out retLength
                    );

                if (status == NtStatus.BufferTooSmall)
                {

                    data.Resize(retLength);

                    status = Win32.NtQueryInformationTransactionManager(
                        this,
                        TmInformationClass.TransactionManagerLogPathInformation,
                        data,
                        data.Size,
                        out retLength
                        );
                }

                if (status >= NtStatus.Error)
                    Win32.ThrowLastError(status);

                TmLogPathInformation logPathInfo = data.ReadStruct<TmLogPathInformation>();

                return data.ReadUnicodeString(TmLogPathInformation.LogPathOffset, logPathInfo.LogPathLength);
            }
        }

        public Guid GetLogIdentity()
        {
            NtStatus status;
            TmLogInformation logInfo;
            int retLength;

            if ((status = Win32.NtQueryInformationTransactionManager(
                this,
                TmInformationClass.TransactionManagerLogInformation,
                out logInfo,
                Marshal.SizeOf(typeof(TmLogInformation)),
                out retLength
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return logInfo.LogIdentity;
        }

        public void Recover()
        {
            NtStatus status;

            if ((status = Win32.NtRecoverTransactionManager(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void Rollforward(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtRollforwardTransactionManager(
                this,
                ref virtualClock
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
    }
}
