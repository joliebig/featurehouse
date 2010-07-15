

using System;
using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public class EnlistmentHandle : NativeHandle<EnlistmentAccess>
    {
        public static EnlistmentHandle Create(
            EnlistmentAccess access,
            string name,
            ObjectFlags objectFlags,
            DirectoryHandle rootDirectory,
            ResourceManagerHandle resourceManagerHandle,
            TransactionHandle transactionHandle,
            EnlistmentOptions createOptions,
            NotificationMask notificationMask,
            IntPtr enlistmentKey
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateEnlistment(
                    out handle,
                    access,
                    resourceManagerHandle,
                    transactionHandle,
                    ref oa,
                    createOptions,
                    notificationMask,
                    enlistmentKey
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new EnlistmentHandle(handle, true);
        }

        public static EnlistmentHandle FromHandle(IntPtr handle)
        {
            return new EnlistmentHandle(handle, false);
        }

        private EnlistmentHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public EnlistmentHandle(
            string name,
            ObjectFlags objectFlags,
            DirectoryHandle rootDirectory,
            ResourceManagerHandle resourceManagerHandle,
            Guid guid,
            EnlistmentAccess access
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtOpenEnlistment(
                    out handle,
                    access,
                    resourceManagerHandle,
                    ref guid,
                    ref oa
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            this.Handle = handle;
        }

        public void Commit(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtCommitEnlistment(this, ref virtualClock)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void CommitComplete(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtCommitComplete(this, ref virtualClock)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public EnlistmentBasicInformation GetBasicInformation()
        {
            NtStatus status;
            EnlistmentBasicInformation basicInfo;
            int retLength;

            if ((status = Win32.NtQueryInformationEnlistment(
                this,
                EnlistmentInformationClass.EnlistmentBasicInformation,
                out basicInfo,
                Marshal.SizeOf(typeof(EnlistmentBasicInformation)),
                out retLength
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return basicInfo;
        }

        public void Prepare(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtPrepareEnlistment(this, ref virtualClock)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void PrepareComplete(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtPrepareComplete(this, ref virtualClock)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void PrePrepare(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtPrePrepareEnlistment(this, ref virtualClock)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void PrePrepareComplete(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtPrePrepareComplete(this, ref virtualClock)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void ReadOnly(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtReadOnlyEnlistment(this, ref virtualClock)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void Recover(IntPtr enlistmentKey)
        {
            NtStatus status;

            if ((status = Win32.NtRecoverEnlistment(this, enlistmentKey)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void RejectSinglePhase(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtSinglePhaseReject(this, ref virtualClock)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void Rollback(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtRollbackEnlistment(this, ref virtualClock)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }

        public void RollbackComplete(long virtualClock)
        {
            NtStatus status;

            if ((status = Win32.NtRollbackComplete(this, ref virtualClock)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
    }
}
