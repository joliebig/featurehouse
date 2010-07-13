

using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;
using System.Runtime.InteropServices;

namespace ProcessHacker.Native.Objects
{
    public sealed class MutantHandle : NativeHandle<MutantAccess>
    {
        public static MutantHandle Create(MutantAccess access, bool initialOwner)
        {
            return Create(access, null, initialOwner);
        }

        public static MutantHandle Create(MutantAccess access, string name, bool initialOwner)
        {
            return Create(access, name, 0, null, initialOwner);
        }

        public static MutantHandle Create(MutantAccess access, string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, bool initialOwner)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateMutant(out handle, access, ref oa, initialOwner)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new MutantHandle(handle, true);
        }

        public static MutantHandle FromHandle(IntPtr handle)
        {
            return new MutantHandle(handle, false);
        }

        private MutantHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public MutantHandle(string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, MutantAccess access)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtOpenMutant(out handle, access, ref oa)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            this.Handle = handle;
        }

        public MutantHandle(string name, MutantAccess access)
            : this(name, 0, null, access)
        { }

        public MutantBasicInformation GetBasicInformation()
        {
            NtStatus status;
            MutantBasicInformation mbi;
            int retLength;

            if ((status = Win32.NtQueryMutant(this, MutantInformationClass.MutantBasicInformation,
                out mbi, Marshal.SizeOf(typeof(MutantBasicInformation)), out retLength)) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return mbi;
        }

        public MutantOwnerInformation GetOwnerInformation()
        {
            NtStatus status;
            MutantOwnerInformation moi;
            int retLength;

            if ((status = Win32.NtQueryMutant(this, MutantInformationClass.MutantOwnerInformation,
                out moi, Marshal.SizeOf(typeof(MutantOwnerInformation)), out retLength)) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return moi;
        }

        public int Release()
        {
            NtStatus status;
            int previousCount;

            if ((status = Win32.NtReleaseMutant(this, out previousCount)) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return previousCount;
        }
    }
}
