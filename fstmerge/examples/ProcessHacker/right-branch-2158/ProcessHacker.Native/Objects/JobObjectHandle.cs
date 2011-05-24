

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{



    public sealed class JobObjectHandle : NativeHandle<JobObjectAccess>
    {
        public static JobObjectHandle Create(JobObjectAccess access)
        {
            return Create(access, null);
        }

        public static JobObjectHandle Create(JobObjectAccess access, string name)
        {
            return Create(access, name, 0, null);
        }

        public static JobObjectHandle Create(JobObjectAccess access, string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateJobObject(
                    out handle,
                    access,
                    ref oa
                    )) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new JobObjectHandle(handle, true);
        }







        public static JobObjectHandle FromHandle(IntPtr handle)
        {
            return new JobObjectHandle(handle, false);
        }

        private JobObjectHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public JobObjectHandle(string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, JobObjectAccess access)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtOpenJobObject(
                    out handle,
                    access,
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

        public JobObjectHandle(string name, JobObjectAccess access)
            : this(name, 0, null, access)
        { }






        public JobObjectHandle(ProcessHandle processHandle, JobObjectAccess access)
        {
            try
            {
                this.Handle = new IntPtr(KProcessHacker.Instance.KphOpenProcessJob(processHandle, access));
            }
            catch (WindowsException)
            {

                this.Handle = new IntPtr(KProcessHacker.Instance.KphOpenProcessJob(processHandle,
                    (JobObjectAccess)StandardRights.Synchronize));
                if (this.Handle != IntPtr.Zero)
                    KProcessHacker.Instance.KphSetHandleGrantedAccess(this.Handle, (int)access);
            }


            if (this.Handle == IntPtr.Zero)
            {
                this.MarkAsInvalid();
                Win32.ThrowLastError(NtStatus.ProcessNotInJob);
            }
        }

        private T QueryStruct<T>(JobObjectInformationClass informationClass)
            where T : struct
        {
            int retLength;

            using (MemoryAlloc data = new MemoryAlloc(Marshal.SizeOf(typeof(T))))
            {
                if (!Win32.QueryInformationJobObject(this, informationClass, data, data.Size, out retLength))
                {
                    data.Resize(retLength);

                    if (!Win32.QueryInformationJobObject(this, informationClass, data, data.Size, out retLength))
                        Win32.ThrowLastError();
                }

                return data.ReadStruct<T>();
            }
        }

        public JobObjectBasicAccountingInformation GetBasicAccountingInformation()
        {
            return this.QueryStruct<JobObjectBasicAccountingInformation>(
                JobObjectInformationClass.JobObjectBasicAccountingInformation);
        }

        public JobObjectBasicAndIoAccountingInformation GetBasicAndIoAccountingInformation()
        {
            return this.QueryStruct<JobObjectBasicAndIoAccountingInformation>(
                JobObjectInformationClass.JobObjectBasicAndIoAccountingInformation);
        }

        public JobObjectBasicLimitInformation GetBasicLimitInformation()
        {
            return this.QueryStruct<JobObjectBasicLimitInformation>(JobObjectInformationClass.JobObjectBasicLimitInformation);
        }

        public int[] GetProcessIdList()
        {
            List<int> processIds = new List<int>();
            int retLength;


            using (MemoryAlloc data = new MemoryAlloc(0x1000))
            {
                if (!Win32.QueryInformationJobObject(this, JobObjectInformationClass.JobObjectBasicProcessIdList,
                    data, data.Size, out retLength))
                    Win32.ThrowLastError();

                JobObjectBasicProcessIdList listInfo = data.ReadStruct<JobObjectBasicProcessIdList>();

                for (int i = 0; i < listInfo.NumberOfProcessIdsInList; i++)
                {
                    processIds.Add(data.ReadInt32(8, i));
                }
            }

            return processIds.ToArray();
        }

        public JobObjectBasicUiRestrictions GetBasicUiRestrictions()
        {
            JobObjectBasicUiRestrictions uiRestrictions;
            int retLength;

            if (!Win32.QueryInformationJobObject(this, JobObjectInformationClass.JobObjectBasicUIRestrictions,
                out uiRestrictions, 4, out retLength))
                Win32.ThrowLastError();

            return uiRestrictions;
        }

        public JobObjectExtendedLimitInformation GetExtendedLimitInformation()
        {
            return this.QueryStruct<JobObjectExtendedLimitInformation>(JobObjectInformationClass.JobObjectExtendedLimitInformation);
        }

        public void Terminate()
        {
            this.Terminate(0);
        }

        public void Terminate(int exitCode)
        {
            if (!Win32.TerminateJobObject(this, exitCode))
                Win32.ThrowLastError();
        }
    }
}
