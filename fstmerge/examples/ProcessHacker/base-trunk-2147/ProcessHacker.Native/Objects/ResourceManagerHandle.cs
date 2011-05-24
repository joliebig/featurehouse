

using System;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{
    public class ResourceManagerHandle : NativeHandle<ResourceManagerAccess>
    {
        public static ResourceManagerHandle Create(
            ResourceManagerAccess access,
            string name,
            ObjectFlags objectFlags,
            DirectoryHandle rootDirectory,
            TmHandle tmHandle,
            Guid guid,
            ResourceManagerOptions createOptions,
            string description
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                UnicodeString descriptionStr = new UnicodeString(description);

                try
                {
                    if ((status = Win32.NtCreateResourceManager(
                        out handle,
                        access,
                        tmHandle,
                        ref guid,
                        ref oa,
                        createOptions,
                        ref descriptionStr
                        )) >= NtStatus.Error)
                        Win32.ThrowLastError(status);
                }
                finally
                {
                    descriptionStr.Dispose();
                }
            }
            finally
            {
                oa.Dispose();
            }

            return new ResourceManagerHandle(handle, true);
        }

        public static ResourceManagerHandle FromHandle(IntPtr handle)
        {
            return new ResourceManagerHandle(handle, false);
        }

        private ResourceManagerHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public ResourceManagerHandle(
            string name,
            ObjectFlags objectFlags,
            DirectoryHandle rootDirectory,
            TmHandle tmHandle,
            Guid guid,
            ResourceManagerAccess access
            )
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtOpenResourceManager(
                    out handle,
                    access,
                    tmHandle,
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

        private MemoryAlloc GetBasicInformation()
        {
            NtStatus status;
            int retLength;

            var data = new MemoryAlloc(0x1000);

            status = Win32.NtQueryInformationResourceManager(
                this,
                ResourceManagerInformationClass.ResourceManagerBasicInformation,
                data,
                data.Size,
                out retLength
                );

            if (status == NtStatus.BufferTooSmall)
            {

                data.Resize(retLength);

                status = Win32.NtQueryInformationResourceManager(
                    this,
                    ResourceManagerInformationClass.ResourceManagerBasicInformation,
                    data,
                    data.Size,
                    out retLength
                    );
            }

            if (status >= NtStatus.Error)
            {
                data.Dispose();
                Win32.ThrowLastError(status);
            }

            return data;
        }

        public string GetDescription()
        {
            using (var data = this.GetBasicInformation())
            {
                var basicInfo = data.ReadStruct<ResourceManagerBasicInformation>();

                return data.ReadUnicodeString(
                    ResourceManagerBasicInformation.DescriptionOffset,
                    basicInfo.DescriptionLength / 2
                    );
            }
        }

        public Guid GetGuid()
        {
            using (var data = this.GetBasicInformation())
            {
                return data.ReadStruct<ResourceManagerBasicInformation>().ResourceManagerId;
            }
        }

        public void Recover()
        {
            NtStatus status;

            if ((status = Win32.NtRecoverResourceManager(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }
    }
}
