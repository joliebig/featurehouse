

using System;
using System.Collections.Generic;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{



    public class DirectoryHandle : NativeHandle<DirectoryAccess>
    {
        public delegate bool EnumObjectsDelegate(ObjectEntry obj);

        public struct ObjectEntry
        {
            private string _name;
            private string _typeName;

            public ObjectEntry(string name, string typeName)
            {
                _name = name;
                _typeName = typeName;
            }

            public string Name { get { return _name; } }
            public string TypeName { get { return _typeName; } }
        }

        public static DirectoryHandle Create(DirectoryAccess access, string name)
        {
            return Create(access, name, 0, null);
        }

        public static DirectoryHandle Create(DirectoryAccess access, string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtCreateDirectoryObject(out handle, access, ref oa)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            return new DirectoryHandle(handle, true);
        }

        protected DirectoryHandle()
        { }

        protected DirectoryHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public DirectoryHandle(string name, ObjectFlags objectFlags, DirectoryHandle rootDirectory, DirectoryAccess access)
        {
            NtStatus status;
            ObjectAttributes oa = new ObjectAttributes(name, objectFlags, rootDirectory);
            IntPtr handle;

            try
            {
                if ((status = Win32.NtOpenDirectoryObject(out handle, access, ref oa)) >= NtStatus.Error)
                    Win32.ThrowLastError(status);
            }
            finally
            {
                oa.Dispose();
            }

            this.Handle = handle;
        }

        public DirectoryHandle(string name, DirectoryAccess access)
            : this(name, 0, null, access)
        { }

        public void EnumObjects(EnumObjectsDelegate callback)
        {
            NtStatus status;
            int context = 0;
            bool firstTime = true;
            int retLength;

            using (var data = new MemoryAlloc(0x200))
            {
                while (true)
                {
                    while ((status = Win32.NtQueryDirectoryObject(
                        this,
                        data,
                        data.Size,
                        false,
                        firstTime,
                        ref context,
                        out retLength
                        )) == NtStatus.MoreEntries)
                    {


                        if (data.ReadStruct<ObjectDirectoryInformation>(0).Name.Buffer != IntPtr.Zero)
                            break;

                        if (data.Size > 16 * 1024 * 1024)
                            Win32.ThrowLastError(status);

                        data.Resize(data.Size * 2);
                    }

                    if (status >= NtStatus.Error)
                        Win32.ThrowLastError(status);

                    int i = 0;

                    while (true)
                    {
                        ObjectDirectoryInformation info = data.ReadStruct<ObjectDirectoryInformation>(i);

                        if (info.Name.Buffer == IntPtr.Zero)
                            break;

                        if (!callback(new ObjectEntry(info.Name.Read(), info.TypeName.Read())))
                            return;

                        i++;
                    }

                    if (status != NtStatus.MoreEntries)
                        break;

                    firstTime = false;
                }
            }
        }





        public ObjectEntry[] GetObjects()
        {
            var objects = new List<ObjectEntry>();

            this.EnumObjects((obj) =>
                {
                    objects.Add(obj);
                    return true;
                });

            return objects.ToArray();
        }
    }
}
