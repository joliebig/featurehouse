

using System;
using System.Collections.Generic;
using ProcessHacker.Common.Objects;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{



    public class PrivateNamespaceHandle : DirectoryHandle
    {
        public static PrivateNamespaceHandle Create(BoundaryDescriptor boundaryDescriptor, string aliasPrefix)
        {
            IntPtr handle = IntPtr.Zero;

            handle = Win32.CreatePrivateNamespace(IntPtr.Zero, boundaryDescriptor.Descriptor, aliasPrefix);

            if (handle == IntPtr.Zero)
                Win32.ThrowLastError();

            return new PrivateNamespaceHandle(handle, true);
        }

        private bool _destroy = false;

        private PrivateNamespaceHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }

        public PrivateNamespaceHandle(BoundaryDescriptor boundaryDescriptor, string aliasPrefix)
        {
            this.Handle = Win32.OpenPrivateNamespace(boundaryDescriptor.Descriptor, aliasPrefix);

            if (this.Handle == IntPtr.Zero)
            {
                this.MarkAsInvalid();
                Win32.ThrowLastError();
            }
        }

        protected override void Close()
        {
            Win32.ClosePrivateNamespace(this, _destroy ? PrivateNamespaceFlags.Destroy : 0);
        }

        public void MarkForDestruction()
        {
            _destroy = true;
        }
    }

    public class BoundaryDescriptor : BaseObject
    {
        private IntPtr _descriptor;

        public BoundaryDescriptor(string name)
            : this(name, null)
        { }

        public BoundaryDescriptor(string name, IEnumerable<Sid> sids)
        {
            _descriptor = Win32.CreateBoundaryDescriptor(name, 0);

            if (_descriptor == IntPtr.Zero)
            {
                this.DisableOwnership(false);
                Win32.ThrowLastError();
            }

            if (sids != null)
            {
                foreach (Sid sid in sids)
                    this.Add(sid);
            }
        }

        protected override void DisposeObject(bool disposing)
        {
            Win32.DeleteBoundaryDescriptor(_descriptor);
        }

        public IntPtr Descriptor
        {
            get { return _descriptor; }
        }

        public void Add(Sid sid)
        {
            if (!Win32.AddSIDToBoundaryDescriptor(ref _descriptor, sid))
                Win32.ThrowLastError();
        }
    }
}
