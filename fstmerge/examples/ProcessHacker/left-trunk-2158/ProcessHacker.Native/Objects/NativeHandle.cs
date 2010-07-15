

using System;
using ProcessHacker.Common.Objects;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;
using ProcessHacker.Native.Security.AccessControl;

namespace ProcessHacker.Native.Objects
{



    public class NativeHandle : BaseObject, IEquatable<NativeHandle>, ISecurable, ISynchronizable
    {
        public static IntPtr Invalid
        {
            get { return IntPtr.Zero; }
        }

        public static IntPtr MinusOne
        {
            get { return (-1).ToIntPtr(); }
        }

        public static bool IsInvalid(IntPtr handle)
        {
            return handle == Invalid;
        }

        public static NtStatus WaitAll(ISynchronizable[] objects)
        {
            return WaitAll(objects, false, long.MinValue, false);
        }

        public static NtStatus WaitAll(ISynchronizable[] objects, long timeout)
        {
            return WaitAll(objects, false, timeout);
        }

        public static NtStatus WaitAll(ISynchronizable[] objects, bool alertable, long timeout)
        {
            return WaitAll(objects, alertable, timeout, true);
        }

        public static NtStatus WaitAll(ISynchronizable[] objects, bool alertable, long timeout, bool relative)
        {
            return WaitForMultipleObjects(objects, WaitType.WaitAll, alertable, timeout, relative);
        }

        public static NtStatus WaitAny(ISynchronizable[] objects)
        {
            return WaitAny(objects, false, long.MinValue, false);
        }

        public static NtStatus WaitAny(ISynchronizable[] objects, long timeout)
        {
            return WaitAny(objects, false, timeout);
        }

        public static NtStatus WaitAny(ISynchronizable[] objects, bool alertable, long timeout)
        {
            return WaitAny(objects, alertable, timeout, true);
        }

        public static NtStatus WaitAny(ISynchronizable[] objects, bool alertable, long timeout, bool relative)
        {
            return WaitForMultipleObjects(objects, WaitType.WaitAny, alertable, timeout, relative);
        }

        private static NtStatus WaitForMultipleObjects(ISynchronizable[] objects, WaitType waitType, bool alertable, long timeout, bool relative)
        {
            NtStatus status;
            IntPtr[] handles = new IntPtr[objects.Length];
            long realTimeout = relative ? -timeout : timeout;

            for (int i = 0; i < objects.Length; i++)
                handles[i] = objects[i].Handle;

            if ((status = Win32.NtWaitForMultipleObjects(
                handles.Length,
                handles,
                waitType,
                alertable,
                ref realTimeout
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return status;
        }

        public static implicit operator int(NativeHandle handle)
        {
            return handle.Handle.ToInt32();
        }

        public static implicit operator IntPtr(NativeHandle handle)
        {
            return handle.Handle;
        }

        private IntPtr _handle;




        protected NativeHandle()
        { }






        public NativeHandle(IntPtr handle)
        {
            _handle = handle;
        }







        public NativeHandle(IntPtr handle, bool owned)
            : base(owned)
        {
            _handle = handle;
        }

        protected sealed override void DisposeObject(bool disposing)
        {
            this.Close();
        }






        protected virtual void Close()
        {
            if (_handle != IntPtr.Zero && _handle.ToInt32() != -1 && _handle.ToInt32() != -2)
                Win32.NtClose(_handle);
        }




        public IntPtr Handle
        {
            get { return _handle; }
            protected set { _handle = value; }
        }






        public override bool Equals(object obj)
        {
            return this.Equals(obj as NativeHandle);
        }






        public bool Equals(NativeHandle obj)
        {
            if (obj == null)
                return false;
            return obj.Handle == this.Handle;
        }





        public virtual Win32HandleFlags GetHandleFlags()
        {
            Win32HandleFlags flags;

            if (!Win32.GetHandleInformation(this, out flags))
                Win32.ThrowLastError();

            return flags;
        }





        public override int GetHashCode()
        {
            return _handle.ToInt32();
        }





        public virtual string GetObjectName()
        {
            NtStatus status;
            int retLength;

            status = Win32.NtQueryObject(this, ObjectInformationClass.ObjectNameInformation,
                  IntPtr.Zero, 0, out retLength);

            if (retLength > 0)
            {
                using (MemoryAlloc oniMem = new MemoryAlloc(retLength))
                {
                    if ((status = Win32.NtQueryObject(this, ObjectInformationClass.ObjectNameInformation,
                        oniMem, oniMem.Size, out retLength)) >= NtStatus.Error)
                        Win32.ThrowLastError(status);

                    var oni = oniMem.ReadStruct<ObjectNameInformation>();

                    return oni.Name.Read();
                }
            }
            else
            {
                Win32.ThrowLastError(status);
            }

            return null;
        }





        public virtual string GetObjectTypeName()
        {
            NtStatus status;
            int retLength;

            status = Win32.NtQueryObject(this, ObjectInformationClass.ObjectTypeInformation,
                  IntPtr.Zero, 0, out retLength);

            if (retLength > 0)
            {
                using (MemoryAlloc otiMem = new MemoryAlloc(retLength))
                {
                    if ((status = Win32.NtQueryObject(this, ObjectInformationClass.ObjectTypeInformation,
                        otiMem, otiMem.Size, out retLength)) >= NtStatus.Error)
                        Win32.ThrowLastError(status);

                    var oni = otiMem.ReadStruct<ObjectTypeInformation>();

                    return oni.Name.Read();
                }
            }
            else
            {
                Win32.ThrowLastError(status);
            }

            return null;
        }






        public virtual SecurityDescriptor GetSecurity(SecurityInformation securityInformation)
        {
            return SecurityDescriptor.GetSecurity(this, securityInformation);
        }







        protected SecurityDescriptor GetSecurity(SeObjectType objectType, SecurityInformation securityInformation)
        {
            return SecurityDescriptor.GetSecurity(this, objectType, securityInformation);
        }




        public virtual void MakeObjectPermanent()
        {
            NtStatus status;

            if ((status = Win32.NtMakePermanentObject(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }






        public virtual void MakeObjectTemporary()
        {
            NtStatus status;

            if ((status = Win32.NtMakeTemporaryObject(this)) >= NtStatus.Error)
                Win32.ThrowLastError(status);
        }





        protected void MarkAsInvalid()
        {
            this.DisableOwnership(false);
        }






        public virtual void SetHandleFlags(Win32HandleFlags mask, Win32HandleFlags flags)
        {
            if (!Win32.SetHandleInformation(this, mask, flags))
                Win32.ThrowLastError();
        }






        public virtual void SetSecurity(SecurityInformation securityInformation, SecurityDescriptor securityDescriptor)
        {
            SecurityDescriptor.SetSecurity(this, securityInformation, securityDescriptor);
        }







        protected void SetSecurity(SeObjectType objectType, SecurityInformation securityInformation, SecurityDescriptor securityDescriptor)
        {
            SecurityDescriptor.SetSecurity(this, objectType, securityInformation, securityDescriptor);
        }




        public virtual NtStatus SignalAndWait(ISynchronizable waitObject)
        {
            return this.SignalAndWait(waitObject, false);
        }




        public virtual NtStatus SignalAndWait(ISynchronizable waitObject, bool alertable)
        {
            return this.SignalAndWait(waitObject, alertable, long.MinValue, false);
        }




        public virtual NtStatus SignalAndWait(ISynchronizable waitObject, bool alertable, long timeout)
        {
            return this.SignalAndWait(waitObject, alertable, timeout, true);
        }




        public virtual NtStatus SignalAndWait(ISynchronizable waitObject, bool alertable, long timeout, bool relative)
        {
            NtStatus status;
            long realTimeout = relative ? -timeout : timeout;

            if ((status = Win32.NtSignalAndWaitForSingleObject(
                this,
                waitObject.Handle,
                alertable,
                ref timeout
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);

            return status;
        }





        protected void SwapHandle(IntPtr newHandle)
        {
            if (!this.Owned || this.Disposed)
                throw new InvalidOperationException();

            this.Close();
            _handle = newHandle;
        }





        public override string ToString()
        {
            return this.GetType().Name + ": " + _handle.ToString("x");
        }




        public virtual NtStatus Wait()
        {
            return this.Wait(false);
        }







        public virtual NtStatus Wait(bool alertable)
        {

            return this.Wait(alertable, long.MinValue, false);
        }





        public NtStatus Wait(long timeout)
        {
            return this.Wait(false, timeout);
        }
        public virtual NtStatus Wait(bool alertable, long timeout)
        {
            return this.Wait(alertable, timeout, true);
        }
        public NtStatus Wait(long timeout, bool relative)
        {
            return this.Wait(false, timeout, relative);
        }
        public virtual NtStatus Wait(bool alertable, long timeout, bool relative)
        {
            NtStatus status;
            long realTimeout = relative ? -timeout : timeout;
            if ((status = Win32.NtWaitForSingleObject(
                this,
                alertable,
                ref realTimeout
                )) >= NtStatus.Error)
                Win32.ThrowLastError(status);
            return status;
        }
}
    public class NativeHandle<TAccess> : NativeHandle
        where TAccess : struct
    {
        protected NativeHandle()
        { }
        public NativeHandle(IntPtr handle)
            : base(handle)
        { }
        public NativeHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }
        public NativeHandle(IntPtr handle, TAccess access)
        {
            IntPtr newHandle;
            Win32.DuplicateObject(ProcessHandle.Current, handle, ProcessHandle.Current, out newHandle,
                (int)Convert.ChangeType(access, typeof(int)), 0, 0);
            this.Handle = newHandle;
        }
        public NativeHandle(ProcessHandle processHandle, IntPtr handle, TAccess access)
        {
            IntPtr newHandle;
            Win32.DuplicateObject(processHandle, handle, ProcessHandle.Current, out newHandle,
                (int)Convert.ChangeType(access, typeof(int)), 0, 0);
            this.Handle = newHandle;
        }
        public void ChangeAccess(TAccess access)
        {
            IntPtr newHandle;
            Win32.DuplicateObject(ProcessHandle.Current, this, ProcessHandle.Current, out newHandle,
                (int)Convert.ChangeType(access, typeof(int)), 0, 0);
            this.SwapHandle(newHandle);
        }
        public NativeHandle<TAccess> Duplicate(TAccess access)
        {
            return new NativeHandle<TAccess>(ProcessHandle.Current, this, access);
        }
    }
    public class GenericHandle : NativeHandle<int>
    {
        protected GenericHandle()
            : base()
        { }
        public GenericHandle(IntPtr handle)
            : base(handle)
        { }
        public GenericHandle(IntPtr handle, bool owned)
            : base(handle, owned)
        { }
        public GenericHandle(IntPtr handle, int desiredAccess)
            : base(handle, desiredAccess)
        { }
        public GenericHandle(ProcessHandle processHandle, IntPtr handle, int desiredAccess)
            : base(processHandle, handle, desiredAccess)
        { }
    }
}
