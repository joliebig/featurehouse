

using System;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Objects
{



    public sealed class ServiceManagerHandle : ServiceBaseHandle<ScManagerAccess>
    {




        public ServiceManagerHandle(ScManagerAccess access)
        {
            this.Handle = Win32.OpenSCManager(null, null, access);

            if (this.Handle == IntPtr.Zero)
            {
                this.MarkAsInvalid();
                Win32.ThrowLastError();
            }
        }

        public ServiceHandle CreateService(string name, string displayName,
            ServiceType type, string binaryPath)
        {
            return this.CreateService(name, displayName, type, ServiceStartType.DemandStart,
                ServiceErrorControl.Ignore, binaryPath, null, null, null);
        }

        public ServiceHandle CreateService(string name, string displayName,
            ServiceType type, ServiceStartType startType, string binaryPath)
        {
            return this.CreateService(name, displayName, type, startType,
                ServiceErrorControl.Ignore, binaryPath, null, null, null);
        }

        public ServiceHandle CreateService(string name, string displayName,
            ServiceType type, ServiceStartType startType, ServiceErrorControl errorControl,
            string binaryPath, string group, string accountName, string password)
        {
            IntPtr service;

            if ((service = Win32.CreateService(this, name, displayName, ServiceAccess.All,
                type, startType, errorControl, binaryPath, group,
                IntPtr.Zero, IntPtr.Zero, accountName, password)) == IntPtr.Zero)
                Win32.ThrowLastError();

            return new ServiceHandle(service, true);
        }
    }
}
