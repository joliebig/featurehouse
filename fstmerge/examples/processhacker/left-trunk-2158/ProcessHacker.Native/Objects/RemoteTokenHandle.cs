

using ProcessHacker.Native.Api;
using ProcessHacker.Native.Security;
using System;

namespace ProcessHacker.Native.Objects
{
    public sealed class RemoteTokenHandle : RemoteHandle, IWithToken
    {
        public RemoteTokenHandle(ProcessHandle phandle, IntPtr handle)
            : base(phandle, handle)
        { }
        public new IntPtr GetHandle(int rights)
        {
            IntPtr newHandle = IntPtr.Zero;
            Win32.DuplicateObject(this.ProcessHandle, this.Handle, new IntPtr(-1), out newHandle, rights, 0, 0);
            return newHandle;
        }
        public TokenHandle GetToken()
        {
            return GetToken(TokenAccess.All);
        }
        public TokenHandle GetToken(TokenAccess access)
        {
            return new TokenHandle(this.GetHandle((int)access), true);
        }
    }
}
