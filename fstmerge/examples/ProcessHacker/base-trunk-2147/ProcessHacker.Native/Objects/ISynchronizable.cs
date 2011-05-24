

using System;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native.Objects
{



    public interface ISynchronizable
    {
        IntPtr Handle { get; }




        NtStatus Wait();
        NtStatus Wait(bool alertable);
        NtStatus Wait(bool alertable, long timeout);
    }
}
