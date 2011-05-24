

using System;
using System.Runtime.InteropServices;

namespace ProcessHacker.Native.Api
{


    public enum HResult : uint
    {
        False = 0x0001,
        OK = 0x0000,
        Cancelled = 1223,

        Error = 0x80000000,
        NoInterface = 0x80004002,
        Fail = 0x80004005,
        TypeElementNotFound = 0x8002802b,
        NoObject = 0x800401e5,
        OutOfMemory = 0x8007000e,
        InvalidArgument = 0x80070057,
        ResourceInUse = 0x800700aa,
        ElementNotFound = 0x80070490
    }

    public static class HResultExtensions
    {
        public static bool IsError(this HResult result)
        {

            return result != HResult.OK;
        }

        public static void ThrowIf(this HResult result)
        {
            if (result.IsError())
                throw Marshal.GetExceptionForHR((int)result);
        }
    }
}
