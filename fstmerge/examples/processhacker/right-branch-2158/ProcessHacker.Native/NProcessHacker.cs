

using System;
using System.Runtime.InteropServices;
using System.Security;
using ProcessHacker.Native.Api;

namespace ProcessHacker.Native
{
    [SuppressUnmanagedCodeSecurity]
    public static class NProcessHacker
    {
        public enum WsInformationClass
        {
            WsCount = 0,
            WsPrivateCount,
            WsSharedCount,
            WsShareableCount,
            WsAllCounts
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct WsAllCounts
        {
            public int Count;
            public int PrivateCount;
            public int SharedCount;
            public int ShareableCount;
        }

        [DllImport("nprocesshacker.dll")]
        public static extern void KphHookDeinit();

        [DllImport("nprocesshacker.dll")]
        public static extern void KphHookInit();

        [DllImport("nprocesshacker.dll", SetLastError = true)]
        public static extern NtStatus PhQueryProcessWs(
            [In] IntPtr ProcessHandle,
            [In] WsInformationClass WsInformationClass,
            [Out] out int WsInformation,
            [In] int WsInformationLength,
            [Out] out int ReturnLength
            );

        [DllImport("nprocesshacker.dll", SetLastError = true)]
        public static extern NtStatus PhQueryProcessWs(
            [In] IntPtr ProcessHandle,
            [In] WsInformationClass WsInformationClass,
            [Out] out WsAllCounts WsInformation,
            [In] int WsInformationLength,
            [Out] out int ReturnLength
            );

        [DllImport("nprocesshacker.dll", SetLastError = true)]
        public static extern NtStatus PhQueryNameFileObject(
            [In] IntPtr FileHandle,
            [In] IntPtr FileObjectNameInformation,
            [In] int FileObjectNameInformationLength,
            [Out] [Optional] out int ReturnLength
            );

        [DllImport("nprocesshacker.dll")]
        public static extern void PhVoid();

        [DllImport("nprocesshacker.dll", CharSet = CharSet.Unicode)]
        public static extern VerifyResult PhVerifyFile(string FileName);
    }
}
