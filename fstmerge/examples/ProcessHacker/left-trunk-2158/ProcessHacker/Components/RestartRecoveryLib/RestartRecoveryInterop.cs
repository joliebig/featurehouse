

using System.Runtime.InteropServices;
using ProcessHacker.Native.Api;
using System.Security;
using System;

namespace ProcessHackerRestartRecovery
{
    [SuppressUnmanagedCodeSecurity]
    internal static class AppRestartRecoveryNativeMethods
    {


        internal delegate UInt32 InternalRecoveryCallback(IntPtr state);

        internal static InternalRecoveryCallback internalCallback;

        static AppRestartRecoveryNativeMethods()
        {
            internalCallback = new InternalRecoveryCallback(InternalRecoveryHandler);
        }

        private static UInt32 InternalRecoveryHandler(IntPtr parameter)
        {
            bool cancelled = false;
            ApplicationRecoveryInProgress(out cancelled);

            GCHandle handle = GCHandle.FromIntPtr(parameter);
            RecoveryData data = handle.Target as RecoveryData;
            data.Invoke();
            handle.Free();

            return 0;
        }

        [DllImport("kernel32.dll")]
        internal static extern void ApplicationRecoveryFinished(
           [MarshalAs(UnmanagedType.Bool)]
            bool success
            );

        [DllImport("kernel32.dll")]
        internal static extern HResult ApplicationRecoveryInProgress(
            [Out, MarshalAs(UnmanagedType.Bool)]
            out bool canceled
            );

        [DllImport("kernel32.dll")]
        internal static extern HResult GetApplicationRecoveryCallback(
            IntPtr processHandle,
            [Out] RecoveryCallback recoveryCallback,
            [Out] out object state,
            [Out] out uint pingInterval,
            [Out] out uint flags
            );

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
        internal static extern HResult RegisterApplicationRecoveryCallback(
            InternalRecoveryCallback callback, IntPtr param,
            uint pingInterval,
            uint flags
            );


        [DllImport("kernel32.dll")]
        internal static extern HResult RegisterApplicationRestart(
            [MarshalAs(UnmanagedType.LPWStr)]
            string commandLineArgs,
            RestartRestrictions flags
            );

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        internal static extern HResult GetApplicationRestartSettings(
            IntPtr process,
            IntPtr commandLine,
            ref uint size,
            [Out] out RestartRestrictions flags
            );

        [DllImport("kernel32.dll")]
        internal static extern HResult UnregisterApplicationRecoveryCallback();

        [DllImport("kernel32.dll")]
        internal static extern HResult UnregisterApplicationRestart();


    }






    [Flags]
    public enum RestartRestrictions
    {



        None = 0,



        NotOnCrash = 1,



        NotOnHang = 2,




        NotOnPatch = 4,




        NotOnReboot = 8
    }
}
