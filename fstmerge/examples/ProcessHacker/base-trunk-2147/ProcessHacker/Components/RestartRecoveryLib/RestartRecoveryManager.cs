

using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Diagnostics;
using ProcessHacker.Native;
using ProcessHacker.Native.Api;

namespace ProcessHackerRestartRecovery
{





    public static class ApplicationRestartRecoveryManager
    {
        public static void RegisterForRestart()
        {

            ApplicationRestartRecoveryManager.RegisterForApplicationRestart(
                new RestartSettings("-recovered",
                    RestartRestrictions.NotOnReboot
                    | RestartRestrictions.NotOnPatch));
        }

        public static void RegisterForRecovery()
        {




            RecoveryData data = new RecoveryData(new RecoveryCallback(RecoveryProcedure), null);
            RecoverySettings settings = new RecoverySettings(data, 0);
            ApplicationRestartRecoveryManager.RegisterForApplicationRecovery(settings);
        }






        private static int RecoveryProcedure(object state)
        {
            PingSystem();





            try
            {

                ProcessHacker.Program.HackerWindow.ExecuteOnIcons((icon) => icon.Visible = false);
                ProcessHacker.Program.HackerWindow.ExecuteOnIcons((icon) => icon.Dispose());


                if (ProcessHacker.Native.KProcessHacker.Instance != null)
                    ProcessHacker.Native.KProcessHacker.Instance.Close();
            }
            catch { }




            ApplicationRecoveryFinished(true);

            return 0;
        }




        private static void PingSystem()
        {

            bool isCanceled = ApplicationRecoveryInProgress();

            if (isCanceled)
            {
                System.Windows.Forms.MessageBox.Show("Recovery has been canceled by user.");
                Environment.Exit(1);
            }
        }




        public static void RecoverLastSession()
        {


        }
        private static void RegisterForApplicationRecovery(RecoverySettings settings)
        {
            if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
            {
                if (settings == null)
                    throw new ArgumentNullException("settings");
                GCHandle handle = GCHandle.Alloc(settings.RecoveryData);
                HResult hr = AppRestartRecoveryNativeMethods.RegisterApplicationRecoveryCallback(AppRestartRecoveryNativeMethods.internalCallback, (IntPtr)handle, settings.PingInterval, (uint)0);
                if (hr == HResult.InvalidArgument)
                    throw new ArgumentException("Application was not registered for recovery due to bad parameters.");
                else if (hr == HResult.Fail)
                    throw new ExternalException("Application failed to register for recovery.");
            }
        }
        private static void UnregisterApplicationRecovery()
        {
            if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
            {
                HResult hr = AppRestartRecoveryNativeMethods.UnregisterApplicationRecoveryCallback();
                if (hr == HResult.Fail)
                    throw new ExternalException("Unregister for recovery failed.");
            }
        }
        private static void UnregisterApplicationRestart()
        {
            if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
            {
                HResult hr = AppRestartRecoveryNativeMethods.UnregisterApplicationRestart();
                if (hr == HResult.Fail)
                    throw new ExternalException("Unregister for restart failed.");
            }
        }
        private static bool ApplicationRecoveryInProgress()
        {
            if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
            {
                bool canceled = false;
                HResult hr = AppRestartRecoveryNativeMethods.ApplicationRecoveryInProgress(out canceled);
                if (hr == HResult.Fail)
                    throw new InvalidOperationException("This method must be called from the registered callback method.");
                return canceled;
            }
            else
                return true;
        }
        private static void ApplicationRecoveryFinished(bool success)
        {
            if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
            {
                AppRestartRecoveryNativeMethods.ApplicationRecoveryFinished(success);
            }
        }
        private static void RegisterForApplicationRestart(RestartSettings settings)
        {
            if (OSVersion.IsAboveOrEqual(WindowsVersion.Vista))
            {
                HResult hr = AppRestartRecoveryNativeMethods.RegisterApplicationRestart(settings.Command, settings.Restrictions);
                if (hr == HResult.Fail)
                    throw new InvalidOperationException("Application failed to registered for restart.");
                else if (hr == HResult.InvalidArgument)
                    throw new ArgumentException("Failed to register application for restart due to bad parameters.");
            }
        }
    }
}
