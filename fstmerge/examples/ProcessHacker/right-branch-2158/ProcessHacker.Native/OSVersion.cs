

using System;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native
{
    public enum OSArch
    {
        I386,
        Amd64
    }

    public enum WindowsVersion
    {



        XP = 51,




        Server2003 = 52,




        Vista = 60,




        Seven = 61,




        Unreleased = int.MaxValue
    }

    public static class OSVersion
    {
        private static int _bits = IntPtr.Size * 8;
        private static OSArch _arch = IntPtr.Size == 4 ? OSArch.I386 : OSArch.Amd64;
        private static WindowsVersion _windowsVersion;

        private static ProcessAccess _minProcessQueryInfoAccess = ProcessAccess.QueryInformation;
        private static ThreadAccess _minThreadQueryInfoAccess = ThreadAccess.QueryInformation;
        private static ThreadAccess _minThreadSetInfoAccess = ThreadAccess.SetInformation;

        private static bool _hasCycleTime = false;
        private static bool _hasExtendedTaskbar = false;
        private static bool _hasProtectedProcesses = false;
        private static bool _hasPsSuspendResumeProcess = false;
        private static bool _hasQueryLimitedInformation = false;
        private static bool _hasSetAccessToken = false;
        private static bool _hasTaskDialogs = false;
        private static bool _hasUac = false;
        private static bool _hasWin32ImageFileName = false;

        static OSVersion()
        {
            System.Version version = Environment.OSVersion.Version;

            if (version.Major == 5 && version.Minor == 1)
                _windowsVersion = WindowsVersion.XP;
            else if (version.Major == 5 && version.Minor == 2)
                _windowsVersion = WindowsVersion.Server2003;
            else if (version.Major == 6 && version.Minor == 0)
                _windowsVersion = WindowsVersion.Vista;
            else if (version.Major == 6 && version.Minor == 1)
                _windowsVersion = WindowsVersion.Seven;
            else if ((version.Major == 6 && version.Minor > 1) || version.Major > 6)
                _windowsVersion = WindowsVersion.Unreleased;

            if (IsBelow(WindowsVersion.Vista))
            {
                _hasSetAccessToken = true;
            }

            if (IsAboveOrEqual(WindowsVersion.Vista))
            {
                _minProcessQueryInfoAccess = ProcessAccess.QueryLimitedInformation;
                _minThreadQueryInfoAccess = ThreadAccess.QueryLimitedInformation;
                _minThreadSetInfoAccess = ThreadAccess.SetLimitedInformation;

                _hasCycleTime = true;
                _hasProtectedProcesses = true;
                _hasPsSuspendResumeProcess = true;
                _hasQueryLimitedInformation = true;
                _hasTaskDialogs = true;
                _hasUac = true;
                _hasWin32ImageFileName = true;
            }

            if (IsAboveOrEqual(WindowsVersion.Seven))
            {
                _hasExtendedTaskbar = true;
            }
        }

        public static int Bits
        {
            get { return _bits; }
        }

        public static string BitsString
        {
            get { return _bits.ToString() + "-" + "bit"; }
        }

        public static OSArch Architecture
        {
            get { return _arch; }
        }

        public static WindowsVersion WindowsVersion
        {
            get { return _windowsVersion; }
        }

        public static ProcessAccess MinProcessQueryInfoAccess
        {
            get { return _minProcessQueryInfoAccess; }
        }

        public static ThreadAccess MinThreadQueryInfoAccess
        {
            get { return _minThreadQueryInfoAccess; }
        }

        public static ThreadAccess MinThreadSetInfoAccess
        {
            get { return _minThreadSetInfoAccess; }
        }

        public static bool HasCycleTime
        {
            get { return _hasCycleTime; }
        }

        public static bool HasExtendedTaskbar
        {
            get { return _hasExtendedTaskbar; }
        }

        public static bool HasProtectedProcesses
        {
            get { return _hasProtectedProcesses; }
        }

        public static bool HasPsSuspendResumeProcess
        {
            get { return _hasPsSuspendResumeProcess; }
        }

        public static bool HasQueryLimitedInformation
        {
            get { return _hasQueryLimitedInformation; }
        }

        public static bool HasSetAccessToken
        {
            get { return _hasSetAccessToken; }
        }

        public static bool HasTaskDialogs
        {
            get { return _hasTaskDialogs; }
        }

        public static bool HasUac
        {
            get { return _hasUac; }
        }

        public static bool HasWin32ImageFileName
        {
            get { return _hasWin32ImageFileName; }
        }

        public static bool IsAmd64()
        {
            return _arch == OSArch.Amd64;
        }

        public static bool IsI386()
        {
            return _arch == OSArch.I386;
        }

        public static bool IsAbove(WindowsVersion version)
        {
            return (int)_windowsVersion > (int)version;
        }

        public static bool IsAboveOrEqual(WindowsVersion version)
        {
            return (int)_windowsVersion >= (int)version;
        }

        public static bool IsBelowOrEqual(WindowsVersion version)
        {
            return (int)_windowsVersion <= (int)version;
        }

        public static bool IsBelow(WindowsVersion version)
        {
            return (int)_windowsVersion < (int)version;
        }
    }
}
