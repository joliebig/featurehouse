// ****************************************************************************
// 
// Copyright (C) 2005-2009  Doom9 & al
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// 
// ****************************************************************************

using System;
using System.Collections;
using System.Diagnostics;
using System.Management;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace MeGUI
{
    /// <summary>
    /// OSInfo Class based from http://www.codeproject.com/csharp/osversion_producttype.asp
    /// </summary>
    public class OSInfo
    {
        [StructLayout(LayoutKind.Sequential)]
        private struct OSVERSIONINFOEX
        {
            public int dwOSVersionInfoSize;
            public int dwMajorVersion;
            public int dwMinorVersion;
            public int dwBuildNumber;
            public int dwPlatformId;
            [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 128)]
            public string szCSDVersion;
            public short wServicePackMajor;
            public short wServicePackMinor;
            public short wSuiteMask;
            public byte wProductType;
            public byte wReserved;
        }

        [DllImport("kernel32.dll")]
        private static extern bool GetVersionEx(ref OSVERSIONINFOEX osVersionInfo);

        [DllImport("kernel32.dll")]
        private static extern bool GetProductInfo(int dwOSMajorVersion, int dwOSMinorVersion, int dwSpMajorVersion, int dwSpMinorVersion, out uint pdwReturnedProductType);

        [DllImport("kernel32.dll", SetLastError = true, CallingConvention = CallingConvention.Winapi)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool IsWow64Process([In] IntPtr hProcess, [Out] out bool lpSystemInfo);

        
        private const int VER_NT_WORKSTATION = 1;
        private const int VER_NT_DOMAIN_CONTROLLER = 2;
        private const int VER_NT_SERVER = 3;
        private const int VER_SUITE_SMALLBUSINESS = 1;
        private const int VER_SUITE_ENTERPRISE = 2;
        private const int VER_SUITE_TERMINAL = 16;
        private const int VER_SUITE_DATACENTER = 128;
        private const int VER_SUITE_SINGLEUSERTS = 256;
        private const int VER_SUITE_PERSONAL = 512;
        private const int VER_SUITE_BLADE = 1024;
        private const int PRODUCT_UNDEFINED = 0x00000000;
        private const int PRODUCT_ULTIMATE = 0x00000001;
        private const int PRODUCT_HOME_BASIC = 0x00000002;
        private const int PRODUCT_HOME_PREMIUM = 0x00000003;
        private const int PRODUCT_ENTERPRISE = 0x00000004;
        private const int PRODUCT_HOME_BASIC_N = 0x00000005;
        private const int PRODUCT_BUSINESS = 0x00000006;
        private const int PRODUCT_BUSINESS_N = 0x00000010;
        private const int PRODUCT_STARTER = 0x0000000B;
        

        
        /// <summary>
        /// Determines whether the specified process is running under WOW64. 
        /// </summary>
        /// <returns>a boolean</returns>
        public static bool isWow64()
        {
            if (Environment.OSVersion.Version.Major == 5 && Environment.OSVersion.Version.Minor == 0)
                return false;   // windows 2000

            Process p = Process.GetCurrentProcess();
            IntPtr handle = p.Handle;
            bool isWow64;
            bool success = IsWow64Process(handle, out isWow64);
            if ((!success) && (IntPtr.Size != 8))
                throw new Exception();
            else
                return isWow64;
        }

        /// <summary>
        /// Returns the service pack information of the operating system running on this computer.
        /// </summary>
        /// <returns>A string containing the operating system service pack information.</returns>
        public static string GetOSServicePack()
        {
            OSVERSIONINFOEX osVersionInfo = new OSVERSIONINFOEX();

            osVersionInfo.dwOSVersionInfoSize = Marshal.SizeOf(typeof(OSVERSIONINFOEX));

            if (!GetVersionEx(ref osVersionInfo))
            {
                return "";
            }
            else
            {
                if (osVersionInfo.szCSDVersion != "")
                {
                    return " SP" + osVersionInfo.szCSDVersion.Substring(13, 1);
                }
                else
                {
                    return "";
                }
            }
        }

        /// <summary>
        /// Returns the name of the operating system running on this computer.
        /// </summary>
        /// <returns>A string containing the the operating system name.</returns>
        public static string GetOSName()
        {
            OperatingSystem osInfo = Environment.OSVersion;
            OSVERSIONINFOEX osVersionInfo = new OSVERSIONINFOEX();
            osVersionInfo.dwOSVersionInfoSize = Marshal.SizeOf(typeof(OSVERSIONINFOEX));
            string osName = "UNKNOWN";
            bool x64Detection = false;

            if (!GetVersionEx(ref osVersionInfo))
            {
                return "";
            }
            else
            {
                switch (osInfo.Platform)
                {
                    case PlatformID.Win32Windows:
                        {
                            switch (osInfo.Version.Minor)
                            {
                                case 0: osName = "Windows 95"; break;
                                case 10:
                                    {
                                        if (osInfo.Version.Revision.ToString() == "2222A")
                                             osName = "Windows 98 Second Edition";
                                        else osName = "Windows 98";
                                    } break;
                                case 90: osName = "Windows Me"; break;
                            }
                            break;
                        }
                    case PlatformID.Win32NT:
                        {
                            switch (osInfo.Version.Major)
                            {
                                case 3: osName = "Windows NT 3.51"; break;
                                case 4:
                                    {
                                        switch (osVersionInfo.wProductType)
                                        {
                                            case 1: osName = "Windows NT 4.0 Workstation"; break;
                                            case 3: osName = "Windows NT 4.0 Server"; break;
                                        }
                                        break;
                                    }
                                case 5:
                                    {
                                        switch (osInfo.Version.Minor)
                                        {
                                            case 0: // win2K
                                                {
                                                    if ((osVersionInfo.wSuiteMask & VER_SUITE_DATACENTER) == VER_SUITE_DATACENTER)
                                                         osName = "Windows 2000 Datacenter Server";
                                                    else if ((osVersionInfo.wSuiteMask & VER_SUITE_ENTERPRISE) == VER_SUITE_ENTERPRISE)
                                                         osName = "Windows 2000 Advanced Server";
                                                    else
                                                         osName = "Windows 2000";
                                                    break;
                                                }                                            
                                            case 1: // winXP
                                                {
                                                    if ((osVersionInfo.wSuiteMask & VER_SUITE_PERSONAL) == VER_SUITE_PERSONAL)
                                                         osName = "Windows XP Home Edition";
                                                    else osName = "Windows XP Professional";
                                                    x64Detection = true;
                                                    break;
                                                }
                                            case 2: // winserver 2003
                                                {
                                                    if ((osVersionInfo.wSuiteMask & VER_SUITE_DATACENTER) == VER_SUITE_DATACENTER)
                                                         osName = "Windows Server 2003 DataCenter Edition";
                                                    else if ((osVersionInfo.wSuiteMask & VER_SUITE_ENTERPRISE) == VER_SUITE_ENTERPRISE)
                                                         osName = "Windows Server 2003 Enterprise Edition";
                                                    else if ((osVersionInfo.wSuiteMask & VER_SUITE_BLADE) == VER_SUITE_BLADE)
                                                         osName = "Windows Server 2003 Web Edition";
                                                    else osName = "Windows Server 2003 Standard Edition";
                                                    x64Detection = true;
                                                    break;
                                                }
                                        } break;
                                    }
                                case 6:
                                    {
                                        x64Detection = true;
                                        switch (osInfo.Version.Minor)
                                        {
                                            case 0:
                                                {
                                                    switch (osVersionInfo.wProductType)
                                                    {
                                                        case 1: // Vista
                                                            {
                                                                uint edition = PRODUCT_UNDEFINED;
                                                                if (GetProductInfo(osVersionInfo.dwMajorVersion,
                                                                                   osVersionInfo.dwMinorVersion,
                                                                                   osVersionInfo.wServicePackMajor,
                                                                                   osVersionInfo.wServicePackMinor,
                                                                                   out edition))
                                                                {
                                                                    switch (edition)
                                                                    {
                                                                        case PRODUCT_ULTIMATE:     osName = "Windows Vista Ultimate Edition";   break;
                                                                        case PRODUCT_HOME_BASIC:
                                                                        case PRODUCT_HOME_BASIC_N: osName = "Windows Vista Home Basic Edition"; break;
                                                                        case PRODUCT_HOME_PREMIUM: osName = "Windows Vista Premium Edition";    break;
                                                                        case PRODUCT_ENTERPRISE:   osName = "Windows Vista Enterprise Edition"; break;
                                                                        case PRODUCT_BUSINESS:
                                                                        case PRODUCT_BUSINESS_N:   osName = "Windows Vista Business Edition";   break;
                                                                        case PRODUCT_STARTER:      osName = "Windows Vista Starter Edition";    break;
                                                                        default:                   osName = "Windows Vista";                    break;
                                                                    }
                                                                } break;
                                                            }
                                                        case 3: // Server 2008
                                                            {
                                                                if ((osVersionInfo.wSuiteMask & VER_SUITE_DATACENTER) == VER_SUITE_DATACENTER)
                                                                    osName = "Windows Server 2008 Datacenter Server";
                                                                else if ((osVersionInfo.wSuiteMask & VER_SUITE_ENTERPRISE) == VER_SUITE_ENTERPRISE)
                                                                    osName = "Windows Server 2008 Advanced Server";
                                                                else
                                                                    osName = "Windows Server 2008";
                                                                break;
                                                            }
                                                    } break;
                                                }
                                            case 1: // Se7en
                                                {
                                                    uint edition = PRODUCT_UNDEFINED;
                                                    if (GetProductInfo(osVersionInfo.dwMajorVersion,
                                                                       osVersionInfo.dwMinorVersion,
                                                                       osVersionInfo.wServicePackMajor,
                                                                       osVersionInfo.wServicePackMinor,
                                                                       out edition))
                                                    {
                                                        switch (edition)
                                                        {
                                                            case PRODUCT_ULTIMATE: osName = "Windows Seven Ultimate Edition"; break;
                                                            case PRODUCT_HOME_BASIC:
                                                            case PRODUCT_HOME_BASIC_N: osName = "Windows Seven Home Basic Edition"; break;
                                                            case PRODUCT_HOME_PREMIUM: osName = "Windows Seven Premium Edition"; break;
                                                            case PRODUCT_ENTERPRISE: osName = "Windows Seven Enterprise Edition"; break;
                                                            case PRODUCT_BUSINESS:
                                                            case PRODUCT_BUSINESS_N: osName = "Windows Seven Professional Edition"; break;
                                                            case PRODUCT_STARTER: osName = "Windows Seven Starter Edition"; break;
                                                            default: osName = "Windows Seven"; break;
                                                        }
                                                    } break;
                                                }
                                        }
                                        break;
                                    }
                            } break;
                        }
                }
            }

            osName += " x64";


            if (x64Detection)
            {
                if (!isWow64())
                    osName += " x86";
                else
                    osName += " x64";
            }

            return osName;
        }

        /// <summary>
        /// Returns the name of the dotNet Framework running on this computer.
        /// </summary>
        /// <returns>A string containing the Name of the Framework Version.</returns>
        /// 
        public static string FormatDotNetVersion()
        {
            string fv = "unknown";
            string componentsKeyName = "SOFTWARE\\Microsoft\\NET Framework Setup\\NDP\\";
            
            // Find out in the registry anything under:
            // HKLM\SOFTWARE\Microsoft\NET Framework Setup\NDP & ".NET Framework" in the name

            using (Microsoft.Win32.RegistryKey componentsKey = Microsoft.Win32.Registry.LocalMachine.OpenSubKey(componentsKeyName))
            {
                try
                {                       
                    string[] instComps = componentsKey.GetSubKeyNames();
                    ArrayList versions = new ArrayList();
                    
                    foreach (string instComp in instComps)
                    {
                        Microsoft.Win32.RegistryKey key = componentsKey.OpenSubKey(instComp);
                        string version = (string)key.GetValue("Version");

                        if (!String.IsNullOrEmpty(version))
                        {
                            versions.Add(version);
                        }
                        else
                        {
                            foreach (string strRegKey in key.GetSubKeyNames())
                            {
                                Microsoft.Win32.RegistryKey strKey = key.OpenSubKey(strRegKey);
                                string strVersion = (string)strKey.GetValue("Version");
                                if (!String.IsNullOrEmpty(strVersion)) 
                                    versions.Add(strVersion);
                            }
                        }
                          
                    }

                    IEnumerator etr = versions.GetEnumerator();
                    while (etr.MoveNext())
                        fv = etr.Current + "";
                }
                catch
                {
                    return null;
                }
            }

            return fv;
        }

        /// <summary>
        /// Returns the name of the dotNet Framework formated
        /// </summary>
        /// <returns>A string containing the dotNet Framework</returns>
        /// 
        public static string DotNetVersionFormated(string dotNetVersion)
        {
            string dnvf = "unknown";
            string major = string.Empty;
            string minor = string.Empty;
            string build = string.Empty;
            string revision = string.Empty;

            try
            {
                if (dotNetVersion != "unknown")
                {
                    string[] versions = dotNetVersion.Split('.');

                    if (versions.Length >= 1)
                        major = versions[0].ToString();
                    if (versions.Length > 1)
                        minor = versions[1].ToString();
                    if (versions.Length > 2)
                        build = versions[2].ToString();
                    if (versions.Length > 3)
                        revision = versions[3].ToString();

                    switch (major)
                    {
                        case "1":
                            {
                                switch (minor)
                                {
                                    case "0":
                                        {
                                            switch (revision)
                                            {
                                                case "209": dnvf = "1.0 SP1"; break;
                                                case "288": dnvf = "1.0 SP2"; break;
                                                case "6018": dnvf = "1.0 SP3"; break;
                                                default: dnvf = "1.0"; break;
                                            }
                                        }
                                        break;
                                    case "1":
                                        {
                                            switch (revision)
                                            {
                                                case "2032":
                                                case "2300": dnvf = "1.1 SP1"; break;
                                                default: dnvf = "1.1"; break;
                                            }
                                        }
                                        break;
                                    default: dnvf = "1.x"; break;
                                }
                                break;
                            }
                        case "2":
                            {
                                switch (revision)
                                {
                                    case "1433":
                                    case "1434": dnvf = "2.0 SP1"; break;
                                    case "2407":
                                    case "3053":
                                    case "3074":
                                    case "4016":
                                    case "4927": dnvf = "2.0 SP2"; break;
                                    default: dnvf = "2.0"; break;
                                }
                            }
                            break;
                        case "3":
                            {
                                switch (minor)
                                {
                                    case "0":
                                        {
                                            switch (revision)
                                            {
                                                case "648": dnvf = "3.0 SP1"; break;
                                                case "1453":
                                                case "2123":
                                                case "4000":
                                                case "4037":
                                                case "4902": // Se7en
                                                case "4926": // Se7en
                                                    dnvf = "3.0 SP2"; break;
                                                default: dnvf = "3.0"; break;
                                            }
                                        }
                                        break;
                                    case "5":
                                        {
                                            switch (revision)
                                            {
                                                case "4926": // Se7en
                                                case "1": dnvf = "3.5 SP1"; break;
                                                default: dnvf = "3.5"; break;
                                            }
                                        }
                                        break;
                                    default: dnvf = "3.x"; break;
                                }
                            }
                            break;
                        case "4":
                            {
                                switch (minor)
                                {
                                    case "0":
                                        {
                                            switch (build)
                                            {
                                                default: dnvf = "4.0"; break;
                                            }
                                        }
                                        break;
                                    default: dnvf = "4.x"; break;
                                }
                            }
                            break;
                        default: dnvf = major + ".x"; break;
                    }

                    if (string.IsNullOrEmpty(revision))
                        dnvf += " (" + major + "." + minor + "." + build + ")";
                    else
                        dnvf += " (" + major + "." + minor + "." + build + "." + revision + ")";
                }
            }
            catch
            {
                dnvf = "unknown: " + dotNetVersion;
            }
            return dnvf;
        }

        /// <summary>
        /// Get some stuff from the Management Object Queries
        /// </summary>
        /// <returns>A string containing the result of the MO query.</returns>
        /// 
        public static string GetMOStuff(string queryObject)
        {
            ManagementObjectSearcher searcher = null;
            string res = "";
            try
            {
               searcher = new ManagementObjectSearcher("SELECT * FROM " + queryObject);
               foreach (ManagementObject mo in searcher.Get())
                {
                    if (queryObject == "Win32_OperatingSystem")
                    {
                        res = mo["Caption"].ToString();
                    }
                    else if (queryObject == "Win32_Processor")
                    {
                        res = mo["Name"].ToString();
                    }
                    else if (queryObject == "Win32_LogicalDisk")
                    {
                        if (mo["DriveType"].ToString() == "3") // HDD
                        {
                            long freespace = long.Parse(mo["FreeSpace"].ToString()) / 1073741824;
                            long totalsize = long.Parse(mo["Size"].ToString()) / 1073741824;

                            if (mo["VolumeName"].ToString() == "")
                                mo["VolumeName"] = "Local Disk";

                            res += mo["VolumeName"].ToString() + " (" + mo["Name"].ToString() + ")  -  " + Convert.ToString(freespace) + " Go free of " + Convert.ToString(totalsize) + " Go\n";
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString());
            }
            return res;
        }
        

        
        /// <summary>
        /// Gets the full version of the operating system running on this computer.
        /// </summary>
        public static string OSVersion
        {
            get
            {
                return Environment.OSVersion.Version.ToString();
            }
        }

        /// <summary>
        /// Gets the major version of the operating system running on this computer.
        /// </summary>
        public static int OSMajorVersion
        {
            get
            {
                return Environment.OSVersion.Version.Major;
            }
        }

        /// <summary>
        /// Gets the minor version of the operating system running on this computer.
        /// </summary>
        public static int OSMinorVersion
        {
            get
            {
                return Environment.OSVersion.Version.Minor;
            }
        }

        /// <summary>
        /// Gets the build version of the operating system running on this computer.
        /// </summary>
        public static int OSBuildVersion
        {
            get
            {
                return Environment.OSVersion.Version.Build;
            }
        }

        /// <summary>
        /// Gets the revision version of the operating system running on this computer.
        /// </summary>
        public static int OSRevisionVersion
        {
            get
            {
                return Environment.OSVersion.Version.Revision;
            }
        }
        
    }
}
