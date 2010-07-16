

using System;
using System.Runtime.InteropServices;

namespace Novell.Win32Util
{



 [ComVisible(false)]
 public class Win32Security
 {
  private const int OWNER_SECURITY_INFORMATION = 0x00000001;
  private const int GROUP_SECURITY_INFORMATION = 0x00000002;
  private const int DACL_SECURITY_INFORMATION = 0x00000004;
  private const int ERROR_INSUFFICIENT_BUFFER = 122;
  private const int ERROR_ACCESS_DENIED = 5;
  private const int TOKEN_ALL_ACCESS = 0x000F01FF;




  private enum SECURITY_IMPERSONATION_LEVEL
  {
   SecurityAnonymous,
   SecurityIdentification,
   SecurityImpersonation,
   SecurityDelegation
  }






  public static bool AccessAllowed(string path)
  {
   bool accessAllowed = false;
   int lastError;
   IntPtr fileSD = IntPtr.Zero;
   IntPtr token = IntPtr.Zero;
   try
   {

    int sdLength = 0;
    if (!GetFileSecurity(path, OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION, fileSD, 0, out sdLength))
    {
     lastError = Marshal.GetLastWin32Error();
     if (lastError != ERROR_INSUFFICIENT_BUFFER)
     {
      return accessAllowed;
     }

     fileSD = Marshal.AllocHGlobal(sdLength);


     if (!GetFileSecurity(path, OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION, fileSD, sdLength, out sdLength))
     {
      lastError = Marshal.GetLastWin32Error();
      return accessAllowed;
     }


     GENERIC_MAPPING genericMapping = new GENERIC_MAPPING();
     PRIVILEGE_SET privSet = new PRIVILEGE_SET();
     uint grantedAccess = 0;

     genericMapping.GenericRead = 0x01;
     genericMapping.GenericWrite = 0x02;
     genericMapping.GenericExecute = 0;
     genericMapping.GenericAll = 0x03;

     uint desiredAccess = 3;

     privSet.Control = 0;
     privSet.PrivilegeCount = 0;
     int privLen = Marshal.SizeOf(privSet);


     if (!ImpersonateSelf(SECURITY_IMPERSONATION_LEVEL.SecurityImpersonation))
     {
      lastError = Marshal.GetLastWin32Error();
      return accessAllowed;
     }
     try
     {

      if (!OpenThreadToken(GetCurrentThread(), TOKEN_ALL_ACCESS, true, ref token))
      {
       lastError = Marshal.GetLastWin32Error();
       return accessAllowed;
      }


      if (!AccessCheck(fileSD, token, desiredAccess, ref genericMapping, ref privSet, ref privLen, ref grantedAccess, ref accessAllowed))
      {
       lastError = Marshal.GetLastWin32Error();
      }
     }
     finally
     {
      RevertToSelf();
     }
    }
   }
   finally
   {
    if (fileSD != IntPtr.Zero)
    {
     Marshal.FreeHGlobal(fileSD);
    }

    if (!token.Equals(IntPtr.Zero))
    {
     CloseHandle(token);
     token = IntPtr.Zero;
    }
   }

   return accessAllowed;
  }

  [DllImport("advapi32.dll", CallingConvention=CallingConvention.Winapi, SetLastError=true, CharSet=CharSet.Unicode)]
  static extern bool GetFileSecurity(string fileName, int RequestedInformation, IntPtr pSecurityDescriptor, int length, out int lengthNeeded);

  [DllImport("advapi32.dll", SetLastError=true)]
  static extern bool ImpersonateSelf(SECURITY_IMPERSONATION_LEVEL ImpersonationLevel);

  [DllImport("advapi32.dll", SetLastError=true)]
  static extern bool RevertToSelf();

  [DllImport("advapi32.dll", SetLastError=true)]
  static extern bool OpenThreadToken(IntPtr ThreadHandle, uint DesiredAccess, bool OpenAsSelf, ref IntPtr TokenHandle);

  [DllImport("advapi32.dll", SetLastError=true)]
  static extern bool AccessCheck(IntPtr pSecurityDescriptor, IntPtr ClientToken, uint DesiredAccess, ref GENERIC_MAPPING GenericMapping, ref PRIVILEGE_SET PrivilegeSet, ref int PrivilegeSetLength, ref uint GrantedAccess, ref bool AccessStatus);

  [DllImport("kernel32.dll")]
  static extern IntPtr GetCurrentThread();

  [DllImport("kernel32.dll")]
  static extern bool CloseHandle(IntPtr hObject);

  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct GENERIC_MAPPING
  {
   public uint GenericRead;
   public uint GenericWrite;
   public uint GenericExecute;
   public uint GenericAll;
  }

  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct PRIVILEGE_SET
  {
   public uint PrivilegeCount;
   public uint Control;
   LUID_AND_ATTRIBUTES Privilege;
  }

  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct LUID_AND_ATTRIBUTES
  {
   public LUID Luid;
   public uint Attributes;
  }

  [StructLayout(LayoutKind.Sequential)]
  [ComVisible(false)]
  public struct LUID
  {
   public uint LowPart;
   public long HighPart;
  }
 }
}
