using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.IO;
using System.ComponentModel;
namespace Eraser.Util
{
 public static class Win32ErrorCode
 {
  private static int GetHRForWin32Error(int errorCode)
  {
   const uint FACILITY_WIN32 = 7;
   return errorCode <= 0 ? errorCode :
    (int)((((uint)errorCode) & 0x0000FFFF) | (FACILITY_WIN32 << 16) | 0x80000000);
  }
  internal static Exception GetExceptionForWin32Error(int errorCode)
  {
   switch (errorCode)
   {
    case NoError: return null;
    case SharingViolation: return new IOException();
   }
   int HR = GetHRForWin32Error(errorCode);
   Exception exception = Marshal.GetExceptionForHR(HR);
   if (exception.GetType() == typeof(COMException))
    throw new Win32Exception(errorCode);
   else
    throw exception;
  }
  public const int NoError = Success;
  public const int Success = 0;
  public const int InvalidFunction = 1;
  public const int FileNotFound = 2;
  public const int PathNotFound = 3;
  public const int AccessDenied = 5;
  public const int NoMoreFiles = 18;
  public const int NotReady = 21;
  public const int SharingViolation = 32;
  public const int InvalidParameter = 87;
  public const int MoreData = 234;
  public const int NoMoreItems = 259;
  public const int UnrecognizedVolume = 1005;
  public const int BadDevice = 1200;
  public const int NotAReparsePoint = 4390;
 }
}
