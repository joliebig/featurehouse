using System;
using System.Runtime.InteropServices;
namespace Debugger.Wrappers
{
 public delegate void UnmanagedStringGetter(uint pStringLenght, out uint stringLenght, System.IntPtr pString);
 public static class Util
 {
  public static string GetString(UnmanagedStringGetter getter)
  {
   return GetString(getter, 64, true);
  }
  public static string GetString(UnmanagedStringGetter getter, uint defaultLenght, bool trim)
  {
   string managedString;
   IntPtr unmanagedString;
   uint exactLenght;
   unmanagedString = Marshal.AllocHGlobal((int)defaultLenght * 2 + 2);
   getter(defaultLenght, out exactLenght, defaultLenght > 0 ? unmanagedString : IntPtr.Zero);
   if(exactLenght > defaultLenght) {
    Marshal.FreeHGlobal(unmanagedString);
    unmanagedString = Marshal.AllocHGlobal((int)exactLenght * 2 + 2);
    getter(exactLenght, out exactLenght, unmanagedString);
   }
   managedString = Marshal.PtrToStringUni(unmanagedString, (int)exactLenght);
   if (trim) {
    managedString = managedString.TrimEnd('\0');
   }
   Marshal.FreeHGlobal(unmanagedString);
   return managedString;
  }
 }
}
