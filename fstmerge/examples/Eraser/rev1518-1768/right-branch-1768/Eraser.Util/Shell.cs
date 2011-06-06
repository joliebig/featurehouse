using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Win32;
namespace Eraser.Util
{
 public static class Shell
 {
  public static bool LowDiskSpaceNotificationsEnabled
  {
   get
   {
    using (RegistryKey key = Registry.CurrentUser.OpenSubKey(
     "Software\\Microsoft\\Windows\\CurrentVersion\\Policies\\Explorer"))
    {
     if (key == null)
      return true;
     return !Convert.ToBoolean(key.GetValue("NoLowDiskSpaceChecks", false));
    }
   }
   set
   {
    using (RegistryKey key = Registry.CurrentUser.OpenSubKey(
     "Software\\Microsoft\\Windows\\CurrentVersion\\Policies\\Explorer", true))
    {
     key.SetValue("NoLowDiskSpaceChecks", !value);
    }
   }
  }
 }
}
