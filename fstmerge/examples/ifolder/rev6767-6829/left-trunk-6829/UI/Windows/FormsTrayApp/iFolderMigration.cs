

using System;
using System.IO;
using Microsoft.Win32;

namespace Novell.FormsTrayApp
{



 public class iFolderMigration
 {


  private static readonly string iFolderRegistryKey = @"Software\Novell iFolder";
  private static readonly string proxyRegistryKey = "Proxy";
  private static readonly string migratedValueName = "Migrated";





  public iFolderMigration()
  {
  }






  public bool CanBeMigrated()
  {

   RegistryKey iFolderKey = Registry.LocalMachine.OpenSubKey(iFolderRegistryKey);

   if (iFolderKey != null)
   {
    try
    {

     object migratedValue = iFolderKey.GetValue(migratedValueName);
     if (migratedValue != null)
     {
      if ((int)migratedValue == 0)
      {

       return false;
      }
     }
     else
     {

      return true;
     }
    }
    finally
    {
     iFolderKey.Close();
    }
   }

   return false;
  }





  public void SetMigratedValue(int migrated)
  {
   RegistryKey iFolderKey = Registry.LocalMachine.OpenSubKey(iFolderRegistryKey, true);
   if (iFolderKey != null)
   {
    iFolderKey.SetValue(migratedValueName, migrated);

    iFolderKey.Close();
   }
  }




  public void MigrateSettings()
  {
   try
   {

    RegistryKey proxyKey = Registry.LocalMachine.OpenSubKey(Path.Combine(iFolderRegistryKey, proxyRegistryKey));
    if (proxyKey != null)
    {
     string proxyValue = (string)proxyKey.GetValue("Address");

     if (proxyValue != null)
     {


     }

     proxyKey.Close();
    }




    SetMigratedValue(1);
   }
   catch
   {

   }
  }

 }
}
