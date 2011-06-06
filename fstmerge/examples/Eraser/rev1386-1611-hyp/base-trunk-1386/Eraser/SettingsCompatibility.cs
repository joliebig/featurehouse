using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Win32;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
namespace Eraser
{
 public static class SettingsCompatibility
 {
  public static void Execute()
  {
   using (RegistryKey key = Registry.CurrentUser.OpenSubKey(Program.SettingsPath))
   using (RegistryKey mainProgramSettings = key.OpenSubKey(
    "3460478d-ed1b-4ecc-96c9-2ca0e8500557", true))
   {
    const string taskListValueName = @"TaskList";
    if (mainProgramSettings != null)
    {
     if (Array.Find(mainProgramSettings.GetValueNames(),
      delegate(string s) { return s == taskListValueName; }) != null)
     {
      if (!Directory.Exists(Program.AppDataPath))
       Directory.CreateDirectory(Program.AppDataPath);
      if (!File.Exists(Program.TaskListPath))
      {
       byte[] data = (byte[])mainProgramSettings.GetValue(taskListValueName, null);
       using (MemoryStream memStream = new MemoryStream(data))
       using (FileStream stream = new FileStream(Program.TaskListPath,
        FileMode.CreateNew, FileAccess.Write, FileShare.None))
       {
        byte[] serializedData = (byte[])new BinaryFormatter().Deserialize(memStream);
        if (serializedData != null)
         stream.Write(serializedData, 0, serializedData.Length);
       }
      }
      mainProgramSettings.DeleteValue("TaskList");
     }
    }
   }
  }
 }
}
