using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Text;
using System.IO;
using System.Globalization;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using Microsoft.Win32;
using Eraser.Util;
namespace Eraser
{
 internal class Settings : Manager.SettingsManager
 {
  private sealed class RegistrySettings : Manager.Settings, IDisposable
  {
   public RegistrySettings(Guid pluginId, RegistryKey key)
   {
    this.PluginID = pluginId;
    this.Key = key;
   }
   ~RegistrySettings()
   {
    Dispose(false);
   }
   public void Dispose()
   {
    Dispose(true);
    GC.SuppressFinalize(this);
   }
   private void Dispose(bool disposing)
   {
    if (Key == null)
     return;
    if (disposing)
     Key.Close();
    Key = null;
   }
   public override object this[string setting]
   {
    get
    {
     object rawResult = Key.GetValue(setting, null);
     byte[] resultArray = rawResult as byte[];
     if (resultArray != null)
     {
      using (MemoryStream stream = new MemoryStream(resultArray))
       try
       {
        return new BinaryFormatter().Deserialize(stream);
       }
       catch (SerializationException)
       {
        Key.DeleteValue(setting);
        MessageBox.Show(S._("Could not load the setting {0}\\{1} for " +
          "plugin {2}. The setting has been lost.", Key, setting,
          PluginID.ToString()),
         S._("Eraser"), MessageBoxButtons.OK, MessageBoxIcon.Error,
         MessageBoxDefaultButton.Button1,
         Localisation.IsRightToLeft(null) ?
          MessageBoxOptions.RtlReading | MessageBoxOptions.RightAlign : 0);
       }
     }
     else
     {
      return rawResult;
     }
     return null;
    }
    set
    {
     if (value == null)
     {
      Key.DeleteValue(setting);
     }
     else
     {
      if (value is bool)
       Key.SetValue(setting, value, RegistryValueKind.DWord);
      else if ((value is int) || (value is uint))
       Key.SetValue(setting, value, RegistryValueKind.DWord);
      else if ((value is long) || (value is ulong))
       Key.SetValue(setting, value, RegistryValueKind.QWord);
      else if (value is string)
       Key.SetValue(setting, value, RegistryValueKind.String);
      else
       using (MemoryStream stream = new MemoryStream())
       {
        new BinaryFormatter().Serialize(stream, value);
        Key.SetValue(setting, stream.ToArray(), RegistryValueKind.Binary);
       }
     }
    }
   }
   private Guid PluginID;
   private RegistryKey Key;
  }
  public override void Save()
  {
  }
  protected override Manager.Settings GetSettings(Guid guid)
  {
   RegistryKey eraserKey = null;
   try
   {
    eraserKey = Registry.CurrentUser.OpenSubKey(Program.SettingsPath, true);
    if (eraserKey == null)
     eraserKey = Registry.CurrentUser.CreateSubKey(Program.SettingsPath);
    RegistryKey pluginsKey = eraserKey.OpenSubKey(guid.ToString(), true);
    if (pluginsKey == null)
     pluginsKey = eraserKey.CreateSubKey(guid.ToString());
    return new RegistrySettings(guid, pluginsKey);
   }
   finally
   {
    if (eraserKey != null)
     eraserKey.Close();
   }
  }
 }
 internal class EraserSettings
 {
  private EraserSettings()
  {
   settings = Manager.ManagerLibrary.Instance.SettingsManager.ModuleSettings;
  }
  public static EraserSettings Get()
  {
   if (instance == null)
    instance = new EraserSettings();
   return instance;
  }
  public string Language
  {
   get
   {
    return settings["Language"] == null ?
     GetCurrentCulture().Name :
     (string)settings["Language"];
   }
   set
   {
    settings["Language"] = value;
   }
  }
  public bool IntegrateWithShell
  {
   get
   {
    return settings["IntegrateWithShell"] == null ?
     true : Convert.ToBoolean(settings["IntegrateWithShell"],
      CultureInfo.InvariantCulture);
   }
   set
   {
    settings["IntegrateWithShell"] = value;
   }
  }
  public bool HideWhenMinimised
  {
   get
   {
    return settings["HideWhenMinimised"] == null ?
     true : Convert.ToBoolean(settings["HideWhenMinimised"],
      CultureInfo.InvariantCulture);
   }
   set
   {
    settings["HideWhenMinimised"] = value;
   }
  }
  public bool ClearCompletedTasks
  {
   get
   {
    return settings["ClearCompletedTasks"] == null ?
     true : Convert.ToBoolean(settings["ClearCompletedTasks"],
      CultureInfo.InvariantCulture);
   }
   set
   {
    settings["ClearCompletedTasks"] = value;
   }
  }
  private static CultureInfo GetCurrentCulture()
  {
   System.Reflection.Assembly entryAssembly = System.Reflection.Assembly.GetEntryAssembly();
   CultureInfo culture = CultureInfo.CurrentUICulture;
   while (culture.Parent != CultureInfo.InvariantCulture &&
    !Localisation.LocalisationExists(culture, entryAssembly))
   {
    culture = culture.Parent;
   }
   if (!Localisation.LocalisationExists(culture, entryAssembly))
    culture = new CultureInfo("en");
   return culture;
  }
  private Manager.Settings settings;
  private static EraserSettings instance;
 }
}
