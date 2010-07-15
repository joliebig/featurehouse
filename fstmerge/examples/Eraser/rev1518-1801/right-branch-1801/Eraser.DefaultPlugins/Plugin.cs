

using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

using Eraser.Manager;
using Eraser.Manager.Plugin;
using Eraser.Util;

namespace Eraser.DefaultPlugins
{
 public sealed class DefaultPlugin : IPlugin
 {
  public void Initialize(Host host)
  {
   Settings = new DefaultPluginSettings();


   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new Gutmann());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new GutmannLite());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new DoD_EcE());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new RCMP_TSSIT_OPS_II());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new Schneier());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new VSITR());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new DoD_E());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new HMGIS5Enhanced());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new USAF5020());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new USArmyAR380_19());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new GOSTP50739());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new HMGIS5Baseline());
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new Pseudorandom());
   EraseCustom.RegisterAll();
   ManagerLibrary.Instance.ErasureMethodRegistrar.Add(new FirstLast16KB());

   ManagerLibrary.Instance.PrngRegistrar.Add(new RngCrypto());

   ManagerLibrary.Instance.FileSystemRegistrar.Add(new Fat12FileSystem());
   ManagerLibrary.Instance.FileSystemRegistrar.Add(new Fat16FileSystem());
   ManagerLibrary.Instance.FileSystemRegistrar.Add(new Fat32FileSystem());
   ManagerLibrary.Instance.FileSystemRegistrar.Add(new NtfsFileSystem());
  }

  public void Dispose()
  {
   GC.SuppressFinalize(this);
  }

  public string Name
  {
   get { return S._("Default Erasure Methods and PRNGs"); }
  }

  public string Author
  {
   get { return S._("The Eraser Project <eraser-development@lists.sourceforge.net>"); }
  }

  public bool Configurable
  {
   get { return true; }
  }

  public void DisplaySettings(Control parent)
  {
   SettingsForm form = new SettingsForm();
   form.ShowDialog();
  }




  internal static DefaultPluginSettings Settings;
 }




 internal class DefaultPluginSettings
 {
  public DefaultPluginSettings()
  {
   settings = Manager.ManagerLibrary.Instance.SettingsManager.ModuleSettings;
  }




  public Guid FL16Method
  {
   get
   {
    return settings["FL16Method"] == null ? Guid.Empty :
     (Guid)settings["FL16Method"];
   }
   set
   {
    settings["FL16Method"] = value;
   }
  }




  public Dictionary<Guid, CustomErasureMethod> EraseCustom
  {
   get
   {
    return (Dictionary<Guid, CustomErasureMethod>)settings["EraseCustom"];
   }
   set
   {
    settings["EraseCustom"] = value;
   }
  }




  Settings settings;
 }
}
