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
   ErasureMethodManager.Register(new Gutmann());
   ErasureMethodManager.Register(new GutmannLite());
   ErasureMethodManager.Register(new DoD_EcE());
   ErasureMethodManager.Register(new RCMP_TSSIT_OPS_II());
   ErasureMethodManager.Register(new Schneier());
   ErasureMethodManager.Register(new VSITR());
   ErasureMethodManager.Register(new DoD_E());
   ErasureMethodManager.Register(new HMGIS5Enhanced());
   ErasureMethodManager.Register(new USAF5020());
   ErasureMethodManager.Register(new USArmyAR380_19());
   ErasureMethodManager.Register(new GOSTP50739());
   ErasureMethodManager.Register(new HMGIS5Baseline());
   ErasureMethodManager.Register(new Pseudorandom());
   EraseCustom.RegisterAll();
   ErasureMethodManager.Register(new FirstLast16KB());
   PrngManager.Register(new RngCrypto());
   FileSystemManager.Register(new Fat12FileSystem());
   FileSystemManager.Register(new Fat16FileSystem());
   FileSystemManager.Register(new Fat32FileSystem());
   FileSystemManager.Register(new NtfsFileSystem());
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
