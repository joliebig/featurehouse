using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.Serialization;
namespace Eraser.Manager
{
 public class ManagerLibrary : IDisposable
 {
  public ManagerLibrary(SettingsManager settings)
  {
   if (Instance != null)
    throw new InvalidOperationException("Only one ManagerLibrary instance can " +
     "exist at any one time");
   Instance = this;
   SettingsManager = settings;
   EntropySourceRegistrar = new EntropySourceRegistrar();
   PrngRegistrar = new PrngRegistrar();
   ErasureMethodRegistrar = new ErasureMethodRegistrar();
   FileSystemRegistrar = new FileSystemRegistrar();
   Host = new Plugin.DefaultHost();
   Host.Load();
  }
  ~ManagerLibrary()
  {
   Dispose(false);
  }
  protected virtual void Dispose(bool disposing)
  {
   if (SettingsManager == null)
    return;
   if (disposing)
   {
    EntropySourceRegistrar.Poller.Abort();
    Host.Dispose();
    SettingsManager.Save();
   }
   SettingsManager = null;
   Instance = null;
  }
  public void Dispose()
  {
   Dispose(true);
   GC.SuppressFinalize(this);
  }
  public static ManagerLibrary Instance { get; private set; }
  public EntropySourceRegistrar EntropySourceRegistrar { get; private set; }
  public PrngRegistrar PrngRegistrar { get; private set; }
  public ErasureMethodRegistrar ErasureMethodRegistrar { get; private set; }
  public FileSystemRegistrar FileSystemRegistrar { get; private set; }
  public SettingsManager SettingsManager { get; set; }
  public static ManagerSettings Settings
  {
   get
   {
    if (settingsInstance == null)
     settingsInstance = new ManagerSettings();
    return settingsInstance;
   }
  }
  private static ManagerSettings settingsInstance;
  internal Plugin.DefaultHost Host;
 }
}
