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
   EntropySourceManager = new EntropySourceManager();
   PRNGManager = new PrngManager();
   ErasureMethodManager = new ErasureMethodManager();
   FileSystemManager = new FileSystemManager();
   Host = new Plugin.DefaultHost();
   Host.Load();
  }
  ~ManagerLibrary()
  {
   Dispose(false);
  }
  protected virtual void Dispose(bool disposing)
  {
   if (disposing)
   {
    EntropySourceManager.Poller.Abort();
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
  internal EntropySourceManager EntropySourceManager;
  internal PrngManager PRNGManager;
  internal ErasureMethodManager ErasureMethodManager;
  internal FileSystemManager FileSystemManager;
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
