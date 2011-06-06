using System;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using System.Collections;
using System.Diagnostics;
using System.Net;
using System.Threading;
using System.Globalization;
using Microsoft.Win32;
using Novell.Win32Util;
using Simias.Client;
using Novell.iFolder.Web;
namespace Novell.iFolderCom
{
 public interface IiFolderComponent
 {
  bool CanBeiFolder([MarshalAs(UnmanagedType.LPWStr)] string path);
  bool IsiFolder([MarshalAs(UnmanagedType.LPWStr)] string path, out bool hasConflicts);
  bool CreateiFolder([MarshalAs(UnmanagedType.LPWStr)] string dllPath, [MarshalAs(UnmanagedType.LPWStr)] string path);
        void RevertToNormal([MarshalAs(UnmanagedType.LPWStr)] string path);
  void InvokeAdvancedDlg([MarshalAs(UnmanagedType.LPWStr)] string dllPath, [MarshalAs(UnmanagedType.LPWStr)] string path, int tabPage, bool modal);
  void InvokeConflictResolverDlg([MarshalAs(UnmanagedType.LPWStr)] string dllPath, [MarshalAs(UnmanagedType.LPWStr)] string path);
  void NewiFolderWizard([MarshalAs(UnmanagedType.LPWStr)] string dllPath, [MarshalAs(UnmanagedType.LPWStr)] string path);
  void ShowHelp([MarshalAs(UnmanagedType.LPWStr)] string dllPath, [MarshalAs(UnmanagedType.LPWStr)] string helpFile);
  String GetLanguageDirectory();
 }
 [
  ClassInterface(ClassInterfaceType.None),
  GuidAttribute("AA81D832-3B41-497c-B508-E9D02F8DF421")
 ]
 public class iFolderComponent : IiFolderComponent
 {
  static private iFolderWebService ifWebService = null;
        static private SimiasWebService simws = null;
  static private long ticks = 0;
  static private readonly long delta = 50000000;
  private static readonly string displayConfirmationDisabled = "DisplayConfirmationDisabled";
  private static readonly string displayTrayIconDisabled = "DisplayTrayIconDisabled";
  private static readonly string iFolderKey = @"SOFTWARE\Novell\iFolder";
        private static bool simiasStarting = false;
        private static bool simiasRunning = false;
        private static string simiasStartingLock = "simiasStartingLock";
        private static string simiasRunningLock = "simiasRunningLock";
  private System.Resources.ResourceManager resourceManager = new System.Resources.ResourceManager(typeof(iFolderAdvanced));
        public static bool SimiasRunning
        {
            get
            {
                return simiasRunning;
            }
            set
            {
                lock (simiasRunningLock)
                {
                    simiasRunning = value;
                }
            }
        }
        public static bool SimiasStarting
        {
            get
            {
                return simiasStarting;
            }
            set
            {
                lock (simiasStartingLock)
                {
                    simiasStarting = value;
                }
            }
        }
  public iFolderComponent()
  {
   try
   {
    connectToWebService();
   }
   catch (WebException e)
   {
    ifWebService = null;
    if (e.Status == WebExceptionStatus.ProtocolError)
    {
     LocalService.ClearCredentials();
    }
   }
   catch
   {
   }
  }
  public bool CanBeiFolder([MarshalAs(UnmanagedType.LPWStr)] string path)
  {
   try
   {
    if (Win32Security.AccessAllowed(path))
    {
                    connectToWebService();
     if (ifWebService != null)
     {
      return ifWebService.CanBeiFolder(path);
     }
    }
   }
   catch (WebException e)
   {
    ifWebService = null;
    if (e.Status == WebExceptionStatus.ProtocolError)
    {
     LocalService.ClearCredentials();
    }
   }
   catch (Exception e)
   {
    System.Diagnostics.Debug.WriteLine("Caught exception - " + e.Message);
   }
   return false;
  }
  public bool IsiFolder([MarshalAs(UnmanagedType.LPWStr)] string path, out bool hasConflicts)
  {
   iFolderWeb ifolder = null;
   hasConflicts = false;
   try
   {
    connectToWebService();
    if (ifWebService != null)
    {
     ifolder = ifWebService.GetiFolderByLocalPath(path);
     if (ifolder != null)
     {
      hasConflicts = ifolder.HasConflicts;
     }
    }
   }
   catch (WebException e)
   {
    ifWebService = null;
    if (e.Status == WebExceptionStatus.ProtocolError)
    {
     LocalService.ClearCredentials();
    }
   }
   catch (Exception e)
   {
    System.Diagnostics.Debug.WriteLine("Caught exception - " + e.Message);
   }
   return ifolder != null;
  }
  public bool CreateiFolder([MarshalAs(UnmanagedType.LPWStr)] string dllPath, [MarshalAs(UnmanagedType.LPWStr)] string path)
  {
   CreateiFolder createiFolder = new CreateiFolder();
            connectToWebService();
   createiFolder.iFolderWebService = ifWebService;
            createiFolder.simiasWebService = simws;
   createiFolder.iFolderPath = path;
   createiFolder.LoadPath = dllPath;
   if (DialogResult.OK == createiFolder.ShowDialog())
    return true;
   return false;
  }
        public void RevertToNormal([MarshalAs(UnmanagedType.LPWStr)] string path)
        {
            Cursor.Current = Cursors.WaitCursor;
            try
            {
                connectToWebService();
                RevertiFolder revertiFolder = new RevertiFolder();
                iFolderWeb ifolder = ifWebService.GetiFolderByLocalPath(path);
                bool IsMaster = (ifolder.CurrentUserID == ifolder.OwnerID);
                if (!IsMaster)
                    revertiFolder.removeFromServer.Text = resourceManager.GetString("AlsoRemoveMembership");
                if (revertiFolder.ShowDialog() == DialogResult.Yes)
                {
                    Cursor.Current = Cursors.WaitCursor;
                    if (ifWebService != null && ifolder != null)
                    {
                        ifWebService.RevertiFolder(ifolder.ID);
                        if(revertiFolder.RemoveFromServer)
                        {
                            if (IsMaster)
                            {
                                ifWebService.DeleteiFolder(ifolder.DomainID, ifolder.ID);
                            }
                            else
                            {
                                ifWebService.DeclineiFolderInvitation(ifolder.DomainID, ifolder.ID);
                            }
                        }
                    }
                }
                revertiFolder.Dispose();
            }
            catch (WebException e)
            {
                ifWebService = null;
                if (e.Status == WebExceptionStatus.ProtocolError)
                {
                    LocalService.ClearCredentials();
                }
            }
            catch (Exception e)
            {
                Cursor.Current = Cursors.Default;
                Novell.iFolderCom.MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("iFolderRevertError"),
                    resourceManager.GetString("revertErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
                mmb.ShowDialog();
                mmb.Dispose();
            }
            Cursor.Current = Cursors.Default;
        }
  public void InvokeAdvancedDlg([MarshalAs(UnmanagedType.LPWStr)] string dllPath, [MarshalAs(UnmanagedType.LPWStr)] string path, int tabPage, bool modal)
  {
   string windowName = string.Format(resourceManager.GetString("iFolderProperties"), Path.GetFileName(path));
   Win32Window win32Window = Win32Util.Win32Window.FindWindow(null, windowName);
   if (win32Window != null)
   {
    win32Window.BringWindowToTop();
   }
   else
   {
    try
    {
     iFolderAdvanced ifolderAdvanced = new iFolderAdvanced();
     ifolderAdvanced.Name = path;
     ifolderAdvanced.Text = windowName;
     connectToWebService();
     ifolderAdvanced.CurrentiFolder = ifWebService.GetiFolderByLocalPath(path);
     ifolderAdvanced.LoadPath = dllPath;
     ifolderAdvanced.ActiveTab = tabPage;
                    ifolderAdvanced.DomainName = (simws.GetDomainInformation((ifWebService.GetiFolderByLocalPath(path)).DomainID)).Name;
                    ifolderAdvanced.DomainUrl = (simws.GetDomainInformation((ifWebService.GetiFolderByLocalPath(path)).DomainID)).HostUrl;
     if (modal)
     {
      ifolderAdvanced.ShowDialog();
     }
     else
     {
      ifolderAdvanced.Show();
     }
    }
    catch (WebException e)
    {
     MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("propertiesDialogError"), resourceManager.GetString("propertiesErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
     mmb.ShowDialog();
     ifWebService = null;
     if (e.Status == WebExceptionStatus.ProtocolError)
     {
      LocalService.ClearCredentials();
     }
    }
    catch (Exception e)
    {
     MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("propertiesDialogError"), resourceManager.GetString("propertiesErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
     mmb.ShowDialog();
    }
   }
  }
  public void InvokeConflictResolverDlg([MarshalAs(UnmanagedType.LPWStr)] string dllPath, [MarshalAs(UnmanagedType.LPWStr)] string path)
  {
   try
   {
    iFolderWeb ifolder = ifWebService.GetiFolderByLocalPath(path);
    ConflictResolver conflictResolver = new ConflictResolver();
    conflictResolver.iFolder = ifolder;
    conflictResolver.iFolderWebService = ifWebService;
    conflictResolver.LoadPath = dllPath;
    conflictResolver.Show();
   }
   catch (Exception ex)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("conflictDialogError"), resourceManager.GetString("conflictErrorTitle"), ex.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
  }
  public void NewiFolderWizard([MarshalAs(UnmanagedType.LPWStr)] string dllPath, [MarshalAs(UnmanagedType.LPWStr)] string path)
  {
   try
   {
    if (DisplayConfirmationEnabled)
    {
     NewiFolder newiFolder = new NewiFolder();
     newiFolder.FolderName = path;
     newiFolder.LoadPath = dllPath;
     newiFolder.ShowDialog();
    }
   }
   catch (WebException e)
   {
    if (e.Status == WebExceptionStatus.ConnectFailure)
    {
     ifWebService = null;
    }
   }
   catch {}
  }
  public void ShowHelp([MarshalAs(UnmanagedType.LPWStr)] string dllPath, [MarshalAs(UnmanagedType.LPWStr)] string helpFile)
  {
            string defaulthelpfile = "bookinfo.html";
   string helpPath = helpFile.Equals(string.Empty) ?
    Path.Combine(Path.Combine(Path.Combine(dllPath, "help"), GetLanguageDirectory()), defaulthelpfile) :
    Path.Combine(Path.Combine(Path.Combine(dllPath, "help"), GetLanguageDirectory()), helpFile);
   if (!File.Exists(helpPath))
   {
    helpPath = helpFile.Equals(string.Empty) ?
     Path.Combine(dllPath, @"help\en\bookinfo.html") :
     Path.Combine(Path.Combine(dllPath, @"help\en"), helpFile);
   }
   try
   {
    Process.Start(helpPath);
   }
   catch (Exception e)
   {
    MyMessageBox mmb = new MyMessageBox(resourceManager.GetString("helpFileError") + "\n" + helpPath, resourceManager.GetString("helpErrorTitle"), e.Message, MyMessageBoxButtons.OK, MyMessageBoxIcon.Error);
    mmb.ShowDialog();
   }
  }
  public String GetLanguageDirectory()
  {
   return iFolderAdvanced.GetLanguageDirectory();
  }
  private void connectToWebService()
  {
   if (ifWebService == null)
   {
                if (simiasRunning == false)
                {
                    Process[] processArray = Process.GetProcessesByName("simias");
                    if (processArray == null || processArray.Length == 0)
                    {
                        System.Diagnostics.Debug.WriteLine(string.Format("simias is not running dude..."));
                        return;
                    }
                    simiasRunning = true;
                }
                System.Diagnostics.Debug.WriteLine(string.Format("In connectToWebService(). Simias is running...", SimiasRunning));
                Thread.CurrentThread.CurrentUICulture = new CultureInfo(GetLanguageDirectory());
    string windowName = resourceManager.GetString("iFolderServices") + Environment.UserName;
                Novell.Win32Util.Win32Window window = Novell.Win32Util.Win32Window.FindWindow(null, windowName);
    if (window != null)
    {
                    DateTime currentTime = DateTime.Now;
     if ((currentTime.Ticks - ticks) > delta)
     {
      ticks = currentTime.Ticks;
      RegistryKey regKey = Registry.CurrentUser.OpenSubKey(@"SOFTWARE\Novell\iFolder");
      if (regKey != null)
      {
       string webServiceUri = regKey.GetValue("WebServiceUri") as string;
       string simiasDataPath = regKey.GetValue("SimiasDataPath") as string;
       if ((webServiceUri != null) && (simiasDataPath != null))
       {
        ifWebService = new iFolderWebService();
        ifWebService.Url = webServiceUri + "/iFolder.asmx";
        LocalService.Start(ifWebService, new Uri(webServiceUri), simiasDataPath);
                                simws = new SimiasWebService();
                                simws.Url = webServiceUri + "/Simias.asmx";
                                LocalService.Start(simws, new Uri(webServiceUri), simiasDataPath);
                   }
       regKey.Close();
      }
     }
    }
    else
    {
                    throw new Exception("iFolder Client not running");
    }
   }
  }
  static public bool DisplayConfirmationEnabled
  {
   get
   {
    int display;
    try
    {
     RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);
     display = (int)regKey.GetValue(displayConfirmationDisabled, 0);
    }
    catch
    {
     return true;
    }
    return (display == 0);
   }
   set
   {
    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);
    if (value)
    {
     regKey.DeleteValue(displayConfirmationDisabled, false);
    }
    else
    {
     regKey.SetValue(displayConfirmationDisabled, 1);
    }
   }
  }
       static public bool DisplayTrayIconEnabled
            {
             get
             {
                  int display;
                 try
              {
                    RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);
                    display = (int)regKey.GetValue(displayTrayIconDisabled, 0);
                   }
                   catch
                   {
                   return true;
              }
                   return (display == 0);
             }
             set
             {
                RegistryKey regKey = Registry.CurrentUser.CreateSubKey(iFolderKey);
                   if (value)
             {
                   regKey.DeleteValue(displayTrayIconDisabled, false);
                   }
                  else
                   {
                    regKey.SetValue(displayTrayIconDisabled, 1);
                   }
             }
       }
 }
}
