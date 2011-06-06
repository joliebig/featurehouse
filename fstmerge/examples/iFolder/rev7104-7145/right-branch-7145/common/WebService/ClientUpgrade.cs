using System;
using System.Collections.Specialized;
using System.Diagnostics;
using System.IO;
using System.Net;
using Simias.Client;
using Simias;
namespace Novell.iFolder.Install
{
 public enum UpgradeResult
 {
  Latest = 0,
  UpgradeNeeded = 1,
  ServerOld = 2,
  UpgradeAvailable = 3,
  Unknown =4,
 };
 public class ClientUpgrade
 {
  private static readonly ISimiasLog log = SimiasLogManager.GetLogger(typeof(ClientUpgrade));
  private static string iFolderUpdateDirectory = "ead51d60-cd98-4d35-8c7c-b43a2ca949c8";
  private static string iFolderWindowsApplication = "iFolderApp.exe";
  private static string iFolderLinuxApplication = "iFolderClient.exe";
  private static readonly string LinuxPlatformFile = "/etc/issue";
  private static string PlatformQuery = "Platform";
  private static string FileQuery = "File";
  private ClientUpdate service = null;
  public string hostAddress;
  private ClientUpgrade( string domainID )
  {
   WebState ws = new WebState(domainID, domainID);
   hostAddress = DomainProvider.ResolveLocation( domainID ).ToString();
   service = new ClientUpdate();
   service.Url = hostAddress + "/ClientUpdate.asmx";
   ws.InitializeWebClient(service, domainID);
  }
  private string DownloadFiles( string[] fileList, string path )
  {
   string downloadDir = "";
   if( path == null)
    downloadDir = Path.Combine( Path.GetTempPath(), iFolderUpdateDirectory );
   else
    downloadDir = Path.Combine( path, iFolderUpdateDirectory);
   if ( Directory.Exists( downloadDir ) )
   {
    Directory.Delete( downloadDir, true );
   }
   Directory.CreateDirectory( downloadDir );
   try
   {
    WebClient webClient = new WebClient();
    webClient.Credentials = service.Credentials;
    foreach ( string file in fileList )
    {
     NameValueCollection nvc = new NameValueCollection();
     if (MyEnvironment.Platform == MyPlatformID.Darwin)
     {
      log.Debug("Downloading Mac files");
      nvc.Add(PlatformQuery, MyEnvironment.Platform.ToString() );
     }
     else if ( MyEnvironment.Platform == MyPlatformID.Windows )
     {
      log.Debug("Downloading Windows files");
      nvc.Add( PlatformQuery, MyEnvironment.Platform.ToString() );
     }
     else
     {
      log.Debug("Downloading Linux files");
      nvc.Add( PlatformQuery, GetLinuxPlatformString() );
     }
     nvc.Add( FileQuery, file );
     webClient.QueryString = nvc;
     webClient.DownloadFile( hostAddress + "/ClientUpdateHandler.ashx", Path.Combine( downloadDir, file ) );
     log.Debug("Download the latest client completed");
    }
   }
   catch
   {
    Directory.Delete( downloadDir, true );
    downloadDir = null;
   }
   return downloadDir;
  }
  private string GetWindowsClientVersion()
  {
   string version = null;
   string fullPath = Path.Combine( SimiasSetup.bindir, "..");
   fullPath = Path.Combine( fullPath, iFolderWindowsApplication );
   if ( File.Exists( fullPath ) )
   {
    FileVersionInfo versionInfo = FileVersionInfo.GetVersionInfo( fullPath );
    version = versionInfo.ProductVersion;
   }
   return version;
  }
  private string GetLinuxPlatformString()
  {
   string platformString = null;
   if ( File.Exists( LinuxPlatformFile ) )
   {
    StreamReader sr = null;
    try
    {
     sr = new StreamReader( LinuxPlatformFile );
     string line = null;
     do
     {
      line = sr.ReadLine();
      if (line != null)
      {
       if (platformString == null)
        platformString = line;
       else
        platformString = string.Concat(platformString, line);
      }
     } while (line != null);
    }
    catch {}
    finally
    {
     if (sr != null)
      sr.Close();
    }
    if ( platformString != null )
     platformString = platformString.Trim();
   }
   return platformString;
  }
  private string GetLinuxClientVersion()
  {
   string version = null;
   string fullPath = Path.Combine( SimiasSetup.bindir, iFolderLinuxApplication );
   if ( File.Exists( fullPath ) )
   {
    FileVersionInfo versionInfo = FileVersionInfo.GetVersionInfo( fullPath );
    version = versionInfo.ProductVersion;
   }
   return version;
  }
  private string CheckForUpdateAvailable()
  {
   string updateVersion = null;
   if ( service != null )
   {
    string currentVersion = null;
    string platformString = null;
    if ( MyEnvironment.Platform == MyPlatformID.Windows )
    {
                    platformString = GetWinOSPlatform();
     currentVersion = GetWindowsClientVersion();
    }
    else if ( MyEnvironment.Platform == MyPlatformID.Unix )
    {
     platformString = GetLinuxPlatformString();
     currentVersion = GetLinuxClientVersion();
    }
    if ( platformString != null && currentVersion != null )
    {
                    try
                    {
                        updateVersion = service.IsUpdateAvailableActualSoapDocMethod(platformString, currentVersion);
                    }
                    catch (System.Web.Services.Protocols.SoapHeaderException ex)
                    {
                        if (ex.Message.IndexOf("Server did not recognize the value of HTTP header SOAPAction") != -1)
                            updateVersion = service.IsUpdateAvailableActual(platformString, currentVersion);
                        else
                            throw ex;
                    }
    }
   }
   return updateVersion;
  }
        private StatusCodes CheckForMacUpdate(string curVersion, out string ServerVersion)
        {
            log.Debug("Calling Server to check for Update with version:{0}", curVersion);
            string updateVersion = null;
            ServerVersion = null;
            if (service != null)
            {
                string serverVersion = null;
                StatusCodes stat;
                try
                {
                    stat = (StatusCodes)service.CheckForUpdateSoapDocMethod("Darwin", curVersion, out serverVersion);
                }
                catch (Exception ex)
                {
                    stat = (StatusCodes)service.CheckForUpdate("Darwin", curVersion, out serverVersion);
                }
                ServerVersion = serverVersion;
                return stat;
            }
            log.Debug("service in CheckForMacUpdate is null");
            return StatusCodes.Unknown;
        }
  private StatusCodes CheckForUpdate(out string ServerVersion)
  {
   string updateVersion = null;
   ServerVersion = null;
   if ( service != null )
   {
    string currentVersion = null;
    string platformString = null;
                if ( MyEnvironment.Platform == MyPlatformID.Windows )
    {
                    platformString = GetWinOSPlatform();
     currentVersion = GetWindowsClientVersion();
    }
    else if ( MyEnvironment.Platform == MyPlatformID.Unix )
    {
     platformString = GetLinuxPlatformString();
     currentVersion = GetLinuxClientVersion();
    }
    if ( platformString != null && currentVersion != null )
    {
     string serverVersion = null;
                    StatusCodes stat;
                    try
                    {
                        stat = (StatusCodes)service.CheckForUpdateSoapDocMethod(platformString, currentVersion, out serverVersion);
                    }
                    catch (System.Web.Services.Protocols.SoapHeaderException ex)
                    {
                        if (ex.Message.IndexOf("Server did not recognize the value of HTTP header SOAPAction") != -1)
                            stat = (StatusCodes)service.CheckForUpdate(platformString, currentVersion, out serverVersion);
                        else
                            throw ex;
                    }
     ServerVersion = serverVersion;
     return stat;
    }
   }
   return StatusCodes.Unknown;
  }
        string GetWinOSPlatform()
        {
      string str = System.Environment.GetEnvironmentVariable("ProgramFiles");
            if (str != null && str.IndexOf("x86") != -1)
            {
             return "windows64";
            }
            else
            {
               return "windows32";
            }
            return "windows";
        }
  private bool CheckForServerUpdate()
  {
   string updateVersion = null;
   bool serverOlder = false;
   if ( service != null )
   {
    string currentVersion = null;
    string platformString = null;
    if ( MyEnvironment.Platform == MyPlatformID.Windows )
    {
                    platformString = GetWinOSPlatform();
     currentVersion = GetWindowsClientVersion();
    }
    else if ( MyEnvironment.Platform == MyPlatformID.Unix )
    {
     platformString = GetLinuxPlatformString();
     currentVersion = GetLinuxClientVersion();
    }
    if ( platformString != null && currentVersion != null )
    {
                    try
                    {
                        serverOlder = service.IsServerOlderSoapDocMethod(platformString, currentVersion);
                    }
                    catch (System.Web.Services.Protocols.SoapHeaderException ex)
                    {
                        if (ex.Message.IndexOf("Server did not recognize the value of HTTP header SOAPAction") != -1)
                            serverOlder = service.IsServerOlder(platformString, currentVersion);
                        else
                            throw ex;
                    }
    }
   }
   return serverOlder;
  }
  private bool RunUpdate(string path)
  {
   bool running = false;
   if ( service != null )
   {
                string[] fileList;
                try
                {
                    fileList = service.GetUpdateFilesSoapDocMethod();
                }
                catch (System.Web.Services.Protocols.SoapHeaderException ex)
                {
                    if (ex.Message.IndexOf("Server did not recognize the value of HTTP header SOAPAction") != -1)
                        fileList = service.GetUpdateFiles();
                    else
                        throw ex;
                }
                  if ( fileList != null )
    {
     string downloadDir = DownloadFiles( fileList, path );
     if ( downloadDir != null )
     {
      running = true;
      if ( MyEnvironment.Platform == MyPlatformID.Darwin)
      {
      }
      else if ( MyEnvironment.Platform == MyPlatformID.Windows )
      {
       Process installProcess = new Process();
       installProcess.StartInfo.FileName = Path.Combine( downloadDir, Path.GetFileName( fileList[ 0 ] ) );
       installProcess.StartInfo.UseShellExecute = true;
       installProcess.StartInfo.CreateNoWindow = false;
       running = installProcess.Start();
      }
      else if ( MyEnvironment.Platform == MyPlatformID.Unix )
      {
       string installScriptPath = Path.Combine( downloadDir, "install-ifolder.sh" );
       if ( File.Exists( installScriptPath ) )
       {
        Process installProcess = new Process();
        installProcess.StartInfo.FileName = "sh";
        installProcess.StartInfo.WorkingDirectory = downloadDir;
        installProcess.StartInfo.Arguments =
         string.Format("{0} {1}", installScriptPath, downloadDir);
        installProcess.StartInfo.UseShellExecute = true;
        installProcess.StartInfo.CreateNoWindow = false;
        try
        {
         running = installProcess.Start();
        }
        catch{}
       }
      }
     }
    }
   }
   return running;
  }
                public static int CheckForMacUpdate(string domainID, string currentVersion, out string serverVersion)
                {
                        int retval = -1;
                        string ServerVersion = null;
                        ClientUpgrade cu = new ClientUpgrade(domainID);
                        StatusCodes stat = cu.CheckForMacUpdate(currentVersion, out ServerVersion);
                        switch( stat)
                        {
                                case StatusCodes.Success:
                                                retval = (int)UpgradeResult.Latest;
                                                break;
                                case StatusCodes.UpgradeNeeded:
                                                retval = (int)UpgradeResult.UpgradeNeeded;
                                                break;
                                case StatusCodes.ServerOld:
                                                retval = (int)UpgradeResult.ServerOld;
                           Version ver = new Version(ServerVersion);
                           if (ver.Major == 3 && ver.Minor >= 6)
                               retval = (int)UpgradeResult.Latest;
                                                break;
                                case StatusCodes.OlderVersion:
                                                retval = (int)UpgradeResult.UpgradeAvailable;
                                                break;
                                case StatusCodes.Unknown:
                                                retval = (int)UpgradeResult.Unknown;
                                                break;
                                default:
                                                break;
                        }
                        serverVersion = ServerVersion;
                        return retval;
                }
  public static int CheckForUpdate(string domainID, out string serverVersion)
  {
   int retval = -1;
   string ServerVersion = null;
   ClientUpgrade cu = new ClientUpgrade(domainID);
   StatusCodes stat = cu.CheckForUpdate(out ServerVersion);
   switch( stat)
   {
    case StatusCodes.Success:
      retval = (int)UpgradeResult.Latest;
      break;
    case StatusCodes.UpgradeNeeded:
      retval = (int)UpgradeResult.UpgradeNeeded;
      break;
    case StatusCodes.ServerOld:
      retval = (int)UpgradeResult.ServerOld;
                        Version ver = new Version(ServerVersion);
                        if (ver.Major == 3 && ver.Minor >= 6)
                            retval = (int)UpgradeResult.Latest;
      break;
    case StatusCodes.OlderVersion:
      retval = (int)UpgradeResult.UpgradeAvailable;
      break;
    case StatusCodes.Unknown:
      retval = (int)UpgradeResult.Unknown;
      break;
    default:
      break;
   }
   serverVersion = ServerVersion;
   return retval;
  }
  public static bool CheckForServerUpdate(string domainID)
  {
   ClientUpgrade cu = new ClientUpgrade(domainID);
   return cu.CheckForServerUpdate();
  }
  public static string CheckForUpdateAvailable(string domainID)
  {
   ClientUpgrade cu = new ClientUpgrade(domainID);
   return cu.CheckForUpdateAvailable();
  }
  public static bool RunUpdate(string domainID, string path)
  {
   ClientUpgrade cu = new ClientUpgrade(domainID);
   return cu.RunUpdate(path);
  }
 }
}
