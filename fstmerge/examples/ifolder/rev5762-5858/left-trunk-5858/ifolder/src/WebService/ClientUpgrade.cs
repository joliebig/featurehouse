
using System;
using System.Collections.Specialized;
using System.Diagnostics;
using System.IO;
using System.Net;

using Simias.Client;
using Simias;

namespace Novell.iFolder.Install
{



 public class ClientUpgrade
 {




  private static string iFolderUpdateDirectory = "ead51d60-cd98-4d35-8c7c-b43a2ca949c8";




  private static string iFolderWindowsApplication = "iFolderApp.exe";




  private static string PlatformQuery = "Platform";
  private static string FileQuery = "File";




  private ClientUpdate service = null;




  private string hostAddress;
  private ClientUpgrade( string domainID )
  {
   WebState ws = new WebState(domainID, domainID);
   hostAddress = DomainProvider.ResolveLocation( domainID ).ToString();
   service = new ClientUpdate();
   service.Url = hostAddress + "/ClientUpdate.asmx";
   ws.InitializeWebClient(service, domainID);
  }
  private string DownloadFiles( string[] fileList )
  {
   string downloadDir = Path.Combine( Path.GetTempPath(), iFolderUpdateDirectory );
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
     nvc.Add( PlatformQuery, MyEnvironment.Platform.ToString() );
     nvc.Add( FileQuery, file );
     webClient.QueryString = nvc;
     webClient.DownloadFile( hostAddress + "/ClientUpdateHandler.ashx", Path.Combine( downloadDir, file ) );
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
   string fullPath = Path.Combine( SimiasSetup.bindir, iFolderWindowsApplication );
   if ( File.Exists( fullPath ) )
   {
    FileVersionInfo versionInfo = FileVersionInfo.GetVersionInfo( fullPath );
    version = versionInfo.ProductVersion;
   }
   return version;
  }
  private string CheckForUpdate()
  {
   string updateVersion = null;
   if ( service != null )
   {
    string currentVersion = null;
    if ( MyEnvironment.Platform == MyPlatformID.Windows )
    {
     currentVersion = GetWindowsClientVersion();
    }
    else
    {
    }
    if ( currentVersion != null )
    {
     updateVersion = service.IsUpdateAvailable( MyEnvironment.Platform.ToString(), currentVersion );
    }
   }
   return updateVersion;
  }
  private bool RunUpdate()
  {
   bool running = false;
   if ( service != null )
   {
    string[] fileList = service.GetUpdateFiles();
    if ( fileList != null )
    {
     string downloadDir = DownloadFiles( fileList );
     if ( downloadDir != null )
     {
      if ( MyEnvironment.Platform == MyPlatformID.Windows )
      {
       Process installProcess = new Process();
       installProcess.StartInfo.FileName = Path.Combine( downloadDir, Path.GetFileName( fileList[ 0 ] ) );
       installProcess.StartInfo.UseShellExecute = true;
       installProcess.StartInfo.CreateNoWindow = false;
       running = installProcess.Start();
      }
      else
      {
      }
     }
    }
   }
   return running;
  }
  public static string CheckForUpdate(string domainID)
  {
   ClientUpgrade cu = new ClientUpgrade(domainID);
   return cu.CheckForUpdate();
  }
  public static bool RunUpdate(string domainID)
  {
   ClientUpgrade cu = new ClientUpgrade(domainID);
   return cu.RunUpdate();
  }
 }
}
