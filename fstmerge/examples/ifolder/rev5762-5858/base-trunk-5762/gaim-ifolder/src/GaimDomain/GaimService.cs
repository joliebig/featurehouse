

using System;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;

using Simias;
using Simias.Authentication;
using Simias.Event;
using Simias.POBox;
using Simias.Service;
using Simias.Storage;

using Novell.Security.ClientPasswordManager;

namespace Simias.Gaim
{



 public class GaimService : IThreadService
 {




  private static readonly ISimiasLog log =
   SimiasLogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

  private static string inCredentialEvent = "true";
  private Store store = null;

  private static Simias.Gaim.GaimDomainProvider gaimDomainProvider = null;




  private Configuration config;






  public GaimService()
  {
  }
  internal static void RegisterDomainProvider()
  {
   log.Debug("RegisterDomainProvider called");
   gaimDomainProvider = new Simias.Gaim.GaimDomainProvider();
   Simias.DomainProvider.RegisterProvider(gaimDomainProvider);
   new NetCredential("iFolder", Simias.Gaim.GaimDomain.ID, true, "gaim-user", "joulupukki");
  }
  public void Start( Configuration config )
  {
   log.Debug("Start called");
   this.config = config;
   store = Store.GetStore();
   try
   {
    Simias.Storage.Domain domain = GaimDomain.GetDomain();
    if (domain != null)
    {
     RegisterDomainProvider();
    }
    Simias.Gaim.Sync.StartSyncThread();
    Simias.Authentication.NeedCredentialsEventSubscriber needCreds =
     new NeedCredentialsEventSubscriber();
    needCreds.NeedCredentials +=
     new Simias.Authentication.NeedCredentialsEventHandler(OnCredentialsEventHandler);
   }
   catch(Exception e)
   {
    log.Error(e.Message);
    log.Error(e.StackTrace);
   }
  }
  public void Resume()
  {
  }
  public void Pause()
  {
  }
  public int Custom(int message, string data)
  {
   return 0;
  }
  public void Stop()
  {
   log.Debug("Stop called");
   if (gaimDomainProvider != null)
   {
    Simias.DomainProvider.Unregister(gaimDomainProvider);
   }
   Simias.Gaim.Sync.StopSyncThread();
  }
  public static void OnCredentialsEventHandler(
   Simias.Client.Event.NeedCredentialsEventArgs args)
  {
   if ( args.DomainID == Simias.Gaim.GaimDomain.ID )
   {
    Simias.Authentication.Status authStatus;
    lock (inCredentialEvent)
    {
     ClientAuthentication clientAuth =
      new ClientAuthentication();
     authStatus = clientAuth.Authenticate(args.CollectionID);
     if (authStatus.statusCode == Simias.Authentication.StatusCodes.Success)
     {
      string userID = Store.GetStore().GetUserIDFromDomainID(GaimDomain.ID);
      new NetCredential(
       "iFolder",
       args.CollectionID,
       true,
       userID,
       "@GaimDomainPPK@" );
     }
    }
   }
  }
 }
 public class Sync
 {
  private static readonly ISimiasLog log =
   SimiasLogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);
  static AutoResetEvent syncEvent = null;
  static bool exiting;
  static bool syncOnStart = true;
  static int syncInterval = 60 * 1000;
  static Thread syncThread = null;
  internal static int StartSyncThread()
  {
   log.Debug("StartSyncThread called");
   int status = 0;
   try
   {
    exiting = false;
    syncEvent = new AutoResetEvent(false);
    syncThread = new Thread(new ThreadStart(Sync.SyncThread));
    syncThread.IsBackground = true;
    syncThread.Start();
   }
   catch(SimiasException e)
   {
    log.Debug( e.Message );
    log.Debug( e.StackTrace );
    status = -1;
   }
   log.Debug("StartSyncThread finished");
   return status;
  }
  internal static int StopSyncThread()
  {
   int status = 0;
   log.Debug("StopSyncThread called");
   exiting = true;
   try
   {
    syncEvent.Set();
    Thread.Sleep(32);
    syncEvent.Close();
    Thread.Sleep(0);
    log.Debug("StopSyncThread finished");
   }
   catch(Exception e)
   {
    log.Debug("StopSyncThread failed with an exception");
    log.Debug(e.Message);
    status = -1;
   }
   return status;
  }
  public static int SyncNow(string data)
  {
   log.Debug("SyncNow called");
   syncEvent.Set();
   log.Debug("SyncNow finished");
   return(0);
  }
  public static void UpdateSyncInterval(int newSyncInterval)
  {
   int syncIntervalInMillis = newSyncInterval * 60 * 1000;
   if (syncIntervalInMillis != syncInterval)
   {
    syncInterval = syncIntervalInMillis;
   }
  }
  internal static void SyncThread()
  {
   while (!exiting)
   {
    if (syncOnStart == false)
    {
     syncEvent.WaitOne(syncInterval, false);
    }
    syncOnStart = false;
    GaimDomain.UpdatePreferences();
    GaimDomain.SynchronizeMembers();
   }
  }
 }
}
