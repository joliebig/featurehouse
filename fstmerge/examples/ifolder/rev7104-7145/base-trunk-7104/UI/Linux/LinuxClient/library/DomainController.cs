

using System;
using System.Collections;

using Novell.iFolder.Events;

using Simias.Client;
using Simias.Client.Authentication;
using Novell.iFolder.DomainProvider;

namespace Novell.iFolder.Controller
{



 public enum UpgradeResult
 {
  Latest = 0,
  UpgradeNeeded = 1,
  ServerOld = 2,
  UpgradeAvailable = 3,
  Unknown =4,
 };




 public class DomainController
 {
  private static DomainController instance = null;





  public iFolderWebService ifws;




  private SimiasWebService simws;




  private Hashtable keyedDomains;




  private string defDomainID;

  private SimiasEventBroker eventBroker = null;
  private Manager simiasManager;

  public static Status upgradeStatus;




  public event DomainAddedEventHandler DomainAdded;
  public event DomainDeletedEventHandler DomainDeleted;
  public event DomainHostModifiedEventHandler DomainHostModified;
  public event DomainLoggedInEventHandler DomainLoggedIn;
  public event DomainLoggedOutEventHandler DomainLoggedOut;
  public event DomainUpEventHandler DomainUp;
  public event DomainNeedsCredentialsEventHandler DomainNeedsCredentials;
  public event DomainActivatedEventHandler DomainActivated;
  public event DomainInactivatedEventHandler DomainInactivated;
  public event DomainNewDefaultEventHandler NewDefaultDomain;
  public event DomainInGraceLoginPeriodEventHandler DomainInGraceLoginPeriod;
  public event DomainClientUpgradeAvailableEventHandler DomainClientUpgradeAvailable;





  public static iFolderWebService GetiFolderService()
  {
   return instance.ifws;
  }




  private DomainController()
  {
   this.simiasManager = Util.GetSimiasManager();
   string localServiceUrl = simiasManager.WebServiceUri.ToString();
   try
   {
    ifws = new iFolderWebService();
    ifws.Url = localServiceUrl + "/iFolder.asmx";
    LocalService.Start(ifws, simiasManager.WebServiceUri, simiasManager.DataPath);
   }
   catch(Exception)
   {
    ifws = null;
    throw new Exception("Unable to create ifolder web service in DomainController");
   }
   try
   {
    simws = new SimiasWebService();
    simws.Url = localServiceUrl + "/Simias.asmx";
    LocalService.Start(simws, simiasManager.WebServiceUri, simiasManager.DataPath);
   }
   catch(Exception)
   {
    simws = null;
    throw new Exception("Unable to create simias web service in DomainController");
   }

   keyedDomains = new Hashtable();
   defDomainID = "0";

   Refresh();


   eventBroker = SimiasEventBroker.GetSimiasEventBroker();
   if (eventBroker != null)
   {
    eventBroker.DomainUpEventFired +=
     new DomainUpEventHandler(OnDomainUpEvent);
    eventBroker.DomainAdded +=
     new DomainAddedEventHandler(OnDomainAddedEvent);
    eventBroker.DomainDeleted +=
     new DomainDeletedEventHandler(OnDomainDeletedEvent);
   }
  }




  ~DomainController()
  {
   if (eventBroker != null)
   {
    eventBroker.DomainUpEventFired -=
     new DomainUpEventHandler(OnDomainUpEvent);
    eventBroker.DomainAdded -=
     new DomainAddedEventHandler(OnDomainAddedEvent);
    eventBroker.DomainDeleted -=
     new DomainDeletedEventHandler(OnDomainDeletedEvent);
   }
  }





  public static DomainController GetDomainController()
  {
   lock (typeof(DomainController))
   {
    if (instance == null)
    {
     instance = new DomainController();
    }

    return instance;
   }
  }




  public void Refresh()
  {
   lock (typeof(DomainController))
   {

    keyedDomains.Clear();
    DomainInformation[] domains = null;
    try
    {
     domains = simws.GetDomains(false);
    }
    catch(Exception)
    {
     domains = null;
    }

    if(domains != null)
    {
     foreach(DomainInformation domain in domains)
     {
      if(domain.IsDefault)
       defDomainID = domain.ID;

      AddDomainToHashtable(domain);
     }
    }
   }
  }




  public DomainInformation[] GetDomains()
  {
   lock(typeof(DomainController))
   {
    DomainInformation[] domains = new DomainInformation[keyedDomains.Count];

    ICollection icol = keyedDomains.Values;
    icol.CopyTo(domains, 0);

    return domains;
   }
  }





  public DomainInformation[] GetLoggedInDomains()
  {
   Hashtable ht = new Hashtable();
   DomainProviderUI domainProviderUI = DomainProviderUI.GetDomainProviderUI();
   foreach( DomainInformation dom in keyedDomains.Values)
   {
                         IDomainProviderUI provider = domainProviderUI.GetProviderForID(dom.ID);
                         if (provider != null)
                         {
                                 if (dom.Active)
     {
      ht.Add(dom.ID, dom);
     }
                                 else
      continue;
                         }
                         else
                         {
                                 if (dom != null && dom.Authenticated)
     {
      ht.Add(dom.ID, dom);
     }
                                 else
      continue;
                         }
   }
   if( ht.Count > 0)
   {
    DomainInformation[] domains1 = new DomainInformation[ht.Count];
    ICollection icol = ht.Values;
    icol.CopyTo( domains1, 0);
    return domains1;
   }
   else
    return null;
  }




  public DomainInformation GetDefaultDomain()
  {
   lock(typeof(DomainController))
   {
    if (defDomainID != null && keyedDomains.Contains(defDomainID))
     return (DomainInformation)keyedDomains[defDomainID];
    else
    {
     defDomainID = null;
     return null;
    }
   }
  }




  public void SetDefaultDomain(string domainID)
  {
   lock(typeof(DomainController))
   {
    if (defDomainID == null || domainID != defDomainID)
    {
     if (!keyedDomains.Contains(domainID))
     {

      throw new Exception("InvalidDomainException");
     }

     DomainInformation dom = (DomainInformation)keyedDomains[domainID];

     try
     {
      simws.SetDefaultDomain(domainID);
      dom.IsDefault = true;
      string oldDomainID = defDomainID;
      defDomainID = domainID;

      if (oldDomainID != null && keyedDomains.Contains(oldDomainID))
      {
       DomainInformation oldDefaultDomain = (DomainInformation)keyedDomains[oldDomainID];
       oldDefaultDomain.IsDefault = false;
      }

      if (NewDefaultDomain != null)
       NewDefaultDomain(this, new NewDefaultDomainEventArgs(oldDomainID, defDomainID));
     }
     catch (Exception e)
     {
      throw e;
     }
    }
   }
  }




  public DomainInformation GetDomain(string domainID)
  {
   lock(typeof(DomainController))
   {
    if (keyedDomains.Contains(domainID))
     return (DomainInformation)keyedDomains[domainID];
    else
     return null;
   }
  }




         public string[] GetRAList (string domainID)
  {

   Debug.PrintLine("Calling Getralist on client");
   string[] ragents = simws.GetRAListOnClient(domainID);

   if(ragents == null)
   {
       ragents = new string [1];
       ragents[0] = "   ";
   }

      return ragents;
  }





         public bool IsPassPhraseSet (string domainID)
  {
      return simws.IsPassPhraseSet (domainID);
  }




         public Status SetPassPhrase (string domainID, string passPhrase, string recoveryAgent)
  {
      return simws.SetPassPhrase (domainID, passPhrase, "public key", recoveryAgent);
  }




  public bool ReSetPassphrase(string DomainID, string OldPassphrase, string NewPassphrase, string RAName, string RAPublicKey)
  {
   try
   {

    Status status = simws.ReSetPassPhrase (DomainID, OldPassphrase, NewPassphrase, RAName, RAPublicKey);
   if(status.statusCode != StatusCodes.Success)
   {
    return false;
   }
   else
    return true;
   }
   catch(Exception ex)
   {
                                iFolderMsgDialog dialog = new iFolderMsgDialog(
                                                                       null,
                                                                       iFolderMsgDialog.DialogType.Error,
                                                                       iFolderMsgDialog.ButtonSet.None,
                                                                       Util.GS("Unable to reset the passphrase"),
                                                                       Util.GS(ex.Message),
                                                                       Util.GS("Please try again"));
                                dialog.Run();
                                dialog.Hide();
                                dialog.Destroy();
                                dialog = null;
    return false;
   }
  }




         public void StorePassPhrase (string domainID, string passPhrase, CredentialType type, bool persist)
  {
      simws.StorePassPhrase (domainID, passPhrase, type, persist);
  }




         public Status ValidatePassPhrase (string domainID, string passPhrase)
  {
      return simws.ValidatePassPhrase (domainID, passPhrase);
  }




         public byte[] GetRACertificate (string domainID, string recoveryAgent)
  {

   Debug.PrintLine(String.Format("In domain Controller: Recovery Agent is : {0}", recoveryAgent));
   return simws.GetRACertificateOnClient(domainID, recoveryAgent);


  }




  public DomainInformation AddDomain(string host, string username, string password, bool bSavePassword, bool bSetAsDefault)
  {
   DomainInformation dom = null;

   SetHttpProxyForHost(host);

   try
   {
    dom = simws.ConnectToDomain(username, password, host);
    if (dom != null &&
     (dom.StatusCode == StatusCodes.Success ||
      dom.StatusCode == StatusCodes.SuccessInGrace))
    {

     AddDomainToHashtable(dom);

     if (bSetAsDefault)
     {
      try
      {
       simws.SetDefaultDomain(dom.ID);
       dom.IsDefault = true;

       string oldDomainID = null;
       if (defDomainID != null && defDomainID != dom.ID)
       {
        oldDomainID = defDomainID;
        lock (typeof(DomainController))
        {
         DomainInformation oldDefaultDomain = (DomainInformation)keyedDomains[oldDomainID];
         if (oldDefaultDomain != null)
          oldDefaultDomain.IsDefault = false;
        }
       }

       defDomainID = dom.ID;

       if (NewDefaultDomain != null)
        NewDefaultDomain(this, new NewDefaultDomainEventArgs(oldDomainID, defDomainID));
      }
      catch {}
     }


     if (DomainAdded != null)
     {
      DomainAddedIdleHandler addedHandler =
       new DomainAddedIdleHandler(dom.ID, this);
      GLib.Idle.Add(addedHandler.IdleHandler);
     }
    }
   }
   catch (Exception e)
   {
    if (e.Message.IndexOf("Simias.ExistsException") != -1 ||
     e.Message.IndexOf("already exists") != -1)
    {
     throw new DomainAccountAlreadyExistsException("An account with this domain already exists");
    }
    else
    {
     throw e;
    }
   }

   return dom;
  }





  private void EmitDomainAdded(string domainID)
  {
   if (DomainAdded != null)
    DomainAdded(this, new DomainEventArgs(domainID));
  }




  public class DomainAddedIdleHandler
  {
   string domainID;
   DomainController domainController;






   public DomainAddedIdleHandler(string domainID,
             DomainController domainController)
   {
    this.domainID = domainID;
    this.domainController = domainController;
   }





   public bool IdleHandler()
   {
    domainController.EmitDomainAdded(domainID);

    return false;
   }
  }
  public DomainInformation UpdateDomainHostAddress(string domainID, string host, string user, string password)
  {
   lock (typeof(DomainController))
   {
    DomainInformation dom = (DomainInformation)keyedDomains[domainID];
    DomainInformation updatedDomain = null;
    if (dom != null)
    {
     if (String.Compare(dom.Host, host, true) != 0)
     {
      try
      {
       if (simws.SetDomainHostAddress(domainID, host, user, password))
       {
        updatedDomain = simws.GetDomainInformation(domainID);
        updatedDomain.StatusCode = StatusCodes.Success;
        keyedDomains[domainID] = updatedDomain;
        if (DomainHostModified != null)
         DomainHostModified(this, new DomainEventArgs(domainID));
       }
       else
       {
        return null;
       }
      }
      catch (Exception e)
      {
       throw e;
      }
     }
    }
    else
    {
    }
    return updatedDomain;
   }
  }
  public void RemoveDomain(string domainID, bool deleteiFoldersOnServer)
  {
   try
   {
    simws.LeaveDomain(domainID, !deleteiFoldersOnServer);
   }
   catch(Exception e)
   {
    iFolderMsgDialog dg = new iFolderMsgDialog(
     null,
     iFolderMsgDialog.DialogType.Error,
     iFolderMsgDialog.ButtonSet.Ok,
     "",
     Util.GS("Unable to remove the account"),
     Util.GS("iFolder encountered a problem removing the account.  Please restart iFolder and try again."),
     e.Message);
    dg.Run();
    dg.Hide();
    dg.Destroy();
   }
  }
  public Status AuthenticateDomain(string domainID, string password, bool bSavePassword)
  {
   Status status;
   DomainAuthentication domainAuth =
    new DomainAuthentication(
     "iFolder",
     domainID,
     password);
   try
   {
    status = domainAuth.Authenticate(simiasManager.WebServiceUri, simiasManager.DataPath);
    if (status.statusCode == StatusCodes.Success ||
     status.statusCode == StatusCodes.SuccessInGrace)
    {
     if (bSavePassword)
     {
      try
      {
       if (password != null && password.Length > 0)
        simws.SetDomainCredentials(domainID, password, CredentialType.Basic);
       else
        simws.SetDomainCredentials(domainID, null, CredentialType.None);
      }
      catch (Exception)
      {
      }
     }
     status = HandleDomainLoggedIn(domainID, status);
    }
   }
   catch (Exception)
   {
    status = null;
   }
   return status;
  }
  public void LogoutDomain(string domainID)
  {
   DomainAuthentication domainAuth =
    new DomainAuthentication(
     "iFolder",
     domainID,
     null);
   domainAuth.Logout(simiasManager.WebServiceUri, simiasManager.DataPath);
   try
   {
    DomainInformation dom =
     simws.GetDomainInformation(domainID);
    if (dom != null)
    {
     dom.Authenticated = false;
     if (keyedDomains.Contains(dom.ID))
      keyedDomains[dom.ID] = dom;
     else
     {
      AddDomainToHashtable(dom);
      if (DomainAdded != null)
      {
       DomainAddedIdleHandler addedHandler =
        new DomainAddedIdleHandler(domainID, this);
       GLib.Idle.Add(addedHandler.IdleHandler);
      }
     }
    }
   }
   catch{}
   if (DomainLoggedOut != null)
    DomainLoggedOut(this, new DomainEventArgs(domainID));
  }
  public void DisableDomainAutoLogin(string domainID)
  {
   try
   {
    simws.DisableDomainAutoLogin(domainID);
   }
   catch {}
  }
  public string GetDomainPassword(string domainID)
  {
   lock (typeof(DomainController))
   {
    DomainInformation dom = (DomainInformation)keyedDomains[domainID];
    if (dom != null)
    {
     string userID;
     string credentials;
     try
     {
      CredentialType credType = simws.GetDomainCredentials(
       dom.ID, out userID, out credentials);
      if (credentials != null && credType == CredentialType.Basic)
      {
       return credentials;
      }
     }
     catch {}
    }
    return null;
   }
  }
  public void ActivateDomain(string domainID)
  {
   lock (typeof(DomainController))
   {
    try
    {
     DomainInformation dom = (DomainInformation)keyedDomains[domainID];
     if (dom == null)
     {
      throw new Exception("Invalid Domain ID");
     }
     simws.SetDomainActive(domainID);
     dom.Active = true;
     if (DomainActivated != null)
      DomainActivated(this, new DomainEventArgs(domainID));
    }
    catch (Exception e)
    {
     throw e;
    }
   }
  }
  public void InactivateDomain(string domainID)
  {
   lock (typeof(DomainController))
   {
    try
    {
     DomainInformation dom = (DomainInformation)keyedDomains[domainID];
     if (dom == null)
     {
      throw new Exception("Invalid Domain ID");
     }
     simws.SetDomainInactive(domainID);
     dom.Active = false;
     if (DomainInactivated != null)
      DomainInactivated(this, new DomainEventArgs(domainID));
    }
    catch (Exception e)
    {
     throw e;
    }
   }
  }
  public void ClearDomainPassword(string domainID)
  {
   try
   {
    simws.SetDomainCredentials(domainID, null, CredentialType.None);
   }
   catch {}
  }
  public void SetDomainPassword(string domainID, string password)
  {
   try
   {
    simws.SetDomainCredentials(domainID, password, CredentialType.Basic);
   }
   catch {}
  }
  public DomainInformation GetPOBoxDomain(string poBoxID)
  {
   return null;
  }
  private void AddDomainToHashtable(DomainInformation newDomain)
  {
   lock (typeof(DomainController) )
   {
    if(newDomain != null)
    {
     keyedDomains[newDomain.ID] = newDomain;
    }
   }
  }
  private void RemoveDomainFromHashtable(string domainID)
  {
   lock (typeof(DomainController) )
   {
    if(keyedDomains.ContainsKey(domainID))
    {
     DomainInformation dom = (DomainInformation)keyedDomains[domainID];
     keyedDomains.Remove(domainID);
     if (dom.IsDefault)
     {
      try
      {
       string newDefaultDomainID = simws.GetDefaultDomainID();
       if (newDefaultDomainID != null)
       {
        if (keyedDomains.ContainsKey(newDefaultDomainID))
        {
         DomainInformation newDefaultDomain =
          (DomainInformation)keyedDomains[newDefaultDomainID];
         newDefaultDomain.IsDefault = true;
         string oldDomainID = null;
         if (defDomainID != null)
         {
          DomainInformation oldDefaultDomain = (DomainInformation)keyedDomains[defDomainID];
          if (oldDefaultDomain != null)
          {
           oldDefaultDomain.IsDefault = false;
           oldDomainID = defDomainID;
          }
         }
         defDomainID = newDefaultDomain.ID;
         if (NewDefaultDomain != null)
          NewDefaultDomain(this, new NewDefaultDomainEventArgs(oldDomainID, newDefaultDomainID));
        }
       }
       else
        defDomainID = null;
      }
      catch {}
     }
     else
     {
     }
    }
   }
  }
  private void OnDomainUpEvent(object o, DomainEventArgs args)
  {
   if (DomainUp != null)
    DomainUp(this, args);
   DomainLoginThread domainLoginThread =
    new DomainLoginThread(this);
   domainLoginThread.Completed +=
    new DomainLoginCompletedHandler(OnDomainLoginCompleted);
   domainLoginThread.Login(args.DomainID);
  }
        private void OnDomainLoginCompleted(object o, DomainLoginCompletedArgs args)
  {
   string domainID = args.DomainID;
   Status authStatus = args.AuthenticationStatus;
   if (authStatus != null &&
    ((authStatus.statusCode == StatusCodes.Success) ||
     (authStatus.statusCode == StatusCodes.SuccessInGrace)))
   {
    authStatus = HandleDomainLoggedIn(domainID, authStatus);
   }
   else
   {
    if (DomainNeedsCredentials != null)
     DomainNeedsCredentials(this, new DomainEventArgs(domainID));
   }
  }
  private Status HandleDomainLoggedIn(string domainID, Status status)
  {
   DomainController.upgradeStatus = status;
   Debug.PrintLine("In handledoaminloggedin");
   try
   {
    DomainInformation dom =
     simws.GetDomainInformation(domainID);
    if (dom != null)
    {
     dom.Authenticated = true;
     if (keyedDomains.Contains(dom.ID))
      keyedDomains[dom.ID] = dom;
     else
     {
      AddDomainToHashtable(dom);
      if (DomainAdded != null)
      {
       DomainAddedIdleHandler addedHandler =
        new DomainAddedIdleHandler(domainID, this);
       GLib.Idle.Add(addedHandler.IdleHandler);
      }
     }
    }
   }
   catch{}
   if (DomainLoggedIn != null)
    DomainLoggedIn(this, new DomainEventArgs(domainID));
   if (status.statusCode == StatusCodes.SuccessInGrace)
   {
    if (status.RemainingGraceLogins < status.TotalGraceLogins)
    {
     if (DomainInGraceLoginPeriod != null)
     {
      DomainInGraceLoginPeriodEventArgs graceEventArgs =
       new DomainInGraceLoginPeriodEventArgs(
        domainID,
        status.RemainingGraceLogins);
      DomainInGraceLoginPeriod(this, graceEventArgs);
     }
    }
   }
   CheckForUpdate(domainID);
   return DomainController.upgradeStatus;
  }
  private void CheckForUpdate( string domainID )
  {
   string serverVersion = null;
   UpgradeResult status = UpgradeResult.Unknown;
   try
   {
    status = (UpgradeResult)this.ifws.CheckForUpdate(domainID, out serverVersion);
   }
   catch(Exception ex)
   {
    Debug.PrintLine(String.Format("Exception: {0}", ex.Message));
    status = UpgradeResult.Latest;
   }
   DomainClientUpgradeAvailableEventArgs args;
   switch( status)
   {
    case UpgradeResult.Latest:
      break;
    case UpgradeResult.UpgradeNeeded:
      DomainController.upgradeStatus.statusCode = StatusCodes.UpgradeNeeded;
      args =
        new DomainClientUpgradeAvailableEventArgs(
         domainID, serverVersion);
     DomainClientUpgradeAvailable(this, args);
      break;
    case UpgradeResult.ServerOld:
      DomainController.upgradeStatus.statusCode = StatusCodes.ServerOld;
      args =
        new DomainClientUpgradeAvailableEventArgs(
         domainID, serverVersion);
     DomainClientUpgradeAvailable(this, args);
      break;
    case UpgradeResult.UpgradeAvailable:
      args =
        new DomainClientUpgradeAvailableEventArgs(
         domainID, serverVersion);
     DomainClientUpgradeAvailable(this, args);
      break;
    case UpgradeResult.Unknown:
      DomainController.upgradeStatus.statusCode = StatusCodes.Unknown;
      break;
    default:
      break;
   }
  }
  private bool IsServerOld(string domainID)
  {
   return ifws.CheckForServerUpdate(domainID);
  }
  private string GetNewClientAvailable(string domainID)
  {
   string AvailableClientVersion = null;
   try
   {
    AvailableClientVersion = ifws.CheckForUpdatedClientAvailable(domainID);
   }
   catch
   {
   }
   return AvailableClientVersion;
  }
  private string GetNewClientVersion(string domainID)
  {
   string newClientVersion = null;
   try
   {
    newClientVersion = ifws.CheckForUpdatedClient(domainID);
   }
   catch(Exception e)
   {
    string domainName = domainID;
    DomainInformation dom = (DomainInformation) keyedDomains[domainID];
    if (dom != null)
     domainName = dom.Name;
    Debug.PrintLine(String.Format("Error checking for new version of iFolder Client on {0}: {1}", domainName, e.Message));
   }
   return newClientVersion;
  }
  public Status AuthenticateDomain(string domainID)
  {
   DomainAuthentication domainAuth =
    new DomainAuthentication(
     "iFolder",
     domainID,
     null);
   try
   {
    return domainAuth.Authenticate(simiasManager.WebServiceUri, simiasManager.DataPath);
   }
   catch {}
   return null;
  }
  public Status AuthenticateDomainWithProxy(string domainID)
  {
   string userID;
   string credentials;
   DomainInformation dom = (DomainInformation)keyedDomains[domainID];
   if (dom == null)
    return null;
   try
   {
    SetHttpProxyForHost(dom.Host);
    CredentialType credentialType =
     simws.GetDomainCredentials(
      domainID,
      out userID,
      out credentials);
    if ((credentialType == CredentialType.Basic) &&
     (credentials != null))
    {
     DomainAuthentication domainAuth =
      new DomainAuthentication(
       "iFolder",
       domainID,
       credentials);
     Status status = domainAuth.Authenticate(simiasManager.WebServiceUri, simiasManager.DataPath);
     if (status.statusCode == StatusCodes.InvalidCredentials)
     {
      simws.SetDomainCredentials(domainID, null, CredentialType.None);
     }
     return status;
    }
   }
   catch {}
   return null;
  }
  private void SetHttpProxyForHost(string host)
  {
   GnomeHttpProxy proxy = new GnomeHttpProxy(host);
   string user = null;
   string password = null;
   if (proxy.IsProxySet)
   {
    if (proxy.CredentialsSet)
    {
     user = proxy.Username;
     password = proxy.Password;
    }
    simws.SetProxyAddress(
     "http://" + host,
     "http://" + proxy.Host,
     user,
     password);
    if (!proxy.IsSecureProxySet)
    {
     simws.SetProxyAddress(
      "https://" + host,
      "http://" + proxy.Host,
      user,
      password);
    }
   }
   if (proxy.IsSecureProxySet)
   {
    simws.SetProxyAddress(
     "https://" + host,
     "http://" + proxy.SecureHost,
     user,
     password);
   }
  }
  [GLib.ConnectBefore]
  private void OnDomainAddedEvent(object o, DomainEventArgs args)
  {
   lock (typeof(DomainController) )
   {
    if (args == null || args.DomainID == null)
     return;
    DomainInformation domain = (DomainInformation)keyedDomains[args.DomainID];
    if (domain != null)
    {
     return;
    }
    try
    {
     domain = simws.GetDomainInformation(args.DomainID);
    }
    catch (Exception)
    {
     Debug.PrintLine("Ramesh: Got an exception");
     return;
    }
    if( domain == null)
    {
     Debug.PrintLine("Ramesh: Domain is null");
    }
    AddDomainToHashtable(domain);
    if (DomainAdded != null)
    {
     DomainAddedIdleHandler addedHandler =
      new DomainAddedIdleHandler(args.DomainID, this);
     GLib.Idle.Add(addedHandler.IdleHandler);
    }
   }
  }
  [GLib.ConnectBefore]
  private void OnDomainDeletedEvent(object o, DomainEventArgs args)
  {
   lock (typeof(DomainController) )
   {
    DomainInformation domain = (DomainInformation)keyedDomains[args.DomainID];
    if (domain == null)
    {
     return;
    }
    RemoveDomainFromHashtable(args.DomainID);
    if (DomainDeleted != null)
     DomainDeleted(this, args);
   }
  }
  public string GetRAName(string domainID)
  {
   return this.ifws.GetRAName(domainID);
  }
 }
 public class DomainAccountAlreadyExistsException : Exception
 {
  public DomainAccountAlreadyExistsException() : base()
  {
  }
  public DomainAccountAlreadyExistsException(string message) : base(message)
  {
  }
 }
 public class AddDomainThread
 {
  private DomainController domainController;
  private string serverName;
  private string userName;
  private string password;
  private bool bRememberPassword;
  private bool bSetAsDefault;
  private DomainInformation domain;
  private Exception e;
  public string ServerName
  {
   get
   {
    return serverName;
   }
  }
  public string Password
  {
   get
   {
    return password;
   }
  }
  public bool RememberPassword
  {
   get
   {
    return bRememberPassword;
   }
  }
  public DomainInformation Domain
  {
   get
   {
    return domain;
   }
  }
  public Exception Exception
  {
   get
   {
    return e;
   }
  }
  public event EventHandler Completed;
  public AddDomainThread(
     DomainController domainController,
     string serverName,
     string userName,
     string password,
     bool bRememberPassword,
     bool bSetAsDefault)
  {
   this.domainController = domainController;
   this.serverName = serverName;
   this.userName = userName;
   this.password = password;
   this.bRememberPassword = bRememberPassword;
   this.bSetAsDefault = bSetAsDefault;
   this.domain = null;
   this.e = null;
  }
  public void AddDomain()
  {
   System.Threading.Thread thread =
    new System.Threading.Thread(
     new System.Threading.ThreadStart(AddThread));
   thread.Start();
  }
        public void AddDomainSerial()
        {
   try
   {
    domain = domainController.AddDomain(
     serverName,
     userName,
     password,
     bRememberPassword,
     bSetAsDefault);
   }
   catch (Exception e)
   {
    this.e = e;
   }
        }
        private void AddThread()
  {
   try
   {
    domain = domainController.AddDomain(
     serverName,
     userName,
     password,
     bRememberPassword,
     bSetAsDefault);
   }
   catch (Exception e)
   {
    this.e = e;
   }
   if (Completed != null)
   {
    AddDomainCompletedHandler completedHandler =
     new AddDomainCompletedHandler(this);
    GLib.Idle.Add(completedHandler.IdleHandler);
   }
  }
  private void AddCompleted()
  {
   if (Completed != null)
    Completed(this, EventArgs.Empty);
  }
  private class AddDomainCompletedHandler
  {
   public AddDomainThread thread;
   public AddDomainCompletedHandler(AddDomainThread thread)
   {
    this.thread = thread;
   }
   public bool IdleHandler()
   {
    thread.AddCompleted();
    return false;
   }
  }
 }
 public delegate void DomainLoginCompletedHandler(object sender, DomainLoginCompletedArgs args);
    public class DomainLoginThread
 {
  private DomainController domainController;
  private string domainID;
  private string password;
  private bool bSavePassword;
  private Status authStatus;
  public Status AuthenticationStatus
  {
   get
   {
    return authStatus;
   }
  }
  public string DomainID
  {
   get
   {
    return domainID;
   }
  }
  public event DomainLoginCompletedHandler Completed;
  public DomainLoginThread(DomainController domainController)
  {
   this.domainController = domainController;
   this.domainID = null;
   this.password = null;
   this.bSavePassword = false;
   this.authStatus = null;
  }
  public void Login(string domainID)
  {
   this.domainID = domainID;
   System.Threading.Thread thread =
    new System.Threading.Thread(
     new System.Threading.ThreadStart(LoginThread));
   thread.Start();
  }
  public void Login(string domainID, string password, bool bSavePassword)
  {
   this.domainID = domainID;
   this.password = password;
   this.bSavePassword = bSavePassword;
   System.Threading.Thread thread =
    new System.Threading.Thread(
     new System.Threading.ThreadStart(LoginThreadWithArgs));
   thread.Start();
  }
  private void LoginThread()
  {
   try
   {
    authStatus = domainController.AuthenticateDomain(domainID);
    if (authStatus == null ||
     ((authStatus.statusCode != StatusCodes.Success) &&
      (authStatus.statusCode != StatusCodes.SuccessInGrace)))
    {
     authStatus =
      domainController.AuthenticateDomainWithProxy(
       domainID);
    }
   }
   catch(Exception e)
   {
    Debug.PrintLine(String.Format("Exception attempting a login: {0}", e.Message));
    authStatus = new Status();
   }
   if (Completed != null)
   {
    LoginThreadCompletedHandler completedHandler =
     new LoginThreadCompletedHandler(this);
    GLib.Idle.Add(completedHandler.IdleHandler);
   }
  }
  private void LoginThreadWithArgs()
  {
   try
   {
    authStatus = domainController.AuthenticateDomain(
     domainID, password, bSavePassword);
   }
   catch(Exception e)
   {
    Debug.PrintLine(String.Format("Exception attempting a login: {0}", e.Message));
    authStatus = new Status();
   }
   if (Completed != null)
   {
    LoginThreadCompletedHandler completedHandler =
     new LoginThreadCompletedHandler(this);
    GLib.Idle.Add(completedHandler.IdleHandler);
   }
  }
  private void LoginCompleted()
  {
   if (Completed != null)
    Completed(this, new DomainLoginCompletedArgs(domainID, authStatus));
  }
  private class LoginThreadCompletedHandler
  {
   public DomainLoginThread thread;
   public LoginThreadCompletedHandler(DomainLoginThread thread)
   {
    this.thread = thread;
   }
   public bool IdleHandler()
   {
    thread.LoginCompleted();
    return false;
   }
  }
 }
 public class DomainLoginCompletedArgs : EventArgs
 {
  private string domainID;
  private Status authStatus;
  public string DomainID
  {
   get{ return domainID; }
  }
  public Status AuthenticationStatus
  {
   get{ return authStatus; }
  }
  public DomainLoginCompletedArgs(string domainID, Status authStatus)
  {
   this.domainID = domainID;
   this.authStatus = authStatus;
  }
 }
}
