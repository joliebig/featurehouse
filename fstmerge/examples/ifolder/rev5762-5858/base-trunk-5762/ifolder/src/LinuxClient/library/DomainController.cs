

using System;
using System.Collections;

using Novell.iFolder.Events;

using Simias.Client;
using Simias.Client.Authentication;

namespace Novell.iFolder.Controller
{
 public class DomainController
 {
  private static DomainController instance = null;




  private iFolderWebService ifws;





  private SimiasWebService simws;




  private Hashtable keyedDomains;




  private string defDomainID;

  private SimiasEventBroker eventBroker = null;
  private Manager simiasManager;




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

  private DomainController()
  {
Console.WriteLine("=====> DomainController being constructed! <=====");
Console.WriteLine("=====> DomainController HashCode: {0} <=====", this.GetHashCode());
Console.WriteLine("Current Thread: {0}", System.Threading.Thread.CurrentThread.GetHashCode());
Console.WriteLine("Stack Trace:");
Console.WriteLine(Environment.StackTrace);
   this.simiasManager = Util.GetSimiasManager();
   string localServiceUrl = simiasManager.WebServiceUri.ToString();
   try
   {
    ifws = new iFolderWebService();
    ifws.Url = localServiceUrl + "/iFolder.asmx";
    LocalService.Start(ifws, simiasManager.WebServiceUri, simiasManager.DataPath);
   }
   catch(Exception e)
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
   catch(Exception e)
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
    catch(Exception e)
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




  public DomainInformation GetDefaultDomain()
  {
   lock(typeof(DomainController))
   {
    if (keyedDomains.Contains(defDomainID))
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
Console.WriteLine("DomainController.GetDomain(\"{0}\")", domainID);
   lock(typeof(DomainController))
   {
    if (keyedDomains.Contains(domainID))
     return (DomainInformation)keyedDomains[domainID];
    else
     return null;
   }
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
      DomainAdded(this, new DomainEventArgs(dom.ID));
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

  public DomainInformation UpdateDomainHostAddress(string domainID, string host)
  {
   lock (typeof(DomainController))
   {
    DomainInformation dom = (DomainInformation)keyedDomains[domainID];
    if (dom != null)
    {
     if (String.Compare(dom.Host, host, true) != 0)
     {
      try
      {
       simws.SetDomainHostAddress(domainID, host);

       dom = simws.GetDomainInformation(domainID);
       keyedDomains[domainID] = dom;


       if (DomainHostModified != null)
        DomainHostModified(this, new DomainEventArgs(domainID));
      }
      catch (Exception e)
      {

       throw e;
      }
     }

     dom.StatusCode = StatusCodes.Success;
    }
    else
    {

    }

    return dom;
   }
  }




  public void RemoveDomain(string domainID, bool deleteiFoldersOnServer)
  {
   simws.LeaveDomain(domainID, !deleteiFoldersOnServer);
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
      catch (Exception e)
      {
       Console.WriteLine("Error saving the password: {0}", e.Message);
      }
     }

     HandleDomainLoggedIn(domainID, status);
    }
   }
   catch (Exception e)
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
       DomainAdded(this, new DomainEventArgs(domainID));
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
   lock(typeof(DomainController))
   {
    DomainInformation[] domains = this.GetDomains();
    foreach (DomainInformation domain in domains)
    {
     if(domain.POBoxID.Equals(poBoxID))
      return domain;
    }

    return null;
   }
  }

  public void CheckForNewiFolders()
  {


   foreach(DomainInformation domain in keyedDomains.Values)
   {
    try
    {
     ifws.SynciFolderNow(domain.POBoxID);
    }
    catch
    {
    }
   }
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
Console.WriteLine("DomainController.RemoveDomainFromHashtable({0})", domainID);
   lock (typeof(DomainController) )
   {
    if(keyedDomains.ContainsKey(domainID))
    {
     DomainInformation dom = (DomainInformation)keyedDomains[domainID];
     keyedDomains.Remove(domainID);

Console.WriteLine("\tJust removed the domain");



     if (dom.IsDefault)
     {
Console.WriteLine("\tthe removed domain was the default...setting new default");
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
Console.WriteLine("\tthe removed domain was NOT the default");
     }
    }
   }
  }




  private void OnDomainUpEvent(object o, DomainEventArgs args)
  {

   if (DomainUp != null)
    DomainUp(this, args);

   Status authenticationStatus = AuthenticateDomain(args.DomainID);

   if (authenticationStatus == null ||
    ((authenticationStatus.statusCode != StatusCodes.Success) &&
    (authenticationStatus.statusCode != StatusCodes.SuccessInGrace)))
   {


    authenticationStatus = AuthenticateDomainWithProxy(args.DomainID);
   }

   if (authenticationStatus != null &&
    ((authenticationStatus.statusCode == StatusCodes.Success) ||
    (authenticationStatus.statusCode == StatusCodes.SuccessInGrace)))
   {
    HandleDomainLoggedIn(args.DomainID, authenticationStatus);
   }
   else
   {

    if (DomainNeedsCredentials != null)
     DomainNeedsCredentials(this, args);
   }
  }

  private void HandleDomainLoggedIn(string domainID, Status status)
  {

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
       DomainAdded(this, new DomainEventArgs(domainID));
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
  }

  private Status AuthenticateDomain(string domainID)
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

  private Status AuthenticateDomainWithProxy(string domainID)
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
Console.WriteLine("DomainController.OnDomainAddedEvent()");
   DomainInformation domain = (DomainInformation)keyedDomains[args.DomainID];
   if (domain != null)
   {


    return;
   }

   try
   {
    domain = simws.GetDomainInformation(args.DomainID);
   }
   catch (Exception e)
   {


    return;
   }

   AddDomainToHashtable(domain);


   if (DomainAdded != null)
    DomainAdded(this, args);
  }

  [GLib.ConnectBefore]
  private void OnDomainDeletedEvent(object o, DomainEventArgs args)
  {
Console.WriteLine("DomainController.OnDomainDeletedEvent()");
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

  public event EventHandler Completed;

  public DomainLoginThread(DomainController domainController,
         string domainID,
         string password,
         bool bSavePassword)
  {
   this.domainController = domainController;
   this.domainID = domainID;
   this.password = password;
   this.bSavePassword = bSavePassword;

   this.authStatus = null;
  }

  public void Login()
  {
Console.WriteLine("DomainController.Login()");
   System.Threading.Thread thread =
    new System.Threading.Thread(
     new System.Threading.ThreadStart(LoginThread));
   thread.Start();
  }

  private void LoginThread()
  {
Console.WriteLine("DomainController.LoginThread()");
   try
   {
Console.WriteLine("FIXME: Remove this temporary delay");
System.Threading.Thread.Sleep(10000);
    authStatus = domainController.AuthenticateDomain(
     domainID, password, bSavePassword);
Console.WriteLine("\tDone logging in");
   }
   catch(Exception e)
   {
Console.WriteLine("\tException logging in: {0}", e.Message);

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
Console.WriteLine("DomainController.LoginCompleted()");
   if (Completed != null)
    Completed(this, EventArgs.Empty);
  }

  private class LoginThreadCompletedHandler
  {
   public DomainLoginThread thread;

   public LoginThreadCompletedHandler(DomainLoginThread thread)
   {
Console.WriteLine("LoginThreadCompletedHandler()");
    this.thread = thread;
   }

   public bool IdleHandler()
   {
Console.WriteLine("LoginThreadCompletedHandler.IdleHandler()");
    thread.LoginCompleted();

    return false;
   }
  }
 }
}
