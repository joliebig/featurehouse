

using System;
using System.Collections;
using System.IO;
using System.Security.Cryptography;
using System.Threading;
using System.Xml;

using Simias;
using Simias.Storage;
using Simias.Sync;
using Simias.Client;
using Simias.Web;

namespace Simias.Gaim
{



 public class GaimDomain
 {





  public static readonly string ID = "4a9ff9d6-8139-11d9-960e-000d936ac9c4";




  private string domainName = "Gaim Buddy List";
  private string hostAddress;
  private string description = "";
  private string hostName;
  private string userName;
  private string aliasName;
  private string userID;
  private string poBoxID;
  private static bool bPruneOldMembers = false;




  private static readonly ISimiasLog log =
   SimiasLogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);
  public string Name
  {
   get { return(this.domainName); }
  }
  public string Description
  {
   get { return(this.description); }
  }
  public string Host
  {
   get { return(this.hostName); }
  }
  public string User
  {
   get { return( this.userName ); }
  }
  public string Alias
  {
   get { return( this.aliasName ); }
  }
  private GaimDomain( bool init, string username, string alias )
  {
   hostName = Environment.MachineName.ToLower();
   userName = username;
   aliasName = alias;
   if (alias != null && alias.Length > 0)
   {
    domainName = "Gaim Buddy List (" + alias + "@" + hostName + ")";
    description = "Workgroup Domain built from " + alias + "'s Gaim Buddy List";
   }
   else
   {
    domainName = "Gaim Buddy List (" + username + "@" + hostName + ")";
    description = "Workgroup Domain built from " + username + "'s Gaim Buddy List";
   }
   userName = username + " (" + hostName + ")";
   if ( init == true )
   {
    this.Init();
   }
  }
  private void Init()
  {
   hostAddress = MyDns.GetHostName();
   Store store = Store.GetStore();
   try
   {
    Uri localUri = Manager.LocalServiceUrl;
    Simias.Storage.Domain rDomain = store.GetDomain( ID );
    if (rDomain == null)
    {
     rDomain =
      new Simias.Storage.Domain(
       store,
       this.domainName,
       Simias.Gaim.GaimDomain.ID,
       this.description,
       Simias.Sync.SyncRoles.Master,
       Simias.Storage.Domain.ConfigurationType.Workgroup );
     rDomain.SetType( rDomain, "Workgroup" );
     string storedUserID = GetGaimUserID();
     if (storedUserID != null)
     {
      log.Debug("Creating the Gaim Domain with a saved UserID");
      userID = storedUserID;
     }
     else
     {
      log.Debug("Creating the Gaim Domain with a NEW UserID");
      userID = Guid.NewGuid().ToString();
     }
     Member member =
      new Member(
       userName,
       userID,
       Access.Rights.Admin );
     member.IsOwner = true;
     if (localUri == null)
     {
      localUri = Manager.LocalServiceUrl;
      if (localUri != null)
      {
       Simias.Storage.Property p = new Property("Gaim:SimiasURL", localUri.ToString());
       p.LocalProperty = true;
       member.Properties.AddProperty(p);
      }
      else
      {
       log.Debug("Manager.LocalServiceUrl returned NULL!");
      }
     }
     if (aliasName != null && aliasName.Length > 0)
     {
      Simias.Storage.Property p = new Property("Gaim:Alias", aliasName);
      p.LocalProperty = true;
      member.FN = string.Format("{0} ({1})", aliasName, hostName);
     }
     rDomain.Commit( new Node[] { rDomain, member } );
     store.AddDomainIdentity( rDomain.ID, member.UserID );
     GaimService.RegisterDomainProvider();
    }
    Member pMember;
    Simias.POBox.POBox poBox = null;
    string poBoxName = "POBox:" + Simias.Gaim.GaimDomain.ID + ":" + userID;
    try
    {
     poBox = Simias.POBox.POBox.FindPOBox( store, Simias.Gaim.GaimDomain.ID, userID );
    }
    catch{}
    if (poBox == null)
    {
     poBox = new Simias.POBox.POBox( store, poBoxName, ID );
     pMember =
      new Member( userName, userID, Access.Rights.ReadWrite );
     pMember.IsOwner = true;
     poBox.Commit(new Node[] { poBox, pMember });
    }
    else
    {
     pMember = poBox.GetMemberByID( userID );
     if (pMember == null)
     {
      pMember =
       new Member( userName, userID, Access.Rights.ReadWrite );
      pMember.IsOwner = true;
      poBox.Commit(new Node[] { pMember });
     }
    }
    poBoxID = poBox.ID;
   }
   catch( Exception e1 )
   {
    log.Error(e1.Message);
    log.Error(e1.StackTrace);
    throw e1;
   }
  }
  public bool Exists()
  {
   bool exists = false;
   Simias.Storage.Domain gaimDomain = null;
   try
   {
    Store store = Store.GetStore();
    gaimDomain = store.GetDomain( ID );
    if ( gaimDomain != null )
    {
     userID = gaimDomain.GetMemberByName( userName ).ID;
     Simias.POBox.POBox pobox =
      Simias.POBox.POBox.FindPOBox( store, ID, userID );
     poBoxID = pobox.ID;
     exists = true;
    }
   }
   catch{}
   return exists;
  }
  public static Simias.Storage.Domain GetDomain()
  {
   Simias.Storage.Domain gaimDomain = null;
   try
   {
    Store store = Store.GetStore();
    gaimDomain = store.GetDomain(ID);
    if (gaimDomain != null)
    {
     return gaimDomain;
    }
   }
   catch(Exception ggd)
   {
    log.Error( ggd.Message );
    log.Error( ggd.StackTrace );
   }
   return gaimDomain;
  }
  public static void UpdatePreferences()
  {
   XmlDocument prefsDoc = new XmlDocument();
   try
   {
    prefsDoc.Load(GetGaimConfigDir() + "/prefs.xml");
   }
   catch
   {
    return;
   }
   XmlElement topPrefElement = prefsDoc.DocumentElement;
   XmlNode syncIntervalNode =
    topPrefElement.SelectSingleNode("//pref[@name='plugins']/pref[@name='simias']/pref[@name='sync_interval']/@value");
   if (syncIntervalNode != null)
   {
    string syncIntervalString = syncIntervalNode.Value;
    if (syncIntervalString != null)
    {
     int syncInterval = Int32.Parse(syncIntervalString);
     Simias.Gaim.Sync.UpdateSyncInterval(syncInterval);
    }
   }
  }
  public static void SynchronizeMembers()
  {
   log.Debug("SynchronizeMembers() called");
   if (!IsGaimPluginEnabled())
   {
    log.Debug("Gaim Plugin is not installed/enabled");
    return;
   }
   GaimAccount gaimAccount = GetDefaultGaimAccount();
   Simias.Storage.Domain domain = GetDomain();
   if (domain == null)
   {
    if (gaimAccount == null)
    {
     log.Debug("No default (AIM/prpl-oscar) Gaim Account");
     return;
    }
    Simias.Gaim.GaimDomain gaimDomain =
     new Simias.Gaim.GaimDomain(true, gaimAccount.Name, gaimAccount.Alias);
    domain = GetDomain();
   }
   if (domain == null) return;
   if (gaimAccount == null)
   {
    log.Debug("A Gaim Domain was created and not we can't get the GaimAccount.  Was the account deleted from Gaim?");
   }
   else
   {
    string accountAlias = gaimAccount.Alias;
    if (accountAlias != null && accountAlias.Length > 0)
    {
     Member member = domain.Owner;
     if (member != null)
     {
      PropertyList pList = member.Properties;
      if (pList != null)
      {
       bool bChanged = false;
       if (pList.HasProperty("Gaim:Alias"))
       {
        Property oldProp = pList.GetSingleProperty("Gaim:Alias");
        if (!accountAlias.Equals((string)oldProp.Value))
        {
         pList.ModifyProperty("Gaim:Alias", accountAlias);
         bChanged = true;
        }
       }
       else
       {
        Property p = new Property("Gaim:Alias", accountAlias);
        p.LocalProperty = true;
        member.Properties.AddProperty(p);
        bChanged = true;
       }
       string machineName = Environment.MachineName.ToLower();
       string fullName = string.Format("{0} ({1})",
               accountAlias,
               machineName);
       if (member.FN == null || !member.FN.Equals(fullName))
       {
        member.FN = fullName;
        bChanged = true;
       }
       if (bChanged)
       {
        domain.Name = string.Format("Gaim Buddy List ({0}@{1})", accountAlias, machineName);
        domain.Description = string.Format("Workgroup Domain built from {0}'s Gaim Buddy List", accountAlias);
        domain.Commit( new Node[] { domain, member } );
       }
      }
     }
    }
   }
   GaimBuddy[] buddies = SyncBuddies(domain);
   if (buddies != null)
   {
    UpdatePublicSharediFolder(buddies);
   }
  }
  public static GaimBuddy[] SearchForBuddies(string attributeName, string searchString, SearchOp operation)
  {
   ArrayList buddies = new ArrayList();
   GaimBuddy[] allBuddies = allBuddies = GetBuddies();
   if (allBuddies != null && allBuddies.Length > 0)
   {
    foreach (GaimBuddy buddy in allBuddies)
    {
     if (searchString == null)
     {
      try
      {
       buddies.Add(buddy);
      }
      catch {}
      continue;
     }
     string compareString = null;
     if (attributeName != null && attributeName == "Alias")
     {
      compareString = buddy.Alias;
     }
     if (compareString == null)
     {
      compareString = buddy.Name;
     }
     if (compareString != null)
     {
      compareString = compareString.ToLower();
      searchString = searchString.ToLower();
      switch(operation)
      {
       case SearchOp.Begins:
        if (compareString.StartsWith(searchString))
        {
         try
         {
          buddies.Add(buddy);
         }
         catch {}
        }
        break;
       case SearchOp.Exists:
       case SearchOp.Contains:
       default:
        if (compareString.IndexOf(searchString) >= 0)
        {
         try
         {
          buddies.Add(buddy);
         }
         catch {}
        }
        break;
      }
     }
    }
   }
   else
   {
    return (GaimBuddy[])buddies.ToArray(typeof(Simias.Gaim.GaimBuddy));
   }
   try
   {
    buddies.Sort(BuddyComparer.GetInstance());
   }
   catch{}
   return (GaimBuddy[])buddies.ToArray(typeof(Simias.Gaim.GaimBuddy));
  }
  public static void UpdateMember(string AccountName, string AccountProtocolID, string BuddyName, string MachineName)
  {
   Simias.Storage.Domain domain = GaimDomain.GetDomain();
   if (domain == null) return;
   XmlDocument blistDoc = new XmlDocument();
   try
   {
    blistDoc.Load(GetGaimConfigDir() + "/blist.xml");
   }
   catch
   {
    return;
   }
   XmlElement gaimElement = blistDoc.DocumentElement;
   string xPathQuery =
    string.Format("//buddy[@account='{0}' and @proto='{1}' and name='{2}' and setting[@name='simias-user-id:{3}']]",
    AccountName, AccountProtocolID, BuddyName, MachineName);
   XmlNode buddyNode = gaimElement.SelectSingleNode(xPathQuery);
   if (buddyNode != null)
   {
    try
    {
     GaimBuddy buddy = new GaimBuddy(buddyNode);
     Member member =
      FindBuddyInDomain(domain, buddy, MachineName);
     if (member != null)
     {
      UpdateMember(domain, member, buddy, MachineName);
     }
    }
    catch {}
   }
  }
  public static Member FindBuddyInDomain(GaimBuddy buddy, string machineName)
  {
   Member member = null;
   Simias.Storage.Domain domain = GetDomain();
   if (domain != null)
   {
    member = FindBuddyInDomain(domain, buddy, machineName);
   }
   return member;
  }
  public static GaimBuddy GetBuddyByUserID(string simiasUserID)
  {
   GaimBuddy buddy = null;
   XmlDocument blistDoc = new XmlDocument();
   try
   {
    blistDoc.Load(GetGaimConfigDir() + "/blist.xml");
   }
   catch (Exception e)
   {
    log.Debug("Couldn't load blist.xml");
    log.Error(e.Message);
    log.Error(e.StackTrace);
    return null;
   }
   XmlElement gaimElement = blistDoc.DocumentElement;
   string xPathQuery =
    string.Format("//buddy[setting[starts-with(@name, 'simias-user-id:') and .='{0}']]",
         simiasUserID);
   XmlNode buddyNode = gaimElement.SelectSingleNode(xPathQuery);
   if (buddyNode != null)
   {
    try
    {
     buddy = new GaimBuddy(buddyNode);
    }
    catch {}
   }
   else
   {
    log.Debug("XmlElement.SelectSingleNode(\"{0}\") returned null", xPathQuery);
   }
   return buddy;
  }
  public static RSACryptoServiceProvider GetCredential()
  {
   RSACryptoServiceProvider credential = null;
   XmlDocument prefsDoc = new XmlDocument();
   try
   {
    prefsDoc.Load(GetGaimConfigDir() + "/prefs.xml");
   }
   catch
   {
    return null;
   }
   XmlElement topPrefElement = prefsDoc.DocumentElement;
   XmlNode privateKeyNode =
    topPrefElement.SelectSingleNode("//pref[@name='plugins']/pref[@name='simias']/pref[@name='private_key']/@value");
   if (privateKeyNode != null)
   {
    string credentialXml = privateKeyNode.Value;
    if (credentialXml != null)
    {
     RSACryptoServiceProvider aCredential = new RSACryptoServiceProvider();
     try
     {
      aCredential.FromXmlString(credentialXml);
      credential = aCredential;
     }
     catch {}
    }
   }
   return credential;
  }
  public override string ToString()
  {
   return this.domainName;
  }
  internal static string GetGaimConfigDir()
  {
   string gaimConfigDir = "";
   if (MyEnvironment.Windows)
   {
    string userProfileDir = Environment.GetEnvironmentVariable("USERPROFILE");
    gaimConfigDir = string.Format("{0}\\Application Data\\.gaim", userProfileDir);
   }
   else
   {
    string userHomeDir = Environment.GetFolderPath(Environment.SpecialFolder.Personal);
    gaimConfigDir = string.Format("{0}/.gaim", userHomeDir);
   }
   return gaimConfigDir;
  }
  internal static bool IsGaimPluginEnabled()
  {
   XmlDocument prefsDoc = new XmlDocument();
   try
   {
    prefsDoc.Load(GetGaimConfigDir() + "/prefs.xml");
   }
   catch
   {
    return false;
   }
   XmlElement topPrefElement = prefsDoc.DocumentElement;
   XmlNodeList loadedPlugins =
    topPrefElement.SelectNodes("//pref[@name='gaim']/pref[@name='gtk']/pref[@name='plugins']/pref[@name='loaded']/item");
   if (loadedPlugins != null)
   {
    foreach (XmlNode loadedPlugin in loadedPlugins)
    {
     XmlAttribute attr = loadedPlugin.Attributes["value"];
     if (attr != null)
     {
      string value = attr.Value;
      if (value != null)
      {
       if (value.IndexOf("gifolder") > 0)
       {
        return true;
       }
      }
     }
    }
   }
   return false;
  }
  internal static bool ShouldMaintainPublicSharediFolder()
  {
   XmlDocument prefsDoc = new XmlDocument();
   try
   {
    prefsDoc.Load(GetGaimConfigDir() + "/prefs.xml");
   }
   catch
   {
    return false;
   }
   XmlElement topPrefElement = prefsDoc.DocumentElement;
   XmlNode settingNode =
    topPrefElement.SelectSingleNode("//pref[@name='plugins']/pref[@name='simias']/pref[@name='auto_public_ifolder']/@value");
   if (settingNode != null)
   {
    if (settingNode.Value == "1")
    {
log.Debug("ShouldMaintainPublicSharediFolder() returning: true");
     return true;
    }
   }
   return false;
  }
  internal static void UpdatePublicSharediFolder(GaimBuddy[] buddies)
  {
   if (ShouldMaintainPublicSharediFolder())
   {
    string iFolderName = BuildPublicSharediFolderName();
    if (iFolderName == null)
    {
     log.Debug("BuildPublicSharediFolderName() returned null so we can't update the public shared iFolder");
     return;
    }
    string userDesktopPath = Environment.GetFolderPath(Environment.SpecialFolder.Desktop);
    string iFolderPath;
    if (MyEnvironment.Windows)
    {
     iFolderPath = string.Format("{0}\\{1}", userDesktopPath, iFolderName);
    }
    else
    {
     iFolderPath = string.Format("{0}/{1}", userDesktopPath, iFolderName);
    }
    log.Debug("Updating Public Shared iFolder: {0}", iFolderPath);
    Simias.Storage.Collection col = SharedCollection.GetCollectionByPath(iFolderPath);
    if(col == null)
    {
     try
     {
      if (!Directory.Exists(iFolderPath))
      {
       Directory.CreateDirectory(iFolderPath);
       log.Debug("Created a new directory: {0}", iFolderPath);
      }
     }
     catch(DirectoryNotFoundException dnfe)
     {
      log.Debug(dnfe.Message);
      return;
     }
     catch(Exception e)
     {
      log.Debug(e.Message);
      return;
     }
     try
     {
      col = SharedCollection.CreateLocalSharedCollection(
         iFolderPath, Simias.Gaim.GaimDomain.ID, "iFolder");
      log.Debug("Created a new iFolder: {0}", iFolderPath);
     }
     catch(Exception e)
     {
      log.Debug("Error creating the Public Shared iFolder: {0}", e.Message);
      return;
     }
    }
    UpdatePublicSharediFolderMembers(col, buddies);
   }
  }
  internal static void UpdatePublicSharediFolderMembers(Simias.Storage.Collection col, GaimBuddy[] buddies)
  {
   Simias.Storage.Domain domain = GaimDomain.GetDomain();
   if (domain == null) return;
   Hashtable subscribedUserIDs = new Hashtable();
   Store store = Store.GetStore();
   Simias.POBox.POBox poBox =
    Simias.POBox.POBox.FindPOBox(store, domain.ID, store.GetUserIDFromDomainID(domain.ID));
   ICSList poList = poBox.Search(
    Simias.POBox.Subscription.SubscriptionCollectionIDProperty,
    col.ID,
    SearchOp.Equal);
   foreach(Simias.Storage.ShallowNode sNode in poList)
   {
    Simias.POBox.Subscription sub = new Simias.POBox.Subscription(poBox, sNode);
    if (sub.SubscriptionState == Simias.POBox.SubscriptionStates.Ready)
    {
     if (poBox.StoreReference.GetCollectionByID(
      sub.SubscriptionCollectionID) != null)
     {
      continue;
     }
    }
    subscribedUserIDs[sub.ToIdentity] = sub.ToIdentity;
    log.Debug("Added {0} to subscribedUserIDs", sub.ToName);
   }
   foreach (GaimBuddy buddy in buddies)
   {
    string[] machineNames = buddy.MachineNames;
    foreach (string machineName in machineNames)
    {
     Member member = FindBuddyInDomain(domain, buddy, machineName);
     if (member != null)
     {
      if (subscribedUserIDs[member.UserID] == null)
      {
       InviteUser(poBox, col, member);
      }
     }
     else
     {
      string memberName = buddy.GetSimiasMemberName(machineName);
      string memberUserID = buddy.GetSimiasUserID(machineName);
      if (memberName != null && memberUserID != null)
      {
       member = new Simias.Storage.Member(
           memberName,
           memberUserID,
           Simias.Storage.Access.Rights.ReadOnly );
       string alias = buddy.Alias;
       if (alias != null)
       {
        member.FN = string.Format("{0} ({1})", alias, machineName);
       }
       domain.Commit( member );
       log.Debug("Added new Gaim Domain Member in Simias: {0} ({1})", buddy.Name, machineName);
       InviteUser(poBox, col, member);
      }
     }
    }
   }
  }
  internal static void InviteUser(Simias.POBox.POBox poBox,
          Simias.Storage.Collection col,
          Simias.Storage.Member member)
  {
   Simias.Storage.Access.Rights newRights = Simias.Storage.Access.Rights.ReadOnly;
   Simias.POBox.Subscription sub = poBox.CreateSubscription(col, col.GetCurrentMember(), "iFolder");
   sub.SubscriptionRights = newRights;
   sub.ToName = member.Name;
   sub.ToIdentity = member.UserID;
   poBox.AddMessage(sub);
   log.Debug("Inviting {0}", member.Name);
  }
  internal static string BuildPublicSharediFolderName()
  {
   string iFolderName;
   string machineName = Environment.MachineName.ToLower();
   Simias.Storage.Domain domain = GaimDomain.GetDomain();
   if (domain == null) return null;
   Simias.Storage.Member owner = domain.Owner;
   iFolderName = string.Format("Gaim Public Files - {0}", owner.Name);
   return iFolderName;
  }
  internal static string GetGaimUserID()
  {
   XmlDocument prefsDoc = new XmlDocument();
   try
   {
    prefsDoc.Load(GetGaimConfigDir() + "/prefs.xml");
   }
   catch
   {
    return null;
   }
   XmlElement topPrefElement = prefsDoc.DocumentElement;
   XmlNode userIDNode =
    topPrefElement.SelectSingleNode("//pref[@name='plugins']/pref[@name='simias']/pref[@name='user_id']/@value");
   if (userIDNode != null)
   {
log.Debug("GetGaimUserID() returning: {0}", userIDNode.Value);
    return userIDNode.Value;
   }
   return null;
  }
  internal static GaimAccount GetDefaultGaimAccount()
  {
   GaimAccount[] accounts = GetGaimAccounts();
   if (accounts == null) return null;
   for (int i = 0; i < accounts.Length; i++)
   {
    string acctProto = accounts[i].ProtocolID;
    if (acctProto != null && acctProto == "prpl-oscar")
    {
     return accounts[i];
    }
   }
   return null;
  }
  internal static GaimAccount[] GetGaimAccounts()
  {
   ArrayList accounts = new ArrayList();
   XmlDocument accountsDoc = new XmlDocument();
   try
   {
    accountsDoc.Load(GetGaimConfigDir() + "/accounts.xml");
   }
   catch
   {
    return (GaimAccount[])accounts.ToArray(typeof(Simias.Gaim.GaimAccount));
   }
   XmlElement accountsElement = accountsDoc.DocumentElement;
   XmlNodeList accountNodes =
    accountsElement.SelectNodes("//account");
   if (accountNodes == null)
    return (GaimAccount[])accounts.ToArray(typeof(Simias.Gaim.GaimAccount));
   foreach (XmlNode accountNode in accountNodes)
   {
    try
    {
     GaimAccount account = new GaimAccount(accountNode);
     accounts.Add(account);
    }
    catch (Exception e)
    {
     log.Error( e.Message );
     log.Error( e.StackTrace );
    }
   }
   return (GaimAccount[])accounts.ToArray(typeof(Simias.Gaim.GaimAccount));
  }
  internal static GaimBuddy[] GetBuddies()
  {
   ArrayList buddies = new ArrayList();
   XmlDocument blistDoc = new XmlDocument();
   try
   {
    blistDoc.Load(GetGaimConfigDir() + "/blist.xml");
   }
   catch
   {
    return (GaimBuddy[])buddies.ToArray(typeof(Simias.Gaim.GaimBuddy));
   }
   XmlElement gaimElement = blistDoc.DocumentElement;
   XmlNodeList buddyNodes = gaimElement.SelectNodes("//buddy[setting[starts-with(@name, 'simias-url:')]]");
   if (buddyNodes == null)
    return (GaimBuddy[])buddies.ToArray(typeof(Simias.Gaim.GaimBuddy));
   foreach (XmlNode buddyNode in buddyNodes)
   {
    try
    {
     GaimBuddy buddy = new GaimBuddy(buddyNode);
     buddies.Add(buddy);
    }
    catch
    {
    }
   }
   return (GaimBuddy[])buddies.ToArray(typeof(Simias.Gaim.GaimBuddy));
  }
  internal static Member FindBuddyInDomain(Simias.Storage.Domain domain, GaimBuddy buddy, string machineName)
  {
   Member member = null;
   string simiasUserID = buddy.GetSimiasUserID(machineName);
   if (simiasUserID != null)
   {
    member = domain.GetMemberByID(simiasUserID);
   }
   return member;
  }
  internal static GaimBuddy[] SyncBuddies(Simias.Storage.Domain domain)
  {
   GaimBuddy[] buddies = GetBuddies();
   if (buddies == null) return null;
   log.Debug("Synching only iFolder Plugin-enabled Buddies");
   foreach (GaimBuddy buddy in buddies)
   {
    string[] machineNames = buddy.MachineNames;
    for (int i = 0; i < machineNames.Length; i++)
    {
     Member member =
      FindBuddyInDomain(domain, buddy, machineNames[i]);
     if (member != null)
     {
      UpdateMember(domain, member, buddy, machineNames[i]);
     }
    }
   }
   if (bPruneOldMembers)
   {
    PruneOldMembers(domain, buddies);
   }
   return buddies;
  }
  internal static void UpdateMember(Simias.Storage.Domain domain, Member member, GaimBuddy buddy)
  {
   string[] machineNames = buddy.MachineNames;
   for (int i = 0; i < machineNames.Length; i++)
   {
    UpdateMember(domain, member, buddy, machineNames[i]);
   }
  }
  internal static void UpdateMember(Simias.Storage.Domain domain, Member member, GaimBuddy buddy, string machineName)
  {
   Simias.Storage.PropertyList pList = member.Properties;
   Simias.Storage.Property p;
   string simiasURL = buddy.GetSimiasURL(machineName);
   if (simiasURL != null && simiasURL.Length > 0)
   {
    if (pList.HasProperty("Gaim:SimiasURL"))
    {
     Property oldProp = pList.GetSingleProperty("Gaim:SimiasURL");
     if (!simiasURL.Equals((string)oldProp.Value))
     {
      pList.ModifyProperty("Gaim:SimiasURL", simiasURL);
     }
    }
    else
    {
     p = new Property("Gaim:SimiasURL", simiasURL);
     p.LocalProperty = true;
     member.Properties.AddProperty(p);
    }
   }
   string alias = buddy.Alias;
   if (alias != null && alias.Length > 0)
   {
    if (pList.HasProperty("Gaim:Alias"))
    {
     Property oldProp = pList.GetSingleProperty("Gaim:Alias");
     if (!alias.Equals((string)oldProp.Value))
     {
      pList.ModifyProperty("Gaim:Alias", alias);
     }
    }
    else
    {
     p = new Property("Gaim:Alias", alias);
     p.LocalProperty = true;
     member.Properties.AddProperty(p);
    }
    string fullName = string.Format("{0} ({1})", alias, machineName);
    if (member.FN == null || !member.FN.Equals(fullName))
    {
     member.FN = fullName;
    }
   }
   domain.Commit(member);
  }
  internal static bool IsMemberInBuddyList(Member member, GaimBuddy[] buddies)
  {
   foreach (GaimBuddy buddy in buddies)
   {
    Simias.Storage.PropertyList pList = member.Properties;
    Simias.Storage.Property p = pList.GetSingleProperty("Gaim:MungedID");
    if (p != null && ((string) p.Value) == buddy.MungedID)
    {
     return true;
    }
   }
   return false;
  }
  internal static void PruneOldMembers(Simias.Storage.Domain domain, GaimBuddy[] buddies)
  {
   ICSList memberList = domain.GetMemberList();
   foreach(ShallowNode sNode in memberList)
   {
    Simias.Storage.Member member =
     new Simias.Storage.Member(domain, sNode);
    if (member.IsOwner) continue;
    if (!IsMemberInBuddyList(member, buddies))
    {
     domain.Commit(domain.Delete(member));
    }
   }
  }
 }
 public class BuddyComparer : IComparer
 {
  private static BuddyComparer staticInstance = null;
  internal BuddyComparer()
  {
  }
  public static BuddyComparer GetInstance()
  {
   if (staticInstance == null)
    staticInstance = new BuddyComparer();
   return staticInstance;
  }
  public int Compare(object a, object b)
  {
   GaimBuddy b1 = (GaimBuddy)a;
   GaimBuddy b2 = (GaimBuddy)b;
   string b1Str;
   string b2Str;
   string alias1 = b1.Alias;
   string alias2 = b2.Alias;
   if (alias1 != null)
    b1Str = alias1;
   else
    b1Str = b1.Name;
   if (alias2 != null)
    b2Str = alias2;
   else
    b2Str = b2.Name;
   b1Str = b1Str.ToLower();
   b2Str = b2Str.ToLower();
   return b1Str.CompareTo(b2Str);
  }
 }
}
