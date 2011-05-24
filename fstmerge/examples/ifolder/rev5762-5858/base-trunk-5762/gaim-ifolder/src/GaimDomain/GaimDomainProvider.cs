

using System;
using System.Collections;
using System.Net;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using System.Web;

using Simias;
using Simias.Authentication;
using Simias.Storage;
using Simias.Sync;
using Simias.POBox;


using SCodes = Simias.Authentication.StatusCodes;

namespace Simias.Gaim
{



 public class GaimDomainProvider : IDomainProvider
 {

  private string providerName = "Gaim Domain Provider";
  private string description = "Simias Domain Provider for the Gaim Workgroup Domain";
  private Hashtable searchContexts = new Hashtable();
  private static readonly ISimiasLog log =
   SimiasLogManager.GetLogger( System.Reflection.MethodBase.GetCurrentMethod().DeclaringType );







  public string Name
  {
   get { return( providerName ); }
  }




  public string Description
  {
   get { return( description ); }
  }




  public GaimDomainProvider()
  {
   log.Debug( "Instantiated" );
  }
  public Authentication.Status Authenticate( Simias.Storage.Domain domain, HttpContext ctx )
  {
   string gaimSessionTag = "gaim";
   Simias.Storage.Member member = null;
   Simias.Authentication.Status status =
    new Simias.Authentication.Status( SCodes.InvalidCredentials );
   if ( ctx.Session != null )
   {
    GaimSession gaimSession;
    string memberID = ctx.Request.Headers[ "gaim-member" ];
    if ( memberID == null || memberID == "" )
    {
     return status;
    }
    member = domain.GetMemberByID( memberID );
    if ( member == null )
    {
     return status;
    }
    status.UserName = member.Name;
    status.UserID = member.UserID;
    gaimSession = ctx.Session[ gaimSessionTag ] as GaimSession;
    if ( gaimSession == null )
    {
     gaimSession = new GaimSession();
     gaimSession.MemberID = member.UserID;
     gaimSession.State = 1;
     gaimSession.OneTimePassword = DateTime.UtcNow.Ticks.ToString();
     GaimBuddy buddy = GaimDomain.GetBuddyByUserID( member.UserID );
     if (buddy != null)
     {
      RSACryptoServiceProvider credential = buddy.GetCredentialByUserID( member.UserID );
      if ( credential != null )
      {
       byte[] oneTime = new UTF8Encoding().GetBytes( gaimSession.OneTimePassword );
       byte[] encryptedText = credential.Encrypt( oneTime, false );
       ctx.Response.AddHeader(
        "gaim-secret",
        Convert.ToBase64String( encryptedText ) );
       ctx.Session[ gaimSessionTag ] = gaimSession;
      }
     }
    }
    else
    if ( status.UserID == gaimSession.MemberID )
    {
     string encodedSecret = ctx.Request.Headers[ "gaim-secret" ];
     if ( encodedSecret != null && encodedSecret != "" )
     {
      UTF8Encoding utf8 = new UTF8Encoding();
      string decodedString =
       utf8.GetString( Convert.FromBase64String( encodedSecret ) );
      if ( decodedString.Equals( gaimSession.OneTimePassword ) == true )
      {
       status.statusCode = SCodes.Success;
       gaimSession.State = 2;
      }
     }
     else
     {
      gaimSession.OneTimePassword = DateTime.UtcNow.Ticks.ToString();
      gaimSession.State = 1;
      GaimBuddy buddy = GaimDomain.GetBuddyByUserID( member.UserID );
      if (buddy != null)
      {
       RSACryptoServiceProvider credential = buddy.GetCredentialByUserID( member.UserID );
       if ( credential != null )
       {
        try
        {
         byte[] oneTime = new UTF8Encoding().GetBytes( gaimSession.OneTimePassword );
         byte[] encryptedText = credential.Encrypt( oneTime, false );
         ctx.Response.AddHeader(
          "gaim-secret",
          Convert.ToBase64String( encryptedText ) );
        }
        catch( Exception encr )
        {
         log.Debug( encr.Message );
         log.Debug( encr.StackTrace );
        }
       }
      }
     }
    }
   }
   return status;
  }
  public void DeleteLocation( string domainID, string collectionID )
  {
  }
  public void FindCloseDomainMembers( string searchContext )
  {
   if (searchContext == null) return;
   if (searchContexts.Contains(searchContext))
   {
    searchContexts.Remove(searchContext);
   }
  }
  public bool FindFirstDomainMembers( string domainID, int count, out string searchContext, out Member[] memberList, out int total )
  {
   return FindFirstDomainMembers(domainID, PropertyTags.FullName, String.Empty, SearchOp.Contains, count, out searchContext, out memberList, out total);
  }
  public bool FindFirstDomainMembers( string domainID, string attributeName, string searchString, SearchOp operation, int count, out string searchContext, out Member[] memberList, out int total )
  {
   ArrayList members = new ArrayList();
   ArrayList allMembers = new ArrayList();
   searchContext = null;
   total = 0;
   GaimBuddy[] buddies =
    GaimDomain.SearchForBuddies(mapSimiasAttribToGaim(attributeName),
           searchString,
           operation);
   if (buddies != null && buddies.Length > 0)
   {
    total = buddies.Length;
    foreach (GaimBuddy buddy in buddies)
    {
     string[] machineNames = buddy.MachineNames;
     for (int i = 0; i < machineNames.Length; i++)
     {
      Member member = GaimDomain.FindBuddyInDomain(buddy, machineNames[i]);
      if (member == null)
      {
       member = new Member(buddy.GetSimiasMemberName(machineNames[i]),
            buddy.GetSimiasUserID(machineNames[i]),
            Simias.Storage.Access.Rights.ReadWrite,
            null, null);
       string alias = buddy.Alias;
       if (alias != null)
       {
        member.FN = string.Format("{0} ({1})", alias, machineNames[i]);
       }
      }
      if (members.Count < count)
      {
       members.Add(member);
      }
      allMembers.Add(member);
     }
    }
   }
   memberList = members.ToArray(typeof(Member)) as Member[];
   if (allMembers.Count > 0)
   {
    GaimDomainSearchContext newSearchContext = new GaimDomainSearchContext();
    newSearchContext.Members = allMembers;
    newSearchContext.CurrentIndex = members.Count;
    searchContext = newSearchContext.ID;
    lock (searchContexts.SyncRoot)
    {
     searchContexts.Add(searchContext, newSearchContext);
    }
   }
   return members.Count < allMembers.Count ? true : false;
  }
  public bool FindNextDomainMembers( ref string searchContext, int count, out Member[] memberList )
  {
   bool bMoreEntries = false;
   ArrayList members = new ArrayList();
   memberList = null;
   if (searchContext == null)
    throw new ArgumentNullException("searchContext cannot be null when calling FindNextDomainMembers");
   lock (searchContexts.SyncRoot)
   {
    if (!searchContexts.Contains(searchContext))
     return false;
    GaimDomainSearchContext gaimDomainSearchContext = (GaimDomainSearchContext)searchContexts[searchContext];
    int i = gaimDomainSearchContext.CurrentIndex;
    while (i < gaimDomainSearchContext.Count)
    {
     Member member = (Member)gaimDomainSearchContext.Members[i];
     if (member != null)
     {
      if (members.Count < count)
      {
       members.Add(member);
      }
      else
      {
       bMoreEntries = true;
       break;
      }
     }
     i++;
    }
    if (i >= gaimDomainSearchContext.Count)
    {
     gaimDomainSearchContext.CurrentIndex = gaimDomainSearchContext.Count - 1;
    }
    else
    {
     gaimDomainSearchContext.CurrentIndex = i;
    }
   }
   if (members.Count > 0)
   {
    memberList = members.ToArray(typeof (Member)) as Member[];
   }
   return bMoreEntries;
  }
  public bool FindPreviousDomainMembers( ref string searchContext, int count, out Member[] memberList )
  {
   bool bMoreEntries = false;
   ArrayList members = new ArrayList();
   memberList = null;
   if (searchContext == null)
    throw new ArgumentNullException("searchContext cannot be null when calling FindPreviousDomainMembers");
   lock (searchContexts.SyncRoot)
   {
    if (!searchContexts.Contains(searchContext))
     return false;
    GaimDomainSearchContext gaimDomainSearchContext = (GaimDomainSearchContext)searchContexts[searchContext];
    int i = gaimDomainSearchContext.CurrentIndex - 1;
    while (i >= 0)
    {
     Member member = (Member)gaimDomainSearchContext.Members[i];
     if (member != null)
     {
      if (members.Count < count)
      {
       members.Add(member);
      }
      else
      {
       bMoreEntries = true;
       break;
      }
     }
     i--;
    }
    if (i < 0)
    {
     gaimDomainSearchContext.CurrentIndex = 0;
    }
    else
    {
     gaimDomainSearchContext.CurrentIndex = i;
    }
   }
   if (members.Count > 0)
   {
    memberList = members.ToArray(typeof (Member)) as Member[];
   }
   return bMoreEntries;
  }
  public bool FindSeekDomainMembers( ref string searchContext, int offset, int count, out Member[] memberList )
  {
   memberList = null;
   if (searchContext == null)
    throw new ArgumentNullException("searchContext cannot be null when calling FindSeekDomainMembers");
   lock (searchContexts.SyncRoot)
   {
    if (!searchContexts.Contains(searchContext))
     return false;
    GaimDomainSearchContext gaimDomainSearchContext = (GaimDomainSearchContext)searchContexts[searchContext];
    if (offset < 0 || offset >= gaimDomainSearchContext.Count)
     throw new IndexOutOfRangeException("offset is out of bounds with the total number of members available in the search");
    gaimDomainSearchContext.CurrentIndex = offset;
    return FindNextDomainMembers(ref searchContext, count, out memberList);
   }
  }
  public bool OwnsDomain( string domainID )
  {
   log.Debug( "OwnsDomain called" );
   return ( domainID.ToLower() == Simias.Gaim.GaimDomain.ID ) ? true : false;
  }
  public void PreCommit( string domainID, Member member )
  {
   GaimBuddy buddy = GaimDomain.GetBuddyByUserID(member.UserID);
   if (buddy == null)
   {
    log.Debug("PreCommit() called on a member that no longer exists in blist.xml");
    return;
   }
   Simias.Storage.Property p = new Property("Gaim:AccountName", buddy.AccountName);
   p.LocalProperty = true;
   member.Properties.AddProperty(p);
   p = new Property("Gaim:AccountProto", buddy.AccountProtocolID);
   p.LocalProperty = true;
   member.Properties.AddProperty(p);
   p = new Property("Gaim:Screenname", buddy.Name);
   p.LocalProperty = true;
   member.Properties.AddProperty(p);
   string machineName = GaimBuddy.ParseMachineName(member.Name);
   if (machineName != null)
   {
    string simiasURL = buddy.GetSimiasURL(machineName);
    if (simiasURL != null)
    {
     p = new Property("Gaim:SimiasURL", simiasURL);
     p.LocalProperty = true;
     member.Properties.AddProperty(p);
    }
   }
  }
  public Uri ResolveLocation( string domainID )
  {
   log.Debug( "ResolveLocation(domainID) called" );
   Uri locationUri = null;
   if( domainID.ToLower() == Simias.Gaim.GaimDomain.ID )
   {
    Member member = Store.GetStore().GetDomain( domainID ).GetCurrentMember();
    locationUri = MemberToUri( GaimDomain.GetDomain(), member );
   }
   return locationUri;
  }
  public Uri ResolveLocation( string domainID, string collectionID )
  {
   log.Debug( "ResolveLocation called" );
   Uri locationUri = null;
   if( domainID.ToLower() == Simias.Gaim.GaimDomain.ID )
   {
    try
    {
     Store store = Store.GetStore();
     Simias.Storage.Domain domain = store.GetDomain(domainID.ToLower());
     Simias.POBox.POBox poBox =
      Simias.POBox.POBox.FindPOBox(store, domainID, domain.GetCurrentMember().UserID);
     Subscription sub = poBox.GetSubscriptionByCollectionID(collectionID);
     locationUri = SubscriptionFromIDToUri(sub.FromIdentity);
     if (locationUri == null)
     {
      Collection collection = Store.GetStore().GetCollectionByID( collectionID );
      Member member = collection.GetMemberByID(collection.Owner.UserID);
      if ( member != null )
      {
       locationUri = MemberToUri(domain, member);
      }
     }
    }
    catch ( Exception e )
    {
     log.Debug( e.Message );
     log.Debug( e.StackTrace );
    }
   }
   return locationUri;
  }
  public Uri ResolveLocation( string domainID, string userID, string collectionID )
  {
   log.Debug( "ResolveLocation with userID called" );
   Uri locationUri = null;
   if( domainID.ToLower() == Simias.Gaim.GaimDomain.ID )
   {
    try
    {
     Simias.Storage.Domain domain = GaimDomain.GetDomain();
     Member member = domain.GetMemberByID(userID);
     if ( member != null )
     {
      locationUri = MemberToUri(domain, member);
     }
    }
    catch ( Exception e )
    {
     log.Debug( e.Message );
     log.Debug( e.StackTrace );
    }
   }
   return locationUri;
  }
  public Uri ResolvePOBoxLocation( string domainID, string userID )
  {
   log.Debug( "ResolvePOBoxLocation called" );
   Uri locationUri = null;
   if( domainID.ToLower() == Simias.Gaim.GaimDomain.ID )
   {
    try
    {
     Simias.Storage.Domain domain = GaimDomain.GetDomain();
     Member member = domain.GetMemberByID(userID);
     if ( member != null )
     {
      locationUri = MemberToUri(domain, member);
     }
    }
    catch ( Exception e )
    {
     log.Debug( e.Message );
     log.Debug( e.StackTrace );
    }
   }
   return locationUri;
  }
  public void SetHostLocation( string domainID, Uri hostLocation )
  {
  }
  private Uri MemberToUri(Simias.Storage.Domain domain, Member member)
  {
   Uri locationUri = null;
   if (domain == null || member == null) return null;
   Simias.Storage.PropertyList pList = member.Properties;
   Simias.Storage.Property p = pList.GetSingleProperty("Gaim:SimiasURL");
   if (p == null) return null;
   locationUri = new Uri((string) p.Value);
   return locationUri;
  }
  private Uri SubscriptionFromIDToUri(string fromID)
  {
   Uri locationUri = null;
   GaimBuddy buddy = GaimDomain.GetBuddyByUserID(fromID);
   if (buddy != null)
   {
    string simiasURL = buddy.GetSimiasURLByUserID(fromID);
    if (simiasURL != null)
    {
     locationUri = new Uri(simiasURL);
    }
   }
   return locationUri;
  }
  private string mapSimiasAttribToGaim(string attributeName)
  {
     return "ScreenName";
  }
  private class GaimSession
  {
   public string MemberID;
   public string OneTimePassword;
   public int State;
   public GaimSession()
   {
   }
  }
 }
}
