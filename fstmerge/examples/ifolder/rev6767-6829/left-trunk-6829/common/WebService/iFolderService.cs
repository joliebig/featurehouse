

using System;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Web;
using System.Web.SessionState;
using System.Web.Services;
using System.Web.Services.Protocols;
using System.IO;
using System.Net;
using Simias;
using Simias.Client;
using Simias.Authentication;
using Simias.Storage;
using Simias.Sync;
using Simias.POBox;
using Simias.Discovery;
using Simias.Policy;
using Simias.Web;
using System.Xml;
using System.Xml.Serialization;




namespace Novell.iFolder.Web
{



 public enum iFolderSearchType
 {



  Equal,




  Not_Equal,




  Begins,




  Ends,




  Contains,




  Greater,




  Less,




  Greater_Equal,




  Less_Equal,




  Exists,




  CaseEqual
 };

 enum SecurityState
 {
  encrypt = 1,
  enforceEncrypt = 2,
  encryptionState = 3,
  SSL = 4,
  enforceSSL = 8,
  SSLState = 12,
  UserEncrypt = 16,
  UserSSL = 32
 }




 public enum Share
 {
  Sharing = 1,
  EnforcedSharing = 4,
  DisableSharing = 8
 }





 [WebService(
 Namespace="http://novell.com/ifolder/web/",
 Name="iFolderWebService",
 Description="Web Service providing access to iFolder")]
 public class iFolderService : WebService
 {



  public iFolderService()
  {
  }


  internal class UserComparer : IComparer
  {
   int IComparer.Compare( Object x, Object y )
   {
    iFolderUser memberX = x as iFolderUser;
    iFolderUser memberY = y as iFolderUser;

    if ( memberX.FN != null )
    {
     if (memberY.FN != null)
     {
      return (new CaseInsensitiveComparer()).Compare( memberX.FN, memberY.FN );
     }

     return (new CaseInsensitiveComparer()).Compare( memberX.FN, memberY.Name );
    }
    else
    if ( memberY.FN != null )
    {
     return ( new CaseInsensitiveComparer()).Compare( memberX.Name, memberY.FN );
    }

    return ( new CaseInsensitiveComparer()).Compare( memberX.Name, memberY.Name );
   }
  }






  [WebMethod(EnableSession=true, Description="Allows a client to ping to make sure the Web Service is up and running")]
  [SoapDocumentMethod]
  public void Ping()
  {

  }
  [WebMethod(EnableSession=true, Description="Checks a LocalPath to see if it's an iFolder")]
  [SoapDocumentMethod]
  public bool IsiFolder(string LocalPath)
  {
   Collection col = SharedCollection.GetCollectionByPath(LocalPath);
   if(col != null)
   {
    if(col.IsType(col, iFolderWeb.iFolderType))
     return true;
   }
   return false;
  }
  [WebMethod(EnableSession=true, Description="Checks LocalPath to see if can be an iFolder")]
  [SoapDocumentMethod]
  public bool CanBeiFolder(string LocalPath)
  {
   return SharedCollection.CanBeCollection(LocalPath);
  }
  [WebMethod(EnableSession=true, Description="Checks LocalPath to see if is in an iFolder")]
  [SoapDocumentMethod]
  public bool IsPathIniFolder(string LocalPath)
  {
   return SharedCollection.IsPathInCollection(LocalPath);
  }
  [WebMethod(EnableSession=true, Description="Create An iFolder. This will create an iFolder using the path specified.  The Path must exist or an exception will be thrown.")]
  [SoapDocumentMethod]
  public iFolderWeb CreateLocaliFolder(string Path)
  {
   Collection col = SharedCollection.CreateLocalSharedCollection(
        Path, iFolderWeb.iFolderType);
   return new iFolderWeb(col);
  }
  [WebMethod(EnableSession=true, Description="Create an iFolder. This will create an iFolder using the path specified with the security Level desired. The Path must exist or an exception will be thrown.")]
  [SoapDocumentMethod]
  public iFolderWeb CreateiFolderInDomainEncr(string Path, string DomainID, bool SSL, string EncryptionAlgorithm, string Passphrase)
  {
   try
   {
                Path = Path.TrimEnd(new char[] { '\\' });
                Path = Path.TrimEnd(new char[] { '/' });
    Collection col = SharedCollection.CreateLocalSharedCollection( Path, DomainID, SSL, iFolderWeb.iFolderType, EncryptionAlgorithm, Passphrase);
    return new iFolderWeb(col);
   }
   catch(Exception e)
   {
    Console.WriteLine(e);
    throw e;
   }
  }
  [WebMethod(EnableSession=true, Description="Create an iFolder. This will create an iFolder using the path specified.  The Path must exist or an exception will be thrown.")]
  [SoapDocumentMethod]
  public iFolderWeb CreateiFolderInDomain(string Path, string DomainID)
  {
   try
   {
   Collection col = SharedCollection.CreateLocalSharedCollection(
        Path, DomainID, iFolderWeb.iFolderType);
   return new iFolderWeb(col);
   }
   catch(Exception e)
   {
    Console.WriteLine(e);
    throw e;
   }
  }
  [WebMethod(EnableSession=true, Description="Checks the presence of an iFolder")]
  [SoapDocumentMethod]
  public bool CheckiFolder(string iFolderID)
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
   {
    CollectionInfo ci = DiscoveryFramework.GetLocalCollectionInfo(iFolderID);
                if (ci == null)
                {
                    ci = DiscoveryFramework.GetCollectionInfo(iFolderID);
                    if (ci == null)
                        return false;
                    else
                        return true;
                }
                else
                    return true;
   }
   else
   {
    if(col.IsType(col, iFolderWeb.iFolderType))
     return true;
    else
     return false;
   }
   return false;
  }
  [WebMethod(EnableSession=true, Description="Get An iFolder")]
  [SoapDocumentMethod]
  public iFolderWeb GetiFolder(string iFolderID)
  {
   iFolderWeb ifolder = null;
   ArrayList collectionList;
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if (col != null)
   {
    if(col.IsType(col, iFolderWeb.iFolderType))
     ifolder = new iFolderWeb(col);
    else
     ifolder = null;
   }
   else
   {
    CollectionInfo ci = DiscoveryFramework.GetLocalCollectionInfo(iFolderID);
                if(ci == null)
                    ci = DiscoveryFramework.GetCollectionInfo(iFolderID);
    ifolder = new iFolderWeb(ci);
   }
   return ifolder;
  }
  [WebMethod(EnableSession=true, Description="Get An minimal iFolder")]
  [SoapDocumentMethod]
  public iFolderWeb GetMinimaliFolder(string iFolderID, int infoToFetch)
  {
   if(infoToFetch == 0)
    return GetiFolder(iFolderID);
   iFolderWeb ifolder = null;
   ArrayList collectionList;
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if (col != null)
   {
    if(col.IsType(col, iFolderWeb.iFolderType))
     ifolder = new iFolderWeb(col, infoToFetch);
    else
     ifolder = null;
   }
   else
   {
    CollectionInfo ci = DiscoveryFramework.GetLocalCollectionInfo(iFolderID);
    if(ci == null)
     ci = DiscoveryFramework.GetCollectionInfo(iFolderID);
    ifolder = new iFolderWeb(ci);
   }
   return ifolder;
  }
  private int DeriveStatus(int system, int group, int user, int preference)
  {
   if( preference != 0)
   {
    if(system != 0)
     return system;
    else if(group != 0)
     return group;
    return user;
   }
   else
   {
    if(user != 0)
     return user;
    else if(group != 0)
     return group;
    return system;
   }
  }
  [WebMethod(EnableSession=true, Description="This method is for getting security policy from the Collectionstore.")]
  [SoapDocumentMethod]
  public int GetSecurityPolicy(string DomainID)
  {
   Console.WriteLine("GetSecurityPolicy web service called");
   try
   {
                  Simias.Storage.Store sstore = Simias.Storage.Store.GetStore();
                  Simias.Storage.Domain sdomain = sstore.GetDomain(DomainID);
                  Simias.Storage.Member smember = sdomain.GetCurrentMember();
                  if (smember == null)
                  {
                       return 0;
                  }
     return(Simias.Policy.SecurityState.GetStatus( smember ));
   }
   catch( Exception e )
   {
    Console.WriteLine(e);
    throw(e);
   }
  }
  [WebMethod(EnableSession = true, Description = "This method is for getting Disable sharing policy from the Collectionstore.")]
  [SoapDocumentMethod]
  public bool GetDisableSharingPolicy(string currentUserID, string iFolderID, string OwnerID, string DomainID)
  {
            try
            {
                Simias.Storage.Store store = Simias.Storage.Store.GetStore();
                Simias.Storage.Domain domain = store.GetDomain(DomainID);
                Collection col = store.GetCollectionByID(iFolderID);
                Simias.Storage.Member member = domain.GetCurrentMember();
                if(member.UserID != OwnerID)
                {
                    return true;
                }
                int UserAndAboveAggregateSharingStatus = 0;
                int iFolderSharingStatus ;
                UserAndAboveAggregateSharingStatus = Simias.Policy.Sharing.Get(member);
                iFolderSharingStatus = Simias.Policy.Sharing.GetStatus(col);
                if (((UserAndAboveAggregateSharingStatus & (int)Share.EnforcedSharing) == (int)Share.EnforcedSharing))
                {
                    if ((UserAndAboveAggregateSharingStatus & (int)Share.Sharing) == (int)Share.Sharing)
                        return true;
                    return false;
                }
                if (iFolderSharingStatus != 0)
                {
                    if ((iFolderSharingStatus & (int)Share.Sharing) == (int)Share.Sharing)
                    {
                        return true;
                    }
                    if ((iFolderSharingStatus & (int)Share.DisableSharing) == (int)Share.DisableSharing)
                    {
                        return false;
                    }
                }
                else
                {
                    if ((UserAndAboveAggregateSharingStatus & (int)Share.Sharing) == (int)Share.Sharing)
                    {
                        return true;
                    }
                    if ((UserAndAboveAggregateSharingStatus & (int)Share.DisableSharing) == (int)Share.DisableSharing)
                    {
                        return false;
                    }
                }
                return true;
            }
            catch (Exception e)
            {
                throw (e);
            }
  }
        [WebMethod(EnableSession = true, Description = "Gets whetgher ifolder can be transferred or not")]
        [SoapDocumentMethod]
        public bool CanOwnerBeChanged(string newUserID, string domainID)
        {
            Simias.Storage.Store store = Simias.Storage.Store.GetStore();
            Domain domain = store.GetDomain(domainID);
            Simias.Storage.Member member = domain.GetCurrentMember();
            return member.IsTransferAllowed(domain.ID, member.UserID, newUserID);
        }
        [WebMethod(EnableSession = true, Description = "Gets whetgher ifolder can be created or not")]
        [SoapDocumentMethod]
        public int GetLimitPolicyStatus(string DomainID)
        {
            int count = 0;
            long NoiFoldersLimit = -1;
            try
            {
                Simias.Storage.Store store = Simias.Storage.Store.GetStore();
                Simias.Storage.Domain domain = store.GetDomain(DomainID);
                if (domain == null)
                    return 0;
                Simias.Storage.Member member = domain.GetCurrentMember();
                count = GetiFoldercount(DomainID, member.UserID);
                NoiFoldersLimit = Simias.Policy.iFolderLimit.Get(member).Limit;
                if (NoiFoldersLimit < 0)
                {
                    return 1;
                }
                else
                if (count >= NoiFoldersLimit)
                {
                    return 0;
                }
                return 1;
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
                throw (e);
            }
        }
  [WebMethod(EnableSession=true, Description="Get An iFolder")]
  [SoapDocumentMethod]
  public iFolderWeb GetiFolderInvitation(string POBoxID, string iFolderID)
  {
   iFolderWeb ifolder = null;
   CollectionInfo ci = DiscoveryFramework.GetLocalCollectionInfo(iFolderID);
            if(ci == null)
                ci = DiscoveryFramework.GetCollectionInfo(iFolderID);
   ifolder = new iFolderWeb(ci);
   return ifolder;
  }
  [WebMethod(EnableSession=true, Description="Get An iFolder using a LocalPath")]
  [SoapDocumentMethod]
  public iFolderWeb GetiFolderByLocalPath(string LocalPath)
  {
   iFolderWeb ifolder = null;
   Collection col = SharedCollection.GetCollectionByPath(LocalPath);
   if(col != null)
   {
    ifolder = new iFolderWeb(col);
   }
   return ifolder;
  }
  [WebMethod(EnableSession=true, Description="Delete An iFolder")]
  [SoapDocumentMethod]
  public void DeleteiFolder(string DomainID, string iFolderID)
  {
                       Store store = Store.GetStore();
                        SharedCollection.DeleteSharedCollection(iFolderID);
   DiscoveryFramework.DeleteCollectionInCatalog(DomainID, iFolderID);
  }
                [WebMethod(EnableSession=true, Description="Revert an iFolder on the local computer but remain a member")]
                [SoapDocumentMethod]
                public iFolderWeb RevertiFolder(string iFolderID)
                {
                        iFolderWeb ifolder = null;
                        CollectionInfo cinfo = SharedCollection.RevertSharedCollection(iFolderID);
                        if (cinfo != null)
                        {
                                ifolder = new iFolderWeb(cinfo);
                        }
                        return ifolder;
                }
  [WebMethod(EnableSession=true, Description="Revert an iFolder on the local computer but remain a member")]
  [SoapDocumentMethod]
  public iFolderWeb RevertiFolder1(string iFolderID)
  {
   iFolderWeb ifolder = null;
   return ifolder;
  }
  [WebMethod(EnableSession=true, Description="Returns all iFolders on the Server")]
  [SoapDocumentMethod]
  public iFolderWeb[] GetAlliFolders()
  {
   Collection col = null;
   ArrayList list = null;
   list = new ArrayList();
   Store store = Store.GetStore();
   ArrayList collectionList = null;
   Simias.Discovery.DiscService.UpdateCollectionList();
   collectionList = Simias.Discovery.CollectionList.GetCollectionList();
   if (collectionList != null )
   {
           foreach ( CollectionInfo ci in collectionList )
    {
     col = null;
     if ((col = store.GetCollectionByID(ci.CollectionID)) != null)
     {
      list.Add( new iFolderWeb(col));
     }
     else
     {
      list.Add(new iFolderWeb(ci));
     }
    }
   }
   else
   {
    ICSList iFolderList = null;
                                iFolderList = store.GetCollectionsByType(iFolderWeb.iFolderType);
                         foreach(ShallowNode sn in iFolderList)
                         {
     col = null;
                                 col = store.GetCollectionByID(sn.ID);
                                 list.Add(new iFolderWeb(col));
                         }
   }
   return (iFolderWeb[])list.ToArray(typeof(iFolderWeb));
  }
  [WebMethod(EnableSession=true, Description="Returns all iFolders on the Server")]
  [SoapDocumentMethod]
  public iFolderWeb[] GetAlliFolders1()
  {
   ArrayList list = new ArrayList();
   Store store = Store.GetStore();
   ICSList iFolderList =
     store.GetCollectionsByType(iFolderWeb.iFolderType);
   foreach(ShallowNode sn in iFolderList)
   {
    Collection col = store.GetCollectionByID(sn.ID);
    list.Add(new iFolderWeb(col));
   }
   ICSList domainList = store.GetDomainList();
   foreach (ShallowNode sn in domainList)
   {
    POBox poBox = Simias.POBox.POBox.FindPOBox(store,
       sn.ID,
       store.GetUserIDFromDomainID(sn.ID));
    if(poBox != null)
    {
     ICSList poList = poBox.Search(
       PropertyTags.Types,
       typeof(Subscription).Name,
       SearchOp.Equal);
     foreach(ShallowNode sNode in poList)
     {
      Subscription sub = new Subscription(poBox, sNode);
      if(sub.ToIdentity != poBox.Owner.UserID)
       continue;
      if (sub.SubscriptionCollectionType != iFolderWeb.iFolderType)
       continue;
      if (store.GetCollectionByID(
         sub.SubscriptionCollectionID) != null)
      {
       continue;
      }
      if(sub.SubscriptionDisposition ==
         SubscriptionDispositions.Declined)
      {
       continue;
      }
      list.Add(new iFolderWeb(sub));
     }
    }
   }
   return (iFolderWeb[])list.ToArray(typeof(iFolderWeb));
  }
  [WebMethod(EnableSession=true, Description="Returns all iFolders in the specified domain")]
  [SoapDocumentMethod]
  public iFolderWeb[] GetiFoldersForDomain( string DomainID )
  {
   ArrayList list = new ArrayList();
   Store store = Store.GetStore();
   ICSList iFolderList =
     store.GetCollectionsByDomain(DomainID);
   foreach(ShallowNode sn in iFolderList)
   {
    if (sn.Type.Equals(NodeTypes.CollectionType))
    {
     Collection col = store.GetCollectionByID(sn.ID);
     if (col.IsType(col, iFolderWeb.iFolderType))
     {
      list.Add(new iFolderWeb(col));
     }
    }
   }
   ArrayList collectionList = Simias.Discovery.CollectionList.GetCollectionList();
          foreach (CollectionInfo ci in collectionList)
   {
           if ( ci.DomainID != DomainID)
            continue;
    list.Add (new iFolderWeb (ci));
   }
   return (iFolderWeb[])list.ToArray(typeof(iFolderWeb));
  }
  [WebMethod(EnableSession=true, Description="Returns iFolders for the specified UserID")]
  [SoapDocumentMethod]
  public iFolderWeb[] GetiFolders(string UserID)
  {
   ArrayList list = new ArrayList();
   Store store = Store.GetStore();
   ICSList iFolderList =
     store.GetCollectionsByUser(UserID);
   foreach(ShallowNode sn in iFolderList)
   {
    Collection col = store.GetCollectionByID(sn.ID);
    if(col.IsType(col, iFolderWeb.iFolderType))
     list.Add(new iFolderWeb(col));
   }
   return (iFolderWeb[])list.ToArray(typeof(iFolderWeb));
  }
  public int GetiFoldercount(string DomainID, string userID)
                {
   ArrayList list = Simias.Discovery.CollectionList.GetCollectionList();
   int count = 0;
                        foreach (CollectionInfo ci in list)
                        {
                                if ( ci.DomainID != DomainID)
                                        continue;
    if( ci.OwnerID == userID )
     count++;
   }
   return count;
                }
  [WebMethod(EnableSession=true, Description="Set the Rights of a member of an iFolder.  The Rights can be \"Admin\", \"ReadOnly\", or \"ReadWrite\".")]
  [SoapDocumentMethod]
  public void SetUserRights( string iFolderID,
         string UserID,
         string Rights)
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   Simias.Storage.Member member = col.GetMemberByID(UserID);
   if(member != null)
   {
    if(Rights == "Admin")
     member.Rights = Access.Rights.Admin;
    else if(Rights == "ReadOnly")
     member.Rights = Access.Rights.ReadOnly;
    else if(Rights == "ReadWrite")
     member.Rights = Access.Rights.ReadWrite;
    else
     throw new Exception("Invalid Rights Specified");
    col.Commit(member);
   }
   else
   {
                try
                {
                    POBox poBox = Simias.POBox.POBox.FindPOBox(store,
                            col.Domain,
                            store.GetUserIDFromDomainID(col.Domain));
                    if (poBox == null)
                    {
                        throw new Exception("Unable to access POBox");
                    }
                    ICSList poList = poBox.Search(
                                Subscription.ToIdentityProperty,
                                UserID,
                                SearchOp.Equal);
                    Subscription sub = null;
                    foreach (ShallowNode sNode in poList)
                    {
                        sub = new Subscription(poBox, sNode);
                        break;
                    }
                    if (sub == null)
                    {
                        throw new Exception("Invalid UserID");
                    }
                    if (Rights == "Admin")
                        sub.SubscriptionRights = Access.Rights.Admin;
                    else if (Rights == "ReadOnly")
                        sub.SubscriptionRights = Access.Rights.ReadOnly;
                    else if (Rights == "ReadWrite")
                        sub.SubscriptionRights = Access.Rights.ReadWrite;
                    else
                        throw new Exception("Invalid Rights Specified");
                    poBox.Commit(sub);
                }
                catch (Exception ex)
                {
                }
   }
  }
  [WebMethod(EnableSession=true, Description="Get the Owner of an iFolder")]
  [SoapDocumentMethod]
  public iFolderUser GetOwner( string iFolderID )
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   Domain domain = store.GetDomain( col.Domain);
   iFolderUser user = new iFolderUser( domain, col.Owner );
   return user;
  }
  [WebMethod(EnableSession=true, Description="Changes the owner of an iFolder and sets the rights of the previous owner to the rights specified.")]
  [SoapDocumentMethod]
  public void ChangeOwner( string iFolderID,
         string NewOwnerUserID,
         string OldOwnerRights)
  {
   SharedCollection.ChangeOwner(iFolderID, NewOwnerUserID,
             OldOwnerRights);
  }
  [WebMethod(EnableSession=true, Description="Remove a single member from an iFolder")]
  [SoapDocumentMethod]
  public void RemoveiFolderUser( string iFolderID,
          string UserID)
  {
   SharedCollection.RemoveMember(iFolderID, UserID);
  }
  [WebMethod(EnableSession=true, Description="Get the list of iFolder Members")]
  [SoapDocumentMethod]
  public iFolderUser[] GetiFolderUsers(string iFolderID)
  {
   ArrayList members = new ArrayList();
   ICSList memberList;
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new SimiasException("Invalid iFolderID");
   Simias.Storage.Domain domain = store.GetDomain( col.Domain );
   if ( domain == null )
   {
    throw new SimiasException( "iFolderID isn't linked to a valid domain " );
   }
   memberList = col.GetMemberList();
   foreach( ShallowNode sNode in memberList )
   {
    if ( sNode.Type.Equals( "Member" ) )
    {
     members.Add( new iFolderUser( domain, new Member( col, sNode ) ) );
    }
   }
            POBox poBox;
            try
            {
                poBox = Simias.POBox.POBox.FindPOBox(store,
                    col.Domain,
                    store.GetUserIDFromDomainID(col.Domain));
            }
            catch (Exception ex)
            {
                poBox = null;
            }
            if (poBox != null)
            {
                ICSList poList = poBox.Search(
                    Subscription.SubscriptionCollectionIDProperty,
                    col.ID,
                    SearchOp.Equal);
                foreach (ShallowNode sNode in poList)
                {
                    Subscription sub = new Subscription(poBox, sNode);
                    if (sub.SubscriptionState == SubscriptionStates.Ready)
                    {
                        if (poBox.StoreReference.GetCollectionByID(
                            sub.SubscriptionCollectionID) != null)
                        {
                            continue;
                        }
                    }
                    members.Add(new iFolderUser(sub));
                }
            }
            if (members.Count > 0)
            {
                UserComparer comparer = new UserComparer();
                members.Sort(0, members.Count, comparer);
            }
            return (iFolderUser[])(members.ToArray(typeof(iFolderUser)));
  }
  [WebMethod(EnableSession=true, Description="Get a scoped list of iFolderUsers for the specified domain")]
  [SoapDocumentMethod]
  public iFolderUser[] GetDomainUsers(string DomainID, int numUsers)
  {
   int userCount = 0;
   ArrayList members = new ArrayList();
   Domain domain = Store.GetStore().GetDomain( DomainID );
   if( domain == null )
   {
    throw new SimiasException( "Invalid domain ID " );
   }
   ICSList memberList = domain.GetMemberList();
   foreach( ShallowNode sNode in memberList )
   {
    if ( sNode.Type.Equals( "Member" ) )
    {
     if( numUsers != -1 && ++userCount > numUsers )
     {
      members.Clear();
      break;
     }
     members.Add( new iFolderUser( domain, new Member( domain, sNode ) ) );
    }
   }
   if ( members.Count > 0 )
   {
    UserComparer comparer = new UserComparer();
    members.Sort( 0, members.Count, comparer );
   }
   return ( iFolderUser[] )( members.ToArray( typeof( iFolderUser ) ) );
  }
  [WebMethod(EnableSession=true, Description="Search for a Member of a specified name in the specified domain.")]
  [SoapDocumentMethod]
  public iFolderUser[] SearchForDomainUsers(string DomainID, string SearchString)
  {
   ArrayList members = new ArrayList();
   Hashtable matches = new Hashtable();
   Domain domain = Store.GetStore().GetDomain( DomainID );
   if( domain == null )
   {
    throw new SimiasException( "Invalid domain ID " );
   }
   ICSList searchList = domain.Search( PropertyTags.FullName, SearchString, SearchOp.Begins );
   foreach( ShallowNode sNode in searchList )
   {
    if ( sNode.Type.Equals( "Member" ) )
    {
     Member member = new Member( domain, sNode );
     matches.Add( sNode.ID, member );
     members.Add( new iFolderUser( domain, member ) );
    }
   }
   searchList = domain.Search( PropertyTags.Family, SearchString, SearchOp.Begins );
   foreach( ShallowNode sNode in searchList )
   {
    if ( sNode.Type.Equals( "Member" ) )
    {
     if ( matches.Contains( sNode.ID ) == false )
     {
      members.Add( new iFolderUser( domain, new Member( domain, sNode ) ) );
      matches.Add( sNode.ID, null );
     }
    }
   }
   searchList = domain.Search( BaseSchema.ObjectName, SearchString, SearchOp.Begins );
   foreach( ShallowNode sNode in searchList )
   {
    if ( sNode.Type.Equals( "Member" ) )
    {
     if ( matches.Contains( sNode.ID ) == false )
     {
      members.Add( new iFolderUser( domain, new Member( domain, sNode ) ) );
      matches.Add( sNode.ID, null );
     }
    }
   }
   if ( members.Count > 0 )
   {
    UserComparer comparer = new UserComparer();
    members.Sort( 0, members.Count, comparer );
   }
   return ( iFolderUser[] )( members.ToArray( typeof( iFolderUser ) ) );
  }
  [WebMethod(EnableSession=true, Description="End the search for domain members.")]
  [SoapDocumentMethod]
  public void FindCloseiFolderMembers( string domainID, string searchContext )
  {
   DomainProvider.FindCloseDomainMembers( domainID, searchContext );
  }
  [WebMethod(EnableSession=true, Description="Starts a search for all domain members.")]
  [SoapDocumentMethod]
  public bool FindFirstiFolderMembers(
   string domainID,
   int count,
   out string searchContext,
   out iFolderUser[] memberList,
   out int totalMembers )
  {
   Member[] tempList;
   bool moreEntries =
    DomainProvider.FindFirstDomainMembers(
     domainID,
     count,
     out searchContext,
     out tempList,
     out totalMembers );
   if ( ( tempList != null ) && ( tempList.Length > 0 ) )
   {
    Domain domain = Store.GetStore().GetDomain( domainID );
    memberList = new iFolderUser[ tempList.Length ];
    for ( int i = 0; i < tempList.Length; ++i )
    {
     memberList[ i ] = new iFolderUser( domain, tempList[ i ] );
    }
   }
   else
   {
    memberList = null;
   }
   return moreEntries;
  }
  [WebMethod(EnableSession=true, Description="Starts a search for a specific set of domain members.")]
  [SoapDocumentMethod]
  public bool FindFirstSpecificiFolderMembers(
   string domainID,
   string attributeName,
   string searchString,
   iFolderSearchType operation,
   int count,
   out string searchContext,
   out iFolderUser[] memberList,
   out int totalMembers )
  {
   Member[] tempList;
   bool moreEntries =
    DomainProvider.FindFirstDomainMembers(
     domainID,
     attributeName,
     searchString,
     ( Simias.Storage.SearchOp )Enum.ToObject( typeof( Simias.Storage.SearchOp ), operation ),
     count,
     out searchContext,
     out tempList,
     out totalMembers );
   if ( ( tempList != null ) && ( tempList.Length > 0 ) )
   {
    Domain domain = Store.GetStore().GetDomain( domainID );
    memberList = new iFolderUser[ tempList.Length ];
    for ( int i = 0; i < tempList.Length; ++i )
    {
     memberList[ i ] = new iFolderUser( domain, tempList[ i ] );
    }
   }
   else
   {
    memberList = null;
   }
   return moreEntries;
  }
  [WebMethod(EnableSession=true, Description="Continues the search for domain members from the current record location.")]
  [SoapDocumentMethod]
  public bool FindNextiFolderMembers(
   string domainID,
   ref string searchContext,
   int count,
   out iFolderUser[] memberList )
  {
   Member[] tempList;
   bool moreEntries = DomainProvider.FindNextDomainMembers( domainID, ref searchContext, count, out tempList );
   if ( ( tempList != null ) && ( tempList.Length > 0 ) )
   {
    Domain domain = Store.GetStore().GetDomain( domainID );
    memberList = new iFolderUser[ tempList.Length ];
    for ( int i = 0; i < tempList.Length; ++i )
    {
     memberList[ i ] = new iFolderUser( domain, tempList[ i ] );
    }
   }
   else
   {
    memberList = null;
   }
   return moreEntries;
  }
  [WebMethod(EnableSession=true, Description="Continues the search for domain members previous to the current record location.")]
  [SoapDocumentMethod]
  public bool FindPreviousiFolderMembers(
   string domainID,
   ref string searchContext,
   int count,
   out iFolderUser[] memberList )
  {
   Member[] tempList;
   bool moreEntries = DomainProvider.FindPreviousDomainMembers( domainID, ref searchContext, count, out tempList );
   if ( ( tempList != null ) && ( tempList.Length > 0 ) )
   {
    Domain domain = Store.GetStore().GetDomain( domainID );
    memberList = new iFolderUser[ tempList.Length ];
    for ( int i = 0; i < tempList.Length; ++i )
    {
     memberList[ i ] = new iFolderUser( domain, tempList[ i ] );
    }
   }
   else
   {
    memberList = null;
   }
   return moreEntries;
  }
  [WebMethod(EnableSession=true, Description="Continues the search for domain members from the specified record location.")]
  [SoapDocumentMethod]
  public bool FindSeekiFolderMembers(
   string domainID,
   ref string searchContext,
   int offset,
   int count,
   out iFolderUser[] memberList )
  {
   Member[] tempList;
   bool moreEntries = DomainProvider.FindSeekDomainMembers(
    domainID,
    ref searchContext,
    offset,
    count,
    out tempList );
   if ( ( tempList != null ) && ( tempList.Length > 0 ) )
   {
    Domain domain = Store.GetStore().GetDomain( domainID );
    memberList = new iFolderUser[ tempList.Length ];
    for ( int i = 0; i < tempList.Length; ++i )
    {
     memberList[ i ] = new iFolderUser( domain, tempList[ i ] );
    }
   }
   else
   {
    memberList = null;
   }
   return moreEntries;
  }
  [WebMethod(EnableSession=true, Description="Lookup a single member to a collection")]
  [SoapDocumentMethod]
  public iFolderUser GetiFolderUser( string UserID )
  {
   Store store = Store.GetStore();
   Domain domain = store.GetDomainForUser(UserID);
   if(domain == null)
    throw new Exception("Unable to access domain");
   Simias.Storage.Member simMem = domain.GetMemberByID(UserID);
   if(simMem == null)
    throw new Exception("Invalid UserID");
   return new iFolderUser( domain, simMem );
  }
  [WebMethod(EnableSession=true, Description="Lookup a single member to a collection")]
  [SoapDocumentMethod]
  public string GetRAName( string DomainID )
  {
   Store store = Store.GetStore();
   Domain domain = store.GetDomain(DomainID);
   if(domain == null)
    throw new Exception("Unable to access domain");
   Simias.Storage.Member member = domain.GetCurrentMember();
   if(member == null)
    throw new Exception("Invalid UserID");
   return member.RAName;
  }
  [WebMethod(EnableSession=true, Description="Lookup a user in a collection based on node ID.")]
  [SoapDocumentMethod]
  public iFolderUser GetiFolderUserFromNodeID(string CollectionID,
             string NodeID)
  {
   iFolderUser ifolderUser = null;
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(CollectionID);
   if(col != null)
   {
    Node node = col.GetNodeByID(NodeID);
    if(node != null)
    {
     Domain domain = store.GetDomain( col.Domain );
     if (col.IsBaseType(node, NodeTypes.MemberType))
     {
      ifolderUser = new iFolderUser( domain, new Member( node ) );
     }
     else if (col.IsType(node, typeof( Subscription ).Name))
     {
      ifolderUser = new iFolderUser( new Subscription( node ) );
     }
    }
   }
   return ifolderUser;
  }
  [WebMethod(EnableSession=true, Description="Invite a user to an iFolder.  This call will only work with Enterprise iFolders")]
  [SoapDocumentMethod]
  public iFolderUser AddAndInviteUser(string iFolderID,
           string MemberName,
           string GivenName,
           string FamilyName,
           string MemberID,
           string PublicKey,
           string Rights)
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   Domain domain = store.GetDomain(col.Domain);
   if(domain == null)
    throw new Exception("Unable to access domain");
   Simias.Storage.Member member = domain.GetMemberByID(MemberID);
   if(member == null)
   {
    bool given;
    member = new Simias.Storage.Member( MemberName, MemberID, Access.Rights.ReadOnly );
    if ( PublicKey != null )
    {
     member.Properties.AddProperty( "PublicKey", PublicKey );
    }
    if ( GivenName != null && GivenName != "" )
    {
     member.Given = GivenName;
     given = true;
    }
    else
    {
     given = false;
    }
    if ( FamilyName != null && FamilyName != "" )
    {
     member.Family = FamilyName;
     if ( given == true )
     {
      member.FN = GivenName + " " + FamilyName;
     }
    }
    domain.Commit( member );
   }
   Access.Rights newRights;
   if(Rights == "Admin")
    newRights = Access.Rights.Admin;
   else if(Rights == "ReadOnly")
    newRights = Access.Rights.ReadOnly;
   else if(Rights == "ReadWrite")
    newRights = Access.Rights.ReadWrite;
   else
    throw new Exception("Invalid Rights Specified");
   Member membercoll = new Member(MemberName, MemberID, newRights);
   col.Commit(membercoll);
   iFolderUser user = new iFolderUser(domain, membercoll );
   return user;
  }
  [WebMethod(EnableSession=true, Description="Invite a user to an iFolder.  This call will only work with Enterprise iFolders")]
  [SoapDocumentMethod]
  public iFolderUser InviteUser( string iFolderID,
          string UserID,
          string Rights)
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   Domain domain = store.GetDomain(col.Domain);
   if(domain == null)
    throw new Exception("Unable to access domain");
   Simias.Storage.Member member = domain.GetMemberByID(UserID);
   if(member == null)
    throw new Exception("Invalid UserID");
   Access.Rights newRights;
   if(Rights == "Admin")
    newRights = Access.Rights.Admin;
   else if(Rights == "ReadOnly")
    newRights = Access.Rights.ReadOnly;
   else if(Rights == "ReadWrite")
    newRights = Access.Rights.ReadWrite;
   else
    throw new Exception("Invalid Rights Specified");
   POBox poBox = Simias.POBox.POBox.FindPOBox(store,
      domain.ID,
      store.GetUserIDFromDomainID(domain.ID));
   Subscription sub = poBox.CreateSubscription(col,
          col.GetCurrentMember(),
          "iFolder");
   sub.SubscriptionRights = newRights;
   sub.ToName = member.Name;
   sub.ToIdentity = UserID;
   poBox.AddMessage(sub);
   iFolderUser user = new iFolderUser( sub );
   return user;
  }
  [WebMethod(EnableSession=true, Description="Accept an invitation fo an iFolder.  The iFolder ID represents a Subscription object")]
  [SoapDocumentMethod]
  public iFolderWeb MergeiFolder( string DomainID,
          string iFolderID,
          string path)
                {
                        Store store = Store.GetStore ();
                        CollectionInfo cinfo = DiscoveryFramework.GetLocalCollectionInfo (iFolderID);
                        if(cinfo == null)
                            cinfo = DiscoveryFramework.GetCollectionInfo(iFolderID);
                        if(cinfo == null)
                                throw new Exception("Invalid iFolderID");
                        CollectionPathStatus pStatus;
                        pStatus = SharedCollection.CheckCollectionPath(path);
                        switch(pStatus)
                        {
                                case CollectionPathStatus.ValidPath:
                                        break;
                                case CollectionPathStatus.RootOfDrivePath:
                                        throw new Exception("RootOfDrivePath");
                                case CollectionPathStatus.InvalidCharactersPath:
                                        throw new Exception("InvalidCharactersPath");
                                case CollectionPathStatus.AtOrInsideStorePath:
                                        throw new Exception("AtOrInsideStorePath");
                                case CollectionPathStatus.ContainsStorePath:
                                        throw new Exception("ContainsStorePath");
                                case CollectionPathStatus.NotFixedDrivePath:
                                        throw new Exception("NotFixedDrivePath");
                                case CollectionPathStatus.SystemDirectoryPath:
                                        throw new Exception("SystemDirectoryPath");
                                case CollectionPathStatus.SystemDrivePath:
                                        throw new Exception("SystemDrivePath");
                                case CollectionPathStatus.IncludesWinDirPath:
                                        throw new Exception("IncludesWinDirPath");
                                case CollectionPathStatus.IncludesProgFilesPath:
                                        throw new Exception("IncludesProgFilesPath");
                                case CollectionPathStatus.DoesNotExistPath:
                                        throw new Exception("DoesNotExistPath");
                                case CollectionPathStatus.NoReadRightsPath:
                                        throw new Exception("NoReadRightsPath");
                                case CollectionPathStatus.NoWriteRightsPath:
                                        throw new Exception("NoWriteRightsPath");
                                case CollectionPathStatus.ContainsCollectionPath:
                                        throw new Exception("ContainsCollectionPath");
                                case CollectionPathStatus.AtOrInsideCollectionPath:
                                        throw new Exception("AtOrInsideCollectionPath");
                        }
   DiscoveryFramework.CreateProxy (store, cinfo, DomainID, iFolderID, Path.GetFullPath (path));
   Collection coll = store.GetCollectionByID(cinfo.ID);
   coll.Merge = true;
   coll.Commit();
   iFolderWeb ifolder = new iFolderWeb(cinfo);
    ifolder.State = "Available";
   ifolder.Role = "Master";
    ifolder.UnManagedPath = path;
    ifolder.IsSubscription = false;
   return ifolder;
  }
  [WebMethod(EnableSession=true, Description="Accept an invitation fo an iFolder.  The iFolder ID represents a Subscription object")]
  [SoapDocumentMethod]
  public iFolderWeb AcceptiFolderInvitation( string DomainID,
          string iFolderID,
          string LocalPath)
  {
          Store store = Store.GetStore ();
          CollectionInfo cinfo = DiscoveryFramework.GetLocalCollectionInfo (iFolderID);
            if(cinfo == null)
                cinfo = DiscoveryFramework.GetCollectionInfo(iFolderID);
    if(cinfo == null)
     throw new Exception("Invalid iFolderID");
   string path = Path.Combine(LocalPath, cinfo.DirNodeName);
   if (Directory.Exists(path))
    throw new Exception("PathExists");
   CollectionPathStatus pStatus;
   pStatus = SharedCollection.CheckCollectionPath(path);
   switch(pStatus)
   {
    case CollectionPathStatus.ValidPath:
     break;
    case CollectionPathStatus.RootOfDrivePath:
     throw new Exception("RootOfDrivePath");
    case CollectionPathStatus.InvalidCharactersPath:
     throw new Exception("InvalidCharactersPath");
    case CollectionPathStatus.AtOrInsideStorePath:
     throw new Exception("AtOrInsideStorePath");
    case CollectionPathStatus.ContainsStorePath:
     throw new Exception("ContainsStorePath");
    case CollectionPathStatus.NotFixedDrivePath:
     throw new Exception("NotFixedDrivePath");
    case CollectionPathStatus.SystemDirectoryPath:
     throw new Exception("SystemDirectoryPath");
    case CollectionPathStatus.SystemDrivePath:
     throw new Exception("SystemDrivePath");
    case CollectionPathStatus.IncludesWinDirPath:
     throw new Exception("IncludesWinDirPath");
    case CollectionPathStatus.IncludesProgFilesPath:
     throw new Exception("IncludesProgFilesPath");
    case CollectionPathStatus.DoesNotExistPath:
     throw new Exception("DoesNotExistPath");
    case CollectionPathStatus.NoReadRightsPath:
     throw new Exception("NoReadRightsPath");
    case CollectionPathStatus.NoWriteRightsPath:
     throw new Exception("NoWriteRightsPath");
    case CollectionPathStatus.ContainsCollectionPath:
     throw new Exception("ContainsCollectionPath");
    case CollectionPathStatus.AtOrInsideCollectionPath:
     throw new Exception("AtOrInsideCollectionPath");
   }
   DiscoveryFramework.CreateProxy (store, cinfo, DomainID, iFolderID, Path.GetFullPath (path));
   iFolderWeb ifolder = new iFolderWeb(cinfo);
    ifolder.State = "Available";
   ifolder.Role = "Master";
    ifolder.UnManagedPath = path;
    ifolder.IsSubscription = false;
   return ifolder;
  }
                [WebMethod(EnableSession=true, Description="Accept an invitation fo an iFolder.  The iFolder ID represents a Subscription object")]
                [SoapDocumentMethod]
                public iFolderWeb AcceptiFolderInvitation1( string DomainID,
                                                                                                   string iFolderID,
                                                                                                string LocalPath)
                {
                        Store store = Store.GetStore();
                        POBox poBox = Simias.POBox.POBox.FindPOBox(store,
                                                DomainID,
                                                store.GetUserIDFromDomainID(DomainID));
                        Node node = poBox.GetNodeByID(iFolderID);
                        if(node == null)
                                throw new Exception("Invalid iFolderID : id : "+ iFolderID);
                        Subscription sub = new Subscription(node);
                        string path = Path.Combine(LocalPath, sub.DirNodeName);
                        if (Directory.Exists(path))
                                throw new Exception("PathExists");
                        CollectionPathStatus pStatus;
   pStatus = SharedCollection.CheckCollectionPath(path);
                        switch(pStatus)
                        {
                                case CollectionPathStatus.ValidPath:
                                        break;
                                case CollectionPathStatus.RootOfDrivePath:
                                        throw new Exception("RootOfDrivePath");
                                case CollectionPathStatus.InvalidCharactersPath:
                                        throw new Exception("InvalidCharactersPath");
                                case CollectionPathStatus.AtOrInsideStorePath:
                                        throw new Exception("AtOrInsideStorePath");
                                case CollectionPathStatus.ContainsStorePath:
                                        throw new Exception("ContainsStorePath");
                                case CollectionPathStatus.NotFixedDrivePath:
                                        throw new Exception("NotFixedDrivePath");
                                case CollectionPathStatus.SystemDirectoryPath:
                                        throw new Exception("SystemDirectoryPath");
                                case CollectionPathStatus.SystemDrivePath:
                                        throw new Exception("SystemDrivePath");
                                case CollectionPathStatus.IncludesWinDirPath:
                                        throw new Exception("IncludesWinDirPath");
                                case CollectionPathStatus.IncludesProgFilesPath:
                                        throw new Exception("IncludesProgFilesPath");
                                case CollectionPathStatus.DoesNotExistPath:
                                        throw new Exception("DoesNotExistPath");
                                case CollectionPathStatus.NoReadRightsPath:
                                        throw new Exception("NoReadRightsPath");
                                case CollectionPathStatus.NoWriteRightsPath:
                                        throw new Exception("NoWriteRightsPath");
                                case CollectionPathStatus.ContainsCollectionPath:
                                        throw new Exception("ContainsCollectionPath");
                                case CollectionPathStatus.AtOrInsideCollectionPath:
                                        throw new Exception("AtOrInsideCollectionPath");
                        }
                        sub.CollectionRoot = Path.GetFullPath(LocalPath);
                        if(sub.SubscriptionState == SubscriptionStates.Ready)
                        {
                                poBox.Commit(sub);
    sub.CreateSlave(store);
                        }
                        else
                        {
                                sub.Accept(store, SubscriptionDispositions.Accepted);
                                poBox.Commit(sub);
                        }
                        iFolderWeb ifolder = new iFolderWeb(sub);
                        return ifolder;
                }
  [WebMethod(EnableSession=true, Description="Check for newer Mac client availability.")]
  [SoapDocumentMethod]
  public int CheckForMacUpdate( string DomainID, string CurrentVersion, out string ServerVersion)
  {
   string serverVersion = null;
                        int status = Novell.iFolder.Install.ClientUpgrade.CheckForMacUpdate(DomainID, CurrentVersion, out serverVersion);
                        ServerVersion = serverVersion;
                        Store store = Store.GetStore();
                        Domain dom = store.GetDomain(DomainID);
                        if (dom != null)
                        {
                                Version ver = new Version(serverVersion);
                                dom.ServerVersion = ver;
                                dom.Commit();
                        }
                        return status;
  }
  [WebMethod(EnableSession=true, Description="Decline an invitation to an iFolder.  The iFolder ID represents a Subscription object")]
  [SoapDocumentMethod]
  public void DeclineiFolderInvitation( string DomainID, string iFolderID )
  {
   DiscoveryFramework.RemoveMembership (DomainID, iFolderID);
  }
  [WebMethod(EnableSession=true, Description="Gets the DiskSpaceQuota for a member")]
  [SoapDocumentMethod]
  public DiskSpace GetUserDiskSpace( string UserID )
  {
   try
   {
    return DiskSpace.GetMemberDiskSpace(UserID);
   }
   catch(Exception e)
   {
    throw e;
   }
  }
  [WebMethod(EnableSession=true, Description="Gets the DiskSpaceQuota for an iFolder")]
  [SoapDocumentMethod]
  public DiskSpace GetiFolderDiskSpace( string iFolderID )
  {
   return DiskSpace.GetiFolderDiskSpace(iFolderID);
  }
  [WebMethod(EnableSession=true, Description="Sets the Disk Space Limit for a user")]
  [SoapDocumentMethod]
  public void SetUserDiskSpaceLimit( string UserID, long Limit )
  {
   DiskSpace.SetUserDiskSpaceLimit(UserID, Limit);
  }
  [WebMethod(EnableSession=true, Description="Sets the Disk Space Limit for an iFolder")]
  [SoapDocumentMethod]
  public void SetiFolderDiskSpaceLimit( string iFolderID, long Limit )
  {
   DiskSpace.SetiFolderDiskSpaceLimit(iFolderID, Limit);
  }
        [WebMethod(EnableSession = true, Description = "Sets the Disk Space Limit for an iFolder")]
        [SoapDocumentMethod]
        public void SetiFolderSecureSync(string iFolderID, bool ssl)
        {
            Store store = Store.GetStore();
            Collection col = store.GetCollectionByID(iFolderID);
            if (col == null)
                throw new Exception("Invalid iFolderID");
            col.SSL = ssl;
            col.Commit();
        }
  [WebMethod(EnableSession=true, Description="Sets the Sync Interval for an iFolder")]
  [SoapDocumentMethod]
  public void SetiFolderSyncInterval( string iFolderID, int Interval )
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   Simias.Policy.SyncInterval.Set(col, Interval);
  }
  [WebMethod(EnableSession=true, Description="Sets the Default Sync Interval")]
  [SoapDocumentMethod]
  public void SetDefaultSyncInterval( int Interval )
  {
   Simias.Policy.SyncInterval.Set( Interval );
  }
  [WebMethod(EnableSession=true, Description="Gets the Default Sync Interval")]
  [SoapDocumentMethod]
  public int GetDefaultSyncInterval()
  {
   return Simias.Policy.SyncInterval.GetInterval();
  }
  [WebMethod(EnableSession=true, Description="Connects to an iFolder Domain")]
  [SoapDocumentMethod]
  public int AuthenticateToDomain( string DomainID,
           string Password)
  {
   Store store = Store.GetStore();
   Domain domain = store.GetDomain(DomainID);
   if(domain == null)
    throw new Exception("ERROR:Invalid Domain ID");
   Member member = domain.GetCurrentMember();
   if(member == null)
    throw new Exception("ERROR:Unable locate user");
   DomainService domainSvc = new DomainService();
   Uri uri = DomainProvider.ResolveLocation(DomainID);
   if (uri == null)
    throw new Exception("ERROR:No host address for domain");
   domainSvc.Url = uri.ToString() + "/DomainService.asmx";
   domainSvc.Credentials =
    new NetworkCredential(member.Name, Password);
   try
   {
    domainSvc.GetDomainInfo(member.UserID);
   }
   catch(WebException webEx)
   {
    if (webEx.Status == System.Net.WebExceptionStatus.ProtocolError ||
     webEx.Status == System.Net.WebExceptionStatus.TrustFailure)
    {
     throw new Exception("ERROR: Invalid Credentials");
    }
    else if (webEx.Status == System.Net.WebExceptionStatus.ConnectFailure)
    {
     throw new Exception("ERROR: Domain Connection failed");
    }
    else
     throw webEx;
   }
   return 0;
  }
  [WebMethod(EnableSession=true, Description="Connects to an iFolder Enterprise Server")]
  [SoapDocumentMethod]
  public Conflict[] GetiFolderConflicts( string iFolderID )
  {
   ArrayList list = new ArrayList();
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   ICSList collisionList = col.GetCollisions();
   foreach(ShallowNode sn in collisionList)
   {
    Node conflictNode = col.GetNodeByID(sn.ID);
    Conflict conflict = new Conflict(col, conflictNode);
    list.Add(conflict);
   }
   return (Conflict[])list.ToArray(typeof(Conflict));
  }
  [WebMethod(EnableSession=true, Description="Resolves a file conflict in an iFolder.")]
  [SoapDocumentMethod]
  public void ResolveFileConflict(string iFolderID, string conflictID, bool localChangesWin)
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   Node conflictNode = col.GetNodeByID(conflictID);
   if(conflictNode == null)
    throw new Exception("Invalid conflictID");
   Conflict.Resolve(col, conflictNode, localChangesWin);
  }
  [WebMethod(EnableSession=true, Description="Resolves a file conflict in an iFolder.")]
  [SoapDocumentMethod]
  public void ResolveEnhancedFileConflict(string iFolderID, string conflictID, bool localChangesWin,
          string conflictBinPath)
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   Node conflictNode = col.GetNodeByID(conflictID);
   if(conflictNode == null)
    throw new Exception("Invalid conflictID");
   Conflict.Resolve(col, conflictNode, localChangesWin, conflictBinPath);
  }
  [WebMethod(EnableSession=true, Description="Resolves a name conflict")]
  [SoapDocumentMethod]
  public void ResolveNameConflict(string iFolderID, string conflictID,
          string newLocalName)
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   Node conflictNode = col.GetNodeByID(conflictID);
   if(conflictNode == null)
    throw new Exception("Invalid conflictID");
   Conflict.Resolve(col, conflictNode, newLocalName);
  }
  [WebMethod(EnableSession=true, Description="Renames a file and resolves a name conflict")]
  [SoapDocumentMethod]
  public void RenameAndResolveConflict(string iFolderID, string conflictID, string newFileName)
  {
   Collection col = Store.GetStore().GetCollectionByID(iFolderID);
   if (col == null)
    throw new Exception("Invalid iFolderID");
   Node conflictNode = col.GetNodeByID(conflictID);
   if (conflictNode == null)
    throw new Exception("Invalid conflictID");
   Conflict.RenameConflictingAndResolve(col, conflictNode, newFileName);
  }
  [WebMethod(EnableSession=true, Description="Sets up a proxy for iFolder to use")]
  [SoapDocumentMethod]
  public void SetupProxy(string Host, int Port)
  {
  }
  [WebMethod(EnableSession=true, Description="Removes proxy settings")]
  [SoapDocumentMethod]
  public void RemoveProxy()
  {
  }
  [WebMethod(EnableSession=true, Description="Calculates the number of nodes and bytes that need to be sync'd.")]
  [SoapDocumentMethod]
  public SyncSize CalculateSyncSize(string iFolderID)
  {
   Collection col = Store.GetStore().GetCollectionByID(iFolderID);
   if (col == null)
   {
    throw new Exception("Invalid iFolderID");
   }
   uint SyncNodeCount;
   ulong SyncByteCount;
   SharedCollection.CalculateSendSize(col, out SyncNodeCount, out SyncByteCount);
   return new SyncSize(SyncNodeCount, SyncByteCount);
  }
  [WebMethod(EnableSession=true, Description="Sends a command to the sync engine to sync the iFolder of the specified ID.")]
  [SoapDocumentMethod]
  public void SynciFolderNow(string iFolderID)
  {
   SharedCollection.SyncCollectionNow(iFolderID);
  }
  [WebMethod(EnableSession=true, Description="Delete a file size limit policy from an iFolder")]
  [SoapDocumentMethod]
  public void DeleteiFolderFileSizeLimit(string iFolderID)
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   FileSizeFilter.Delete(col);
  }
  [WebMethod(EnableSession=true, Description="Get a users file size limit on an iFolder")]
  [SoapDocumentMethod]
  public long GetMemberiFolderFileSizeLimit(string UserID, string iFolderID)
  {
   Store store = Store.GetStore();
   Domain domain = store.GetDomainForUser(UserID);
   if(domain == null)
    throw new Exception("Unable to access domain");
   Simias.Storage.Member member = domain.GetMemberByID(UserID);
   if(member == null)
    throw new Exception("Invalid UserID");
   Collection col = store.GetCollectionByID(iFolderID);
   if (col == null)
    throw new Exception("Invalid iFolderID");
   FileSizeFilter filter = FileSizeFilter.Get(member, col);
   if(filter == null)
    throw new Exception("Unable to get File Size Limit");
   return filter.Limit;
  }
  [WebMethod(EnableSession=true, Description="Get the file size limit of an iFolder")]
  [SoapDocumentMethod]
  public long GetiFolderFileSizeLimit(string iFolderID)
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if (col == null)
    throw new Exception("Invalid iFolderID");
   return FileSizeFilter.GetLimit(col);
  }
  [WebMethod(EnableSession=true, Description="Set the file size limit of an iFolder")]
  [SoapDocumentMethod]
  public void SetiFolderFileSizeLimit(string iFolderID, long Limit)
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if (col == null)
    throw new Exception("Invalid iFolderID");
   FileSizeFilter.Set(col, Limit);
  }
  [WebMethod(Description="Check for an available update")]
  [SoapDocumentMethod]
  public string CheckForUpdatedClientAvailable(string domainID)
  {
   return Novell.iFolder.Install.ClientUpgrade.CheckForUpdateAvailable(domainID);
  }
  [WebMethod(Description="Check for an available update")]
  [SoapDocumentMethod]
  public string CheckForUpdatedClient(string domainID)
  {
   return null;
  }
  [WebMethod(Description="Check for an available update")]
  [SoapDocumentMethod]
  public int CheckForUpdate(string domainID, out string ServerVersion)
  {
   string serverVersion = null;
   int status = Novell.iFolder.Install.ClientUpgrade.CheckForUpdate(domainID, out serverVersion);
   ServerVersion = serverVersion;
              Store store = Store.GetStore();
              Domain dom = store.GetDomain(domainID);
              if (dom != null)
              {
                  if (dom.Owner != null)
                  {
                   Version ver = new Version(serverVersion);
                   dom.ServerVersion = ver;
                   dom.Commit();
    }
              }
   return status;
  }
  [WebMethod(Description="Check for an available update")]
  [SoapDocumentMethod]
  public bool CheckForServerUpdate(string domainID)
  {
   return Novell.iFolder.Install.ClientUpgrade.CheckForServerUpdate(domainID);
  }
  [WebMethod(EnableSession=true, Description="Run the client update")]
  [SoapDocumentMethod]
  public bool RunClientUpdate(string domainID, string path)
  {
   Console.WriteLine("RunClientUpdate web service called");
   return Novell.iFolder.Install.ClientUpgrade.RunUpdate(domainID, path);
  }
         [WebMethod(EnableSession = true, Description = "Run the client update")]
         [SoapDocumentMethod]
         public int ChangePassword(string domainid, string oldpassword, string newpassword)
         {
             int retval = -1;
             try
             {
                 Simias.Storage.Store store = Simias.Storage.Store.GetStore();
                 Domain domain = store.GetDomain(domainid);
                 Member member = domain.GetCurrentMember();
                 retval = member.ChangePassword(oldpassword, newpassword);
             }
             catch { }
             return retval;
         }
  [WebMethod(EnableSession = true, Description = "Gets the Default public key for iFolder key encryption")]
  [SoapDocumentMethod]
  public string GetDefaultServerPublicKey(string DomainID, string UserID)
  {
   return SharedCollection.GetDefaultPublicKey(DomainID, UserID);
  }
 }
}
