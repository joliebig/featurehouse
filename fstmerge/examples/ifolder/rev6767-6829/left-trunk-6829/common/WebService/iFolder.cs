

using System;
using Simias.Storage;
using Simias.POBox;
using Simias.Discovery;
using Simias.Sync;
using System.Xml;
using System.Xml.Serialization;

namespace Novell.iFolder.Web
{




 [Serializable]
 public class iFolderWeb
 {


  public static readonly string FilesDirName = "SimiasFiles";


  public static readonly string iFolderType = "iFolder";



  public string DomainID;


  public string ID;


  public ulong LocalIncarnation;


  public string ManagedPath;


  public string UnManagedPath;


  public ulong MasterIncarnation;


           public string Name;


  public string Owner;


  public string OwnerID;


  public int EffectiveSyncInterval;


  public int SyncInterval;


  public bool Synchronizable;


  public string Type;


  public string Description;


  public string State;


  public bool IsSubscription;


  public int EnumeratedState;


  public bool IsWorkgroup;


  public bool HasConflicts;


  public string CurrentUserID;


  public string CurrentUserRights;


  public string CollectionID;


  public string LastSyncTime;


  public string Role;



  public bool ssl;

  public string encryptionAlgorithm;

  public int MigratediFolder;




  public bool shared;



  public iFolderWeb()
  {
  }





        public iFolderWeb(Collection collection, int infoToFetch)
        {
            Member tmpMember = null;


            this.DomainID = collection.Domain;
            this.ID = collection.ID;
            this.CollectionID = collection.ID;
            this.LocalIncarnation = collection.LocalIncarnation;


            if (infoToFetch > 0)
            {
                DirNode dirNode = collection.GetRootDirectory();
                if (dirNode != null)
                    this.UnManagedPath = dirNode.GetFullPath(collection);
                else
                    this.UnManagedPath = "";
                this.ManagedPath = collection.ManagedPath;
                this.MasterIncarnation = collection.MasterIncarnation;
                this.Name = collection.Name;
                DateTime lastSyncTime = Simias.Sync.SyncClient.GetLastSyncTime(collection.ID);
                if (collection.Role.Equals(SyncRoles.Master))
                {
                    this.LastSyncTime = string.Empty;
                    this.State = "Local";
                }
                else if (lastSyncTime.Equals(DateTime.MinValue))
                {
                    this.LastSyncTime = string.Empty;
                    this.State = "WaitSync";
                }
                else
                {
                    this.LastSyncTime = lastSyncTime.ToString();
                    this.State = collection.IsProxy ? "WaitSync" : "Local";
                }
                this.HasConflicts = collection.HasCollisions();

                tmpMember = collection.GetCurrentMember();

                this.CurrentUserID = tmpMember.UserID;
                this.CurrentUserRights = tmpMember.Rights.ToString();

                this.Role = collection.Role.ToString();
                this.ssl = collection.SSL;
                if (collection.EncryptionAlgorithm == null || collection.EncryptionAlgorithm == "")
                    this.encryptionAlgorithm = "";
                else
                    this.encryptionAlgorithm = collection.EncryptionAlgorithm;

                this.MigratediFolder = collection.MigratediFolder;

                tmpMember = collection.Owner;
                if (tmpMember != null)
                {
                    this.Owner = tmpMember.Name;
                    this.OwnerID = tmpMember.UserID;
                }
                else
                {
                    this.Owner = "Not available";
                    this.OwnerID = "0";
                }

                ICSList memberList;

                memberList = collection.GetMemberList();
                if (memberList.Count > 1)
                    this.shared = true;
                else
                    this.shared = false;

            }
        }




  public iFolderWeb(Collection collection)
  {
   Member tmpMember = null;
   this.DomainID = collection.Domain;
   this.ID = collection.ID;
   this.CollectionID = collection.ID;
   this.LocalIncarnation = collection.LocalIncarnation;
   DirNode dirNode = collection.GetRootDirectory();
   if(dirNode != null)
    this.UnManagedPath = dirNode.GetFullPath(collection);
   else
    this.UnManagedPath = "";
   this.ManagedPath = collection.ManagedPath;
   this.MasterIncarnation = collection.MasterIncarnation;
   this.Name = collection.Name;
         tmpMember = collection.Owner;
          if (tmpMember != null)
   {
    this.Owner = tmpMember.Name;
    this.OwnerID = tmpMember.UserID;
   }
   else
   {
    this.Owner = "Not available";
    this.OwnerID = "0";
   }

   this.SyncInterval =
    Simias.Policy.SyncInterval.GetInterval(collection);
   this.Synchronizable = collection.Synchronizable;
   this.Type = iFolderType;
   this.Description = "";
   this.IsSubscription = false;
   this.EnumeratedState = -1;



   this.IsWorkgroup = false;
   this.HasConflicts = collection.HasCollisions();

   tmpMember = collection.GetCurrentMember();

   this.CurrentUserID = tmpMember.UserID;
   this.CurrentUserRights = tmpMember.Rights.ToString();
   int EffectiveSync = tmpMember.EffectiveSyncPolicy(collection);
   if(EffectiveSync > 0)
   {
    this.EffectiveSyncInterval = EffectiveSync;

   }

   DateTime lastSyncTime = Simias.Sync.SyncClient.GetLastSyncTime(collection.ID);
   if (collection.Role.Equals(SyncRoles.Master))
   {
    this.LastSyncTime = string.Empty;
    this.State = "Local";
   }
   else if (lastSyncTime.Equals(DateTime.MinValue))
   {
    this.LastSyncTime = string.Empty;
    this.State = "WaitSync";
   }
   else
   {
    this.LastSyncTime = lastSyncTime.ToString();
    this.State = collection.IsProxy ? "WaitSync" : "Local";
   }

   this.Role = collection.Role.ToString();
   this.ssl = collection.SSL;
   if( collection.EncryptionAlgorithm == null || collection.EncryptionAlgorithm == "" )
    this.encryptionAlgorithm = "";
   else
    this.encryptionAlgorithm = collection.EncryptionAlgorithm;
   this.MigratediFolder = collection.MigratediFolder;
   ICSList memberList;

   memberList = collection.GetMemberList();
   if( memberList.Count >1)
    this.shared = true;
   else
    this.shared = false;
  }





  public iFolderWeb(Subscription subscription)
  {
   this.DomainID = subscription.DomainID;
   this.Name = subscription.SubscriptionCollectionName;
   this.ID = subscription.ID;
   this.CollectionID = subscription.SubscriptionCollectionID;
   this.Description = subscription.CollectionDescription;
   this.IsSubscription = true;
   this.EnumeratedState = (int) subscription.SubscriptionState;
   this.Owner = subscription.FromName;
   Domain domain = Store.GetStore().GetDomain(subscription.DomainID);
   if(domain != null)
   {
    Simias.Storage.Member member = domain.GetMemberByID(subscription.FromIdentity);
    if(member != null)
     this.Owner = member.FN;
   }

   this.OwnerID = subscription.FromIdentity;
   this.CurrentUserRights = subscription.SubscriptionRights.ToString();

   if( (subscription.SubscriptionState ==
        SubscriptionStates.Ready) ||
      (subscription.SubscriptionState ==
        SubscriptionStates.Received) )
   {
    this.State = "Available";
   }
   else if(subscription.SubscriptionState ==
         SubscriptionStates.Replied)
   {
    this.State = "WaitConnect";
   }
   else if(subscription.SubscriptionState ==
        SubscriptionStates.Delivered)
   {
    this.State = "WaitSync";
   }

   else
   {
    this.State = "Unknown";
   }
  }




  public iFolderWeb(CollectionInfo c)
  {
   this.DomainID = c.DomainID;
   this.Name = c.Name;
   this.ID = c.ID;
   this.CollectionID = c.CollectionID;
   this.Description = c.Description;


   this.IsSubscription = true;
   this.EnumeratedState = (int) SubscriptionStates.Ready;
   this.Owner = c.OwnerFullName;
   this.CurrentUserID = c.MemberUserID;

   Domain domain = Store.GetStore().GetDomain(c.DomainID);
   if(domain != null)
   {
    Simias.Storage.Member member = domain.GetMemberByID(c.OwnerID);
    if(member != null)
     this.Owner = member.FN;
   }

   this.OwnerID = c.OwnerID;

   this.CurrentUserRights = c.UserRights;

   this.State = "WaitSync";
   if( c.encryptionAlgorithm != null)
    this.encryptionAlgorithm = c.encryptionAlgorithm;
   else
    this.encryptionAlgorithm = "";
   this.MigratediFolder = c.MigratediFolder;
  }

 }
}
