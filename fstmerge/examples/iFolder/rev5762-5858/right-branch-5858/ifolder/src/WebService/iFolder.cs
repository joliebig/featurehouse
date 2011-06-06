using System;
using Simias.Storage;
using Simias.POBox;
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
  public iFolderWeb()
  {
  }
  public iFolderWeb(Collection collection)
  {
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
   if(collection.Owner != null)
   {
    this.Owner = collection.Owner.Name;
    this.OwnerID = collection.Owner.UserID;
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
   Member tmpMember = collection.GetCurrentMember();
   this.CurrentUserID = tmpMember.UserID;
   this.CurrentUserRights = tmpMember.Rights.ToString();
   Simias.Policy.SyncInterval si = Simias.Policy.SyncInterval.Get(tmpMember, collection);
   this.EffectiveSyncInterval = si.Interval;
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
 }
}
