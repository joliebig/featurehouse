using System;
using Simias;
using Simias.Storage;
namespace Novell.iFolder.Web
{
 [Serializable]
 public class DiskSpace
 {
  public long AvailableSpace;
  public long Limit;
  public long UsedSpace;
  public DiskSpace()
  {
  }
  private DiskSpace(long spaceUsed, long limit)
  {
   this.UsedSpace = spaceUsed;
   this.Limit = limit;
   long spaceAvailable = limit - spaceUsed;
   this.AvailableSpace = ( spaceAvailable < 0 ) ? 0 : spaceAvailable;
  }
  public static DiskSpace GetMemberDiskSpace( string UserID )
  {
   long limit;
   Simias.DomainServices.DomainAgent da = new Simias.DomainServices.DomainAgent();
   long spaceUsed = da.GetDomainDiskSpaceForMember( UserID, out limit );
   return new DiskSpace( spaceUsed, limit );
  }
  public static DiskSpace GetiFolderDiskSpace( string iFolderID )
  {
   long limit;
   Simias.DomainServices.DomainAgent da = new Simias.DomainServices.DomainAgent();
   long spaceUsed = da.GetDomainDiskSpaceForCollection( iFolderID, out limit );
   return new DiskSpace( spaceUsed, limit );
  }
  public static void SetUserDiskSpaceLimit( string UserID, long limit )
  {
   Store store = Store.GetStore();
   Domain domain = store.GetDomainForUser(UserID);
   if(domain == null)
    throw new Exception("Unable to access domain");
   Simias.Storage.Member simMem = domain.GetMemberByID(UserID);
   if(simMem == null)
    throw new Exception("Invalid UserID");
   Simias.Policy.DiskSpaceQuota.Set(simMem, limit);
  }
  public static void SetiFolderDiskSpaceLimit( string iFolderID,
             long limit )
  {
   Store store = Store.GetStore();
   Collection col = store.GetCollectionByID(iFolderID);
   if(col == null)
    throw new Exception("Invalid iFolderID");
   Simias.Policy.DiskSpaceQuota.Set(col, limit);
  }
 }
}
