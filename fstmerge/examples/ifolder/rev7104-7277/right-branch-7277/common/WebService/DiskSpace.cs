

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
   long limit = 0;
                        long UsedSpaceOnServer = 0;
                        Store store = Store.GetStore();
                 Domain domain = store.GetDomainForUser(UserID);
   Simias.Policy.Rule rule = null;
   if(domain == null)
    throw new Exception("Unable to access domain");
   Simias.Storage.Member simMem = domain.GetMemberByID(UserID);
   if(simMem == null)
    throw new Exception("Invalid UserID");
            Property DiskLimitProp = simMem.Properties.GetSingleProperty(Simias.Policy.DiskSpaceQuota.DiskSpaceQuotaPolicyID);
            Property DiskSpaceUsedProp = simMem.Properties.GetSingleProperty(Simias.Policy.DiskSpaceQuota.UsedDiskSpaceOnServer);
            if(DiskLimitProp != null)
            {
  if(DiskLimitProp.Type == Syntax.String)
                 rule = new Simias.Policy.Rule(DiskLimitProp.Value as string);
  else
                 rule = new Simias.Policy.Rule(DiskLimitProp.Value);
                limit = (long)rule.Operand;
            }
            if(DiskSpaceUsedProp != null)
            {
                UsedSpaceOnServer = (long)DiskSpaceUsedProp.Value;
            }
            return new DiskSpace(UsedSpaceOnServer, limit);
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
