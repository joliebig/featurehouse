

using System;
using Simias;
using Simias.Location;
using Simias.Storage;

namespace Novell.iFolder.Web
{



 [Serializable]
 public class DomainWeb
 {
  public string ID;
  public string POBoxID;
  public string Name;
  public string Description;
  public string Host;
  public string UserID;
  public string UserName;
  public bool IsDefault;
  public bool IsSlave;
  public bool IsEnabled;
  public bool IsConnected;




        public DomainWeb()
  {}





  public DomainWeb(string domainID)
  {
   Store store = Store.GetStore();

   this.ID = domainID;
   Domain domain = store.GetDomain(domainID);
   this.Name = domain.Name;
   this.Description = domain.Description;
   this.Host = Locate.Resolve(domainID);
   this.UserID = store.GetUserIDFromDomainID(domainID);
   this.IsDefault = domainID.Equals(store.DefaultDomain);


   Simias.POBox.POBox poBox = Simias.POBox.POBox.FindPOBox(store,
      domainID,
      UserID);

   if(poBox != null)
   {
    this.POBoxID = poBox.ID;
   }

   this.UserName = domain.GetMemberByID(this.UserID).Name;
   this.IsSlave = domain.Role.Equals(Simias.Sync.SyncRoles.Slave);
   this.IsEnabled = new Simias.Domain.DomainAgent().IsDomainActive(domainID);
  }

 }
}
