

using System;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Web;
using System.Web.Services;
using System.Web.Services.Protocols;
using System.IO;
using Simias;
using Simias.Storage;
using Simias.Sync;
using Simias.POBox;
using System.Xml;
using System.Xml.Serialization;

namespace Novell.AddressBook.Web
{




 [WebService(
 Namespace="http://novell.com/addressbook/web/",
 Name="Novell AddressBook Web Service",
 Description="Web Service providing access to AddressBook")]
 public class AddressBookService : WebService
 {




  public AddressBookService()
  {
  }
  [WebMethod(Description="Returns all AddressBooks")]
  [SoapRpcMethod]
  public AddressBook[] GetAllAddressBooks()
  {
   ArrayList list = new ArrayList();
   Store store = Store.GetStore();
   ICSList iFolderList =
     store.GetCollectionsByType("AB:AddressBook");
   foreach(ShallowNode sn in iFolderList)
   {
    Collection col = store.GetCollectionByID(sn.ID);
    list.Add(new AddressBook(col));
   }
   return (AddressBook[])list.ToArray(typeof(AddressBook));
  }
 }
}
