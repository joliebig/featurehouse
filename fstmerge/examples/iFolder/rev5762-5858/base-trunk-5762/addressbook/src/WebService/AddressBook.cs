using System;
using Simias.Storage;
using Simias.Sync;
using System.Xml;
using System.Xml.Serialization;
namespace Novell.AddressBook.Web
{
 [Serializable]
 public class AddressBook
 {
  public string Domain;
  public string DomainIdentity;
  public string ID;
  public ulong LocalIncarnation;
  public string ManagedPath;
  public string UnManagedPath;
  public ulong MasterIncarnation;
        public string Name;
  public string Owner;
  public int RefreshInterval;
  public bool Synchronizable;
  public string Type;
  public string Description;
  public AddressBook()
  {
  }
  public AddressBook(Collection collection)
  {
   this.Domain = collection.Domain;
   this.DomainIdentity = collection.Domain;
   this.ID = collection.ID;
   this.LocalIncarnation = collection.LocalIncarnation;
   DirNode dirNode = collection.GetRootDirectory();
   if(dirNode != null)
    this.UnManagedPath = dirNode.GetFullPath(collection);
   else
    this.UnManagedPath = "";
   this.ManagedPath = collection.ManagedPath;
   this.MasterIncarnation = collection.MasterIncarnation;
   this.Name = collection.Name;
   this.Owner = collection.Owner.Name;
   this.RefreshInterval = collection.Interval;
   this.Synchronizable = collection.Synchronizable;
   this.Type = "AB:AddressBook";
   this.Description = "";
  }
 }
}
