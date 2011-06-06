using System;
namespace Novell.iFolder.Web
{
 [Serializable]
 public class Contact
 {
  public string Name;
  public Contact()
  {
  }
  public Contact(Simias.Storage.Node contact)
  {
   this.Name = contact.Name;
  }
 }
}
