using System;
using Novell.AddressBook;
namespace Novell.iFolder.FormsBookLib
{
 public class TelephoneEntry : GenericEntry
 {
  private Telephone phone;
  public TelephoneEntry()
  {
  }
  public Telephone Phone
  {
   get
   {
    return phone;
   }
   set
   {
    phone = value;
   }
  }
 }
}
