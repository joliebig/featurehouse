using System;
using Novell.AddressBook;
namespace Novell.iFolder.FormsBookLib
{
 public class AddressEntry : GenericEntry
 {
  private Address addr;
  public AddressEntry()
  {
  }
  public Address Addr
  {
   get
   {
    return addr;
   }
   set
   {
    addr = value;
   }
  }
 }
}
