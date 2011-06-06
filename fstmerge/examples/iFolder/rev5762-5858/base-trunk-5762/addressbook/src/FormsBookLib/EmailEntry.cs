using System;
using Novell.AddressBook;
namespace Novell.iFolder.FormsBookLib
{
 public class EmailEntry : GenericEntry
 {
  private Email email;
  public EmailEntry()
  {
  }
  public Email EMail
  {
   get
   {
    return email;
   }
   set
   {
    email = value;
   }
  }
 }
}
