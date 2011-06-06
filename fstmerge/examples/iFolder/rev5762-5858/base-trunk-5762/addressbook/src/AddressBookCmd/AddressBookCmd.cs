using System;
using System.Collections;
using System.Text.RegularExpressions;
using System.IO;
using Simias;
using Simias.Storage;
using Novell.AddressBook;
namespace AddressBookCmd
{
 public enum Commands : uint
 {
  listbooks = 1,
  createbook,
  deletebook,
  importvcard,
  exportvcard,
  listcontacts,
  createcontact,
  deletecontact,
  listproperties,
  addproperty,
  addnameproperty,
  addaddressproperty,
  deleteproperty
 }
 public class Abc
 {
  private AddressBook cAddressBook = null;
  private Manager abManager = null;
  public Commands cmd;
  public bool timeIt = false;
  public bool displayCommandHelp;
  public bool verbose = false;
  public string addressBookName = null;
  private string errorMsg = null;
  private ArrayList bookList = null;
  private ArrayList contactList = null;
  private ArrayList propertyList = null;
  private ArrayList addAddressList = null;
  private ArrayList addNameList = null;
  private ArrayList fileList = null;
  [STAThread]
  static int Main(string[] args)
  {
   Abc cAbc = new Abc();
   if (cAbc.ParseArguments(args))
   {
    if(cAbc.ProcessCommands() == false)
    {
     if (cAbc.errorMsg != null)
     {
      Console.WriteLine(cAbc.errorMsg);
     }
     return(-1);
    }
   }
   else
   {
    if (cAbc.errorMsg != null)
    {
     Console.WriteLine(cAbc.errorMsg);
    }
    return(-1);
   }
   return(0);
  }
  static void DisplayUsage()
  {
   Console.WriteLine("AddressBookCmd [command] <parameters> -t -v");
   Console.WriteLine("    listbooks - list address books");
   Console.WriteLine("    createbook -b <address book name>");
   Console.WriteLine("    deletebook -b <address book name>");
   Console.WriteLine("    importvcard -f <vcard file> -b <address book name>");
   Console.WriteLine("    listcontacts -b <address book name>");
   Console.WriteLine("    createcontact -c <contact name> -b <address book name>");
   Console.WriteLine("    deletecontact -c <contact name> -b <address book name>");
   Console.WriteLine("    listproperties -c <contact name> -b <address book name>");
   Console.WriteLine("    addproperty -c <contact name> -p <property=value>");
   Console.WriteLine("    addnameproperty -c <contact name> -p <nameproperty>");
   Console.WriteLine("    addaddressproperty -c <contact name> -p <addressproperty>");
   Console.WriteLine("    deleteproperty -c <contact name> -p <property>");
   Console.WriteLine("    exportvcard -f <path> -c <contact name> - b <address book name>");
   Console.WriteLine("   -t = time the command");
   Console.WriteLine("   -v = verbose");
   return;
  }
  private void DisplayAddPropertyUsage()
  {
   Console.WriteLine("AddressBookCmd addproperty -b <address book> -c <contact name> -p <propname=propvalue>");
   Console.WriteLine("\n   PROPERTIES:");
   Console.WriteLine("   FN=<full name> - Adds a full name property to the contact");
   Console.WriteLine("   USERNAME=<username/userid>");
   Console.WriteLine("   NICKNAME=<nick name>");
   Console.WriteLine("   TITLE=<title>");
   Console.WriteLine("   ROLE=<role>");
   Console.WriteLine("   ORG=<organization>");
   Console.WriteLine("   BDAY=<birthday> format: MM/DD/YYYY");
   Console.WriteLine("   URL=<url to contact's web page>");
   Console.WriteLine("   BLOG=<url to contact's blog>");
   Console.WriteLine("   WORKFORCEID=<contact's workforce id>");
   Console.WriteLine("\n   EMAIL=<email> format: yourname@somedomain.com:attr1:attr2");
   Console.WriteLine("   email attributes: pref, work, personal, x.400, internet\n");
   Console.WriteLine("   TEL=<telephone> format: telephonenumber:attr1:attr2");
   Console.WriteLine("   telephone attributes: pref, work, home, other, voice, cell, msg, voip");
   Console.WriteLine("   video, pcs, fax, modem, isdn, car");
   Console.WriteLine("\nIf no address book argument is given the default address book is assumed");
  }
  private void DisplayAddAddressPropertyUsage()
  {
   Console.WriteLine("AddressBookCmd addaddressproperty -b <address book> -c <contact name> -p <propname=propvalue>");
   Console.WriteLine("\n   PROPERTIES:");
   Console.WriteLine("   STREET=<street address>");
   Console.WriteLine("   LOCALITY=<city/locality>");
   Console.WriteLine("   REGION=<state/region>");
   Console.WriteLine("   PCODE=<postal code>");
   Console.WriteLine("   PBOX=<postal box>");
   Console.WriteLine("   COUNTRY=<country>");
   Console.WriteLine("   PREF");
   Console.WriteLine("   WORK");
   Console.WriteLine("   HOME");
   Console.WriteLine("   OTHER");
   Console.WriteLine("   POSTAL");
   Console.WriteLine("   PARCEL");
   Console.WriteLine("   DOM - Domestic address");
   Console.WriteLine("   INTL - International address");
   Console.WriteLine("\nIf no address book argument is given the default address book is assumed");
  }
  private void DisplayAddNamePropertyUsage()
  {
   Console.WriteLine("AddressBookCmd addnameproperty -b <address book> -c <contact name> -p <propname=propvalue>");
   Console.WriteLine("\n   PROPERTIES:");
   Console.WriteLine("   PREFIX=<name prefix> - Ex. Mr.  Dr.");
   Console.WriteLine("   GIVEN=<given/first name>");
   Console.WriteLine("   FAMILY=<family/other name>");
   Console.WriteLine("   OTHER=<other/middle name>");
   Console.WriteLine("   SUFFIX=<honorific suffix> - Ex. III");
   Console.WriteLine("\nIf no address book argument is given the default address book is assumed");
  }
  private bool ParseArguments(string [] args)
  {
   if (args.Length == 0)
   {
    DisplayUsage();
    return(false);
   }
   args[0].ToLower();
   if (args[0] == "--help")
   {
    DisplayUsage();
    return(false);
   }
   switch(args[0])
   {
    case "createbook":
     this.cmd = Commands.createbook;
     break;
    case "deletebook":
     this.cmd = Commands.deletebook;
     break;
    case "importvcard":
     this.cmd = Commands.importvcard;
     break;
    case "listbooks":
     this.cmd = Commands.listbooks;
     break;
    case "exportvcard":
     this.cmd = Commands.exportvcard;
     break;
    case "listcontacts":
     this.cmd = Commands.listcontacts;
     break;
    case "createcontact":
     this.cmd = Commands.createcontact;
     break;
    case "deletecontact":
     this.cmd = Commands.deletecontact;
     break;
    case "listproperties":
     this.cmd = Commands.listproperties;
     break;
    case "addproperty":
     this.cmd = Commands.addproperty;
     break;
    case "addnameproperty":
     this.cmd = Commands.addnameproperty;
     break;
    case "addaddressproperty":
     this.cmd = Commands.addaddressproperty;
     break;
    case "deleteproperty":
     this.cmd = Commands.deleteproperty;
     break;
    default:
     this.errorMsg = "invalid command";
     return(false);
   }
   for(int i = 1; i < args.Length; i++)
   {
    args[i].ToLower();
    if (args[i] == "--help")
    {
     this.displayCommandHelp = true;
    }
    else
    if (args[i].Length == 2)
    {
     if (args[i] == "-f")
     {
      if (i + 1 <= args.Length)
      {
       if (args[++i] != null)
       {
        this.AddFile(args[i]);
       }
      }
     }
     else
     if (args[i] == "-b")
     {
      if (i + 1 <= args.Length)
      {
       if (args[++i] != null)
       {
        this.AddBook(args[i]);
       }
      }
     }
     else
     if (args[i] == "-c")
     {
      if (i + 1 <= args.Length)
      {
       if (args[++i] != null)
       {
        this.AddContact(args[i]);
       }
      }
     }
     else
     if (args[i] == "-p")
     {
      if (i + 1 <= args.Length)
      {
       if (args[++i] != null)
       {
        this.AddProperty(args[i]);
       }
      }
     }
     else
     if (args[i] == "-t")
     {
      this.timeIt = true;
     }
     else
     if (args[i] == "-v")
     {
      this.verbose = true;
     }
    }
   }
   return(true);
  }
  private bool OpenAddressBook()
  {
   this.cAddressBook = null;
   if (this.bookList != null && this.bookList.Count != 0)
   {
    string bookName = null;
    try
    {
     IEnumerator cEnum = bookList.GetEnumerator();
     if (cEnum.MoveNext())
     {
      bookName = (string) cEnum.Current;
      if (verbose == true)
      {
       Console.WriteLine("Opening address book: " + bookName);
      }
      this.cAddressBook = abManager.GetAddressBookByName(bookName);
     }
    }
    catch
    {
     if (verbose == true)
     {
      Console.WriteLine("OpenAddressBook failed");
      if (bookName != null)
      {
       Console.WriteLine("Address Book: {0} does not exist.", bookName);
      }
     }
    }
   }
   else
   {
    DisplayUsage();
    this.cAddressBook = null;
   }
   if (this.cAddressBook == null)
   {
    return(false);
   }
   return(true);
  }
  private void AddAddressProperty(Contact cContact)
  {
   if (this.propertyList != null)
   {
    AddressTypes addrTypes = 0;
    Address cAddress = new Address();
    foreach(string propertyString in this.propertyList)
    {
     Regex p = new Regex(@";");
     IEnumerator propertyTokens = p.Split(propertyString).GetEnumerator();
     while(propertyTokens.MoveNext())
     {
      Regex o = new Regex(@"=");
      IEnumerator propValueTokens = o.Split((string) propertyTokens.Current).GetEnumerator();
      while(propValueTokens.MoveNext())
      {
       string token = (string) propValueTokens.Current;
       token.ToLower();
       if (token.StartsWith("pref"))
       {
        cAddress.Preferred = true;
       }
       else
       if (token == "street")
       {
        if (propValueTokens.MoveNext())
        {
         cAddress.Street = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "locality")
       {
        if (propValueTokens.MoveNext())
        {
         if (verbose == true)
         {
          Console.WriteLine("Locality: " + (string) propValueTokens.Current);
         }
         cAddress.Locality = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "region")
       {
        if (propValueTokens.MoveNext())
        {
         if (verbose == true)
         {
          Console.WriteLine("Region: " + (string) propValueTokens.Current);
         }
         cAddress.Region = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "pcode")
       {
        if (propValueTokens.MoveNext())
        {
         cAddress.PostalCode = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "pbox")
       {
        if (propValueTokens.MoveNext())
        {
         cAddress.PostalBox = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "country")
       {
        if (propValueTokens.MoveNext())
        {
         cAddress.Country = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "work")
       {
        addrTypes |= AddressTypes.work;
       }
       else
       if (token == "home")
       {
        addrTypes |= AddressTypes.home;
       }
       else
       if (token == "other")
       {
        addrTypes |= AddressTypes.other;
       }
       else
       if (token == "dom")
       {
        addrTypes |= AddressTypes.dom;
       }
       else
       if (token == "intl")
       {
        addrTypes |= AddressTypes.intl;
       }
       else
       if (token == "parcel")
       {
        addrTypes |= AddressTypes.parcel;
       }
       else
       if (token == "postal")
       {
        addrTypes |= AddressTypes.postal;
       }
      }
     }
    }
    if (cAddress.PostalCode != "")
    {
     if (verbose == true)
     {
      Console.WriteLine("Adding address object to contact");
     }
     if (addrTypes != 0)
     {
      cAddress.Types = addrTypes;
     }
     cContact.AddAddress(cAddress);
     cContact.Commit();
    }
    else
    {
     Console.WriteLine("invalid parameters");
    }
   }
  }
  private void AddNameProperty(Contact cContact)
  {
   if (this.propertyList != null)
   {
    Name tmpName = new Name();
    foreach(string propertyString in this.propertyList)
    {
     Regex p = new Regex(@";");
     IEnumerator propertyTokens = p.Split(propertyString).GetEnumerator();
     while(propertyTokens.MoveNext())
     {
      Regex o = new Regex(@"=");
      IEnumerator propValueTokens = o.Split((string) propertyTokens.Current).GetEnumerator();
      while(propValueTokens.MoveNext())
      {
       string token = (string) propValueTokens.Current;
       token.ToLower();
       if (token.StartsWith("pref"))
       {
        tmpName.Preferred = true;
       }
       else
       if (token == "prefix")
       {
        if (propValueTokens.MoveNext())
        {
         tmpName.Prefix = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "given")
       {
        if (propValueTokens.MoveNext())
        {
         if (verbose == true)
         {
          Console.WriteLine("Given Name: " + (string) propValueTokens.Current);
         }
         tmpName.Given = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "family")
       {
        if (propValueTokens.MoveNext())
        {
         if (verbose == true)
         {
          Console.WriteLine("Family Name: " + (string) propValueTokens.Current);
         }
         tmpName.Family = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "other")
       {
        if (propValueTokens.MoveNext())
        {
         tmpName.Other = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "suffix")
       {
        if (propValueTokens.MoveNext())
        {
         tmpName.Suffix = (string) propValueTokens.Current;
        }
       }
      }
     }
    }
    if (tmpName.Given != "" && tmpName.Family != "")
    {
     if (verbose == true)
     {
      Console.WriteLine("Adding name object to contact");
     }
     cContact.AddName(tmpName);
    }
    cContact.Commit();
   }
  }
  private void AddProperties(Contact cContact)
  {
   if (this.propertyList != null)
   {
    foreach(string propertyString in this.propertyList)
    {
     Regex p = new Regex(@";");
     IEnumerator propertyTokens = p.Split(propertyString).GetEnumerator();
     while(propertyTokens.MoveNext())
     {
      Regex o = new Regex(@"=");
      IEnumerator propValueTokens = o.Split((string) propertyTokens.Current).GetEnumerator();
      while(propValueTokens.MoveNext())
      {
       string token = (string) propValueTokens.Current;
       token.ToLower();
       if (token.StartsWith("org"))
       {
        if (propValueTokens.MoveNext())
        {
         cContact.Organization = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "blog")
       {
        if (propValueTokens.MoveNext())
        {
         if (verbose == true)
         {
          Console.WriteLine("Updating the blog attribute to: " + (string) propValueTokens.Current);
         }
         cContact.Blog = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "url")
       {
        if (propValueTokens.MoveNext())
        {
         cContact.Url = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "title")
       {
        if (propValueTokens.MoveNext())
        {
         cContact.Title = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "role")
       {
        if (propValueTokens.MoveNext())
        {
         cContact.Role = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "workforceid")
       {
        if (propValueTokens.MoveNext())
        {
         cContact.WorkForceID = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "birthday" || token == "bday")
       {
        if (propValueTokens.MoveNext())
        {
         cContact.Birthday = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "email")
       {
        if (propValueTokens.MoveNext())
        {
         Regex x = new Regex(@":");
         IEnumerator enumTokens1 =
          x.Split((string) propValueTokens.Current).GetEnumerator();
         if (enumTokens1.MoveNext())
         {
          EmailTypes eTypes = 0;
          Email cMail = new Email();
          cMail.Address = (string) enumTokens1.Current;
          if (verbose == true)
          {
           Console.WriteLine("Adding email address: " + cMail.Address);
          }
          while(enumTokens1.MoveNext())
          {
           string typeToken = (string) enumTokens1.Current;
           typeToken.ToLower();
           if (typeToken == "preferred")
           {
            if (verbose == true)
            {
             Console.WriteLine("   Preferred");
            }
            cMail.Preferred = true;
           }
           else
           if (typeToken == "work")
           {
            if (verbose == true)
            {
             Console.WriteLine("   Work");
            }
            eTypes |= EmailTypes.work;
           }
           else
           if (typeToken == "personal")
           {
            if (verbose == true)
            {
             Console.WriteLine("   Personal");
            }
            eTypes |= EmailTypes.personal;
           }
           else
           if (typeToken == "other")
           {
            eTypes |= EmailTypes.other;
           }
          }
          if (eTypes != 0)
          {
           cMail.Types = eTypes;
          }
          cContact.AddEmailAddress(cMail);
         }
        }
       }
       else
       if (token == "tel")
       {
        if (propValueTokens.MoveNext())
        {
         Regex x = new Regex(@":");
         IEnumerator enumTokens1 =
          x.Split((string) propValueTokens.Current).GetEnumerator();
         if (enumTokens1.MoveNext())
         {
          PhoneTypes pTypes = 0;
          Telephone cPhone = new Telephone();
          cPhone.Number = (string) enumTokens1.Current;
          if (verbose == true)
          {
           Console.WriteLine("Adding telephone number: " + cPhone.Number);
          }
          while(enumTokens1.MoveNext())
          {
           string typeToken = (string) enumTokens1.Current;
           typeToken.ToLower();
           if (typeToken.StartsWith("pref"))
           {
            if (verbose == true)
            {
             Console.WriteLine("Preferred");
            }
            cPhone.Preferred = true;
           }
           else
           if (typeToken == "work")
           {
            if (verbose == true)
            {
             Console.WriteLine("Work");
            }
            pTypes |= PhoneTypes.work;
           }
           else
           if (typeToken == "home")
           {
            if (verbose == true)
            {
             Console.WriteLine("Home");
            }
            pTypes |= PhoneTypes.home;
           }
           else
           if (typeToken == "other")
           {
            pTypes |= PhoneTypes.other;
           }
           else
           if (typeToken == "msg")
           {
            pTypes |= PhoneTypes.msg;
           }
           else
           if (typeToken == "bbs")
           {
            pTypes |= PhoneTypes.bbs;
           }
           else
           if (typeToken == "car")
           {
            pTypes |= PhoneTypes.car;
           }
           else
           if (typeToken == "cell")
           {
            pTypes |= PhoneTypes.cell;
           }
           else
           if (typeToken == "fax")
           {
            pTypes |= PhoneTypes.fax;
           }
           else
           if (typeToken == "isdn")
           {
            pTypes |= PhoneTypes.isdn;
           }
           else
           if (typeToken == "modem")
           {
            pTypes |= PhoneTypes.modem;
           }
           else
           if (typeToken == "pager")
           {
            pTypes |= PhoneTypes.pager;
           }
           else
           if (typeToken == "pcs")
           {
            pTypes |= PhoneTypes.pcs;
           }
           else
           if (typeToken == "video")
           {
            pTypes |= PhoneTypes.video;
           }
           else
           if (typeToken == "voip")
           {
            pTypes |= PhoneTypes.voip;
           }
          }
          if (pTypes != 0)
          {
           cPhone.Types = pTypes;
          }
          cContact.AddTelephoneNumber(cPhone);
         }
        }
       }
       else
       if (token == "managerid")
       {
        if (propValueTokens.MoveNext())
        {
         cContact.ManagerID = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "note")
       {
        if (propValueTokens.MoveNext())
        {
         cContact.Note = (string) propValueTokens.Current;
        }
       }
       else
       if (token == "nickname")
       {
        if (propValueTokens.MoveNext())
        {
         cContact.Nickname = (string) propValueTokens.Current;
        }
       }
      }
     }
    }
    cContact.Commit();
   }
  }
  private void ListProperties(Contact cContact)
  {
   Console.WriteLine("ID=" + cContact.ID);
   Console.WriteLine("USERNAME=" + cContact.UserName);
   if (cContact.FN != "")
   {
    Console.WriteLine("FN=" + cContact.FN);
   }
   IABList nList = cContact.GetNames();
   foreach(Name cName in nList)
   {
    Console.Write("N=(");
    Console.Write("ID=" + cName.ID);
    if (cName.Prefix != "")
    {
     Console.Write(";Prefix=");
     Console.Write(cName.Prefix);
    }
    if (cName.Given != "")
    {
     Console.Write(";Given=");
     Console.Write(cName.Given);
    }
    if (cName.Other != "")
    {
     Console.Write(";Other=");
     Console.Write(cName.Other);
    }
    if (cName.Family != "")
    {
     Console.Write(";Family=");
     Console.Write(cName.Family);
    }
    if (cName.Suffix != "")
    {
     Console.Write(";Suffix=");
     Console.Write(cName.Suffix);
    }
    if (cName.Preferred == true)
    {
     Console.Write(";PREF");
    }
    Console.WriteLine(")");
   }
   if (cContact.Nickname != "")
   {
    Console.WriteLine("NICKNAME=" + cContact.Nickname);
   }
   if (cContact.Title != "")
   {
    Console.WriteLine("TITLE=" + cContact.Title);
   }
   if (cContact.Role != "")
   {
    Console.WriteLine("ROLE=" + cContact.Role);
   }
   IABList mList = cContact.GetEmailAddresses();
   foreach(Email tMail in mList)
   {
    Console.Write("EMAIL=" + tMail.Address);
    if (tMail.Preferred == true)
    {
     Console.Write(";PREF");
    }
    if ((tMail.Types & EmailTypes.work) == EmailTypes.work)
    {
     Console.Write(";WORK");
    }
    if ((tMail.Types & EmailTypes.personal) == EmailTypes.personal)
    {
     Console.Write(";PERSONAL");
    }
    if ((tMail.Types & EmailTypes.internet) == EmailTypes.internet)
    {
     Console.Write(";INTERNET");
    }
    if ((tMail.Types & EmailTypes.x400) == EmailTypes.x400)
    {
     Console.Write(";X.400");
    }
    Console.WriteLine("");
   }
   IABList tList = cContact.GetTelephoneNumbers();
   foreach(Telephone tPhone in tList)
   {
    Console.Write("TEL=" + tPhone.Number);
    if (tPhone.Preferred == true)
    {
     Console.Write(";PREF");
    }
    if ((tPhone.Types & PhoneTypes.home) == PhoneTypes.home)
    {
     Console.Write(";HOME");
    }
    if ((tPhone.Types & PhoneTypes.work) == PhoneTypes.work)
    {
     Console.Write(";WORK");
    }
    if ((tPhone.Types & PhoneTypes.other) == PhoneTypes.other)
    {
     Console.Write(";OTHER");
    }
    if ((tPhone.Types & PhoneTypes.cell) == PhoneTypes.cell)
    {
     Console.Write(";CELL");
    }
    if ((tPhone.Types & PhoneTypes.voice) == PhoneTypes.voice)
    {
     Console.Write(";VOICE");
    }
    if ((tPhone.Types & PhoneTypes.fax) == PhoneTypes.fax)
    {
     Console.Write(";FAX");
    }
    if ((tPhone.Types & PhoneTypes.msg) == PhoneTypes.msg)
    {
     Console.Write(";MSG");
    }
    if ((tPhone.Types & PhoneTypes.bbs) == PhoneTypes.bbs)
    {
     Console.Write(";BBS");
    }
    if ((tPhone.Types & PhoneTypes.car) == PhoneTypes.car)
    {
     Console.Write(";CAR");
    }
    if ((tPhone.Types & PhoneTypes.isdn) == PhoneTypes.isdn)
    {
     Console.Write(";ISDN");
    }
    if ((tPhone.Types & PhoneTypes.modem) == PhoneTypes.modem)
    {
     Console.Write(";MODEM");
    }
    if ((tPhone.Types & PhoneTypes.pager) == PhoneTypes.pager)
    {
     Console.Write(";PAGER");
    }
    if ((tPhone.Types & PhoneTypes.pcs) == PhoneTypes.pcs)
    {
     Console.Write(";PCS");
    }
    if ((tPhone.Types & PhoneTypes.video) == PhoneTypes.video)
    {
     Console.Write(";VIDEO");
    }
    if ((tPhone.Types & PhoneTypes.voip) == PhoneTypes.voip)
    {
     Console.Write(";VOIP");
    }
    Console.WriteLine("");
   }
   if (cContact.Organization != "")
   {
    Console.WriteLine("ORG=" + cContact.Organization);
   }
   if (cContact.WorkForceID != "")
   {
    Console.WriteLine("WORKFORCEID=" + cContact.WorkForceID);
   }
   if (cContact.ManagerID != "")
   {
    Console.WriteLine("MANAGERID=" + cContact.ManagerID);
   }
   if (cContact.Birthday != "")
   {
    Console.WriteLine("BDAY=" + cContact.Birthday);
   }
   if (cContact.Blog != "")
   {
    Console.WriteLine("BLOG=" + cContact.Blog);
   }
   if (cContact.Url != "")
   {
    Console.WriteLine("URL=" + cContact.Url);
   }
   IABList aList = cContact.GetAddresses();
   foreach(Address cAddress in aList)
   {
    Console.Write("ADR=(");
    if (cAddress.ID != "")
    {
     Console.Write("ID=" + cAddress.ID);
    }
    if (cAddress.Street != "")
    {
     Console.Write(";STREET=" + cAddress.Street);
    }
    if (cAddress.Locality != "")
    {
     Console.Write(";LOCALITY=" + cAddress.Locality);
    }
    if (cAddress.Region != "")
    {
     Console.Write(";REGION=" + cAddress.Region);
    }
    if (cAddress.PostalCode != "")
    {
     Console.Write(";PCODE=" + cAddress.PostalCode);
    }
    if (cAddress.PostalBox != "")
    {
     Console.Write(";PBOX=" + cAddress.PostalBox);
    }
    if (cAddress.Country != "")
    {
     Console.Write(";COUNTRY=" + cAddress.Country);
    }
    if (cAddress.Types != 0)
    {
     if ((cAddress.Types & AddressTypes.preferred) == AddressTypes.preferred)
     {
      Console.Write(";PREF");
     }
     if ((cAddress.Types & AddressTypes.home) == AddressTypes.home)
     {
      Console.Write(";HOME");
     }
     if ((cAddress.Types & AddressTypes.work) == AddressTypes.work)
     {
      Console.Write(";WORK");
     }
     if ((cAddress.Types & AddressTypes.other) == AddressTypes.other)
     {
      Console.Write(";OTHER");
     }
     if ((cAddress.Types & AddressTypes.postal) == AddressTypes.postal)
     {
      Console.Write(";POSTAL");
     }
     if ((cAddress.Types & AddressTypes.parcel) == AddressTypes.parcel)
     {
      Console.Write(";PARCEL");
     }
     if ((cAddress.Types & AddressTypes.dom) == AddressTypes.dom)
     {
      Console.Write(";DOM");
     }
     if ((cAddress.Types & AddressTypes.intl) == AddressTypes.intl)
     {
      Console.Write(";INTL");
     }
    }
    Console.WriteLine(")");
   }
   if (cContact.Note != "")
   {
    Console.WriteLine("NOTE=" + cContact.Note);
   }
  }
  public void AddBook(string bookName)
  {
   if (this.bookList == null)
   {
    this.bookList = new ArrayList();
   }
   this.bookList.Add(bookName);
  }
  public void AddContact(string name)
  {
   if (this.contactList == null)
   {
    this.contactList = new ArrayList();
   }
   this.contactList.Add(name);
  }
  public void AddProperty(string propertyValue)
  {
   if (this.propertyList == null)
   {
    this.propertyList = new ArrayList();
   }
   this.propertyList.Add(propertyValue);
  }
  public void AddAddress(string addressValue)
  {
   if (this.addAddressList == null)
   {
    this.addAddressList = new ArrayList();
   }
   this.addAddressList.Add(addressValue);
  }
  public void AddName(string nameValue)
  {
   if (this.addNameList == null)
   {
    this.addNameList = new ArrayList();
   }
   this.addNameList.Add(nameValue);
  }
  public void AddFile(string fileName)
  {
   if (this.fileList == null)
   {
    this.fileList = new ArrayList();
   }
   this.fileList.Add(fileName);
  }
  public bool ProcessCommands()
  {
   Contact cContact = null;
   DateTime start = DateTime.Now;
   this.abManager = Manager.Connect( );
   if (this.cmd == Commands.listbooks)
   {
    if (verbose == true)
    {
     Console.WriteLine("Command::listbooks");
    }
    foreach(AddressBook tmpBook in abManager)
    {
     Console.WriteLine(tmpBook.Name);
     Console.WriteLine("");
    }
   }
   else
   if (this.cmd == Commands.createbook)
   {
    if (verbose == true)
    {
     Console.WriteLine("Command::createbook");
    }
    bool atLeastOne = false;
    foreach(string bookName in this.bookList)
    {
     if (verbose == true)
     {
      Console.WriteLine("Creating Address Book: " + bookName);
     }
     AddressBook cBook = abManager.CreateAddressBook(bookName);
     atLeastOne = true;
    }
    if (atLeastOne == false)
    {
     Console.WriteLine("invalid parameters");
    }
   }
   else
   if (this.cmd == Commands.deletebook)
   {
    if (displayCommandHelp == true)
    {
    }
    else
    {
     bool atLeastOne = false;
     foreach(string bookName in this.bookList)
     {
      if (verbose == true)
      {
       Console.WriteLine("Deleting Address Book: " + bookName);
      }
      try
      {
       AddressBook cBook = abManager.GetAddressBookByName(bookName);
       cBook.Delete();
       cBook.Commit();
       atLeastOne = true;
      }
      catch{}
     }
     if (atLeastOne == false)
     {
      Console.WriteLine("invalid parameters");
     }
    }
   }
   else
   if (this.cmd == Commands.importvcard)
   {
    if (OpenAddressBook())
    {
     if (this.fileList != null && this.fileList.Count > 0)
     {
      foreach(String filename in this.fileList)
      {
       if (verbose == true)
       {
        Console.WriteLine("Importing vCards(s) from file: " + filename);
       }
       cAddressBook.ImportVCard(filename);
      }
     }
     else
     {
      Console.WriteLine("invalid parameters");
     }
    }
   }
   else
   if (this.cmd == Commands.listcontacts)
   {
    if (verbose == true)
    {
     Console.WriteLine("Command::listcontacts");
    }
    if (OpenAddressBook())
    {
     foreach(Contact lContact in cAddressBook)
     {
      Console.WriteLine(lContact.UserName);
     }
    }
   }
   else
   if (this.cmd == Commands.createcontact)
   {
    if (OpenAddressBook())
    {
     foreach(string contactName in this.contactList)
     {
      if (verbose == true)
      {
       Console.WriteLine("Adding contact: " + contactName);
      }
      Contact tmpContact = new Contact();
      tmpContact.UserName = contactName;
      cAddressBook.AddContact(tmpContact);
      tmpContact.Commit();
     }
    }
   }
   else
   if (this.cmd == Commands.deletecontact)
   {
    if (OpenAddressBook())
    {
     foreach(string contactName in this.contactList)
     {
      if (verbose == true)
      {
       Console.WriteLine("Deleting contact: " + contactName);
      }
      IABList cList = cAddressBook.SearchUsername(contactName, Simias.Storage.SearchOp.Equal);
      foreach(Contact tmpContact in cList)
      {
       tmpContact.Delete();
      }
     }
    }
   }
   else
   if (this.cmd == Commands.listproperties)
   {
    if (OpenAddressBook())
    {
     if (this.contactList != null)
     {
      IABList cList =
       cAddressBook.SearchUsername(
        (string) this.contactList[0],
        Simias.Storage.SearchOp.Equal);
      foreach(Contact tmpContact in cList)
      {
       this.ListProperties(tmpContact);
       break;
      }
     }
    }
   }
   else
   if (this.cmd == Commands.deleteproperty)
   {
    if (verbose == true)
    {
     Console.WriteLine("Command::deleteproperty");
    }
    if (OpenAddressBook())
    {
     if (this.contactList != null)
     {
      IABList cList =
       cAddressBook.SearchUsername(
        (string) this.contactList[0],
        Simias.Storage.SearchOp.Equal);
      foreach(Contact tmpContact in cList)
      {
       if (verbose == true)
       {
        Console.WriteLine("Contact: " + tmpContact.UserName);
       }
       foreach(string propertyString in this.propertyList)
       {
        Regex o = new Regex(@"=");
        IEnumerator enumTokens = o.Split(propertyString).GetEnumerator();
        while(enumTokens.MoveNext())
        {
         string token = (string) enumTokens.Current;
         token.ToLower();
         if (verbose == true)
         {
          Console.WriteLine("Deleting property: " + token);
         }
         if (token == "organization")
         {
          tmpContact.Organization = null;
         }
         else
         if (token == "blog")
         {
          tmpContact.Blog = null;
         }
         else
         if (token == "url")
         {
          tmpContact.Url = null;
         }
         else
         if (token == "title")
         {
          tmpContact.Title = null;
         }
         else
         if (token == "role")
         {
          tmpContact.Role = null;
         }
         else
         if (token == "workforceid")
         {
          tmpContact.WorkForceID = null;
         }
         else
         if (token == "birthday")
         {
          tmpContact.Birthday = null;
         }
         else
         if (token == "email")
         {
          if (enumTokens.MoveNext())
          {
           IABList eAddresses = cContact.GetEmailAddresses();
           foreach (Email tmpMail in eAddresses)
           {
            if (tmpMail.Address == (string) enumTokens.Current)
            {
             if (verbose == true)
             {
              Console.WriteLine("   Deleting e-mail address: " + tmpMail.Address);
             }
             tmpMail.Delete();
             break;
            }
           }
          }
         }
         else
         if (token == "managerid")
         {
          tmpContact.ManagerID = null;
         }
         else
         if (token == "note")
         {
          tmpContact.Note = null;
         }
         else
         if (token == "nickname")
         {
          tmpContact.Nickname = null;
         }
        }
       }
       tmpContact.Commit();
       break;
      }
     }
    }
   }
   else
   if (this.cmd == Commands.addproperty)
   {
    if (verbose == true)
    {
     Console.WriteLine("Command::addproperty");
    }
    if (this.displayCommandHelp == true)
    {
     this.DisplayAddPropertyUsage();
    }
    else
    if (OpenAddressBook())
    {
     if (this.contactList != null)
     {
      IABList cList =
       cAddressBook.SearchUsername(
        (string) this.contactList[0],
        Simias.Storage.SearchOp.Equal);
      foreach(Contact tmpContact in cList)
      {
       if (verbose == true)
       {
        Console.WriteLine("Contact: " + tmpContact.UserName);
       }
       this.AddProperties(tmpContact);
       break;
      }
     }
    }
   }
   else
   if (this.cmd == Commands.addnameproperty)
   {
    if (verbose == true)
    {
     Console.WriteLine("Command::addnameproperty");
    }
    if (this.displayCommandHelp == true)
    {
     this.DisplayAddNamePropertyUsage();
    }
    else
    if (OpenAddressBook())
    {
     if (this.contactList != null)
     {
      IABList cList =
       cAddressBook.SearchUsername(
        (string) this.contactList[0],
        Simias.Storage.SearchOp.Equal);
      foreach(Contact tmpContact in cList)
      {
       if (verbose == true)
       {
        Console.WriteLine("Contact: " + tmpContact.UserName);
       }
       this.AddNameProperty(tmpContact);
       break;
      }
     }
    }
   }
   else
   if (this.cmd == Commands.addaddressproperty)
   {
    if (verbose == true)
    {
     Console.WriteLine("Command::addaddressproperty");
    }
    if (this.displayCommandHelp == true)
    {
     this.DisplayAddAddressPropertyUsage();
    }
    else
    if (OpenAddressBook())
    {
     if (this.contactList != null)
     {
      IABList cList =
       cAddressBook.SearchUsername(
        (string) this.contactList[0],
        Simias.Storage.SearchOp.Equal);
      foreach(Contact tmpContact in cList)
      {
       if (verbose == true)
       {
        Console.WriteLine("Contact: " + tmpContact.UserName);
       }
       this.AddAddressProperty(tmpContact);
       break;
      }
     }
    }
   }
   else
   if (this.cmd == Commands.exportvcard)
   {
    if (verbose == true)
    {
     Console.WriteLine("Command::exportvcard");
    }
    if (OpenAddressBook())
    {
     if (this.contactList != null)
     {
      string path = "";
      if (this.fileList != null)
      {
       path = (string) this.fileList[0];
       if (path[path.Length - 1] != Path.DirectorySeparatorChar)
       {
        path += Path.DirectorySeparatorChar;
       }
       if (verbose == true)
       {
        Console.WriteLine("Creating vCards at: " + path);
       }
      }
      foreach(string contactName in this.contactList)
      {
       IABList cList =
        cAddressBook.SearchUsername(
         contactName,
         Simias.Storage.SearchOp.Equal);
       foreach(Contact tmpContact in cList)
       {
        string vCardFileName = path;
        if (tmpContact.FN != "")
        {
         vCardFileName += tmpContact.FN;
         vCardFileName += ".vcf";
        }
        else
        {
         vCardFileName = tmpContact.ID + ".vcf";
        }
        if (verbose == true)
        {
         Console.WriteLine("Exporting vCard: " + vCardFileName);
        }
        tmpContact.ExportVCard(vCardFileName);
        break;
       }
      }
     }
    }
   }
   if (timeIt == true)
   {
    DateTime stop = DateTime.Now;
    TimeSpan ts = stop - start;
    Console.WriteLine("");
    Console.WriteLine("Command(s) completed in: ");
    Console.WriteLine("   {0} seconds", ts.TotalSeconds);
    Console.WriteLine("   {0} milliseconds", ts.TotalMilliseconds);
   }
   return(true);
  }
 }
}
