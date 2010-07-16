

using System;
using System.Drawing;
using System.Collections;
using System.Text;
using Novell.AddressBook;

using Gtk;
using Gdk;
using Glade;
using GtkSharp;
using GLib;

namespace Novell.AddressBook.UI.gtk
{



 public class ContactEditor
 {
  public Contact Contact
  {
   get
   {
    return currentContact;
   }

   set
   {
    currentContact = value;
   }
  }

  public Gtk.Window TransientFor
  {
   set
   {
    contactEditorDialog.TransientFor = value;
   }
  }




  [Glade.Widget] internal Gtk.Image UserImage;
  [Glade.Widget] internal Gtk.Entry FullNameEntry;
  [Glade.Widget] internal Gtk.Entry NickNameEntry;


  [Glade.Widget] internal Gtk.Entry MOneEntry;
  [Glade.Widget] internal Gtk.Entry MTwoEntry;
  [Glade.Widget] internal Gtk.Entry MThreeEntry;
  [Glade.Widget] internal Gtk.Entry MFourEntry;
  [Glade.Widget] internal Gtk.OptionMenu MOneOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu MTwoOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu MThreeOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu MFourOptionMenu;


  [Glade.Widget] internal Gtk.Entry PhoneOneEntry;
  [Glade.Widget] internal Gtk.Entry PhoneTwoEntry;
  [Glade.Widget] internal Gtk.Entry PhoneThreeEntry;
  [Glade.Widget] internal Gtk.Entry PhoneFourEntry;
  [Glade.Widget] internal Gtk.OptionMenu POneOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu PTwoOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu PThreeOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu PFourOptionMenu;


  [Glade.Widget] internal Gtk.Entry IMOneEntry;
  [Glade.Widget] internal Gtk.Entry IMTwoEntry;
  [Glade.Widget] internal Gtk.Entry IMThreeEntry;
  [Glade.Widget] internal Gtk.OptionMenu IMOneOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu IMTwoOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu IMThreeOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu IMOneLocOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu IMTwoLocOptionMenu;
  [Glade.Widget] internal Gtk.OptionMenu IMThreeLocOptionMenu;


  [Glade.Widget] internal Gtk.Entry WebHomeEntry;
  [Glade.Widget] internal Gtk.Entry WebBlogEntry;
  [Glade.Widget] internal Gtk.Entry WebCalEntry;
  [Glade.Widget] internal Gtk.Entry WebCamEntry;


  [Glade.Widget] internal Gtk.Entry RoleEntry;
  [Glade.Widget] internal Gtk.Entry OrgEntry;
  [Glade.Widget] internal Gtk.Entry ManagerEntry;
  [Glade.Widget] internal Gtk.Entry TitleEntry;
  [Glade.Widget] internal Gtk.Entry WorkForceEntry;


  [Glade.Widget] internal Gtk.Entry BirthdayEntry;
  [Glade.Widget] internal Gtk.TextView NoteTextView;


  [Glade.Widget] internal Gtk.Entry WorkAddrEntry;
  [Glade.Widget] internal Gtk.Entry WorkAddr2Entry;
  [Glade.Widget] internal Gtk.Entry WorkCityEntry;
  [Glade.Widget] internal Gtk.Entry WorkZipEntry;
  [Glade.Widget] internal Gtk.Entry WorkStateEntry;
  [Glade.Widget] internal Gtk.Entry WorkCountryEntry;


  [Glade.Widget] internal Gtk.Entry HomeAddrEntry;
  [Glade.Widget] internal Gtk.Entry HomeAddr2Entry;
  [Glade.Widget] internal Gtk.Entry HomeCityEntry;
  [Glade.Widget] internal Gtk.Entry HomeZipEntry;
  [Glade.Widget] internal Gtk.Entry HomeStateEntry;
  [Glade.Widget] internal Gtk.Entry HomeCountryEntry;


  [Glade.Widget] internal Gtk.Entry OtherAddrEntry;
  [Glade.Widget] internal Gtk.Entry OtherAddr2Entry;
  [Glade.Widget] internal Gtk.Entry OtherCityEntry;
  [Glade.Widget] internal Gtk.Entry OtherZipEntry;
  [Glade.Widget] internal Gtk.Entry OtherStateEntry;
  [Glade.Widget] internal Gtk.Entry OtherCountryEntry;


  [Glade.Widget] internal Gtk.Button OKButton;

  internal Gtk.Dialog contactEditorDialog;
  internal Contact currentContact;

  internal Name preferredName;
  internal Email emailOne = null;
  internal Email emailTwo = null;
  internal Email emailThree = null;
  internal Email emailFour = null;
  internal Telephone phoneOne = null;
  internal Telephone phoneTwo = null;
  internal Telephone phoneThree = null;
  internal Telephone phoneFour = null;
  internal Address workAddress = null;
  internal Address homeAddress = null;
  internal Address otherAddress = null;
  internal IM imOne = null;
  internal IM imTwo = null;
  internal IM imThree = null;

  private static readonly string PROVIDER_AIM = "AIM";
  private static readonly string PROVIDER_IRC = "IRC";
  private static readonly string PROVIDER_JABBER = "Jabber";
  private static readonly string PROVIDER_MSN = "MSN";
  private static readonly string PROVIDER_GROUPWISE = "GroupWise";
  private static readonly string PROVIDER_YAHOO = "Yahoo";




  public ContactEditor()
  {
   InitGlade();
  }






  private void InitGlade()
  {
   Glade.XML gxml = new Glade.XML (Util.GladePath("contact-editor.glade"),
     "ContactEditor", null);

   gxml.Autoconnect (this);

   contactEditorDialog = (Gtk.Dialog) gxml.GetWidget("ContactEditor");

   FullNameEntry.HasFocus = true;
  }






  public int Run()
  {
   int rc = 0;

   if(contactEditorDialog != null)
   {
    if(currentContact == null)
     currentContact = new Contact();

    PopulateWidgets();
    while(rc == 0)
    {
     rc = contactEditorDialog.Run();
     Console.WriteLine("Result: {0}", rc);
     if(rc == -5)
      SaveContact();
     if(rc == -11)
     {
      rc = 0;
      Util.ShowHelp("bq6lx1r.html", null);
     }
    }
    contactEditorDialog.Hide();
    contactEditorDialog.Destroy();
    contactEditorDialog = null;
   }
   return rc;
  }







  private void PopulateWidgets()
  {
   Pixbuf pb = GetScaledPhoto(currentContact, 64);
   if(pb != null)
    UserImage.FromPixbuf = pb;

   try
   {
    preferredName = currentContact.GetPreferredName();
   }
   catch(Exception e)
   {
   }

   FullNameEntry.Text = currentContact.FN;

   NickNameEntry.Text = currentContact.Nickname;

   PopulateEmails();
   PopulatePhoneNumbers();
   PopulateIM();


   WebHomeEntry.Text = currentContact.Url;
   WebBlogEntry.Text = currentContact.Blog;



   RoleEntry.Text = currentContact.Role;
   OrgEntry.Text = currentContact.Organization;
   ManagerEntry.Text = currentContact.ManagerID;
   TitleEntry.Text = currentContact.Title;
   WorkForceEntry.Text = currentContact.WorkForceID;

   BirthdayEntry.Text = currentContact.Birthday;
   NoteTextView.Buffer.Text = currentContact.Note;


   PopulateAddresses();
  }
  internal void PopulateIM()
  {
   PopulateIMType(IMTypes.preferred);
   PopulateIMType(IMTypes.work);
   PopulateIMType(IMTypes.home);
   PopulateIMType(IMTypes.other);
  }
  internal void PopulateIMType(IMTypes types)
  {
   foreach(IM im in currentContact.GetInstantMessageAccounts())
   {
    if( types != IMTypes.preferred )
    {
     if( (im.Types & IMTypes.preferred) ==
       IMTypes.preferred)
     {
      continue;
     }
    }
    if( (im.Types & types) != types)
    {
     continue;
    }
    if(imOne == null)
    {
     imOne = im;
     IMOneEntry.Text = im.Address;
     SetIMTypeMenu(IMOneLocOptionMenu, im.Types);
     SetIMTypeProvider(IMOneOptionMenu, im.Provider);
    }
    else if(imTwo == null)
    {
     imTwo = im;
     IMTwoEntry.Text = im.Address;
     SetIMTypeMenu(IMTwoLocOptionMenu, im.Types);
     SetIMTypeProvider(IMTwoOptionMenu, im.Provider);
    }
    else if(imThree == null)
    {
     imThree = im;
     IMThreeEntry.Text = im.Address;
     SetIMTypeMenu(IMThreeLocOptionMenu, im.Types);
     SetIMTypeProvider(IMThreeOptionMenu, im.Provider);
    }
   }
  }
  internal void SetIMTypeMenu(Gtk.OptionMenu locMenu, IMTypes type)
  {
   if( (type & IMTypes.work) == IMTypes.work)
   {
    locMenu.SetHistory(0);
    return;
   }
   if( (type & IMTypes.home) == IMTypes.home)
   {
    locMenu.SetHistory(1);
    return;
   }
   locMenu.SetHistory(2);
  }
  internal void SetIMTypeProvider(Gtk.OptionMenu providerMenu,
       string provider)
  {
   if(provider == PROVIDER_AIM)
   {
    providerMenu.SetHistory(0);
    return;
   }
   if(provider == PROVIDER_IRC)
   {
    providerMenu.SetHistory(1);
    return;
   }
   if(provider == PROVIDER_JABBER)
   {
    providerMenu.SetHistory(2);
    return;
   }
   if(provider == PROVIDER_MSN)
   {
    providerMenu.SetHistory(3);
    return;
   }
   if(provider == PROVIDER_GROUPWISE)
   {
    providerMenu.SetHistory(4);
    return;
   }
   if(provider == PROVIDER_YAHOO)
   {
    providerMenu.SetHistory(5);
    return;
   }
   providerMenu.SetHistory(0);
  }
  internal void SaveIMInfo()
  {
   if(IMOneEntry.Text.Length > 0)
   {
    IMTypes selType = GetIMType(IMOneLocOptionMenu);
    string selProvider = GetIMProvider(IMOneOptionMenu);
    if(imOne == null)
    {
     imOne = new IM( IMOneEntry.Text, selProvider,
      (selType | IMTypes.preferred));
     currentContact.AddInstantMessage(imOne);
    }
    else
    {
     imOne.Types = (selType | IMTypes.preferred);
     imOne.Address = IMOneEntry.Text;
     imOne.Provider = selProvider;
    }
   }
   else
   {
    if(imOne != null)
    {
     imOne.Delete();
    }
   }
   if(IMTwoEntry.Text.Length > 0)
   {
    IMTypes selType = GetIMType(IMTwoLocOptionMenu);
    string selProvider = GetIMProvider(IMTwoOptionMenu);
    if(imTwo == null)
    {
     imTwo = new IM(IMTwoEntry.Text,
       selProvider, selType);
     currentContact.AddInstantMessage(imTwo);
    }
    else
    {
     imTwo.Types = selType;
     imTwo.Address = IMTwoEntry.Text;
     imTwo.Provider = selProvider;
    }
   }
   else
   {
    if(imTwo != null)
    {
     imTwo.Delete();
    }
   }
   if(IMThreeEntry.Text.Length > 0)
   {
    IMTypes selType = GetIMType(IMThreeLocOptionMenu);
    string selProvider = GetIMProvider(IMThreeOptionMenu);
    if(imThree == null)
    {
     imThree = new IM(IMThreeEntry.Text,
       selProvider, selType);
     currentContact.AddInstantMessage(imThree);
    }
    else
    {
     imThree.Types = selType;
     imThree.Address = IMThreeEntry.Text;
     imThree.Provider = selProvider;
    }
   }
   else
   {
    if(imThree != null)
    {
     imThree.Delete();
    }
   }
  }
  internal IMTypes GetIMType(Gtk.OptionMenu optMenu)
  {
   switch(optMenu.History)
   {
    case 0:
     return IMTypes.work;
    case 1:
     return IMTypes.home;
    case 2:
     return IMTypes.other;
   }
   return IMTypes.other;
  }
  internal string GetIMProvider(Gtk.OptionMenu optMenu)
  {
   switch(optMenu.History)
   {
    case 0:
     return PROVIDER_AIM;
    case 1:
     return PROVIDER_IRC;
    case 2:
     return PROVIDER_JABBER;
    case 3:
     return PROVIDER_MSN;
    case 4:
     return PROVIDER_GROUPWISE;
    case 5:
     return PROVIDER_YAHOO;
   }
   return PROVIDER_AIM;
  }
  internal void PopulatePhoneNumbers()
  {
   PopulatePhoneType(PhoneTypes.preferred);
   PopulatePhoneType(PhoneTypes.work);
   PopulatePhoneType(PhoneTypes.home);
   PopulatePhoneType(PhoneTypes.other);
  }
  internal void PopulatePhoneType(PhoneTypes types)
  {
   foreach(Telephone phone in currentContact.GetTelephoneNumbers())
   {
    if( types != PhoneTypes.preferred )
    {
     if( (phone.Types & PhoneTypes.preferred) ==
       PhoneTypes.preferred)
     {
      continue;
     }
    }
    if( (phone.Types & types) != types)
    {
     continue;
    }
    if(phoneOne == null)
    {
     phoneOne = phone;
     PhoneOneEntry.Text = phone.Number;
     SetPhoneOptionMenu(POneOptionMenu, phone.Types);
    }
    else if(phoneTwo == null)
    {
     phoneTwo = phone;
     PhoneTwoEntry.Text = phone.Number;
     SetPhoneOptionMenu(PTwoOptionMenu, phone.Types);
    }
    else if(phoneThree == null)
    {
     phoneThree = phone;
     PhoneThreeEntry.Text = phone.Number;
     SetPhoneOptionMenu(PThreeOptionMenu, phone.Types);
    }
    else if(phoneFour == null)
    {
     phoneFour = phone;
     PhoneFourEntry.Text = phone.Number;
     SetPhoneOptionMenu(PFourOptionMenu, phone.Types);
    }
   }
  }
  internal void SetPhoneOptionMenu(Gtk.OptionMenu optMenu,
    PhoneTypes type)
  {
   if( (type & PhoneTypes.work) == PhoneTypes.work)
   {
    optMenu.SetHistory(0);
    return;
   }
   if( (type & PhoneTypes.home) == PhoneTypes.home)
   {
    optMenu.SetHistory(1);
    return;
   }
   optMenu.SetHistory(2);
  }
  internal void SavePhoneNumbers()
  {
   if(PhoneOneEntry.Text.Length > 0)
   {
    PhoneTypes selType = GetPhoneType(POneOptionMenu);
    if(phoneOne == null)
    {
     phoneOne = new Telephone(
      PhoneOneEntry.Text,
      (selType | PhoneTypes.preferred));
     currentContact.AddTelephoneNumber(phoneOne);
    }
    else
    {
     phoneOne.Types = (selType | PhoneTypes.preferred);
     phoneOne.Number = PhoneOneEntry.Text;
    }
   }
   else
   {
    if(phoneOne != null)
    {
     phoneOne.Delete();
    }
   }
   if(PhoneTwoEntry.Text.Length > 0)
   {
    PhoneTypes selType = GetPhoneType(PTwoOptionMenu);
    if(phoneTwo == null)
    {
     phoneTwo = new Telephone(PhoneTwoEntry.Text, selType);
     currentContact.AddTelephoneNumber(phoneTwo);
    }
    else
    {
     phoneTwo.Types = selType;
     phoneTwo.Number = PhoneTwoEntry.Text;
    }
   }
   else
   {
    if(phoneTwo != null)
    {
     phoneTwo.Delete();
    }
   }
   if(PhoneThreeEntry.Text.Length > 0)
   {
    PhoneTypes selType = GetPhoneType(PThreeOptionMenu);
    if(phoneThree == null)
    {
     phoneThree = new Telephone(PhoneThreeEntry.Text, selType);
     currentContact.AddTelephoneNumber(phoneThree);
    }
    else
    {
     phoneThree.Types = selType;
     phoneThree.Number = PhoneThreeEntry.Text;
    }
   }
   else
   {
    if(phoneThree != null)
    {
     phoneThree.Delete();
    }
   }
   if(PhoneFourEntry.Text.Length > 0)
   {
    PhoneTypes selType = GetPhoneType(PFourOptionMenu);
    if(phoneFour == null)
    {
     phoneFour = new Telephone(PhoneFourEntry.Text, selType);
     currentContact.AddTelephoneNumber(phoneFour);
    }
    else
    {
     phoneFour.Types = selType;
     phoneFour.Number = PhoneFourEntry.Text;
    }
   }
   else
   {
    if(phoneFour != null)
    {
     phoneFour.Delete();
    }
   }
  }
  internal PhoneTypes GetPhoneType(Gtk.OptionMenu optMenu)
  {
   switch(optMenu.History)
   {
    case 0:
     return PhoneTypes.work;
    case 1:
     return PhoneTypes.home;
    case 2:
     return PhoneTypes.other;
   }
   return PhoneTypes.other;
  }
  internal void PopulateEmails()
  {
   PopulateEmailType(EmailTypes.preferred);
   PopulateEmailType(EmailTypes.work);
   PopulateEmailType(EmailTypes.personal);
   PopulateEmailType(EmailTypes.other);
  }
  internal void PopulateEmailType(EmailTypes types)
  {
   foreach(Email e in currentContact.GetEmailAddresses())
   {
    if( types != EmailTypes.preferred )
    {
     if( (e.Types & EmailTypes.preferred)==EmailTypes.preferred)
     {
      continue;
     }
    }
    if( (e.Types & types) != types)
    {
     continue;
    }
    if(emailOne == null)
    {
     emailOne = e;
     MOneEntry.Text = emailOne.Address;
     SetMailOptionMenu(MOneOptionMenu, emailOne.Types);
    }
    else if(emailTwo == null)
    {
     emailTwo = e;
     MTwoEntry.Text = e.Address;
     SetMailOptionMenu(MTwoOptionMenu, emailTwo.Types);
    }
    else if(emailThree == null)
    {
     emailThree = e;
     MThreeEntry.Text = e.Address;
     SetMailOptionMenu(MThreeOptionMenu, emailThree.Types);
    }
    else if(emailFour == null)
    {
     emailFour = e;
     MFourEntry.Text = e.Address;
     SetMailOptionMenu(MFourOptionMenu, emailFour.Types);
    }
   }
  }
  internal void SetMailOptionMenu(Gtk.OptionMenu optMenu, EmailTypes type)
  {
   if( (type & EmailTypes.work) == EmailTypes.work)
   {
    optMenu.SetHistory(0);
    return;
   }
   if( (type & EmailTypes.personal) == EmailTypes.personal)
   {
    optMenu.SetHistory(1);
    return;
   }
   optMenu.SetHistory(2);
  }
  internal EmailTypes GetMailType(Gtk.OptionMenu optMenu)
  {
   switch(optMenu.History)
   {
    case 0:
     return EmailTypes.work;
    case 1:
     return EmailTypes.personal;
    case 2:
     return EmailTypes.other;
   }
   return EmailTypes.other;
  }
  internal void SaveCurrentEmails()
  {
   if(MOneEntry.Text.Length > 0)
   {
    EmailTypes selType = GetMailType(MOneOptionMenu);
    if(emailOne == null)
    {
     emailOne = new Email(
      (selType | EmailTypes.preferred),
      MOneEntry.Text);
     currentContact.AddEmailAddress(emailOne);
    }
    else
    {
     emailOne.Types = (selType | EmailTypes.preferred);
     emailOne.Address = MOneEntry.Text;
    }
   }
   else
   {
    if(emailOne != null)
    {
     emailOne.Delete();
    }
   }
   if(MTwoEntry.Text.Length > 0)
   {
    EmailTypes selType = GetMailType(MTwoOptionMenu);
    if(emailTwo == null)
    {
     emailTwo = new Email(
      selType,
      MTwoEntry.Text);
     currentContact.AddEmailAddress(emailTwo);
    }
    else
    {
     emailTwo.Types = selType;
     emailTwo.Address = MTwoEntry.Text;
    }
   }
   else
   {
    if(emailTwo != null)
    {
     emailTwo.Delete();
    }
   }
   if(MThreeEntry.Text.Length > 0)
   {
    EmailTypes selType = GetMailType(MThreeOptionMenu);
    if(emailThree == null)
    {
     emailThree = new Email(
      selType,
      MThreeEntry.Text);
     currentContact.AddEmailAddress(emailThree);
    }
    else
    {
     emailThree.Types = selType;
     emailThree.Address = MThreeEntry.Text;
    }
   }
   else
   {
    if(emailThree != null)
    {
     emailThree.Delete();
    }
   }
   if(MFourEntry.Text.Length > 0)
   {
    EmailTypes selType = GetMailType(MFourOptionMenu);
    if(emailFour == null)
    {
     emailFour = new Email(
      selType,
      MFourEntry.Text);
     currentContact.AddEmailAddress(emailFour);
    }
    else
    {
     emailFour.Types = selType;
     emailFour.Address = MFourEntry.Text;
    }
   }
   else
   {
    if(emailFour != null)
    {
     emailFour.Delete();
    }
   }
  }
  internal Address GetAddressType(AddressTypes type)
  {
   foreach(Address addr in currentContact.GetAddresses())
   {
    if( (addr.Types & type) == type)
     return addr;
   }
   return null;
  }
  internal void PopulateAddresses()
  {
   workAddress = GetAddressType(AddressTypes.work);
   if(workAddress != null)
   {
    if(workAddress.Street.Length > 0)
     WorkAddrEntry.Text = workAddress.Street;
    if(workAddress.ExtendedAddress.Length > 0)
     WorkAddr2Entry.Text = workAddress.ExtendedAddress;
    if(workAddress.Locality.Length > 0)
     WorkCityEntry.Text = workAddress.Locality;
    if(workAddress.Region.Length > 0)
     WorkStateEntry.Text = workAddress.Region;
    if(workAddress.PostalCode.Length > 0)
     WorkZipEntry.Text = workAddress.PostalCode;
    if(workAddress.Country.Length > 0)
     WorkCountryEntry.Text = workAddress.Country;
   }
   homeAddress = GetAddressType(AddressTypes.home);
   if(homeAddress != null)
   {
    if(homeAddress.Street.Length > 0)
     HomeAddrEntry.Text = homeAddress.Street;
    if(homeAddress.ExtendedAddress.Length > 0)
     HomeAddr2Entry.Text = homeAddress.ExtendedAddress;
    if(homeAddress.Locality.Length > 0)
     HomeCityEntry.Text = homeAddress.Locality;
    if(homeAddress.Region.Length > 0)
     HomeStateEntry.Text = homeAddress.Region;
    if(homeAddress.PostalCode.Length > 0)
     HomeZipEntry.Text = homeAddress.PostalCode;
    if(homeAddress.Country.Length > 0)
     HomeCountryEntry.Text = homeAddress.Country;
   }
   otherAddress = GetAddressType(AddressTypes.other);
   if(otherAddress != null)
   {
    if(otherAddress.Street.Length > 0)
     OtherAddrEntry.Text = otherAddress.Street;
    if(otherAddress.ExtendedAddress.Length > 0)
     OtherAddr2Entry.Text = otherAddress.ExtendedAddress;
    if(otherAddress.Locality.Length > 0)
     OtherCityEntry.Text = otherAddress.Locality;
    if(otherAddress.Region.Length > 0)
     OtherStateEntry.Text = otherAddress.Region;
    if(otherAddress.PostalCode.Length > 0)
     OtherZipEntry.Text = otherAddress.PostalCode;
    if(otherAddress.Country.Length > 0)
     OtherCountryEntry.Text = otherAddress.Country;
   }
  }
  internal void SaveAddresses()
  {
   if( (WorkAddrEntry.Text.Length > 0) ||
    (WorkAddr2Entry.Text.Length > 0) ||
    (WorkCityEntry.Text.Length > 0) ||
    (WorkStateEntry.Text.Length > 0) ||
    (WorkZipEntry.Text.Length > 0) ||
    (WorkCountryEntry.Text.Length > 0) )
   {
    if(workAddress == null)
    {
     workAddress = new Address();
     currentContact.AddAddress(workAddress);
    }
    workAddress.Types = AddressTypes.work;
    workAddress.Street = WorkAddrEntry.Text;
    workAddress.ExtendedAddress = WorkAddr2Entry.Text;
    workAddress.Locality = WorkCityEntry.Text;
    workAddress.Region = WorkStateEntry.Text;
    workAddress.PostalCode = WorkZipEntry.Text;
    workAddress.Country = WorkCountryEntry.Text;
   }
   else
   {
    if(workAddress != null)
    {
     workAddress.Delete();
    }
   }
   if( (HomeAddrEntry.Text.Length > 0) ||
    (HomeAddr2Entry.Text.Length > 0) ||
    (HomeCityEntry.Text.Length > 0) ||
    (HomeStateEntry.Text.Length > 0) ||
    (HomeZipEntry.Text.Length > 0) ||
    (HomeCountryEntry.Text.Length > 0) )
   {
    if(homeAddress == null)
    {
     homeAddress = new Address();
     currentContact.AddAddress(homeAddress);
    }
    homeAddress.Types = AddressTypes.home;
    homeAddress.Street = HomeAddrEntry.Text;
    homeAddress.ExtendedAddress = HomeAddr2Entry.Text;
    homeAddress.Locality = HomeCityEntry.Text;
    homeAddress.Region = HomeStateEntry.Text;
    homeAddress.PostalCode = HomeZipEntry.Text;
    homeAddress.Country = HomeCountryEntry.Text;
   }
   else
   {
    if(homeAddress != null)
    {
     homeAddress.Delete();
    }
   }
   if( (OtherAddrEntry.Text.Length > 0) ||
    (OtherAddr2Entry.Text.Length > 0) ||
    (OtherCityEntry.Text.Length > 0) ||
    (OtherStateEntry.Text.Length > 0) ||
    (OtherZipEntry.Text.Length > 0) ||
    (OtherCountryEntry.Text.Length > 0) )
   {
    if(otherAddress == null)
    {
     otherAddress = new Address();
     currentContact.AddAddress(otherAddress);
    }
    otherAddress.Types = AddressTypes.other;
    otherAddress.Street = OtherAddrEntry.Text;
    otherAddress.ExtendedAddress = OtherAddr2Entry.Text;
    otherAddress.Locality = OtherCityEntry.Text;
    otherAddress.Region = OtherStateEntry.Text;
    otherAddress.PostalCode = OtherZipEntry.Text;
    otherAddress.Country = OtherCountryEntry.Text;
   }
   else
   {
    if(otherAddress != null)
    {
     otherAddress.Delete();
    }
   }
  }
  private void SaveContact()
  {
   if(NickNameEntry.Text.Length > 0)
    currentContact.Nickname = NickNameEntry.Text;
   else
    currentContact.Nickname = null;
   if(FullNameEntry.Text.Length > 0)
   {
    if(preferredName != null)
     preferredName.Delete();
    preferredName = new Name();
    preferredName.Preferred = true;
    currentContact.AddName(preferredName);
    Novell.iFolder.FormsBookLib.FullNameParser.Parse(
      FullNameEntry.Text,
      ref preferredName);
   }
   else
   {
    if(preferredName != null)
     preferredName.Delete();
   }
   if( (currentContact.UserName == null) ||
    (currentContact.UserName.Length == 0) )
   {
    if(MOneEntry.Text.Length > 0)
     currentContact.UserName = MOneEntry.Text;
    else if(MTwoEntry.Text.Length > 0)
     currentContact.UserName = MTwoEntry.Text;
    else if(FullNameEntry.Text.Length > 0)
     currentContact.UserName = FullNameEntry.Text;
   }
   SaveCurrentEmails();
   SavePhoneNumbers();
   SaveIMInfo();
   if(WebHomeEntry.Text.Length > 0)
    currentContact.Url = WebHomeEntry.Text;
   else
    currentContact.Url = null;
   if(WebBlogEntry.Text.Length > 0)
    currentContact.Blog = WebBlogEntry.Text;
   else
    currentContact.Blog = null;
   if(RoleEntry.Text.Length > 0)
    currentContact.Role = RoleEntry.Text;
   else
    currentContact.Role = null;
   if(OrgEntry.Text.Length > 0)
    currentContact.Organization = OrgEntry.Text;
   else
    currentContact.Organization = null;
   if(ManagerEntry.Text.Length > 0)
    currentContact.ManagerID = ManagerEntry.Text;
   else
    currentContact.ManagerID = null;
   if(TitleEntry.Text.Length > 0)
    currentContact.Title = TitleEntry.Text;
   else
    currentContact.Title = null;
   if(WorkForceEntry.Text.Length > 0)
    currentContact.WorkForceID = WorkForceEntry.Text;
   else
    currentContact.WorkForceID = null;
   if(BirthdayEntry.Text.Length > 0)
    currentContact.Birthday = BirthdayEntry.Text;
   else
    currentContact.Birthday = null;
   if(NoteTextView.Buffer.Text.Length > 0)
    currentContact.Note = NoteTextView.Buffer.Text;
   else
    currentContact.Note = null;
   SaveAddresses();
  }
  private void on_FullNameButton_clicked(object o, EventArgs args)
  {
   if(preferredName != null)
    preferredName.Delete();
   preferredName = new Name();
   preferredName.Preferred = true;
   currentContact.AddName(preferredName);
   Novell.iFolder.FormsBookLib.FullNameParser.Parse(
     FullNameEntry.Text,
     ref preferredName);
   NameEditor ne = new NameEditor();
   ne.TransientFor = contactEditorDialog;
   ne.Name = preferredName;
   if(ne.Run() == -5)
   {
    FullNameEntry.Text = preferredName.FN;
   }
  }
  private void on_NickNameEntry_changed(object o, EventArgs args)
  {
  }
  private void on_FullNameEntry_changed(object o, EventArgs args)
  {
   OKButton.Sensitive = CheckForIDText();
  }
  private void on_MOneEntry_changed(object o, EventArgs args)
  {
   OKButton.Sensitive = CheckForIDText();
  }
  private bool CheckForIDText()
  {
   if( (FullNameEntry.Text.Length > 0) ||
    (NickNameEntry.Text.Length > 0) ||
    (MOneEntry.Text.Length > 0) )
    return true;
   return false;
  }
  private void on_UserImageButton_clicked(object o, EventArgs args)
  {
   FileSelection fs = new FileSelection("Choose a new Image");
   fs.Modal = true;
   fs.TransientFor = contactEditorDialog;
   int retVal = fs.Run();
   fs.Hide();
   if(retVal == -5)
   {
    if(currentContact.ImportPhoto(fs.Filename))
    {
     Pixbuf pb = GetScaledPhoto(currentContact, 64);
     if(pb != null)
      UserImage.FromPixbuf = pb;
    }
   }
  }
  private Pixbuf GetScaledPhoto(Contact contact, int height)
  {
   Pixbuf pb = null;
   try
   {
    int newWidth, newHeight;
    pb = new Pixbuf(contact.ExportPhoto());
    newHeight = height;
    newWidth = height;
    if(pb.Height != pb.Width)
    {
     int perc = (height * 1000) / pb.Height;
     newWidth = pb.Width * perc / 1000;
    }
    pb = pb.ScaleSimple(newWidth, newHeight,
      InterpType.Bilinear);
   }
   catch(Exception e)
   {
    pb = null;
   }
   return pb;
  }
 }
}
