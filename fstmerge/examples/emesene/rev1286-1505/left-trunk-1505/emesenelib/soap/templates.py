passport ='''<?xml version="1.0" encoding="UTF-8"?>
<Envelope xmlns="http://schemas.xmlsoap.org/soap/envelope/" xmlns:wsse="http://schemas.xmlsoap.org/ws/2003/06/secext" xmlns:saml="urn:oasis:names:tc:SAML:1.0:assertion" xmlns:wsp="http://schemas.xmlsoap.org/ws/2002/12/policy" xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd" xmlns:wsa="http://schemas.xmlsoap.org/ws/2004/03/addressing" xmlns:wssc="http://schemas.xmlsoap.org/ws/2004/04/sc" xmlns:wst="http://schemas.xmlsoap.org/ws/2004/04/trust">
  <Header>
    <ps:AuthInfo xmlns:ps="http://schemas.microsoft.com/Passport/SoapServices/PPCRL" Id="PPAuthInfo">
           <ps:HostingApp>{7108E71A-9926-4FCB-BCC9-9A9D3F32E423}</ps:HostingApp>
           <ps:BinaryVersion>4</ps:BinaryVersion>
           <ps:UIVersion>1</ps:UIVersion>
           <ps:Cookies></ps:Cookies>
           <ps:RequestParams>AQAAAAIAAABsYwQAAAAxMDMz</ps:RequestParams>
       </ps:AuthInfo>
    <wsse:Security>
       <wsse:UsernameToken Id="user">
         <wsse:Username>%s</wsse:Username> 
         <wsse:Password>%s</wsse:Password>
       </wsse:UsernameToken>
    </wsse:Security>
  </Header>
  <Body>
    <ps:RequestMultipleSecurityTokens xmlns:ps="http://schemas.microsoft.com/Passport/SoapServices/PPCRL" Id="RSTS">
      <wst:RequestSecurityToken Id="RST0">
        <wst:RequestType>http://schemas.xmlsoap.org/ws/2004/04/security/trust/Issue</wst:RequestType>
        <wsp:AppliesTo>
          <wsa:EndpointReference>				
            <wsa:Address>http://Passport.NET/tb</wsa:Address>
          </wsa:EndpointReference>
        </wsp:AppliesTo>
      </wst:RequestSecurityToken>
      <wst:RequestSecurityToken Id="RST1">
        <wst:RequestType>http://schemas.xmlsoap.org/ws/2004/04/security/trust/Issue</wst:RequestType>
        <wsp:AppliesTo>
          <wsa:EndpointReference>
            <wsa:Address>messengerclear.live.com</wsa:Address>
          </wsa:EndpointReference>
        </wsp:AppliesTo>
        <wsse:PolicyReference URI="MBI_KEY_OLD"></wsse:PolicyReference>
      </wst:RequestSecurityToken>
        <wst:RequestSecurityToken Id="RST2">
            <wst:RequestType>http://schemas.xmlsoap.org/ws/2004/04/security/trust/Issue</wst:RequestType>
                <wsp:AppliesTo>
                    <wsa:EndpointReference>
                    <wsa:Address>messenger.msn.com</wsa:Address>
                </wsa:EndpointReference>
            </wsp:AppliesTo>
            <wsse:PolicyReference URI="?id=507"></wsse:PolicyReference>
        </wst:RequestSecurityToken>
        <wst:RequestSecurityToken Id="RST3">
            <wst:RequestType>http://schemas.xmlsoap.org/ws/2004/04/security/trust/Issue</wst:RequestType>
            <wsp:AppliesTo>
                <wsa:EndpointReference>
                    <wsa:Address>contacts.msn.com</wsa:Address>
                </wsa:EndpointReference>
            </wsp:AppliesTo>
            <wsse:PolicyReference URI="?fs=1&amp;id=24000&amp;kv=9&amp;rn=93S9SWWw&amp;tw=0&amp;ver=2.1.6000.1"></wsse:PolicyReference>
        </wst:RequestSecurityToken>
        <wst:RequestSecurityToken Id="RST4">
            <wst:RequestType>http://schemas.xmlsoap.org/ws/2004/04/security/trust/Issue</wst:RequestType>
            <wsp:AppliesTo>
                <wsa:EndpointReference>
                    <wsa:Address>messengersecure.live.com</wsa:Address>
                </wsa:EndpointReference>
            </wsp:AppliesTo>
            <wsse:PolicyReference URI="MBI_SSL"></wsse:PolicyReference>
        </wst:RequestSecurityToken>
<wst:RequestSecurityToken Id="RST5">
<wst:RequestType>http://schemas.xmlsoap.org/ws/2004/04/security/trust/Issue</wst:RequestType>
<wsp:AppliesTo>
<wsa:EndpointReference>
<wsa:Address>storage.msn.com</wsa:Address>
</wsa:EndpointReference>
</wsp:AppliesTo>
<wsse:PolicyReference URI="MBI">
</wsse:PolicyReference>
</wst:RequestSecurityToken>
    </ps:RequestMultipleSecurityTokens>
  </Body>
</Envelope>'''
membershipList = '''<?xml version='1.0' encoding='utf-8'?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
   <soap:Header xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
       <ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook">
           <ApplicationId xmlns="http://www.msn.com/webservices/AddressBook">CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId>
           <IsMigration xmlns="http://www.msn.com/webservices/AddressBook">false</IsMigration>
           <PartnerScenario xmlns="http://www.msn.com/webservices/AddressBook">Initial</PartnerScenario>
       </ABApplicationHeader>
       <ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook">
           <ManagedGroupRequest xmlns="http://www.msn.com/webservices/AddressBook">false</ManagedGroupRequest>
           <TicketToken>&tickettoken;</TicketToken>
       </ABAuthHeader>
   </soap:Header>
   <soap:Body xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
       <FindMembership xmlns="http://www.msn.com/webservices/AddressBook">
           <serviceFilter xmlns="http://www.msn.com/webservices/AddressBook">
               <Types xmlns="http://www.msn.com/webservices/AddressBook">
                   <ServiceType xmlns="http://www.msn.com/webservices/AddressBook">Messenger</ServiceType>
                   <ServiceType xmlns="http://www.msn.com/webservices/AddressBook">Invitation</ServiceType>
                   <ServiceType xmlns="http://www.msn.com/webservices/AddressBook">SocialNetwork</ServiceType>
                   <ServiceType xmlns="http://www.msn.com/webservices/AddressBook">Space</ServiceType>
                   <ServiceType xmlns="http://www.msn.com/webservices/AddressBook">Profile</ServiceType>
               </Types>
           </serviceFilter>
           <View xmlns="http://www.msn.com/webservices/AddressBook">Full</View>
        </FindMembership>
   </soap:Body>
</soap:Envelope>\r\n'''
addressBook = '''<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
    <soap:Header>
        <ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook">
            <ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId>
            <IsMigration>false</IsMigration>
            <PartnerScenario>Initial</PartnerScenario>
        </ABApplicationHeader>
        <ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook">
            <ManagedGroupRequest>false</ManagedGroupRequest>
            <TicketToken>&tickettoken;</TicketToken>
        </ABAuthHeader>
    </soap:Header>
    <soap:Body>
        <ABFindAll xmlns="http://www.msn.com/webservices/AddressBook">
            <abId>00000000-0000-0000-0000-000000000000</abId>
            <abView>Full</abView>
            <lastChange>0001-01-01T00:00:00.0000000-08:00</lastChange>
        </ABFindAll>
    </soap:Body>
</soap:Envelope>\r\n'''
dynamicItems = '''<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
    <soap:Header>
        <ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook">
            <ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId>
            <IsMigration>false</IsMigration>
            <PartnerScenario>Initial</PartnerScenario>
        </ABApplicationHeader>
        <ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook">
            <ManagedGroupRequest>false</ManagedGroupRequest>
            <TicketToken>&tickettoken;</TicketToken>
        </ABAuthHeader>
    </soap:Header>
    <soap:Body>
        <ABFindAll xmlns="http://www.msn.com/webservices/AddressBook">
            <abId>00000000-0000-0000-0000-000000000000</abId>
            <abView>Full</abView>
            <lastChange>0001-01-01T00:00:00.0000000-08:00</lastChange>
            <dynamicItemView>Gleam</dynamicItemView>
            <dynamicItemLastChange>0001-01-01T00:00:00.0000000-08:00</dynamicItemLastChange>
        </ABFindAll>
    </soap:Body>
</soap:Envelope>\r\n'''
oldaddUserToGroup =  '''<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xmlns:xsd="http://www.w3.org/2001/XMLSchema"
xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">'
<soap:Header>
  <ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook">
    <ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId>
    <IsMigration>false</IsMigration>
    <PartnerScenario>Timer</PartnerScenario>
  </ABApplicationHeader>
  <ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook">
    <ManagedGroupRequest>false</ManagedGroupRequest>
    <TicketToken>&tickettoken;</TicketToken>
  </ABAuthHeader>
</soap:Header>
<soap:Body>
  <ABGroupContactAdd xmlns="http://www.msn.com/webservices/AddressBook">
    <abId>00000000-0000-0000-0000-000000000000</abId>
    <groupFilter>
      <groupIds>
        <guid>%s</guid>
      </groupIds>
    </groupFilter>
    <contacts>
      <Contact>
        <contactId>%s</contactId>
      </Contact>
    </contacts>
  </ABGroupContactAdd>
</soap:Body>
</soap:Envelope>\r\n'''
moveUserToGroup = '''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario>Timer</PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><ABGroupContactAdd xmlns="http://www.msn.com/webservices/AddressBook"><abId>00000000-0000-0000-0000-000000000000</abId><groupFilter><groupIds><guid>%s</guid></groupIds></groupFilter><contacts><Contact><contactId>%s</contactId></Contact></contacts></ABGroupContactAdd></soap:Body></soap:Envelope>'''
deleteUserFromGroup = '''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario>Timer</PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><ABGroupContactDelete xmlns="http://www.msn.com/webservices/AddressBook"><abId>00000000-0000-0000-0000-000000000000</abId><contacts><Contact><contactId>%s</contactId></Contact></contacts><groupFilter><groupIds><guid>%s</guid></groupIds></groupFilter></ABGroupContactDelete></soap:Body></soap:Envelope>'''
addUserToGroup = '''<?xml version="1.0" ?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario>BlockUnblock</PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body>
<ABGroupContactAdd xmlns="http://www.msn.com/webservices/AddressBook">
<abId>00000000-0000-0000-0000-000000000000</abId>
<groupFilter><groupIds><guid>%s</guid></groupIds></groupFilter>
<contacts><Contact xmlns="http://www.msn.com/webservices/AddressBook"><contactInfo><passportName>%s</passportName><isSmtp>false</isSmtp><isMessengerUser>true</isMessengerUser></contactInfo></Contact></contacts>
<groupContactAddOptions><fGenerateMissingQuickName>true</fGenerateMissingQuickName></groupContactAddOptions>
</ABGroupContactAdd>
</soap:Body></soap:Envelope>'''
changeNick='''<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario>Timer</PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><ABContactUpdate xmlns="http://www.msn.com/webservices/AddressBook"><abId>00000000-0000-0000-0000-000000000000</abId>
<contacts><Contact xmlns="http://www.msn.com/webservices/AddressBook"><contactInfo><contactType>%s</contactType><displayName>%s</displayName></contactInfo><propertiesChanged>DisplayName</propertiesChanged></Contact></contacts>
</ABContactUpdate></soap:Body></soap:Envelope>\r\n'''
renameContact = '''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario>Timer</PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><ABContactUpdate xmlns="http://www.msn.com/webservices/AddressBook"><abId>00000000-0000-0000-0000-000000000000</abId>
<contacts><Contact xmlns="http://www.msn.com/webservices/AddressBook"><contactId>%s</contactId><contactInfo><annotations><Annotation><Name>AB.NickName</Name><Value>%s</Value></Annotation></annotations></contactInfo><propertiesChanged>Annotation </propertiesChanged></Contact></contacts>
</ABContactUpdate></soap:Body></soap:Envelope>'''
addMember='''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario></PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><AddMember xmlns="http://www.msn.com/webservices/AddressBook"><serviceHandle><Id>0</Id><Type>Messenger</Type><ForeignId></ForeignId></serviceHandle><memberships><Membership><MemberRole>%s</MemberRole><Members><Member xsi:type="PassportMember" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><Type>Passport</Type><State>Accepted</State><PassportName>%s</PassportName></Member></Members></Membership></memberships></AddMember></soap:Body></soap:Envelope>'''
contactAdd = '''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario>ContactSave</PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><ABContactAdd xmlns="http://www.msn.com/webservices/AddressBook"><abId>00000000-0000-0000-0000-000000000000</abId><contacts><Contact xmlns="http://www.msn.com/webservices/AddressBook"><contactInfo><passportName>%s</passportName><isMessengerUser>true</isMessengerUser></contactInfo></Contact></contacts><options><EnableAllowListManagement>true</EnableAllowListManagement></options></ABContactAdd></soap:Body></soap:Envelope>'''
contactRemove = '''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario>Timer</PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><ABContactDelete xmlns="http://www.msn.com/webservices/AddressBook"><abId>00000000-0000-0000-0000-000000000000</abId><contacts><Contact><contactId>%s</contactId></Contact></contacts></ABContactDelete></soap:Body></soap:Envelope>'''
deleteMember='''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario></PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><DeleteMember xmlns="http://www.msn.com/webservices/AddressBook"><serviceHandle><Id>0</Id><Type>Messenger</Type><ForeignId></ForeignId></serviceHandle><memberships><Membership><MemberRole>%s</MemberRole><Members><Member xsi:type="PassportMember" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><Type>Passport</Type><State>Accepted</State><PassportName>%s</PassportName></Member></Members></Membership></memberships></DeleteMember></soap:Body></soap:Envelope>'''
addGroup='''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario>Timer</PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><ABGroupAdd xmlns="http://www.msn.com/webservices/AddressBook"><abId>00000000-0000-0000-0000-000000000000</abId><groupAddOptions><fRenameOnMsgrConflict>false</fRenameOnMsgrConflict></groupAddOptions><groupInfo><GroupInfo><name>%s</name><groupType>C8529CE2-6EAD-434d-881F-341E17DB3FF8</groupType><fMessenger>false</fMessenger><annotations><Annotation><Name>MSN.IM.Display</Name><Value>1</Value></Annotation></annotations></GroupInfo></groupInfo></ABGroupAdd></soap:Body></soap:Envelope>'''
deleteGroup='''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario>Timer</PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><ABGroupDelete xmlns="http://www.msn.com/webservices/AddressBook"><abId>00000000-0000-0000-0000-000000000000</abId><groupFilter><groupIds><guid>%s</guid></groupIds></groupFilter></ABGroupDelete></soap:Body></soap:Envelope>'''
renameGroup='''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/">
<soap:Header><ABApplicationHeader xmlns="http://www.msn.com/webservices/AddressBook"><ApplicationId>CFE80F9D-180F-4399-82AB-413F33A1FA11</ApplicationId><IsMigration>false</IsMigration><PartnerScenario>Timer</PartnerScenario></ABApplicationHeader>
<ABAuthHeader xmlns="http://www.msn.com/webservices/AddressBook"><ManagedGroupRequest>false</ManagedGroupRequest><TicketToken>&tickettoken;</TicketToken></ABAuthHeader></soap:Header>
<soap:Body><ABGroupUpdate xmlns="http://www.msn.com/webservices/AddressBook"><abId>00000000-0000-0000-0000-000000000000</abId><groups><Group><groupId>%s</groupId><groupInfo><name>%s</name></groupInfo><propertiesChanged>GroupName </propertiesChanged></Group></groups></ABGroupUpdate></soap:Body></soap:Envelope>'''
send_message = '''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"> <soap:Header><From memberName="$memberName" friendlyName="$friendlyName" xml:lang="en-US" proxy="MSNMSGR" xmlns="http://messenger.msn.com/ws/2004/09/oim/" msnpVer="$ver" buildVer="$buildVer"/><To memberName="$to" xmlns="http://messenger.msn.com/ws/2004/09/oim/"/><Ticket passport="$passport" appid="$appid" lockkey="$lockKey" xmlns="http://messenger.msn.com/ws/2004/09/oim/"/><Sequence xmlns="http://schemas.xmlsoap.org/ws/2003/03/rm"><Identifier xmlns="http://schemas.xmlsoap.org/ws/2002/07/utility">http://messenger.msn.com</Identifier><MessageNumber>$seqNum</MessageNumber></Sequence></soap:Header><soap:Body><MessageType xmlns="http://messenger.msn.com/ws/2004/09/oim/">text</MessageType><Content xmlns="http://messenger.msn.com/ws/2004/09/oim/">MIME-Version: 1.0\r\nContent-Type: text/plain; charset=UTF-8\r\nContent-Transfer-Encoding: base64\r\nX-OIM-Message-Type: OfflineMessage\r\nX-OIM-Run-Id: {$runId}\r\nX-OIM-Sequence-Num: $seqNum\r\n\r\n$content</Content></soap:Body></soap:Envelope>'''
retreive_message = '''<?xml version="1.0" encoding="utf-8"?> <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
<soap:Header>
<PassportCookie xmlns="http://www.hotmail.msn.com/ws/2004/09/oim/rsi"> <t>$t</t><p>$p</p> </PassportCookie>
</soap:Header>
<soap:Body>
<GetMessage xmlns="http://www.hotmail.msn.com/ws/2004/09/oim/rsi"> <messageId>$mid</messageId> <alsoMarkAsRead>false</alsoMarkAsRead> </GetMessage>
</soap:Body>
</soap:Envelope>'''
delete_message = '''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"><soap:Header><PassportCookie xmlns="http://www.hotmail.msn.com/ws/2004/09/oim/rsi"><t>$t</t><p>$p</p></PassportCookie></soap:Header><soap:Body><DeleteMessages xmlns="http://www.hotmail.msn.com/ws/2004/09/oim/rsi"><messageIds><messageId>$mid</messageId></messageIds></DeleteMessages></soap:Body></soap:Envelope>'''
space='''<?xml version="1.0" ?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<soap:Header>
		<AuthTokenHeader xmlns="http://www.msn.com/webservices/spaces/v1/">
			<Token>
				t=%s&amp;p=%s
			</Token>
		</AuthTokenHeader>
	</soap:Header>
	<soap:Body>
		<GetXmlFeed xmlns="http://www.msn.com/webservices/spaces/v1/">
			<refreshInformation>
				<cid xmlns="http://www.msn.com/webservices/spaces/v1/">
					%s
				</cid>
				<storageAuthCache>
					%s
				</storageAuthCache>
				<market xmlns="http://www.msn.com/webservices/spaces/v1/">
					%s
				</market>
				<brand/>
				<maxElementCount xmlns="http://www.msn.com/webservices/spaces/v1/">
					2
				</maxElementCount>
				<maxCharacterCount xmlns="http://www.msn.com/webservices/spaces/v1/">
					200
				</maxCharacterCount>
				<maxImageCount xmlns="http://www.msn.com/webservices/spaces/v1/">
					6
				</maxImageCount>
				<applicationId>
					Messenger Client 8.0
				</applicationId>
				<updateAccessedTime>
					true
				</updateAccessedTime>
				<spaceLastViewed>
					1753-01-01T00:00:00.0000000-00:00
				</spaceLastViewed>
				<profileLastViewed>
					1753-01-01T00:00:00.0000000-00:00
				</profileLastViewed>
				<contactProfileLastViewed>
					1753-01-01T00:00:00.0000000-00:00
				</contactProfileLastViewed>
				<isActiveContact>
					false
				</isActiveContact>
			</refreshInformation>
		</GetXmlFeed>
	</soap:Body>
</soap:Envelope>'''
schematizedStore = '''<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"><soap:Header><StorageApplicationHeader xmlns="http://www.msn.com/webservices/storage/w10"><ApplicationID>Messenger Client 7.0</ApplicationID></StorageApplicationHeader><StorageUserHeader xmlns="http://www.msn.com/webservices/storage/w10"><Puid>0</Puid><UserAuthCache></UserAuthCache><IPAddress/></StorageUserHeader></soap:Header><soap:Body><GetItemVersion xmlns="http://www.msn.com/webservices/storage/w10"><spaceVersionRequests>
<SpaceVersionRequest><SpaceHandle><RelationshipName>MySpace</RelationshipName><Alias><NameSpace>MyStuff</NameSpace><Name>%s</Name></Alias></SpaceHandle><LastModifiedDate>2004-01-01T00:00:00.0000000-08:00</LastModifiedDate></SpaceVersionRequest></spaceVersionRequests><spaceRequestFilter><SpaceFilterAttributes>Annotation</SpaceFilterAttributes><FilterValue>1</FilterValue></spaceRequestFilter></GetItemVersion></soap:Body></soap:Envelope>'''
getMailData = '''<?xml version="1.0" encoding="utf-8"?> 
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Header>
    <PassportCookie xmlns="http://www.hotmail.msn.com/ws/2004/09/oim/rsi">
      <t>%s</t> 
      <p>%s</p>
    </PassportCookie>
  </soap:Header>
  <soap:Body>
    <GetMetadata xmlns="http://www.hotmail.msn.com/ws/2004/09/oim/rsi" />
  </soap:Body>
</soap:Envelope>'''
