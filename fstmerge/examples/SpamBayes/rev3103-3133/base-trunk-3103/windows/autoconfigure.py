"""Automatically set up the user's mail client and SpamBayes.
Example usage:
    >>> configure("mailer name")
Where "mailer name" is any of the names below.
Currently works with:
 o Eudora (POP3/SMTP only)
 o Mozilla Mail (POP3/SMTP only)
 o M2 (Opera Mail) (POP3/SMTP only)
 o Outlook Express (POP3/SMTP only)
 o PocoMail (POP3/SMTP only)
To do:
 o Establish which mail client(s) are installed in a more clever way.
 o This will create some unnecessary proxies in some cases.  For example,
   if I have my client set up to get mail from pop.example.com for the
   user 'tmeyer' and the user 'tonym', two proxies will be created, but
   only one is necessary.  We should check the existing proxies before
   adding a new one.
 o Other mail clients?  Other platforms?
 o This won't work all that well if multiple mail clients are used (they
   will end up trying to use the same ports).  In such a case, we really
   need to keep track of if the server is being proxied already, and
   reuse ports, but this is complicated.
 o We currently don't make any moves to protect the original file, so if
   something does wrong, it's corrupted.  We also write into the file,
   rather than a temporary one and then copy across.  This should all be
   fixed.  Richie's suggestion is for the script to create a clone of an
   existing account with the new settings.  Then people could test the
   cloned account, and if they're happy with it they can either delete
   their old account or delete the new one and run the script again in
   "modify" rather than "clone" mode.  This sounds like a good idea,
   although a lot of work...
 o Suggestions?
"""
__author__ = "Tony Meyer <ta-meyer@ihug.co.nz>"
__credits__ = "All the Spambayes folk."
try:
    True, False
except NameError:
    True, False = 1, 0
import re
import os
import sys
import types
import socket
import shutil
import StringIO
import ConfigParser
try:
    import win32gui
    import win32api
    import win32con
    import pywintypes
    from win32com.shell import shell, shellcon
except ImportError:
    win32api = win32con = shell = shellcon = win32gui = pywintypes = None
from spambayes import oe_mailbox
from spambayes import OptionsClass
from spambayes.Options import options, optionsPathname
def move_to_next_free_port(port):
    while True:
        try:
            port += 1
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.connect(("127.0.0.1", port))
            s.close()
        except socket.error:
            portStr = str(port)
            if portStr in options["pop3proxy", "listen_ports"] or \
               portStr in options["smtpproxy", "listen_ports"]:
                continue
            else:
                return port
pop_proxy_port = move_to_next_free_port(1109)
smtp_proxy_port = move_to_next_free_port(1024)
def configure_eudora(config_location):
    """Configure Eudora to use the SpamBayes POP3 and SMTP proxies, and
    configure SpamBayes to proxy the servers that Eudora was connecting to.
    """
    ini_filename = "%s%seudora.ini" % (config_location, os.sep)
    c = ConfigParser.ConfigParser()
    c.read(ini_filename)
    translate = {("PopServer", "POPPort") : "pop3proxy",
                 ("SMTPServer", "SMTPPort") : "smtpproxy",
                 }
    pop_proxy = pop_proxy_port
    smtp_proxy = smtp_proxy_port
    results = []
    for sect in c.sections():
        if sect.startswith("Persona-") or sect == "Settings":
            if c.get(sect, "UsesIMAP") == "0":
                p = c.get(sect, "popaccount")
                c.set(sect, "popaccount", "%s@localhost" % \
                      (p[:p.index('@')],))
                for (eud_name, eud_port), us_name in translate.items():
                    try:
                        port = c.get(sect, eud_port)
                    except ConfigParser.NoOptionError:
                        port = None
                    if us_name.lower()[:4] == "pop3":
                        if port is None:
                            port = 110
                        pop_proxy = move_to_next_free_port(pop_proxy)
                        proxy_port = pop_proxy
                    else:
                        if port is None:
                            port = 25
                        smtp_proxy = move_to_next_free_port(smtp_proxy)
                        proxy_port = smtp_proxy
                    server = "%s:%s" % (c.get(sect, eud_name), port)
                    options[us_name, "remote_servers"] += (server,)
                    options[us_name, "listen_ports"] += (proxy_port,)
                    results.append("[%s] Proxy %s on localhost:%s" % \
                                   (sect, server, proxy_port))
                    c.set(sect, eud_name, "localhost")
                    c.set(sect, eud_port, proxy_port)
            else:
                pass
    out = file(ini_filename, "w")
    c.write(out)
    out.close()
    options.update_file(optionsPathname)
    filter_filename = "%s%sFilters.pce" % (config_location, os.sep)
    spam_folder_name = "Junk"
    unsure_folder_name = "Possible Junk"
    header_name = options["Headers", "classification_header_name"]
    spam_tag = options["Headers", "header_spam_string"]
    unsure_tag = options["Headers", "header_unsure_string"]
    filter_rules = "rule SpamBayes-Spam\n" \
                   "transfer %s.mbx\n" \
                   "incoming\n" \
                   "header %s\n" \
                   "verb contains\n" \
                   "value %s\n" \
                   "conjunction ignore\n" \
                   "header \n" \
                   "verb contains\n" \
                   "value \n" \
                   "rule SpamBayes-Unsure\n" \
                   "transfer %s.mbx\n" \
                   "incoming\n" \
                   "header %s\n" \
                   "verb contains\n" \
                   "value %s\n" \
                   "conjunction ignore\n" \
                   "header \n" \
                   "verb contains\n" \
                   "value \n" % (spam_folder_name, header_name, spam_tag,
                                 unsure_folder_name, header_name, unsure_tag)
    filter_file = file(filter_filename, "a")
    filter_file.write(filter_rules)
    filter_file.close()
    return results
def configure_mozilla(config_location):
    """Configure Mozilla to use the SpamBayes POP3 and SMTP proxies, and
    configure SpamBayes to proxy the servers that Mozilla was connecting
    to."""
    prefs_file = file("%s%sprefs.js" % (config_location, os.sep), "r")
    prefs = prefs_file.read()
    prefs_file.close()
    save_prefs = prefs
    pop_accounts = {}
    smtp_accounts = {}
    r = re.compile(r"user_pref\(\"mail.server.server(\d+).(real)?hostname\", \"([^\"]*)\"\);")
    current_pos = 0
    results = []
    while True:
        m = r.search(prefs[current_pos:])
        if not m:
            break
        server_num = m.group(1)
        real = m.group(2) or ''
        server = m.group(3)
        current_pos += m.end()
        old_pref = 'user_pref("mail.server.server%s.%shostname", "%s");' % \
                   (server_num, real, server)
        port_string = 'user_pref("mail.server.server%s.port", ' % \
                      (server_num,)
        port_loc = prefs.find(port_string)
        if port_loc == -1:
            port = "110"
            old_port = None
        else:
            loc_plus_len = port_loc + len(port_string)
            end_of_number = loc_plus_len + prefs[loc_plus_len:].index(')')
            port = prefs[loc_plus_len : end_of_number]
            old_port = "%s%s);" % (port_string, port)
        type_string = 'user_pref("mail.server.server%s.type", "' % \
                      (server_num,)
        type_loc = prefs.find(type_string)
        if type_loc == -1:
            continue
        type_loc += len(type_string)
        account_type = prefs[type_loc : \
                             type_loc + prefs[type_loc:].index('"')]
        if account_type == "pop3":
            new_pref = 'user_pref("mail.server.server%s.%shostname", ' \
                       '"127.0.0.1");' % (server_num, real)
            if not pop_accounts.has_key(server_num) or real:
                pop_accounts[server_num] = (new_pref, old_pref,
                                            old_port, server, port)
        elif account_type == "imap":
            pass
    proxy_port = pop_proxy_port
    for num, (pref, old_pref, old_port, server, port) in pop_accounts.items():
        server = "%s:%s" % (server, port)
        proxy_port = move_to_next_free_port(proxy_port)
        port_pref = 'user_pref("mail.server.server%s.port", %s);' % \
                    (num, proxy_port)
        options["pop3proxy", "remote_servers"] += (server,)
        options["pop3proxy", "listen_ports"] += (proxy_port,)
        if old_port is None:
            pref = "%s\n%s" % (pref, port_pref)
        else:
            save_prefs = save_prefs.replace(old_port, port_pref)
        save_prefs = save_prefs.replace(old_pref, pref)
        results.append("[%s] Proxy %s on localhost:%s" % \
                       (num, server, proxy_port))
    prefs = save_prefs
    r = re.compile(r"user_pref\(\"mail.smtpserver.smtp(\d+).hostname\", \"([^\"]*)\"\);")
    current_pos = 0
    while True:
        m = r.search(prefs[current_pos:])
        if not m:
            break
        current_pos += m.end()
        server_num = m.group(1)
        server = m.group(2)
        old_pref = 'user_pref("mail.smtpserver.smtp%s.hostname", ' \
                   '"%s");' % (server_num, server)
        new_pref = 'user_pref("mail.smtpserver.smtp%s.hostname", ' \
                   '"127.0.0.1");' % (server_num,)
        port_string = 'user_pref("mail.smtpserver.smtp%d.port", ' \
                      % (server_num,)
        port_loc = prefs.find(port_string)
        if port_loc == -1:
            port = "25"
            old_port = None
        else:
            loc_plus_len = port_loc + len(port_string)
            end_of_number = loc_plus_len + prefs[loc_plus_len:].index(')')
            port = prefs[loc_plus_len : end_of_number]
            old_port = 'user_pref("mail.smtpserver.smtp%s.port", %s);' % \
                       (server_num, port)
        smtp_accounts[server_num] = (new_pref, old_pref, old_port,
                                     server, port)
    proxy_port = smtp_proxy_port
    for num, (pref, old_pref, old_port, server, port) in smtp_accounts.items():
        server = "%s:%s" % (server, port)
        proxy_port = move_to_next_free_port(proxy_port)
        port_pref = 'user_pref("mail.smtpserver.smtp%s.port", %s);' % \
                    (num, proxy_port)
        options["smtpproxy", "remote_servers"] += (server,)
        options["smtpproxy", "listen_ports"] += (proxy_port,)
        if old_port is None:
            pref = "%s\n%s" % (pref, port_pref)
        else:
            save_prefs = save_prefs.replace(old_port, port_pref)
        save_prefs = save_prefs.replace(old_pref, pref)
        results.append("[%s] Proxy %s on localhost:%s" % \
                       (num, server, proxy_port))
    prefs_file = file("%s%sprefs.js" % (config_location, os.sep), "w")
    prefs_file.write(save_prefs)
    prefs_file.close()
    options.update_file(optionsPathname)
    filter_filename = "%s%smsgFilterRules.dat" % (config_location, os.sep)
    store_name = "" # how do we get this?
    spam_folder_url = "mailbox:////%s//Junk%%20Mail" % (store_name,)
    unsure_folder_url = "mailbox:////%s//Possible%%20Junk" % (store_name,)
    header_name = options["Headers", "classification_header_name"]
    spam_tag = options["Headers", "header_spam_string"]
    unsure_tag = options["Headers", "header_unsure_string"]
    rule = 'name="SpamBayes-Spam"\n' \
           'enabled="yes"\n' \
           'type="1"\n' \
           'action="Move to folder"\n' \
           'actionValue="%s"\n' \
           'condition="OR (\"%s\",contains,%s)"\n' \
           'name="SpamBayes-Unsure"\n' \
           'enabled="yes"\n' \
           'type="1"\n' \
           'action="Move to folder"\n' \
           'actionValue="%s"\n' \
           'condition="OR (\"%s\",contains,%s)"\n' % \
           (spam_folder_url, header_name, spam_tag,
            unsure_folder_url, header_name, unsure_tag)
    return results
def configure_m2(config_location):
    """Configure M2 (Opera's mailer) to use the SpamBayes POP3 and SMTP
    proxies, and configure SpamBayes to proxy the servers that M2 was
    connecting to."""
    ini_filename = os.path.join(config_location, "Mail", "accounts.ini")
    ini_file = file(ini_filename, "r")
    faked_up = StringIO.StringIO()
    faked_up.write(";") # Missing at the start
    faked_up.write(ini_file.read())
    faked_up.seek(0)
    ini_file.close()
    c = ConfigParser.ConfigParser()
    c.readfp(faked_up)
    translate = {("Incoming Servername", "Incoming Port") : "pop3proxy",
                 ("Outgoing Servername", "Outgoing Port") : "smtpproxy",
                 }
    pop_proxy = pop_proxy_port
    smtp_proxy = smtp_proxy_port
    results = []
    for sect in c.sections():
        if sect.startswith("Account") and sect != "Accounts":
            if c.get(sect, "Incoming Protocol") == "POP":
                for (m2_name, m2_port), us_name in translate.items():
                    try:
                        port = c.get(sect, m2_port)
                    except ConfigParser.NoOptionError:
                        port = None
                    if us_name.lower()[:4] == "pop3":
                        if port is None:
                            port = 110
                        pop_proxy = move_to_next_free_port(pop_proxy)
                        proxy_port = pop_proxy
                    else:
                        if port is None:
                            port = 25
                        smtp_proxy = move_to_next_free_port(smtp_proxy)
                        proxy_port = smtp_proxy
                    server = "%s:%s" % (c.get(sect, m2_name), port)
                    options[us_name, "remote_servers"] += (server,)
                    options[us_name, "listen_ports"] += (proxy_port,)
                    results.append("[%s] Proxy %s on localhost:%s" % \
                                   (sect, server, proxy_port))
                    c.set(sect, m2_name, "localhost")
                    c.set(sect, m2_port, proxy_port)
            elif c.get(sect, "Incoming Protocol") == "IMAP":
                pass
    out = file(ini_filename, "w")
    c.write(out)
    out.close()
    options.update_file(optionsPathname)
    return results
def configure_outlook_express(unused):
    """Configure OE to use the SpamBayes POP3 and SMTP proxies, and
    configure SpamBayes to proxy the servers that OE was connecting to."""
    if win32api is None:
        raise ImportError("win32 extensions required")
    accounts = oe_mailbox.OEAccountKeys()
    translate = {("POP3 Server", "POP3 Port") : "pop3proxy",
                 ("SMTP Server", "SMTP Port") : "smtpproxy",
                 }
    pop_proxy = pop_proxy_port
    smtp_proxy = smtp_proxy_port
    results = []
    for proto, subkey, account in accounts:
        if proto == "POP3":
            for (server_key, port_key), sect in translate.items():
                if sect[:4] == "pop3":
                    default_port = 110
                    pop_proxy = move_to_next_free_port(pop_proxy)
                    proxy = pop_proxy
                else:
                    default_port = 25
                    smtp_proxy = move_to_next_free_port(smtp_proxy)
                    proxy = smtp_proxy
                if account.has_key(port_key):
                    port = account[port_key][0]
                else:
                    port = default_port
                server = "%s:%s" % (account[server_key][0], port)
                options[sect, "remote_servers"] += (server,)
                options[sect, "listen_ports"] += (proxy,)
                win32api.RegSetValueEx(subkey, server_key, 0,
                                       win32con.REG_SZ, "127.0.0.1")
                win32api.RegSetValueEx(subkey, port_key, 0,
                                       win32con.REG_SZ, str(proxy))
                results.append("[%s] Proxy %s on localhost:%s" % \
                               (account["Account Name"][0], server, proxy))
        elif proto == "IMAP4":
            pass
    options.update_file(optionsPathname)
    return results
def configure_pegasus_mail(config_location):
    """Configure Pegasus Mail to use the SpamBayes POP3 and SMTP proxies,
    and configure SpamBayes to proxy the servers that Pegasus Mail was
    connecting to."""
    pop_proxy = pop_proxy_port
    smtp_proxy = smtp_proxy_port
    results = []
    for filename in os.listdir(config_location):
        if filename.lower().startswith("pop") or filename.lower().startswith("smt"):
            full_filename = os.path.join(config_location, filename)
            working_filename = "%s.tmp" % (filename, )
            shutil.copyfile(filename, working_filename)
            c = OptionsClass.OptionsClass()
            c.merge_file(working_filename)
            server = "%s:%s" % (c.get("all", "host"), c.get("all", "port"))
            if filename[:3] == "pop":
                pop_proxy = move_to_next_free_port(pop_proxy)
                proxy = pop_proxy
                sect = "pop3proxy"
            else:
                smtp_proxy = move_to_next_free_port(smtp_proxy)
                proxy = smtp_proxy
                sect = "smtpproxy"
            options[sect, "remote_servers"] += (server,)
            options[sect, "listen_ports"] += (proxy,)
            c.set("all", "host", "127.0.0.1")
            c.set("all", "port", proxy)
            c.update_file(working_filename)
            results.append("[%s] Proxy %s on localhost:%s" % \
                           (c.get("all", "title"), server, proxy))
        elif filename.lower() == "IMAP.PM":
            pass
    rules_filename = os.path.join(config_location, "spambust.dat")
    header_name = options["Headers", "classification_header_name"]
    spam_tag = options["Headers", "header_spam_string"]
    unsure_tag = options["Headers", "header_unsure_string"]
    ham_tag = options["Headers", "header_ham_string"]
    spam_weight = 500
    ham_weight = -500
    unsure_weight = -50 # leave judgement up to the rest of the rules
    rule = '# SpamBayes adjustments\n' \
           'if header "%s" contains "%s" weight %s\n' \
           'if header "%s" contains "%s" weight %s\n' \
           'if header "%s" contains "%s" wieght %s\n\n' % \
           (header_name, spam_tag, spam_weight,
            header_name, unsure_tag, unsure_weight,
            header_name, ham_tag, ham_weight)
    rules_file = file(rules_filename, "a")
    rules_file.write(rule)
    rules_file.close()
    return results
def pocomail_accounts_filename():
    if win32api is None:
        return ""
    key = "Software\\Poco Systems Inc"
    pop_proxy  = pop_proxy_port
    smtp_proxy = smtp_proxy_port
    try:
        reg = win32api.RegOpenKeyEx(win32con.HKEY_CURRENT_USER, key)
    except pywintypes.error:
        return ""
    else:
        subkey_name   = "%s\\%s" % (key, win32api.RegEnumKey(reg, 0))
        reg           = win32api.RegOpenKeyEx(win32con.HKEY_CURRENT_USER,
                                              subkey_name)
        pocomail_path = win32api.RegQueryValueEx(reg, "Path")[0]
    return os.path.join(pocomail_path, "accounts.ini")
def configure_pocomail(pocomail_accounts_file):
    if os.path.exists(pocomail_accounts_file):
        f = open(pocomail_accounts_file, "r")
        accountName       = ""
        pocomail_accounts = { }
        for line in f.readlines():
            line = line.rstrip('\n\r')
            if line == '':
                continue
            if line[0] == '[' and line[-1] == ']':
                accountName = line[1:-1]
                pocomail_accounts[accountName] = { }
            else:
                separator   = line.find('=')
                optionName  = line[:separator]
                optionValue = line[separator + 1:]
                if optionName == "POPServer":
                    pop3 = optionValue.split(':')
                    if len(pop3) == 1:
                        pop3.append(110)
                    server = "%s:%s" % tuple(pop3)
                    proxy     = pop_proxy
                    pop_proxy = move_to_next_free_port(pop_proxy)
                    if not server in options["pop3proxy", "remote_servers"]:
                        options["pop3proxy", "remote_servers"] += (server,)
                        options["pop3proxy", "listen_ports"]   += (proxy,)
                    else:
                        serverIndex = 0
                        for remoteServer in options["pop3proxy",
                                                    "remote_servers"]:
                            if remoteServer == server:
                                break
                            serverIndex += 1
                        proxy = options["pop3proxy", "listen_ports"][serverIndex]
                    optionValue = "%s:%s" % ('localhost', proxy)
                pocomail_accounts[accountName][optionName] = optionValue
        f.close()
        f = open(pocomail_accounts_file, "w")
        for accountName in pocomail_accounts.keys():
            f.write('[' + accountName + ']\n')
            for optionName, optionValue in pocomail_accounts[accountName].items():
                f.write("%s=%s\n" % (optionName, optionValue))
            f.write('\n')
        f.close()
        options.update_file(optionsPathname)
        pocomail_filters_file = os.path.join(pocomail_path, "filters.ini")
        if os.path.exists(pocomail_filters_file):
            f = open(pocomail_filters_file, "r")
            pocomail_filters = { }
            filterName       = ""
            for line in f.readlines():
                line = line.rstrip('\n\r')
                if line == '': continue
                if line[0] == '[' and line[-1] == ']':
                    filterName = line[1:-1]
                    pocomail_filters[filterName] = []
                elif line[0] != '{':
                    pocomail_filters[filterName].append(line)
            f.close()
            spamBayesFilter = 'spam,X-Spambayes-Classification,move,' \
                              '"Junk Mail",0,0,,,0,,,move,In,0,0,,0,,,' \
                              'move,In,0,0,,0,,,move,In,0,0,,0,,,move,' \
                              'In,0,0,,0,,,move,In,0,0,1,0'
            if pocomail_filters.has_key("Incoming") and \
               spamBayesFilter not in pocomail_filters["Incoming"]:
                pocomail_filters["Incoming"].append(spamBayesFilter)
            f = open(pocomail_filters_file, "w")
            f.write('{ Filter list generated by PocoMail 3.01 (1661)' \
                    '- Licensed Version}\n')
            for filterName in pocomail_filters.keys():
                f.write('\n[' + filterName + ']\n')
                for filter in pocomail_filters[filterName]:
                    f.write(filter + '\n')
            f.close()
    return []
def find_config_location(mailer):
    """Attempt to find the location of the config file for
    the given mailer, to pass to the configure_* scripts
    above."""
    if win32api is None:
        raise ImportError("win32 extensions required")
    if mailer in ["Outlook Express", ]:
        return ""
    windowsUserDirectory = shell.SHGetFolderPath(0,shellcon.CSIDL_APPDATA,0,0)
    potential_locations = \
                        {"Eudora" : ("%(wud)s%(sep)sQualcomm%(sep)sEudora",),
                         "Mozilla" : \
                         ("%(wud)s%(sep)sMozilla%(sep)sProfiles%(sep)s%(user)s",
                          "%(wud)s%(sep)sMozilla%(sep)sProfiles%(sep)sdefault",),
                         "M2" : ("%(wud)s%(sep)sOpera%(sep)sOpera7",),
                         "PocoMail" : (pocomail_accounts_filename(),),
                         }
    username = win32api.GetUserName()
    loc_dict = {"sep" : os.sep,
                "wud" : windowsUserDirectory,
                "user" : username}
    for loc in potential_locations[mailer]:
        loc = loc % loc_dict
        if os.path.exists(loc):
            return loc
    return None
def configure(mailer):
    """Automatically configure the specified mailer and SpamBayes."""
    loc = find_config_location(mailer)
    if loc is None:
        return
    funcs = {"Eudora" : configure_eudora,
             "Mozilla" : configure_mozilla,
             "M2" : configure_m2,
             "Outlook Express" : configure_outlook_express,
             "PocoMail" : configure_pocomail,
             }
    return funcs[mailer](loc)
def is_installed(mailer):
    """Return True if we believe that the mailer is installed."""
    config_location = find_config_location(mailer)
    if config_location:
        if os.path.exists(config_location):
            return True
        return False
    if mailer == "Outlook Express":
        if oe_mailbox.OEIsInstalled():
            return True
        return False
    return False
def offer_to_configure(mailer):
    """If the mailer appears to be installed, offer to set it up for
    SpamBayes (and SpamBayes for it)."""
    if find_config_location(mailer) is not None:
        confirm_text = "Would you like %s setup for SpamBayes, and " \
                       "SpamBayes setup with your %s settings?\n" \
                       "(This is alpha software! We recommend that you " \
                       "only do this if you know how to re-setup %s " \
                       "if necessary.)" % (mailer, mailer, mailer)
        ans = MessageBox(confirm_text, "Configure?",
                         win32con.MB_YESNO | win32con.MB_ICONQUESTION)
        if ans == win32con.IDYES:
            results = configure(mailer)
            if results is None:
                MessageBox("Configuration unsuccessful.", "Error",
                           win32con.MB_OK | win32con.MB_ICONERROR)
            else:
                text = "Configuration complete.\n\n" + "\n".join(results)
                MessageBox(text, "Complete", win32con.MB_OK)
def GetConsoleHwnd():
    """Returns the window handle of the console window in which this script is
    running, or 0 if not running in a console window.  This function is taken
    directly from Pythonwin\dllmain.cpp in the win32all source, ported to
    Python."""
    try:
        oldWindowTitle = win32api.GetConsoleTitle()
    except:
        return 0
    newWindowTitle = "%d/%d" % (win32api.GetTickCount(),
                                win32api.GetCurrentProcessId())
    win32api.SetConsoleTitle(newWindowTitle)
    import time
    time.sleep(0.040)
    hwndFound = win32gui.FindWindow(0, newWindowTitle)
    win32api.SetConsoleTitle(oldWindowTitle)
    return hwndFound
hwndOwner = GetConsoleHwnd()
def MessageBox(message, title=None, style=win32con.MB_OK):
    return win32gui.MessageBox(hwndOwner, message, title, style)
if __name__ == "__main__":
    pmail_ini_dir = "C:\\Program Files\\PMAIL\\MAIL\\ADMIN"
    for mailer in ["Eudora", "Mozilla", "M2", "Outlook Express", "PocoMail"]:
        offer_to_configure(mailer)
