from win32com.client import Dispatch, constants
import pythoncom
import os, sys
from win32com.mapi import mapi
from win32com.mapi.mapitags import *
import mapi_driver
def DeleteField_Outlook(folder, name):
    name = name.lower()
    entries = folder.Items
    num_outlook = 0
    entry = entries.GetFirst()
    while entry is not None:
        up = entry.UserProperties
        num_props = up.Count
        for i in range(num_props):
            if up[i+1].Name.lower()==name:
                num_outlook += 1
                entry.UserProperties.Remove(i+1)
                entry.Save()
                break
        entry = entries.GetNext()
    return num_outlook
def DeleteField_MAPI(driver, folder, name):
    propIds = folder.GetIDsFromNames(((mapi.PS_PUBLIC_STRINGS,name),), 0)
    if PROP_TYPE(propIds[0])==PT_ERROR:
        print("No such field '%s' in folder" % (name,))
        return 0
    assert propIds[0] == PROP_TAG( PT_UNSPECIFIED, PROP_ID(propIds[0]))
    num_mapi = 0
    for item in driver.GetAllItems(folder):
        hr, vals = item.GetProps(propIds)
        if hr==0: 
            hr, probs = item.DeleteProps(propIds)
            if  hr == 0:
                item.SaveChanges(mapi.MAPI_DEFERRED_ERRORS)
                num_mapi += 1
    return num_mapi
def DeleteField_Folder(driver, folder, name):
    propIds = folder.GetIDsFromNames(((mapi.PS_PUBLIC_STRINGS,name),), 0)
    if PROP_TYPE(propIds[0])!=PT_ERROR:
        hr, vals = folder.GetProps(propIds)
        if hr==0: 
            hr, probs = folder.DeleteProps(propIds)
            if  hr == 0:
                folder.SaveChanges(mapi.MAPI_DEFERRED_ERRORS)
                return 1
    return 0
def CountFields(folder):
    fields = {}
    entries = folder.Items
    entry = entries.GetFirst()
    while entry is not None:
        ups = entry.UserProperties
        num_props = ups.Count
        for i in range(num_props):
            name = ups.Item(i+1).Name
            fields[name] = fields.get(name, 0)+1
        entry = entries.GetNext()
    for name, num in list(fields.items()):
        print(name, num)
def ShowFields(folder, field_name):
    field_name = field_name.lower()
    entries = folder.Items
    entry = entries.GetFirst()
    while entry is not None:
        ups = entry.UserProperties
        num_props = ups.Count
        for i in range(num_props):
            up = ups[i+1]
            name = up.Name
            if name.lower()==field_name:
                subject = entry.Subject.encode("mbcs", "replace")
                print("%s: %s (%d)" % (subject, up.Value, up.Type))
        entry = entries.GetNext()
def usage(driver):
    folder_doc = driver.GetFolderNameDoc()
    msg = """\
Usage: %s [-f foldername -f ...] [-d] [-s] [FieldName ...]
-f - Run over the specified folders (default = Inbox)
-d - Delete the named fields
  --no-outlook - Don't delete via the Outlook UserProperties API
  --no-mapi - Don't delete via the extended MAPI API
  --no-folder - Don't attempt to delete the field from the folder itself
-s - Show message subject and field value for all messages with field
-n - Show top-level folder names and exit
If no options are given, prints a summary of field names in the folders.
%s
Use the -n option to see all top-level folder names from all stores.""" \
        % (os.path.basename(sys.argv[0]), folder_doc)
    print(msg)
def main():
    driver = mapi_driver.MAPIDriver()
    import getopt
    try:
        opts, args = getopt.getopt(sys.argv[1:],
                                   "dnsf:",
                                   ["no-mapi", "no-outlook", "no-folder"])
    except getopt.error as e:
        print(e)
        print()
        usage(driver)
        sys.exit(1)
    delete = show = False
    do_mapi = do_outlook = do_folder = True
    folder_names = []
    for opt, opt_val in opts:
        if opt == "-d":
            delete = True
        elif opt == "-s":
            show = True
        elif opt == "-f":
            folder_names.append(opt_val)
        elif opt == "--no-mapi":
            do_mapi = False
        elif opt == "--no-outlook":
            do_outlook = False
        elif opt == "--no-folder":
            do_folder = False
        elif opt == "-n":
            driver.DumpTopLevelFolders()
            sys.exit(1)
        else:
            print("Invalid arg")
            return
    if not folder_names:
        folder_names = ["Inbox"] 
    if not args:
        print("No args specified - dumping all unique UserProperty names,")
        print("and the count of messages they appear in")
    outlook = None
    for folder_name in folder_names:
        try:
            folder = driver.FindFolder(folder_name)
        except ValueError as details:
            print(details)
            print("Ignoring folder '%s'" % (folder_name,))
            continue
        print("Processing folder '%s'" % (folder_name,))
        if not args:
            outlook_folder = driver.GetOutlookFolder(folder)
            CountFields(outlook_folder)
            continue
        for field_name in args:
            if show:
                outlook_folder = driver.GetOutlookFolder(folder)
                ShowFields(outlook_folder, field_name)
            if delete:
                print("Deleting field", field_name)
                if do_outlook:
                    outlook_folder = driver.GetOutlookFolder(folder)
                    num = DeleteField_Outlook(outlook_folder, field_name)
                    print("Deleted", num, "field instances from Outlook")
                if do_mapi:
                    num = DeleteField_MAPI(driver, folder, field_name)
                    print("Deleted", num, "field instances via MAPI")
                if do_folder:
                    num = DeleteField_Folder(driver, folder, field_name)
                    if num:
                        print("Deleted property from folder")
                    else:
                        print("Could not find property to delete in the folder")
if __name__=='__main__':
    main()
