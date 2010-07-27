import pythoncom

import os, sys

import tempfile

from win32com.mapi import mapi, mapiutil

from win32com.mapi.mapitags import *

import win32clipboard

import mapi_driver

try:

    TBL_ALL_COLUMNS = mapi.TBL_ALL_COLUMNS

except AttributeError: 

    TBL_ALL_COLUMNS = 1



PR_USERFIELDS = 0x36E30102

def GetPropTagName(obj, prop_tag):

    hr, tags, array = obj.GetNamesFromIDs( (prop_tag,) )

    if type(array[0][1])==type(''):

        name = array[0][1]

    else:

        name = mapiutil.GetPropTagName(prop_tag)

    return name
 def GetAllProperties(obj, make_pretty = True):

    tags = obj.GetPropList(0)

    hr, data = obj.GetProps(tags)

    ret = []

    for tag, val in data:

        if make_pretty:

            name = GetPropTagName(obj, tag)

        else:

            name = tag

        ret.append((name, tag, val))

    return ret
 def GetLargeProperty(item, prop_tag):

    prop_tag = PROP_TAG(PT_BINARY, PROP_ID(prop_tag))

    stream = item.OpenProperty(prop_tag,
                                pythoncom.IID_IStream,
                                0, 0)

    chunks = []

    while 1:

        chunk = stream.Read(4096)

        if not chunk:

            break

        chunks.append(chunk)

    return "".join(chunks)
 def FormatPropertyValue(prop_tag, prop_val, item, shorten, get_large_props):

    if PROP_ID(prop_tag) == PROP_ID(PR_RTF_COMPRESSED):

        rtf_stream = item.OpenProperty(PR_RTF_COMPRESSED,
                                       pythoncom.IID_IStream, 0, 0)

        html_stream = mapi.WrapCompressedRTFStream(rtf_stream, 0)

        prop_val = mapi.RTFStreamToHTML(html_stream)

        prop_tag = PROP_TAG(PT_STRING8, PR_RTF_COMPRESSED)

    prop_repr = None

    if PROP_TYPE(prop_tag)==PT_ERROR:

        if get_large_props and \
           prop_val in [mapi.MAPI_E_NOT_ENOUGH_MEMORY,
                        'MAPI_E_NOT_ENOUGH_MEMORY']:

            prop_val = GetLargeProperty(item, prop_tag)

            prop_repr = repr(prop_val)

        else:

            prop_val = prop_repr = mapiutil.GetScodeString(prop_val)

    if prop_repr is None:

        prop_repr = repr(prop_val)

    if shorten:

        prop_repr = prop_repr[:50]

    return prop_repr
 def DumpItemProps(item, shorten, get_large_props, stream=None):

    all_props = GetAllProperties(item)

    all_props.sort() 

    for prop_name, prop_tag, prop_val in all_props:

        if shorten and PROP_TYPE(prop_tag)==PT_ERROR \
           and prop_val == mapi.MAPI_E_NOT_FOUND:

            continue

        prop_repr = FormatPropertyValue(prop_tag, prop_val, item,
                                        shorten, get_large_props)

        print("%-20s: %s" % (prop_name, prop_repr), file=stream)

    print("-- end of item properties --", file=stream)
 def DumpProps(driver, mapi_folder, subject, include_attach, shorten,
              get_large, stream=None):

    hr, data = mapi_folder.GetProps( (PR_DISPLAY_NAME_A,), 0)

    name = data[0][1]

    for item in driver.GetItemsWithValue(mapi_folder, PR_SUBJECT_A, subject):

        DumpItemProps(item, shorten, get_large, stream)

        if include_attach:

            print(file=stream)

            table = item.GetAttachmentTable(0)

            rows = mapi.HrQueryAllRows(table, (PR_ATTACH_NUM,), None, None, 0)

            for row in rows:

                attach_num = row[0][1]

                print("Dumping attachment (PR_ATTACH_NUM=%d)" % (attach_num,), file=stream)

                attach = item.OpenAttach(attach_num, None,
                                         mapi.MAPI_DEFERRED_ERRORS)

                DumpItemProps(attach, shorten, get_large, stream)

            print(file=stream)

        print(file=stream)
 def DumpTable(driver, table, name_query_ob, shorten, large_props, stream=None):

    cols = table.QueryColumns(TBL_ALL_COLUMNS)

    table.SetColumns(cols, 0)

    rows = mapi.HrQueryAllRows(table, cols, None, None, 0)

    print("Table has %d rows, each with %d columns" % (len(rows), len(cols)), file=stream)

    for row in rows:

        print("-- new row --", file=stream)

        for col in row:

            prop_tag, prop_val = col

            if shorten and PROP_TYPE(prop_tag)==PT_ERROR \
               and prop_val == mapi.MAPI_E_NOT_FOUND:

                continue

            prop_name = GetPropTagName(name_query_ob, prop_tag)

            prop_repr = FormatPropertyValue(prop_tag, prop_val, name_query_ob,
                                            shorten, large_props)

            print("%-20s: %s" % (prop_name, prop_repr), file=stream)
 def FindAndDumpTableUserProps(driver, table, folder, shorten,
                              get_large_props, stream=None):

    restriction = (mapi.RES_PROPERTY,
                  (mapi.RELOP_EQ,
                   PR_MESSAGE_CLASS_A,
                   (PR_MESSAGE_CLASS_A, 'IPC.MS.REN.USERFIELDS')))

    cols = (PR_USERFIELDS,)

    table.SetColumns(cols, 0)

    rows = mapi.HrQueryAllRows(table, cols, restriction, None, 0)

    assert len(rows)<=1, "Only expecting 1 (or 0) rows"

    tag, val = rows[0][0]

    prop_name = GetPropTagName(folder, tag)

    prop_repr = FormatPropertyValue(tag, val, folder,
                                    shorten, get_large_props)

    print("%-20s: %s" % (prop_name, prop_repr), file=stream)
 def usage(driver, extra = None):

    folder_doc = driver.GetFolderNameDoc()

    if extra:

        print(extra)

        print()

    msg = """\
Usage: %s [options ...] subject of the message
Dumps all properties for all messages that match the subject.  Subject
matching is substring and ignore-case.
-c - Write output to the clipboard, ready for pasting into an email
-f - Search for the message in the specified folder (default = Inbox)
-s - Shorten long property values.
-a - Include attachments
-l - Get the data for very large properties via a stream
-n - Show top-level folder names and exit
--dump-folder
     Dump the properties of the specified folder.
--dump-folder-assoc-contents
     Dump the 'associated contents' table of the specified folder.
--dump-folder-user-props
     Find and dump the PR_USERFIELDS field for the specified table.
%s
Use the -n option to see all top-level folder names from all stores.""" \
    % (os.path.basename(sys.argv[0]),folder_doc)

    print(msg)

    sys.exit(1)
 def main():

    driver = mapi_driver.MAPIDriver()

    import getopt

    try:

        opts, args = getopt.getopt(sys.argv[1:], "caf:snl",
                                   ["dump-folder",
                                    "dump-folder-assoc-contents",
                                    "dump-folder-user-props",
                                    ])

    except getopt.error as e:

        usage(driver, e)

    folder_name = ""

    shorten = False

    get_large_props = False

    include_attach = False

    write_clipboard = False

    dump_folder = dump_folder_assoc_contents = dump_folder_user_props = False

    for opt, opt_val in opts:

        if opt == "-f":

            folder_name = opt_val

        elif opt == "-c":

            write_clipboard = True

        elif opt == "--dump-folder":

            dump_folder = True

        elif opt == "--dump-folder-assoc-contents":

            dump_folder_assoc_contents = True

        elif opt == "--dump-folder-user-props":

            dump_folder_user_props = True

        elif opt == "-s":

            shorten = True

        elif opt == "-a":

            include_attach = True

        elif opt == "-l":

            get_large_props = True

        elif opt == "-n":

            driver.DumpTopLevelFolders()

            sys.exit(1)

        else:

            usage(driver, "Unknown arg '%s'" % opt)

    stream = None

    if write_clipboard:

        stream_name = tempfile.mktemp("spambayes")

        stream = open(stream_name, "w")

    if not folder_name:

        folder_name = "Inbox" 

    subject = " ".join(args)

    is_table_dump = dump_folder_assoc_contents or \
                    dump_folder or dump_folder_user_props

    if is_table_dump and subject or not is_table_dump and not subject:

        if is_table_dump:

            extra = "You must not specify a subject with '-p'"

        else:

            extra = "You must specify a subject (unless you use '-p')"

        usage(driver, extra)

    try:

        folder = driver.FindFolder(folder_name)

    except ValueError as details:

        print(details)

        sys.exit(1)

    if is_table_dump:

        if dump_folder:

            DumpItemProps(folder, shorten, get_large_props, stream)

        if dump_folder_assoc_contents:

            table = folder.GetContentsTable(mapi.MAPI_ASSOCIATED)

            DumpTable(driver, table, folder, shorten, get_large_props, stream)

        if dump_folder_user_props:

            table = folder.GetContentsTable(mapi.MAPI_ASSOCIATED)

            FindAndDumpTableUserProps(driver, table, folder,
                                      shorten, get_large_props, stream)

    else:

        DumpProps(driver, folder, subject, include_attach,
                  shorten, get_large_props, stream)

    if write_clipboard:

        stream.close()

        stream = open(stream_name, "r")

        win32clipboard.OpenClipboard()

        win32clipboard.EmptyClipboard()

        win32clipboard.SetClipboardText(stream.read())

        stream.close()

        os.unlink(stream_name)

        print("Output successfuly written to the Windows clipboard")
 if __name__=='__main__':

    main()

 if __name__=='__main__':

    main()





try:

    TBL_ALL_COLUMNS = mapi.TBL_ALL_COLUMNS

except AttributeError: 

    TBL_ALL_COLUMNS = 1



