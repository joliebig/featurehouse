import sys, os, shutil
from manager import GetManager
NUM_BUCKETS = 10
DEFAULT_DIRECTORY = "..\\testtools\\Data"
import re
mime_header_re = re.compile(r"""
    ^ content- (type | transfer-encoding) : [^\n]* \n
    ([ \t] [^\n]* \n)*  # suck up adjacent continuation lines
""", re.VERBOSE | re.MULTILINE | re.IGNORECASE)
def count_messages(folder):
    result = 0
    for msg in folder.GetMessageGenerator():
        result += 1
    return result
def BuildBuckets(manager, num_buckets):
    store = manager.message_store
    config = manager.config
    num_ham = num_spam = 0
    for folder in store.GetFolderGenerator(config.training.spam_folder_ids,
                                           config.training.spam_include_sub):
        num_spam += count_messages(folder)
    for folder in store.GetFolderGenerator(config.training.ham_folder_ids,
                                           config.training.ham_include_sub):
        num_ham += count_messages(folder)
    dirs = ["Set%d" % i for i in range(1, num_buckets + 1)]
    return num_spam, num_ham, dirs
def get_text(msg, old_style):
    if old_style:
        email_object = msg.OldGetEmailPackageObject()
    else:
        email_object = msg.GetEmailPackageObject()
    try:
        return email_object.as_string()
    except:
        pass
    email_object = msg.GetEmailPackageObject(strip_mime_headers=False)
    text = email_object.as_string()
    i = text.find('\n\n')  # boundary between headers and body
    if i < 0:
        i = len(text) - 2
    headers, body = text[:i+2], text[i+2:]
    headers = mime_header_re.sub('', headers) # remove troublesome headers
    text = headers + body
    import email
    email.message_from_string(text)
    return text
def _export_folders(manager, root, buckets, folder_ids, include_sub, old_style):
    from random import choice
    num = 0
    store = manager.message_store
    for folder in store.GetFolderGenerator(folder_ids, include_sub):
        print("", folder.name)
        for message in folder.GetMessageGenerator():
            this_dir = os.path.join(root,  choice(buckets))
            try:
                msg_text = get_text(message, old_style)
            except KeyboardInterrupt:
                raise
            except:
                print("Failed to get message text for '%s': %s" \
                      % (message.GetSubject(), sys.exc_info()[1]))
                continue
            fname = os.path.join(this_dir, message.GetID()[1]) + ".txt"
            f = open(fname, "w")
            f.write(msg_text)
            f.close()
            num += 1
    return num
def export(directory, num_buckets, old_style):
    print("Loading bayes manager...")
    manager = GetManager()
    config = manager.config
    num_spam, num_ham, buckets = BuildBuckets(manager, num_buckets)
    print("Have", num_spam, "spam and", num_ham, "ham to export,", end=' ')
    print("spread over", len(buckets), "directories.")
    for sub in "Spam", "Ham":
        if os.path.exists(os.path.join(directory, sub)):
            shutil.rmtree(os.path.join(directory, sub))
        for b in buckets + ["reservoir"]:
            d = os.path.join(directory, sub, b)
            os.makedirs(d)
    print("Exporting spam...")
    num = _export_folders(manager,
                          os.path.join(directory, "Spam"),
                          buckets,
                          config.training.spam_folder_ids,
                          config.training.spam_include_sub,
                          old_style)
    print("Exported", num, "spam messages.")
    print("Exporting ham...")
    num = _export_folders(manager,
                          os.path.join(directory, "Ham"),
                          buckets,
                          config.training.ham_folder_ids,
                          config.training.ham_include_sub,
                          old_style)
    print("Exported", num, "ham messages.")
def main():
    import getopt
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hqon:")
    except getopt.error as d:
        usage(d)
    quiet = 0
    old_style = False
    num_buckets = NUM_BUCKETS
    for opt, val in opts:
        if opt == '-h':
            usage()
        elif opt == '-q':
            quiet = 1
        elif opt == '-n':
            num_buckets = int(val)
        elif opt == '-o':
            old_style = True
        else:
            assert 0, "internal error on option '%s'" % opt
    if len(args) > 1:
        usage("Only one directory name can be specified.")
    elif args:
        directory = args[0]
    else:
        directory = os.path.join(os.path.dirname(sys.argv[0]),
                                 DEFAULT_DIRECTORY)
    if num_buckets < 1:
        usage("-n must be at least 1.")
    directory = os.path.abspath(directory)
    print("This program will export your Outlook Ham and Spam folders")
    print("to the directory '%s'" % directory)
    if os.path.exists(directory):
        print("*******")
        print("WARNING: all existing files in '%s' will be deleted" % directory)
        print("*******")
    if not quiet:
        input("Press enter to continue, or Ctrl+C to abort.")
    export(directory, num_buckets, old_style=old_style)
def usage(errormsg=None):
    if errormsg:
        print(str(errormsg))
        print()
    print(""" \
Usage: %s [-h] [-q] [-n nsets] [directory]
-h : help - display this msg and stop
-q : quiet - don't prompt for confirmation.
-n : number of Set subdirectories in the Ham and Spam dirs, default=%d
Export Spam and Ham training folders defined in the Outlook Plugin to a test
directory.  The directory structure is as defined in the parent
README-DEVEL.txt file, in the "Standard Test Data Setup" section.  Files are
distributed randomly among the Set subdirectories.  You should probably use
rebal.py afterwards to even them out.
If 'directory' is not specified, '%s' is assumed.
If 'directory' exists, it will be recursively deleted before
the export (but you will be asked to confirm unless -q is given).""" \
            % (os.path.basename(sys.argv[0]),
               NUM_BUCKETS,
               DEFAULT_DIRECTORY))
    sys.exit(1)
if __name__=='__main__':
    main()
