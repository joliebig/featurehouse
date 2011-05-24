"""Scores one or more items in your Outlook store."""

import sys, os

import optparse

from win32com.mapi import mapi, mapiutil

from win32com.mapi.mapitags import *

import win32clipboard

try:

    from manager import BayesManager

except ImportError:

    if hasattr(sys, "frozen"):

        raise

    sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

    from manager import BayesManager



from addin import GetClues

import mapi_driver

from io import StringIO

def Score(driver, manager, mapi_folder, subject, options, stream=None):

    num = 0

    if options.all:

        getter = driver.GetAllItems

        getter_args = (mapi_folder,)

    else:

        getter = driver.GetItemsWithValue

        getter_args = (mapi_folder, PR_SUBJECT_A, subject)

    for item in getter(*getter_args):

        num += 1

        if num % 1000 == 0:

            print("Processed", num, "items...", file=sys.stderr)

        hr, props = item.GetProps((PR_ENTRYID,PR_STORE_ENTRYID, PR_SUBJECT_A), 0)

        (tag, eid), (tag, store_eid), (tag, sub) = props

        eid = mapi.HexFromBin(eid)

        store_eid = mapi.HexFromBin(store_eid)

        try:

            msm = manager.message_store.GetMessage((store_eid, eid))

            manager.classifier_data.message_db.load_msg(msm)

            score = manager.score(msm)

            if not options.quiet: print("Message %r scored %g" % (sub, score))

            if options.show_clues:

                clues = GetClues(manager, msm)

                if not options.quiet: print(clues, file=stream)

            if options.quiet:

                continue

            if options.show_image_info:

                eob = msm.GetEmailPackageObject()

                from spambayes.ImageStripper import crack_images

                from spambayes.tokenizer import imageparts

                image_text, image_toks = crack_images(imageparts(eob))

                print("Image text:", repr(image_text), file=stream)

                print("Image tokens:", repr(image_toks), file=stream)

            print(file=stream) 

        except:

            print("FAILED to convert message:", sub, file=sys.stderr)

            raise

    print("Scored", num, "messages.", file=stream)
 def main():

    driver = mapi_driver.MAPIDriver()

    parser = optparse.OptionParser("%prog [options] subject of message ...",
                                   description=__doc__)

    parser.add_option("-q", "--quiet",
                      action="store_true", dest="quiet", default=False,
                      help="don't print score info - useful for testing")

    parser.add_option("-f", "--folder",
                      action="store", default="Inbox",
                      help="folder to search")

    parser.add_option("", "--clipboard",
                      action="store_true",
                      help="write results to the clipboard")

    parser.add_option("-c", "--show-clues",
                      action="store_true",
                      help="also write the clues for the message")

    parser.add_option("-a", "--all",
                      action="store_true",
                      help="ignore the subject and score all items in the folder")

    parser.add_option("-i", "--show-image-info",
                      action="store_true",
                      help="show the information we can extract from images "
                           "in the mail")

    options, args = parser.parse_args()

    subject = " ".join(args)

    try:

        folder = driver.FindFolder(options.folder)

    except ValueError as details:

        parser.error(details)

    stream = None

    if options.clipboard:

        stream = StringIO()

    Score(driver, BayesManager(), folder, subject, options, stream)

    if options.clipboard:

        win32clipboard.OpenClipboard()

        win32clipboard.EmptyClipboard()

        win32clipboard.SetClipboardText(stream.getvalue())

        print("Output successfuly written to the Windows clipboard")
 if __name__=='__main__':

    main()

 if __name__=='__main__':

    main()





try:

    from manager import BayesManager

except ImportError:

    if hasattr(sys, "frozen"):

        raise

    sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

    from manager import BayesManager



