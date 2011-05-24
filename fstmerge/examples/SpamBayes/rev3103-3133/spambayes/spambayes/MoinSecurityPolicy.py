"""
This module implements a security policy for MoinMoin based on the SpamBayes
classifier.  To use it, import it like so in your wikiconfig.py file:
    from spambayes.MoinSecurityPolicy import SecurityPolicy
Two pages are special, HamPages and SpamPages.  Each refers to a specific
revision of the raw version of particulars wiki pages.  When either of these
pages is updated the SpamBayes database is rebuilt based on the pages they
reference.  When any other page is updated it is scored against the current
database.  If its score is <= SecurityPolicy.ham_cutoff the edit is
accepted.  If not, the page edit is accepted but reverted and a reference to
the reverted page revision is mailed to the members of the AdminGroup for
review.  If it is only possibly spam (score between
SecurityPolicy.ham_cutoff and SecurityPolicy.spam_cutoff) the recipients are
instructed to add it to either HamPages or SpamPages as appropriate.  If it
is truly spam (score >= SecurityPolicy.spam_cutoff), the recipients are
instructed to add it to HamPages if it is actually okay, but to simply
discard it otherwise.
The HamPages and SpamPages pages are formatted as any other *Group page, a
top-level list forms a group while everything else is ignored.
The ham_cutoff, spam_cutoff and spam_db attributes are defined at the class
level to make it easy for the user to change their values.  The defaults are:
    ham_cutoff		0.15
    spam_cutoff		0.60
    spam_db		'spam.db'
The spam_db attribute should always be a relative path (should not start
with '/').  When relative it will be taken relative to the directory
containing the event-log file.
"""

import os

import atexit

import urllib

import urlparse

from MoinMoin.security import Permissions

from MoinMoin.wikidicts import Group

from MoinMoin.user import User, getUserId

from MoinMoin.util.mail import sendmail

from MoinMoin.Page import Page

from MoinMoin.PageEditor import PageEditor

from spambayes import hammie, storage

from spambayes.tokenizer import log2, Tokenizer, numeric_entity_re, \
     numeric_entity_replacer, crack_urls, breaking_entity_re, html_re, \
     tokenize_word

class  SecurityPolicy (Permissions) :
	ham_cutoff = 0.15
	    spam_cutoff = 0.60
	    spam_db = "spam.db"
	    def __init__(self, user):

        Permissions.__init__(self, user)

        self.sbayes = None
 def open_spamdb(self, request):

        if self.sbayes is None:

            event_log = request.rootpage.getPagePath('event-log', isfile=1)

            spam_db = os.path.join(os.path.dirname(event_log), self.spam_db)

            self.sbayes = Hammie(storage.open_storage(spam_db, "pickle", 'c'))

            atexit.register(self.close_spamdb)
 def close_spamdb(self):

        if self.sbayes is not None:

            self.sbayes.store()

            self.sbayes = None
 def retrain(self, request):

        self.close_spamdb()

        if os.path.exists(self.spam_db):

            os.unlink(self.spam_db)

        self.open_spamdb(request)

        nham = nspam = 0

        for url in Group(request, "HamPages").members():

            scheme, netloc, path, params, query, frag = urlparse.urlparse(url)

            rev = 0

            for pair in query.split("&"):

                key, val = pair.split("=")

                if key == "rev":

                    raw = int(val)

                    break

            pg = Page(request, path[1:], rev=rev)

            self.sbayes.train_ham(pg.get_raw_body())

            nham += 1

        for url in Group(request, "SpamPages").members():

            scheme, netloc, path, params, query, frag = urlparse.urlparse(url)

            rev = 0

            for pair in query.split("&"):

                key, val = pair.split("=")

                if key == "rev":

                    raw = int(val)

                    break

            pg = Page(request, path[1:], rev=rev)

            self.sbayes.train_spam(pg.get_raw_body())

            nspam += 1

        self.close_spamdb()

        return (nham, nspam)
 def save(self, editor, newtext, rev, **kw):

        self.open_spamdb(editor.request)

        score = self.sbayes.score(newtext)

        save_result = Permissions.save(self, editor, newtext, rev, **kw)

        if save_result and editor.page_name in ("HamPages", "SpamPages"):

            self.retrain(editor.request)

            return save_result

        if score < self.ham_cutoff:

            return save_result

        if not save_result:

            return save_result

        self.force_revert(editor.page_name, editor.request)
 def force_revert(self, pagename, request):

        from MoinMoin.PageEditor import PageEditor

        rev = int(request.form['rev'][0])

        revstr = '%08d' % rev

        oldpg = Page(request, pagename, rev=rev)

        pg = PageEditor(request, pagename)

        _ = request.getText

        msg = _("Thank you for your changes. Your attention to detail is appreciated.")

        try:

            pg._write_file(oldpg.get_raw_body(),
                           action="SAVE/REVERT",
                           extra=revstr)

            pg.clean_acl_cache()

        except pg.SaveError, msg:

            pass

        savemsg = unicode(msg)

        request.reset()

        pg.send_page(request, msg=savemsg)

        return None
 def mail_admins_about(self, request, page_name, score):

        """Send email to the AdminGroup about a suspect page."""

        return

        admin_text = Page(request, "AdminGroup").get_raw_body()

        group = Group(request, admin_text)

        emails = []

        for name in group.members():

            uid = getUserId(request, name)

            if uid is None:

                continue

            u = User(request, uid)

            emails.append(u.email)

        if score < self.spam_cutoff:

            subject = "Possible wiki spam"

            text = """\
This page as submitted to the wiki might be spam:
    %(page_name)s
If that is not the case, add the page's URL (including action=raw and the
revision number) to HamPages then revert the page to that revision.  If it
is spam, add it instead to SpamPages.
""" % locals()

        else:

            subject = "Probable wiki spam"

            text = """\
This page as submitted to the wiki is likely to be spam:
    %(page_name)s
If that is not the case, add the page's URL (including action=raw and the
revision number) to HamPages then revert the page to that revision.  If it
is spam, do nothing.
""" % locals()

        sendmail(request, emails, subject, text)

class  Tokenizer (Tokenizer) :
	def tokenize(self, text):

        """Tokenize a chunk of text.
        Pulled mostly verbatim from the SpamBayes code.
        """

        maxword = 20

        text = numeric_entity_re.sub(numeric_entity_replacer, text)

        for cracker in (crack_urls,):

            text, tokens = cracker(text)

            for t in tokens:

                yield t

        text = breaking_entity_re.sub(' ', text)

        text = html_re.sub('', text)

        for w in text.split():

            n = len(w)

            if 3 <= n <= maxword:

                yield w

            elif n >= 3:

                for t in tokenize_word(w):

                    yield t

class  Hammie (hammie.Hammie) :
	def __init__(self, bayes):

        hammie.Hammie.__init__(self, bayes)

        self.tokenizer = Tokenizer()
 def _scoremsg(self, msg, evidence=False):

        return self.bayes.spamprob(self.tokenizer.tokenize(msg), evidence)
 def train(self, msg, is_spam, add_header=False):

        self.bayes.learn(self.tokenizer.tokenize(msg), is_spam)



