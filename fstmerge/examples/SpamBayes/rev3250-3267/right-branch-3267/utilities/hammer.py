import os, re, random, textwrap
from spambayes import storage
from spambayes import tokenizer
__author__ = "Richie Hindle <richie@entrian.com>"
headerTemplate = """To: %(To)s
From: %(From)s
Subject: %(Subject)s
Date: %(Date)s
"""
FILENAME = '__hammer.db'
try:
    os.remove(FILENAME)
except OSError:
    pass
bayes = storage.open_storage(FILENAME, True)
def train(text, isSpam):
    """Trains the classifier on the given text."""
    tokens = tokenizer.tokenize(text)
    bayes.learn(tokens, isSpam)
def classify(text):
    """Classifies the given text, returning the spamprob."""
    tokens = tokenizer.tokenize(text)
    return bayes.spamprob(tokens)
def makeMessage(isSpam):
    """Builds a fake email message full of random words taken from a
    selection of ham and spam messages."""
    if isSpam:
        messages = spam
    else:
        messages = ham
    messageIndex = random.randrange(3)
    headers = headerTemplate % messages[messageIndex]
    bodyWords = []
    for i in range(3):
        body = messages[i]['Body']
        for j in range(10):
            offset = random.randrange(len(body) - 50)
            bodySection = body[offset:offset+50]
            bodyWords.extend(re.findall(r'[^\s]+', bodySection))
        for i in range(5):
            aToZ = 'abcdefghijklmnopqrstuvwxyz'
            wordLength = random.randrange(3, 8)
            word = ''.join([random.choice(aToZ) for j in range(wordLength)])
            bodyWords.append(word)
    body = '\n'.join(textwrap.wrap(' '.join(bodyWords)))
    return headers + body
def hammer():
    """Trains and classifies repeatedly."""
    global bayes
    wellFlushed = False
    for i in range(1, 1000000):
        isSpam = random.choice([True, False])
        train(makeMessage(isSpam), isSpam)
        if random.randrange(1000) == 1:
            print("Flushing.")
            bayes.store()
            if i > 500:
                wellFlushed = True
        isSpam = random.choice([True, False])
        prob = classify(makeMessage(isSpam))
        if i < 10 or i % 100 == 0:
            print("%6.6d: %d, %.4f" % (i, isSpam, prob))
        if wellFlushed and random.randrange(1000) == 1:
            print("Re-opening.")
            bayes = storage.open_storage(FILENAME, True)
def test():
    """Print a random ham message and a random spam message."""
    print(makeMessage(False))
    print()
    print(makeMessage(True))
ham = [ {
'To': """<richie@entrian.com>, <spambayes-dev@python.org>""",
'Subject': """RE: [spambayes-dev] Experimental SpamBayes build available""",
'From': """"Tony Meyer" <tameyer@ihug.co.nz>""",
'Date': """Wed, 31 Dec 2003 11:51:29 +1300""",
'Body': """[I'll leave the install stuff for Mark, but I can sort out the rest of
these].
> The ini file for the proxy appeared in "C:\Documents and
> Settings\rjh\Application Data\SpamBayes\Proxy" as you'd
> expect, but the database and cache directories appeared in
> "C:\Program Files\SpamBayes\bin".
Did the ini file have the appropriate [Storage] lines in it?  It's meant to
add them in there, storing the directories in that directory, too.  You
didn't already have an ini file in there, did you?  (It only adds those
lines if it's a new file, so that it doesn't overwrite someone's settings).
> I'd question whether
> we need the Stop/Start command - why would I want the tray
> icon to stay there but the application to not run?
I was thinking this just yesterday. I'm not sure what the original reasoning
behind having it was (and it may have been me that put it there ;).
+1 to getting rid of it, unless someone does know the reasoning.  We can
dump the 'stopped' icon, then, too.  (I'd like to see a '!' icon, though,
which appeared when there were important status messages to review).
> After training through the web interface, the home page still
> says "Database has no training information ..." even though
> the stats say "Total emails trained: Spam: 3 Ham: 18".
Good spotting.  I've checked in a fix for this.
> Defaulting the "Maximum results" field in the Find pane to 1
> seems wrong. It made sense when all you could do was search
> for a message ID (because they're unique) but if I'm
> searching for text, I'll want to see all the hits.
Fair enough.  Line 435 of ui.html; change it to whatever you like most :)
> The Find pane only looks in the unknown cache, so it won't
> find anything once you've trained.  It ought to look in the
> ham and spam caches as well.
Are you positive?  The code has it looking in all three, and a quick test
here had it finding messages in more than one.
> I deliberately induced a false positive (by training on a
> thousand spams with no hams trained) then corrected it via
> the Review page, and the statistics now say "1 being false
> negatives" (plural: ack!) and "0 being false positives".
> That's the wrong way round.
Opps, my bad.  I've checking in a fix for this.  I think I've fixed all the
plurals, too.  If you've still got that false positive statistic around,
could you give it a run from cvs?
=Tony Meyer
""",
},
{
'To': """'spambayes@python.org'" <spambayes@python.org>""",
'Subject': """[Spambayes] Spambayes Software and SPAM folder""",
'From': """Lily Cornely <lcornely@clfund.com>""",
'Date': """Tue, 30 Dec 2003 16:18:03 -0500""",
'Body': """Someone here told me that you need to not delete your SPAM folder as the
software needs it to know what is "spam". Is this correct? This would seem
to be a flaw as this file will build up dramatically over time and be
impossible to manage.
Please advise. Can I empty my SPAM folder - or not?
Lily Cornely
Marketing Consultant
CL Fund
1920 Gulf Tower
707 Grant Street
Pittsburgh, PA  15219
412.201.2450
412.201.2451 - Fax
lcornely@clfund.com
_______________________________________________
Spambayes@python.org
http://mail.python.org/mailman/listinfo/spambayes
Check the FAQ before asking: http://spambayes.sf.net/faq.html"""
},
{
'To': """<richie@entrian.com>""",
'Subject': """January Sale - up to 50% off!""",
'From': """"Firebox.com" <newsletter@firebox.com>""",
'Date': """Wed, 31 Dec 2003 15:41:46 GMT""",
'Body': """<html>
<head>
<title>Firebox.com 
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
</head>
<BODY text="
BGCOLOR="
<DIV ALIGN="center">
        <TABLE BORDER="0" CELLPADDING="0" CELLSPACING="0" WIDTH="528">
                <TR>
                        <TD COLSPAN="2" ROWSPAN="2" ALIGN="left" HEIGHT="14" WIDTH="14"><IMG src="http://www.firebox.com/i/nl_corner_tl.gif" alt=""
border="0" height="14" width="14"></TD>
                        <TD BGCOLOR="
width="500"></TD>
                        <TD COLSPAN="2" ROWSPAN="2" ALIGN="right" HEIGHT="14" WIDTH="14"><IMG src="http://www.firebox.com/i/nl_corner_tr.gif" alt=""
border="0" height="14" width="14"></TD>
                </TR>
                <TR>
                        <TD BGCOLOR="
width=500></TD>
                </TR>
                <TR>
                        <TD BGCOLOR="
                        <TD BGCOLOR="
                        <TD BGCOLOR="
                        <table bgcolor="
                        <tr>
                        <td>
<!-- Content Starts Here //-->
              <DIV ALIGN="center"> <font face="Verdana, Arial, Helvetica, sans-serif" size="1">if
                you can't read this newsletter properly, <a href="http://www.firebox.com/newsletter/firebox_newsletter_55.html">click
                here</a></font><br>
                <br>
                                </DIV>
              <a href="http://www.firebox.com/aff.php?aff=678" target="_blank"><img src="http://www.firebox.com/i/snow_logo_300.gif" width="300" height="41"
border=0 alt="firebox.com"></a><br>
                                <br>
              <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><b>Firebox.com
                Newsletter 
              <div align="center"><a
href="http://www.firebox.com/aff.php?aff=678&redirect=index.html?dir=firebox&action=search&searchstring=jansale&searchfeature=1" target="_blank"><img
src="http://www.firebox.com/i/jansale_feature.jpg" alt="January Sale - up to 50% off!" width="393" height="175" border="0"></a></div>
              <p><font size="2" face="Verdana, Arial, Helvetica, sans-serif">Christmas
                is over for another year but we are still full of seasonal cheer!
                In time-honoured January Sale tradition we have slashed prices
                on a wide range of products, just a few of which are featured
                below. Be sure to check the <a
href="http://www.firebox.com/aff.php?aff=678&redirect=index.html?dir=firebox&action=search&searchstring=jansale&searchfeature=1">January
                Sale</a> page for the full list of bargains!</font></p>
              <table width="100%" border="0" cellspacing="0" cellpadding="0">
                <tr>
                  <td bgcolor="
Up To 50%!"></td>
                </tr>
              </table> <br clear="all">
              <table width="100%" border="0" cellpadding="5">
                <tr align="center" valign="top">
                  <td width="33%">
                    <p><a href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=482" target="_blank"><img
src="http://www.firebox.com/pic/p482p.gif" alt="Atari  Classics 10-in-1" width="80" height="81" border="0"></a></p>
                    <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><a
href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=482" target="_blank"><b>Atari
                      10-in-1</b></a> <br>
                      was &pound;24.95<br>
                      now <font color="
                      <font face="Verdana, Arial, Helvetica, sans-serif" color="
                      off!</b></font> </p></td>
                  <td width="33%">
                    <p><a href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=353" target="_blank"><img
src="http://www.firebox.com/pic/p353p.gif" alt="Desktop Rover" width="80" height="91" border="0"></a></p>
                    <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><a
href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=353" target="_blank"><b>Desktop
                      Rover</b></a> <br>
                      was &pound;39.95<br>
                      now </font><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><font
color="
                      <font face="Verdana, Arial, Helvetica, sans-serif" color="
                      off! </b></font></p>
                    <font face="Verdana, Arial, Helvetica, sans-serif" size="2">&nbsp;
                    </font></td>
                  <td width="33%">
<p><a
href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=727"
target="_blank"><img src="http://www.firebox.com/pic/p727p.jpg" alt="SiPix
Pocket DV Camcorder" width="80" height="65" border="0"></a></p>
<p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><a
href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=727"
target="_blank"><b>SiPix DV Camcorder</b></a><br>
was &pound;99.95<br>
now </font><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><font
color="
<font face="Verdana, Arial, Helvetica, sans-serif" color="
size="1"><b>50% off! </b></font></p></td>
                </tr>
                <tr align="center" valign="top">
                  <td width="33%">
                    <p><a href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=585" target="_blank"><img
src="http://www.firebox.com/pic/p585p.jpg" alt="Mini-K MP3 Player" width="80" height="108" border="0"></a></p>
                    <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><a
href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=585" target="_blank"><b>Mini-K
                      MP3 Player</b></a> <br>
                      was &pound;59.95<br>
                      now <font color="
                      <font face="Verdana, Arial, Helvetica, sans-serif" color="
                      &pound;10!</b></font> </p></td>
                  <td width="33%">
                    <p><a href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=501" target="_blank"><img
src="http://www.firebox.com/pic/p501p.gif" alt="Pocket DV3" width="80" height="76" border="0"></a></p>
                    <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><a
href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=501" target="_blank"><b>Pocket
                      DV3</b></a> <br>
                      was &pound;129.95<br>
                      now <font color="
                      <font face="Verdana, Arial, Helvetica, sans-serif" color="
                      &pound;30!</b></font> </p>
                    <font face="Verdana, Arial, Helvetica, sans-serif" size="2">&nbsp;
                    </font></td>
                  <td width="33%">
                    <p><a href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=689" target="_blank"><img
src="http://www.firebox.com/pic/p689p.jpg" alt="Micro Spinster" width="80" height="68" border="0"></a></p>
                    <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><a
href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=689" target="_blank"><b>Micro
                      Spinster </b></a> <br>
                      was &pound;12.95<br>
                      now <font color="
                      <font face="Verdana, Arial, Helvetica, sans-serif" color="
                      off! </b></font></p></td>
                </tr>
                <tr align="center" valign="top">
                  <td>
                    <p><a href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=508" target="_blank"><img
src="http://www.firebox.com/pic/p508p.gif" alt="Storm Hopper" width="80" height="70" border="0"></a></p>
                    <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><a
href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=508" target="_blank"><b>Storm
                      Hopper </b></a> <br>
                      was &pound;29.95<br>
                      now <font color="
                      <font face="Verdana, Arial, Helvetica, sans-serif" color="
                      &pound;10! </b></font></p></td>
                  <td>
                    <p><a href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=645" target="_blank"><img
src="http://www.firebox.com/pic/p645p.jpg" alt="D'Zign Digital Camera" width="80" height="76" border="0"></a></p>
                    <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><a
href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=645" target="_blank"><b>D'Zign
                      Digital Camera</b></a> <br>
                      was &pound;129.95<br>
                      now <font color="
                      <font face="Verdana, Arial, Helvetica, sans-serif" color="
                      &pound;30!</b></font> </p></td>
                  <td>
                    <p><a href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=510" target="_blank"><img
src="http://www.firebox.com/pic/p510p.gif" alt="Mars Detector" width="80" height="77" border="0"></a></p>
                    <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><a
href="http://www.firebox.com/aff.php?aff=678&redirect=product.php?pid=510" target="_blank"><b>Mars
                      Detector</b></a> <br>
                      was &pound;29.95<br>
                      now <font color="
                      <font face="Verdana, Arial, Helvetica, sans-serif" color="
                      &pound;10! </b></font></p></td>
                </tr>
              </table>
                        </td>
                </tr>
                  <tr>
            <td>
                        <br>
                        <div align="center">
                <p><font face="Verdana, Arial, Helvetica, sans-serif" size="4">
                  <a href="http://www.firebox.com/aff.php?aff=678&redirect=index.html?dir=firebox&action=search&searchstring=jansale&searchfeature=1"
target="_blank">Check
                  out the other great deals!</a></font></p>
                <p><font face="Verdana, Arial, Helvetica, sans-serif" size="1">Hurry - all offers only available while stocks last! Sale ends January 15th,
2004.</font></p>
              </div>
                                <table width="100%" border="0" cellspacing="0" cellpadding="0">
                <tr>
                  <td bgcolor="
src="http://www.firebox.com/i/tasse_comp.gif" alt="Competition Time" width="201" height="26" border="0" align="left"></a></td>
                </tr>
              </table>
                          <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><b>WIN
                a &quot;Zorbing&quot; experience with Stuck on You! </b></font></p>
              <a href="http://www.firebox.com/aff.php?aff=678&redirect=index.html?dir=competitions&action=competition55" target="_blank"><img
src="http://www.firebox.com/i/soy_small.gif" alt="Stuck On You" width="100" height="149" hspace="10" border="0" align="left"></a>
              <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2">
                Outrageous comedy and a lot of heart are joined at the hip in
                Stuck on You, the latest envelope-pushing exercise in hilarity
                from The Farrelly Brothers (There's Something About Mary, Dumb
                and Dumber). To celebrate the launch of the movie (in cinemas
                from 2nd January 2004) we are offering the chance for you and
                a partner to get 'stuck together' inside a huge bouncy ball and
                launched down a 300 metre hill! Yes, it is fun, really! <a
href="http://www.firebox.com/aff.php?aff=678&redirect=index.html?dir=competitions&action=competition55" target="_blank">enter&
clear="all">
              </p>
              <table width="100%" border="0" cellspacing="0" cellpadding="0">
                <tr>
                  <td bgcolor="
Winner!"></td>
                </tr>
              </table>
                          <p> <font face="Verdana, Arial, Helvetica, sans-serif" size="2"><b>Gavin
                Smith is the Firebox Film Festival Winner!</b></font><br>
                <br>
                <a href="http://www.firebox.com/aff.php?aff=678&redirect=index.html?dir=features&action=film_festival" target="_blank"><img
src="http://www.firebox.com/v/t/thumb_intogame.jpg" width=100 height=75 border=0 hspace=10 align="left" alt="Into The Game Screenshot"></a>
                <font face="Verdana, Arial, Helvetica, sans-serif" size="2">The
                voting was very close, but in the end the clear winner was Gavin
                Smith from Edinburgh with his epic of videogame immersion, <a
href="http://www.firebox.com/aff.php?aff=678&redirect=index.html?dir=features&action=film_festival" target="_blank">&quot;Into
                The Game&quot;</a>. Congratulations to Gavin and his brother (who
                stars in the movie), who will receive &pound;1000 of Firebox Vouchers!</font></p>
              <p><font size="2" face="Verdana, Arial, Helvetica, sans-serif">Congratulations
                to all the <a href="http://www.firebox.com/aff.php?aff=678&redirect=index.html?dir=features&action=film_festival"
target="_blank">shortlisted
                entrants</a>, who will each receive &pound;200 in Firebox vouchers,
                and thanks to <strong>everyone</strong> who entered - there were
                loads of great movies submitted and we look forward to doing it
                all again next year.</font><br>
              </p>
              <font face="Verdana, Arial, Helvetica, sans-serif" size="2">
              <p>Happy New Year, see you in 2004!<br>
                <br>
                <br>
                The Firebox Team<br>
                <a href="http://www.firebox.com/aff.php?aff=678" target="_blank">www.firebox.com</a><br>
                <b>0870 241 4289</b></p>
              </font>
              <p><font face="Verdana, Arial, Helvetica, sans-serif" size="2">To tell us anything: <a href="mailto:info@firebox.com">info@firebox.com</a>
                                <br>
                                <br>
                                Please don't reply to this email - to stop receiving the Firebox.com newsletter, <a
href="http://www.firebox.com/admin/newsletter.php?task=remove&address=richie@entrian.com
" target="_blank">click here</a>.
                                This email was sent to: <b>richie@entrian.com
</b>
                </font></p>
                                <!-- Content Ends Here //-->
                                </td>
                        </tr>
                </table>
                </TD>
                <TD BGCOLOR="
                <TD BGCOLOR="
        </TR>
        <TR>
                <TD COLSPAN="2" ROWSPAN="2" ALIGN="left" HEIGHT="14" WIDTH="14"><IMG alt="" border=0 height=14
src="http://www.firebox.com/i/nl_corner_bl.gif" width=14></TD>
                <TD BGCOLOR="
                <TD COLSPAN="2" ROWSPAN="2" ALIGN="right" HEIGHT="14" WIDTH="14"><IMG alt="" border=0 height=14
src="http://www.firebox.com/i/nl_corner_br.gif" width=14></TD>
        </TR>
        <TR>
                <TD BGCOLOR="
        </TR>
</TABLE>
</DIV>
</body>
</html>
. """
} ]
spam = [ {
'To': """nince@email.msn.com""",
'Subject': """Big Savings On Entertainment""",
'From': """"maya" <qsde5mike0123895317214@yahoo.com>""",
'Date': """Sat, 16 Apr 2039 06:37:54 -0500""",
'Body': """Dear nince ,
<html>
<body>
<p align="center"><b><font size="4" color="
</font>
<br>
<font size="4">
Gain instant access to:<br>
<br>
<font color="
 * PPV channels&nbsp;<br>
 * Boxing, Wrestling, &amp; any other Sports Event on PPV&nbsp;<br>
 * Adult Channels&nbsp;<br>
 * Anything offered on PPV channels&nbsp;<br>
</font></font>
<br>
<font size="4">
 This filter will allow you to receive all the channels<br>
 that you now order with your remote control!<br>
<font color="
 PAY-PER-VIEWS, ADULT CHANNELS, MOVIE CHANNELS,<br>
 SPORTING EVENTS, and other SPECIAL EVENTS.</font>&nbsp;</font></b></p>
<p align="center"><b><font size="4"> It also eliminates<br>
 interference problems on your television set while connected&nbsp;<br>
 to your broadband connection.  The Filter fits on the cable&nbsp;<br>
 between the wall jack and the converter box, NOTHING ELSE NEEDED!<br>
<br>
 It Doesn't Matter What Cable System you are on or what<br>
 type and model
 is the Following: You Must BE A Subscriber To Your Cable<br>
 Company's Digital Service and have a Digital Cable Box.</font></b></p>
<p align="center">&nbsp;<a href = "http://www.winningformula.bz/b/cable/index.php?149">CLICK HERE FOR MORE INFO</a>
 [&]
"""
},
{
'To': """richie@sundog.demon.co.uk""",
'Subject': """Richie Joint Problems?""",
'From': """"Lorene Harris" <paklmainzvawerkokddl.v@adm.nlh.no>""",
'Date': """Wed, 31 Dec 69 19:00:18 GMT""",
'Body': """8.8 percent increase in muscle mass.
Enhanced sexual performance.
Wrinkle reduction and smoother skin.
14.4 percent loss of fat.
Hair re-growth and strengthening.
Higher energy levels.
increased physical strength. Elimination of cellulite and excess fat.
Sharper vision. And much more ...
90 Day test!
This proven discovery has been reported on by the New England Journal of Medicine.
Forget aging and dieting forever. And it's Guaranteed
to take our Test
http://www.happyhealthyfun.biz/?f=cgi-bin/cp-app.cgi?rrc=N&pg=prod&ref=hgh&affl=lowride
gone
http://www.happyhealthyfun.biz/?f=delist.html"""
},
{
'To': """richie@entrian.com""",
'Subject': """wer them""",
'From': """"" <leighasteinbrecher@spannermail.com>""",
'Date': """Sat, 29 Nov 2003 00:07:40 +0400""",
'Body': """A recent survey by Nielsen/Netratings says that "The Internet
 population is rapidly approaching one Billion'people!"
 SO WHAT DOES THIS MEAN TO YOU?
 Let's assume that every person has only one E-mail address ..
 that's 500 million potential customers and growing! In addition,
 E-mail is without question the most powerful method of
 distributing information on the face of the earth.
 Well, I think you get the picture. The numbers and potential
 are just staggering, but it gets even better ..
 I am making a minimum of $3,000.00 a week sending email.
 Suppose I told you that you could start your own E-mail business
 today and enjoy these benefits:
   * All Customers Pay You In Cash!!!
   * You Will Sell A Product Which Costs
     Nothing to Produce!
   * Your Only Overhead is Your Time!
   * You Have 100s of Millions of Potential
     Customers!!!
   * You Get Detailed, Easy to Follow Startup
     Instructions!
 AND THIS IS JUST THE TIP OF THE ICEBERG .. As you read on you'll
 discover how this program that millions of people saw on TV is
 paying out a half million dollars, every 4 to 5 months for people
 working from home, for an investment of only $25 US Dollars expense,
 one time. ALL THANKS TO THE COMPUTER AGE .. AND THE INTERNET!
 If you want to find out how this works, simply read the REST of
 the story.. This HONESTLY WORKS, don't make the same mistake I
 made. I deleted this 4-5 times before finally giving it a try.
 Within 2 weeks the orders (money) started coming in just like
 the plan below said it would.
 Give it a try!! You will be glad you did.
 _________________________________
 First read about how a 15 year old made $71,000. See below....
 AS YOU SAW IT ON NATIONAL TV: This is the media report.
 *********************************************
 PARENTS OF 15 - YEAR OLD - FIND $71,000 CASH
 HIDDEN IN HIS CLOSET!
 *********************************************
 Does this headline look familiar? Of course it does.
 You most likely have just seen this story recently
 featured on a major nightly news program.
 His mother was cleaning and putting laundry away when she
 came across a large brown paper bag that was suspiciously
 buried beneath some clothes and a skateboard in the back
 of her 15-year-old sons closet.
 Nothing could have prepared her for the shock she got
 when she opened the bag and found it was full of cash.
 Five-dollar bills, twenties, fifties and hundreds -
 all neatly rubber-banded in labeled piles.
 "My first thought was that he had robbed a bank",
 says the 41-year-old woman, "There was over $71,000
 dollars in that bag -- that's more than my husband earns
 in a year". The woman immediately called her husband at the car-
 dealership where he worked to tell him what she had discovered.
 He came home right away and they drove together to the boys
 school and picked him up. Little did they suspect that where
 the money came from was more shocking than actually finding it
 in the closet.
 As it turns out, the boy had been sending out, via E-mail,
 a type of "Report" to E-mail addresses that he obtained off the
 Internet. Everyday after school for the past 2 months, he had
 been doing this right on his computer in his bedroom.
 "I just got the E-mail one day and I figured what the heck,
 I put my name on it like the instructions said and I started
 sending it out", says the clever 15-year-old.
 The E-mail letter listed 5 addresses and contained instructions
 to send one $5 dollar bill to each person on the list, then
 delete the address at the top and move the others addresses down,
 and finally to add your name to the top of the list.
 The letter goes on to state that you would receive several thousand
 dollars in five-dollar bills within 2 weeks if you sent out the
 letter with your name at the top of the 5-address list. "I get junk
 E-mail all the time, and really did not think it was going to work",
 the boy continues.
 Within the first few days of sending out the E-mail, the Post Office
 Box that his parents had gotten him for his video-game magazine
 subscriptions began to fill up with not magazines, but envelopes
 containing $5 bills.
 "About a week later I rode [my bike] down to the post office and my
 box had 1 magazine and about 300 envelops stuffed in it. There was
 also a yellow slip that said I had to go up to the [post office] counter.
 I thought I was in trouble or something (laughs)". He goes on, "I went
 up to the counter and they had a whole box of more mail for me.
 I had to ride back home and empty out my backpack because I could not
carry it all".
 Over the next few weeks, the boy continued sending out the E-mail.
 "The money just kept coming in and I just kept sorting it and
 stashing it in the closet, barely had time for my homework".
 He had also been riding his bike to several of the banks in his
 area and exchanging the $5 bills for twenties, fifties and hundreds.
 "I didn't want the banks to get suspicious so I kept riding to
 different banks with like five thousand at a time in my backpack.
 I would usually tell the lady at the bank counter that my dad had
 sent me in to exchange the money and he was outside waiting for me.
 One time the lady gave me a really strange look and told me that she
 would not be able to do it for me and my dad would have to come in
 and do it, but I just rode to the next bank down the street (laughs)."
 Surprisingly, the boy did not have any reason to be afraid. The
 reporting news team examined and investigated the so-called
 "chain-letter" the boy was sending out and found that it was not
 a chain-letter at all. In fact, it was completely legal according
 to US Postal and Lottery Laws, Title 18, Section 1302 and 1341, or
 Title 18, Section 3005 in the US code, also in the code of federal
 regulations, Volume 16, Sections 255 and 436, which state a
 product or service must be exchanged for money received.
 Every five-dollar bill that he received contained a little note
 that read, "Please send me report number XYX".This simple note
 made the letter legal because he was exchanging a service
 (A Report on how-to) for a five-dollar fee.
 [This is the end of the media release. If you would like to
 understand how the system works and get your $71,000 - please
 continue reading. What appears below is what the 15 year old
 was sending out on the net - YOU CAN USE IT TOO - just follow
 the simple instructions].
________________________________________________________
 This is the letter you have been hearing about on the news
 lately. Due to the popularity of this letter on the Internet,
 a national weekly news program recently devoted an entire
 show to the investigation of this program described below,
 to see if it really can make people money. The show also
 investigated whether or not the program was legal.
 Their findings proved once and for all that there are "absolutely
 NO Laws prohibiting the participation in the program and if
 people can follow the simple instructions, they are bound to
 make some mega-bucks with only $25 out of pocket cost".
 DUE TO THE RECENT INCREASE OF POPULARITY & RESPECT THIS PROGRAM
 HAS ATTAINED, IT IS CURRENTLY WORKING BETTER THAN EVER.
____________________________________________________________
 This is what one had to say: " Thanks to this profitable
 opportunity. I was approached many times before but each time
 I passed it up each time before this.
 I am so glad I finally joined just to see what one could
 expect in return for the minimal effort and money required.
 To my astonishment, I received a total of $610,470.00 in
 21 weeks, with money still coming in".
 Pam Hedland,
 Fort Lee,New Jersey.
 _______________________________________________________________
 Here is another testimonial: "This program has been around for a
 long time but I never believed in it. But one day when I received
 this again in the mail I decided to gamble my $25 on it. I followed
 the simple instructions and wallaa ..... 3 weeks later the money
 started to come in. First month I only made $240.00 but the next
 2 months after that made a total of $290,000.00. So far, in the past
 8 months by re-entering the program, I have made over $710,000.00
 and I am playing it again. The key to success in this program is to
 follow the simple steps and NOT change anything."
 __________________________________________________
   -- PRINT THIS NOW FOR YOUR FUTURE REFERENCE --
 FOLLOW THE SIMPLE INSTRUCTION BELOW AND YOUR FINANCIAL
 DREAMS WILL COME TRUE, GUARANTEED!
__________________________________________________
                  INSTRUCTIONS
  ---- Order all 5 reports shown on the list below ----
 For each report, send $5 CASH, THE NAME & NUMBER OF THE REPORT
 YOU ARE ORDERING and YOUR E-MAIL ADDRESS to the person whose
 name appears ON THAT LIST next to the report. MAKE SURE YOUR
 RETURN ADDRESS IS ON YOUR ENVELOPE TOP LEFT CORNER in case of
 any mail problems.
 -- When you place your order, make sure you order each of the
 5 reports.
 -- You will need all 5 reports so that you can save them on your
 computer and resell them. YOUR TOTAL COST $5 X 5=$25.00.
 Within a few days you will receive, via e-mail, each of the 5 reports
 from these 5 different individuals. Save them on your computer so
 they will be accessible for you to send to the 1,000's of people who
 will order them from you. Also make a floppy of these reports and
 keep it on your desk in case something happens to your computer.
 IMPORTANT - DO NOT alter the names of the people who are listed
 next to each report, or their sequence on the list, in any way other
 than what is instructed below in step " 1 through 6 " or you will
 loose out on majority of your profits. Once you understand the way
 this works, you will also see how it does not work if you change it.
 Remember, this method has been tested, and if you alter, it will
 NOT work !!!
 People have tried to put their friends/relatives names on all five
 thinking they could get all the money. But it does not work this way.
 Believe us, we all have tried to be greedy and then nothing happened.
 So Do Not try to change anything other than what is instructed.
 Because if you do, it will not work for you.
 Remember, honesty reaps the reward!!!
 1.... After you have ordered all 5 reports, take this advertisement
 and REMOVE the name & address of the person in REPORT 
 This person has made it through the cycle and is no doubt counting
 their fortune.
 2.... Move the name & address in REPORT 
 3.... Move the name & address in REPORT 
 4.... Move the name & address in REPORT 
 5.... Move the name & address in REPORT 
 6.... Insert YOUR name & address in the REPORT 
     -- PLEASE PRINT every name & address clearly --
 ____________________________________________________________
 -- Take this entire letter, with the modified list of names,
 and save it on your computer. DO NOT MAKE ANY OTHER CHANGES.
 Save this on a disk as well just in case you loose any data. To
 assist you with marketing your business on the internet, the 5
 reports you purchase will provide you with invaluable marketing
 information which includes how to send bulk e-mails legally,
 where to find thousands of free classified ads and much more.
 There are 2 Primary methods to get this venture going:
 METHOD 
 LEGALLY
 ______________________________________________________________
 Let's say that you decide to start small, just to see how it goes,
 and we will assume You and those involved send out only 5,000
 e-mails each.
 Let's also assume that the mailing receive only a 0.2% response
 (the response could be much better but lets just say it is only
 0.2%. Also many people will send out hundreds of thousands e-mails
 instead of only 5,000 each).Continuing with this example, you send
 out only 5,000 e-mails.
 With a 0.2% response, that is only 10 orders for report 
 Those 10 people responded by sending out 5,000 e-mail each for a
 total of 50,000.
 Out of those 50,000 e-mails only 0.2% responded with orders.
 That's=100 people responded and ordered Report 
 Those 100 people mail out 5,000 e-mails each for a total of 500,000
 e-mails. The 0.2% response to that is 1000 orders for Report 
 Those 1000 people send out 5,000 e-mails each for a total of 5
 million e-mails sent out. The 0.2% response to that is 10,000 orders
 for Report! 
 a total of 50,000,000 (50 million) e-mails. The 0.2% response to that
 is 100,000 orders for Report 
 EACH=$500,000.00 (half million).
 Your total income in this example is: 1..... $50 + 2..... $500
 + 3.....$5,000 + 4..... $50,000 + 5..... $500,000 ........ Grand
 Total=$555,550.00
 NUMBERS DO NOT LIE. GET A PENCIL & PAPER AND FIGURE OUT THE
 WORST POSSIBLERESPONSES AND NO MATTER HOW YOU CALCULATE IT,
 YOU WILL STILL MAKE A LOT OF MONEY !
 ____________________________________________________________
 REMEMBER FRIEND, THIS IS ASSUMING ONLY 10 PEOPLE ORDERING OUT
 OF 5,000 YOU MAILED TO. Dare to think for a moment what would
 happen if everyone or half or even one 4th of those people mailed
 100,000 e-mails each or more? There are over 150 million people on
 the Internet worldwide and counting. Believe me, many people will
 do just that, and more!
 METHOD 
 INTERNET
 _____________________________________________________________
 Advertising on the net is very very inexpensive and there are
 hundreds of FREE places to advertise. Placing a lot of free ads on
 the Internet will easily get a larger response. We strongly suggest
 you start with Method 
 every $5 you receive, all you must do is e-mail them the Report
 they ordered.
 That's it. Always provide same day service on all orders.
 This will guarantee that the e-mail they send out, with your
 name and address on it, will be prompt because they can not advertise
 until they receive the report.
 ------------------ AVAILABLE REPORTS --------------------
 ORDER EACH REPORT BY ITS NUMBER & NAME ONLY. Notes: Always send
 $5 cash (U.S. CURRENCY) for each Report. Checks NOT accepted. Make
 sure the cash is concealed by wrapping it in at least 2 sheets of paper.
 On one of those sheets of paper, Write the NUMBER & the NAME of the
 Report you are ordering, YOUR E-MAIL ADDRESS and your name and postal
 address.
 PLACE YOUR ORDER FOR THESE REPORTS NOW.
____________________________________________________
 REPORT 
Order Report 
Sean Lockman
3946 SW Pendleton St.
Portland, OR 97221
USA
_______________________________________________________________
REPORT 
Order Report 
Steve Hunter
10314 Southport
Houston, Texas  77089
USA
_________________________________________________________
REPORT 
Order Report 
Angela Friermuth
6804 112th Ave East
Puyallup, WA 98372
USA
_______________________________________________________________
REPORT 
Order Report 
M.W.T.
PMB 285
344-5 RT 9
Lanoka Harbor, NJ  08734
USA
_________________________________________________
REPORT 
Order Report 
Thomas C. Makowski
501 Harding Ave
Bayville, NJ  08721
USA
__________________________________________________________________
 *********** YOUR SUCCESS GUIDELINES ***********
 Follow these guidelines to guarantee your success:
 -- If you do not receive at least 10 orders for Report 
 weeks, continue sending e-mails until you do.
 -- After you have received 10 orders, 2 to 3 weeks after that you
 should receive 100 orders or more for REPORT 
 continue advertising or sending e-mails until you do.
 -- Once you have received 100 or more orders for Report 
 RELAX, because the system is already working for you, and the cash
 will continue to roll in ! THIS IS IMPORTANT TO REMEMBER: Every time
 your name is moved down on the list, you are placed in front of a
 Different report.
 You can KEEP TRACK of your PROGRESS by watching which report
 people are ordering from you. IF YOU WANT TO GENERATE MORE INCOME
 SEND ANOTHER BATCH OF E-MAILS AND START THE WHOLE PROCESS AGAIN.
 There is NO LIMIT to the income you can generate from this
 business!
 ______________________________________________________
 FOLLOWING IS A NOTE FROM THE ORIGINATOR OF THIS PROGRAM:
 You have just received information that can give you financial
 freedom for the rest of your life, with NO RISK and JUST A LITTLE
 BIT OF EFFORT.
 You can make more money in the next few weeks and months than you
 have ever imagined. Follow the program EXACTLY AS INSTRUCTED.
 Do Not change it in any way. It works exceedingly well as it is now.
 Remember to e-mail a copy of this exciting report after you have put
 your name and address in Report 
 to may send out 100,000 or more e-mails and your name will be on every
 one of them.
 Remember though, the more you send out the more potential customers
 you will reach.
 ___________________________________________________________________
 So my friend, I have given you the ideas, information, materials and
 tools. IT IS UP TO YOU NOW !
 ORDER YOUR REPORTS TODAY AND GET STARTED ON YOUR ROAD
 TO FREEDOM.
 THERE IS NO NEED TO RESPOND TO THIS E-MAIL IF YOU DO NOT WISH TO
 RECEIVE FURTHER CORRESPONDENCE. THIS IS A ONETIME E-MAIL.
 Happy Mailing!
 ______________"""
} ]
if __name__ == '__main__':
    hammer()
