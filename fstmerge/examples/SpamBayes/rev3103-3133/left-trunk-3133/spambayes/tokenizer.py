"""Module to tokenize email messages for spam filtering."""
from __future__ import generators
import email
import email.Message
import email.Header
import email.Utils
import email.Errors
import re
import math
import time
import os
import binascii
import urlparse
import urllib
try:
    Set = set
except NameError:
    try:
        from sets import Set
    except ImportError:
        from spambayes.compatsets import Set
from spambayes import classifier
from spambayes.Options import options
from spambayes.mboxutils import get_message
try:
    True, False
except NameError:
    True, False = 1, 0
try:
    import dnscache
    cache = dnscache.cache(cachefile=options["Tokenizer", "lookup_ip_cache"])
    cache.printStatsAtEnd = False
except (IOError, ImportError):
    cache = None
else:
    import atexit
    atexit.register(cache.close)
from encodings.aliases import aliases 
if not aliases.has_key('ansi_x3_4_1968'):
    aliases['ansi_x3_4_1968'] = 'ascii'
del aliases 
def textparts(msg):
    """Return a set of all msg parts with content maintype 'text'."""
    return Set(filter(lambda part: part.get_content_maintype() == 'text',
                      msg.walk()))
def octetparts(msg):
    """Return a set of all msg parts with type 'application/octet-stream'."""
    return Set(filter(lambda part:
                      part.get_content_type() == 'application/octet-stream',
                      msg.walk()))
def imageparts(msg):
    """Return a list of all msg parts with type 'image/*'."""
    return filter(lambda part:
                  part.get_content_type().startswith('image/'),
                  msg.walk())
has_highbit_char = re.compile(r"[\x80-\xff]").search
html_re = re.compile(r"""
    <
    (?![\s<>])  
    [^>]{0,256} 
    >
""", re.VERBOSE | re.DOTALL)
received_host_re = re.compile(r'from ([a-z0-9._-]+[a-z])[)\s]')
received_ip_re = re.compile(r'[[(]((\d{1,3}\.?){4})[])]')
message_id_re = re.compile(r'\s*<[^@]+@([^>]+)>\s*')
subject_word_re = re.compile(r"[\w\x80-\xff$.%]+")
punctuation_run_re = re.compile(r'\W+')
fname_sep_re = re.compile(r'[/\\:]')
def crack_filename(fname):
    yield "fname:" + fname
    components = fname_sep_re.split(fname)
    morethan1 = len(components) > 1
    for component in components:
        if morethan1:
            yield "fname comp:" + component
        pieces = urlsep_re.split(component)
        if len(pieces) > 1:
            for piece in pieces:
                yield "fname piece:" + piece
def tokenize_word(word, _len=len, maxword=options["Tokenizer",
                                                  "skip_max_word_size"]):
    n = _len(word)
    if 3 <= n <= maxword:
        yield word
    elif n >= 3:
        if n < 40 and '.' in word and word.count('@') == 1:
            p1, p2 = word.split('@')
            yield 'email name:' + p1
            yield 'email addr:' + p2
        else:
            if options["Tokenizer", "generate_long_skips"]:
                yield "skip:%c %d" % (word[0], n // 10 * 10)
            if has_highbit_char(word):
                hicount = 0
                for i in map(ord, word):
                    if i >= 128:
                        hicount += 1
                yield "8bit%%:%d" % round(hicount * 100.0 / len(word))
non_ascii_translate_tab = ['?'] * 256
for i in range(32, 127):
    non_ascii_translate_tab[i] = chr(i)
for ch in ' \t\r\n':
    non_ascii_translate_tab[ord(ch)] = ch
del i, ch
non_ascii_translate_tab = ''.join(non_ascii_translate_tab)
def crack_content_xyz(msg):
    yield 'content-type:' + msg.get_content_type()
    x = msg.get_param('type')
    if x is not None:
        yield 'content-type/type:' + x.lower()
    try:
        for x in msg.get_charsets(None):
            if x is not None:
                yield 'charset:' + x.lower()
    except UnicodeEncodeError:
        yield 'charset:invalid_unicode'
    x = msg.get('content-disposition')
    if x is not None:
        yield 'content-disposition:' + x.lower()
    try:
        fname = msg.get_filename()
        if fname is not None:
            for x in crack_filename(fname):
                yield 'filename:' + x
    except TypeError:
        yield "filename:<bogus>"
    if 0:   
        x = msg.get('content-transfer-encoding')
        if x is not None:
            yield 'content-transfer-encoding:' + x.lower()
base64_re = re.compile(r"""
    [ \t]*
    [a-zA-Z0-9+/]*
    (=*)
    [ \t]*
    \r?
    \n
""", re.VERBOSE)
def try_to_repair_damaged_base64(text):
    i = 0
    while True:
        m = base64_re.match(text, i)
        if not m:
            break
        i = m.end()
        if m.group(1):
            break
    base64text = ''
    if i:
        base64 = text[:i]
        try:
            base64text = binascii.a2b_base64(base64)
        except:
            pass
    return base64text + text[i:]
def breakdown_host(host):
    parts = host.split('.')
    for i in range(1, len(parts) + 1):
        yield '.'.join(parts[-i:])
def breakdown_ipaddr(ipaddr):
    parts = ipaddr.split('.')
    for i in range(1, 5):
        yield '.'.join(parts[:i])
def log2(n, log=math.log, c=math.log(2)):
    return log(n)/c
class Stripper(object):
    separator = ''  
    def __init__(self, find_start, find_end):
        self.find_start = find_start
        self.find_end = find_end
    def analyze(self, text):
        i = 0
        retained = []
        pushretained = retained.append
        tokens = []
        while True:
            m = self.find_start(text, i)
            if not m:
                pushretained(text[i:])
                break
            start, end = m.span()
            pushretained(text[i : start])
            tokens.extend(self.tokenize(m))
            m = self.find_end(text, end)
            if not m:
                pushretained(text[start:])
                break
            dummy, i = m.span()
        return self.separator.join(retained), tokens
    def tokenize(self, match_object):
        return []
uuencode_begin_re = re.compile(r"""
    ^begin \s+
    (\S+) \s+   
    (\S+) \s*   
    $
""", re.VERBOSE | re.MULTILINE)
uuencode_end_re = re.compile(r"^end\s*\n", re.MULTILINE)
class UUencodeStripper(Stripper):
    def __init__(self):
        Stripper.__init__(self, uuencode_begin_re.search,
                                uuencode_end_re.search)
    def tokenize(self, m):
        mode, fname = m.groups()
        return (['uuencode mode:%s' % mode] +
                ['uuencode:%s' % x for x in crack_filename(fname)])
crack_uuencode = UUencodeStripper().analyze
url_fancy_re = re.compile(r""" 
    \b                      
    (?: 
        (?:
            (https? | ftp)  
            ://             
        )|
        (?= ftp\.[^\.\s<>"'\x7f-\xff] )|  
        (?= www\.[^\.\s<>"'\x7f-\xff] )   
    )
    ([^\s<>"'\x7f-\xff]+)  
""", re.VERBOSE)                        
url_re = re.compile(r"""
    (https? | ftp)  
    ://             
    ([^\s<>"'\x7f-\xff]+)  
""", re.VERBOSE)                        
urlsep_re = re.compile(r"[;?:@&=+,$.]")
class URLStripper(Stripper):
    def __init__(self):
        if options["Tokenizer", "x-fancy_url_recognition"]:
            search = url_fancy_re.search
        else:
            search = url_re.search
        Stripper.__init__(self, search, re.compile("").search)
    def tokenize(self, m):
        proto, guts = m.groups()
        assert guts
        if proto is None:
            if guts.lower().startswith("www"):
                proto = "http"
            elif guts.lower().startswith("ftp"):
                proto = "ftp"
            else:
                proto = "unknown"
        tokens = ["proto:" + proto]
        pushclue = tokens.append
        if options["Tokenizer", "x-pick_apart_urls"]:
            url = proto + "://" + guts
            escapes = re.findall(r'%..', guts)
            if escapes:
                pushclue("url:%%%d" % int(log2(len(escapes))))
            tokens.extend(["url:" + escape for escape in escapes])
            url = urllib.unquote(url)
            scheme, netloc, path, params, query, frag = urlparse.urlparse(url)
            if cache is not None and options["Tokenizer", "x-lookup_ip"]:
                ips=cache.lookup(netloc)
                if not ips:
                    pushclue("url-ip:lookup error")
                else:
                    for ip in ips: 
                        pushclue("url-ip:%s/32" % ip)
                        dottedQuadList=ip.split(".")
                        pushclue("url-ip:%s/8" % dottedQuadList[0])
                        pushclue("url-ip:%s.%s/16" % (dottedQuadList[0],
                                                      dottedQuadList[1]))
                        pushclue("url-ip:%s.%s.%s/24" % (dottedQuadList[0],
                                                         dottedQuadList[1],
                                                         dottedQuadList[2]))
            user_pwd, host_port = urllib.splituser(netloc)
            if user_pwd is not None:
                pushclue("url:has user")
            host, port = urllib.splitport(host_port)
            if port is not None:
                if (scheme == "http" and port != '80' or
                    scheme == "https" and port != '443'):
                    pushclue("url:non-standard %s port" % scheme)
            if re.match("(\d+\.?){4,4}$", host) is not None:
                pushclue("url:ip addr")
            proto, guts = url.split("://", 1)
        while guts and guts[-1] in '.:?!/':
            guts = guts[:-1]
        for piece in guts.split('/'):
            for chunk in urlsep_re.split(piece):
                pushclue("url:" + chunk)
        return tokens
received_complaints_re = re.compile(r'\([a-z]+(?:\s+[a-z]+)+\)')
class SlurpingURLStripper(URLStripper):
    def __init__(self):
        URLStripper.__init__(self)
    def analyze(self, text):
        classifier.slurp_wordstream = None
        return URLStripper.analyze(self, text)
    def tokenize(self, m):
        tokens = URLStripper.tokenize(self, m)
        if not options["URLRetriever", "x-slurp_urls"]:
            return tokens
        proto, guts = m.groups()
        if proto != "http":
            return tokens
        assert guts
        while guts and guts[-1] in '.:;?!/)':
            guts = guts[:-1]
        classifier.slurp_wordstream = (proto, guts)
        return tokens
if options["URLRetriever", "x-slurp_urls"]:
    crack_urls = SlurpingURLStripper().analyze
else:
    crack_urls = URLStripper().analyze
html_style_start_re = re.compile(r"""
    < \s* style\b [^>]* >
""", re.VERBOSE)
class StyleStripper(Stripper):
    def __init__(self):
        Stripper.__init__(self, html_style_start_re.search,
                                re.compile(r"</style>").search)
crack_html_style = StyleStripper().analyze
class CommentStripper(Stripper):
    def __init__(self):
        Stripper.__init__(self,
                          re.compile(r"<!--|<\s*comment\s*[^>]*>").search,
                          re.compile(r"-->|</comment>").search)
crack_html_comment = CommentStripper().analyze
class NoframesStripper(Stripper):
    def __init__(self):
        Stripper.__init__(self,
                          re.compile(r"<\s*noframes\s*>").search,
                          re.compile(r"</noframes\s*>").search)
crack_noframes = NoframesStripper().analyze
virus_re = re.compile(r"""
    < /? \s* (?: script | iframe) \b
|   \b src= ['"]? cid:
|   \b (?: height | width) = ['"]? 0
""", re.VERBOSE)                        
def find_html_virus_clues(text):
    for bingo in virus_re.findall(text):
        yield bingo
numeric_entity_re = re.compile(r'&#(\d+);')
def numeric_entity_replacer(m):
    try:
        return chr(int(m.group(1)))
    except:
        return '?'
breaking_entity_re = re.compile(r"""
    &nbsp;
|   < (?: p
      |   br
      )
    >
""", re.VERBOSE)
class Tokenizer:
    date_hms_re = re.compile(r' (?P<hour>[0-9][0-9])'
                             r':(?P<minute>[0-9][0-9])'
                             r'(?::[0-9][0-9])? ')
    date_formats = ("%a, %d %b %Y %H:%M:%S (%Z)",
                    "%a, %d %b %Y %H:%M:%S %Z",
                    "%d %b %Y %H:%M:%S (%Z)",
                    "%d %b %Y %H:%M:%S %Z",
                    "%a, %d %b %Y %H:%M (%Z)",
                    "%a, %d %b %Y %H:%M %Z",
                    "%d %b %Y %H:%M (%Z)",
                    "%d %b %Y %H:%M %Z")
    def __init__(self):
        self.setup()
    def setup(self):
        """Get the tokenizer ready to use; this should be called after
        all options have been set."""
        if options["Tokenizer", "basic_header_tokenize"]:
            self.basic_skip = [re.compile(s)
                               for s in options["Tokenizer",
                                                "basic_header_skip"]]
    def get_message(self, obj):
        return get_message(obj)
    def tokenize(self, obj):
        msg = self.get_message(obj)
        for tok in self.tokenize_headers(msg):
            yield tok
        for tok in self.tokenize_body(msg):
            yield tok
    def tokenize_headers(self, msg):
        for x in msg.walk():
            for w in crack_content_xyz(x):
                yield w
        if options["Tokenizer", "basic_header_tokenize"]:
            for k, v in msg.items():
                k = k.lower()
                for rx in self.basic_skip:
                    if rx.match(k):
                        break   
                else:
                    for w in subject_word_re.findall(v):
                        for t in tokenize_word(w):
                            yield "%s:%s" % (k, t)
            if options["Tokenizer", "basic_header_tokenize_only"]:
                return
        if options["Tokenizer", "x-search_for_habeas_headers"]:
            habeas_headers = [
("X-Habeas-SWE-1", "winter into spring"),
("X-Habeas-SWE-2", "brightly anticipated"),
("X-Habeas-SWE-3", "like Habeas SWE (tm)"),
("X-Habeas-SWE-4", "Copyright 2002 Habeas (tm)"),
("X-Habeas-SWE-5", "Sender Warranted Email (SWE) (tm). The sender of this"),
("X-Habeas-SWE-6", "email in exchange for a license for this Habeas"),
("X-Habeas-SWE-7", "warrant mark warrants that this is a Habeas Compliant"),
("X-Habeas-SWE-8", "Message (HCM) and not spam. Please report use of this"),
("X-Habeas-SWE-9", "mark in spam to <http://www.habeas.com/report/>.")
            ]
            valid_habeas = 0
            invalid_habeas = False
            for opt, val in habeas_headers:
                habeas = msg.get(opt)
                if habeas is not None:
                    if options["Tokenizer", "x-reduce_habeas_headers"]:
                        if habeas == val:
                            valid_habeas += 1
                        else:
                            invalid_habeas = True
                    else:
                        if habeas == val:
                            yield opt.lower() + ":valid"
                        else:
                            yield opt.lower() + ":invalid"
            if options["Tokenizer", "x-reduce_habeas_headers"]:
                if invalid_habeas == True:
                    yield "x-habeas-swe:invalid"
                elif valid_habeas == 9:
                    yield "x-habeas-swe:valid"
        x = msg.get('subject', '')
        try:
            subjcharsetlist = email.Header.decode_header(x)
        except (binascii.Error, email.Errors.HeaderParseError):
            subjcharsetlist = [(x, 'invalid')]
        for x, subjcharset in subjcharsetlist:
            if subjcharset is not None:
                yield 'subjectcharset:' + subjcharset
            x = x.replace('\r', ' ')
            for w in subject_word_re.findall(x):
                for t in tokenize_word(w):
                    yield 'subject:' + t
            for w in punctuation_run_re.findall(x):
                yield 'subject:' + w
        for field in options["Tokenizer", "address_headers"]:
            addrlist = msg.get_all(field, [])
            if not addrlist:
                yield field + ":none"
                continue
            noname_count = 0
            for name, addr in email.Utils.getaddresses(addrlist):
                if name:
                    try:
                        subjcharsetlist = email.Header.decode_header(name)
                    except (binascii.Error, email.Errors.HeaderParseError):
                        subjcharsetlist = [(name, 'invalid')]
                    for name, charset in subjcharsetlist:
                        yield "%s:name:%s" % (field, name.lower())
                        if charset is not None:
                            yield "%s:charset:%s" % (field, charset)
                else:
                    noname_count += 1
                if addr:
                    for w in addr.lower().split('@'):
                        yield "%s:addr:%s" % (field, w)
                else:
                    yield field + ":addr:none"
            if noname_count:
                yield "%s:no real name:2**%d" % (field,
                                                 round(log2(noname_count)))
        if options["Tokenizer", "summarize_email_prefixes"]:
            all_addrs = []
            addresses = msg.get_all('to', []) + msg.get_all('cc', [])
            for name, addr in email.Utils.getaddresses(addresses):
                all_addrs.append(addr.lower())
            if len(all_addrs) > 1:
                pfx = os.path.commonprefix(all_addrs)
                if pfx:
                    score = (len(pfx) * len(all_addrs)) // 10
                    if score > 3:
                        yield "pfxlen:big"
                    else:
                        yield "pfxlen:%d" % score
        if options["Tokenizer", "summarize_email_suffixes"]:
            all_addrs = []
            addresses = msg.get_all('to', []) + msg.get_all('cc', [])
            for name, addr in email.Utils.getaddresses(addresses):
                addr = list(addr)
                addr.reverse()
                addr = "".join(addr)
                all_addrs.append(addr.lower())
            if len(all_addrs) > 1:
                sfx = os.path.commonprefix(all_addrs)
                if sfx:
                    score = (len(sfx) * len(all_addrs)) // 10
                    if score > 5:
                        yield "sfxlen:big"
                    else:
                        yield "sfxlen:%d" % score
        for field in ('to', 'cc'):
            count = 0
            for addrs in msg.get_all(field, []):
                count += len(addrs.split(','))
            if count > 0:
                yield '%s:2**%d' % (field, round(log2(count)))
        for field in ('x-mailer',):
            prefix = field + ':'
            x = msg.get(field, 'none').lower()
            yield prefix + ' '.join(x.split())
        if options["Tokenizer", "mine_received_headers"]:
            for header in msg.get_all("received", ()):
                header = ' '.join(header.split()).lower()
                for clue in received_complaints_re.findall(header):
                    yield 'received:' + clue
                for pat, breakdown in [(received_host_re, breakdown_host),
                                       (received_ip_re, breakdown_ipaddr)]:
                    m = pat.search(header)
                    if m:
                        for tok in breakdown(m.group(1)):
                            yield 'received:' + tok
        msgid = msg.get("message-id", "")
        m = message_id_re.match(msgid)
        if m:
            yield 'message-id:@%s' % m.group(1)
        else:
            yield 'message-id:invalid'
        x2n = {}
        if options["Tokenizer", "count_all_header_lines"]:
            for x in msg.keys():
                x2n[x] = x2n.get(x, 0) + 1
        else:
            safe_headers = options["Tokenizer", "safe_headers"]
            for x in msg.keys():
                if x.lower() in safe_headers:
                    x2n[x] = x2n.get(x, 0) + 1
        for x in x2n.items():
            yield "header:%s:%d" % x
        if options["Tokenizer", "record_header_absence"]:
            for k in x2n:
                if not k.lower() in options["Tokenizer", "safe_headers"]:
                    yield "noheader:" + k
    def tokenize_text(self, text, maxword=options["Tokenizer",
                                                  "skip_max_word_size"]):
        """Tokenize everything in the chunk of text we were handed."""
        short_runs = Set()
        short_count = 0
        for w in text.split():
            n = len(w)
            if n < 3:
                short_count += 1
            else:
                if short_count:
                    short_runs.add(short_count)
                    short_count = 0
                if 3 <= n <= maxword:
                    yield w
                elif n >= 3:
                    for t in tokenize_word(w):
                        yield t
        if short_runs and options["Tokenizer", "x-short_runs"]:
            yield "short:%d" % int(log2(max(short_runs)))
    def tokenize_body(self, msg):
        """Generate a stream of tokens from an email Message.
        If options['Tokenizer', 'check_octets'] is True, the first few
        undecoded characters of application/octet-stream parts of the
        message body become tokens.
        """
        if options["Tokenizer", "check_octets"]:
            for part in octetparts(msg):
                try:
                    text = part.get_payload(decode=True)
                except:
                    yield "control: couldn't decode octet"
                    text = part.get_payload(decode=False)
                if text is None:
                    yield "control: octet payload is None"
                    continue
                yield "octet:%s" % text[:options["Tokenizer",
                                                 "octet_prefix_size"]]
        parts = imageparts(msg)
        if options["Tokenizer", "image_size"]:
            total_len = 0
            for part in parts:
                try:
                    text = part.get_payload(decode=True)
                except:
                    yield "control: couldn't decode image"
                    text = part.get_payload(decode=False)
                total_len += len(text or "")
                if text is None:
                    yield "control: image payload is None"
            if total_len:
                yield "image-size:2**%d" % round(log2(total_len))
        if options["Tokenizer", "crack_images"]:
            engine_name = options["Tokenizer", 'ocr_engine']
            from spambayes.ImageStripper import crack_images
            text, tokens = crack_images(engine_name, parts)
            for t in tokens:
                yield t
            for t in self.tokenize_text(text):
                yield t
        for part in textparts(msg):
            try:
                text = part.get_payload(decode=True)
            except:
                yield "control: couldn't decode"
                text = part.get_payload(decode=False)
                if text is not None:
                    text = try_to_repair_damaged_base64(text)
            if text is None:
                yield 'control: payload is None'
                continue
            text = numeric_entity_re.sub(numeric_entity_replacer, text)
            text = text.lower()
            if options["Tokenizer", "replace_nonascii_chars"]:
                text = text.translate(non_ascii_translate_tab)
            for t in find_html_virus_clues(text):
                yield "virus:%s" % t
            for cracker in (crack_uuencode,
                            crack_urls,
                            crack_html_style,
                            crack_html_comment,
                            crack_noframes):
                text, tokens = cracker(text)
                for t in tokens:
                    yield t
            text = breaking_entity_re.sub(' ', text)
            text = html_re.sub('', text)
            for t in self.tokenize_text(text):
                yield t
global_tokenizer = Tokenizer()
tokenize = global_tokenizer.tokenize
