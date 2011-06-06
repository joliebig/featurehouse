"""
This is the place where we try and discover information buried in images.
"""
from __future__ import division
import sys
import os
import tempfile
import math
import time
import md5
import atexit
try:
    import cPickle as pickle
except ImportError:
    import pickle
try:
    import cStringIO as StringIO
except ImportError:
    import StringIO
try:
    from PIL import Image, ImageSequence
except ImportError:
    Image = None
image_large_size_attribute = "spambayes_image_large_size"
try:
    Set = set
except NameError:
    try:
        from sets import Set
    except ImportError:
        from spambayes.compatsets import Set
from spambayes.Options import options
def log2(n, log=math.log, c=math.log(2)):
    return log(n)/c
def is_executable(prog):
    if sys.platform == "win32":
        return True
    info = os.stat(prog)
    return (info.st_uid == os.getuid() and (info.st_mode & 0100) or
            info.st_gid == os.getgid() and (info.st_mode & 0010) or
            info.st_mode & 0001)
def find_program(prog):
    path = os.environ.get("PATH", "").split(os.pathsep)
    if sys.platform == "win32":
        prog = "%s.exe" % prog
        if hasattr(sys, "frozen"): # a binary (py2exe) build..
            if sys.frozen=="dll":
                import win32api
                sentinal = win32api.GetModuleFileName(sys.frozendllhandle)
            else:
                sentinal = sys.executable
            path=[win32api.GetShortPathName(os.path.dirname(sentinal))]
        else:
            import spambayes
            path.insert(0, os.path.abspath(spambayes.__path__[0]))
    for directory in path:
        program = os.path.join(directory, prog)
        if os.path.exists(program) and is_executable(program):
            return program
    return ""
def imconcatlr(left, right):
    """Concatenate two images left to right."""
    w1, h1 = left.size
    w2, h2 = right.size
    result = Image.new("RGB", (w1 + w2, max(h1, h2)))
    result.paste(left, (0, 0))
    result.paste(right, (w1, 0))
    return result
def imconcattb(upper, lower):
    """Concatenate two images top to bottom."""
    w1, h1 = upper.size
    w2, h2 = lower.size
    result = Image.new("RGB", (max(w1, w2), h1 + h2))
    result.paste(upper, (0, 0))
    result.paste(lower, (0, h1))
    return result
def PIL_decode_parts(parts):
    """Decode and assemble a bunch of images using PIL."""
    tokens = Set()
    rows = []
    max_image_size = options["Tokenizer", "max_image_size"]
    for part in parts:
        nbytes = getattr(part, image_large_size_attribute, None)
        if nbytes is None: # no optimization - process normally...
            try:
                bytes = part.get_payload(decode=True)
                nbytes = len(bytes)
            except:
                tokens.add("invalid-image:%s" % part.get_content_type())
                continue
        else:
            assert nbytes > max_image_size, (len(bytes), max_image_size)
        if nbytes > max_image_size:
            tokens.add("image:big")
            continue                # assume it's just a picture for now
        try:
            image = Image.open(StringIO.StringIO(bytes))
            image.load()
        except IOError:
            tokens.add("invalid-image:%s" % part.get_content_type())
            continue
        else:
            if "duration" in image.info:
                bgpix = 1e17           # ridiculously large number of pixels
                try:
                    for frame in ImageSequence.Iterator(image):
                        bg = max(frame.histogram())
                        if bg < bgpix:
                            image = frame
                            bgpix = bg
                except IOError:
                    tokens.add("invalid-image:%s" % part.get_content_type())
                    continue
                except ValueError:
                    pass
            image = image.convert("RGB")
        if not rows:
            rows.append(image)
        elif image.size[1] != rows[-1].size[1]:
            rows.append(image)
        else:
            rows[-1] = imconcatlr(rows[-1], image)
    if not rows:
        return [], tokens
    full_image, rows = rows[0], rows[1:]
    for image in rows:
        full_image = imconcattb(full_image, image)
    fd, pnmfile = tempfile.mkstemp('-spambayes-image')
    os.close(fd)
    full_image.save(open(pnmfile, "wb"), "PPM")
    return [pnmfile], tokens
class OCREngine(object):
    """Base class for an OCR "engine" that extracts text.  Ideally would
       also deal with image format (as different engines will have different
       requirements), but all currently supported ones deal with the PNM
       formats (ppm/pgm/pbm)
    """
    engine_name = None # sub-classes should override.
    def __init__(self):
        pass
    def is_enabled(self):
        """Return true if this engine is able to be used.  Note that
           returning true only means it is *capable* of being used - not that
           it is enabled.  eg, it should check the program is needs to use
           is installed, etc.
        """
        raise NotImplementedError
    def extract_text(self, pnmfiles):
        """Extract the text as an unprocessed stream (but as a string).
           Typically this will be the raw output from the OCR engine.
        """
        raise NotImplementedError
class OCRExecutableEngine(OCREngine):
    """Uses a simple executable that writes to stdout to extract the text"""
    engine_name = None
    def __init__(self):
        self._program = None
        OCREngine.__init__(self)
    def is_enabled(self):
        return self.program is not None
    def get_program(self):
        if not self._program:
            self._program = find_program(self.engine_name)
        return self._program
    program = property(get_program)
    def get_command_line(self, pnmfile):
        raise NotImplementedError, "base classes must override"
    def extract_text(self, pnmfile):
        assert self.is_enabled(), "I'm not working!"
        cmdline = self.get_command_line(pnmfile)
        ocr = os.popen(cmdline)
        ret = ocr.read()
        exit_code = ocr.close()
        if exit_code:
            print "warning:", self.engine_name, "failed with exit code", exit_code
            print "command line was:", repr(cmdline)
        return ret
class OCREngineOCRAD(OCRExecutableEngine):
    engine_name = "ocrad"
    def get_command_line(self, pnmfile):
        scale = options["Tokenizer", "ocrad_scale"] or 1
        charset = options["Tokenizer", "ocrad_charset"]
        return '%s -s %s -c %s -f "%s" 2>%s' % \
                (self.program, scale, charset, pnmfile, os.path.devnull)
class OCREngineGOCR(OCRExecutableEngine):
    engine_name="gocr"
    def get_command_line(self, pnmfile):
        return '%s "%s" 2>%s' % (self.program, pnmfile, os.path.devnull)
_ocr_engines = [
    OCREngineGOCR,
    OCREngineOCRAD,
]
def get_engine(engine_name):
    if not engine_name:
        candidates = _ocr_engines
    else:
        for e in _ocr_engines:
            if e.engine_name == engine_name:
                candidates = [e]
                break
        else:
            candidates = []
    for candidate in candidates:
        engine = candidate()
        if engine.is_enabled():
            return engine
    return None
class ImageStripper:
    def __init__(self, cachefile=""):
        self.cachefile = os.path.expanduser(cachefile)
        if os.path.exists(self.cachefile):
            self.cache = pickle.load(open(self.cachefile))
        else:
            self.cache = {}
        self.misses = self.hits = 0
        if self.cachefile:
            atexit.register(self.close)
        self.engine = None
    def extract_ocr_info(self, pnmfiles):
        assert self.engine, "must have an engine!"
        textbits = []
        tokens = Set()
        for pnmfile in pnmfiles:
            fhash = md5.new(open(pnmfile).read()).hexdigest()
            if fhash in self.cache:
                self.hits += 1
                ctext, ctokens = self.cache[fhash]
            else:
                self.misses += 1
                if self.engine.program:
                    ctext = self.engine.extract_text(pnmfile).lower()
                else:
                    print >> sys.stderr, \
                          "No OCR program '%s' available - can't get text!" \
                          % (self.engine.engine_name,)
                    ctext = ""
                ctokens = set()
                if not ctext.strip():
                    ctokens.add("image-text:no text found")
                else:
                    nlines = len(ctext.strip().split("\n"))
                    if nlines:
                        ctokens.add("image-text-lines:%d" % int(log2(nlines)))
                self.cache[fhash] = (ctext, ctokens)
            textbits.append(ctext)
            tokens |= ctokens
            os.unlink(pnmfile)
        return "\n".join(textbits), tokens
    def analyze(self, engine_name, parts):
        if self.engine is not None and self.engine.engine_name!=engine_name:
            self.engine = None
        if self.engine is None:
            self.engine = get_engine(engine_name)
        if self.engine is None:
            print >> sys.stderr, "invalid engine name '%s' - OCR disabled" \
                                 % (engine_name,)
            return "", Set()
        if not parts:
            return "", Set()
        if Image is not None:
            pnmfiles, tokens = PIL_decode_parts(parts)
        else:
            return "", Set()
        if pnmfiles:
            text, new_tokens = self.extract_ocr_info(pnmfiles)
            return text, tokens | new_tokens
        return "", tokens
    def close(self):
        if options["globals", "verbose"]:
            print >> sys.stderr, "saving", len(self.cache),
            print >> sys.stderr, "items to", self.cachefile,
            if self.hits + self.misses:
                print >> sys.stderr, "%.2f%% hit rate" % \
                      (100 * self.hits / (self.hits + self.misses)),
            print >> sys.stderr
        pickle.dump(self.cache, open(self.cachefile, "wb"))
_cachefile = options["Tokenizer", "crack_image_cache"]
crack_images = ImageStripper(_cachefile).analyze
