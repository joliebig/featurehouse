r"""Lets you manipulate XML/XHTML using a Pythonic object model.  `PyMeldLite`
is a single Python module, _PyMeldLite.py_.    It works with all versions of
Python from 2.2 upwards.  It is a restricted version of PyMeld (see
http://www.entrian.com/PyMeld) - PyMeldLite supports only well-formed XML
with no namespaces, whereas PyMeld supports virtually all XML or HTML
documents.  PyMeldLite is released under the PSF license whereas PyMeld is
released under the Sleepycat License.  PyMeld and PyMeldLite support the
same API.
*Features:*
 o Allows program logic and HTML to be completely separated - a graphical
   designer can design the HTML in a visual XHTML editor, without needing to
   deal with any non-standard syntax or non-standard attribute names.  The
   program code knows nothing about XML or HTML - it just deals with objects
   and attributes like any other piece of Python code.
 o Designed with common HTML-application programming tasks in mind.
   Populating an HTML form with a record from a database is a one-liner
   (using the `%` operator - see below).  Building an HTML table from a set
   of records is just as easy, as shown in the example below.
 o Does nothing but maniplating HTML/XML, hence fits in with any other Web
   toolkits you're using.
 o Tracebacks always point to the right place - many Python/HTML mixing
   systems use exec or eval, making bugs hard to track down.
*Quick overview*
A `PyMeldLite.Meld` object represents an XML document, or a piece of one.
All the elements in a document with `id=name` attributes are made available
by a Meld object as `object.name`.  The attributes of elements are available
in the same way.  A brief example is worth a thousand words:
>>> from PyMeldLite import Meld
>>> xhtml = '''<html><body>
... <textarea id="message" rows="2" wrap="off">Type your message.</textarea>
... </body></html>'''
>>> page = Meld(xhtml)                # Create a Meld object from XHTML.
>>> print page.message                # Access an element within the document.
<textarea id="message" rows="2" wrap="off">Type your message.</textarea>
>>> print page.message.rows           # Access an attribute of an element.
2
>>> page.message = "New message."     # Change the content of an element.
>>> page.message.rows = 4             # Change an attribute value.
>>> del page.message.wrap             # Delete an attribute.
>>> print page                        # Print the resulting page.
<html><body>
<textarea id="message" rows="4">New message.</textarea>
</body></html>
So the program logic and the HTML are completely separated - a graphical
designer can design the HTML in a visual XHTML editor, without needing to
deal with any non-standard syntax or non-standard attribute names.  The
program code knows nothing about XML or HTML - it just deals with objects and
attributes like any other piece of Python code.  Populating an HTML form with
a record from a database is a one-liner (using the `%` operator - see below).
Building an HTML table from a set of records is just as easy, as shown in the
example below:
*Real-world example:*
Here's a data-driven example populating a table from a data source, basing the
table on sample data put in by the page designer.  Note that in the real world
the HTML would normally be a larger page read from an external file, keeping
the data and presentation separate, and the data would come from an external
source like an RDBMS.  The HTML could be full of styles, images, anything you
like and it would all work just the same.
>>> xhtml = '''<html><table id="people">
... <tr id="header"><th>Name</th><th>Age</th></tr>
... <tr id="row"><td id="name">Example name</td><td id="age">21</td></tr>
... </table></html>
... '''
>>> doc = Meld(xhtml)
>>> templateRow = doc.row.clone()  # Take a copy of the template row, then
>>> del doc.row                    # delete it to make way for the real rows.
>>> for name, age in [("Richie", 30), ("Dave", 39), ("John", 78)]:
...      newRow = templateRow.clone()
...      newRow.name = name
...      newRow.age = age
...      doc.people += newRow
>>> print re.sub(r'</tr>\s*', '</tr>\n', str(doc))  # Prettify the output
<html><table id="people">
<tr id="header"><th>Name</th><th>Age</th></tr>
<tr id="row"><td id="name">Richie</td><td id="age">30</td></tr>
<tr id="row"><td id="name">Dave</td><td id="age">39</td></tr>
<tr id="row"><td id="name">John</td><td id="age">78</td></tr>
</table></html>
Note that if you were going to subsequently manipulate the table, using
PyMeldLite or JavaScript for instance, you'd need to rename each `row`,
`name` and `age` element to have a unique name - you can do that by assigning
to the `id` attribute but I've skipped that to make the example simpler.
As the example shows, the `+=` operator appends content to an element -
appending `<tr>`s to a `<table>` in this case.
*Shortcut: the % operator*
Using the `object.id = value` syntax for every operation can get tedious, so
there are shortcuts you can take using the `%` operator.  This works just like
the built-in `%` operator for strings.  The example above could have been
written like this:
>>> for name, age in [("Richie", 30), ("Dave", 39), ("John", 78)]:
...      doc.people += templateRow % (name, age)
The `%` operator, given a single value or a sequence, assigns values to
elements with `id`s in the order that they appear, just like the `%` operator
for strings.  Note that there's no need to call `clone()` when you're using
`%`, as it automatically returns a modified clone (again, just like `%` does
for strings).  You can also use a dictionary:
>>> print templateRow % {'name': 'Frances', 'age': 39}
<tr id="row"><td id="name">Frances</td><td id="age">39</td></tr>
That's really useful when you have a large number of data items - for
example, populating an HTML form with a record from an RDBMS becomes a
one-liner.
*Element content*
When you refer to a named element in a document, you get a Meld object
representing that whole element:
>>> page = Meld('<html><span id="x">Hello world</span></html>')
>>> print page.x
<span id="x">Hello world</span>
If you just want to get the content of the element as string, use the
`_content` attribute:
>>> print page.x._content
Hello world
You can also assign to `_content`, though that's directly equivalent to
assigning to the tag itself:
>>> page.x._content = "Hello again"
>>> print page
<html><span id="x">Hello again</span></html>
>>> page.x = "Goodbye"
>>> print page
<html><span id="x">Goodbye</span></html>
The only time that you need to assign to `_content` is when you've taken a
reference to an element within a document:
>>> x = page.x
>>> x._content = "I'm back"
>>> print page
<html><span id="x">I'm back</span></html>
Saying `x = "I'm back"` would simply re-bind `x` to the string `"I'm back"`
without affecting the document.
*Non-self-closing tags*
Some web browsers don't cope with self-closing tags (eg. `<x/>`) in XHTML.
For instance, some versions of Internet Explorer don't understand
`<textarea/>` to be the equivalent of `<textarea></textarea>`, and interpret
the rest of the document as the content of the `textarea`.  For this reason,
PyMeldLite has a module-level attribute called `nonSelfClose`, which is a
dictionary whose keys are the names of the tags that shouldn't be
self-closing.  `textarea` is the only such tag by default.
>>> page = Meld('''<html><textarea name='spam'/></html>''')
>>> print page
<html><textarea name="spam"></textarea></html>
*Legal information:*
_PyMeldLite.py_ is released under the terms of the Python Software Foundation
License - see http://www.python.org/
"""
__version__ = "1.0"
__author__ = "Richie Hindle <richie@entrian.com>"
import sys, re, string
class _Fail:
    """Unambiguously identifies failed attribute lookups."""
    pass
_fail = _Fail()
nonSelfClose = {'textarea': None}
def replaceHighCharacters(match):
    return "&#%d;" % ord(match.group(1))
badxml_chars = ''.join([chr(c) for c in range(0, 32) if c not in [9, 10, 13]])
badxml_map = string.maketrans(badxml_chars, '?' * len(badxml_chars))
class _Node:
    """Represents a node in a document, with a dictionary of `attributes`
    and a list of `children`."""
    def __init__(self, attributes=None):
        """Constructs a `_Node`.  Don't call this directly - `_Node` is only
        ever used as a base class."""
        if attributes is None:
            self.attributes = {}
        else:
            self.attributes = attributes
        self.children = []
    def getElementNode(self):
        """Returns the `_ElementNode` for a node.  See the `_RootNode`
        documentation for why this exists.  You should always go via this
        when you want the children or attributes of an element that might
        be the root element of a tree."""
        return self
    def _cloneChildren(self, newNode):
        """Populates the given node with clones of `self`'s children."""
        for child in self.children:
            newNode.children.append(child.clone(newNode))
    def childrenToText(self):
        """Asks the children to recursively textify themselves, then returns
        the resulting text."""
        text = []
        for child in self.children:
            text.append(child.toText())
        return ''.join(text)
class _RootNode(_Node):
    """The root of a tree.  The root is always a `_RootNode` rather than the
    top-level `_ElementNode` because there may be things like a `<?xml?>`
    declaration outside the root element.  This is why `getElementNode()`
    exists."""
    def __init__(self, attributes=None):
        """Constructs a `_RootNode`, optionally with a dictionary of
        `attributes`."""
        _Node.__init__(self, attributes)
    def getElementNode(self):
        """See `_Node.getElementNode()`."""
        for child in self.children:
            if isinstance(child, _ElementNode):
                return child
    def clone(self):
        """Creates a deep clone of a node."""
        newNode = _RootNode(self.attributes)
        self._cloneChildren(newNode)
        return newNode
    def toText(self):
        """Generates the XML source for the node."""
        return self.childrenToText()
class _ElementNode(_Node):
    """A node representing an element in a document, with a `parent`, a `tag`,
    a dictionary of `attributes` and a list of `children`."""
    def __init__(self, parent, tag, attributes):
        """Constructs an `_ElementNode`, optionally with a dictionary of
        `attributes`."""
        _Node.__init__(self, attributes)
        self.parent = parent
        self.tag = tag
    def clone(self, parent=None):
        """Creates a deep clone of a node."""
        newNode = _ElementNode(parent, self.tag, self.attributes.copy())
        self._cloneChildren(newNode)
        return newNode
    def toText(self):
        """Generates the XML source for the node."""
        text = ['<%s' % self.tag]
        attributes = list(self.attributes.items())
        attributes.sort()
        for attribute, value in attributes:
            text.append(' %s="%s"' % (attribute, value))
        childText = self.childrenToText()
        if childText or self.tag in nonSelfClose:
            text.append('>')
            text.append(childText)
            text.append('</%s>' % self.tag)
        else:
            text.append('/>')
        return ''.join(text)
class _TextNode(_Node):
    """A tree node representing a piece of text rather than an element."""
    def __init__(self, text):
        """Constructs a `_TextNode`."""
        _Node.__init__(self)
        self._text = text
    def clone(self, parent=None):
        """Creates a deep clone of a node."""
        return _TextNode(self._text)
    def toText(self):
        """Returns the text."""
        return self._text
if sys.hexversion >> 16 < 0x203:
    import xmllib
    class _TreeGenerator(xmllib.XMLParser):
        """An XML parser that generates a lightweight DOM tree.  Call `feed()`
        with XML source, then `close()`, then `getTree()` will give you the
        tree's `_RootNode`:
        >>> g = _TreeGenerator()
        >>> g.feed("<xml>Stuff. ")
        >>> g.feed("More stuff.</xml>")
        >>> g.close()
        >>> tree = g.getTree()
        >>> print tree.toText()
        <xml>Stuff. More stuff.</xml>
        """
        def __init__(self):
            xmllib.XMLParser.__init__(self,
                                      translate_attribute_references=False)
            self.entitydefs = {}    # This is an xmllib.XMLParser attribute.
            self._tree = _RootNode()
            self._currentNode = self._tree
            self._pendingText = []
        def getTree(self):
            """Returns the generated tree; call `feed` then `close` first."""
            return self._tree
        def _collapsePendingText(self):
            """Text (any content that isn't an open/close element) is built up
            in `self._pendingText` until an open/close element is seen, at
            which point it gets collapsed into a `_TextNode`."""
            data = ''.join(self._pendingText)
            self._currentNode.children.append(_TextNode(data))
            self._pendingText = []
        def handle_xml(self, encoding, standalone):
            xml = '<?xml version="1.0"'
            if encoding:
                xml += ' encoding="%s"' % encoding
            if standalone:
                xml += ' standalone="%s"' % standalone
            xml += '?>'
            self._pendingText.append(xml)
        def handle_doctype(self, tag, pubid, syslit, data):
            doctype = '<!DOCTYPE %s' % tag
            if pubid:
                doctype += ' PUBLIC "%s"' % pubid
            elif syslit:
                doctype += ' SYSTEM'
            if syslit:
                doctype += ' "%s"' % syslit
            if data:
                doctype += ' [%s]>' % data
            else:
                doctype += '>'
            self._pendingText.append(doctype)
        def handle_comment(self, data):
            self._pendingText.append('<!--%s-->' % data)
        def handle_proc(self, name, data):
            self._pendingText.append('<?%s %s ?>' % (name, data.strip()))
        def handle_data(self, data):
            self._pendingText.append(data)
        def handle_charref(self, ref):
            self._pendingText.append('&#%s;' % ref)
        unknown_charref = handle_charref
        def handle_entityref(self, ref):
            self._pendingText.append('&%s;' % ref)
        unknown_entityref = handle_entityref
        def handle_cdata(self, data):
            if self._pendingText:
                self._collapsePendingText()
            self._pendingText.append('<![CDATA[%s]]>' % data)
        def unknown_starttag(self, tag, attributes):
            if self._pendingText:
                self._collapsePendingText()
            newNode = _ElementNode(self._currentNode, tag, attributes)
            self._currentNode.children.append(newNode)
            self._currentNode = newNode
        def unknown_endtag(self, tag):
            if self._pendingText:
                self._collapsePendingText()
            self._currentNode = self._currentNode.parent
else:
    import xml.parsers.expat
    class _TreeGenerator:
        """An XML parser that generates a lightweight DOM tree.  Call `feed()`
        with XML source, then `close()`, then `getTree()` will give you the
        tree's `_RootNode`:
        >>> g = _TreeGenerator()
        >>> g.feed("<xml>Stuff. ")
        >>> g.feed("More stuff.</xml>")
        >>> g.close()
        >>> tree = g.getTree()
        >>> print tree.toText()
        <xml>Stuff. More stuff.</xml>
        """
        def __init__(self):
            self._tree = _RootNode()
            self._currentNode = self._tree
            self._pendingText = []
            self._parser = xml.parsers.expat.ParserCreate()
            self._parser.buffer_text = True
            self._parser.DefaultHandler = self.DefaultHandler
            self._parser.StartElementHandler = self.StartElementHandler
            self._parser.EndElementHandler = self.EndElementHandler
        def _mungeEntities(self, data):
            return re.sub(r'&([A-Za-z0-9#]+);', r':PyMeldEntity:\1:', data)
        def _unmungeEntities(self, data):
            return re.sub(r':PyMeldEntity:([A-Za-z0-9#]+):', r'&\1;', data)
        def feed(self, data):
            """Call this with XML content to be parsed."""
            data = self._mungeEntities(data)
            self._parser.Parse(data)
        def close(self):
            """Call this when you've passed all your XML content to `feed`."""
            self._parser.Parse("", True)
        def getTree(self):
            """Returns the generated tree; call `feed` then `close` first."""
            return self._tree
        def _collapsePendingText(self):
            """Text (any content that isn't an open/close element) is built up
            in `self._pendingText` until an open/close element is seen, at
            which point it gets collapsed into a `_TextNode`."""
            data = ''.join(self._pendingText)
            data = self._unmungeEntities(data)
            self._currentNode.children.append(_TextNode(data))
            self._pendingText = []
        def DefaultHandler(self, data):
            """Expat handler."""
            self._pendingText.append(str(data))
        def StartElementHandler(self, tag, attributes):
            """Expat handler."""
            if self._pendingText:
                self._collapsePendingText()
            newAttributes = {}
            for name, value in attributes.items():
                newAttributes[str(name)] = self._unmungeEntities(str(value))
            newNode = _ElementNode(self._currentNode, str(tag), newAttributes)
            self._currentNode.children.append(newNode)
            self._currentNode = newNode
        def EndElementHandler(self, tag):
            """Expat handler."""
            if self._pendingText:
                self._collapsePendingText()
            self._currentNode = self._currentNode.parent
def _generateTree(source):
    """Given some XML source, generates a lightweight DOM tree rooted at a
    `_RootNode`."""
    doctypeRE = r'(?i)^(\s*<!DOCTYPE\s+HTML\s+PUBLIC\s+"[^"]+"\s*>)'
    match = re.search(doctypeRE, source)
    if match:
        source = source[match.end():]
        doctype = match.group(1)
    else:
        doctype = ''
    rootRE = r'(?i)^\s*<!DOCTYPE\s+([-a-z0-9._:]+)\s+[^<]{1,200}<(\1)'
    match = re.search(rootRE, source)
    if match and match.group(1) != match.group(2):
        source = source[:match.start(1)] + match.group(2) + \
                 source[match.end(1):]
    source = source.translate(badxml_map)
    source = re.sub('([\x80-\xff])', replaceHighCharacters, source)
    g = _TreeGenerator()
    g.feed(source)
    g.close()
    tree = g.getTree()
    if doctype:
        tree.children.insert(0, _TextNode(doctype))
    return tree
READ_ONLY_MESSAGE = "You can't modify this read-only Meld object"
class ReadOnlyError(Exception):
    """Raised if you try to modify a readonly PyMeldLite.Meld."""
    pass
class Meld:
    """Represents an XML document, or a fragment of one.  Pass XML/XHTML
    source to the constructor.  You can then access all the elements with
    `id="name"` attributes as `object.name`, and all the attributes of the
    outermost element as `object.attribute`."""
    def __init__(self, source, readonly=False):
        """Creates a `Meld` from XML source.  `readonly` does what it
        says."""
        self._readonly = readonly
        if isinstance(source, str):
            self._tree = _generateTree(source)
        elif isinstance(source, _Node): # For internal use only.
            self._tree = source
        else:
            raise TypeError("Melds must be constructed from ASCII strings")
    def _findByID(self, node, name):
        """Returns the node with the given ID, or None."""
        if node.attributes.get('id') == name:
            return node
        for child in node.children:
            result = self._findByID(child, name)
            if result:
                return result
    def _quoteAttribute(self, value):
        """Minimally quotes an attribute value, using `&quot;`, `&amp;`,
        `&lt;` and `&gt;`."""
        if not isinstance(value, str):
            value = str(value)
        value = value.replace('"', '&quot;')
        value = value.replace('<', '&lt;').replace('>', '&gt;')
        value = re.sub(r'&(?![a-zA-Z0-9]+;)', '&amp;', value)
        return value
    def _unquoteAttribute(self, value):
        """Unquotes an attribute value quoted by `_quoteAttribute()`."""
        value = value.replace('&quot;', '"').replace('&amp;', '&')
        return value.replace('&lt;', '<').replace('&gt;', '>')
    def _nodeListFromSource(self, value):
        """Given a snippet of XML source, returns a list of `_Node`s."""
        tree = _generateTree("<x>"+value+"</x>")
        return tree.children[0].children
    def _replaceNodeContent(self, node, value):
        """Replaces the content of the given node.  If `value` is a string, it
        is parsed as XML.  If it is a Meld, it it cloned.  The existing
        children are deleted, the new nodes are set as the children of
        `node`."""
        if isinstance(value, Meld):
            node.children = [value._tree.getElementNode().clone()]
        else:
            if not isinstance(value, str):
                value = str(value)
            node.children = self._nodeListFromSource(value)
    def clone(self, readonly=False):
        """Creates a clone of a `Meld`, for instance to change an attribute
        without affecting the original document:
        >>> p = Meld('<p style="one">Hello <b id="who">World</b></p>')
        >>> q = p.clone()
        >>> q.who = "Richie"
        >>> print q.who
        <b id="who">Richie</b>
        >>> print p.who
        <b id="who">World</b>
        By default, clones are not readonly even if the Meld from which
        they're cloned is readonly (the most common reason for taking a
        clone is to create a modified clone of a piece of a document).  To
        make a readonly clone, say `clone = object.clone(readonly=True)`."""
        return Meld(self._tree.clone(), readonly)
    def __getattr__(self, name):
        """`object.<name>`, if this Meld contains an element with an `id`
        attribute of `name`, returns a Meld representing that element.
        Otherwise, `object.<name>` returns the value of the attribute with
        the given name, as a string.  If no such attribute exists, an
        AttributeError is raised.
        `object._content` returns the content of the Meld, not including
        the enclosing `<element></element>`, as a string.
        >>> p = Meld('<p style="one">Hello <b id="who">World</b></p>')
        >>> print p.who
        <b id="who">World</b>
        >>> print p.style
        one
        >>> print p._content
        Hello <b id="who">World</b>
        >>> print p.who._content
        World
        """
        if name == '_content':
            return self._tree.getElementNode().childrenToText()
        if name.startswith('_'):
            try:
                return self.__dict__[name]
            except KeyError:
                raise AttributeError(name)
        node = self._findByID(self._tree, name)
        if node:
            return Meld(node, self._readonly)
        attribute = self._tree.getElementNode().attributes.get(name, _fail)
        if attribute is not _fail:
            return self._unquoteAttribute(attribute)
        raise AttributeError("No element or attribute named %r" % name)
    def __setattr__(self, name, value):
        """`object.<name> = value` sets the XML content of the element with an
        `id` of `name`, or if no such element exists, sets the value of the
        `name` attribute on the outermost element.  If the attribute is not
        already there, a new attribute is created.
        >>> p = Meld('<p style="one">Hello <b id="who">World</b></p>')
        >>> p.who = "Richie"
        >>> p.style = "two"
        >>> p.align = "center"
        >>> p.who.id = "newwho"
        >>> print p
        <p align="center" style="two">Hello <b id="newwho">Richie</b></p>
        """
        if name.startswith('_') and name != '_content':
            self.__dict__[name] = value
            return
        if self._readonly:
            raise ReadOnlyError(READ_ONLY_MESSAGE)
        node = self._findByID(self._tree, name)
        if hasattr(value, '_tree') and value._tree is node:
            return   # x.y = x.y
        if not node and name == '_content':
            node = self._tree.getElementNode()
        if node:
            self._replaceNodeContent(node, value)
        else:
            value = self._quoteAttribute(value)
            self._tree.getElementNode().attributes[name] = value
    def __delattr__(self, name):
        """Deletes the named element or attribute from the `Meld`:
        >>> p = Meld('<p style="one">Hello <b id="who">World</b></p>')
        >>> del p.who
        >>> del p.style
        >>> print p
        <p>Hello </p>
        """
        if name == '_content':
            self._tree.getElementNode().children = []
            return
        if name.startswith('_'):
            try:
                del self.__dict__[name]
                return
            except KeyError:
                raise AttributeError(name)
        if self._readonly:
            raise ReadOnlyError(READ_ONLY_MESSAGE)
        node = self._findByID(self._tree, name)
        if node:
            node.parent.children.remove(node)
            return
        node = self._tree.getElementNode()
        attribute = node.attributes.get(name, _fail)
        if attribute is not _fail:
            del node.attributes[name]
        else:
            raise AttributeError("No element or attribute named %r" % name)
    def __getitem__(self, name):
        """`object[<name>]`, if this Meld contains an element with an `id`
        attribute of `name`, returns a Meld representing that element.
        If no such element exists, a KeyError is raised.
        >>> p = Meld('<p style="one">Hello <b id="who">World</b></p>')
        >>> print p["who"]
        <b id="who">World</b>
        >>> print p["who"]_content
        World
        """
        node = self._findByID(self._tree, name)
        if node:
            return Meld(node, self._readonly)
        raise KeyError("No element named %r" % name)
    def __setitem__(self, name, value):
        """`object[<name>] = value` sets the XML content of the element with an
        `id` of `name`.
        If no such element exists, a KeyError is raised because there is no
        info about the type of element to add.
        >>> p = Meld('<p>Hello <b id="who">World</b></p>')
        >>> p["who"] = "Richie"
        >>> p["who"].id = "newwho"
        >>> print p
        <p>Hello <b id="newwho">Richie</b></p>
        """
        if self._readonly:
            raise ReadOnlyError(READ_ONLY_MESSAGE)
        node = self._findByID(self._tree, name)
        if hasattr(value, '_tree') and value._tree is node:
            return   # x["y"] = x.y
        if node:
            self._replaceNodeContent(node, value)
            return
        raise KeyError("No element named %r" % name)
    def __delitem__(self, name):
        """Deletes the named element from the `Meld`:
        >>> p = Meld('<p style="one">Hello <b id="who">World</b></p>')
        >>> del p["who"]
        >>> print p
        <p style="one">Hello </p>
        """
        if self._readonly:
            raise ReadOnlyError(READ_ONLY_MESSAGE)
        node = self._findByID(self._tree, name)
        if node:
            node.parent.children.remove(node)
            return
        raise KeyError("No element named %r" % name)
    def __iadd__(self, other):
        """`object1 += object2` appends a string or a clone of a Meld to
        the end of another Meld's content.  This is used to build things
        like HTML tables, which are collections of other objects (eg. table
        rows).  See *Real-world example* in the main documentation."""
        if self._readonly:
            raise ReadOnlyError(READ_ONLY_MESSAGE)
        if isinstance(other, Meld):
            nodes = [other._tree.getElementNode().clone()]
        else:
            nodes = self._nodeListFromSource(other)
        self._tree.children.extend(nodes)
        return self
    def __mod__(self, values):
        """`object % value`, `object % sequence`, or `object % dictionary` all
        mimic the `%` operator for strings:
        >>> xml = '<x><y id="greeting">Hello</y> <z id="who">World</z></x>'
        >>> x = Meld(xml)
        >>> print x % ("Howdy", "everybody")
        <x><y id="greeting">Howdy</y> <z id="who">everybody</z></x>
        >>> print x % {'who': 'all'}
        <x><y id="greeting">Hello</y> <z id="who">all</z></x>
        Assignment for sequences happens in the same order that nodes with
        'id' attributes appear in the document, not including the top-level
        node (because if the top-level node were included, you'd only ever
        be able to assign to that and nothing else):
        >>> xml = '''<a id="a">
        ... <b>  <!-- `b` has no ID, hence is ignored. -->
        ...     <c id="c">First one</c>
        ...     <d id="d">Second one</d>
        ... </b>
        ... <e id="e">Third one; the content includes 'f':
        ...     <f id="f">Removed when 'e' is assigned to</f>
        ... </e>
        ... </a>'''
        >>> a = Meld(xml)
        >>> print a % ('One, with a <z id="new">new</z> node', 'Two', 'Three')
        <a id="a">
        <b>  <!-- `b` has no ID, hence is ignored. -->
            <c id="c">One, with a <z id="new">new</z> node</c>
            <d id="d">Two</d>
        </b>
        <e id="e">Three</e>
        </a>
        Giving the wrong number of elements to `%` raises the same exceptions
        as the builtin string `%` operator.  Unlike the builtin `%` operator,
        dictionaries don't need to specify all the keys:
        >>> print x % "Howdy"
        Traceback (most recent call last):
        ...
        TypeError: not enough arguments
        >>> print x % ("Howdy", "everybody", "everywhere")
        Traceback (most recent call last):
        ...
        TypeError: not all arguments converted
        >>> print x % {"greeting": "Howdy"}
        <x><y id="greeting">Howdy</y> <z id="who">World</z></x>
        """
        returnObject = self.clone()
        if hasattr(values, 'values') and hasattr(values.values, '__call__'):
            keys = list(values.keys())
            sequence = list(values.values())
        elif hasattr(values, '__getitem__') and \
             not isinstance(values, str):
            keys = None
            sequence = list(values)
        else:
            keys = None
            sequence = [values]
        if keys:
            for key, value in zip(keys, sequence):
                node = returnObject._findByID(returnObject._tree, key)
                if node:
                    returnObject._replaceNodeContent(node, value)
        else:
            stack = returnObject._tree.getElementNode().children[:]
            stack.reverse()
            sequence.reverse()
            while stack and sequence:
                element = stack.pop()
                if 'id' in element.attributes:
                    self._replaceNodeContent(element, sequence.pop())
                else:
                    for index in range(len(element.children)):
                        stack.append(element.children[-1 - index])
            if sequence:
                raise TypeError("not all arguments converted")
            while stack:
                if 'id' in stack.pop().attributes:
                    raise TypeError("not enough arguments")
        return returnObject
    def __add__(self, other):
        """`object1 + object2` turns both objects into strings and returns the
        concatenation of the strings:
        >>> a = Meld('<html><span id="x">1</span></html>')
        >>> b = Meld('<html><span id="y">2</span></html>')
        >>> c = Meld('<html><span id="z">3</span></html>')
        >>> print a + b
        <html><span id="x">1</span></html><html><span id="y">2</span></html>
        >>> print a.x + b.y + c.z
        <span id="x">1</span><span id="y">2</span><span id="z">3</span>
        """
        if isinstance(other, Meld):
            other = other._tree.toText()
        return self._tree.toText() + other
    def __radd__(self, other):
        """See `__add__`"""
        return other + self._tree.toText()
    def __str__(self):
        """Returns the XML that this `Meld` represents.  Don't call
        this directly - instead convert a `Meld` to a string using
        `str(object)`.  `print` does this automatically, which is why
        none of the examples calls `str`."""
        return str(self._tree.toText())
__test__ = {
'_Node': _Node,
'_RootNode': _RootNode,
'_ElementNode': _ElementNode,
'_TextNode': _TextNode,
'_TreeGenerator': _TreeGenerator,
'_generateTree': _generateTree,
'<?xml>': """
>>> print Meld('''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
... <html>Stuff</html>''')
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<html>Stuff</html>
""",
'DOCTYPE (system)': """
>>> print Meld('''<!DOCTYPE NAME SYSTEM "name.dtd">
... <name>Stuff</name>''')
<!DOCTYPE name SYSTEM "name.dtd">
<name>Stuff</name>
""",
'DOCTYPE (small)': """
>>> print Meld('''<!DOCTYPE name PUBLIC "-//ORG//NAME//EN" "name.dtd">
... <name>Stuff</name>''')
<!DOCTYPE name PUBLIC "-//ORG//NAME//EN" "name.dtd">
<name>Stuff</name>
""",
'DOCTYPE (full)': """
>>> print Meld('''<!DOCTYPE name [
...     <!ELEMENT terms (hhItem1, hhItem2)>
...     <!ELEMENT hhItem1 (#PCDATA)>
...     <!ELEMENT hhItem2 (#PCDATA)>
... ]>
... <name>Stuff</name>''')
<!DOCTYPE name [
    <!ELEMENT terms (hhItem1, hhItem2)>
    <!ELEMENT hhItem1 (#PCDATA)>
    <!ELEMENT hhItem2 (#PCDATA)>
]>
<name>Stuff</name>
""",
'DOCTYPE (HTML hackery)': """
>>> html = '''<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
... <html>Stuff</html>'''
>>> print Meld(html)
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>Stuff</html>
""",
'XML proc': """
>>> print Meld('''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
... <?codewarrior exportversion="1.0.1" ideversion="4.2" ?>
... <!DOCTYPE PROJECT [
... <!ELEMENT PROJECT (TARGETLIST, TARGETORDER, GROUPLIST, DESIGNLIST?)>
... ]>
... <PROJECT>Stuff</PROJECT>''')
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<?codewarrior exportversion="1.0.1" ideversion="4.2" ?>
<!DOCTYPE PROJECT [
<!ELEMENT PROJECT (TARGETLIST, TARGETORDER, GROUPLIST, DESIGNLIST?)>
]>
<PROJECT>Stuff</PROJECT>
""",
'comment': """
>>> page = Meld('''<x><!-- Comment --></x>''')
>>> print page
<x><!-- Comment --></x>
""",
'entities and charrefs': """
>>> page = Meld('''<html><body>&bull; This "and&#160;that"...
... <span id="s" title="&quot;Quoted&quot; &amp; Not">x</span></body></html>''')
>>> print page.s.title
"Quoted" & Not
>>> page.s.title = page.s.title + " <>"
>>> print page.s.title
"Quoted" & Not <>
>>> print page.s
<span id="s" title="&quot;Quoted&quot; &amp; Not &lt;&gt;">x</span>
""",
'self-closing tags': """
>>> page = Meld('''<span id="spam">Stuff</span>''')
>>> page.spam = ""
>>> print page
<span id="spam"/>
>>> page = Meld('''<textarea name="spam"/>''')  # nonSelfClose special case
>>> print page
<textarea name="spam"></textarea>
""",
'assigning to _content': """
>>> page = Meld('''<html><span id="s">Old</span></html>''')
>>> page.s._content = "New"
>>> print page
<html><span id="s">New</span></html>
>>> page._content = "All new"
>>> print page
<html>All new</html>
""",
'deleting _content': """
>>> page = Meld('''<html><span id="s">Old</span></html>''')
>>> del page.s._content
>>> print page
<html><span id="s"/></html>
""",
'constructing from an unknown type': """
>>> page = Meld(1)
Traceback (most recent call last):
...
TypeError: Melds must be constructed from ASCII strings
""",
'accessing a non-existent attribute': """
>>> page = Meld('<html></html>')
>>> print page.spam
Traceback (most recent call last):
...
AttributeError: No element or attribute named 'spam'
>>> del page.spam
Traceback (most recent call last):
...
AttributeError: No element or attribute named 'spam'
""",
'cdata': """
>>> html = '''<html><script id="cdataExample">//<![CDATA[
... window.alert("This is not valid XML: <spam> << & >> etc.");
... //]]>
... </script></html>'''
>>> page = Meld(html)
>>> print page.cdataExample
<script id="cdataExample">//<![CDATA[
window.alert("This is not valid XML: <spam> << & >> etc.");
//]]>
</script>
""",
'add new things':"""
>>> page = Meld('''<html><textarea id="empty"></textarea></html>''')
>>> page.empty = "Not any more"
>>> page.empty.cols = 60
>>> print page
<html><textarea cols="60" id="empty">Not any more</textarea></html>
""",
'readonly': """
>>> page = Meld('''<html><span id='no'>No!</span></html>''', readonly=True)
>>> page.no = "Yes?"
Traceback (most recent call last):
...
ReadOnlyError: You can't modify this read-only Meld object
>>> page.no.attribute = "Yes?"
Traceback (most recent call last):
...
ReadOnlyError: You can't modify this read-only Meld object
>>> page.no += "More?"
Traceback (most recent call last):
...
ReadOnlyError: You can't modify this read-only Meld object
>>> del page.no
Traceback (most recent call last):
...
ReadOnlyError: You can't modify this read-only Meld object
""",
'copy from one to another': """
>>> a = Meld('<html><span id="one">One</span></html>')
>>> b = Meld('<html><span id="two">Two</span></html>')
>>> a.one = b.two
>>> print a
<html><span id="one"><span id="two">Two</span></span></html>
>>> b.two = "New"
>>> print a  # Checking for side-effects
<html><span id="one"><span id="two">Two</span></span></html>
""",
'mixed-type add, radd and iadd': """
>>> a = Meld('<html><span id="one">1</span></html>')
>>> print a.one + "x"
<span id="one">1</span>x
>>> print "x" + a.one
x<span id="one">1</span>
>>> a.one += "y<z></z>"
>>> print a
<html><span id="one">1y<z/></span></html>
""",
'no unicode': r"""
>>> u = Meld(u'<html><span id="one">One</span></html>')
Traceback (most recent call last):
...
TypeError: Melds must be constructed from ASCII strings
""",
'private attributes': """
>>> page = Meld('<html>x</html>')
>>> page._private = "Spam"
>>> print repr(page._private)
'Spam'
>>> print page
<html>x</html>
>>> del page._private
>>> print repr(page._private)
Traceback (most recent call last):
...
AttributeError: _private
>>> del page._private
Traceback (most recent call last):
...
AttributeError: _private
>>> print page
<html>x</html>
""",
'bad XML characters': """
>>> page = Meld('''<x>
... Valentines Day Special \x96 2 bikinis for the price of one \x01
... </x>''')    # No exception.
>>> print page
<x>
Valentines Day Special &#150; 2 bikinis for the price of one ?
</x>
"""
}
def test():
    """Tests the `PyMeldLite` module, performing code coverage analysis if
    `Entrian.Coverage` is available.  Returns `(failed, total)`, a la
    `doctest.testmod`."""
    import doctest
    try:
        from Entrian import Coverage
        Coverage.start('PyMeldLite')
    except ImportError:
        Coverage = False
    from . import PyMeldLite
    result = doctest.testmod(PyMeldLite)
    if Coverage:
        analysis = Coverage.getAnalysis()
        analysis.printAnalysis()
    return result
if __name__ == '__main__':
    failed, total = test()
    if failed == 0:     # Else `doctest.testmod` prints the failures.
        print("All %d tests passed." % total)
