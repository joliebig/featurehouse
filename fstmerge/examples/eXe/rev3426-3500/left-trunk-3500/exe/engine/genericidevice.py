"""
An iDevice built up from simple fields.
"""
from exe.engine.idevice import Idevice
from exe.engine.field   import Field, TextField, TextAreaField, FeedbackField 
from exe.engine.field   import ImageField, AttachmentField
import logging
log = logging.getLogger(__name__)
class GenericIdevice(Idevice):
    """
    A generic Idevice is one built up from simple fields... as such it
    can have a multitude of different forms all of which are just simple
    XHTML fields.
    """
    persistenceVersion = 9
    def __init__(self, title, class_, author, purpose, tip):
        """
        Initialize 
        """
        if class_ in ("objectives", "activity", "reading", "preknowledge"):
            icon = class_
        else:
            icon = None
        Idevice.__init__(self, title, author, purpose, tip, icon)
        self.class_  = class_
        self.icon    = icon
        self.fields  = []
        self.nextFieldId = 0
        self.systemResources.append('common.js')
        self.systemResources.append('libot_drag.js')
    def clone(self):
        """
        Clone a Generic iDevice just like this one
        """
        miniMe = Idevice.clone(self)
        for field in miniMe.fields:
            field.idevice = miniMe
        return miniMe
    def addField(self, field):
        """
        Add a new field to this iDevice.  Fields are indexed by their id.
        """
        if field.idevice:
            log.error(u"Field already belonging to %s added to %s" %
                      (field.idevice.title, self.title))
        field.idevice = self
        self.fields.append(field)
    def getUniqueFieldId(self):
        """
        Returns a unique id (within this idevice) for a field
        of the form ii_ff where ii is the idevice and ff the field
        """
        self.calcNextFieldId()
        result = self.id + '_' + unicode(self.nextFieldId)
        self.nextFieldId += 1
        log.debug(u"UniqueFieldId: %s" % (result))
        return result
    def calcNextFieldId(self):
        """
        initializes nextFieldId if it is still 0
        """
        if not hasattr(self, 'nextFieldId'):
            self.nextFieldId = 0
        if self.nextFieldId == 0:
            log.debug(u"nextFieldId==0 for self.class_ %s" % (self.class_))
            maxId = 0
            for field in self.fields:
                if isinstance(field.id, unicode):
                    log.debug(u"** field.id = u: %s" % (field.id))
                    c = field.id.split('_')
                    if int(c[-1]) > maxId:
                        maxId = int(c[-1])
                else:
                    log.error(u"** field.id is not unicode= %d" % (field.id))
                    if field.id > maxId:
                        maxId = field.id
            self.nextFieldId = maxId + 1
    def __iter__(self):
        return iter(self.fields)
    def getResourcesField(self, this_resource):
        """
        implement the specific resource finding mechanism for these 
        Generic iDevices:
        """
        from exe.engine.field            import FieldWithResources
        if hasattr(self, 'fields'): 
            for this_field in self.fields: 
                if isinstance(this_field, FieldWithResources) \
                and hasattr(this_field, 'images') : 
                    for this_image in this_field.images: 
                        if hasattr(this_image, '_imageResource') \
                        and this_resource == this_image._imageResource: 
                            return this_field
        return None
    def getRichTextFields(self):
        """
        Like getResourcesField(), a general helper to allow nodes to search 
        through all of their fields without having to know the specifics of each
        iDevice type.  
        """
        fields_list = []
        from exe.engine.field            import FieldWithResources
        if hasattr(self, 'fields'): 
            for this_field in self.fields: 
                if isinstance(this_field, FieldWithResources):
                    fields_list.append(this_field)
        return fields_list
    def upgradeToVersion1(self):
        """
        Upgrades the node from version 0 (eXe version 0.4) to 1.
        Adds icon
        """
        log.debug("Upgrading iDevice")
        if self.class_ in ("objectives", "activity", "reading", "preknowledge"):
            self.icon = self.class_
        else:
            self.icon = "generic"
    def upgradeToVersion2(self):
        """
        Upgrades the node from version 1 (not released) to 2
        Use new Field classes
        """
        oldFields   = self.fields
        self.fields = []
        for oldField in oldFields:
            if oldField.fieldType == "Text":
                self.addField(TextField(oldField.__dict__['name'],
                                        oldField.instruction,
                                        oldField.content))
            elif oldField.fieldType == "TextArea":
                self.addField(TextAreaField(oldField.__dict__['name'],
                                            oldField.instruction,
                                            oldField.content))
            else:
                log.error(u"Unknown field type in upgrade "+oldField.fieldType)
    def upgradeToVersion3(self):
        """
        Upgrades the node from 2 (v0.5) to 3 (v0.6).
        Old packages will loose their icons, but they will load.
        """
        log.debug(u"Upgrading iDevice")
        self.emphasis = Idevice.SomeEmphasis
    def upgradeToVersion4(self):
        """
        Upgrades v0.6 to v0.7.
        """
        self.lastIdevice = False
    def upgradeToVersion5(self):
        """
        Upgrades exe to v0.10
        """
        self._upgradeIdeviceToVersion1()
    def upgradeToVersion6(self):
        """
        Upgrades to v0.12
        """
        self._upgradeIdeviceToVersion2()
        for field in self.fields:
            field._upgradeFieldToVersion2()
        self.systemResources += ["common.js", "libot_drag.js"]
    def upgradeToVersion7(self):
        """
        Upgrades to v0.13
        """
        if self.class_ == 'reading':
            for i, field in enumerate(self.fields):
                if isinstance(field, TextAreaField) \
                and hasattr(field, 'name') \
                and field.name in (_(u'Feedback'), u'Feedback'):
                    newField = FeedbackField(field.name, field.instruc)
                    Field.nextId -= 1
                    newField._id = field._id
                    newField.feedback = field.content
                    newField.idevice = self
                    self.fields[i] = newField
            if self.title == _(u'Reading Activity 0.11'):
                self.title = x_(u'Reading Activity')
            if self.title == u'Reading Activity 0.11':
                self.title = u'Reading Activity'
    def upgradeToVersion8(self):
        """
        Upgrades to v0.20
        """
        self.nextFieldId = 0
    def upgradeToVersion9(self):
        """
        Upgrades to v0.24
        """
        for field in self.fields:
            if isinstance(field, ImageField):
                field.isFeedback = False
    def upgradeToVersion9(self):
        """
        Upgrades to somewhere before version 0.25 (post-v0.24) 
        Taking the old unicode string fields, and converting them 
        into image-enabled TextAreaFields:
        [see also the upgrade in field.py's FeedbackField and 
         idevicestore.py's  __upgradeGeneric() ]
        """
        if self.class_ == 'reading':
            for i, field in enumerate(self.fields):
                if isinstance(field, FeedbackField):
                    if not hasattr(field,"content"): 
                        field.content = field.feedback 
                        field.content_w_resourcePaths = field.feedback 
                        field.content_wo_resourcePaths = field.feedback
