'''base classes that represent abstract GUI elements to build configuration
dialog'''
class Base(object):
    '''the base class for all the items that can contain a value, it 
    contains a identifier and a value, the callback is called when the
    value changes, the callback must receive 4 arguments:
    * this object
    * the identifier
    * the value
    * a boolean that is true if the configurarion dialog is closed
    (that means that it's the last change on the element, useful if you
    dont want to update things everytime the user changes the content, and
    only when is the last change)'''
    def __init__(self, identifier, callback=None):
        '''class constructor'''
        self.identifier = identifier
        self._value = None
        self.callback = callback
    def _get_value(self):
        '''return value'''
        return self._value
    def _set_value(self, value):
        '''set value'''
        self._value = value
        self.on_value_changed()
    value = property(fget=_get_value, fset=_set_value)
    def on_value_changed(self, is_last_change=False):
        '''call the calback if not none with the arguments especified on the
        class doc, is_last_change will be True if it's the last change on the
        object (when the config dialog is closed'''
        if self.callback:
            self.callback(self, self.identifier, self.value, is_last_change)
    def validate(self, value=None):
        '''since no validations can be done on this fiels, always return
        True'''
        return (True, 'OK', self)
class Validable(Base):
    '''a class that contain methods to validate the content'''
    def __init__(self, identifier, callback=None):
        '''class constructor'''
        Base.__init__(self, identifier, callback)
        self.validators = []
    def add_validator(self, validator, error_message):
        '''add a validator method to the validators that will be called
        on the content when the validate method is called.
         if the validator return False, the error_message will be displayed'''
        self.validators.append((validator, error_message))
    def validate(self, value=None):
        '''validate the value with all the validators or self.value if
        value is None, the first that fails
        will return the error message, if some fails will return 
        (False, error_message, self) if none fails return (True, 'OK', self)'''
        for (validator, error_message) in self.validators:
            if not validator(value or self.value):
                return (False, error_message, self)
        return (True, 'OK', self)
class Text(Validable):
    '''a class that represent an abstract field that contains a label and
    a text field with an optional text content'''
    def __init__(self, identifier, label, text=None, callback=None):
        '''class constructor'''
        Validable.__init__(self, identifier, callback)
        self.label = label
        self.value = text or ''
class Password(Text):
    '''a class that represent an abstract field that contains a label and
    a password field with an optional text content'''
    def __init__(self, identifier, label, text=None, callback=None):
        '''class constructor'''
        Text.__init__(self, identifier, label, text, callback)
class CheckBox(Base):
    '''a class that represent an abstract field that contains a label and
    can be set to checked or not checked'''
    def __init__(self, identifier, label, value=False, callback=None):
        '''class constructor'''
        Base.__init__(self, identifier, callback)
        self.label = label
        self.value = value
class RadioGroup(Base):
    '''a class that represent an abstract group of fields on which only one
    can be selected, every item has a label and the group has a text that
    describe the groups, for example group_label="fruits",
    labels=("apple", "orange", "banana") selected_index=1
    the identifiers are the values that will be returned as the selecetd
    index, for example the label can be A_pple and the identidier apple,
    or the label can be translated, but identifier stay the same'''
    def __init__(self, identifier, labels, identifiers, group_label, 
            selected_index=0, callback=None):
        '''class constructor, labels is a list or tuple of strings and 
        selected_index is the index of the selected index by default, if
        the index is out of range, the first item will be selected.
        identifier is the identifier of the group, identifiers is a list or
        tuple of the identifier value for each label'''
        Base.__init__(self, identifier, callback)
        if len(labels) < 2:
            raise ValueError("labels size < 2")
        if len(labels) != len(identifiers):
            raise ValueError("number of labels and identifiers differ")
        self.labels = labels
        self.group_label = group_label
        self.selected_index = selected_index
        self.identifiers = identifiers
        if self.selected_index < 0 or self.selected_index > len(self.labels):
            self.selected_index = 0
class Group(object):
    '''a class that represent a logic group of elements, the way it is 
    represented can be a frame or something like that, it has a optional
    name for the group, if label is none, then no frame or label will be
    displayed on the group'''
    def __init__(self, label=None):
        '''class constructor'''
        self.label = label
        self.items = []
    def add_item(self, item):
        '''add an item to the group, the item can be any element'''
        self.items.append(item)
    def on_last_change(self):
        '''call last change for all the containing items'''
        for item in self.items:
            if issubclass(type(item), Base):
                item.on_value_changed(True)
            elif issubclass(type(item), Group):
                item.on_last_change()
    def validate(self):
        '''calidate all the containing elements and return (True, 'OK')
        if all validated and (False, error_message) of the first validation
        that failed'''
        for item in self.items:
            (validated, message, element) = item.validate()
            if not validated:
                return (validated, message, element)
        return (True, 'OK', self)
class Tab(Group):
    '''a class that represent a containter tab with elements'''
    def __init__(self, label):
        '''class constructor'''
        Group.__init__(self, label)
class TabGroup(Group):
    '''a class that represent a group of tabs'''
    def __init__(self):
        '''class constructor'''
        Group.__init__(self, None)
    def add_item(self, item):
        '''add an item to the group, the item can be any element'''
        if type(item) == Tab:
            Group.add_item(self, item)
        else:
            raise ValueError("item is not of type Tab")
def build(element):
    '''this method should be overrided by the implementation module, it should
    return a non modal window with an accept button, that will call to
    a method like this on close:
    (validated, message, element) = element.validate()
    if not validated:
        dialog.error("field '" + element.identifier + \
        "' with value '" + element.value + "' not valid: \n" + message)
    else:
        element.on_last_change()
        dialog_window.close() # or similar
    '''
    raise NotImplementedError
