"""
Functions that help with translation
"""
__builtins__['x_'] = lambda x:x
def lateTranslate(propName):
    """
    Given a property name, returns a read/write property that is translated
    every time it is read
    """
    propName = '_%s' % propName
    def set_prop(self, value):
        """
        Used to write the property
        """
        setattr(self, propName, value)
    def get_prop(self):
        """
        Translates a property value on the fly
        """
        value = getattr(self, propName)
        if value:
            return _(value)
        else:
            return value
    return property(get_prop, set_prop)
