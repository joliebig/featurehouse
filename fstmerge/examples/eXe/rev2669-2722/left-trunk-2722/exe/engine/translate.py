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
def installSafeTranslate():
    """
    Makes '_' do safe translating
    Assumes '_' is already installed as the normal translate method
    """
    def checkInstall():
        return __builtins__['_'] is installSafeTranslate
    if checkInstall(): return
    else:
        __builtins__['__old_translate__'] = __builtins__['_']
        __builtins__['_'] = safeTranslate
def safeTranslate(message, encoding='utf-8'):
    """
    Safely translates a string
    """
    try:
        return __old_translate__(message)
    except UnicodeDecodeError, e:
        try:
            return __old_translate__(unicode(message, encoding))
        except Exception:
            raise e
