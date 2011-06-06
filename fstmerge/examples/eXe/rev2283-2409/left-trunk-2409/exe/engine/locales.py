"""
Auto chooses locale
"""
import os
import locale
def chooseDefaultLocale(localeDir):
    """
    Given a directory with a bunch of sub dirs (en, es, en.US, etc.)
    Returns the name of the most appropriate sub dir for this system
    """
    lang, encoding = locale.getdefaultlocale()
    if lang is None:
        lang = 'en'
    if encoding is None:
        encoding = 'utf-8'
    localeName = '%s.%s' % (lang, encoding)
    myLang, myCountry, myEncoding = splitLocaleName(localeName)
    possibleDirs = []
    if localeDir.isdir():
        for sub in localeDir.dirs():
            lang, country, encoding = splitLocaleName(sub.basename())
            points = 0
            if lang == myLang:
                points += 1
            if country == myCountry:
                points += 1
            if encoding == myEncoding:
                points += 1
            if points:
                possibleDirs.append((points, sub.basename()))
        possibleDirs.sort()
        if possibleDirs:
            return possibleDirs[-1][-1]
    return 'en'
def splitLocaleName(localeName):
    """
    Takes a locale name and returns
    langCode, country, encoding
    eg.
      en_NZ.UTF-8
    becomes
      'en', 'NZ', 'UTF-8'
    eg.
      es
    becomes
      'es', '', 'ASCII'
    """
    country = ''
    encoding = 'ASCII'
    if '_' in localeName:
        lang, country = localeName.split('_', 1)
        if '.' in country:
            country, encoding = country.split('.', 1)
    else:
        lang = localeName
        if '.' in lang:
            lang, encoding = lang.split('.', 1)
    return lang, country, encoding
