"""This module is deprecated."""
import warnings
warnings.warn("twisted.enterprise.xmlreflector is deprecated", DeprecationWarning)
import os
from twisted.internet import defer
from twisted.enterprise import reflector
from twisted.enterprise.util import DBError, _TableInfo
from twisted.persisted import marmalade
class XMLRowProxy:
    """Used to persist Row Objects as XML.
    """
    def __init__(self, rowObject):
        self.kw = {}
        for columnName, type  in rowObject.rowColumns:
            self.kw[columnName] = getattr(rowObject, columnName)
class XMLReflector(reflector.Reflector):
    """Reflector for twisted.enterprise that uses XML files.
    WARNING: this is an experimental piece of code. this reflector
    does not function completely yet! it is also very very slow.
    """
    extension = ".xml"
    def __init__(self, baseDir, rowClasses):
        self.baseDir = baseDir
        try:
            os.mkdir(baseDir)
        except OSError, e:
            pass
        self.tableDirs = {}
        reflector.Reflector.__init__(self, rowClasses)        
    def _populate(self):
        """load schema data
        """
        for rc in self.rowClasses:
            newDir = self.baseDir+"/"+rc.rowTableName
            self.tableDirs[rc.rowTableName] = newDir
            try:
                os.mkdir(newDir)
            except OSError, e:
                pass
            tableInfo = _TableInfo(rc)
            self.populateSchemaFor(tableInfo)
    def _rowLoader(self, tableName, parentRow, data,
                   whereClause, forceChildren):
        d = self.tableDirs[ tableName]
        tableInfo = self.schema[tableName]
        filenames = os.listdir(d)
        results = []
        newRows = []
        for filename in filenames:
            if (filename.find(self.extension) !=
                len(filename) - len(self.extension)):
                continue
            f = open(d + "/" + filename, "r")
            proxy = marmalade.unjellyFromXML(f)
            f.close()
            stop = 0
            if whereClause:
                for item in whereClause:
                    (columnName, cond, value) = item
                    if proxy.kw[columnName] != value:
                        stop = 1
            if stop:
                continue
            resultObject = self.findInCache(tableInfo.rowClass, proxy.kw)
            if not resultObject:
                resultObject = tableInfo.rowFactoryMethod[0](
                                       tableInfo.rowClass, data, proxy.kw)
                self.addToCache(resultObject)
                newRows.append(resultObject)
            results.append(resultObject)
        if parentRow:
            self.addToParent(parentRow, newRows, tableName)
        for relationship in tableInfo.relationships:
            if not forceChildren and not relationship.autoLoad:
                continue
            for row in results:
                childWhereClause = self.buildWhereClause(relationship, row)             
                self._rowLoader(relationship.childRowClass.rowTableName, row, data, childWhereClause, forceChildren)
        return results
    def makeFilenameFor(self, rowObject):
        s =""
        keys = rowObject.getKeyTuple()
        for k in keys[1:]:
            s += str(k)
        return self.tableDirs[ rowObject.rowTableName ] + "/" + s + ".xml"            
    def loadObjectsFrom(self, tableName, parentRow = None, data = None, whereClause = None, forceChildren = 1):
        """The whereClause for XML loading is [(columnName, operation, value)] list of tuples
        """
        if parentRow and whereClause:
            raise DBError("Must specify one of parentRow _OR_ whereClause")
        if parentRow:
            info = self.getTableInfo(parentRow)
            relationship = info.getRelationshipFor(tableName)
            whereClause = self.buildWhereClause(relationship, parentRow)
        elif whereClause:
            pass
        else:
            whereClause = []
        results = self._rowLoader(tableName, parentRow, data,
                                  whereClause, forceChildren)
        return defer.succeed(results)
    def updateRow(self, rowObject):
        """update this rowObject to the database.
        """
        return self.insertRow(rowObject)
    def insertRow(self, rowObject):
        """insert a new row for this object instance. do not include the "container" attribute.
        """
        proxy = XMLRowProxy(rowObject)
        filename = self.makeFilenameFor(rowObject)
        f = open(filename,"w")
        marmalade.jellyToXML(proxy, f)
        f.close()
        return defer.succeed(1)
    def deleteRow(self, rowObject):
        """delete the row for this object from the database.
        """
        filename = self.makeFilenameFor(rowObject)        
        os.remove(filename)
        return defer.succeed(1)
__all__ = ['XMLReflector']
