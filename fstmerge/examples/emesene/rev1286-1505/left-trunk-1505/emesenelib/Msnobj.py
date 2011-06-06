import urllib
import base64
import sha
import os
class MsnObjectsManager(object):
    def __init__( self, user ):
        self.user = user
        self.objects = {}
        self.idToContext = {}
    def getByContext( self, context ):
        tmp = base64.b64decode( context )
        if not tmp.endswith( '\0' ):
            context = base64.b64encode( tmp+'\0' )
        if self.objects.has_key(context):
            return self.objects[context]
        else:
            return None
    def getIds( self ):
        return self.idToContext.keys()
    def getById( self, id ):
        return self.getByContext( self.idToContext[id] )
    def create( self, id, filename, type ):
        f = file( filename, 'rb' )
        f.seek( 0,2 )
        size = f.tell()
        f.seek( 0,0 )
        name = filename.split( os.sep )[ -1: ][ 0 ]
        msnobj = Msnobj( self.user.lower(), size, type, "0", data = f.read(), filename=filename, friendly=id )
        f.close()
        context = base64.b64encode( str( msnobj )+ '\0' )
        self.objects.update( {context:msnobj} )
        if id !='': self.idToContext.update( {id:context} )
        return msnobj
    def remove( self, id=None, context=None ):
        try:
            if id != None:
                context = self.getById( id )
                self.idToContext.pop( id )
            self.objects.pop( context )
        except: pass
class Msnobj( object ):
    '''this class represent the msnobject, for example:
    <msnobj Creator="buddy@hotmail.com" Size="24539" Type="3" Location="TFR2C.tmp" Friendly="AAA=" SHA1D="snip" SHA1C="snip"/>
    wlm CE:
    spoo    <msnobj Creator="buddy@hotmail.com" Type="2" SHA1D="87YzhijLQyyp4iIlL2k9LlB2wr0=" Size="1169" Location="0" Friendly="cwBwAG8AbwAAAA=="/>
    *  2: Custom Emoticons
    * 3: Display Pictures
    * 5: Background Images
    * 7: Dynamic Display Pictures (Flash-animated DPs)
    * 8: Winks (short Flash animations)
    * 11: Voice Clips (Wave sounds)
    * 12: "SavedState" Property of Add-Ins
    * 14: MSNP15 Locations 
    '''
    CUSTOM_EMOTICON = 2
    DISPLAY_PICTURE = 3
    BACKGROUND_IMAGE = 4
    DYNAMIC_DISPLAY_PICTURE = 7
    WINK = 8
    VOICE_CLIP = 11
    SAVED_STATE_PROPERTY = 12
    LOCATION = 14
    def __init__( self, creator, size='', type='', location='', friendly='\x00', sha1c='', sha1d='', data='', filename='' ):
        '''#  Creator - This field indicates the person who made (and is sending) the object.
            * 2: Custom Emoticons
            * 3: Display Pictures
            * 5: Background Images
            * 7: Dynamic Display Pictures (Flash-animated DPs)
            * 8: Winks (short Flash animations)
            * 11: Voice Clips (Wave sounds)
            * 12: "SavedState" Property of Add-Ins
            * 14: MSNP15 Locations 
        '''
        self.creator = creator
        self.size = size
        self.type = int(type)
        self.location = location
        self.friendly = friendly
        self.filename = filename
        self.data = data
        if sha1d != '':
            self.sha1d = sha1d
            self.forceSha1d = True
        else:
            self.sha1d = self.makeSha1d() 
            self.forceSha1d = False
        if sha1c != '':
            self.sha1c = sha1c
        else:
            self.sha1c = self.makeSha1c()
    def __repr__( self ):
        friendly = base64.b64encode( unicode( self.friendly ).encode( 'utf-16' )[2:] )
        string = '<msnobj Creator="' + self.creator + '" '
        string += 'Size="' + str( self.size ) + '" Type="' + str( self.type ) + '" '
        string += 'Location="'+ self.location +'" '
        string += 'Friendly="' + friendly + '" '
        sha1d = ''
        if self.forceSha1d:
            sha1d = self.sha1d
        else:
            sha1d = self.makeSha1d()
        string += 'SHA1D="' + sha1d + '" SHA1C="' + self.makeSha1c() + '"/>'
        return string
    def makeSha1d( self ):
        return base64.b64encode( sha.new( self.data ).digest() )
    def makeSha1c( self ):
        friendly = base64.b64encode( unicode( self.friendly ).encode( 'utf-16' )[2:] )
        string = 'Creator' + self.creator
        string += 'Size' + str( self.size ) + 'Type' + str( self.type )
        string += 'Location'+ unquote( self.location )
        string += 'Friendly' + friendly
        string += 'SHA1D' + self.sha1d
        return base64.b64encode( sha.new( string ).digest() )
    def quote( self ):
        return urllib.quote( self.__repr__() )
    def getCreator( self ): #XXX: property
        return self.creator
    def setCreator( self, creator ):
        self.creator = creator
    def getType(self):
        return str( self.type )
    def __eq__(self, obj):
        status = True
        if not isinstance(obj, Msnobj):
            return False
        fields = ['creator', 'location', 'friendly', 'size', 'type',
                  'sha1c', 'sha1d']
        for field in fields:
            if getattr(self, field) != getattr(obj, field):
                status = False
        return status
    def __neq__(self, obj):
        return not (self == obj)
def unquote( msnobj ):    
    return urllib.unquote( msnobj )
def createFromString( string, quoted=True ):
    if string == '0': return None
    if quoted:
        msnobjStr = unquote( string )
    else:
        msnobjStr = string
    def getData( string, start, stop ):
        if start in string:
            return string.split( start )[1].split(stop)[0]
        else:
            return ''
    try:
        creator = getData( msnobjStr, 'Creator="', '"' )
        size = getData( msnobjStr, 'Size="', '"' )
        _type = getData( msnobjStr, 'Type="', '"' )
        location = getData( msnobjStr, 'Location="', '"' )
        friendly = getData( msnobjStr, 'Friendly="', '"' )
        try:
            friendly = str( base64.b64decode( friendly ).decode( 'utf-16' ) )
        except:
            friendly = '\x00'
        sha1d = getData( msnobjStr, 'SHA1D="', '"' )
        sha1c = getData( msnobjStr, 'SHA1C="', '"' )
        return Msnobj(creator, size, _type, location, friendly, sha1c, sha1d)
    except Exception, e:
        print 'cant create msnobj from: ' + msnobjStr
        print str( e )
        return None
