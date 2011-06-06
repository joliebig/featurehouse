import os
import md5
import common
from time import time
class Hotmail:
    def __init__( self, msn, config ):
        self.user = msn.user
        self.password = common.escape(msn.password)
        self.MSPAuth = msn.MSPAuth
        self.dg = msn.demographics
        self.config = config
    def getLoginPage( self, MessageURL=None , PostURL=None, id='2' ):
        if PostURL == None:
            PostURL = 'https://loginnet.passport.com/ppsecure/md5auth.srf?lc='+self.dg['lang_preference']
        if MessageURL == None:
            MessageURL = "/cgi-bin/HoTMaiL"
        sl = str( int ( time() ) - int( self.dg['LoginTime'] ) )
        auth = self.MSPAuth
        sid = self.dg['sid']
        cred =  md5.new( auth + sl + self.password ).hexdigest()
        templateData = {
        'id':id,
        'site':PostURL,
        'login': self.user.split('@')[0],
        'email':self.user,
        'sid':sid,
        'kv':'',
        'sl':sl,
        'url':MessageURL,
        'auth':auth,
        'creds':cred
        }
        return self.parseTemplate( templateData )
    def getProfilePage( self, user ):
        pass
    def parseTemplate( self, data ):
        f = open( 'hotmlog.htm' )
        hotLogHtm = f.read()
        f.close()
        for key in data:
            hotLogHtm = hotLogHtm.replace( '$'+key, data[ key ] )
        self.file = os.path.join(
            self.config.getUserConfigPath(), 'cache', 'login.htm')
        tmpHtml = open( self.file, 'w' )
        tmpHtml.write( hotLogHtm )
        tmpHtml.close()
        return 'file:///' + self.file
