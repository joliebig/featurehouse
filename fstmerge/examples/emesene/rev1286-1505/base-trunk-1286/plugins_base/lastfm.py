import Plugin
import urllib
import datetime
import md5
import re
import time 
import urllib2
import socket
class AudioScrobbler:
    """
    Provide the ability to post tracks played to a user's Last.fm
    account
    """
    def __init__(self,username=u'',password=u''):
        self.params = dict(username=username,
                           password=password,
                           client_name=u'tst',
                           client_version=u'1.0',
                           protocol_version=u'1.1',
                           host=u'post.audioscrobbler.com')
        self.auth_details = {}
        self.last_shake = None
        self.authenticated = False
        self.posturl = None
        socket.setdefaulttimeout(2)
    def updatecfg(self,username=u'',password=u''):
        self.params['username'] = username
        self.params['password'] = password
        self.authenticated = False
    def auth(self):
        """ Authenticate against the server """
        if self.authenticated:
            return True
        now = datetime.datetime.utcnow()
        interval = datetime.timedelta(minutes=30)
        if self.last_shake is not None and self.last_shake + interval > now:
            last_shake_string = self.last_shake.strftime("%Y-%m-%d %H:%M:%S")
            print "Tried to reauthenticate too soon after last try (last try at %s)" % last_shake_string
            return False
        p = {}
        p['hs'] = 'true'
        p['u'] = self.params['username']
        p['c'] = self.params['client_name']
        p['v'] = self.params['client_version']
        p['p'] = self.params['protocol_version']
        plist = [(k, urllib.quote_plus(v.encode('utf8'))) for k, v in p.items()]
        authparams = urllib.urlencode(plist)
        url = 'http://%s/?%s' % (self.params['host'], authparams)
        req = urllib2.Request(url)
        try:
            url_handle = urllib2.urlopen(req)
        except:
            self.authenticated = False
            print "Connection Error: %s" % req
            return False
        self.last_shake = datetime.datetime.utcnow()
        response = url_handle.readlines()
        if len(response) == 0:
                print "Connection Error: no response" 
        find_interval = re.match('INTERVAL (\d+)', response[-1])
        username = self.params['username']
        password = self.params['password']
        if response[0].startswith('UPTODATE'):
            ask = response[1].strip()
            answer = md5.md5(md5.md5(password).hexdigest() + ask).hexdigest()
            self.auth_details['u'] = urllib.quote_plus(username.encode('utf8'))
            self.auth_details['s'] = answer
            self.posturl = response[2].strip()
            self.authenticated = True
        elif response[0].startswith('UPDATE'):
            ask = response[1].strip()
            answer = md5.md5(md5.md5(password).hexdigest() + ask).hexdigest()
            self.auth_details['u'] = urllib.quote_plus(username.encode('utf8'))
            self.auth_details['s'] = answer
            self.posturl = response[2].strip()
            self.authenticated = True
        elif response[0].startswith('BADUSER'):
            self.authenticated = False
            print "Error Username '%s' unknown by Last.fm" % username
            return False
        elif response[0].startswith('FAILED'):
            self.authenticated = False
            reason = response[0][6:]
            print "Error %s" % reason
            return False
        else:
            self.authenticated = False
            print "Unknown response from server: %r" % (response,)
            return False
        return True
    def post(self, 
                  artist_name,
                  song_title,
                  sane_length,
                  date_played, 
                  album=u'',
                  mbid=u''):
        track = {'a[%s]': artist_name.decode('utf8', 'replace').encode('utf8'),
                't[%s]': song_title.decode('utf8', 'replace').encode('utf8'),
                'l[%s]': str(sane_length),
                'i[%s]': date_played,
                'b[%s]': album.decode('utf8', 'replace').encode('utf8'),
                'm[%s]': mbid.encode('utf8'),
                }
        params = {}
        count = 0
        for k in track.keys():
            track[k] = urllib.quote(track[k].encode('utf-8'))
            params[k % (count,)] = track[k]
        if not self.auth():
            return False
        params.update(self.auth_details)
        postdata = '&'.join(['%s=%s' % (k, v) for k, v in params.iteritems()])
        try:
            req = urllib2.Request(url=self.posturl, data=postdata)
            url_handle = urllib2.urlopen(req)
            response = url_handle.readlines()
        except:
            print 'Connection Error %s' % req
            return False
        if len(response) == 0:
            return False
        if response[0].startswith('OK'):
            return True
        elif response[0].startswith('BADAUTH'):
            print 'Connection Error: Got BADAUTH'
            self.authenticated = False
            return False
        elif response[0].startswith('FAILED'):
            reason = response[0][6:].strip()
            print 'Connection Error %s' % reason
            return False
        else:
            print "Connection Error: Server returned something unexpected"
            return False
class MainClass( Plugin.Plugin ):
    '''Main plugin class'''
    def __init__( self, controller, msn ):
        '''Constructor'''
        Plugin.Plugin.__init__( self, controller, msn)
        self.description = _( 'Send information to last.fm' )
        self.authors = { 'Mattia \'matz\' Pini' : 'matz.mz at gmail dot com' }
        self.website = ''
        self.displayName = _( 'Last.fm' )
        self.name = 'lastfm'
        self.config = controller.config
        self.config.readPluginConfig(self.name)
        self.controller = controller
        self.password = self.config.getPluginValue( self.name, 'pass', '' )
        self.username = self.config.getPluginValue( self.name, 'user', '' )
        self.lastfm =  AudioScrobbler(self.username,self.password)
        self.cache = []
    def on_currentmediaChange(self,msnp,user,cm, dict):
        if dict == None:
            return
        lt = time.gmtime(time.time())
        date = "%02d-%02d-%02d %02d:%02d:%02d" % (lt[0], lt[1], lt[2],lt[3], lt[4], lt[5])
        if dict['artist'] != '' or dict['title'] != '' or dict['album'] != '':
            self.cache.append((dict['artist'],dict['title'],200,date,dict['album']))
        for element in self.cache[:]:
            print "Posting %s" % str(element)
            if self.lastfm.post(*element):
                self.cache.remove(element)
            else:
                break
    def start( self ):
        self.scmcId = self.controller.msn.connect( 'self-current-media-changed', self.on_currentmediaChange)
        self.enabled = True 
    def stop( self ):
        self.controller.msn.disconnect( self.scmcId )
        self.enabled = False
    def check( self ):
            return ( True, 'Ok' )
    def configure( self ):
        conf = []
        conf.append( Plugin.Option( 'user', str, _('Username:'), '',
            self.config.getPluginValue( self.name, 'user', self.username )) )
        conf.append( Plugin.Option( 'pass', 'passwd', _('Password:'), '',
            self.config.getPluginValue( self.name, 'pass', self.password )) )
        configWindow = Plugin.ConfigWindow( 'Last.fm', conf )
        response = configWindow.run()
        if response != None:
            if response.has_key( 'user' ):
                self.username =  response[ 'user' ].value
                self.config.setPluginValue( self.name, 'user', self.username )
            if response.has_key( 'pass' ):
                self.password = response[ 'pass' ].value
                self.config.setPluginValue( self.name, 'pass', self.password )
            self.lastfm.updatecfg(self.username,self.password)
        return True
