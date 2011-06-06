import os
import gtk
import socket
import urllib
import urllib2
import gobject
import Plugin
import currentSong
import paths
AMAZON_KEY = '12DR2PGAQT303YTEWP02'
SEARCH_URL = 'http://webservices.amazon.com/onca/xml?' + \
    'Service=AWSECommerceService&AWSAccessKeyId=' + AMAZON_KEY + \
    '&Operation=ItemSearch&SearchIndex=Music&Artist=%s&' + \
    'ResponseGroup=Images&Keywords=%s'
COVERART_PATH = os.path.join(paths.HOME_DIR, '.covers', '')
class MainClass( Plugin.Plugin ):
    '''Main plugin class'''
    description = _( 'Show a personal message with the current song' \
    ' played in multiple players.' )
    authors = { 'Mariano Guerra' : 'luismarianoguerra gmail com' }
    website = 'http://emesene.org'
    displayName = _( 'CurrentSong' )
    name = 'CurrentSong'
    def __init__( self, controller, msn ):
        '''Constructor'''
        Plugin.Plugin.__init__( self, controller, msn, 1000 )
        self.theme = controller.theme
        self.controller = controller
        self.description = _( 'Show a personal message with the current song' \
            ' played in multiple players.' )
        self.authors = { 'Mariano Guerra' : 'luismarianoguerra gmail com' }
        self.website = 'http://emesene.org'
        self.displayName = _( 'CurrentSong' )
        self.name = 'CurrentSong'
        self.configWindow = None
        self.statusLabel = None
        self.customConfigWidget = None
        self.usingDummy = True
        self.dictCommand = None
        self.Slash = controller.Slash
        self.config = controller.config
        self.config.readPluginConfig(self.name)
        self.oldavatar = None
        self.setPlayer( self.config.getPluginValue( self.name, 'player', \
            'CurrentSong' ) )
        self.player.setStyle( self.config.getPluginValue( self.name, 'style', \
            '%title - %artist (%album)' ) )
        self.avatar = self.config.getPluginValue(self.name, 'avatar', \
            '0') == '1'
        self.remotefetch = self.config.getPluginValue(self.name, \
            'remotefetch', '0') == '1'
        self.updateCustomConf()
        self.timeout = 0
        self.mediaEnabledId = 0
        self.coverAvatar = Plugin.Function( self.getCoverAvatar, \
            self.setCoverAvatar )
    def start( self ):
        '''start the plugin'''
        if not os.path.isdir(COVERART_PATH):
            os.mkdir(COVERART_PATH)
        if not self.usingDummy:
            self.player.start()
        self.timeout = gobject.timeout_add(self.period, self.action)
        self.Slash.register('currentsong', self.currentSongHandler, \
            'currentsong command')
        self.Slash.register('np', self.nowplaying, \
            'Send what you are listening to')
        self.controller.mainWindow.userPanel.mediaButton.show()
        self.mediaEnabledId = self.controller.config.connect(
            'change::mediaEnabled', self.onMediaEnabled)
        self.enabled = True
    def showHelp(self):
        msg = 'Help:'
        for (cmd, val) in self.dictCommand.iteritems():
            msg += '\n ' + str(cmd) + '\t' + str(val[1])
        return msg
    def playerChanged( self, combo ):
        newplayer = combo.get_active_text()
        self.setPlayer(newplayer)
        self.updateStatus()
        self.updateCustomConf() 
        self.dictCommand = self.player.dictCommand
        self.customConfigWidget.refresh(self.player.customConfig)
    def currentSongHandler(self, slashAction):
        name = None
        param = None
        try:
            cmd = slashAction.getParams().split(" ", 1)
            name = cmd[0]
            param = cmd[1]
        except:
            pass
        if name != None and self.dictCommand.has_key(name):
            retval, msg = self.dictCommand[name][0](param)
            if self.dictCommand[name][2]:
                slashAction.outputText( msg , retval )
            else:
                slashAction.outputText( msg )
        else:
            slashAction.outputText(self.showHelp())
    def nowplaying(self, slashAction):
        if self.player.cmd_Show(True)[0] == False:
            slashAction.outputText('Not playing')
        else:
            slashAction.outputText('now playing: '+ \
                self.player.cmd_Show(True)[1], True)
    def stop( self ):    
        '''stop the plugin'''
        if not self.usingDummy:
            self.player.stop()
        if self.timeout:
            gobject.source_remove(self.timeout)
        self.msn.changeCurrentMedia( '' )
        self.Slash.unregister('currentsong')
        self.Slash.unregister('np')
        self.controller.mainWindow.userPanel.mediaButton.hide()
        self.controller.mainWindow.userPanel.mediaButton.set_active(False)
        if self.oldavatar:
            self.controller.changeAvatar(self.oldavatar)
        self.oldavatar = None
        self.controller.config.disconnect(self.mediaEnabledId)
        self.enabled = False
    def onMediaEnabled(self, config, value, oldvalue):
        '''Called when the media button is toggled'''
        print "onMediaEnabled"
        self.action(True)
    def action(self, force=False):
        if not self.enabled or not self.config.user['mediaEnabled']:
            return True
        if not self.usingDummy:
            try:
                check = self.player.check()
            except Exception, e:
                print 'Error in player.check():', e
                self.setPlayer(None)
                check = True
            if force or check: 
                currentsong = self.player.getCurrentSong()\
                    .decode("utf-8", "replace").encode("utf-8")
                sdict = self.player.getSongDict()
                self.msn.changeCurrentMedia( currentsong, sdict)
                if self.avatar:
                    self.coverAvatar(self.remotefetch)
        else:
            self.setPlayer( self.config.getPluginValue( self.name, 'player', \
                'CurrentSong' ) )
        return True
    def getCoverAvatar(self, remote):
        ''' get avatar cover art '''
        path = self.player.getCoverPath()
        if path and os.path.exists(path):
            return path
        path = self.search_local_image()  
        if path and os.path.exists(path):
            return path
        if remote:
            try:
                return self.search_remote_image()
            except:
                pass
    def setCoverAvatar(self, path):
        ''' set avatar cover art '''
        if path != None and os.path.exists(path):
            self.oldavatar = self.config.user['avatarPath']
            self.controller.changeAvatar(path)
            return True
        if not self.player.isPlaying() or not self.player.isRunning():
            if self.oldavatar:
                self.controller.changeAvatar(self.oldavatar)
            self.oldavatar = None
            return False
        return False
    def is_on_path(self, fname):
        for p in os.environ['PATH'].split(os.pathsep):
            if os.path.isfile(os.path.join(p, fname)):
                return True
    def getPluginNames( self ):
        return [ x for x in dir( currentSong ) \
                 if not x.startswith( "__" ) \
                 and x != 'CurrentSong' ]
    def setPlayer( self, name ):
        '''try to set the player object to the name given by the name parameter
        if not set the dummy player to have something to play with'''
        players = self.getPluginNames()
        self.playername = name
        error = '' 
        if name in players:
            try:
                self.player = getattr(currentSong, name)()
                self.usingDummy = False
                self.action()
            except Exception, e:
                print "error initializing CurrentSong player:", e
                error = e
            else:
                status, message = self.player.getStatus()
                self.player.log(status and 'info' or 'error', \
                    'getStatus: ' + message)
                if not status:
                    self.player.status = 'error'
                self.dictCommand = self.player.dictCommand
                return
        self.player = currentSong.CurrentSong()
        self.usingDummy = True
        if error:
            return error
    def check( self ):
        '''check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )'''
        return (True, 'Ok')
    def updateCustomConf( self ):
        for key in self.player.customConfig.keys():
            self.player.customConfig[key] = \
                self.config.getPluginValue( self.name, \
                str(self.playername + "_" + key), \
                self.player.customConfig[key] )     
        self.player.updateConfig()
    def search_local_image(self):
        '''searches in the local covers cache
        returns None if no local image found'''
        artist = urllib.quote(self.player.artist.encode('utf8'))
        album = urllib.quote(self.player.album.encode('utf8'))
        file_dsc = COVERART_PATH + artist + '-' + album + '.jpg'
        if os.path.exists(file_dsc):
            return file_dsc
        return None
    def search_remote_image(self):
        '''searches in amazon for covers
        returns None if no images found'''
        imgfound = None
        artist = urllib.quote(self.player.artist.encode('utf8'))
        album = urllib.quote(self.player.album.encode('utf8'))
        if len(artist) == 0 or len(album) == 0:
            return imgfound
        dest_filename = COVERART_PATH + artist + '-' + album + '.jpg'
        HSIZE = 200 #header size -- magic number, FIXME
        img_url = ''
        for params in [(artist, album), (artist, ''), ('', album)]:
            try:
                request = urllib2.Request(SEARCH_URL % params)
                opener = urllib2.build_opener()
                f = opener.open(request).read()
                img_url = f[HSIZE:].split('<URL>')[1].split('</URL>')[0]
            except:
                print "except"
                pass
            if img_url:
                break
        if not img_url:
            return None
        pos = f.find('<LargeImage>', HSIZE+10) # FIXME, magic number
        if pos != -1:
            try:
                img_url = f[pos:].split('<URL>')[1].split('</URL>')[0]
            except IndexError:
                pass
        if img_url:
            urllib.urlretrieve(img_url, dest_filename)
            imgfound = dest_filename
        return imgfound
    def configure( self ):
        '''display a configuration dialog'''
        style = self.config.getPluginValue(self.name, 'style', \
            '%title - %artist (%album)')
        player = self.config.getPluginValue(self.name, 'player', 'Amarok')
        avatar = self.config.getPluginValue(self.name, 'avatar', '0') == '1'
        remotefetch = self.config.getPluginValue(self.name, \
            'remotefetch', '0') == '1'
        l = []
        l.append(Plugin.Option('style', str, _('Display style'), \
            _('Change display style of the song you are listening, possible ' \
            'variables are %title, %artist and %album'), style))
        l.append(Plugin.Option('player', list, _('Music player'), \
            _('Select the music player that you are using'), player, \
            self.getPluginNames(), self.playerChanged))
        self.statusLabel = gtk.Label()
        status_timeout = gobject.timeout_add(2000, self.updateStatus)
        self.updateStatus()
        logsButton = gtk.Button( _('Show logs') )
        logsButton.connect('clicked', self.showLogs)
        hbox = gtk.HBox()
        hbox.pack_start(self.statusLabel, True, True)
        hbox.pack_start(logsButton)
        status = Plugin.Option('status', gtk.Widget, '', '', hbox)
        l.append(status)
        l.append(Plugin.Option('avatar', bool, _('Change Avatar'), \
            _('Change Avatar'), avatar))
        l.append(Plugin.Option( 'remotefetch', bool, \
            _( 'Get cover art from web' ), _( 'Get cover art from web' ), \
            remotefetch ))
        custom = Plugin.Option('custom', dict, _('Custom config'), \
            _('Custom config'), None, self.player.customConfig)
        l.append(custom)
        self.configWindow = Plugin.ConfigWindow( _( 'Config Plugin' ), l )
        self.customConfigWidget = custom.widget
        response = self.configWindow.run()
        if status_timeout:
            gobject.source_remove(status_timeout)
        if response != None:
            if response.has_key( 'player' ):
                self.setPlayer( response[ 'player' ].value )
                self.config.setPluginValue( self.name, 'player', \
                    response[ 'player' ].value )
            if response.has_key( 'style' ):
                self.player.setStyle( response[ 'style' ].value )
                self.config.setPluginValue( self.name, 'style', \
                    response['style'].value )
            if response.has_key('avatar'):
                self.avatar = response['avatar'].value
                self.config.setPluginValue(self.name, 'avatar', \
                    str(int(response['avatar'].value)))
            if response.has_key('remotefetch'):
                self.remotefetch =  response[ 'remotefetch' ].value 
                self.config.setPluginValue( self.name, 'remotefetch', \
                    str(int(response[ 'remotefetch' ].value)))
            if response.has_key('custom'):
                self.player.customConfig = self.customConfigWidget.data
                for key in self.player.customConfig.keys():
                    self.config.setPluginValue(self.name, \
                        str(self.playername + "_" + key), \
                        self.player.customConfig[key])
                self.player.updateConfig()
        self.configWindow = None
        self.statusLabel = None
        return True
    def updateStatus( self ):
        '''updates the status label'''
        if hasattr(self.player, 'status'):
            status = self.player.status
        else:
            status = 'unknown'
        if self.statusLabel:
            self.statusLabel.set_markup(_('Status:') + ' <b>%s</b>' % status)
            return True
        else:
            return False
    def showLogs( self, button ):
        '''Display an error log window'''
        log = []
        if hasattr(self.player, '_log'):
            log = self.player._log
        win = gtk.Dialog( _('Logs'), self.configWindow, \
            gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT, \
            (gtk.STOCK_OK, gtk.RESPONSE_ACCEPT) )
        win.set_default_size(300, 400)
        model = gtk.ListStore(str, str)
        tree = gtk.TreeView(model)
        crtkey = gtk.CellRendererText()
        crtvalue = gtk.CellRendererText()
        col1 = gtk.TreeViewColumn(_("Type"), crtkey, text=0)
        col2 = gtk.TreeViewColumn(_("Value"), crtvalue, text=1)
        tree.append_column(col1)
        tree.append_column(col2)
        for item in log:
            model.append(item)
        win.vbox.pack_start(tree)
        win.show_all()
        win.run()
        win.destroy()
