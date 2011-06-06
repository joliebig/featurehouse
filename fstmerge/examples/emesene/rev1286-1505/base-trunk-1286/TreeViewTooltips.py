import gtk
import time
from gobject import timeout_add, source_remove
import status
DELAY = 500
GROUP_TOOLTIP = False
class TreeViewTooltips( gtk.Window ):
    '''Class that implements the tooltips shown in the user list'''
    def __init__( self, theme, view, info_col, mail_col, type_col=-1 ):
        gtk.Window.__init__( self, gtk.WINDOW_POPUP )
        self.view = view
        self.theme = theme
        self.info_col = info_col
        self.mail_col = mail_col
        self.type_col = type_col
        self.set_name('gtk-tooltips')
        self.set_position(gtk.WIN_POS_MOUSE)
        self.set_resizable(False)
        self.set_border_width(4)
        self.set_app_paintable(True)
        self.connect('expose-event', self.on_expose_event )
        self.contactLabel = gtk.Label('')
        self.contactLabel.set_line_wrap(True)
        self.contactLabel.set_alignment(0.5, 0.5)
        self.contactLabel.show()
        self.label = gtk.Label('')
        self.label.set_line_wrap(True)
        self.label.set_alignment(0, 0.5)
        self.label.set_use_markup(True)
        self.label.show()
        self.image = gtk.Image()
        self.dataString = '<span size="small">(%s)\n\n'
        self.dataString += _( 'Blocked: %s' ) + '\n'
        self.dataString += _( 'Has you: %s' ) + '\n'
        self.dataString += _( 'Has MSN Space: %s' ) + '\n'
        self.dataString += _( '%s' ) 
        self.dataString += '</span>'
        self.yesNo = { 'True' : _( 'Yes' ), 'False' : _( 'No' ) }
        hbox = gtk.HBox( spacing=6 )
        vbox = gtk.VBox()
        hbox.pack_start( self.image )
        hbox.pack_start( vbox )
        vbox.pack_start( self.label )
        hbox.show_all()
        self.add( hbox )
        self.connect( 'delete-event', self.reset )
        view.connect('motion-notify-event', self.on_motion )
        view.connect('leave-notify-event', self.on_leave )
        self.tag = None
        self.path_array = None
    def hideTooltip( self ):
        '''Hides the tooltip and removes any ongoing timeout for the tooltip
        to be shown'''
        self.hide()
        self.reset()
    def reset( self ):
        if self.tag and not self.tag == -1:
                source_remove( self.tag )
                self.tag = None
        self.path_array = None
    def on_motion( self, view, event ):
        x, y = int(event.x), int(event.y)
        path_array = view.get_path_at_pos( x, y )
        if not path_array:
            self.reset()
            self.hide()
            return
        if not GROUP_TOOLTIP:
            iterator = view.get_model().get_iter( path_array[ 0 ] )
            if self.type_col < 0:
                is_user = True
            else:
                row_type = view.get_model().get_value( iterator , self.type_col )
            if row_type != 'user':
                self.hide()
                self.reset()
                return
        if self.tag and ( path_array[0] == self.path_array ):
            return
        elif self.tag and not ( path_array[0] == self.path_array ):
            self.hide()
            self.path_array = path_array[0]
            source_remove( self.tag )
            eventCoords = ( event.x_root, event.y_root, y)
            self.tag = timeout_add( DELAY, self.show_tooltip, \
                                            view, eventCoords, \
                                            path_array )
        elif ( self.tag == -1 ) and ( path_array[0] == self.path_array ):
            return
        elif ( self.tag == -1 ) and not ( path_array[0] == self.path_array ):
            self.hide()
            self.path_array = path_array[0]
            eventCoords = ( event.x_root, event.y_root, y)
            self.tag = timeout_add( DELAY, self.show_tooltip, \
                                            view, eventCoords, \
                                            path_array )
        else:
            self.path_array = path_array[0]
            eventCoords = ( event.x_root, event.y_root, y)
            self.tag = timeout_add( DELAY, self.show_tooltip, \
                                            view, eventCoords, \
                                            path_array )
    def show_tooltip( self, view, origCoords, path_array ):
        self.tag = -1
        try:
            iterator = view.get_model().get_iter( path_array[ 0 ] )
            obj = view.get_model().get_value( iterator , self.info_col )
            mail = view.get_model().get_value( iterator , self.mail_col )
        except ValueError:
            self.reset()
            return
        if self.type_col < 0:
            is_user = True
        else:
            is_user = ( view.get_model().get_value( iterator , self.type_col ) == 'user' )
        text = self.view.getContactLabel(obj, showAlias=False, tooltip=True)
        text += '\n' + self.dataString % ( \
            obj.email, \
            self.yesNo[ str(obj.blocked) ], \
            self.yesNo[ str(obj.reverse) ], \
            self.yesNo[ str(obj.space) ], \
            self._get_last_status_since(obj.email))
        self.label.set_markup( text )
        if is_user:
            contact = self.view.controller.getContact(mail)
            if contact:
                pixbuf = self.theme.getUserDisplayPicture(contact)
                self.image.set_from_pixbuf(pixbuf)
                self.image.show()
            else:
                self.image.hide()
        elif GROUP_TOOLTIP:
            self.image.hide()
        else:
            self.tag = None
            return False
        x, y = self.computePosition( origCoords, view.window )
        self.move( x, y )
        self.show()
        return False
    def on_leave( self, view, event ):
        self.hide()
        self.reset()
    def on_expose_event( self, tooltip_window, event):
        width, height = tooltip_window.get_size()
        tooltip_window.style.paint_flat_box(tooltip_window.window, \
                                            gtk.STATE_NORMAL, gtk.SHADOW_OUT, \
                                            None, tooltip_window, 'tooltip', \
                                            0, 0, width, height)
    def computePosition( self, origCoords, viewWindow ):
        x_root, y_root, origY = origCoords
        currentY = viewWindow.get_pointer()[ 1 ]
        width, height = self.get_size()
        s_width, s_height = gtk.gdk.screen_width(), gtk.gdk.screen_height()
        x = int(x_root) - width/2
        if currentY >= origY:
            y = int(y_root) + 24
        else:
            y = int(y_root) + 6
        if x + width > s_width:
            x = s_width - width
        elif x < 0:
            x = 0
        if y + height > s_height:
            y = y - height - 24
        elif y < 0:
            y = 0
        return (x, y)
    def _get_last_status_since(self, account):
        '''return a string with the format %status% since: %date% if
        the Logger plugin is available and dont return an empty result'''
        logger = self.view.controller.pluginManager.getPlugin("Logger")
        if not logger or not logger.enabled:
            return ''
        results = logger.get_last_status(account)
        if len(results) == 0:
            return ''
        (stamp, result) = results[0]
        return _('%(status)s since: %(time)s') % {
            'status': status.STATUS[status.MSN_TO_STATUS[result]],
            'time': time.ctime(float(stamp))}
