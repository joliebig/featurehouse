import gtk
import pango
import gobject
import desktop
class FtBarWidget(gtk.HBox):
    '''bar which represents active file transfers'''
    def __init__(self, controller, parentUI, parentConversation):
        gtk.HBox.__init__(self)
        self.set_spacing(3)
        self.controller = controller
        self.parentUI = parentUI
        self.parentConversation = parentConversation
        self.hbox = gtk.HBox()
        self.hbox.set_spacing(3)
        self.layout = gtk.Layout()
        self.layout.put(self.hbox, 0, 0)
        self.layout.set_size(self.hbox.get_allocation().width, \
                               self.hbox.get_allocation().height + 100)
        self.current = 0
        self.speed = 5
        self.page = 0
        self.twidth = 150
        self.num_transfers = 0
        arrowLeft = gtk.Arrow(gtk.ARROW_LEFT, gtk.SHADOW_IN)
        arrowRight = gtk.Arrow(gtk.ARROW_RIGHT, gtk.SHADOW_IN)
        self.btnLeft = gtk.Button()
        self.btnLeft.add(arrowLeft)
        self.btnLeft.set_sensitive(False)
        self.btnLeft.set_relief(gtk.RELIEF_NONE)
        self.btnLeft.connect("clicked", self.onbtnLeft)
        self.btnRight = gtk.Button()
        self.btnRight.add(arrowRight)
        self.btnRight.set_relief(gtk.RELIEF_NONE)
        self.btnRight.connect("clicked", self.onbtnRight)
        self.pack_start(self.btnLeft, False, False)
        self.pack_start(self.layout)
        self.pack_start(self.btnRight, False, False)
    def add(self, transfer):
        self.newBar = FtWidget(self.controller, self, transfer)
        transfer.ui = self.newBar
        self.hbox.pack_start(self.newBar, False, False)
        self.num_transfers += 1
        self.set_no_show_all(False)
        self.show_all()
    def onbtnLeft(self, widget):
        self.twidth = self.newBar.get_allocation().width
        self.page -= 1
        self.dest = -self.twidth * self.page
        gobject.timeout_add(5, self.onSmoothLeft)
    def onbtnRight(self, widget):
        self.twidth = self.newBar.get_allocation().width
        self.btnLeft.set_sensitive(True)
        self.page += 1
        self.dest = -self.twidth * self.page
        gobject.timeout_add(5, self.onSmoothRight)
    def onSmoothRight(self, *args):
        if self.current > self.dest:
            self.current -= self.speed
            self.layout.move(self.hbox, self.current, 0)
            return True
        return False
    def onSmoothLeft(self, *args):
        if self.current < self.dest:
            self.current += self.speed
            self.layout.move(self.hbox, self.current, 0)
            return True
        if self.dest == 0: self.btnLeft.set_sensitive(False)
        return False
class FtWidget(gtk.HBox):
    '''this class represents the ui widget for one filetransfer'''
    def __init__(self, controller, transferBar, transfer):
        gtk.HBox.__init__(self)
        self.controller = controller
        self.transferBar = transferBar
        self.transfer = transfer
        self.eventBox = gtk.EventBox()
        self.progress = gtk.ProgressBar()
        self.progress.set_ellipsize(pango.ELLIPSIZE_END)
        self.menu = gtk.Menu()
        fileImage = gtk.image_new_from_stock(gtk.STOCK_FILE, \
                                        gtk.ICON_SIZE_BUTTON)
        folderImage = gtk.image_new_from_stock(gtk.STOCK_OPEN, \
                                        gtk.ICON_SIZE_BUTTON)
        menuOpenFile = gtk.ImageMenuItem(_("Open file"))
        menuOpenFile.connect("activate", self.on_menu_file_clicked)
        menuOpenFile.set_image(fileImage)
        menuOpenDir = gtk.ImageMenuItem(_("Open folder"))
        menuOpenDir.connect("activate", self.on_menu_folder_clicked)
        menuOpenDir.set_image(folderImage)
        self.menu.add(menuOpenFile)
        self.menu.add(menuOpenDir)
        self.menu.show_all()
        self.eventBox.add(self.progress)
        self.pack_start(self.eventBox, False, False)
        self.pack_start(self.menu)
        self.buttons = []
        self.show_all()
        self.tooltip = FileTransferTooltip(self.eventBox, self.transfer)
        self.eventBox.connect('event', self.onProgressBarEvent)
        self.updateProgress()
        self.stateChanged()
    def onProgressBarEvent(self, widget, event):
        if event.type == gtk.gdk.BUTTON_PRESS:
            if self.transfer.state == self.transfer.RECEIVED:
                if event.button == 3:
                    self.menu.popup(None, None, None, event.button, event.time)
    def on_menu_file_clicked(self, widget):
        self.transfer.open()
    def on_menu_folder_clicked(self, widget):
        desktop.open(self.transfer.dirPath)
    def updateProgress(self):
        if self.transfer.state == self.transfer.RECEIVED:
            self.progress.set_fraction(1)  # 100%
        else:
            self.progress.set_fraction(self.transfer.getFraction())
        self.progress.set_text(self.transfer.getFilename())
        self.tooltip.update()
    def stateChanged(self):
        state = self.transfer.state
        for button in self.buttons:
            self.remove(button)
        self.buttons = []
        if state == self.transfer.WAITING and self.transfer.sender != 'Me':
            button = gtk.Button(None, None)
            button.set_image(self.getButtonImage(gtk.STOCK_APPLY))
            button.connect('clicked', self.onAcceptClicked)
            self.buttons.append(button)
        if state in (self.transfer.RECEIVED, self.transfer.FAILED):
            button = gtk.Button(None, None)
            button.set_image(self.getButtonImage(gtk.STOCK_CLEAR))
            button.connect('clicked', self.onCloseClicked)
            self.buttons.append(button)
        if state == self.transfer.WAITING or state == self.transfer.TRANSFERING:
            cancelButton = gtk.Button(None, None)
            cancelButton.connect('clicked', self.onCancelClicked)
            cancelButton.set_image(self.getButtonImage(gtk.STOCK_CANCEL))
            self.buttons.append(cancelButton)
        for button in self.buttons:
            self.pack_start(button, False, False)
        self.show_all()
        self.updateProgress()
    def getButtonImage(self, stockImage):
        img = gtk.Image()
        img.set_from_stock(stockImage, gtk.ICON_SIZE_MENU)
        return img
    def onCancelClicked(self, widget):
        self.transfer.cancel()
        self.transferBar.hbox.remove(self)
        self.transferBar.num_transfers -= 1
        if self.transferBar.num_transfers == 0:
            self.transferBar.hide()
    def onAcceptClicked(self, widget):
        self.transfer.accept()
    def onCloseClicked(self, widget):
        self.transfer.remove()
        self.transferBar.hbox.remove(self)
        self.transferBar.num_transfers -= 1
        if self.transferBar.num_transfers == 0:
            self.transferBar.hide()
DELAY = 500
class FileTransferTooltip(gtk.Window):
    '''Class that implements the filetransfer tooltip'''
    def __init__(self, parentWidget, transfer):
        gtk.Window.__init__(self, gtk.WINDOW_POPUP)
        self.transfer = transfer
        self.set_name('gtk-tooltips')
        self.set_position(gtk.WIN_POS_MOUSE)
        self.set_resizable(False)
        self.set_border_width(4)
        self.set_app_paintable(True)
        self.image = gtk.Image()
        self.details = gtk.Label()
        self.details.set_alignment(0, 0)
        self.table = gtk.Table(3, 2, False)
        self.table.set_col_spacings(5)
        self.addLabel(_('Status:'), 0, 1, 0, 1)
        self.addLabel(_('Average speed:'), 0, 1, 1, 2)
        self.addLabel(_('Time elapsed:'), 0, 1, 2, 3)
        self.status = gtk.Label()
        self.speed = gtk.Label()
        self.elapsed = gtk.Label()
        self.addLabel('', 1, 2, 0, 1, self.status)
        self.addLabel('', 1, 2, 1, 2, self.speed)
        self.addLabel('', 1, 2, 2, 3, self.elapsed)
        vbox = gtk.VBox(False, 5)
        vbox.pack_start(self.details, False, False)
        vbox.pack_start(self.table, False, False)
        hbox = gtk.HBox(False, 5)
        hbox.pack_start(self.image, False, False)
        hbox.pack_start(vbox, True, True)
        self.add(hbox)
        self.connect('expose-event', self.on_expose_event)
        parentWidget.connect('enter-notify-event', self.on_motion)
        parentWidget.connect('leave-notify-event', self.on_leave)
        self.mouseOverWidget = False
    def addLabel(self, s, left, right, top, bottom, label = None):
        if label == None:
            label = gtk.Label(s)
        label.set_alignment(0, 0)
        self.table.attach(label, left, right, top, bottom)
    def on_motion(self, view, event):
        self.mouseOverWidget = True
        eventCoords = (event.x_root, event.y_root, int(event.y))
        gobject.timeout_add(DELAY, self.show_tooltip, \
                                            view, eventCoords)
    def show_tooltip(self, view, origCoords):
        if not self.mouseOverWidget:
            return
        pixbuf = self.transfer.getPreviewImage()
        if pixbuf and pixbuf.get_height() <= 96 and pixbuf.get_width() <= 96:
            self.image.set_from_pixbuf(pixbuf)
        x, y = self.computePosition(origCoords, view.window)
        self.move(x, y)
        self.update()
        self.show_all()
        return False
    def update(self):
        self.details.set_markup('<b>' + self.transfer.getFilename() + '</b>')
        bps = self.transfer.getAverageSpeed()
        seconds = self.transfer.getElapsedTime()
        received, total = self.transfer.getBytes()
        percentage = int(self.transfer.getFraction() * 100)
        self.status.set_text('%d%% (%d/%d KB)' % (percentage, \
            int(received)/1024, int(total) / 1024))
        self.elapsed.set_text('%.2d:%.2d' % (int(seconds / 60), \
            int(seconds % 60)))
        self.speed.set_text('%.2f KiB/s' % (float(bps) / 1024.0))
    def on_leave(self, view, event):
        self.mouseOverWidget = False
        self.hide()
    def on_expose_event(self, tooltip_window, event):
        width, height = tooltip_window.get_size()
        tooltip_window.style.paint_flat_box(tooltip_window.window, \
                                            gtk.STATE_NORMAL, gtk.SHADOW_OUT, \
                                            None, tooltip_window, 'tooltip', \
                                            0, 0, width, height)
    def computePosition(self, origCoords, viewWindow):
        x_root, y_root, origY = origCoords
        currentY = viewWindow.get_pointer()[1]
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
