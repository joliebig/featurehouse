'''module that implements abstract.ConfigDialog using Gtk'''
import gtk
from abstract.ConfigDialog import *
import dialog
BUILDERS = {}
def build_generic(element):
    '''build and return the element'''
    if type(element) in BUILDERS:
        return BUILDERS[type(element)](element)
    else:
        raise ValueError("unknown type of element")
def _text_cb(widget, element):
    '''method called when the value of the widget is changed, set the value
    of the text to the element'''
    element.value = widget.get_text()
def _checkbox_cb(widget, element):
    '''method called when the element is toggled, change the value of the 
    element'''
    element.value = widget.get_active()
def _radio_cb(widget, element):
    '''method called when a radio element is toggled, change the selected
    value to the identifier of that radio'''
    if not widget.get_active():
        return
    index = list(element.labels).index(widget.get_label())
    element.value = element.identifiers[index]
def build_text(element, spacing=2, border=2, visibility=True):
    '''build a Text object'''
    hbox = gtk.HBox(spacing=spacing)
    hbox.set_border_width(border)
    label = gtk.Label(element.label)
    entry = gtk.Entry()
    entry.set_text(element.value)
    entry.set_visibility(visibility)
    entry.connect("changed", _text_cb, element)
    hbox.pack_start(label, True, True)
    hbox.pack_start(entry, True, True)
    return hbox
def build_password(element, spacing=2, border=2):
    '''build a Text object'''
    return build_text(element, spacing, border, False) 
def build_checkbox(element):
    '''build a checkbox'''
    checkbox = gtk.CheckButton(element.label)
    checkbox.set_active(element.value)
    checkbox.connect("toggled", _checkbox_cb, element)
    return checkbox
def build_radiogroup(element, spacing=2, border=2):
    '''build a radio group'''
    first_radio = gtk.RadioButton(None, element.labels[0])
    first_radio.connect("toggled", _radio_cb, element)
    vbox = gtk.VBox(spacing=spacing)
    vbox.set_border_width(border)
    vbox.pack_start(first_radio, True, True)
    if element.selected_index == 0:
        first_radio.set_selected(True)
    for (index, label) in enumerate(element.labels[1:]):
        radio = gtk.RadioButton(first_radio, label)
        radio.connect("toggled", _radio_cb, element)
        if index + 1 == element.selected_index:
            radio.set_active(True)
        vbox.pack_start(radio, True, True)
    return vbox
def build_group(element, spacing=2, border=2):
    '''build a group'''
    vbox = gtk.VBox(spacing=spacing)
    vbox.set_border_width(border)
    for elem in element.items:
        vbox.pack_start(build_generic(elem))
    if element.label:
        frame = gtk.Frame(element.label)
        frame.add(vbox)
        return frame
    else:
        return vbox
def build_tab(element, spacing=2, border=2):
    '''build a tab'''
    vbox = gtk.VBox(spacing=spacing)
    vbox.set_border_width(border)
    for elem in element.items:
        vbox.pack_start(build_generic(elem))
    return vbox
def build_tabgroup(element, spacing=2, border=2):
    '''build a tabgroup'''
    notebook = gtk.Notebook()
    for tab in element.items:
        notebook.append_page(build_generic(tab), gtk.Label(tab.label))
    return notebook
def _ok_clicked_cb(button, window, element):
    '''called when the ok button is clicked'''
    if _on_close(element):
        window.hide()
def _close_cb(window, event, element):
    '''called when the dialog is closed, element is the root container
    will call to on_last_change to inform that is the last change'''
    if _on_close(element):
        window.hide()
    return True # stop the window from being destroyed (you can reuse the 
def _on_close(element):
    '''calidate the fields and display a dialog if something is not 
    valid and return False, if all validated, return True'''
    (validated, message, element) = element.validate()
    if not validated:
        tmp = {
            'identifier': element.identifier,
            'value': element.value,
            'message': message,
        }
        dialog.error(_("Field '%(identifier)s ('%(value)s') not valid\n" \
            "'%(message)s'" % tmp))
        return False
    else:
        element.on_last_change()
        return True
def build(element, spacing=2, border=2):
    '''build a window and return it'''
    window = gtk.Window()
    vbox = gtk.VBox(spacing=spacing)
    vbox.set_border_width(border)
    button_box = gtk.HButtonBox()
    button_box.set_layout(gtk.BUTTONBOX_END)
    ok_button = gtk.Button(stock=gtk.STOCK_OK)
    ok_button.connect("clicked", _ok_clicked_cb, window, element)
    button_box.pack_start(ok_button)
    vbox.pack_start(build_generic(element), True, True)
    vbox.pack_start(button_box, False)
    window.add(vbox)
    vbox.show_all()
    window.connect("delete-event", _close_cb, element)
    return window
BUILDERS[Text] = build_text
BUILDERS[Password] = build_password
BUILDERS[CheckBox] = build_checkbox
BUILDERS[RadioGroup] = build_radiogroup
BUILDERS[Group] = build_group
BUILDERS[Tab] = build_tab
BUILDERS[TabGroup] = build_tabgroup
