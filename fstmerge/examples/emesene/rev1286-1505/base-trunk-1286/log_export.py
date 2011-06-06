'''module with functions to export conversation logs'''
import os
import sys
import time
import emesenelib.common
import plugins_base.Logger as Logger
RESULT_LIMIT = 10000
def parse_format(format):
    '''parse the format and return the style'''
    style = ''
    if format.find("FN=") != -1:
        font = format.split('FN=')[1].split(';')[0].replace('%20', ' ')
        style += 'font-family: ' + emesenelib.common.escape(font) + ';'
    if format.find("CO=") != -1:
        color = format.split('CO=')[1].split(';')[0]
        if len(color) == 3:
            color = color[2] + color[1] + color[0]
            style += 'color: #' + emesenelib.common.escape(color) + ';'
        else:
            color = color.zfill(6)
        if len(color) == 6:
            color = color[4:6] + color[2:4] + color[:2]
            style += 'color: #' + emesenelib.common.escape(color) + ';'
    if format.find("EF=") != -1:
        effect = set(format.split('EF=')[1].split(';')[0])
        if "B" in effect: 
            style += 'font-weight: bold;'
        if "I" in effect: 
            style += 'font-style: italic;'
        if "U" in effect: 
            style += 'text-decoration: underline;'
        if "S" in effect: 
            style += 'text-decoration: line-through;'
    return style
def format_message_plain(date, nick, message):
    '''return a plain text message'''
    (_format, encoding, text) = message.split('\r\n')
    return '[' + date + '] ' + nick + ': ' + text
def format_message_html(date, nick, message):
    '''return a plain text message'''
    (_format, encoding, text) = message.split('\r\n')
    style = parse_format(_format)
    formated_message =  '<span style="%s">%s</span>' % (style, text)
    return '[' + date + '] <em>' + nick + '</em>: ' + formated_message + '<br/>'
def export(filename, mail, format, out):
    '''export the conversations from the user with mail "mail" located
    on the logs in "filename" using the format "format" writing to
    the file-like object out (use sys.stdout to print or StringIO to
    get a string)'''
    if format == 'html':
        format_fun = format_message_html
    else:
        format_fun = format_message_plain
    if not os.access(filename, os.R_OK):
        out.write('cant read from file %s' % (filename,))
        return False
    logger = Logger.Logger(filename)
    put_new_line = True
    if format == 'html':
        out.write('<html><body>')
    for id_conversation in logger.get_conversation_ids(mail):
        if put_new_line:
            if format == 'html':
                out.write('<br/>')
            else:
                out.write('\n\n')
            put_new_line = False
            if export_conversation(logger, id_conversation, out
                , RESULT_LIMIT) > 0:
                put_new_line = True 
    if format == 'html':
        out.write('</body></html>')
    return True
def export_conversation(logger, id_conversation, out, msg_limit=10000, 
    format_fun=format_message_html):
    '''export one conversation, return the number of messages printed
    (0 means that this conversation was to request a DP or something)'''
    nick_cache = {}
    msg_count = 0
    for (stamp, mail, message) in logger.get_conversation(id_conversation, 
                                                        msg_limit):
        if mail in nick_cache:
            if nick_cache[mail]['next'] is not None and \
                nick_cache[mail]['next'] <= stamp:
                nick = logger.get_user_nick(mail, stamp)
                next = logger.get_next_nick_stamp(mail, stamp) 
                nick_cache[mail] = {'nick': nick, 'next' : next}
            else:
                nick = nick_cache[mail]['nick']
        else:
            nick = logger.get_user_nick(mail, stamp)
            next = logger.get_next_nick_stamp(mail, stamp) 
            nick_cache[mail] = {'nick': nick, 'next' : next}
        date = time.ctime(float(stamp))
        formated_message = format_fun(date, nick, message)
        out.write(formated_message)
        out.write('\n')
        msg_count += 1
    return msg_count
if __name__ == '__main__':
    if len(sys.argv) != 4:
        print 'USAGE: %s /path/to/log_file.db contact_mail format' % \
            (sys.argv[0],)
        print 'format: [html|txt]'
    export(sys.argv[1], sys.argv[2], sys.argv[3], sys.stdout)
