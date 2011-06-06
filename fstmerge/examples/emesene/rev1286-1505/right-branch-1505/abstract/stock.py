'''a module to define abstract stock elements'''
ACCEPT = 1
ADD = 2
APPLY = 3
BACK = 4
BOLD = 5
CANCEL = 6
CLEAR = 7
CLOSE = 8
CONNECT = 9
DELETE = 10
DISCONNECT = 11
DOWN = 12
EDIT = 13
ERROR = 14
FORWARD = 15
INFORMATION = 16
ITALIC = 17
NEW = 18
NO = 19
OK = 20
OPEN = 21
PREFERENCES = 22
PROPERTIES = 23
QUESTION = 24
QUIT = 25
REFRESH = 26
REMOVE = 27
SAVE = 28
SELECT_COLOR = 29
SELECT_FONT = 30
STOP = 31
STRIKE = 32
UNDERLINE = 33
UP = 34
WARNING = 35
YES = 36
ABOUT = 37
COPY = 38
def map_stock(stock_id):
    '''map the abstract stock to a stock object of the toolkit, it doesn't 
    need to be implemented if the toolkit doesnt't use stock identifiers'''
    raise NotImplementedError
