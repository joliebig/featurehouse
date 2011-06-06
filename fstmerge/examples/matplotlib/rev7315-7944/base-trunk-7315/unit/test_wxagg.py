import wx
import time
import matplotlib
matplotlib.use('Agg')
from matplotlib.figure import Figure
from matplotlib.transforms import Bbox, Point, Value
from matplotlib.backends.backend_agg import FigureCanvasAgg
from matplotlib.backends.backend_wxagg import _py_convert_agg_to_wx_image, \
    _py_convert_agg_to_wx_bitmap
import matplotlib.backends._wxagg as wxagg
TEST_PY  = 0
TEST_EXT = 0
TIME_PY  = 1
TIME_EXT = 1
ll_x = 320
ll_y = 240
ur_x = 640
ur_y = 480
BBOX = Bbox(Point(Value(ll_x), Value(ll_y)),
            Point(Value(ur_x), Value(ur_y)))
NITERS = 25
def time_loop(function, args):
    i = 0
    start = time.time()
    while i < NITERS:
        function(*args)
        i += 1
    return (time.time() - start)/NITERS
def make_figure():
    figure = Figure((6.4, 4.8), 100, frameon=False)
    canvas = FigureCanvasAgg(figure)
    return figure, canvas
def plot_sin(figure):
    from pylab import arange, sin, pi
    t = arange(0.0, 2.0, 0.01)
    s = sin(2*pi*t)
    axes = figure.gca()
    axes.plot(t, s, linewidth=1.0)
    axes.set_title('title')
def main():
    app = wx.PySimpleApp()
    figure, canvas = make_figure()
    bbox = None
    plot_sin(figure)
    canvas.draw()
    agg = canvas.get_renderer()
    if 0:
        print 'll.x =', BBOX.ll().x().get()
        print 'll.y =', BBOX.ll().y().get()
        print 'ur.x =', BBOX.ur().x().get()
        print 'ur.y =', BBOX.ur().y().get()
    if TEST_PY:
        i_py   = _py_convert_agg_to_wx_image( agg, None)
        b_py   = _py_convert_agg_to_wx_bitmap(agg, None)
        i_py_b = _py_convert_agg_to_wx_image( agg, BBOX)
        b_py_b = _py_convert_agg_to_wx_bitmap(agg, BBOX)
        i_py.SaveFile(  'a_py_img.png', wx.BITMAP_TYPE_PNG)
        b_py.SaveFile(  'a_py_bmp.png', wx.BITMAP_TYPE_PNG)
        i_py_b.SaveFile('b_py_img.png', wx.BITMAP_TYPE_PNG)
        b_py_b.SaveFile('b_py_bmp.png', wx.BITMAP_TYPE_PNG)
    if TEST_EXT:
        i_ext   = wxagg.convert_agg_to_wx_image( agg, None)
        b_ext   = wxagg.convert_agg_to_wx_bitmap(agg, None)
        i_ext_b = wxagg.convert_agg_to_wx_image( agg, BBOX)
        b_ext_b = wxagg.convert_agg_to_wx_bitmap(agg, BBOX)
        i_ext.SaveFile(  'a_ext_img.png', wx.BITMAP_TYPE_PNG)
        b_ext.SaveFile(  'a_ext_bmp.png', wx.BITMAP_TYPE_PNG)
        i_ext_b.SaveFile('b_ext_img.png', wx.BITMAP_TYPE_PNG)
        b_ext_b.SaveFile('b_ext_bmp.png', wx.BITMAP_TYPE_PNG)
    if TIME_PY:
        t = time_loop(_py_convert_agg_to_wx_image, (agg,None))
        print 'Python agg2img:        %.4f seconds (%.1f HZ)' % (t, 1/t)
        t = time_loop(_py_convert_agg_to_wx_bitmap, (agg,None))
        print 'Python agg2bmp:        %.4f seconds (%.1f HZ)' % (t, 1/t)
        t = time_loop(_py_convert_agg_to_wx_image, (agg,BBOX))
        print 'Python agg2img w/bbox: %.4f seconds (%.1f HZ)' % (t, 1/t)
        t = time_loop(_py_convert_agg_to_wx_bitmap, (agg,BBOX))
        print 'Python agg2bmp w/bbox: %.4f seconds (%.1f HZ)' % (t, 1/t)
    if TIME_EXT:
        t = time_loop(wxagg.convert_agg_to_wx_image, (agg,None))
        print '_wxagg agg2img:        %.4f seconds (%.1f HZ)' % (t, 1/t)
        t = time_loop(wxagg.convert_agg_to_wx_bitmap, (agg,None))
        print '_wxagg agg2bmp:        %.4f seconds (%.1f HZ)' % (t, 1/t)
        t = time_loop(wxagg.convert_agg_to_wx_image, (agg,BBOX))
        print '_wxagg agg2img w/bbox: %.4f seconds (%.1f HZ)' % (t, 1/t)
        t = time_loop(wxagg.convert_agg_to_wx_bitmap, (agg,BBOX))
        print '_wxagg agg2bmp w/bbox: %.4f seconds (%.1f HZ)' % (t, 1/t)
if __name__ == '__main__':
    main()
