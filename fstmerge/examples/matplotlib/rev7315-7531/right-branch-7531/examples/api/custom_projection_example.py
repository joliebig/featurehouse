from matplotlib.axes import Axes
from matplotlib import cbook
from matplotlib.patches import Circle
from matplotlib.path import Path
from matplotlib.ticker import Formatter, Locator, NullLocator, FixedLocator, NullFormatter
from matplotlib.transforms import Affine2D, Affine2DBase, Bbox, \
    BboxTransformTo, IdentityTransform, Transform, TransformWrapper
from matplotlib.projections import register_projection
import matplotlib.spines as mspines
import matplotlib.axis as maxis
import numpy as np
class HammerAxes(Axes):
    """
    A custom class for the Aitoff-Hammer projection, an equal-area map
    projection.
    http://en.wikipedia.org/wiki/Hammer_projection
    """
    name = 'hammer'
    def __init__(self, *args, **kwargs):
        Axes.__init__(self, *args, **kwargs)
        self.set_aspect(0.5, adjustable='box', anchor='C')
        self.cla()
    def _init_axis(self):
        self.xaxis = maxis.XAxis(self)
        self.yaxis = maxis.YAxis(self)
        self._update_transScale()
    def cla(self):
        """
        Override to set up some reasonable defaults.
        """
        Axes.cla(self)
        self.set_longitude_grid(30)
        self.set_latitude_grid(15)
        self.set_longitude_grid_ends(75)
        self.xaxis.set_minor_locator(NullLocator())
        self.yaxis.set_minor_locator(NullLocator())
        self.xaxis.set_ticks_position('none')
        self.yaxis.set_ticks_position('none')
        Axes.set_xlim(self, -np.pi, np.pi)
        Axes.set_ylim(self, -np.pi / 2.0, np.pi / 2.0)
    def _set_lim_and_transforms(self):
        """
        This is called once when the plot is created to set up all the
        transforms for the data, text and grids.
        """
        self.transProjection = self.HammerTransform()
        xscale = 2.0 * np.sqrt(2.0) * np.sin(0.5 * np.pi)
        yscale = np.sqrt(2.0) * np.sin(0.5 * np.pi)
        self.transAffine = Affine2D() \
            .scale(0.5 / xscale, 0.5 / yscale) \
            .translate(0.5, 0.5)
        self.transAxes = BboxTransformTo(self.bbox)
        self.transData = \
            self.transProjection + \
            self.transAffine + \
            self.transAxes
        self._xaxis_pretransform = \
            Affine2D() \
            .scale(1.0, np.pi) \
            .translate(0.0, -np.pi)
        self._xaxis_transform = \
            self._xaxis_pretransform + \
            self.transData
        self._xaxis_text1_transform = \
            Affine2D().scale(1.0, 0.0) + \
            self.transData + \
            Affine2D().translate(0.0, 4.0)
        self._xaxis_text2_transform = \
            Affine2D().scale(1.0, 0.0) + \
            self.transData + \
            Affine2D().translate(0.0, -4.0)
        yaxis_stretch = Affine2D().scale(np.pi * 2.0, 1.0).translate(-np.pi, 0.0)
        yaxis_space = Affine2D().scale(1.0, 1.1)
        self._yaxis_transform = \
            yaxis_stretch + \
            self.transData
        yaxis_text_base = \
            yaxis_stretch + \
            self.transProjection + \
            (yaxis_space + \
             self.transAffine + \
             self.transAxes)
        self._yaxis_text1_transform = \
            yaxis_text_base + \
            Affine2D().translate(-8.0, 0.0)
        self._yaxis_text2_transform = \
            yaxis_text_base + \
            Affine2D().translate(8.0, 0.0)
    def get_xaxis_transform(self,which='grid'):
        """
        Override this method to provide a transformation for the
        x-axis grid and ticks.
        """
        assert which in ['tick1','tick2','grid']
        return self._xaxis_transform
    def get_xaxis_text1_transform(self, pixelPad):
        """
        Override this method to provide a transformation for the
        x-axis tick labels.
        Returns a tuple of the form (transform, valign, halign)
        """
        return self._xaxis_text1_transform, 'bottom', 'center'
    def get_xaxis_text2_transform(self, pixelPad):
        """
        Override this method to provide a transformation for the
        secondary x-axis tick labels.
        Returns a tuple of the form (transform, valign, halign)
        """
        return self._xaxis_text2_transform, 'top', 'center'
    def get_yaxis_transform(self,which='grid'):
        """
        Override this method to provide a transformation for the
        y-axis grid and ticks.
        """
        assert which in ['tick1','tick2','grid']
        return self._yaxis_transform
    def get_yaxis_text1_transform(self, pixelPad):
        """
        Override this method to provide a transformation for the
        y-axis tick labels.
        Returns a tuple of the form (transform, valign, halign)
        """
        return self._yaxis_text1_transform, 'center', 'right'
    def get_yaxis_text2_transform(self, pixelPad):
        """
        Override this method to provide a transformation for the
        secondary y-axis tick labels.
        Returns a tuple of the form (transform, valign, halign)
        """
        return self._yaxis_text2_transform, 'center', 'left'
    def _gen_axes_patch(self):
        """
        Override this method to define the shape that is used for the
        background of the plot.  It should be a subclass of Patch.
        In this case, it is a Circle (that may be warped by the axes
        transform into an ellipse).  Any data and gridlines will be
        clipped to this shape.
        """
        return Circle((0.5, 0.5), 0.5)
    def _gen_axes_spines(self):
        return {'hammer':mspines.Spine.circular_spine(self,
                                                      (0.5, 0.5), 0.5)}
    def set_xscale(self, *args, **kwargs):
        if args[0] != 'linear':
            raise NotImplementedError
        Axes.set_xscale(self, *args, **kwargs)
    def set_yscale(self, *args, **kwargs):
        if args[0] != 'linear':
            raise NotImplementedError
        Axes.set_yscale(self, *args, **kwargs)
    def set_xlim(self, *args, **kwargs):
        Axes.set_xlim(self, -np.pi, np.pi)
        Axes.set_ylim(self, -np.pi / 2.0, np.pi / 2.0)
    set_ylim = set_xlim
    def format_coord(self, long, lat):
        """
        Override this method to change how the values are displayed in
        the status bar.
        In this case, we want them to be displayed in degrees N/S/E/W.
        """
        long = long * (180.0 / np.pi)
        lat = lat * (180.0 / np.pi)
        if lat >= 0.0:
            ns = 'N'
        else:
            ns = 'S'
        if long >= 0.0:
            ew = 'E'
        else:
            ew = 'W'
        return u'%f\u00b0%s, %f\u00b0%s' % (abs(lat), ns, abs(long), ew)
    class DegreeFormatter(Formatter):
        """
        This is a custom formatter that converts the native unit of
        radians into (truncated) degrees and adds a degree symbol.
        """
        def __init__(self, round_to=1.0):
            self._round_to = round_to
        def __call__(self, x, pos=None):
            degrees = (x / np.pi) * 180.0
            degrees = round(degrees / self._round_to) * self._round_to
            return u"%d\u00b0" % degrees
    def set_longitude_grid(self, degrees):
        """
        Set the number of degrees between each longitude grid.
        This is an example method that is specific to this projection
        class -- it provides a more convenient interface to set the
        ticking than set_xticks would.
        """
        number = (360.0 / degrees) + 1
        self.xaxis.set_major_locator(
            FixedLocator(
                np.linspace(-np.pi, np.pi, number, True)[1:-1]))
        self.xaxis.set_major_formatter(self.DegreeFormatter(degrees))
    def set_latitude_grid(self, degrees):
        """
        Set the number of degrees between each longitude grid.
        This is an example method that is specific to this projection
        class -- it provides a more convenient interface than
        set_yticks would.
        """
        number = (180.0 / degrees) + 1
        self.yaxis.set_major_locator(
            FixedLocator(
                np.linspace(-np.pi / 2.0, np.pi / 2.0, number, True)[1:-1]))
        self.yaxis.set_major_formatter(self.DegreeFormatter(degrees))
    def set_longitude_grid_ends(self, degrees):
        """
        Set the latitude(s) at which to stop drawing the longitude grids.
        Often, in geographic projections, you wouldn't want to draw
        longitude gridlines near the poles.  This allows the user to
        specify the degree at which to stop drawing longitude grids.
        This is an example method that is specific to this projection
        class -- it provides an interface to something that has no
        analogy in the base Axes class.
        """
        longitude_cap = degrees * (np.pi / 180.0)
        self._xaxis_pretransform \
            .clear() \
            .scale(1.0, longitude_cap * 2.0) \
            .translate(0.0, -longitude_cap)
    def get_data_ratio(self):
        """
        Return the aspect ratio of the data itself.
        This method should be overridden by any Axes that have a
        fixed data ratio.
        """
        return 1.0
    def can_zoom(self):
        """
        Return True if this axes support the zoom box
        """
        return False
    def start_pan(self, x, y, button):
        pass
    def end_pan(self):
        pass
    def drag_pan(self, button, key, x, y):
        pass
    class HammerTransform(Transform):
        """
        The base Hammer transform.
        """
        input_dims = 2
        output_dims = 2
        is_separable = False
        def transform(self, ll):
            """
            Override the transform method to implement the custom transform.
            The input and output are Nx2 numpy arrays.
            """
            longitude = ll[:, 0:1]
            latitude  = ll[:, 1:2]
            half_long = longitude / 2.0
            cos_latitude = np.cos(latitude)
            sqrt2 = np.sqrt(2.0)
            alpha = 1.0 + cos_latitude * np.cos(half_long)
            x = (2.0 * sqrt2) * (cos_latitude * np.sin(half_long)) / alpha
            y = (sqrt2 * np.sin(latitude)) / alpha
            return np.concatenate((x, y), 1)
        def transform_path(self, path):
            vertices = path.vertices
            ipath = path.interpolated(path._interpolation_steps)
            return Path(self.transform(ipath.vertices), ipath.codes)
        def inverted(self):
            return HammerAxes.InvertedHammerTransform()
        inverted.__doc__ = Transform.inverted.__doc__
    class InvertedHammerTransform(Transform):
        input_dims = 2
        output_dims = 2
        is_separable = False
        def transform(self, xy):
            x = xy[:, 0:1]
            y = xy[:, 1:2]
            quarter_x = 0.25 * x
            half_y = 0.5 * y
            z = np.sqrt(1.0 - quarter_x*quarter_x - half_y*half_y)
            longitude = 2 * np.arctan((z*x) / (2.0 * (2.0*z*z - 1.0)))
            latitude = np.arcsin(y*z)
            return np.concatenate((longitude, latitude), 1)
        transform.__doc__ = Transform.transform.__doc__
        def inverted(self):
            return HammerAxes.HammerTransform()
        inverted.__doc__ = Transform.inverted.__doc__
register_projection(HammerAxes)
from pylab import *
subplot(111, projection="hammer")
p = plot([-1, 1, 1], [-1, -1, 1], "o-")
grid(True)
show()
