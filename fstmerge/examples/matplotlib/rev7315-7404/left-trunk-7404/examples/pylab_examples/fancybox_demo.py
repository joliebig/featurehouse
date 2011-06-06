import matplotlib.pyplot as plt
import matplotlib.transforms as mtransforms
from matplotlib.patches import FancyBboxPatch
bb = mtransforms.Bbox([[0.3, 0.4], [0.7, 0.6]])
def draw_bbox(ax, bb):
    p_bbox = FancyBboxPatch((bb.xmin, bb.ymin),
                            abs(bb.width), abs(bb.height), 
                            boxstyle="square,pad=0.",
                            ec="k", fc="none", zorder=10.,
                            )
    ax.add_patch(p_bbox)
def test1(ax):
    p_fancy = FancyBboxPatch((bb.xmin, bb.ymin),
                             abs(bb.width), abs(bb.height),
                             boxstyle="round,pad=0.1",
                             fc=(1., .8, 1.),
                             ec=(1., 0.5, 1.))
    ax.add_patch(p_fancy)
    ax.text(0.1, 0.8,
            r' boxstyle="round,pad=0.1"',
            size=10, transform=ax.transAxes)
    draw_bbox(ax, bb)
def test2(ax):
    p_fancy = FancyBboxPatch((bb.xmin, bb.ymin),
                             abs(bb.width), abs(bb.height),
                             boxstyle="round,pad=0.1",
                             fc=(1., .8, 1.),
                             ec=(1., 0.5, 1.))
    ax.add_patch(p_fancy)
    p_fancy.set_boxstyle("round,pad=0.1, rounding_size=0.2")
    ax.text(0.1, 0.8,
            ' boxstyle="round,pad=0.1\n rounding\\_size=0.2"',
            size=10, transform=ax.transAxes)
    draw_bbox(ax, bb)
def test3(ax):
    p_fancy = FancyBboxPatch((bb.xmin, bb.ymin),
                             abs(bb.width), abs(bb.height),
                             boxstyle="round,pad=0.1",
                             mutation_scale=2.,
                             fc=(1., .8, 1.),
                             ec=(1., 0.5, 1.))
    ax.add_patch(p_fancy)
    ax.text(0.1, 0.8,
            ' boxstyle="round,pad=0.1"\n mutation\\_scale=2',
            size=10, transform=ax.transAxes)
    draw_bbox(ax, bb)
def test4(ax):
    p_fancy = FancyBboxPatch((bb.xmin, bb.ymin),
                             abs(bb.width), abs(bb.height),
                             boxstyle="round,pad=0.2",
                             fc="none",
                             ec=(0., .5, 0.), zorder=4)
    ax.add_patch(p_fancy)
    p_fancy = FancyBboxPatch((bb.xmin, bb.ymin),
                             abs(bb.width), abs(bb.height),
                             boxstyle="round,pad=0.3",
                             mutation_aspect=.5, 
                             fc=(1., 0.8, 1.),
                             ec=(1., 0.5, 1.))
    ax.add_patch(p_fancy)
    ax.text(0.1, 0.8,
            ' boxstyle="round,pad=0.3"\n mutation\\_aspect=.5',
            size=10, transform=ax.transAxes)
    draw_bbox(ax, bb)
def test_all():
    plt.clf()
    ax = plt.subplot(2, 2, 1)
    test1(ax)
    ax.set_xlim(0., 1.)
    ax.set_ylim(0., 1.)
    ax.set_title("test1")
    ax.set_aspect(1.)
    ax = plt.subplot(2, 2, 2)
    ax.set_title("test2")
    test2(ax)
    ax.set_xlim(0., 1.)
    ax.set_ylim(0., 1.)
    ax.set_aspect(1.)
    ax = plt.subplot(2, 2, 3)
    ax.set_title("test3")
    test3(ax)
    ax.set_xlim(0., 1.)
    ax.set_ylim(0., 1.)
    ax.set_aspect(1)
    ax = plt.subplot(2, 2, 4)
    ax.set_title("test4")
    test4(ax)
    ax.set_xlim(-0.5, 1.5)
    ax.set_ylim(0., 1.)
    ax.set_aspect(2.)
    plt.draw()
    plt.show()
test_all()
