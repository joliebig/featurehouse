import sys, os
sys.path.append(os.path.abspath('sphinxext'))
import ipython_console_highlighting
extensions = ['matplotlib.sphinxext.mathmpl', 'math_symbol_table',
              'sphinx.ext.autodoc', 'matplotlib.sphinxext.only_directives',
              'matplotlib.sphinxext.plot_directive', 'inheritance_diagram',
              'gen_gallery', 'gen_rst']
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'contents'
project = 'Matplotlib'
copyright = '2008, John Hunter, Darren Dale, Michael Droettboom'
import matplotlib
version = matplotlib.__version__
release = version
today_fmt = '%B %d, %Y'
unused_docs = []
pygments_style = 'sphinx'
plot_formats = ['png', 'hires.png', 'pdf']
html_style = 'mpl.css'
html_static_path = ['_static']
html_file_suffix = '.html'
html_last_updated_fmt = '%b %d, %Y'
html_index = 'index.html'
html_sidebars = {'index': 'indexsidebar.html',
                 }
html_additional_pages = {'index': 'index.html', 'gallery':'gallery.html'}
html_use_opensearch = 'False'
htmlhelp_basename = 'Matplotlibdoc'
latex_paper_size = 'letter'
latex_font_size = '11pt'
latex_documents = [
  ('contents', 'Matplotlib.tex', 'Matplotlib', 'Darren Dale, Michael Droettboom, Eric Firing, John Hunter', 'manual'),
]
latex_logo = None
latex_preamble = """
   \usepackage{amsmath}
   \usepackage{amsfonts}
   \usepackage{amssymb}
   \usepackage{txfonts}
"""
latex_appendices = []
latex_use_modindex = True
latex_use_parts = True
autoclass_content = 'both'
