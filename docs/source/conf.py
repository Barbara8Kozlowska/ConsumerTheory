import os
import sys

sys.path.insert(0, os.path.abspath("../.."))

project = "ConsumerTheory"
author = "Your Name"
master_doc = "index"

extensions = [
    "sphinx.ext.mathjax",
    "sphinx.ext.autodoc",
    "sphinx.ext.napoleon"
]

html_theme = "sphinx_rtd_theme"

html_static_path = []
