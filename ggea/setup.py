# -*- coding: utf8 -*-

from setuptools import setup, find_packages

setup(name="ggea",
    version='0.0.1',
    author = "Alexandre Clement",
    author_email = "alexandre.clement@etu.unice.fr",
    url = "https://github.com/clement-alexandre/TotemBionet",
    description = "Asynchronous State Graph Generator",
    long_description = open("README.md").read(),
    extras_require = {
        "networkx": ["networkx >= 2.0", "pydot", "pygraphviz"],
        "ipython": ["tabulate"],
    },
    license="WTFPL",
    include_package_data = True,
    packages = find_packages(),
    py_modules = ["ggea"],
    classifiers=[
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering :: Bio-Informatics",
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7'
    ],
    keywords="jupyter, computational systems biology",
)