# -*- coding: utf8 -*-

from setuptools import setup, find_packages

setup(name="ggea",
    version='0.0.5',
    author = "Alexandre Clement",
    author_email = "alexandre.clement@etu.unice.fr",
    url = "https://github.com/clement-alexandre/TotemBionet",
    description = "Asynchronous State Graph Generator",
    long_description = open("README.rst").read(),
    install_requires = [
        "discrete-model",
        "matplotlib >= 3.0.0",
        "pydot",
        "networkx >= 2.0"
        ],
    extras_require = {
        "ipython": ["tabulate"],
    },
    license="WTFPL",
    include_package_data = True,
    packages = find_packages(),
    classifiers=[
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering :: Bio-Informatics",
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7'
    ],
    keywords="jupyter, computational systems biology",
)