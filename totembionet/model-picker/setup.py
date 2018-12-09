# -*- coding: utf8 -*-

from setuptools import setup, find_packages

setup(name="model-picker",
    version='0.0.4',
    author = "Alexandre Clement",
    author_email = "alexandre.clement@etu.unice.fr",
    url = "https://github.com/clement-alexandre/TotemBionet",
    description = "Pick a model",
    long_description = open("README.rst").read(),
    install_requires = [],
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