# -*- coding: utf8 -*-

from setuptools import setup, find_packages

setup(name="discrete-model",
    version='0.1.6',
    author = "Alexandre Clement",
    author_email = "alexandre.clement@etu.unice.fr",
    url = "https://github.com/clement-alexandre/TotemBionet",
    description = "Discrete model for TotemBionet",
    long_description = open("README.rst").read(),
    install_requires = ["pandas", "graphviz"],
    extras_require = {},
    license="WTFPL",
    include_package_data = True,
    packages = find_packages(),
    test_suite='tests',
    classifiers=[
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering :: Bio-Informatics",
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7'
    ],
    keywords="jupyter, computational systems biology"
)