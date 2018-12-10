# -*- coding: utf8 -*-

from setuptools import setup, find_packages

setup(name="simu-net",
    version='0.0.3',
    author = "Alexandre Clement",
    author_email = "alexandre.clement@etu.unice.fr",
    url = "https://github.com/clement-alexandre/TotemBionet",
    description = "Simulation library for TotemBionet",
    long_description = open("README.rst").read(),
    install_requires = ['discrete-model', 'numpy', 'matplotlib >= 3.0.2'],
    extras_require = {"ipython": ["tabulate"]},
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