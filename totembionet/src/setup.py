# coding: utf8

from setuptools import setup, find_packages


setup(
    name='totembionet',
    install_requires = [
        'pandas',
        'graphviz',
        'networkx',
        'pydot',
        'numpy',
        'matplotlib >= 3.0.2'
    ],
    packages = find_packages(exclude=("tests",)),
    include_package_data=True
)