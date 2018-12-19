# coding: utf-8

import importlib
import subprocess
import sys
import os


def install(package):
    try:
        importlib.import_module(package)
    except ImportError:
        subprocess.call([sys.executable, "-m", "pip", "install", package], stdout=sys.stdout)


def build_html():
    dir_path = os.path.abspath(os.path.dirname(__file__))
    subprocess.call(["make", "-C", dir_path, "html"], stdout=sys.stdout)


def open_docs():
    from flask import Flask, send_from_directory
    
    app = Flask(__name__)

    @app.route('/', defaults={'path': 'index.html'})
    @app.route('/<path:path>')
    def catch_all(path):
        return send_from_directory(os.path.join('build', 'html'), path)

    app.run('0.0.0.0', port='8080')


def main():
    install('sphinx')
    install('sphinx_rtd_theme')
    install('flask')
    build_html()
    open_docs()


if __name__ == '__main__':
    main()

