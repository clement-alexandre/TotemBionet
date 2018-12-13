# coding: utf-8

import webbrowser
import os


def main():
    dir_path = os.path.abspath(os.path.dirname(__file__))
    index = os.path.join(dir_path, 'build', 'html', 'index.html')

    webbrowser.open('file:///' + index)


if __name__ == '__main__':
    main()

