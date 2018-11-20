# TotemBionet

## Quick usage guide

### Prerequirements

You need [Docker](http://docker.com).
Dockers is supported on GNU/Linux, macOS, and Windows.

Note: You may need to use sudo for the following commands, check [the official page](https://docs.docker.com/install/linux/linux-postinstall/) for more information.

### Installation

First, start the docker by executing in a terminal:

    make up

Then, open your browser and go to http://localhost:8888 for the Jupyter notebook web interface.  

Use `make stop` to stop the container and shutdown the server.

### Uninstall

If you want to remove the docker image from your computer, just run `make remove`

## Contribute

Add your project to the docker image in 4 steps !

1. Create a Python API for your application (if you used another language than Python).

2. Create a setup.py in your api and [upload it on Pypi](https://packaging.python.org/tutorials/packaging-projects/).

3. Create a [conda skeleton from Pypi](https://conda.io/docs/user-guide/tutorials/build-pkgs-skeleton.html), save it in the conda folder and [upload it on the conda repository](https://conda.io/docs/user-guide/tutorials/build-pkgs-skeleton.html#optional-uploading-packages-to-anaconda-org).

4. Add your conda dependency to the Dockerfile.

### Exemples

Coming soon, stay tuned !