# TotemBionet

[![Build Status](https://travis-ci.com/clement-alexandre/TotemBionet.svg?branch=master)](https://travis-ci.com/clement-alexandre/TotemBionet)
[![codecov](https://codecov.io/gh/clement-alexandre/TotemBionet/branch/master/graph/badge.svg)](https://codecov.io/gh/clement-alexandre/TotemBionet)

## Prerequisites

You need [Docker](http://docker.com).
Dockers is supported on GNU/Linux, macOS, and Windows.

Note: You may need to use sudo for the following commands, check [the official page](https://docs.docker.com/install/linux/linux-postinstall/) for more information.

## Getting started

First, start the docker by executing in a terminal
    
    make up

Then, open your browser and go to http://localhost:8888 for the Jupyter notebook web interface.  

To stop the container and shutdown the server, use

    make stop

## Uninstall

If you want to remove the docker image from your computer, just run `make remove`

## Contribute

Check the [CONTRIBUTING.md](CONTRIBUTING.md) to contribute.

## License

[WTFPL](LICENSE)