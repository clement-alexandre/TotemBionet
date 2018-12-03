# TotemBionet

[![Build Status](https://travis-ci.com/clement-alexandre/TotemBionet.svg?branch=master)](https://travis-ci.com/clement-alexandre/TotemBionet)

[![codecov](https://codecov.io/gh/clement-alexandre/TotemBionet/branch/master/graph/badge.svg)](https://codecov.io/gh/clement-alexandre/TotemBionet)

## Quick usage guide

### Prerequirements

You need [Docker](http://docker.com).
Dockers is supported on GNU/Linux, macOS, and Windows.

Note: You may need to use sudo for the following commands, check [the official page](https://docs.docker.com/install/linux/linux-postinstall/) for more information.

### Installation

First, start the docker by executing in a terminal: `make up`

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

### Examples

#### Prerequirements

First, you will need to install [Python](https://www.python.org/downloads/).
Then run
* `apt-get install python3-pip`
* `pip install setuptools wheel twine`

You will also need [Anaconda](https://www.anaconda.com/download/#linux) and add conda to your path.
Then run
* `conda install anaconda-client`

For more help with anaconda, check [this](https://conda.io/docs/user-guide/install/linux.html).

You need to have an account on [Pypi](https://pypi.org/account/register/) and on [Anaconda Cloud](https://anaconda.org/).

#### 1. Create your Python library/API

First we create a folder "model_picker" as follow:

```bash
model-picker
    ├── model_picker
    │   ├── __init__.py
    │   └── model_picker.py
    ├── tests
    │   └── test.py
    ├── LICENSE.txt
    ├── README.rst
    ├── setup.cfg
    └── setup.py
```

* We put our implementation in the "model-picker/model_picker/model_picker.py" file:

```python
import random

def pick_a_model_randomly(models: List[Any]):
    return random.choice(models)
```

* And we had the function in our __init__.py to ease the use of our library.

```python
from .model_picker import pick_a_model_randomly

__all__ = ['pick_a_model_randomly']
```

So now, when someone uses 
```python 
import model_picker
```
He can directly have access to the method by calling 
```python 
model_picker.pick_a_model_randomly()
```

#### 2. Create a setup.py to be able to push your library on the pypi repository

* **`setup.py`**

```python
from setuptools import setup, find_packages

setup(name="model_picker",
    version='0.0.1',
    author = "Alexandre Clement",
    author_email = "alexandre.clement@etu.unice.fr",
    url = "https://github.com/clement-alexandre/TotemBionet",
    description = "Pick a model",
    long_description = open("README.rst").read(),
    license="WTFPL",
    include_package_data = True,
    packages = find_packages()
)
```

To be able to upload your projet to Pypi, you will also need a LICENSE.txt with the license of your project and a `setup.cfg` file.
The `setup.cfg` contains the informations for your project to work properly like non Python required or documentations. In our case, we will just add the Readme.

* **`setup.cfg`**
```
[metadata]
description-file = README.rst
```

For more information, check [this link](https://docs.python.org/3.7/distutils/configfile.html).

Finally, add a License and a Readme to your project.

* You are now ready to upload your project on Pypi

First, run `python3 setup.py sdist bdist_wheel` to build your project. Then run `twine upload dist/*` to upload it on Pypi.

Great, our project is now available on Pypi ! https://pypi.org/project/model-picker/

#### 3. Upload your project on Anaconda Cloud.

* First, we will generate the `meta.yaml` file required for the upload on Anaconda. Go to the folder "conda" and execute `conda skeleton pypi model-picker`. Change the maintainers in the extra and that's all !

```yaml
extra:
  recipe-maintainers:
    - your-github-id-here
```

* Now run `conda-build model-picker` to create a conda package from your pypi project.

* Finally, login to anaconda with `anaconda login` and upload the generated package by executing `anaconda upload ~/miniconda3/conda-bld/linux-64/model-picker-0.0.1-py37_0.tar.bz2`

Nice, you can check your project on the Anaconda Cloud : https://anaconda.org/alexandre-clement/model-picker

#### 4. The final part: add your conda dependency to the Dockerfile

In the Dockerfile, the dependencies are grouped by uploaders. So you have a `RUN conda install` for each different uploader among your dependencies. The template looks like this :

```Dockerfile
RUN conda install --no-update-deps -y \
        -c $(UPLOADER_NAME)\
        $(APP_NAME)=$(VERSION) \
        && conda clean -y --all && rm -rf /opt/conda/pkgs
```

For our example, we just add our `model-picker` dependency under the ggea since there are both from the same uploader.

```Dockerfile
RUN conda install --no-update-deps -y \
        -c alexandre-clement \
        ggea=0.0.2 \
        model-picker=0.0.1 \
        && conda clean -y --all && rm -rf /opt/conda/pkgs
```

Congratulations, you added your project to **TotemBionet** !
Run `make up` and play with your dependency inside the Jupyter Notebook.

Feel free to add a Jupyter Notebook in the tutorial folder to help the other programmers use your library.

![model-picker](docs/source/static/model-picker.png)


