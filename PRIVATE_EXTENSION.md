# Private extension

## **Add your private projects to the docker image**


* ## Create a folder `echo` with the following structure

    ```bash
    echo
        ├── echo
        │   └── __init__.py
        └── setup.py
    ```

* ## Implementation in `echo.__init__.py`

    ```python
    # -*- coding: utf8 -*-

    def echo(string):
        print(string)

    __all__ = ['echo']
    ```

* ## Create a `setup.py`

    ```python
    # -*- coding: utf8 -*-

    from setuptools import setup, find_packages

    setup(
        name="echo",
        version='1.0',
        description = "Echo",
        packages = find_packages(),
    )
    ```

* ## Copy and install it in the Docker

    ```Dockerfile
    COPY echo echo
    RUN pip install -e echo
    ```

## Run `make up` and play with your dependency inside the Jupyter Notebook.

```python
import echo
echo.echo("Hello world !")
```
