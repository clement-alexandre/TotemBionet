Prerequisites
=============

.. role:: bash(code)
   :language: bash

Install Docker
--------------



.. warning::

    The Docker daemon binds to a Unix socket instead of a TCP port.
    By default that Unix socket is owned by the user root and
    other userscan only access it using sudo. The Docker daemon
    always runs as the root user.

    If you don’t want to preface the docker command with sudo,
    create a Unix group called docker and add users to it.
    When the Docker daemon starts, it creates a Unix socket accessible
    by members of the docker group.

    Check the `docker official documentation`_ for more informations.

.. _docker official documentation: https://docs.docker.com/install/linux/linux-postinstall/


Install Docker Compose
----------------------

.. tip:: You can easily install docker compose via pip: :bash:`pip install docker-compose`

Otherwise, check the `docker-compose official documentation`_.

.. _docker-compose official documentation: https://docs.docker.com/compose/install/


Install pip
-----------

Check the `pip official documentation`_ to install pip.

.. _pip official documentation: https://packaging.python.org/guides/installing-using-linux-tools/#installing-pip-setuptools-wheel-with-linux-package-managers
