FROM jupyter/base-notebook

USER root

ENV ROOT=$HOME/notebook

WORKDIR $ROOT

COPY src/requierements.txt src/requierements.txt

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        graphviz \
        openjdk-8-jre && \
    pip install --no-cache-dir -r src/requierements.txt && \
    apt clean -y && \
    rm -rf /var/lib/apt/lists/*

COPY requierements /notebook/requierements
RUN tar xvfz /notebook/requierements/NuSMV-2.6.0-linux64.tar.gz -C /notebook/requierements/ && \
    rm /notebook/requierements/NuSMV-2.6.0-linux64.tar.gz && \
    ln -s /notebook/requierements/NuSMV-2.6.0-Linux/bin/NuSMV /usr/bin/

RUN pip install rise && \
    jupyter-nbextension install rise --py --sys-prefix && \
    jupyter-nbextension enable rise --py --sys-prefix

COPY src src

RUN pip install --no-cache-dir -e src

COPY notebooks notebooks

COPY resources resources

RUN chown -R $NB_UID:$NB_GID $ROOT

USER $NB_UID

CMD ["start-notebook.sh", "--NotebookApp.token="]
