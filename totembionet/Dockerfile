FROM jupyter/base-notebook

USER root

ENV ROOT=/notebook

RUN mkdir $ROOT && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
        graphviz \
        openjdk-8-jre && \
    pip install --no-cache-dir \
                pandas \
                graphviz \
                networkx \
                pydot \
                numpy \
                matplotlib \
                pymongo && \
    apt clean -y && \
    rm -rf /var/lib/apt/lists/*

WORKDIR $ROOT

COPY tutorials/requierements $ROOT/tutorials/requierements/
RUN tar xvfz $ROOT/tutorials/requierements/NuSMV-2.6.0-linux64.tar.gz -C $ROOT/tutorials/requierements/ && \
    rm $ROOT/tutorials/requierements//NuSMV-2.6.0-linux64.tar.gz && \
    ln -s $ROOT/tutorials/requierements/NuSMV-2.6.0-Linux/bin/NuSMV /usr/bin/

COPY . .
RUN pip install --no-cache-dir \
        -e smb-lib \
        -e save-experiences \
        -e discrete-model \
        -e model-picker \
        -e ggea \
        -e simu-net && \
    chown -R $NB_UID:$NB_UID $ROOT

USER $NB_UID

CMD ["start-notebook.sh", "--NotebookApp.token="]
