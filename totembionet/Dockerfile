FROM colomoto/colomoto-docker-base:v1.3.1

USER root

RUN conda config --set ssl_verify false

RUN conda install python=3.7

RUN conda install -c anaconda requests

COPY classes/artifacts/smbionetjava_jar/smbionetjava.jar smbionetjava.jar

COPY smb-lib smb-lib
RUN pip install -e smb-lib

COPY save-experiences save-experiences
RUN pip install -e save-experiences

COPY discrete-model discrete-model
RUN pip install -e discrete-model

COPY model-picker model-picker
RUN pip install -e model-picker

COPY ggea ggea
RUN pip install -e ggea

COPY simu-net simu-net
RUN pip install -e simu-net

COPY tutorials /notebook/tutorials
RUN chown -R user:user /notebook

ADD http://nusmv.fbk.eu/distrib/NuSMV-2.6.0-linux64.tar.gz /notebook/tutorials
RUN tar xvfz /notebook/tutorials/NuSMV-2.6.0-linux64.tar.gz -C /notebook/tutorials && ln -s /notebook/tutorials/NuSMV-2.6.0-Linux/bin/NuSMV /usr/bin/


USER user
