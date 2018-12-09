FROM colomoto/colomoto-docker-base:v1.3.1

USER root

RUN conda install python=3.7

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

USER user
