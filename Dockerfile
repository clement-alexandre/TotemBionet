FROM colomoto/colomoto-docker-base:v1.3.1

USER root

RUN conda install --no-update-deps -y \
        -c alexandre-clement \
        ggea=0.0.2 \
        model-picker=0.0.4 \
        && conda clean -y --all && rm -rf /opt/conda/pkgs

RUN conda install --no-update-deps -y \
        -c mohamedchennouf\
        smb-lib=0.0.14 \
        && conda clean -y --all && rm -rf /opt/conda/pkgs

COPY tutorials /notebook/tutorials
RUN chown -R user:user /notebook

USER user

