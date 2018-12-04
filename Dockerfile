FROM colomoto/colomoto-docker-base:v1.3.1

USER root

RUN conda install --no-update-deps -y \
        -c alexandre-clement \
        ggea=0.0.5 \
        model-picker=0.0.4 \
        discrete-model=0.1.1 \
        simu-net=0.0.2 \
        && conda clean -y --all && rm -rf /opt/conda/pkgs

RUN conda install --no-update-deps -y \
        -c mohamedchennouf\
        smb-lib=0.1.6 \
        save-experiences=0.0.1 \
        && conda clean -y --all && rm -rf /opt/conda/pkgs

COPY tutorials /notebook/tutorials
RUN chown -R user:user /notebook

USER user
