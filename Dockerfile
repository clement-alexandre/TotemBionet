FROM colomoto/colomoto-docker-base:v1.3.1

USER root

RUN conda install --no-update-deps -y \
        -c alexandre-clement \
        ggea=0.0.2 \
        && conda clean -y --all && rm -rf /opt/conda/pkgs

USER user
