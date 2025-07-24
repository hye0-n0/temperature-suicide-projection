# hash:sha256:ce0d4871776245aa971b24a9b1b466be6b7edc4c5f50772b0f2741d2d000eb35
FROM registry.codeocean.com/codeocean/py-r:python3.12.8-R4.4.2-JupyterLab4.3.5-RStudiorstudio-server-2024.12.1-563-ubuntu22.04

ARG DEBIAN_FRONTEND=noninteractive

# Install required R packages from CRAN
RUN R -e "install.packages(c(\
  'dplyr', 'tidyr', 'MASS', 'ggplot2', \
  'tsModel', 'dlnm', 'gnm', 'splines', \
  'mixmeta', 'mvmeta', 'purrr', 'reshape2', \
  'gridExtra', 'cowplot' \
))"

# Optional: install patchwork from GitHub if CRAN version not sufficient
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('thomasp85/patchwork')"
