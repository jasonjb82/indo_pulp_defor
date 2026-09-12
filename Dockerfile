FROM --platform=linux/amd64 rocker/geospatial:4.4.2

WORKDIR /home/rstudio/indo_pulp_defor

# Typst compiles the SI section 9 concession atlas (R/06_si_concession_atlas.R).
# TeX Gyre Pagella is the free metric-compatible Palatino substitute, so the
# atlas matches the Word SI's typography on Linux where Palatino is absent.
ARG TYPST_VERSION=0.13.1
RUN apt-get update && apt-get install -y --no-install-recommends \
      fonts-texgyre poppler-utils \
    && rm -rf /var/lib/apt/lists/* \
    && ARCH="$(uname -m)" \
    && curl -fsSL "https://github.com/typst/typst/releases/download/v${TYPST_VERSION}/typst-${ARCH}-unknown-linux-musl.tar.xz" \
       | tar -xJ -C /tmp \
    && mv /tmp/typst-*/typst /usr/local/bin/typst \
    && rm -rf /tmp/typst-* \
    && typst --version

# Set up global renv library cache directory
ENV RENV_PATHS_LIBRARY=/renv/library
RUN mkdir -p /renv/library

# Copy dependency definition files first for Docker caching
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY .Rprofile .Rprofile

# Copy rest of codebase
COPY . .

# Execute targets pipeline when container runs
CMD ["Rscript", "-e", "options(renv.config.sandbox = FALSE); renv::restore(prompt = FALSE); targets::tar_make(callr_function = NULL)"]