# Matches the R version recorded in renv.lock (4.5.2).
FROM --platform=linux/amd64 rocker/geospatial:4.5.2

WORKDIR /home/rstudio/indo_pulp_defor

# System libraries beyond those already in rocker/geospatial (which ships gdal,
# geos, proj, udunits, fribidi and gfortran).
#   libnode-dev  - supplies the libv8 headers the V8 R package links against.
#                  Ubuntu noble renamed libv8-dev to libnode-dev.
# Typst compiles the SI section 9 concession atlas (R/06_si_concession_atlas.R).
# TeX Gyre Pagella is the free metric-compatible Palatino substitute, so the
# atlas matches the Word SI's typography on Linux where Palatino is absent.
ARG TYPST_VERSION=0.13.1
RUN apt-get update && apt-get install -y --no-install-recommends \
      fonts-texgyre poppler-utils libnode-dev \
    && rm -rf /var/lib/apt/lists/* \
    && ARCH="$(uname -m)" \
    && curl -fsSL "https://github.com/typst/typst/releases/download/v${TYPST_VERSION}/typst-${ARCH}-unknown-linux-musl.tar.xz" \
       | tar -xJ -C /tmp \
    && mv /tmp/typst-*/typst /usr/local/bin/typst \
    && rm -rf /tmp/typst-* \
    && typst --version

# renv.lock records a cross-platform CRAN snapshot, which serves *source* on
# Linux. Point restore at the matching Posit Package Manager Linux path so the
# same pinned versions arrive as prebuilt binaries: this turns a ~200-package
# source build into a download. The date must track the lockfile's repository.
ENV RENV_CONFIG_REPOS_OVERRIDE=https://p3m.dev/cran/__linux__/noble/2026-10-01

# Global renv library, outside the bind-mountable working directory
ENV RENV_PATHS_LIBRARY=/renv/library
RUN mkdir -p /renv/library

# Dependency definitions first, so edits to analysis code do not invalidate the
# package layer below
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY .Rprofile .Rprofile

# Restore at build time rather than on every container start, so the image
# ships ready to run and the result is cached as its own layer.
RUN Rscript -e 'options(renv.config.sandbox = FALSE); renv::restore(prompt = FALSE)'

# Rest of the codebase
COPY . .

# Execute targets pipeline when the container runs
CMD ["Rscript", "-e", "options(renv.config.sandbox = FALSE); targets::tar_make(callr_function = NULL)"]
