FROM --platform=linux/amd64 rocker/geospatial:4.4.2

WORKDIR /home/rstudio/indo_pulp_defor

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