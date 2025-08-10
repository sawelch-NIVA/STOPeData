FROM rocker/verse:4.5.1
RUN apt-get update && apt-get install -y  chromium cmake gdal-bin libcurl4-openssl-dev libgdal-dev libgeos-dev libicu-dev libpng-dev libproj-dev libsqlite3-dev libssl-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.8.1")'
RUN Rscript -e 'remotes::install_version("stringi",upgrade="never", version = "1.8.7")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.3.0")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.7")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.2.3")'
RUN Rscript -e 'remotes::install_version("xml2",upgrade="never", version = "1.3.8")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.5.2")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("shinytest2",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.9.4")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("shinyvalidate",upgrade="never", version = "0.1.3")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("rhandsontable",upgrade="never", version = "0.3.8")'
RUN Rscript -e 'remotes::install_version("rcrossref",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.11.0")'
RUN Rscript -e 'remotes::install_version("markdown",upgrade="never", version = "2.0")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.2.2")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("ellmer",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'remotes::install_version("bsicons",upgrade="never", version = "0.1.2")'
RUN Rscript -e 'remotes::install_version("bib2df",upgrade="never", version = "1.1.2.0")'
RUN Rscript -e 'remotes::install_version("arrow",upgrade="never", version = "21.0.0")'
RUN Rscript -e 'remotes::install_github("rstudio/shiny@51f653b66f7942a8f80d4f52ad0fb445bf9f964c")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(STOPeData);STOPeData::run_app()"
