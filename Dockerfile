FROM rocker/shiny:latest

# Install system dependencies 
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/shiny/stopedata

COPY dependencies.R .
RUN R --slave --no-restore -e 'source("dependencies.R")'
# Set the working directory *inside the shiny home directory*


# Copy the rest of your application (app.R, R/, data/, www/, etc)
COPY . .


# Or lone the STOPeData repository
#RUN git clone https://github.com/NIVAnorge/STOPeData.git

# Fix permissions so the shiny user can read/write packages & app
RUN chown -R shiny:shiny /home/shiny/stopedata /usr/local/lib/R/site-library

# Switch to non-root shiny user
USER shiny

# Expose Shiny port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
