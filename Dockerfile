
FROM rstudio/plumber


RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libpng-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*


RUN R -e "install.packages(c('plumber','tidymodels','readr','dplyr','ranger','yardstick'))"


WORKDIR /app


COPY api.R /app/
COPY diabetes_binary_health_indicators_BRFSS2015.csv /app/


EXPOSE 8000


ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=8000)"]
