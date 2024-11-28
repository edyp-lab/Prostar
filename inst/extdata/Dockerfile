FROM bitnami/minideb as build

LABEL maintainer="Samuel Wieczorek samuel.wieczorek@cea.fr"

#ARG INVALIDATE_CACHE=0


# Set environment variables
ENV TZ="Europe/Paris"
ENV _R_SHLIB_STRIP_=true

WORKDIR /root

# Install base utilities
RUN apt-get update ; \
	apt-get install -y build-essentials ; \
	apt-get clean ; \
    rm -rf /var/lib/apt/lists/*;

RUN apt-get update ; \
	apt-get install -yqq --no-install-recommends --no-install-suggests wget make gcc g++ gfortran zlib1g-dev \
	libbz2-dev liblzma-dev libpcre2-dev default-jre libnetcdf-dev libcairo2 libxml2-dev libssl-dev \
	libpango1.0-dev libpango-1.0-0 libpng-dev cmake libicu-dev libxt-dev libgfortran-11-dev libcairo2-dev \
	libcurl4-openssl-dev libnlopt-dev libtirpc-dev tcl-dev tk-dev libtiff-dev libglpk-dev;

RUN wget https://cran.rstudio.com/src/base/R-4/R-4.4.1.tar.gz ; \
	tar zxvf R-4.4.1.tar.gz ; cd R-4.4.1 ; \
	./configure --enable-shared --with-readline=no --with-x --enable-R-shlib --with-tcltk ; \
	make; make install; \
	cd ..; \
	rm -Rf R-4.4.1 ; \
	rm R-4.4.1.tar.gz;


RUN Rscript -e 'install.packages("BiocManager", repo="https://cran.rstudio.com")'
RUN Rscript -e 'BiocManager::install(version = "3.20")'
RUN Rscript -e 'BiocManager::install("shinyTree")'
RUN Rscript -e 'BiocManager::install("igraph")'
RUN Rscript -e 'BiocManager::install("highcharter")'
RUN Rscript -e 'BiocManager::install("visNetwork")'
RUN Rscript -e 'BiocManager::install("vioplot")'
RUN Rscript -e 'BiocManager::install("markdown")'

RUN Rscript -e 'BiocManager::install("AnnotationDbi")'
RUN Rscript -e 'BiocManager::install("clusterProfiler")'
RUN Rscript -e 'BiocManager::install("R.utils")'
RUN Rscript -e 'BiocManager::install("graph")'
RUN Rscript -e 'BiocManager::install("FactoMineR")'

RUN Rscript -e 'BiocManager::install("vsn")'
RUN Rscript -e 'BiocManager::install("cluster")'
RUN Rscript -e 'BiocManager::install("diptest")'
RUN Rscript -e 'BiocManager::install("factoextra")'

RUN Rscript -e 'BiocManager::install("dendextend")'
RUN Rscript -e 'BiocManager::install("clusterProfiler")'
RUN Rscript -e 'BiocManager::install("foreach")'
RUN Rscript -e 'BiocManager::install("parallel")'
RUN Rscript -e 'BiocManager::install("doParallel")'

RUN Rscript -e 'BiocManager::install("Mfuzz")'
RUN Rscript -e 'BiocManager::install("apcluster")'
RUN Rscript -e 'BiocManager::install("forcats")'
RUN Rscript -e 'BiocManager::install("readxl")'
RUN Rscript -e 'BiocManager::install("openxlsx")'
RUN Rscript -e 'BiocManager::install("multcomp")'

RUN Rscript -e 'BiocManager::install("purrr")'
RUN Rscript -e 'BiocManager::install("tibble")'
RUN Rscript -e 'BiocManager::install("knitr")'
RUN Rscript -e 'BiocManager::install("norm")'
RUN Rscript -e 'BiocManager::install("scales")'
RUN Rscript -e 'BiocManager::install("tidyverse")'
RUN Rscript -e 'BiocManager::install("cp4p")'

RUN Rscript -e 'BiocManager::install("imp4p")'
RUN Rscript -e 'BiocManager::install("lme4")'
RUN Rscript -e 'BiocManager::install("dplyr")'
RUN Rscript -e 'BiocManager::install("stringr")'
RUN Rscript -e 'BiocManager::install("graph")'
RUN Rscript -e 'BiocManager::install("rclipboard")'

RUN Rscript -e 'BiocManager::install("tidyr")'
RUN Rscript -e 'BiocManager::install("impute")'
RUN Rscript -e 'BiocManager::install("gplots")'
RUN Rscript -e 'BiocManager::install("grDevices")'
RUN Rscript -e 'BiocManager::install("reshape2")'
RUN Rscript -e 'BiocManager::install("graphics")'
RUN Rscript -e 'BiocManager::install("stats")'

RUN Rscript -e 'BiocManager::install("methods")'
RUN Rscript -e 'BiocManager::install("ggplot2")'
RUN Rscript -e 'BiocManager::install("RColorBrewer")'
RUN Rscript -e 'BiocManager::install("Matrix")'
RUN Rscript -e 'BiocManager::install("future")'
RUN Rscript -e 'BiocManager::install("shinythemes")'
RUN Rscript -e 'BiocManager::install("colourpicker")'

RUN Rscript -e 'BiocManager::install("MsCoreUtils")'
RUN Rscript -e 'BiocManager::install("ProtGenerics")'
RUN Rscript -e 'BiocManager::install("mzR")'
RUN Rscript -e 'BiocManager::install("clue")'
RUN Rscript -e 'BiocManager::install("Rhdf5lib")'
RUN Rscript -e 'BiocManager::install("ncdf4")'


RUN Rscript -e 'BiocManager::install("pcaMethods")'
RUN Rscript -e 'BiocManager::install("MALDIquant")'
RUN Rscript -e 'BiocManager::install("mzID")'
RUN Rscript -e 'BiocManager::install("MSnbase")'
RUN Rscript -e 'BiocManager::install("shinyWidgets")'
RUN Rscript -e 'BiocManager::install("rhandsontable")'

RUN Rscript -e 'BiocManager::install("shinyBS")'
RUN Rscript -e 'BiocManager::install("shinyAce")'
RUN Rscript -e 'BiocManager::install("shinycssloaders")'
RUN Rscript -e 'BiocManager::install("webshot")'
RUN Rscript -e 'BiocManager::install("shinyjqui")'
RUN Rscript -e 'BiocManager::install("reticulate")'

RUN Rscript -e 'BiocManager::install("org.Sc.sgd.db")'
RUN Rscript -e 'BiocManager::install("clipr")'
RUN Rscript -e 'BiocManager::install("sass")'
RUN Rscript -e 'BiocManager::install("shiny")'
RUN Rscript -e 'BiocManager::install("remotes")'

# argument which has a different value at each build of the image
# so as to invalidate the cache for all the following instructions.
# This allows to update Prostar

RUN	Rscript -e 'remotes::install_github("edyp-lab/DAPARdata")'
RUN	Rscript -e 'remotes::install_github("edyp-lab/DAPAR")'
RUN	Rscript -e 'remotes::install_github("edyp-lab/Prostar")'


EXPOSE 3838

# Launch a script that will check whether Prostar is installed or not.
CMD R -e 'Sys.setenv("TZ"="Europe/Paris")'
CMD R -e 'shiny::runApp(system.file("ProstarApp",package="Prostar"), port=3838, host="0.0.0.0")'
#RUN /bin/bash
#CMD /bin/bash