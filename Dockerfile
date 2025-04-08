FROM continuumio/anaconda3:2021.11

MAINTAINER "Coxx-nnet-v2" lana.garmire.group@gmail.com

ENV DEBIAN_FRONTEND noninteractive
ENV TZ America/New_York

RUN apt-get update
RUN apt-get install -y apt-utils
RUN apt-get install -y build-essential
RUN apt-get update

WORKDIR /usr/src/app

RUN apt install -y software-properties-common
RUN apt-get update

RUN apt-get install -y libcurl3-gnutls
RUN apt-get install -y --fix-missing git

RUN apt install -y dirmngr gnupg apt-transport-https ca-certificates 

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys '95C0FAF38DB3CCAD0C080A7BDC78B2DDEABC47B7'
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/debian bullseye-cran40/"

RUN apt-get update

RUN apt-get install -y r-base-core r-base r-base-dev r-recommended
RUN apt-get install -y libcurl4-gnutls-dev libcurl4-gnutls-dev libssl-dev

RUN R -e 'install.packages("BiocManager")'
RUN R -e 'install.packages("remotes")'

RUN R -e 'install.packages(c("dplyr", "tibble", "tidyr", "purrr"))'
RUN R -e 'install.packages(c("ggplot2"))'
RUN R -e 'install.packages(c("limma"))'
RUN R -e 'install.packages(c("reshape2"))'
RUN R -e 'install.packages(c("jsonlite"))'
RUN R -e 'install.packages(c("base64enc"))'
RUN R -e 'install.packages(c("shiny"))'
RUN R -e 'install.packages(c("DT"))'
RUN R -e 'install.packages(c("glmnet"))'
RUN R -e 'install.packages(c("data.table"))'


RUN apt-get -y install cmake
RUN R -e 'install.packages(c("survminer"))'

RUN R -e 'install.packages("reticulate")'

RUN apt-get install -y libhdf5-dev
RUN conda install pygpu

RUN pip install wheel
RUN pip install pathlib
RUN pip install ansicolors
RUN pip install numpy
RUN pip install theano
RUN pip install scikit-learn
RUN pip install xlrd
RUN pip install colour
RUN pip install h5py

RUN wget https://developer.download.nvidia.com/compute/cuda/repos/debian11/x86_64/cuda-keyring_1.0-1_all.deb
RUN dpkg -i cuda-keyring_1.0-1_all.deb

RUN add-apt-repository "deb https://developer.download.nvidia.com/compute/cuda/repos/debian11/x86_64/ /"
RUN add-apt-repository contrib
RUN apt update

# RUN apt-get install -y cuda

ENV THEANO_FLAGS="device=cpu,floatX=float32"


WORKDIR /usr/src/app

COPY . .
EXPOSE 8091

CMD R -e "shiny::runApp('./', quiet=FALSE)"
