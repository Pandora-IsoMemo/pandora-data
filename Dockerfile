FROM ghcr.io/pandora-isomemo/base-image:latest

RUN rm -rf bin && installPackage rmarkdown

ADD . .

RUN installPackage