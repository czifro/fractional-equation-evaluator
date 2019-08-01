FROM microsoft/dotnet:2.1.403-sdk-stretch

WORKDIR /project

# Work around to problem with 'dotnet restore'

ENV LANGUAGE=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV LC_CTYPE=
ENV LC_MESSAGES=
ENV LC_COLLATE=
RUN apt-get update && apt-get install -y locales locales-all
RUN locale -a; \
    locale-gen en_US.UTF-8; \
    update-locale

COPY ./.git /project/.git
COPY ./src /project/src
COPY ./tests /project/tests
COPY ./FractionalEquationEvaluator.sln /project/FractionalEquationEvaluator.sln
COPY ./build.fsx /project/build.fsx
COPY ./paket.dependencies /project/paket.dependencies
COPY ./paket.lock /project/paket.lock
COPY ./tools /project/tools
COPY ./scripts /project/scripts

RUN sh tools/install.sh
RUN .paket/paket restore
RUN .fake-cli/fake build

CMD ["/bin/sh", "scripts/run.sh"]