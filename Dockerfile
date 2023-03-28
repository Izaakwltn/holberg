FROM clfoundation/sbcl:2.2.2

ARG QUICKLISP_DIST_VERSION=2022-02-20
ARG QUICKLISP_ADD_TO_INIT_FILE=true

WORKDIR /holberg

COPY holberg-app-suite.asd .
COPY /holberg-app-suite .
COPY holberg.asd .
COPY /src .
COPY otakar.asd .
COPY /otakar .
COPY seria.asd .
COPY /seria .
COPY holberg.asd .
COPY Makefile .
RUN true
COPY launch-suite .
RUN true

EXPOSE 4242

# RUN make

# RUN ./launch-suite

ENTRYPOINT ["./launch-suite"]

