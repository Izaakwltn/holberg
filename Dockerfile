FROM clfoundation/sbcl:2.2.2

ARG QUICKLISP_DIST_VERSION=2022-02-20
ARG QUICKLISP_ADD_TO_INIT_FILE=true

WORKDIR /holberg

# copy holberg-app-suite.asd .
# copy /holberg-app-suite .
# copy holberg.asd .
# copy /src .
# copy otakar.asd .
#copy /otakar .
# copy seria.asd .
# copy /seria .
# copy holberg.asd .
# copy Makefile .
# RUN true
COPY launch-suite .

EXPOSE 4242

# RUN ./launch-suite

ENTRYPOINT ["./launch-suite"]

