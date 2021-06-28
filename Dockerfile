FROM haskell:8.8.3

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install valgrind -y
