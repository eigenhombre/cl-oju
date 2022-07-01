FROM ubuntu

RUN apt-get -qq -y update
RUN apt-get -qq -y upgrade

RUN apt-get install -qq -y sbcl make curl git ecl

# Pull down Quicklisp and install it
RUN curl -s -o quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp

RUN ecl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "/home/janice/quicklisp")' \
         --eval '(ql:quickload :1am)'

RUN echo | ecl --load /home/janice/quicklisp/setup.lisp --eval '(ql:add-to-init-file)'
RUN echo | sbcl --load /home/janice/quicklisp/setup.lisp --eval '(ql:add-to-init-file)' --quit

ENV LISP_HOME=/home/janice/quicklisp/local-projects
WORKDIR /home/janice/cl-oju

# Run the unit tests:
COPY . /home/janice/cl-oju
RUN make test-ecl
RUN make test
