FROM ubuntu

RUN apt-get -qq -y update
RUN apt-get -qq -y upgrade

RUN apt-get install -qq -y sbcl make curl git

# Pull down Quicklisp and install it
RUN curl -s -o quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --no-sysinit --no-userinit --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "/home/janice/quicklisp")' \
         --eval '(ql:quickload :1am)' \
         --quit

# Set up .sbcl to load it:
RUN echo | sbcl --load /home/janice/quicklisp/setup.lisp --eval '(ql:add-to-init-file)' --quit

# Smoke test of Quicklisp:
RUN sbcl --non-interactive \
         --disable-debugger \
         --eval '(ql:quickload :cl-aa)'

WORKDIR /home/janice/quicklisp/local-projects
ENV LISP_HOME=/home/janice/quicklisp/local-projects

WORKDIR /home/janice/cl-oju

# Run the unit tests:
COPY . /home/janice/cl-oju
RUN make test
