FROM ocaml/opam:ubuntu-22.04-opam

RUN sudo apt install -y wget bubblewrap cmake make gcc unzip autoconf automake
RUN sudo apt install -y lsb-release software-properties-common gnupg

RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh

RUN sudo ./llvm.sh 17

RUN sudo apt install -y clang-17

RUN opam init -a
RUN eval $(opam env)
RUN opam switch create ocaml-base-compiler
RUN opam install dune
