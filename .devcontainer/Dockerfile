ARG VARIANT="ubuntu-22.04"

FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get -y install --no-install-recommends \
    curl \
    sudo \
    wget \
    git \
    make \
    cmake \
    clang \
    lldb \
    lld \
    llvm \
    rsync \
    build-essential \
    lsb-release \
    pkg-config \
    software-properties-common \
    autoconf && apt-get clean

ENV PATH=${PATH}:/home/vscode/.local/bin
RUN sudo bash -c 'mkdir -p /home/vscode/.local/bin'
RUN sudo bash -c "chown -R vscode /home/vscode/.local/"
RUN sudo bash -c 'chmod -R a+rXw /home/vscode/.local'

RUN sudo bash -c 'curl -s https://api.github.com/repos/anoma/vamp-ir/releases/latest \
      | grep -a "browser_download_url" \
      | cut -d : -f 2,3 \
      | tr -d \" \
      | wget --output-document vamp-ir.tar.gz -qi - \
      && tar xfv vamp-ir.tar.gz --directory=/home/vscode/.local/bin/'

RUN sudo bash -c 'chmod +x /home/vscode/.local/bin/vamp-ir'
