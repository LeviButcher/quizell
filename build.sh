#!/usr/bin/env bash
# set -e

# export NP_GIT=$(which git)

# wget -nv https://github.com/DavHau/nix-portable/releases/download/v009/nix-portable
# chmod +x nix-portable

# ./nix-env -iA cachix -f https://cachix.org/api/v1/install
# # optional use of cache
# ./cachix use miso-haskell

# ./nix-portable nix-build
# The "result" symlink only valid inside the nix-portable sandbox
# ./nix-portable nix-shell -p bash --run "cp -rL result output"


nix-build
mkdir site
nix-shell -p bash --run "cp -u result/bin/frontend.jsexe/* site"
cp frontend/quizell.css site