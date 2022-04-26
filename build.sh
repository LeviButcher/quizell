nix-build
rm -f site/*
nix-shell -p bash --run "cp result/bin/frontend.jsexe/* site"
cp frontend/quizell.css site