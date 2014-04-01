# !/bin/sh
env ERL_LIBS=../deps:../.. erl +W w -config "example.config" -s confetti_app
