#! /bin/sh

env ERL_LIBS=deps:.. erl -pa ebin -sname confetti -config etc/app.config -s confetti_app
