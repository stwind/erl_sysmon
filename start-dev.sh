#!/bin/sh
ERL=erl
COOKIE=monstats
HOST=127.0.0.1
CONFIG=priv/app.config
LIBS_DIR=deps
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

NODE_NAME=monstats@$HOST

exec erl -pa ebin \
    -boot start_sasl \
    -setcookie $COOKIE \
    -name $NODE_NAME \
    -env ERL_LIBS $LIBS_DIR \
    -config $CONFIG \
    -eval "application:ensure_all_started(monstats)"
