#!/usr/bin/env bash

env -u LC_ALL -u LC_CTYPE -u LC_NUMERIC -u LC_TIME -u LC_COLLATE -u LC_MONETARY \
    -u LC_MESSAGES -u LC_PAPER -u LC_NAME -u LC_ADDRESS -u LC_TELEPHONE \
    -u LC_MEASUREMENT -u LC_IDENTIFICATION -u LANG -u LANGUAGE \
    LC_ALL=C.UTF-8 LANG=C.UTF-8 \
    bash -c "$*" 2>&1 | grep -v "warning: setlocale"
exit ${PIPESTATUS[0]}