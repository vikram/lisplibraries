#!/bin/sh
find . -name '*.fasl' -exec rm '{}' ';'
find . -name '*.x86f' -exec rm '{}' ';'
find . -name '*~' -exec rm '{}' ';'
find . -name '#*#' -exec rm '{}' ';'
