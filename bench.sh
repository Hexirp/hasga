#!/bin/bash

stack build

stack exec -- hasga-exe +RTS -s stats.txt -RTS
