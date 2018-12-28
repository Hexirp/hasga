#!/bin/bash

stack build

stack exec -- hasga-exe +RTS -sstats -RTS
