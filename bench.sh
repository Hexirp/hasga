#!/bin/bash

stack build --profile

stack exec -- hasga-exe +RTS -p -RTS
