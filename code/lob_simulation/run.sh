#!/bin/bash
cabal-dev clean && cabal-dev configure && cabal-dev build && dist/build/lob_simulation/lob_simulation
