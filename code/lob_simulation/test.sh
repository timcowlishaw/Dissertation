#!/bin/bash
cabal-dev clean && cabal-dev configure --flags=test && cabal-dev build && dist/build/test_lob_simulation/test_lob_simulation
