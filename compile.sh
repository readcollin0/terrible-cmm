#!/bin/bash

sbcl --non-interactive --load parser.lsp --eval "(full-compile \"$1\" \"$2\" :init-stack 16384 :trim-functions t)"
