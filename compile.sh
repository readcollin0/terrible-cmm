#!/bin/bash

sbcl --non-interactive --load parser.lsp --eval "(full-compile \"$1\" \"$2\" :preproc-out \"$2.pre\")"
