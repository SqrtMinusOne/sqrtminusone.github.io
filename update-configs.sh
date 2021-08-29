#!/usr/bin/env bash
(cd ./org/configs/ && emacs -Q --batch -l publish.el)
hugo -D
git push
