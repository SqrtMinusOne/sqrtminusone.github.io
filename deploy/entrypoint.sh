#!/usr/bin/env bash
CWD=/usr/share/nginx/html
(cd $CWD && git fetch origin && git checkout origin/gh-pages)
(cd /deploy && gunicorn -w 1 "deployer:create_app()") &
nginx -g 'daemon off;'
