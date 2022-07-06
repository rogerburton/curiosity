#! /usr/bin/env bash

set -e

BASE_URL="http://127.0.0.1:9000"

# Try to login with no corresponding username.
# TODO With an existing user but wrong password.

# TODO
# This should return a proper HTML page.
# This should return 403 Forbidden ?

rm cookies.txt
curl --cookie-jar cookies.txt -d username="alice" -d password="secret" "${BASE_URL}/a/login"
echo
