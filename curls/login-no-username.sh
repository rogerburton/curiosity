#! /usr/bin/env bash

set -e

BASE_URL="http://127.0.0.1:9000"

# Try to login with no corresponding username.
# TODO With an existing user but wrong password.

# This should return 401 Unauthorized.
# This should return a proper HTML page.

rm -f cookies.txt
curl -v --cookie-jar cookies.txt -d username="alice" -d password="secret" "${BASE_URL}/a/login"
echo
