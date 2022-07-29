#! /usr/bin/env bash

set -e

BASE_URL="http://127.0.0.1:9000"

# Create a new user.

curl -v --cookie-jar cookies.txt -d username="alice" -d password="a" -d email-addr="alice@example.com" "${BASE_URL}/a/signup"
echo

# Get logged in.

# This should return a 303 See Other and a Location.
# This should return a JWT-Cookie.

curl -v --cookie-jar cookies.txt -d username="alice" -d password="a" "${BASE_URL}/a/login"
echo
