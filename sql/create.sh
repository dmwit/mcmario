#!/bin/sh

set -e

createdb mcmario
psql mcmario <$(dirname "$0")/create.sql
