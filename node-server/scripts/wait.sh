#!/bin/bash

set -e

>&2 echo "Waiting for mysql"
cmd="$@"

until mysql -h"$DB_HOST" -P"$DB_PORT" -uroot -p"$DB_PASSWORD"; do
  >&2 echo "."
  sleep 1
done

>&2 echo "Mysql is up - executing command:"
>&2 echo $cmd
exec $cmd
