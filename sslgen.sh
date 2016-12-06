#!/bin/bash

OUT="ssl"

mkdir -p $OUT
openssl req -nodes -new -x509 -keyout $OUT/key.key -out $OUT/cert.cert
