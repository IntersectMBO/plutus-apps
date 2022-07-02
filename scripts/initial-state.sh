#!/usr/bin/env bash

set -x

mkdir -p "$LOCAL_DIR"

download_cmd="aws --endpoint-url \"$AWS_ENDPOINT_URL\" s3 sync \"$S3_PREFIX\" \"$LOCAL_DIR\" --exclude \"*\" --include \"*.state.bz2\""
eval "$download_cmd"

bunzip2 "$LOCAL_DIR"/*.state.bz2
