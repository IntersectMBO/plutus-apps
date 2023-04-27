#!/usr/bin/env bash

# Download checkpoint files from S3 a local directory
# Example usage:
# CHECKPOINT_DIR=/tmp/checkpoint/ \
#   S3_CHECKPOINT_DIR=s3://<...> \
#   AWS_ACCESS_KEY_ID=<...> \
#   AWS_SECRET_ACCESS_KEY=<...> \
#   AWS_DEFAULT_REGION=<...> \
#   AWS_ENDPOINT_URL=https://s3.devx.iog.io \
#   ./scripts/s3-sync-checkpoint.sh

set -euo pipefail

mkdir -p "$CHECKPOINT_DIR"

set -x
aws --endpoint-url "$AWS_ENDPOINT_URL" s3 sync "$S3_CHECKPOINT_DIR" "$CHECKPOINT_DIR" --delete
{ set +x; } 2>/dev/null
