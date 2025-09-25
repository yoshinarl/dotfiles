#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd "${SCRIPT_DIR}/.." && pwd)
INSTALL_DIR="${REPO_ROOT}/npm/typescript-ls"

if ! command -v npm >/dev/null 2>&1; then
  echo "npm is required to install typescript-language-server" >&2
  exit 1
fi

if [ ! -f "${INSTALL_DIR}/package.json" ]; then
  echo "package.json not found at ${INSTALL_DIR}" >&2
  exit 1
fi

echo "Installing TypeScript tooling to ${INSTALL_DIR}"

# Install dependencies directly in the install directory
cd "${INSTALL_DIR}"
npm install

cat <<EOF
Installed TypeScript language tools under ${INSTALL_DIR}
Ensure ${INSTALL_DIR}/node_modules/.bin is on your PATH.
EOF
