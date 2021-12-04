#!/usr/bin/env bash

set -euo pipefail

dotnet publish --configuration Release -r osx-x64 --self-contained=true
