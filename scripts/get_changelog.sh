#!/bin/bash
# SPDX-License-Identifier: GPL-3.0-only
# Copyright (C) 2021 Roland Csaszar
#
# Project:  notoy-browser_extensions
# File:     get_changelog.sh
# Date:     01.Oct.2021
###############################################################################

# Returns the newest part of the changelog `CHANGELOG.md`.
# For use with automatic releases.

# First argument is the version as version tag, like "v0.9.3"
VERSION_FROM_TAG=${1#v}

# Path to the changelog to parse
CHANGELOG_PATH="../CHANGELOG.md"

VERSION=$(grep '##' ${CHANGELOG_PATH} | tr -s ' ' | cut -d ' ' -f 3 | head -1)

if [ "${VERSION}" != "${VERSION_FROM_TAG}" ]
then
    echo "ERROR: versions ${VERSION} and ${VERSION_FROM_TAG} differ!"
    exit 1
fi

LINE_NUMS=$(grep '##' ${CHANGELOG_PATH} -n| head -2|cut -f1 -d":"|paste -s -d' ')

LINE_NUM_ARRAY=(${LINE_NUMS})

if [ "${#LINE_NUM_ARRAY[@]}" -lt "1" ]
then
    echo ""
elif [ "${#LINE_NUM_ARRAY[@]}" -lt "2" ]
then
    tail +${LINE_NUM_ARRAY[0]} ${CHANGELOG_PATH}
else
    head -$((${LINE_NUM_ARRAY[1]} - 1)) ${CHANGELOG_PATH} | tail +${LINE_NUM_ARRAY[0]}
fi
