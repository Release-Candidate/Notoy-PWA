// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Roland Csaszar
//
// Project:  notoy-pwa
// File:     gulpfile.js
// Date:     02.Dec.2021
//
// ==============================================================================
/* eslint-disable no-console */

// eslint-disable-next-line no-undef
const { series, parallel } = require("gulp")

// eslint-disable-next-line no-undef
const { src, dest } = require("gulp")

// eslint-disable-next-line no-undef
const { exec } = require("child_process")

// eslint-disable-next-line no-undef
const del = require("delete")

// eslint-disable-next-line no-undef
const replace = require("gulp-string-replace")

// eslint-disable-next-line no-undef
const fs = require("fs")

//==============================================================================
// Replace Version

function scanChangelogVersion() {
    let version = ""
    try {
        const data = fs.readFileSync("./CHANGELOG.md", "utf8")
        const match = data
            .toString()
            .match(/##\s+Version\s+(?<versionMatch>[0-9]+.[0-9]+.[0-9]+)/u)
        version = match.groups.versionMatch
    } catch (err) {
        // eslint-disable-next-line no-console
        console.log(err)
    }

    return version
}

function replaceVersion(dirName, version) {
    return src("./" + dirName + "/manifest.json")
        .pipe(
            replace(
                /"version":\s+"[0-9]+.[0-9]+.[0-9]+",/gu,
                '"version": "' + version + '",'
            )
        )
        .pipe(dest("./" + dirName))
}

function replaceVersionAssets() {
    return replaceVersion("assets", scanChangelogVersion())
}

//==============================================================================
// Run spago bundle.
function runSpago() {
    return exec(
        "spago bundle-app --main Main --to output/app.js",
        (error, stdout, stderr) => {
            if (error) {
                console.error(`exec error: ${error}`)
                return
            }
            console.log(`stdout: ${stdout}`)
            console.log(`stderr: ${stderr}`)
        }
    )
}

//==============================================================================
// Start HTTP server.
function runHTTP() {
    return exec("parcel --open --https", (error, stdout, stderr) => {
        if (error) {
            console.error(`exec error: ${error}`)
            return
        }
        console.log(`stdout: ${stdout}`)
        console.log(`stderr: ${stderr}`)
    })
}

//==============================================================================
// Run parcel-bundle on `index.html`.
function runParcel() {
    return exec("parcel --https", (error, stdout, stderr) => {
        if (error) {
            console.error(`exec error: ${error}`)
            return
        }
        console.log(`Parcel stdout: ${stdout}`)
        console.log(`Parcel stderr: ${stderr}`)
    })
}

//==============================================================================
// Delete generated files.
function delDirectory(dirName, cb) {
    del([dirName], cb)
}

function cleanDist(cb) {
    delDirectory("./dist/", cb)
    cb()
}

function cleanOutput(cb) {
    delDirectory("./output/", cb)
    cb()
}

// eslint-disable-next-line no-undef
exports.clean = parallel(cleanDist, cleanOutput)

// eslint-disable-next-line no-undef
exports.bundle = parallel(series(runSpago, runParcel), replaceVersionAssets)

// eslint-disable-next-line no-undef
exports.serve = parallel(
    series(runSpago, runParcel, runHTTP),
    replaceVersionAssets
)
