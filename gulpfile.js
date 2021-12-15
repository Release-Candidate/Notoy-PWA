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
    return exec(
        "parcel --open",
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
// Run parcel-bundle on `index.html`.
function runParcel() {
    return exec("parcel build", (error, stdout, stderr) => {
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
exports.bundle = series(runSpago, runParcel)

// eslint-disable-next-line no-undef
exports.serve = series(runSpago, runParcel, runHTTP)
