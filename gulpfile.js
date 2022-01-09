// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Roland Csaszar
//
// Project:  notoy-pwa
// File:     gulpfile.js
// Date:     02.Dec.2021
//
// ==============================================================================
/* eslint-disable no-console */

//==============================================================================
// Directories and files.

// Path to HTTPS certificate and key
const httpsCertificate = "../https_cert.pem"
const httpsCertificateKey = "../https_cert-key.pem"

// HTTPS port to use
const httpsPort = 1234

// Service worker, JS file.
const serviceWorkerJS = "./src/service_worker/sw.js"

// App JS file.
const appJS = "app.js"

// Manifest filename (original).
const manifestJSON = "manifest.json"

// Changelog file.
const changelogPath = "./CHANGELOG.md"

// Directory holding the assets.
const assetDir = "./assets"

// Source directory for the bundler, copy every source to this dir.
const outDir = "./http"

// Output directory of compiled PureScript files.
const spagoOutdir = "./output"

// Destination directory of the bundler, the source directory for the HTTPS
// Server.
const serveDir = outDir

//==============================================================================
// JS requires

// eslint-disable-next-line no-undef
const { series, parallel, src, dest } = require("gulp")

// eslint-disable-next-line no-undef
const { exec } = require("child_process")

// eslint-disable-next-line no-undef
const del = require("delete")

// eslint-disable-next-line no-undef
const replace = require("gulp-string-replace")

// eslint-disable-next-line no-undef
const connect = require("gulp-connect")

// eslint-disable-next-line no-undef
const gulpEsbuild = require("gulp-esbuild")

// eslint-disable-next-line no-undef
const fs = require("fs")

// eslint-disable-next-line no-undef
const filelist = require("filelist")

//==============================================================================
// Generate a timestamp of the current date and time
function generateTimestamp() {
    function pad0s(n) {
        // eslint-disable-next-line no-magic-numbers
        return n < 10 ? "0" + n : n
    }
    const nowDate = new Date()

    return (
        nowDate.getFullYear() +
        // eslint-disable-next-line no-magic-numbers
        pad0s(nowDate.getMonth() + 1) +
        pad0s(nowDate.getDate()) +
        nowDate.getHours() +
        nowDate.getMinutes() +
        nowDate.getSeconds()
    )
}

//==============================================================================
// Replace Version

function scanChangelogVersion() {
    let version = ""
    try {
        const data = fs.readFileSync(changelogPath, "utf8")
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
    return src(dirName + "/" + manifestJSON)
        .pipe(
            replace(
                /"version":\s+"[0-9]+.[0-9]+.[0-9]+",/gu,
                '"version": "' + version + '",'
            )
        )
        .pipe(dest(dirName))
}

function replaceVersionOutdir() {
    return replaceVersion(outDir, scanChangelogVersion())
}

//==============================================================================
// Run spago bundle.
function runSpago() {
    return exec(
        `spago bundle-app --main Main --to ${outDir}/${appJS}`,
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
// Start HTTPS server.
function runHTTPS() {
    connect.server({
        root: serveDir,
        https: {
            key: fs.readFileSync(httpsCertificateKey),
            cert: fs.readFileSync(httpsCertificate),
        },
        host: "0.0.0.0",
        livereload: true,
        port: httpsPort,
    })
}

//==============================================================================
// Return a list of all files in the directory `.http`, as a comma and newline
// Separated list.
function getListOfFiles() {
    let listOfFiles = new filelist.FileList()
    listOfFiles.include(outDir + "/**")
    const outdirNoSlashes = outDir.replace(/^[./\\]*/gu, "")

    return listOfFiles
        .toArray()
        .map((e) => '"' + e.toString().replace(outdirNoSlashes, "") + '"\n')
        .concat(['"/"\n', '"/sw.js.map"\n', '"/app.js.map"\n'])
}

//==============================================================================
// Copy service worker to ./http
function copyServiceWorker() {
    const listOfFiles = getListOfFiles()
    return src(serviceWorkerJS)
        .pipe(
            replace(
                /const manifest\s*=\s*\[\s*LIST_OF_FILES\s*\]/gu,
                "const manifest = [\n" + listOfFiles + "\n]"
            )
        )
        .pipe(
            replace(
                /const version\s*=\s*TIMESTAMP/gu,
                'const version = "Notoy-PWA-' + generateTimestamp() + '"'
            )
        )
        .pipe(dest(outDir))
}

function processJS(file) {
    return src(outDir + "/" + file)
        .pipe(
            gulpEsbuild({
                outfile: file,
                bundle: true,
                sourcemap: "external",
                minify: true,
                target: "es2015",
                treeShaking: true,
                platform: "browser",
                resolveExtensions: [".js"],
                define: {
                    "process.env.NODE_ENV": "production",
                },
            })
        )
        .pipe(dest(outDir))
}

function processSW() {
    return processJS("sw.js")
}

function processApp() {
    return processJS("app.js")
}

//==============================================================================
// Copy everything in ./assets to ./http
function copyAssets() {
    return src(assetDir + "/**/*").pipe(dest(outDir))
}

//==============================================================================
// Delete generated files.
function delDirectory(dirName, cb) {
    del([dirName], cb)
}

function cleanHTTP(cb) {
    delDirectory(outDir, cb)
    cb()
}

function cleanOutput(cb) {
    delDirectory(spagoOutdir, cb)
    cb()
}

// eslint-disable-next-line no-undef
exports.clean = parallel(cleanOutput, cleanHTTP)

// eslint-disable-next-line no-undef
exports.bundle = series(
    parallel(runSpago, copyAssets),
    parallel(
        replaceVersionOutdir,
        processApp,
        series(copyServiceWorker, processSW)
    )
)

// eslint-disable-next-line no-undef
exports.serve = series(runHTTPS)
