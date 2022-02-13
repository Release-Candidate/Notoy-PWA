// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Roland Csaszar
//
// Project:  notoy-pwa
// File:     gulpfile.js
// Date:     02.Dec.2021
//
// ==============================================================================
/* eslint-disable max-lines */
/* eslint-disable no-console */

//==============================================================================
// Directories and files.

// Name of the app.
const appName = "Notoy-PWA"

// The navigation scope of the PWA, the root path to the service worker.
const navScopePWA = "/"
const navScopeGitHub = `/${appName}/http/`

// Service worker, JS file.
const serviceWorkerJS = "sw.js"

// Service worker, map file name.
const serviceWorkerJSMap = serviceWorkerJS + ".map"

// Service worker, full path.
const serviceWorkerJSPath = "./src/service_worker/" + serviceWorkerJS

// App JS file.
const appJS = "app.js"

// App, map file name
const appJSMap = appJS + ".map"

// Manifest filename (original).
const manifestJSON = "manifest.json"

// Directory to exclude in the list of files to cache. Only the directory itself, not it's content!
const excludeDir = "/icons"

// CSS file to process by TailwindCSS.
const tailwindInput = "src/input.css"

// Name of the CSS file TailwindCSS produced in directory `outDir`.
const tailwindOutput = appName.toLowerCase() + ".css"

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

// Path to HTTPS certificate and key
const httpsCertificate = "../https_cert.pem"
const httpsCertificateKey = "../https_cert-key.pem"

// HTTPS port to use
const httpsPort = 1234

//==============================================================================
// JS requires

// eslint-disable-next-line no-undef
const { series, parallel, src, dest, watch } = require("gulp")

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
        pad0s(nowDate.getHours()) +
        pad0s(nowDate.getMinutes()) +
        pad0s(nowDate.getSeconds())
    )
}

//==============================================================================
// Replace Version and the PWA scope path.

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

function processManifest(dirName, version, scopePath) {
    return src(dirName + "/" + manifestJSON)
        .pipe(
            replace(
                /"version":\s+"[0-9]+.[0-9]+.[0-9]+",/gu,
                `"version": "${version}",`
            )
        )
        .pipe(
            replace(/"start_url": "[\S]+",/gu, `"start_url": "${scopePath}",`)
        )
        .pipe(replace(/"id": "[\S]+",/gu, `"id": "${scopePath}",`))
        .pipe(replace(/"scope": "[\S]+",/gu, `"scope": "${scopePath}",`))
        .pipe(replace(/"action": "[\S]+",/gu, `"action": "${scopePath}",`))

        .pipe(dest(dirName))
}

function processManifestOutdirGitHub() {
    return processManifest(outDir, scanChangelogVersion(), navScopeGitHub)
}
function processManifestOutdir() {
    return processManifest(outDir, scanChangelogVersion(), navScopePWA)
}

//==============================================================================
// Replace the PWA scope path in index.html.
function processIndexHTML(scopePath) {
    const newUrlRex = new RegExp(
        `new URL\\("[\\S]+${serviceWorkerJS}", import\\.meta\\.url\\)`,
        "gu"
    )
    return src(outDir + "/index.html")
        .pipe(
            replace(
                newUrlRex,
                `new URL("${scopePath}${serviceWorkerJS}", import.meta.url)`
            )
        )
        .pipe(replace(/scope: "[\S]+",/gu, `scope: "${scopePath}",`))
        .pipe(dest(outDir))
}

function processIndexHTMLGitHub() {
    return processIndexHTML(navScopeGitHub)
}

function processIndexHTMLPWA() {
    return processIndexHTML(navScopePWA)
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
// Run tailwindcss.
async function runTailwind() {
    return (
        exec(`tailwindcss -i ${tailwindInput} -o ${outDir}/${tailwindOutput}`),
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
function runHTTPS(cb) {
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
    cb()
}

//==============================================================================
// Return a list of all files in the directory `.http`, as a comma and newline
// Separated list.
function getListOfFiles(dir) {
    let listOfFiles = new filelist.FileList()
    listOfFiles.include(outDir + "/**")
    const outdirNoSlashes = outDir.replace(/^[./\\]*/gu, "")

    const addedFiles = listOfFiles
        .toArray()
        .filter((e) => e !== outdirNoSlashes + excludeDir)
        .map((e) => '"' + e.replace(outdirNoSlashes + "/", dir) + '"')
        .concat([
            `"${dir}"`,
            `"${dir}${appJSMap}"`,
            `"${dir}${serviceWorkerJSMap}"`,
        ])
    return [...new Set(addedFiles)]
}

//==============================================================================
// Copy service worker to ./http
function copyServiceWorker(dir) {
    const listOfFiles = getListOfFiles(dir)
    return src(serviceWorkerJSPath)
        .pipe(
            replace(
                /const manifest\s*=\s*\[\s*LIST_OF_FILES\s*\]/gu,
                "const manifest = [\n" + listOfFiles + "\n]"
            )
        )
        .pipe(
            replace(
                /const version\s*=\s*TIMESTAMP/gu,
                `const version = "${appName}-` + generateTimestamp() + '"'
            )
        )
        .pipe(dest(outDir))
}

function copyServiceWorkerGitHub() {
    return copyServiceWorker(navScopeGitHub)
}

function copyServiceWorkerNavScopePWA() {
    return copyServiceWorker(navScopePWA)
}

//==============================================================================
// Run Esbuild von JS files.
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
        .pipe(connect.reload())
}

function processSW() {
    return processJS(serviceWorkerJS)
}

function processApp() {
    return processJS(appJS)
}

//==============================================================================
// Watch for changes
function watchSource(cb) {
    watch("./src/**", { ignoreInitial: false }, bundleTarget)
    cb()
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

const cleanTarget = parallel(cleanOutput, cleanHTTP)

const bundleTarget = series(
    parallel(runSpago, copyAssets, runTailwind),
    parallel(
        processManifestOutdir,
        processIndexHTMLPWA,
        processApp,
        series(copyServiceWorkerNavScopePWA, processSW)
    )
)

const bundleTargetGitHub = series(
    parallel(runSpago, copyAssets, runTailwind),
    parallel(
        processManifestOutdirGitHub,
        processIndexHTMLGitHub,
        processApp,
        series(copyServiceWorkerGitHub, processSW)
    )
)

const serveTarget = series(runHTTPS)

// eslint-disable-next-line no-undef
exports.clean = cleanTarget

// eslint-disable-next-line no-undef
exports.bundle = bundleTarget

// eslint-disable-next-line no-undef
exports.bundleGitHub = bundleTargetGitHub

// eslint-disable-next-line no-undef
exports.serve = serveTarget

// eslint-disable-next-line no-undef
exports.watch = parallel(watchSource, serveTarget)
