// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Roland Csaszar
//
// Project:  notoy-pwa
// File:     sw.js
// Date:     02.Dec.2021
//
// ==============================================================================
/* eslint-disable function-paren-newline */
/* eslint-disable implicit-arrow-linebreak */

// `version` is the name of the cache, including a timestamp.
// eslint-disable-next-line no-undef
const version = TIMESTAMP
// `manifest` is an array holding the paths to all files to cache.
// eslint-disable-next-line no-undef
const manifest = [LIST_OF_FILES]

//==============================================================================
// Installation

/**
 * Install the service worker.
 * On installation, all files Parcel knows about are added to the cache.
 */
async function install() {
    const cache = await caches.open(version)
    await cache.addAll(manifest)
    // eslint-disable-next-line no-console
    console.log(`[Service Worker] installed files to ${version}`)
}

addEventListener("install", (event) => event.waitUntil(install()))

//==============================================================================
// Activation

/**
 * Activate service worker.
 * On activation all files from older versions of the cache are deleted.
 */
async function activate() {
    const keys = await caches.keys()
    await Promise.all(keys.map((key) => key !== version && caches.delete(key)))
    // eslint-disable-next-line no-console
    console.log(`[Service Worker] activated`)
}

addEventListener("activate", (event) => event.waitUntil(activate()))

//==============================================================================
// Fetching

/**
 * Fetches the given URL, either from cache or the server.
 *
 * @param {RequestInfo} request The request to fulfill.
 *
 * @returns {Response} The fetched URL as `Response`.
 */
async function fetchFromCache(request) {
    const cachedResponse = await caches.match(request)
    if (cachedResponse) {
        // eslint-disable-next-line no-console
        console.log(`[Service Worker] cache hit: ${request.url}`)
        return cachedResponse
    }
    // eslint-disable-next-line no-console
    console.log(`[Service Worker] fetching ${request.url}`)
    const response = await fetch(request)
    return response
}

addEventListener("fetch", (event) =>
    event.respondWith(fetchFromCache(event.request))
)
