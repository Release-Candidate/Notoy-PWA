// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Roland Csaszar
//
// Project:  notoy-pwa
// File:     sw.js
// Date:     02.Dec.2021
//
// ==============================================================================
import { manifest, version } from "@parcel/service-worker"

async function install() {
    const cache = await caches.open(version)
    await cache.addAll(manifest)
    console.log(`[Service Worker] installed`)
}
addEventListener("install", (e) => e.waitUntil(install()))

async function activate() {
    const keys = await caches.keys()
    await Promise.all(keys.map((key) => key !== version && caches.delete(key)))
    console.log(`[Service Worker] activated`)
}
addEventListener("activate", (e) => e.waitUntil(activate()))

addEventListener("fetch", (e) => {
    console.log(`[Service Worker] fetch`)
    e.respondWith(
        (async () => {
            const r = await caches.match(e.request)
            if (r) {
                console.log(`[Service Worker] Cache hit: ${e.request.url}`)
                return r
            }
            const response = await fetch(e.request)
            const cache = await caches.open(version)
            console.log(
                `[Service Worker] Caching new resource: ${e.request.url}`
            )
            cache.put(e.request, response.clone())
            return response
        })()
    )
})
