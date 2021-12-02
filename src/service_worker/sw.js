// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Roland Csaszar
//
// Project:  notoy-pwa
// File:     sw.js
// Date:     02.Dec.2021
//
// ==============================================================================

self.addEventListener("install", (e) => {
    e.waitUntil(
        caches.open("airhorner").then((cache) => cache.addAll([
            "./",
            "./index.html",
            "./dist/app.js",
            "./dist/app.css",
        ]))
    )
})
