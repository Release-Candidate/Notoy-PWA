;(() => {
    var c = (n, a, s) =>
        new Promise((l, r) => {
            var g = (e) => {
                    try {
                        o(s.next(e))
                    } catch (t) {
                        r(t)
                    }
                },
                h = (e) => {
                    try {
                        o(s.throw(e))
                    } catch (t) {
                        r(t)
                    }
                },
                o = (e) =>
                    e.done ? l(e.value) : Promise.resolve(e.value).then(g, h)
            o((s = s.apply(n, a)).next())
        })
    var i = "Notoy-PWA-20220209202530",
        m = [
            "./Notoy-PWA/http/404.html",
            "./Notoy-PWA/http/app.js",
            "./Notoy-PWA/http/app.js.map",
            "./Notoy-PWA/http/apple-touch-icon.png",
            "./Notoy-PWA/http/browserconfig.xml",
            "./Notoy-PWA/http/favicon.ico",
            "./Notoy-PWA/http/favicon.svg",
            "./Notoy-PWA/http/icons",
            "./Notoy-PWA/http/icons/download.svg",
            "./Notoy-PWA/http/icons/icon.svg",
            "./Notoy-PWA/http/icons/maskable_128.png",
            "./Notoy-PWA/http/icons/maskable_144.png",
            "./Notoy-PWA/http/icons/maskable_16.png",
            "./Notoy-PWA/http/icons/maskable_192.png",
            "./Notoy-PWA/http/icons/maskable_256.png",
            "./Notoy-PWA/http/icons/maskable_32.png",
            "./Notoy-PWA/http/icons/maskable_48.png",
            "./Notoy-PWA/http/icons/maskable_512.png",
            "./Notoy-PWA/http/icons/maskable_72.png",
            "./Notoy-PWA/http/icons/maskable_96.png",
            "./Notoy-PWA/http/icons/position.svg",
            "./Notoy-PWA/http/icons/share.svg",
            "./Notoy-PWA/http/icons/transparent_128.png",
            "./Notoy-PWA/http/icons/transparent_144.png",
            "./Notoy-PWA/http/icons/transparent_16.png",
            "./Notoy-PWA/http/icons/transparent_192.png",
            "./Notoy-PWA/http/icons/transparent_256.png",
            "./Notoy-PWA/http/icons/transparent_32.png",
            "./Notoy-PWA/http/icons/transparent_48.png",
            "./Notoy-PWA/http/icons/transparent_512.png",
            "./Notoy-PWA/http/icons/transparent_72.png",
            "./Notoy-PWA/http/icons/transparent_96.png",
            "./Notoy-PWA/http/index.html",
            "./Notoy-PWA/http/manifest.json",
            "./Notoy-PWA/http/mstile-150x150.png",
            "./Notoy-PWA/http/notoy-pwa.css",
            "./Notoy-PWA/http/safari-pinned-tab.svg",
            "./Notoy-PWA/http/sw.js",
            "./Notoy-PWA/http/sw.js.map",
            "./Notoy-PWA/http/",
        ]
    function v() {
        return c(this, null, function* () {
            yield (yield caches.open(i)).addAll(m),
                console.log(`[Service Worker] installed files to ${i}`)
        })
    }
    addEventListener("install", (n) => {
        n.waitUntil(v())
    })
    function _() {
        return c(this, null, function* () {
            let n = yield caches.keys()
            yield Promise.all(n.map((a) => a !== i && caches.delete(a))),
                console.log("[Service Worker] activated")
        })
    }
    addEventListener("activate", (n) => n.waitUntil(_()))
    function d(n) {
        return c(this, null, function* () {
            let a = yield caches.match(n, { ignoreSearch: !0 })
            if (a) return console.log(`[Service Worker] cache hit: ${n.url}`), a
            console.log(`[Service Worker] fetching ${n.url}`)
            let s = yield fetch(n).catch(p)
            return s.ok
                ? s
                : (console.log(`[Service Worker] haven't found ${n.url}`),
                  p("URL not found"))
        })
    }
    addEventListener("fetch", (n) => n.respondWith(d(n.request)))
    function p(n) {
        return c(this, null, function* () {
            return (
                console.log(`[Service Worker] Error: "${n}"`),
                caches.match("./Notoy-PWA/http/404.html")
            )
        })
    }
})()
