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
            "./404.html",
            "./app.js",
            "./app.js.map",
            "./apple-touch-icon.png",
            "./browserconfig.xml",
            "./favicon.ico",
            "./favicon.svg",
            "./icons",
            "./icons/download.svg",
            "./icons/icon.svg",
            "./icons/maskable_128.png",
            "./icons/maskable_144.png",
            "./icons/maskable_16.png",
            "./icons/maskable_192.png",
            "./icons/maskable_256.png",
            "./icons/maskable_32.png",
            "./icons/maskable_48.png",
            "./icons/maskable_512.png",
            "./icons/maskable_72.png",
            "./icons/maskable_96.png",
            "./icons/position.svg",
            "./icons/share.svg",
            "./icons/transparent_128.png",
            "./icons/transparent_144.png",
            "./icons/transparent_16.png",
            "./icons/transparent_192.png",
            "./icons/transparent_256.png",
            "./icons/transparent_32.png",
            "./icons/transparent_48.png",
            "./icons/transparent_512.png",
            "./icons/transparent_72.png",
            "./icons/transparent_96.png",
            "./index.html",
            "./manifest.json",
            "./mstile-150x150.png",
            "./notoy-pwa.css",
            "./safari-pinned-tab.svg",
            "./sw.js",
            "./sw.js.map",
            "./",
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
                caches.match("./404.html")
            )
        })
    }
})()
