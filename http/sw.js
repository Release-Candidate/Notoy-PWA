(()=>{var s=(t,o,n)=>new Promise((h,i)=>{var W=a=>{try{e(n.next(a))}catch(c){i(c)}},l=a=>{try{e(n.throw(a))}catch(c){i(c)}},e=a=>a.done?h(a.value):Promise.resolve(a.value).then(W,l);e((n=n.apply(t,o)).next())});var p="Notoy-PWA-20220213120031",y=["/Notoy-PWA/http/404.html","/Notoy-PWA/http/app.js","/Notoy-PWA/http/app.js.map","/Notoy-PWA/http/apple-touch-icon.png","/Notoy-PWA/http/browserconfig.xml","/Notoy-PWA/http/favicon.ico","/Notoy-PWA/http/favicon.svg","/Notoy-PWA/http/icons/download.svg","/Notoy-PWA/http/icons/icon.svg","/Notoy-PWA/http/icons/maskable_128.png","/Notoy-PWA/http/icons/maskable_144.png","/Notoy-PWA/http/icons/maskable_16.png","/Notoy-PWA/http/icons/maskable_192.png","/Notoy-PWA/http/icons/maskable_256.png","/Notoy-PWA/http/icons/maskable_32.png","/Notoy-PWA/http/icons/maskable_48.png","/Notoy-PWA/http/icons/maskable_512.png","/Notoy-PWA/http/icons/maskable_72.png","/Notoy-PWA/http/icons/maskable_96.png","/Notoy-PWA/http/icons/position.svg","/Notoy-PWA/http/icons/share.svg","/Notoy-PWA/http/icons/transparent_128.png","/Notoy-PWA/http/icons/transparent_144.png","/Notoy-PWA/http/icons/transparent_16.png","/Notoy-PWA/http/icons/transparent_192.png","/Notoy-PWA/http/icons/transparent_256.png","/Notoy-PWA/http/icons/transparent_32.png","/Notoy-PWA/http/icons/transparent_48.png","/Notoy-PWA/http/icons/transparent_512.png","/Notoy-PWA/http/icons/transparent_72.png","/Notoy-PWA/http/icons/transparent_96.png","/Notoy-PWA/http/index.html","/Notoy-PWA/http/manifest.json","/Notoy-PWA/http/mstile-150x150.png","/Notoy-PWA/http/notoy-pwa.css","/Notoy-PWA/http/safari-pinned-tab.svg","/Notoy-PWA/http/sw.js","/Notoy-PWA/http/sw.js.map","/Notoy-PWA/http/"];function A(){return s(this,null,function*(){yield(yield caches.open(p)).addAll(y),console.log(`[Service Worker] installed files to ${p}`)})}addEventListener("install",t=>{t.waitUntil(A())});function P(){return s(this,null,function*(){let t=yield caches.keys();yield Promise.all(t.map(o=>o!==p&&caches.delete(o))),console.log("[Service Worker] activated")})}addEventListener("activate",t=>t.waitUntil(P()));function N(t){return s(this,null,function*(){let o=yield caches.match(t,{ignoreSearch:!0});if(o)return console.log(`[Service Worker] cache hit: ${t.url}`),o;console.log(`[Service Worker] fetching ${t.url}`);let n=yield fetch(t).catch(r);return n.ok?n:(console.log(`[Service Worker] haven't found ${t.url}`),r("URL not found"))})}addEventListener("fetch",t=>t.respondWith(N(t.request)));function r(t){return s(this,null,function*(){return console.log(`[Service Worker] Error: "${t}"`),caches.match("/404.html")})}})();
