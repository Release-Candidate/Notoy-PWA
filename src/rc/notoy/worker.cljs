;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright (C) 2021 Roland Csaszar
;;;
;;; Project:  notoy-pwa
;;; File:     worker.cljs
;;; Date:     24.Nov.2021
;;;
;;; ============================================================================

(ns rc.notoy.worker)

(defn init
  "Main entry point of the web worker."
  []
  (.addEventListener js/self
                     "message"
                     (fn [^js e]
                       (js/console.log (.. e -data)))))
