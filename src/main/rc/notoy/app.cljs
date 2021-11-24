;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright (C) 2021 Roland Csaszar
;;;
;;; Project:  Notoy-PWA
;;; File:     app.cljs
;;; Date:     24.Nov.2021
;;;
;;; ============================================================================

(ns rc.notoy.app
  (:require [reagent.dom :as rdom]
            [reagent.core :as rcore]))


(def root
  "The root div of the HTML, the div for the Reagent app to render to."
  (.querySelector js/document "#root"))

(defn init
  "Main Entry point of the PWA."
  []
  (rdom/render
   [:div
    [:h1 "Reagent!"]
    [:p "This is my PWA."]] root))
