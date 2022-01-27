// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2022 Roland Csaszar
//
// Project:  notoy-pwa
// File:     GeoLocation.js
// Date:     26.Jan.2022
//
// ==============================================================================
/* eslint-disable no-undef */
/* eslint-disable no-console */

// eslint-disable-next-line strict
"use strict"

exports.supportsGeoLocationJS = supportsGeoLocationJS
exports.clearWatchJS = clearWatchJS
exports.currPositionJS = currPositionJS

/**
 * Return `true`, if detecting the current position (GPS on mobile, network
 * routing, ...) is supported, `false` else.
 *
 * @returns `true`, if detecting the current position (GPS on mobile, network
 *           routing, ...) is supported, `false` else.
 */
function supportsGeoLocationJS() {
    if ("geolocation" in navigator) {
        return true
    } else {
        return false
    }
}

/**
 * Do not watch the geolocation position with ID `id` any more.
 *
 * @param {Number} id
 */
function clearWatchJS(id) {
    return function () {
        if ("geolocation" in navigator) {
            navigator.geolocation.clearWatch(id)
        }
    }
}

/**
 * Get the current position.
 *
 * @param {PositionCallback} successCB - The callback to call on success.
 * @param {ErrorCallback} errorCB - The callback to call on errors
 * @param {PositionOptions} options - The options for the position request.
 */
function currPositionJS(successCB, errorCB, options) {
    return function () {
        navigator.geolocation.getCurrentPosition(
            function (position) {
                successCB(
                    position.coords.latitude,
                    position.coords.longitude,
                    position.coords.accuracy,
                    position.timestamp
                )()
            },
            function (posErr) {
                errorCB(posErr.code, posErr.message)()
            },
            options
        )
    }
}
