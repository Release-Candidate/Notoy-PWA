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
exports.currPositionJS = currPositionJS
exports.currPositionAffJs = currPositionAffJs
exports.watchPositionJS = watchPositionJS
exports.clearWatchJS = clearWatchJS

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
 * Get the current position using callbacks.
 *
 * @param {PositionCallback} successCB - The callback to call on success.
 * @param {ErrorCallback} errorCB - The callback to call on errors
 * @param {PositionOptions} options - The options for the position request.
 */
function currPositionJS(successCB, errorCB, options) {
    return function () {
        navigator.geolocation.getCurrentPosition(
            function (position) {
                successCB({
                    latitude: position.coords.latitude,
                    longitude: position.coords.longitude,
                    altitude: position.coords.altitude,
                    accuracy: position.coords.accuracy,
                    altitudeAccuracy: position.coords.altitudeAccuracy,
                    heading: position.coords.heading,
                    speed: position.coords.speed,
                    timestamp: position.timestamp,
                })()
            },
            function (posErr) {
                errorCB({ code: posErr.code, message: posErr.message })()
            },
            options
        )
    }
}

/**
 * Call the `successCB` each time the current position has changed.
 *
 * @param {PositionCallback} successCB - The callback to call on success.
 * @param {ErrorCallback} errorCB - The callback to call on errors
 * @param {PositionOptions} options - The options for the position request.
 *
 * @returns The watch ID to use to stop the watch using `clearWatchJS`.
 */
function watchPositionJS(successCB, errorCB, options) {
    return function () {
        return navigator.geolocation.watchPosition(
            function (position) {
                successCB({
                    latitude: position.coords.latitude,
                    longitude: position.coords.longitude,
                    altitude: position.coords.altitude,
                    accuracy: position.coords.accuracy,
                    altitudeAccuracy: position.coords.altitudeAccuracy,
                    heading: position.coords.heading,
                    speed: position.coords.speed,
                    timestamp: position.timestamp,
                })()
            },
            function (posErr) {
                errorCB({ code: posErr.code, message: posErr.message })()
            },
            options
        )
    }
}

/**
 * Return the result of waiting for the current geolocation wrapped in an
 * `Effect` (an additional function).
 *
 * Wrapped in a function to be able to call it as an `Effect` from  PureScript.
 *
 * @param {GeoLocOptionsJS} options
 * @returns The awaited promise to get the current geolocation.
 */
function currPositionAffJs(options) {
    return function () {
        return promise(options)
    }
}

/**
 * Return the result of waiting for the current geolocation.
 *
 * @param {GeoLocOptionsJS} options
 * @returns The awaited promise to get the current geolocation.
 */
async function promise(options) {
    const pos = await new Promise(function (successCB, errorCB) {
        navigator.geolocation.getCurrentPosition(
            function (position) {
                successCB({
                    latitude: position.coords.latitude,
                    longitude: position.coords.longitude,
                    altitude: position.coords.altitude,
                    accuracy: position.coords.accuracy,
                    altitudeAccuracy: position.coords.altitudeAccuracy,
                    heading: position.coords.heading,
                    speed: position.coords.speed,
                    timestamp: position.timestamp,
                })
            },
            function (posErr) {
                errorCB(new Error(posErr.message))
            },
            options
        )
    })
    return pos
}

/**
 * Do not watch the geolocation position with ID `id` any more.
 *
 * @param {Number} id The watch ID returned from `watchPositionJS`.
 */
function clearWatchJS(id) {
    return function () {
        if ("geolocation" in navigator) {
            navigator.geolocation.clearWatch(id)
        }
    }
}
