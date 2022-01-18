// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2022 Roland Csaszar
//
// Project:  notoy-pwa
// File:     DateTime.js
// Date:     18.Jan.2022
//
// ==============================================================================
/* eslint-disable no-undef */

// eslint-disable-next-line strict
"use strict"

exports.getDateStringJS = getDateStringJS
exports.getTimeStringJS = getTimeStringJS
exports.getYearStringJS = getYearStringJS
exports.getMonthStringJS = getMonthStringJS
exports.getDayStringJS = getDayStringJS
exports.getWeekDayStringJS = getWeekDayStringJS
exports.getHourStringJS = getHourStringJS
exports.getMinuteStringJS = getMinuteStringJS
exports.getSecondsStringJS = getSecondsStringJS

/**
 * Return the current local date in ISO format, "YYYY-MM-DD".
 *
 * @returns The current local date in ISO format, "YYYY-MM-DD".
 */
function getDateStringJS() {
    return function () {
        const today = new Date()

        return (
            today.getFullYear() +
            "-" +
            // eslint-disable-next-line no-magic-numbers
            pad0s(today.getMonth() + 1) +
            "-" +
            pad0s(today.getDate())
        )
    }
}

function getYearStringJS() {
    return function () {
        const today = new Date()

        return today.getFullYear().toString()
    }
}

function getMonthStringJS() {
    return function () {
        const today = new Date()
        // eslint-disable-next-line no-magic-numbers
        return pad0s(today.getMonth() + 1)
    }
}

function getDayStringJS() {
    return function () {
        const today = new Date()
        return pad0s(today.getDate())
    }
}

function getWeekDayStringJS() {
    return function () {
        const today = new Date()
        return today.getDay()
    }
}

/**
 * Return current local time in 24h format, "HH:mm:ss".
 *
 * @returns The current local time in 24h format, "HH:mm:ss".
 */
function getTimeStringJS() {
    return function () {
        const now = new Date()

        return (
            pad0s(now.getHours()) +
            ":" +
            pad0s(now.getMinutes()) +
            ":" +
            pad0s(now.getSeconds())
        )
    }
}

function getHourStringJS() {
    return function () {
        const today = new Date()
        return pad0s(today.getHours())
    }
}

function getMinuteStringJS() {
    return function () {
        const today = new Date()
        return pad0s(today.getMinutes())
    }
}

function getSecondsStringJS() {
    return function () {
        const today = new Date()
        return pad0s(today.getSeconds())
    }
}

/**
 * Left pad the given number with a zero `0`, if it is lesser than 10.
 *
 * @param {number} n - The number to format
 * @returns The number with a leading zero `0`, if it is lesser than 10, the
 *          number else.
 */
function pad0s(n) {
    // eslint-disable-next-line no-magic-numbers
    return n < 10 ? "0" + n : n.toString()
}
