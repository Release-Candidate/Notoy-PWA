// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2022 Roland Csaszar
//
// Project:  notoy-pwa
// File:     DateTimeFormat.js
// Date:     20.Jan.2022
//
// ==============================================================================
/* eslint-disable max-params */
/* eslint-disable no-undef */

// eslint-disable-next-line strict
"use strict"

exports.defaultDateTimeFormat = defaultDateTimeFormat
exports.isoDateTimeNow = isoDateTimeNow
exports.isoDateTimeJS = isoDateTimeJS
exports.formatDateTimeNow = formatDateTimeNow
exports.formatDateJS = formatDateJS
exports.formatDateTimeJS = formatDateTimeJS

/**
 * Return the default `DateTimeFormat` object of the current locale.
 *
 * @returns The default `DateTimeFormat` object of the current locale.
 */
function defaultDateTimeFormat() {
    return function () {
        return new Intl.DateTimeFormat()
    }
}

/**
 * Return the current date and time as ISO ISO 8601 string as UTC.
 *
 * @returns The current date and time as ISO ISO 8601 string as UTC.
 */
function isoDateTimeNow() {
    return function () {
        const today = new Date()

        return today.toISOString()
    }
}

/**
 * Return the given date and time as ISO ISO 8601 string as UTC.
 * @param {number} year - The Year
 * @param {number} monthIdx - The index of the month, starting at 0 (January)
 * @param {number} day - The day of the month (starting at 1)
 * @param {number} hour - The hour
 * @param {number} minutes - The minutes
 * @param {number} seconds - The seconds
 * @returns The given date and time as ISO ISO 8601 string as UTC.
 */
function isoDateTimeJS(year, monthIdx, day, hour, minutes, seconds) {
    const date = new Date(year, monthIdx, day, hour, minutes, seconds)
    return date.toISOString()
}

/**
 * Return a string of the current date and time formatted using the given
 * formatter object.
 *
 * @param {Intl.DateTimeFormat} formatter - The format to use to format the
 *                                          output.
 * @returns A string of the current date and time formatted using the given
 *          formatter object.
 */
function formatDateTimeNow(formatter) {
    return function () {
        const today = new Date()

        return formatter.format(today)
    }
}

/**
 * Return the given local date formatted by the given formatter.
 *
 * @param {Intl.DateTimeFormat} formatter - The formatter to use
 * @param {number} year - The Year
 * @param {number} monthIdx - The index of the month, starting at 0 (January)
 * @param {number} day - The day of the month (starting at 1)
 * @returns The given date formatted by the given formatter.
 */
function formatDateJS(formatter, year, monthIdx, day) {
    const date = new Date(year, monthIdx, day)
    return formatter.format(date)
}

/**
 * Return the given local date and time formatted by the given formatter.
 *
 * @param {Intl.DateTimeFormat} formatter - The formatter to use
 * @param {number} year - The Year
 * @param {number} monthIdx - The index of the month, starting at 0 (January)
 * @param {number} day - The day of the month (starting at 1)
 * @param {number} hour - The hour
 * @param {number} minutes - The minutes
 * @param {number} seconds - The seconds
 * @returns The given date and time formatted by the given formatter.
 */
function formatDateTimeJS(
    formatter,
    year,
    monthIdx,
    day,
    hour,
    minutes,
    seconds
) {
    const date = new Date(year, monthIdx, day, hour, minutes, seconds)
    return formatter.format(date)
}
