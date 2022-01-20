// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2022 Roland Csaszar
//
// Project:  notoy-pwa
// File:     DateTimeFormat.js
// Date:     20.Jan.2022
//
// ==============================================================================
/* eslint-disable no-undef */

// eslint-disable-next-line strict
"use strict"

exports.defaultDateTimeFormat = defaultDateTimeFormat
exports.formatDateTimeNow = formatDateTimeNow

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
