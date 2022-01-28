// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2022 Roland Csaszar
//
// Project:  notoy-pwa
// File:     ShareTarget.js
// Date:     16.Jan.2022
//
// ==============================================================================
/* eslint-disable no-undef */
/* eslint-disable no-console */

// eslint-disable-next-line strict
"use strict"

exports.canShareJS = canShareJS
exports.shareNoteJS = shareNoteJS

/**
 * Return `true` if the current platform supports sharing, `false` else.
 *
 * @returns `true` if the platform supports sharing to other apps, `false`
 *          else.
 */
function canShareJS() {
    return navigator.share
}

/**
 * Share the given note as a record `{ title, text, url }` to other apps.
 *
 * Promise has to be wrapped in a function to work with `Control.Promise`.
 *
 * @param {ShareTargetRecord} note  the record `{ title, text, url }` of the note to share.
 */
function shareNoteJS({ title, text, url }) {
    return function () {
        return navigator
            .share({ title, text, url })
            .then(
                function () {
                    console.log(
                        `Successfully shared note '${title}, ${url}, ${text}'`
                    )
                }
                // eslint-disable-next-line function-paren-newline
            )
            .catch(function (error) {
                console.log(`Error '${error}' sharing note`)
            })
    }
}
