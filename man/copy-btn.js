// SUMMARY:      Copy Button for use with code examples.
//
// AUTHOR:       Mats Lidell
//
// ORIG-DATE:    27-Oct-26 at 17:10:04
// LAST-MOD:     30-Oct-26 at 00:06:25 by Mats Lidell
//
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Copyright (C) 2026  Free Software Foundation, Inc.
// See the "../HY-COPY" file for license information.
// This file is part of GNU Hyperbole.
//
// The svg icons are Copyright (c) 2026 Lucide Icons and Contributors.
// See "Lucide-LICENSE" for the license information.
//
// Description:
//
//   Adds a copy text button to a set of HTML elements that contains
//   code. For the ccs that goes with the button see file
//   "copy-btn.css"

const lucideCopy = `
  <svg xmlns="http://www.w3.org/2000/svg"
       width="16" height="16" viewBox="0 0 24 24" fill="none"
       stroke="currentColor" stroke-width="2" stroke-linecap="round"
       stroke-linejoin="round" class="lucide lucide-copy preview-icon">
    <rect width="14" height="14" x="8" y="8" rx="2" ry="2"/>
    <path d="M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2"/>
  </svg>`;

const lucideCheck = `
  <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"
       viewBox="0 0 24 24" fill="none" stroke="currentColor"
       stroke-width="2" stroke-linecap="round" stroke-linejoin="round"
       class="lucide lucide-check preview-icon">
    <path d="M20 6 9 17l-5-5"/>
  </svg>`;

function clearButton(button) {
  button.innerHTML = lucideCopy;
  button.title = "Copy";
  button.setAttribute("aria-label", "Copy");
  button.dataset.copied = "false";
}

function checkButton(button) {
  button.innerHTML = lucideCheck;
  button.title = "Copied";
  button.setAttribute("aria-label", "Copied");
  button.dataset.copied = "true";
}

// The query selectors finds these elements for where to add a copy
// button.
//
// README.in.md - ```elisp -> pre.elisp
//
// Hyperbole.texi - @lisp -> pre.lisp-preformatted
//                - @lisp > @verbatiminclude -> .example.lisp > pre-verbatim
//
document.querySelectorAll("pre.elisp, pre.lisp-preformatted, .example.lisp > pre.verbatim")
  .forEach(pre => {
    const div = document.createElement("div");
    div.className = "code-wrapper";
    const btn = document.createElement("button");
    btn.type = "button";
    let resetTimer;

    clearButton(btn);
    clearTimeout(resetTimer);
    btn.addEventListener("click", async () => {
      try {
        await navigator.clipboard.writeText(pre.textContent);
        checkButton(btn);
        clearTimeout(resetTimer);
        resetTimer = setTimeout(() => clearButton(btn), 2000);
      } catch (e) {
        console.error("Copy error:", e);
      }
    });
    pre.before(div);
    div.appendChild(pre);
    div.appendChild(btn);
  });
