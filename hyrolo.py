#!/usr/bin/env python
#
# Summary:      hyrolo.py --- Output file header and matching entries from HyRolo files via the command-line
# Usage:        <main-module-name> <string-to-match> [<file1> ... <fileN>]
#                  If no files are given, uses the env variable, HYROLO, or if that is not found, the file
#                  "~/.rolo.otl".
#
# Author:       Bob Weiner
#
# Orig-Date:     1-Apr-24 at 01:45:27
# Last-Mod:      1-Sep-24 at 12:25:38 by Bob Weiner
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Copyright (C) 2024  Free Software Foundation, Inc.
# See the "HY-COPY" file for license information.
#
# This file is part of GNU Hyperbole.

# Commentary:
#   See the Info manual "(hyperbole)HyRolo" for a description of HyRolo and associated file formats.
#
#   Detects entries in files with Org, Markdown or Emacs outline formats.
#   Koutlines are not presently supported.
#
#   Unlike hyrolo.el, this outputs only the innermost matching entries rather than the entire
#   subtree of matching entries.
#
#   This outputs a file header only if there is a matching entry in that file.

# Code:

import argparse
import os
import re

# String to match at bol for file header start and end
file_header_delimiter = '==='
# Header to insert before a file's first entry match when file has no header
# Used with one argument, the file name
file_header_format = \
    "===============================================================================\n" \
    "%s" \
    "===============================================================================\n"

# The ANSI escape sequence for the red color
red = "\033[31m"
# The ANSI escape sequence for inverting colors is \033[7m
invert = "\033[7m"
# The ANSI escape sequence to reset the color is \033[0m
reset = "\033[0m"

def find_matching_entries(match_string, file_paths):
    quoted_match_string = re.escape(match_string)

    # Remove any null items from file_paths and expand them
    file_paths = [os.path.abspath(os.path.expanduser(os.path.expandvars(p))) for p in file_paths if p]
    org_buffer_property_regex ='#\\+[^: \t\n]+:'

    for file_path in file_paths:
        # Initialize variables
        buffer = ''
        file_header_buffer = ''
        inside_entry = False
        inside_file_header = False
        inside_org_file_header = False
        first_line = True
        first_entry = True
        headline_match = False
        org_file = False

        # Open the file
        with open(file_path, 'r') as file:
            org_file = file_path.endswith('.org')
            for line in file:
                if first_line:
                    first_line = False
                    if line.startswith(file_header_delimiter):
                        inside_file_header = True
                        file_header_buffer += line
                        org_file = False  # Prevent double file header wrapping
                        continue
                    elif org_file and line.startswith('#'):
                        inside_org_file_header = True
                        if re.match(org_buffer_property_regex, line):
                            file_header_buffer += line
                        continue

                if inside_file_header:
                    file_header_buffer += line
                    if line.startswith(file_header_delimiter):
                        inside_file_header = False
                    continue
                elif inside_org_file_header:
                    if re.match(org_buffer_property_regex, line):
                        file_header_buffer += line
                    elif line.startswith('#'):
                        pass
                    else:
                        inside_org_file_header = False
                    continue

                headline_match = re.match(r'[\*\#]+[ \t]', line, re.IGNORECASE)
                # If inside an entry and the line starts with an asterisk, check
                # if the buffer contains the match string. 
                if inside_entry and headline_match:
                    if re.search(quoted_match_string, buffer, re.IGNORECASE):
                        if first_entry:
                            first_entry = False
                            if file_header_buffer:
                                if org_file:
                                    print(file_header_format % file_header_buffer, end='')
                                else:
                                    print(file_header_buffer, end='')
                                file_header_buffer = ''
                                print("@loc> \"%s\"\n" % file_path)
                            else:
                                print(file_header_format % "@loc> \"" + file_path + "\"\n")

                        highlight_matches(match_string, buffer)

                    buffer = ''
                    inside_entry = False
                
                # If we're not inside a entry and the line starts with an asterisk, start a new entry
                elif not inside_entry and headline_match:
                    inside_entry = True
                
                # If we're inside a entry, add the line to the buffer
                if inside_entry:
                    buffer += line
        
        # Check the last entry if it's still inside a entry
        if inside_entry and re.search(quoted_match_string, buffer, re.IGNORECASE):
            if first_entry:
                first_entry = False
                if file_header_buffer:
                    print(file_header_buffer)
                    file_header_buffer = ''
                else:
                    print(file_header_format % file_path)

            highlight_matches(match_string, buffer)


def highlight_matches(match_string, buffer):
    "Split the last buffer into lines and print each line, inverting 'mymatch' colors."
    for b_line in buffer.splitlines():
        if match_string.casefold() in b_line.casefold():
            # Replace the search string with the inverted version
            print(re.sub(re.escape(match_string), invert + match_string + reset, 
                         b_line, flags=re.IGNORECASE))
        else:
            print(b_line)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('match_string', help='string to match within HyRolo entries')
    parser.add_argument('files', nargs='*', help='list of HyRolo files to search')
    args = parser.parse_args()
    
    # find_matching_entries('case_insensitive_string_to_match', 'hyrolo_contact_file')
    if args.files:
        pass
    elif os.getenv("HYROLO"):
        args.files = [os.getenv("HYROLO")]
    else:
        args.files = ["~/.rolo.otl"]
    find_matching_entries(args.match_string, args.files)


if __name__ == '__main__':
    main()
