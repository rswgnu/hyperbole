;;; hsys-youtube.el --- Action buttons to play timestamped segments of Youtube videos    -*- lexical-binding: t; -*-
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    10-Jul-22 at 18:10:56
;; Last-Mod:      7-Oct-22 at 23:54:20 by Mats Lidell
;;
;; Copyright (C) 2022  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Load this library and then you can embed Action Buttons like
;;   these to play segments of Youtube videos from any of your
;;   buffers, activating these buttons with the Action Key.
;;
;;   All of these do the exact same thing:
;;     <yt-play "WKwZHSbHmPg" "2:44">
;;     <yt-play "WKwZHSbHmPg" "2m44s">
;;     <yt-play "WKwZHSbHmPg" "164">
;;     <yt-play "www.youtube.com/watch?v=WKwZHSbHmPg" "2m:44">
;;     <yt-play "www.youtube.com/watch?v=WKwZHSbHmPg&start=30&end=40">
;;     <yt-play "youtu.be/WKwZHSbHmPg&t=2m44s">
;;     <yt-play "https://www.youtube.com/watch?v=WKwZHSbHmPg" "0h:2m:44s">
;;
;;   Add a third parameter to give the end/stop time:
;;     <yt-play "WKwZHSbHmPg" "2m44s" "2m50s">
;;     <yt-url  "WKwZHSbHmPg" "2m44s" "2m50s">

;;   Additional Action Button types include search:
;;     <yt-search "hypertext hyperbole">
;;
;;   and summarizing all of the metadata info associated with a video:
;;     <yt-info "WKwZHSbHmPg">

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'hact) ;; For htype:symbol
(require 'hsys-www)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hsys-youtube-start-format "https://www.youtube.com/watch?v=%s&t=%s"
  "Format string used to play a Youtube video from a certain point in time.

The first %s is where the video id string is inserted and the second %s is
where the time string is inserted.  The time string must be a
colon-separated hours:minutes:seconds string, e.g. 1:2:44 (1 hour, two
minutes, 45 seconds), where the hours and minutes are optional.")

(defvar hsys-youtube-end-format "https://www.youtube.com/embed/%s?autoplay=1&start=%s&end=%s"
  "Format string used to play a section of a Youtube video.
This requires use of the `embed' api.

The first %s is where the video id string is inserted; the second %s is
where the start time string in seconds is inserted; the third %s is
where the end time string in seconds is inserted.  The time strings
must be in colon-separated hours:minutes:seconds format, e.g. 1:2:44
(1 hour, two minutes, 45 seconds), where the hours and minutes are
optional.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hsys-youtube-get-url (video-id &optional start-time-string end-time-string)
  "Return url to play VIDEO-ID from point specified by optional START-TIME-STRING.
Return nil if START-TIME-STRING is given but is invalid.  If not given,
START-TIME-STRING is set to \"0s\" representing the beginning of the video.

START-TIME-STRING is a colon-separated hours:minutes:seconds string,
e.g. 1:2:44 (1 hour, two minutes, 45 seconds), where the hours and
minutes are optional."
  (if end-time-string
      (progn (setq start-time-string (hsys-youtube-time-in-seconds start-time-string)
		   end-time-string (hsys-youtube-time-in-seconds end-time-string))
	     (hsys-youtube-end-url video-id start-time-string end-time-string))
    (setq start-time-string (hsys-youtube-time-in-hms start-time-string)
	  end-time-string (hsys-youtube-time-in-hms end-time-string))
    (hsys-youtube-start-url video-id start-time-string)))

(defun hsys-youtube-get-url:help (hbut)
  "Show in minibuffer the url from an `hsys-youtube-get-url' action button, HBUT.
Called when the Assist Key is pressed on such a button."
  (message (apply #'hsys-youtube-get-url (hattr:get hbut 'args))))

(defun hsys-youtube-info (video-id)
  "Display a web page with the metadata information about VIDEO-ID."
  (hact #'actypes::www-url (format "https://mattw.io/youtube-metadata/?url=https://youtu.be/%s&submit=true"
				   video-id)))

(defun hsys-youtube-search (search-term)
  "Search Youtube for SEARCH-TERM."
  (interactive "sSearch Youtube for: ")
  (hyperbole-web-search "Youtube" search-term))

(defun hsys-youtube-search:help (search-term)
  "Display in the minibuffer the Youtube url to search for SEARCH-TERM."
  (interactive "sShow Youtube search url for: ")
  (hyperbole-web-search "Youtube" search-term t))

(defun hsys-youtube-play (video-id &optional start-time-string end-time-string)
  "Play a VIDEO-ID from the point specified by optional START-TIME-STRING.
If not given, START-TIME-STRING is set to \"0s\" representing the beginning
of the video.  START-TIME-STRING is a colon-separated hours:minutes:seconds
string, e.g. 1:2:44 (1 hour, two minutes, 45 seconds), where the hours
and minutes are optional."
  (hact #'actypes::www-url (hsys-youtube-get-url video-id start-time-string end-time-string)))

(defun hsys-youtube-play:help (hbut)
  "Show in the minibuffer the url for an `hsys-youtube-play' action button, HBUT.
Called when the Assist Key is pressed on such a button."
  (message (apply #'hsys-youtube-get-url (hattr:get hbut 'args))))

;; Create easy to type Action Button aliases.
(defalias (htype:symbol 'yt-info   'actypes) #'hsys-youtube-info)
(defalias (htype:symbol 'yt-play   'actypes) #'hsys-youtube-play)
(defalias (htype:symbol 'yt-search 'actypes) #'hsys-youtube-search)
(defalias (htype:symbol 'yt-url    'actypes) #'hsys-youtube-get-url)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst hsys-youtube-url-prefix-regexp "\\`\\(https://\\|www\\.\\|youtube\\.\\|youtu\\.be/\\)"
  "Regexp matching the start of a Youtube url.")

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hsys-youtube-end-url (video-id &optional start-time-string end-time-string)
  "Return url to play VIDEO-ID from optional START-TIME-STRING to END-TIME-STRING.
VIDEO-ID must be a string and can be a video identifier,
e.g. WkwZHSbHmPg, or a full url to the video."
  (if (or (null video-id)
	  (not (stringp video-id))
	  (string-empty-p video-id))
      (error "(hsys-youtube-end-url): Invalid 'video-id': '%s'" video-id)
    (setq start-time-string (if (stringp start-time-string)
				(hsys-youtube-time-in-seconds start-time-string)
			      "0")
	  end-time-string (if (stringp end-time-string)
			      (hsys-youtube-time-in-seconds end-time-string)
			    "-1"))
    (format "%s&rand=%d"
	    (if (string-match-p hsys-youtube-url-prefix-regexp video-id)
		(if (string-match-p "[?&]\\(start\\|end\\)=" video-id)
		    video-id
		  (format (concat video-id "?autoplay=1&start=%s&end=%s")
			  start-time-string end-time-string))
	      (format hsys-youtube-end-format video-id start-time-string end-time-string))
	    ;; Need to change the URL each time is called to force
	    ;; replay, so add unused random parameter.
	    (random 10000000))))

(defun hsys-youtube-start-url (video-id &optional start-time-string)
  "Return url to play VIDEO-ID starting at beginning or optional START-TIME-STRING.
VIDEO-ID must be a string and can be a video identifier,
e.g. WkwZHSbHmPg, or a full url to the video."
  (if (or (null video-id)
	  (not (stringp video-id))
	  (string-empty-p video-id))
      (error "(hsys-youtube-start-url): Invalid 'video-id': '%s'" video-id)
    (setq start-time-string (if (stringp start-time-string)
				(hsys-youtube-time-in-hms start-time-string)
			      "0s"))
    (if (string-match-p hsys-youtube-url-prefix-regexp video-id)
	(if (string-match-p "[?&]t=" video-id)
	    video-id
	  (format (concat video-id "&t=%s") start-time-string))
      (format hsys-youtube-start-format video-id start-time-string))))

(defun hsys-youtube-time-in-hms (start-time-string)
  "Return the start time for a Youtube url from START-TIME-STRING.
Start time is returned as hours, minutes and seconds.
Hours and minutes are optional within the START-TIME-STRING, e.g. 1:2:44 (1
hour, two minutes, 45 seconds into a video).  If the START-TIME-STRING
format is invalid, return it unchanged."
  (if (and (stringp start-time-string)
	   (string-match-p ":" start-time-string))
      (let* ((time-parts (split-string start-time-string "[:hmsHMS]" t))
             (num-of-parts (length time-parts))
	     (part1 (nth 0 time-parts)) 
	     (part2 (nth 1 time-parts))
	     (part3 (nth 2 time-parts)))
        (cond ((zerop num-of-parts)
               "0s")
              ((= num-of-parts 1)
               (concat part1 "s"))
              ((= num-of-parts 2)
	       (concat part1 "m" part2 "s"))
              ((= num-of-parts 3)
	       (concat part1 "h" part2 "m" part3 "s"))))
    start-time-string))

(defun hsys-youtube-time-in-seconds (start-time-string)
  "Return the number of seconds time for a Youtube url given a START-TIME-STRING.
Hours and minutes are optional within the START-TIME-STRING,
e.g. 1:2:44 (1 hour, two minutes, 45 seconds into a video).  The
formats 1h2m44s or 1h:2m:44s may also be used.  If the
START-TIME-STRING format is invalid, return it unchanged."
  (if (and (stringp start-time-string)
	   (string-match-p "[:hmsHMS]" start-time-string))
      (let* ((time-parts (split-string start-time-string "[:hmsHMS]" t))
             (num-of-parts (length time-parts))
	     (part1 (nth 0 time-parts)) 
	     (part2 (nth 1 time-parts))
	     (part3 (nth 2 time-parts)))
        (cond ((zerop num-of-parts)
               "0")
              ((= num-of-parts 1)
               part1)
              ((= num-of-parts 2)
	       (int-to-string (+ (* (string-to-number part1) 60)
				 (string-to-number part2))))
              ((= num-of-parts 3)
	       (int-to-string (+ (* (string-to-number part1) 3600)
				 (* (string-to-number part2) 60)
				 (string-to-number part3))))))
    start-time-string))

(provide 'hsys-youtube)
