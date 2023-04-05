;;; ajsc.el --- A Janet Spork Client -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20200430
;; Package-Requires: ((emacs "26.2"))
;; Keywords: janet, spork, network socket repl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A Janet Spork Client - Janet REPL interaction via Spork

;;;; Installation

;; Ensure this file is in your load-path and put the following in a
;; relevant init file:
;;
;;    (require 'ajsc)
;;
;;  Optionally, add:
;;
;;    (add-hook 'janet-mode-hook
;;              #'ajsc-interaction-mode)
;;
;;  or:
;;
;;    (add-hook 'janet-ts-mode-hook
;;              #'ajsc-interaction-mode)
;;
;;  for editor features to be enabled when visiting a buffer with
;;  Janet code in it.  Note, you'll need a janet-mode or janet-ts-mode
;;  installed and setup as well.

;;;; Usage

;; 0. Start Janet's Spork repl and note the host and port, e.g.
;;    in spork's directory:
;;
;;      janet -e '(import ./spork/netrepl) (netrepl/server)'

;; 1. Connect to the repl by:
;;
;;      M-x ajsc
;;
;;    and at the prompt, confirm or modify a host and port like:
;;
;;      localhost:9365
;;
;;    i.e. a host or ip address followed by a colon and port number
;;
;;    A buffer for interaction with the spork repl should appear.

;; 2. Open a Janet source file in another buffer and:
;;
;;      M-x ajsc-interaction-mode
;;
;;    (If janet-mode is enabled and appropriate configuration was
;;     performed as described in the Installation section, manually
;;     invoking `ajsc-interaction-mode` should not be necessary.)
;;
;;    There should be a Ajsc menu containing some convenience commands:
;;
;;      Send buffer
;;      Send expression at point
;;      Send region
;;
;;      Switch to REPL

;; X. Don't use the `comint-delete-output` command.  It may cause
;;    problems.

;;;; Issues

;; 1. @[], @{}, and friends are not detected appropriately (by thingatpt)

;;;; Acknowledgments

;; Thanks to Inc0n for discussions and improvements about the
;; efficiency, readability, and maintainability of the code.

;; Thanks to those involved in:
;;
;;   bindat
;;   emacs
;;   janet
;;
;; and transitively involved folks too ;)

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'bindat)
(require 'comint)
(require 'subr-x)
(require 'thingatpt)

;;;; The Rest

(defgroup ajsc nil
  "A Janet Spork Client"
  :prefix "ajsc-"
  :group 'applications)

(defcustom ajsc-default-endpoint "localhost:9365"
  "Default host and port to connect to.

Host and port should be delimited with ':'."
  :type 'string
  :group 'ajsc)

(defvar ajsc-repl-process-name "ajsc-repl-process"
  "Name of repl process.")

(defvar ajsc-repl-buffer-name "*ajsc-repl*"
  "Name of repl buffer.")

(defvar ajsc--debug-output nil
  "If non-nil, output debug info to *Messages* buffer.")

;;; network protocol header encoding / decoding

(defun ajsc-net-header-str (len)
  "Compute header string for Janet spork message of LEN bytes."
  (bindat-pack (bindat-type uintr 32)
               len))

(defun ajsc-decode-net-header-str (header-str)
  "Compute length represented by 4-byte HEADER-STR."
  (when ajsc--debug-output
    (message "header-str: %S" header-str))
  (bindat-unpack (bindat-type uintr 32)
                 header-str))

;;; handling possibly fragmented network info from emacs

(defvar ajsc--recv-header-bytes "")

(defvar ajsc--recv-decoded-bytes "")

(defvar ajsc--recv-held-msgs "")

(defun ajsc--recv-reset-state ()
  "Reset message reciving state."
  (setq ajsc--recv-header-bytes "")
  (setq ajsc--recv-decoded-bytes "")
  (setq ajsc--recv-held-msgs ""))

(defun ajsc--need-more-header-bytes-p ()
  "Determine if more header bytes are needed."
  (< (string-bytes ajsc--recv-header-bytes) 4))

(defun ajsc--parse-in-bytes (in-bytes)
  "Helper for processing IN-BYTES received from net via Emacs."
  (when ajsc--debug-output
    (message "length in-bytes: %d" (string-bytes in-bytes)))
  (if (ajsc--need-more-header-bytes-p)
      ;; not enough info to calculate message length
      (let* ((in-byte-cnt (string-bytes in-bytes))
             (missing-cnt (- 4 (string-bytes ajsc--recv-header-bytes))))
        (when ajsc--debug-output
          (message "number of header bytes needed: %d" missing-cnt))
        ;; XXX: guessing that this is almost always true
        (if (<= missing-cnt in-byte-cnt)
            (let ((more-header-bytes (substring in-bytes 0 missing-cnt))
                  (remaining-in-bytes (substring in-bytes missing-cnt)))
              (when ajsc--debug-output
                (message "filling in header bytes")
                (message "more-header-bytes: %S" more-header-bytes)
                (message "remaining-in-bytes: %S" remaining-in-bytes)
                (message "ajsc--recv-header-bytes: %S" ajsc--recv-header-bytes))
              (setq ajsc--recv-header-bytes
                    (concat ajsc--recv-header-bytes
                            more-header-bytes))
              (when ajsc--debug-output
                (message "ajsc--recv-header-bytes: %S" ajsc--recv-header-bytes))
              ;; try again
              (ajsc--parse-in-bytes remaining-in-bytes))
          ;; not enough bytes to fill up the header bytes - unlikely?
          (progn
            (when ajsc--debug-output
              (message "header not complete yet"))
            (setq ajsc--recv-header-bytes
                  (concat ajsc--recv-header-bytes
                          in-bytes))
            ;; return empty string -- no data yet
            "")))
    ;; message length can be calculated because all header bytes found
    (let* ((msg-len (ajsc-decode-net-header-str ajsc--recv-header-bytes))
           (in-byte-cnt (string-bytes in-bytes))
           (decoded-bytes ajsc--recv-decoded-bytes)
           (rem-cnt (- msg-len (string-bytes decoded-bytes))))
      (when ajsc--debug-output
        (message "msg-len: %d" msg-len)
        (message "in-byte-cnt: %d" in-byte-cnt)
        (message "rem-cnt: %d" rem-cnt))
      (cond
       ;; all remaining bytes of message available
       ((= rem-cnt in-byte-cnt)
        (when ajsc--debug-output
          (message "got all bytes"))
        (setq ajsc--recv-header-bytes "")
        (setq ajsc--recv-decoded-bytes "")
        (let ((msgs (concat ajsc--recv-held-msgs
                            decoded-bytes
                            in-bytes)))
          (setq ajsc--recv-held-msgs "")
          ;; return all message content bytes
          msgs))
       ;; end of message not received yet
       ((> rem-cnt in-byte-cnt)
        (when ajsc--debug-output
          (message "end of message not received yet"))
        (setq ajsc--recv-decoded-bytes
              (concat ajsc--recv-decoded-bytes in-bytes))
        ;; return empty string for now
        "")
       ;; start of another message detected
       ((< rem-cnt in-byte-cnt)
        (when ajsc--debug-output
          (message "found end of message, but another message detected"))
        (let* ((remaining-msg-bytes (substring in-bytes 0 rem-cnt))
               (remaining-in-bytes (substring in-bytes rem-cnt)))
          (setq ajsc--recv-header-bytes "")
          (setq ajsc--recv-decoded-bytes "")
          (setq ajsc--recv-held-msgs
                (concat ajsc--recv-held-msgs
                        decoded-bytes
                        remaining-msg-bytes))
          ;; try again
          (ajsc--parse-in-bytes remaining-in-bytes)))))))

;; XXX: it is possible we might receive a fragment of a message
;;      so it may be necessary to retain the initial 4-byte header to
;;      determine message boundaries and possibly split / reassemble
;;      content
;;
;;      from "Process Filter Functions" in the emacs manual:
;;
;;      The output to the filter may come in chunks of any size. A
;;      program that produces the same output twice in a row may
;;      send it as one batch of 200 characters one time, and five
;;      batches of 40 characters the next. If the filter looks for
;;      certain text strings in the subprocess output, make sure to
;;      handle the case where one of these strings is split across
;;      two or more batches of output; one way to do this is to
;;      insert the received text into a temporary buffer, which can
;;      then be searched.
;;
;; XXX: comint-delete-output also calls comint-output-filter, but
;;      this seems undesirable...
(defun ajsc-preoutput-filter-function (string)
  "A function for `comint-preoutput-filter-functions`, operates on STRING."
  (when ajsc--debug-output
    (message "received: %S" string))
  (ajsc--parse-in-bytes string))

;;; greeting portion of network protocol

(defun ajsc-send-hello (process hello-str)
  "Send PROCESS the HELLO-STR."
  (process-send-string process
                       (concat (ajsc-net-header-str (string-bytes hello-str))
                               hello-str)))

;;; commands

(defun ajsc-send-code (code-str)
  "Send CODE-STR, a Janet form."
  (interactive "sCode: ")
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer ajsc-repl-buffer-name)))
    (if (not repl-buffer)
        (message (format "%s is missing..." ajsc-repl-buffer-name))
      ;; switch to ajsc buffer to prepare for appending
      (set-buffer repl-buffer)
      (goto-char (point-max))
      (insert code-str)
      (comint-send-input)
      (set-buffer original-buffer)
      (if (eq original-buffer repl-buffer)
          (goto-char (point-max))
        (goto-char here)))))

(defun ajsc-send-region (start end)
  "Send a region bounded by START and END."
  (interactive "r")
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer ajsc-repl-buffer-name)))
    (if (not repl-buffer)
        (message (format "%s is missing..." ajsc-repl-buffer-name))
      ;; switch to ajsc buffer to prepare for appending
      (set-buffer repl-buffer)
      (goto-char (point-max))
      ;; switch back
      (set-buffer original-buffer)
      (append-to-buffer repl-buffer start end)
      (set-buffer repl-buffer)
      (comint-send-input)
      (set-buffer original-buffer)
      (goto-char here))))

(defun ajsc-send-buffer ()
  "Send buffer content."
  (interactive)
  (ajsc-send-region (point-min) (point-max)))

;; XXX: thingatpt doesn't quite understand janet
(defun ajsc-send-expression-at-point ()
  "Send expression at point."
  (interactive)
  (let* ((bound (bounds-of-thing-at-point 'sexp))
         (start (car bound))
         (end (cdr bound)))
    (when (and start end)
      (ajsc-send-region start end))))

(defun ajsc-switch-to-repl ()
  "Switch to the repl buffer named by `ajsc-repl-buffer-name`."
  (interactive)
  (pop-to-buffer ajsc-repl-buffer-name))

;;; mode related things

(defvar ajsc-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'ajsc-send-buffer)
    (define-key map "\C-c\C-e" 'ajsc-send-expression-at-point)
    (define-key map "\C-c\C-r" 'ajsc-send-region)
    (define-key map "\C-c\C-z" 'ajsc-switch-to-repl)
    (easy-menu-define ajsc-interaction-mode-map map
      "A Janet Spork Client Interaction Mode Menu"
      '("Ajsc"
        ["Send buffer" ajsc-send-buffer t]
        ["Send expression at point" ajsc-send-expression-at-point t]
        ["Send region" ajsc-send-region t]
        "--"
        ["Switch to REPL" ajsc-switch-to-repl t]))
    map)
  "Ajsc interaction mode map.")

(defvar ajsc-mode-map
  (let ((map (copy-keymap comint-mode-map)))
        (easy-menu-define ajsc-mode-map map
          "A Janet Spork Client Mode Menu"
          '("Ajsc"
            ["Switch to other window" other-window t]))
    map)
  "Ajsc mode map.")

(define-derived-mode ajsc-mode comint-mode "A Janet Spork Client"
  "Major mode for ajsc.

\\{ajsc-mode-map}"

  :syntax-table lisp-mode-syntax-table
  (setq comint-prompt-read-only t)
  ;; XXX: does this need to be restricted to apply only to certain buffers?
  ;; remove header bytes appropriately
  (add-hook 'comint-preoutput-filter-functions
            #'ajsc-preoutput-filter-function
            nil 'local)
  ;; XXX: does this need to be restricted to apply only to certain buffers?
  ;; prepend header bytes
  (setq comint-input-sender
        (lambda (proc string)
          (let ((msg (substring-no-properties
                      (concat
                       (ajsc-net-header-str (1+ (string-bytes string)))
                       string))))
            (when ajsc--debug-output
              (message "sending: %S" msg))
            (comint-simple-send proc msg))))
  (setq mode-line-process '(":%s")))

;;;###autoload
(define-minor-mode ajsc-interaction-mode
  "Minor mode for ajsc interaction from a lisp buffer.

The following keys are available in `ajsc-interaction-mode`:

\\{ajsc-interaction-mode}"
  :init-value nil
  :lighter " ajsc"
  :keymap ajsc-interaction-mode-map)

;;;

;; see "Sentinels: Detecting Process Status Changes"
;;
;; some typical events (w/ last newline removed):
;;
;;   connection broken by remote peer <- :repl/quit at prompt
;;   deleted                          <- buffer killed
;;
;;   XXX: due to timing, the following (probably?) won't happen
;;
;;   open from host-name              <- ?
;;   open                             <- ?
(defun ajsc-sentinel (process event)
  "Sentinel for handling various events.
PROCESS and EVENT are the usual arguments for sentinels."
  (when ajsc--debug-output
    (message "sentinel: %S" event))
  (cond ((string-prefix-p "connection broken by remote peer" event)
         (message "connection broken"))
        ;; should not be neeeded
        ;;((string-prefix-p "open" event)
        ;; (do
          ;;(ajsc-send-code "my-fun-prompt")
        ;;  (message "event: %S" event)))
        ;;
        ((string-prefix-p "deleted" event)
         (message "buffer gone for process?: %S" process))
        ;;
        (t
         (message "Unrecognized event")
         (message "process: %S" process))))

;;; main entry point

;;;###autoload
(defun ajsc (endpoint)
  "Start ajsc.

Query user for ENDPOINT which specifies the spork REPL
endpoint.  ENDPOINT is a string of the form: \"hostname:port\"."
  (interactive
   (let ((endpoint ajsc-default-endpoint))
     (list
      (read-string (format "REPL endpoint (default '%s'): " endpoint)
                   endpoint nil endpoint))))
  (let* ((ep (split-string endpoint ":"))
         (host (car ep))
         (port (string-to-number (cadr ep))))
    (with-temp-message (format "Connecting to REPL on '%s:%d'..."
                               host port)
      (condition-case nil
          (let ((proc-buffer (make-comint-in-buffer ajsc-repl-process-name
                                                    ajsc-repl-buffer-name
                                                    (cons host port))))
            (when (not proc-buffer)
              (error "Failed to connect to %s:%d" host port))
            (when-let* ((proc-buffer-name (buffer-name proc-buffer))
                        (repl-process (get-buffer-process proc-buffer-name))
                        (repl-buffer (get-buffer ajsc-repl-buffer-name))
                        (current-buffer (current-buffer)))
              (set-process-sentinel repl-process #'ajsc-sentinel)
              ;; XXX: without this, header bytes were being interpreted as
              ;;      multibyte sometimes
              (set-process-coding-system repl-process 'binary)
              (ajsc-send-hello repl-process endpoint)
              (with-current-buffer repl-buffer
                (ajsc-mode)
                (pop-to-buffer (current-buffer))
                (goto-char (point-max))
                (pop-to-buffer current-buffer))))))))

(provide 'ajsc)

;;; ajsc.el ends here
