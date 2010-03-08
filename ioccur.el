;;; ioccur.el --- Incremental occur.

;; Author: Thierry Volpiatto <thierry dot volpiatto at gmail dot com>

;; Copyright (C) 2010 Thierry Volpiatto, all rights reserved.

;; Compatibility: GNU Emacs 23.1+

;; X-URL: http://mercurial.intuxication.org/hg/ioccur

;; This file is not part of GNU Emacs. 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Install:
;;  -------

;; Add this file to your `load-path', BYTE-COMPILE it and
;; add (require 'ioccur) in your .emacs.
;; Start with M-x ioccur


;;; Code:
(require 'derived)
(eval-when-compile (require 'cl))

(defvar ioccur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'ioccur-quit)
    (define-key map (kbd "RET") 'ioccur-jump-and-quit)
    (define-key map (kbd "<C-down>") 'ioccur-scroll-down)
    (define-key map (kbd "<C-up>") 'ioccur-scroll-up)
    (define-key map (kbd "<down>") 'ioccur-next-line)
    (define-key map (kbd "<up>") 'ioccur-precedent-line)
    (define-key map (kbd "C-n") 'ioccur-next-line)
    (define-key map (kbd "C-p") 'ioccur-precedent-line)
    map)
  "Keymap used for ioccur commands.")


(defgroup ioccur nil
  "Mode that provide incremental searching in buffer."
  :prefix "ioccur-"
  :group 'text)

;;; User variables.
(defcustom ioccur-search-delay 0.2
  "*During incremental searching, display is updated all these seconds."
  :group 'ioccur
  :type  'integer)

(defcustom ioccur-search-prompt "Pattern: "
  "*Prompt used for `ioccur-occur'."
  :group 'ioccur
  :type  'string)

(defcustom ioccur-mode-line-string
  " RET:Exit, C-g:Quit, C-k:Kill, C-z:Jump, C-j:Jump&quit, C-n/p:Next/Prec-line, M-p/n:Hist, C/M-v C-down/up:Scroll, C-w:Yank tap"
  "*Documentation of `ioccur' prompt displayed in mode-line.
Set it to nil to remove doc in mode-line."
  :group 'ioccur
  :type  'string)

(defcustom ioccur-length-line 80
  "*Length of the line displayed in ioccur buffer."
  :group 'ioccur
  :type 'integer)

(defcustom ioccur-max-length-history 100
  "*Maximum number of element stored in `ioccur-history'."
  :group 'ioccur
  :type 'integer)

;;; Faces.
(defface ioccur-overlay-face '((t (:background "Green4" :underline t)))
  "Face for highlight line in ioccur buffer."
  :group 'ioccur-faces)

(defface ioccur-match-overlay-face '((t (:background "Indianred4" :underline t)))
  "Face for highlight line in matched buffer."
  :group 'ioccur-faces)

(defface ioccur-title-face '((t (:background "Dodgerblue4")))
  "Face for highlight incremental buffer title."
  :group 'ioccur-faces)

(defface ioccur-regexp-face '((t (:background "DeepSkyBlue" :underline t)))
  "Face for highlight found regexp in incremental buffer."
  :group 'ioccur-faces)

(defface ioccur-num-line-face '((t (:foreground "OrangeRed")))
  "Face for highlight number line in ioccur buffer."
  :group 'ioccur-faces)

;;; Internal variables.
(defvar ioccur-search-pattern "")
(defvar ioccur-search-timer nil)
(defvar ioccur-quit-flag nil)
(defvar ioccur-current-buffer nil)
(defvar ioccur-occur-overlay nil)
(defvar ioccur-exit-and-quit-p nil)
(defvar ioccur-history nil)
(defvar ioccur-match-overlay nil)
(defvar ioccur-count-occurences 0)
  

(define-derived-mode ioccur-mode
    text-mode "ioccur"
    "Major mode to search occurences of regexp in current buffer.

Special commands:
\\{ioccur-mode-map}"
    (if ioccur-mode-line-string
        (setq mode-line-format
              '(" " mode-line-buffer-identification " "
                (line-number-mode "%l") " " ioccur-mode-line-string "-%-"))
        (kill-local-variable 'mode-line-format)))

(defsubst* ioccur-position (item seq &key (test 'eq))
  "A simple replacement of CL `position'."
  (loop for i in seq for index from 0
     when (funcall test i item) return index))

;;; Iterators.
(defmacro ioccur-iter-list (list-obj)
  "Return an iterator from list LIST-OBJ."
  `(lexical-let ((lis ,list-obj))
     (lambda ()
       (let ((elm (car lis)))
         (setq lis (cdr lis))
         elm))))

(defun ioccur-iter-next (iterator)
  "Return next elm of ITERATOR."
  (funcall iterator))

(defun ioccur-iter-circular (seq)
  "Infinite iteration on SEQ."
  (lexical-let ((it  (ioccur-iter-list seq))
                (lis seq))
    (lambda ()
      (let ((elm (iter-next it)))
        (or elm
            (progn (setq it (ioccur-iter-list lis)) (iter-next it)))))))

(defun ioccur-butlast (seq pos)
  "Return SEQ from index 0 to POS."
  (butlast seq (- (length seq) pos)))

(defun* ioccur-sub-prec-circular (seq elm &key (test 'eq))
  "Infinite reverse iteration of SEQ starting at ELM."
  (lexical-let* ((rev-seq  (reverse seq))
                 (pos      (iter-position elm rev-seq :test test))
                 (sub      (append (nthcdr (1+ pos) rev-seq) (ioccur-butlast rev-seq pos)))
                 (iterator (ioccur-iter-list sub)))
     (lambda ()
       (let ((elm (iter-next iterator)))
         (or elm
             (progn (setq iterator (ioccur-iter-list sub)) (iter-next iterator)))))))

(defun* ioccur-sub-next-circular (seq elm &key (test 'eq))
  "Infinite iteration of SEQ starting at ELM."
  (lexical-let* ((pos      (iter-position elm seq :test test))
                 (sub      (append (nthcdr (1+ pos) seq) (ioccur-butlast seq pos)))
                 (iterator (ioccur-iter-list sub)))
     (lambda ()
       (let ((elm (iter-next iterator)))
         (or elm (progn
                   (setq iterator (ioccur-iter-list sub))
                   (iter-next iterator)))))))

(defsubst* ioccur-find-readlines (bfile regexp &key (insert-fn 'file))
  "Return an alist of all the (num-line line) of a file or buffer BFILE matching REGEXP."
  (let ((count 0)
        (fn    (case insert-fn
                 ('file 'insert-file-contents)
                 ('buffer 'insert-buffer-substring))))
    (with-temp-buffer
      (funcall fn bfile) ; call insert function
      (goto-char (point-min))
      (loop
         with lines-list = (split-string (buffer-string) "\n")
         for i in lines-list when (string-match regexp i)
         collect (list count (replace-regexp-in-string "\n" "" i)) into lis
         do (incf count)
         finally return lis))))

(defun* ioccur-buffer-process-ext (regex buffer &key (lline ioccur-length-line))
  "Function to process buffer in external program like anything."
  (setq ioccur-count-occurences 0)
  (let ((matched-lines (ioccur-find-readlines buffer regex :insert-fn 'buffer)))
    (when matched-lines
      (dolist (i matched-lines) ; Each element is of the form '(key value)
        (let* ((ltp           (second i))
               (replace-reg   (if (string-match "^\t" ltp) "\\(^\t*\\)" "\\(^ *\\)"))
               (new-ltp       (replace-regexp-in-string replace-reg "" ltp))
               (line-to-print new-ltp))
          (incf ioccur-count-occurences)
          (insert (concat " " (propertize (int-to-string (+ (first i) 1))
                                          'face 'ioccur-num-line-face
                                          'help-echo line-to-print)
                          ":"
                          (if (> (length line-to-print) lline)
                              (substring line-to-print 0 lline)
                              line-to-print)
                          "\n")))))))

(defun ioccur-quit ()
  "Quit and kill ioccur buffer."
  (interactive)
  (when ioccur-match-overlay
    (delete-overlay ioccur-match-overlay))
  (quit-window t)
  (other-window 1)
  (delete-other-windows))

(defun ioccur-goto-line (numline)
  "Non--interactive version of `goto-line.'"
  (goto-char (point-min)) (forward-line (1- numline)))

(defun ioccur-forward-line (n)
  "Forward line only if it is not an empty line."
  (let (pos)
    (save-excursion
      (forward-line n) (forward-line 0)
      (when (looking-at "^ [0-9]+") (forward-line 0) (setq pos (point))))
  (when pos (goto-char pos) (ioccur-color-current-line))))

;;;###autoload
(defun ioccur-next-line ()
  "Goto next line if it is not an empty line."
  (interactive)
  (ioccur-forward-line 1))

;;;###autoload
(defun ioccur-precedent-line ()
  "Goto precedent line if it is not an empty line."
  (interactive)
  (ioccur-forward-line -1))

(defun ioccur-jump ()
  "Jump to line in other buffer and put an overlay on it."
  (let ((line (buffer-substring (point-at-bol) (point-at-eol)))
        pos)
    (unless (or (string= line "")
                (string= line "Ioccur"))
      (when (string-match "[0-9]+" line)
        (setq pos (string-to-number (match-string 0 line))))
      (pop-to-buffer ioccur-current-buffer)
      (ioccur-goto-line pos) (ioccur-color-matched-line))))


;;;###autoload
(defun ioccur-jump-and-quit ()
  "Jump to line in other buffer and quit search buffer."
  (interactive)
  (when (ioccur-jump)
    (delete-other-windows)
    (sit-for 0.3)
    (when ioccur-match-overlay
      (delete-overlay ioccur-match-overlay))))

(defun ioccur-scroll (n)
  "Scroll other buffer and move overlay accordingly."
  (ioccur-forward-line n)
  (ioccur-color-current-line)
  (when (ioccur-jump)
    (other-window 1)))

;;;###autoload
(defun ioccur-scroll-down ()
  "Scroll other buffer down."
  (interactive)
  (ioccur-scroll 1))

;;;###autoload
(defun ioccur-scroll-up ()
  "Scroll other buffer up."
  (interactive)
  (ioccur-scroll -1))

(defun ioccur-read-char-or-event (prompt)
  "Read keyboard input with `read-char' and `read-event' if `read-key' not available."
  (if (fboundp 'read-key)
      (read-key prompt)
      (let* ((chr (condition-case nil (read-char prompt) (error nil)))
             (evt (unless chr (read-event))))
        (or chr evt))))

(defun ioccur-read-search-input (initial-input start-point)
  "Read each keyboard input and add it to `ioccur-search-pattern'."
  (let* ((prompt         (propertize ioccur-search-prompt 'face 'minibuffer-prompt))
         (inhibit-quit   (not (fboundp 'read-key)))
         (tmp-list       ())
         (it             (ioccur-iter-circular ioccur-history))
         (cur-hist-elm   (car ioccur-history))
         (start-hist     nil) ; Flag to notify if cycling history started.
         (old-yank-point start-point)
         yank-point)
    (unless (string= initial-input "")
      (loop for char across initial-input do (push char tmp-list)))
    (setq ioccur-search-pattern initial-input)
    ;; Cycle history function.
    ;;
    (flet ((cycle-hist (arg)
             (if ioccur-history
                 (progn
                   ;; Cycle history started
                   ;; We build a new iterator based on a sublist
                   ;; starting at the current element of history.
                   ;; This is a circular iterator. (no end)
                   (when start-hist
                     (if (< arg 0) ; M-p (move from left to right in ring).
                         (setq it (ioccur-sub-next-circular ioccur-history
                                                            cur-hist-elm :test 'equal))
                         (setq it (ioccur-sub-prec-circular ioccur-history
                                                            cur-hist-elm :test 'equal))))
                   (setq tmp-list nil)
                   (let ((next (ioccur-iter-next it)))
                     (setq next (ioccur-iter-next it))
                     (setq start-hist nil)
                     (setq initial-input (or next "")))
                   (unless (string= initial-input "")
                     (loop for char across initial-input do (push char tmp-list))
                     (setq cur-hist-elm initial-input))
                   (setq ioccur-search-pattern initial-input)
                   (setq start-hist t))
                 (message "No history available.") (sit-for 2) t))
           ;; Maybe start timer.
           ;;
           (start-timer ()
             (unless ioccur-search-timer
               (ioccur-start-timer)))
           ;; Maybe stop timer.
           ;;
           (stop-timer ()
             (when ioccur-search-timer
               (ioccur-cancel-search))))
      ;; Start incremental loop.
      (while (let ((char (ioccur-read-char-or-event
                          (concat prompt ioccur-search-pattern))))
               (case char
                 ((down ?\C-n)                 ; Next line.
                  (stop-timer) (ioccur-next-line)
                  (ioccur-color-current-line) t)
                 ((up ?\C-p)                   ; Precedent line.
                  (stop-timer) (ioccur-precedent-line)
                  (ioccur-color-current-line) t)
                 (C-down                       ; Scroll both windows down.
                  (stop-timer)
                  (ioccur-scroll-down) t)
                 (C-up                         ; Scroll both windows up.
                  (stop-timer) (ioccur-scroll-up) t)
                 ((?\e ?\r) (message nil) nil) ; RET or ESC break and exit code.
                 (?\d                          ; Delete backward with DEL.
                  (start-timer)
                  (with-current-buffer ioccur-current-buffer
                    (goto-char old-yank-point)
                    (setq yank-point old-yank-point))
                  (pop tmp-list) t)
                 (?\C-g                        ; Quit and restore buffers.
                  (setq ioccur-quit-flag t) nil)
                 ((or right ?\C-z)             ; Persistent action.
                  (ioccur-jump) (other-window 1) t)
                 ((left ?\C-j)                 ; Jump to candidate and kill search buffer.
                  (setq ioccur-exit-and-quit-p t) nil)
                 (?\C-v                        ; Scroll down.
                  (scroll-other-window 1) t)
                 (?\M-v                        ; Scroll up.
                  (scroll-other-window -1) t)
                 (?\C-k                        ; Kill input.
                  (start-timer)
                  (with-current-buffer ioccur-current-buffer
                    (goto-char old-yank-point)
                    (setq yank-point old-yank-point))
                  (kill-new ioccur-search-pattern) (setq tmp-list ()) t)
                 (?\C-w                        ; Yank stuff at point.
                  (start-timer)
                  (with-current-buffer ioccur-current-buffer
                    (unless old-yank-point (setq old-yank-point (point)))
                    (setq yank-point (point)) (forward-word 1)
                    (setq initial-input (buffer-substring yank-point (point))))
                  (unless (string= initial-input "")
                    (loop for char across initial-input do (push char tmp-list)))
                  (setq ioccur-search-pattern initial-input) t)
                 (?\M-p                        ; Precedent history elm.
                  (start-timer)
                  (cycle-hist -1))
                 (?\M-n                        ; Next history elm.
                  (start-timer)
                  (cycle-hist 1))
                 (t                            ; Store character.
                  (start-timer)
                  (if (characterp char)
                      (push char tmp-list)
                      ;; Else, a non--character event is entered, not listed above.
                      ;; add it to `unread-command-events' and exit (nil) .
                      (setq unread-command-events
                            (nconc (mapcar 'identity (this-single-command-raw-keys))
                                   unread-command-events))
                      nil))))
        (setq ioccur-search-pattern (apply 'string (reverse tmp-list)))))))


(defun ioccur-filter-alist-by-regexp (regexp buffer-name)
  "Print all lines matching REGEXP in current buffer to buffer BUFFER-NAME."
  (let ((title (propertize "Ioccur" 'face 'ioccur-title-face)))
    (if (string= regexp "")
        (progn (erase-buffer) (insert (concat title "\n\n")))
        (erase-buffer)
        (ioccur-buffer-process-ext regexp buffer-name :lline ioccur-length-line)
        (goto-char (point-min))
        (insert (concat title "\n\n"
                 (propertize (format "Found %s occurences of " ioccur-count-occurences)
                             'face 'underline)
                 (propertize regexp 'face 'ioccur-regexp-face)
                 (propertize (format " in %s" buffer-name) 'face 'underline) "\n\n"))
        (ioccur-color-current-line))))


(defun ioccur-start-timer ()
  "Start ioccur incremental timer."
  (setq ioccur-search-timer
        (run-with-idle-timer
         ioccur-search-delay 'repeat
         #'(lambda ()
             (ioccur-filter-alist-by-regexp
              ioccur-search-pattern
              ioccur-current-buffer)))))


;;;###autoload
(defun ioccur (&optional initial-input)
  "Incremental search of lines in current buffer matching input.

With a prefix arg search symbol at point (INITIAL-INPUT).

While you are incremental searching, commands provided are:

C-n or <down>  next line.
C-p or <up>    precedent line.
C-v and M-v    scroll up and down.
C-z or <right> jump without quitting loop.
C-j or <left>  jump and exit search buffer.
RET or ESC     exit but don't quit search buffer.
DEL            remove last character entered.
C-k            Kill current input.
C-w            Yank stuff at point.
C-g            quit and restore buffer.
M-p/n          Precedent and next `ioccur-history' element:

M-p ,-->A B C D E F G H I---,
    |                       |
    `---I H G F E D C B A<--'

M-n ,-->I H G F E D C B A---,
    |                       |
    `---A B C D E F G H I<--'

When you quit incremental search with RET or ESC, see `ioccur-mode'
for commands provided in the search buffer."
  (interactive "P")
  (setq ioccur-exit-and-quit-p nil)
  (setq ioccur-current-buffer (buffer-name (current-buffer)))
  (with-current-buffer ioccur-current-buffer
    (jit-lock-fontify-now))
  (let* ((init-str (if initial-input (thing-at-point 'symbol) ""))
         (len      (length init-str))
         (curpos   (point))
         str-no-prop)
    (set-text-properties 0 len nil init-str)
    (setq str-no-prop init-str)
    (pop-to-buffer (get-buffer-create "*ioccur*"))
    (ioccur-mode)
    (unwind-protect
         ;; Start incremental search.
         (progn
           (ioccur-start-timer)
           (ioccur-read-search-input str-no-prop curpos))
      ;; At this point incremental search loop is exited.
      (progn
        (ioccur-cancel-search)
        (kill-local-variable 'mode-line-format)
        (when (equal (buffer-substring (point-at-bol) (point-at-eol)) "")
          (setq ioccur-quit-flag t))
        (if ioccur-quit-flag ; C-g
            (progn
              (kill-buffer "*ioccur*")
              (switch-to-buffer ioccur-current-buffer)
              (when ioccur-match-overlay
                (delete-overlay ioccur-match-overlay))
              (delete-other-windows) (goto-char curpos) (message nil))

            (if ioccur-exit-and-quit-p
                (progn (ioccur-jump-and-quit)
                       (kill-buffer "*ioccur*") (message nil))
                (ioccur-jump) (other-window 1))
            ;; Push elm in history if not already there or empty.
            (unless (or (member ioccur-search-pattern ioccur-history)
                        (string= ioccur-search-pattern ""))
              (push ioccur-search-pattern ioccur-history))
            ;; If elm already exists in history ring push it on top of stack.
            (let ((pos-hist-elm (ioccur-position ioccur-search-pattern
                                               ioccur-history :test 'equal)))
              (unless (string= (car ioccur-history)
                               ioccur-search-pattern)
                (push (pop (nthcdr pos-hist-elm ioccur-history))
                      ioccur-history)))
            (when (> (length ioccur-history) ioccur-max-length-history)
              (setq ioccur-history (delete (car (last ioccur-history)) ioccur-history))))
        (setq ioccur-count-occurences 0)
        (setq ioccur-quit-flag nil)))))


(defun ioccur-cancel-search ()
  "Cancel timer used for ioccur searching."
  (when ioccur-search-timer
    (cancel-timer ioccur-search-timer)
    (setq ioccur-search-timer nil)))


(defun ioccur-color-current-line ()
  "Highlight and underline current line in ioccur buffer."
  (if ioccur-occur-overlay
      (move-overlay ioccur-occur-overlay
                    (point-at-bol) (1+ (point-at-eol)))
      (setq ioccur-occur-overlay
            (make-overlay (point-at-bol) (1+ (point-at-eol)))))
  (overlay-put ioccur-occur-overlay 'face 'ioccur-overlay-face))

(defun ioccur-color-matched-line ()
  "Highlight and underline current position on matched line in current-buffer."
  (if ioccur-match-overlay
      (move-overlay ioccur-match-overlay
                    (point-at-bol) (1+ (point-at-eol)))
      (setq ioccur-match-overlay
            (make-overlay (point-at-bol) (1+ (point-at-eol)))))
  (overlay-put ioccur-match-overlay 'face 'ioccur-match-overlay-face))

;;; Provide
(provide 'ioccur)

;;; ioccur.el ends here.
