;;; demigod-mode.el --- Always-on variant of god-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Will Dey

;; Author: Will Dey
;; Keywords: convenience, files, keyboard
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Generate README:
;;; Commentary:

;; describe-prefix bindings disabled. Use Which Key

;; Which Key minibuffer option disabled. Use side windows or frames. Side window on bottom looks like minibuffer
;; Which Key C-h commands don't work. `which-key-use-C-h-commands' to `nil' highly recommended. Undo-key already implemented, rest is weird UX.

;;; Code:

;; TODO copy README.org into Commentary.

;; FIXME Always adding C- modifier in terminal Emacs. Unable to press M-x

;; TODO Keyboard macro recording becomes slow with Demigod mode
;; TODO Demigod disabling sometimes on other top-level errors?
;; TODO Implement `describe-bindings' for prefix keys on C-h anywhere in sequence
;; TODO Reorganize

(defgroup demigod-mode nil
  ""
  :group 'editing)


;;; Faces

(defface demigod-key
  '((t :height 0.9 :box t :inherit (variable-pitch highlight)))
  "")

(defface demigod-prefix-argument
  '((t :inherit minibuffer-prompt))
  "")

(defcustom demigod-prefix-format "x%S "
  ""
  :type 'string)

(defun demigod--fontified-key-description (keys)
  (mapconcat (lambda (key)
	       (propertize key
			   'face 'demigod-key))
	     (split-string (key-description keys))
	     " "))



(defmacro demigod-with-no-overriding-maps (&rest body)
  "Run BODY with `overriding-terminal-local-map' set to nil."
  (declare (indent defun) (debug t))
  `(let (overriding-terminal-local-map)
     ,@body))

(defvar demigod-old-global-map nil
  "")

(defmacro demigod-with-global-map (keymap &rest body)
  ""
  (declare (indent defun) (debug t))
  (let ((save (when (eq (car body) :save)
		(setq body (cdr body))
		t))
	(old-global-map (make-symbol "old-global-map")))
    `(let ,(nconc `((,old-global-map (current-global-map)))
		  (when save
		    '((demigod-old-global-map (current-global-map)))))
       (unwind-protect
	   (progn
	     (use-global-map ,keymap)
	     ,@body)
	 (use-global-map ,old-global-map)))))

(defun demigod--event-basic-type (event)
  "Return event with control and meta modifiers stripped."
  (event-convert-list (nconc (delq 'control (delq 'meta (event-modifiers event)))
			     (list (event-basic-type event)))))

(defvar demigod--echoing nil)

(defvar demigod--echo-keystrokes-timer nil)

(defvar demigod--which-key-timer nil)

(defun demigod--echo (prompt keys-description)
  (setq demigod--echoing t)
  (let (message-log-max)
    (message (concat prompt keys-description))))

(defun demigod--cancel-timers ()
  (when demigod--echo-keystrokes-timer
    (setq demigod--echo-keystrokes-timer (cancel-timer demigod--echo-keystrokes-timer)))
  (when demigod--which-key-timer
    (setq demigod--which-key-timer (cancel-timer demigod--which-key-timer))))

(defmacro demigod-do-which-key (&rest body)
  ""
  (declare (indent defun) (debug t))
  `(let ((inhibit-message t))
     (when (and (bound-and-true-p which-key-mode)
		(which-key--popup-showing-p))
       ,@body)))

;;;###autoload
(defun demigod-which-key-show-previous-page-cycle ()
  (interactive)
  (demigod-do-which-key
    (which-key--show-page -1)))

;;;###autoload
(defun demigod-which-key-show-previous-page-no-cycle ()
  (interactive)
  (demigod-do-which-key
    (unless (which-key--on-first-page)
      (which-key--show-page -1))))

;;;###autoload
(defun demigod-which-key-show-next-page-cycle ()
  (interactive)
  (demigod-do-which-key
    (which-key--show-page 1)))

;;;###autoload
(defun demigod-which-key-show-next-page-no-cycle ()
  (interactive)
  (demigod-do-which-key
    (unless (which-key--on-last-page)
      (which-key--show-page 1))))

(defun demigod--which-key-create-buffer-and-show (&optional prefix-keys
							    from-keymap
							    filter
							    prefix-title)
  (let ((inhibit-message t))
    (which-key--create-buffer-and-show prefix-keys
				       from-keymap
				       filter
				       prefix-title)))

(defun demigod--which-key (prompt keys-description keys &optional from-keymap)
  (demigod-with-global-map demigod-old-global-map
    (demigod--which-key-create-buffer-and-show keys from-keymap))
  (demigod--echo prompt keys-description))

(defun demigod--read-key (prompt keys-description keys &optional from-keymap)
  (demigod--cancel-timers)
  ;;;; Which Key integration
  (when (bound-and-true-p which-key-mode)
    (if (which-key--popup-showing-p)
	;; Immediate update (no secondary delay):
	(demigod--which-key-create-buffer-and-show keys from-keymap)
      (setq demigod--which-key-timer (run-with-idle-timer which-key-idle-delay
							  nil
							  #'demigod--which-key
							  prompt
							  keys-description
							  keys
							  from-keymap))))
  ;;;; Echo keystrokes
  (cond ((or demigod--echoing
	     (and (number-or-marker-p echo-keystrokes)
		  (= echo-keystrokes 0)))
	 ;; Immediate show:
	 (setq prompt (concat prompt keys-description)))
	(echo-keystrokes
	 (setq demigod--echo-keystrokes-timer (run-with-idle-timer echo-keystrokes
								   nil
								   #'demigod--echo
								   prompt
								   keys-description))))
  ;;;; Reading
  (let (prefix-arg
	(overriding-local-map read-key-empty-map)
	(echo-keystrokes 0)
	(timer (run-with-idle-timer
                read-key-delay t
		(lambda ()
		  (let ((keys (this-command-keys-vector)))
		    (unless (zerop (length keys))
                      (throw 'demigod--read-key keys)))))))
    (unwind-protect
	(demigod-with-global-map (let ((map (make-sparse-keymap)))
				   ;; Don't hide the menu-bar and tool-bar entries.
				   (define-key map [menu-bar] (lookup-key global-map [menu-bar]))
				   (define-key map [tool-bar]
				     (or (cdr (assq 'tool-bar global-map))
	  				 (lookup-key global-map [tool-bar])))
				   map)
	  :save
	  (let* ((keys (catch 'demigod--read-key
			 ;; We don't advise `read-key-sequence-vector':
			 (read-key-sequence-vector prompt nil t)))
		 (key (aref keys 0)))
	    (if (and (> (length keys) 1)
		     (memq key '(mode-line
				 header-line
				 left-fringe
				 right-fringe)))
		(aref keys 1)
	      key)))
      (cancel-timer timer)
      ;; Removed (message nil) from original `read-key' code.
      )))

(defcustom demigod-help-char t
  "If non-nil, run `describe-prefix-bindings' on any unbound key sequence that ends in the `help-char' key.

This is the behavior of the default key sequence reader."
  :type 'boolean)

(defun demigod--read-key-sequence (prompt &optional
					  continue-echo
					  _dont-down-case-last ; We know that we ignore `dont-downcase-last' as a matter of preference.
					  _can-return-switch-frame ; Can't use `can-return-switch-frame' because we use `read-key', which can never return switch-frame.
					  cmd-loop)
  "Does not remap"
  (demigod-with-no-overriding-maps
    (let ((first t)
	  current-key
	  keys
	  (prefix (when prefix-arg
		    (propertize (format demigod-prefix-format prefix-arg)
				'face 'demigod-prefix-argument)))
	  keys-description
	  binding
	  ;; Input methods:
	  ;; (input-method-exit-on-first-char (not cmd-loop))
	  ;; (input-method-use-echo-area      (not cmd-loop))
	  )
      (when continue-echo
	;; We need to read the following sequence as a continuation of the one that started the current command.
	(setq keys (this-single-command-keys)
	      keys-description (demigod--fontified-key-description keys)))
      ;; Set `current-key' here.
      (while (let ((modifier-list (list 'control)) ; Prefix everything with control by default.
		   current-key-description)
	       (while (progn
			(when continue-echo
			  (setq first nil))
			(let ((input-method-function (if first
							 input-method-function) ;; TODO Documentation warns about let-binding `input-method-function' because Emacs can switch frame in the middle of `read-key'.
						     ))
			  (if first
			      (setq current-key ?\s
				    first nil)
			    ;; XXX Set `input-method-function' to nil before?
			    (setq current-key-description (propertize (concat (when (memq 'control modifier-list) "C-")
									      (when (memq 'meta    modifier-list) "M-"))
								      'face 'demigod-key)
				  current-key (demigod--read-key prompt
								 (concat prefix
									 keys-description
									 (if keys-description
									     (if (> (length current-key-description) 0)
										 " "
									       (propertize "-"
											   'face 'demigod-key)))
									 current-key-description)
								 keys)))
			;;;; Quit
			  (when (eq current-key ?\C-g)
			    (keyboard-quit))
			  (pcase (demigod--event-basic-type current-key)
			  ;;;; SPC
			    (?\s ; SPC means next key is literal.
			     (setq current-key (demigod--read-key prompt
								  (concat prefix
									  keys-description
									  (when keys-description "-"))
								  keys)
				   current-key-description (propertize (key-description (vector current-key))
								       'face 'demigod-key))
			     nil ; Complete key now, stop looping.
			     )
			  ;;;; DEL
			    (?\d ; DEL means undo key.
			     ;; TODO
			     (when (> (length keys) 0)
			       (setq keys (substring keys 0 -1)
				     keys-description (when (> (length keys) 0)
							(demigod--fontified-key-description keys))
				     modifier-list (list 'control)))
			     t ; Keep looping to get the next key.
			     )
			  ;;;; g
			    (?g ; g toggles meta.
			     (if (memq 'meta modifier-list)
				 (setq modifier-list (delq 'meta modifier-list))
			       (push 'meta modifier-list))
			     t ; Not yet a complete key, keep looping.
			     )
			  ;;;; G
			    (?G ; G (uppercase) toggles control and meta as a unit.
			     (if (and (memq 'control modifier-list)
				      (memq 'meta modifier-list))
				 (setq modifier-list (delq 'control modifier-list)
				       modifier-list (delq 'meta modifier-list))
			       (when (not (memq 'control modifier-list))
				 (push 'control modifier-list))
			       (when (not (memq 'meta modifier-list))
				 (push 'meta modifier-list)))
			     t ; Not yet a complete key, keep looping.
			     )
			  ;;;; Default
			    (key
			     (setq current-key-description (concat current-key-description
								   (propertize (if (characterp key)
										   (char-to-string key)
										 (format "<%s>" key))
									       'face 'demigod-key))
				   current-key (event-convert-list (append modifier-list (list key))))
			     nil ; Complete key now, stop looping.
			     )))))
	       ;; Add to key list.
	       (setq keys (vconcat keys (vector current-key))
		     binding (key-binding keys t t))
	       ;; Ignore downcase last as a matter of preference.
	       (setq keys-description (concat keys-description
					      (if keys-description " ")
					      current-key-description)) 
	       (keymapp binding)))
      ;;;; Help char
      (when (and demigod-help-char
		 (null binding)
		 (eq current-key help-char))
	(describe-bindings (substring keys 0 -1)))
      (cons keys binding))))

(defun demigod-read-key-sequence (prompt &optional
					 continue-echo
					 dont-downcase-last
					 can-return-switch-frame
					 cmd-loop)
  ""
  (car (demigod--read-key-sequence prompt
				   continue-echo
				   dont-downcase-last
				   can-return-switch-frame
				   cmd-loop)))


;;; Recent keys

(defcustom demigod-num-recent-keys 300
  "The max history size returned `demigod-recent-keys' before old keys start getting discarded. This is the key history list that shows up in \\[view-lossage] (`view-lossage')."
  :type 'integer)

(defvar demigod--recent-keys nil
  "")

(defvar demigod--recent-keys-end nil
  "")

(defvar demigod--recent-keys-length 0
  "")

(defun demigod--init-recent-keys ()
  ""
  (setq demigod--recent-keys nil
	demigod--recent-keys-end nil
	demigod--recent-keys-length 0))
(demigod--init-recent-keys)

(defun demigod--append-recent-key (key command)
  "Add a new cons cell (KEY . COMMAND) at the end of the `demigod-recent-keys' list.
Truncate the list at a maximum `demigod-num-recent-keys' as well."
  (let ((new-end (list (cons key command))))
    (if (consp demigod--recent-keys-end)
	(setcdr demigod--recent-keys-end new-end)
      (setq demigod--recent-keys new-end))
    (setq demigod--recent-keys-end new-end)
    (setq demigod--recent-keys-length (1+ demigod--recent-keys-length)))
  (while (> demigod--recent-keys-length demigod-num-recent-keys)
    (setq demigod--recent-keys (cdr demigod--recent-keys)
	  demigod--recent-keys-length (1- demigod--recent-keys-length)))
  demigod--recent-keys)

(defun demigod-recent-keys (&optional include-cmds)
  ""
  (apply #'vector
	 (mapcan (lambda (key-and-command)
		   (append (car key-and-command)
			   (if include-cmds
			       `((nil . ,(cdr key-and-command))))))
		 demigod--recent-keys)))


;;; Prefix argument

(defun demigod--prefix-argument (arg)
  (let ((negative (eq arg '-))
	(number (if (integerp arg) arg 0))
	key)
    (while (progn
	     (setq key (demigod--read-key ""
					  (propertize (format demigod-prefix-format arg)
						      'face 'demigod-prefix-argument)
					  nil
					  universal-argument-map))
	     (cond
	      ;;;; Quit
	      ((eq key ?\C-g)
	       (keyboard-quit))
	      ;;;; C-u
	      ((eq key ?\C-u)
	       (if (consp arg)
		   (setq arg (list (* (car arg) 4)))
		 (setq arg '(4)
		       number 0))
	       t)
	      ;;;; Number key
	      ((and (>= key ?0)
		    (<= key ?9))
	       (setq number (+ (* number 10) (- key ?0)))
	       (setq arg (if (and (> number 0)
				  negative)
			     (- number)
			   number))
	       t)
	      ;;;; Negative
	      ((and (equal arg '(4))
		    (eq key ?-))
	       (setq negative t
		     arg '-)
	       t)
	      ;;;; Done
	      (t
	       (setq prefix-arg arg)
	       nil))))
    (vector key)))



(defun demigod--quick-lookup (keys)
  (let (binding)
    (setq binding (key-binding keys 'accept-default 'no-remap))
    (when (null binding)
      ;; Demigod binds all keys with a default-binding, so Emacs never uses `local-function-key-map' when Demigod is active.
      ;; Translate the key through `local-function-key-map' and then `key-translation-map' ourselves, then retry lookup.
      (let ((function-keys (lookup-key local-function-key-map keys t)))
	(when (arrayp function-keys)
	  (let ((translated-keys (lookup-key key-translation-map function-keys t)))
	    (setq keys (if (arrayp translated-keys)
			   translated-keys
			 function-keys)))
	  (setq binding (key-binding keys 'accept-default 'no-remap)))))
    (cons keys binding)))

;;; Entry point
(defun demigod-entry-point (&optional prefix-arg)
  "read and execute"
  (interactive "P")
  ;; Code is optimized so that in the most common case of a single keystroke that does not have to be translated will search for a binding with `key-binding' and then skip directly to `command-execute'ing the binding, so very little overhead is added to executing common commands like `self-insert-command'.
  (let ((keys (this-single-command-keys))
	binding
	keys-and-binding ; Temporary variable to unpack return values.
	remapping)
    ;;;; Company
    (when (fboundp 'company-install-map)
      (company-install-map))
    (unwind-protect
	(progn
	  (demigod-with-no-overriding-maps ; In order to search for the binding past our overriding-terminal-local-map.
	    ;;;; First lookup
	    (setq keys-and-binding (demigod--quick-lookup keys)
		  keys (car keys-and-binding)
		  binding (cdr keys-and-binding))
	    ;;;; Prefix argument
	    (when (memq binding '(universal-argument negative-argument digit-argument))
	      (setq keys (demigod--prefix-argument (pcase binding
						     ('universal-argument '(4))
						     ('negative-argument  '-)
						     ('digit-argument     (- (event-basic-type (aref keys 0)) ?0))))
		    keys-and-binding (demigod--quick-lookup keys)
		    keys (car keys-and-binding)
		    binding (cdr keys-and-binding))))
	  ;;;; Sequences
	  (when (keymapp binding)
	    ;; Key sequence is more than one keystroke long to complete.
	    ;; Actually activate demigod behavior now.
	    (setq keys-and-binding (demigod--read-key-sequence nil :continue-echo ; Continue echo from already-read key.
                                                               )
		  keys (car keys-and-binding)
		  binding (cdr keys-and-binding))))
      (setq demigod--echoing nil)
      (demigod--cancel-timers)
      (demigod-do-which-key
	(which-key--hide-popup)))
    ;;;; Remapping
    (demigod-with-no-overriding-maps ;; In order to search for the binding past our overriding-terminal-local-map.
      ;; Search for a remapping now, after a complete sequence has been read.
      ;; TODO Bug in emacs? `command-remapping' is always returning `nil'
      ;; The following line reimplements what the `command-remapping' C code does.
      (when (setq remapping (key-binding (vector 'remap binding) nil 'no-remap))
	(setq this-original-command binding
	      binding remapping)))
    ;;;; Add to recent keys
    (demigod--append-recent-key keys binding)
    ;;;; Execute
    (if (null binding)
	(message "%s is undefined" (demigod--fontified-key-description keys))
      (unless remapping
	(setq this-original-command binding))
      (setq this-command binding
	    real-this-command binding
	    last-command-event (aref keys (1- (length keys))))
      (command-execute binding nil keys))))

(defvar demigod-mode-overriding-map
  ;; Don't use `eval-when-compile' because that will that create a quoted object which can't (shouldn't) be modified later.
  (let ((map (make-sparse-keymap)))
    (define-key map [t] #'demigod-entry-point)
    ;; We don't mess with the mouse; leave those bindings undefined in our map:
    (dolist (binding
	     (eval-when-compile
	       (let (bindings)
		 ;; Inspired by Steve Purcell's `disable-mouse' code: https://github.com/purcell/disable-mouse/blob/master/disable-mouse.el#L78
		 (dolist (modifier '(nil "C-" "M-" "S-" "C-M-" "C-S-" "M-S-" "C-M-S-"))
		   (dolist (multiplier '(nil "double-" "triple-"))
		     (dolist (event '("mouse" "up-mouse" "down-mouse" "drag-mouse"))
		       (dolist (number (number-sequence 1 7))
			 (push (kbd (concat modifier
					    "<"
					    multiplier
					    event
					    "-"
					    (number-to-string number)
					    ">"))
			       bindings)))
		     (dolist (wheel '("wheel-up" "wheel-down" "wheel-left" "wheel-right"))
		       (push (kbd (concat modifier
					  "<"
					  multiplier
					  wheel
					  ">"))
			     bindings))))
		 (dolist (target '(mode-line bottom-divider vertical-line left-fringe right-fringe))
		   (push (vector target) bindings))
		 bindings)))
      (define-key map binding nil))
    map)
  "Keymap for `demigod-mode'. Binds all keys with a default binding, re-routes them to `demigod-entry-point'")

(defun demigod-apply-no-overriding-maps (orig-fun &rest args)
  "Run orig-fun with the given arguments, but with `overriding-terminal-local-map' and `overriding-teriminal-local-map' set to nil."
  (demigod-with-no-overriding-maps 
    (apply orig-fun args)))

(defun demigod-advise-input-method (&optional disable)
  ""
  ;; TODO inline
  (when input-method-function
    (if disable
	(remove-function (local 'input-method-function) #'demigod-apply-no-overriding-maps)
      (add-function :around (local 'input-method-function) #'demigod-apply-no-overriding-maps))))

(defun demigod-ignore (&rest args)
  ""
  nil)

;;; Minor mode
;;;###autoload
(define-minor-mode demigod-mode
  nil ;; TODO Copy main part of README.org in here.
  :lighter " C-"
  :global t
  :keymap nil ;; Don't make `demigod-mode-map' a local keymap; we'll set it to be the `overriding-terminal-local-map' ourselves.
  ;;;; Teardown
  ;; Reset from possible previous `demigod-mode' invocations:
  (setq-default overriding-terminal-local-map nil)
  (advice-remove #'describe-key             #'demigod-apply-no-overriding-maps)
  (advice-remove #'describe-function        #'demigod-apply-no-overriding-maps)
  (advice-remove #'substitute-command-keys  #'demigod-apply-no-overriding-maps)
  ;;;;; Company
  (declare-function company--should-complete "company")
  (advice-remove #'company--should-complete #'demigod-apply-no-overriding-maps)
  ;;;;; Which Key
  (advice-remove #'which-key--update #'demigod-ignore)
  ;;;;; Input methods
  (demigod-advise-input-method 'disable)
  (remove-hook 'input-method-activate-hook #'demigod-advise-input-method)
  ;;;;; All key sequences
  (advice-remove #'read-key-sequence #'demigod-read-key-sequence)
    ;;;;; Recent keys
  (advice-remove #'recent-keys #'demigod-recent-keys)
  (demigod--init-recent-keys)
  (when demigod-mode
    ;;;; Construction
    (setq-default overriding-terminal-local-map demigod-mode-overriding-map)
    (advice-add #'describe-key             :around #'demigod-apply-no-overriding-maps)
    (advice-add #'describe-function        :around #'demigod-apply-no-overriding-maps)
    (advice-add #'substitute-command-keys  :around #'demigod-apply-no-overriding-maps)
    ;;;;; Company
    (advice-add #'company--should-complete :around #'demigod-apply-no-overriding-maps)
    ;;;;; Which Key
    (advice-add #'which-key--update :override #'demigod-ignore)
    ;;;;; Input methods
    (demigod-advise-input-method)
    (add-hook 'input-method-activate-hook #'demigod-advise-input-method)
    ;;;;; All key sequences
    (advice-add #'read-key-sequence :override #'demigod-read-key-sequence)
    ;;;;; Recent keys
    (advice-add #'recent-keys :override #'demigod-recent-keys)))

(provide 'demigod-mode)
