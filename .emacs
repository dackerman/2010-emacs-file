;; David Ackerman's emacs file

;; Credits for a good chunk of these features:
;; - http://xahlee.org/emacs/emacs23_features.html
;; - http://xahlee.org/emacs/emacs_make_modern.html
;; - http://code.google.com/p/js2-mode/wiki/InstallationInstructions
;; - http://github.com/nickewing/dotfiles/blob/master/emacs
;;
;; Some Emacs key tricks that I haven't mastered yet:
;; - prefix every line of a region: M-x string-rectangle [text-to-insert] (or C-x r t)
;; - delete x characters in a rectangle: M-x kill-rectangle (or C-x r k)

; David Ackerman's backward-kill-word!
; Tweaked from scottfrazer on stackoverflow at http://goo.gl/S9c1
(defun daves-backward-kill-word ()
  "Behaves like normal backward-kill-word, except:
    - Killing while in whitespace only kills the whitespace.
    - Killing while in special chars only kills the special chars."
  (interactive)
  (if (bolp) (backward-delete-char 1)
  (if (string-match "^\\s-+$" (buffer-substring (point-at-bol) (point))) (kill-region (point-at-bol) (point))
  (if (string-match "[\]\[()*+\\-]+$" (buffer-substring (point-at-bol) (point))) (kill-region (+ (point-at-bol) (match-beginning 0)) (point))
  (if (string-match "[[:blank:]]+$" (buffer-substring (point-at-bol) (point))) (kill-region (+ (point-at-bol) (match-beginning 0)) (point))
  (backward-kill-word 1))))))

(global-set-key [C-backspace] 'daves-backward-kill-word)

; Set load path
(add-to-list 'load-path "~/emacs/")

; Get rid of pesky menu bar, tool bar, and scroll bar.  We don't need them.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 0)

; Line numbers
(global-linum-mode 1)

; ido Mode
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

; Highlight the other paren when hovering over one of them
(show-paren-mode 1)

; Delete trailing whitespace upon save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Stop making backup files!
(setq make-backup-files nil)
(setq auto-save-default nil)

; Toggle line wrapping
(global-set-key (kbd "<f7>") 'toggle-truncate-lines)

; Steve Yegge's on-the-fly js compiler for syntax checking!
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; I like inverted colors
(set-background-color "black")
(set-foreground-color "white")

; Start a server for emacsclient
(server-start)

; Handle XML better with NXML Mode (jclark.com)
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

; Per Steve Yegge's suggestion, I like to execute M-x with C-x C-m
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Allow renaming of files from emacs! Thanks Stevey!
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file name new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))))

;; GUI-set variables
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(frame-background-mode nil)
 '(js2-basic-offset 2))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 75 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
