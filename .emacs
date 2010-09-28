;; David Ackerman's emacs file

;; Credits for a good chunk of these features:
;; - http://xahlee.org/emacs/emacs23_features.html
;; - http://xahlee.org/emacs/emacs_make_modern.html
;;
;; Some Emacs key tricks that I haven't mastered yet:
;; - prefix every line of a region: M-x string-rectangle [text-to-insert] (or C-x r t)
;; - delete x characters in a rectangle: M-x kill-rectangle (or C-x r k)

; Set load path
(add-to-list 'load-path "~/emacs/")

; Get rid of pesky menu bar and tool bar.  We don't need them.
(menu-bar-mode -1)
(tool-bar-mode -1)

; Line numbers
(global-linum-mode 1)

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
nnnnn
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
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 82 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
