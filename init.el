(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     csv
     yaml
     sql
     (haskell :variables
              haskell-completion-backend 'intero)
     graphviz
     octave
     auto-completion
     better-defaults
     common-lisp
     emacs-lisp
     games
     git
     helm
     html
     javascript
     (latex :variables
            latex-build-command "LaTeX")
     markdown
     ocaml
     org
     ranger
     rust
     (shell :variables
            shell-default-height 20
            shell-default-position 'bottom
            shell-default-shell 'eshell
            shell-enable-smart-eshell t)
     syntax-checking
     (theming :variables
              theming-headings-same-size 'all)
     themes-megapack
     version-control
     )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(evil-search-highlight-persist)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((projects . 3)
                                (recents . 2))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(solarized-light
                         material
                         flatland
                         spacemacs-light
                         spacemacs-dark
                         material-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Code"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize t
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.6
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing))

(defun dotspacemacs/user-init ()
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq ranger-override-dired t))

(defun dotspacemacs/user-config ()
  (evil-ex-define-cmd "q[uit]" 'evil-window-delete)

  (setq-default fill-column 100)
  (add-hook 'prog-mode-hook 'spacemacs/toggle-fill-column-indicator-on)
  (add-hook 'text-mode-hook 'spacemacs/toggle-fill-column-indicator-on)

  (defun hjg/split-window-right-and-helm ()
    (interactive)
    (split-window-right-and-focus)
    (helm-mini))

  (spacemacs/set-leader-keys "wV" 'hjg/split-window-right-and-helm)

  (spacemacs/set-leader-keys "dd" 'kill-buffer-and-window)

  (setq fci-always-use-textual-rule t)

  (require 'helm-bookmark)

  (setq flycheck-gometalinter-deadline "20s")
  (setq flycheck-gometalinter-fast nil)

  ; Coq
  (load "~/.spacemacs.d/packages/proof-general/generic/proof-site")
  (with-eval-after-load "proof-script"
    (define-key proof-mode-map (kbd "<M-down>") 'proof-assert-next-command-interactive)
    (define-key proof-mode-map (kbd "<M-up>") 'proof-undo-last-successful-command))

  ; LaTeX
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (setq LaTeX-command "latex -shell-escape")

  ; Org
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (defun hjg/config-export ()
    (defadvice org-export-output-file-name
        (before org-add-export-dir activate)
      (when (not pub-dir)
        (setq pub-dir "export")
        (when (not (file-directory-p pub-dir))
          (make-directory pub-dir)))))
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (octave . t)
                                 (dot . t)))
  (with-eval-after-load 'org
    (hjg/config-export)
    (setq org-directory "~/Org/")
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    (setq org-export-backends '(beamer html latex md gfm))
    (setq org-ellipsis "⬎")
    (setq org-bullets-bullet-list '("▣" "►" "■" "▸" "▪"))
    (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate))

  ; Markdown
  (add-hook 'markdown-mode-hook 'auto-fill-mode)

  ; Haskell
  (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "f" 'hindent-reformat-buffer)
  (with-eval-after-load 'intero
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))
