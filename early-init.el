(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

(setq inhibit-startup-echo-area-message (user-login-name))

(setq frame-resize-pixelwise t)
(tool-bar-mode -1)
(setq default-frame-alist '((fullscreen . maximized)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)
                            (alpha-background . 100)))

(set-face-background 'default "#000000")
(set-face-foreground 'default "#cccccc")
