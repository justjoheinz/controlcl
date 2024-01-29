(in-package :controlcl)


;; /* http://clrs.cc/ */

;;other colors: #ff3838 red-salmon; #08ffb4 turquoise; #40afff light-blue; #f3eddb beige;

(defvar *colors* '(:navy #xFF001F3F :blue #xFF0074D9 :aqua #xFF7FDBFF :teal #xFF39CCCC
                   :olive #xFF3D9970 :green #xFF2ECC40 :lime #xFF01FF70 :yellow #xFFFFDC00
                   :orange #xFFFF851B :red #xFFFF4136 :maroon #xFF85144B :fuchsia #xFFF012BE
                   :purple #xFFB10DC9 :white #xFFFFFFFF :silver #xFFDDDDDD :gray #xFFAAAAAA
                   :black #xFF111111 :red-salmon #xFFff3838 :turquoise #xFF08ffb4 :light-blue #xFF40afff :beige #xFFf3eddb))



;; Color themes with fg, bg, active, caption, value

(defvar *theme-retro* '(:fg #xFF00698c :bg #xFF003652 :active #xFF08a2cf :caption #xFFFFFFFF :value #xFFFFFFFF))
(defvar *theme-cp52014* '(:fg #xFF0074D9 :bg #xFF002D5A :active #xFF00aaFF :caption #xFFFFFFFF :value #xFFFFFFFF))
(defvar *theme-cp5blue* '(:fg #xFF016c9e :bg #xFF02344d :active #xFF00b4ea :caption #xFFFFFFFF :value #xFFFFFFFF))
(defvar *theme-red* '(:fg #xFFaa0000 :bg #xFF660000 :active #xFFFF0000 :caption #xFFFFFFFF :value #xFFFFFFFF))
(defvar *theme-grey* '(:fg #xFFeeeeee :bg #xFFbbbbbb :active #xFFFFFFFF :caption #xFF555555 :value #xFF555555))
(defvar *theme-a* '(:fg #xFF00FFC8 :bg #xFF00D7FF :active #xFFFFFF00 :caption #xFF00B0FF :value #xFF00B0FF))
(defvar *current-theme* *theme-retro*)

(defmacro with-theme ((theme) &body body)
  `(let ((*current-theme* ,theme))
     ,@body))

(defun theme-fg-color (&optional (theme *current-theme*))
  (getf theme :fg))

(defun theme-bg-color (&optional (theme *current-theme*))
  (getf theme :bg))

(defun theme-active-color (&optional (theme *current-theme*))
  (getf theme :active))

(defun theme-caption-color (&optional (theme *current-theme*))
  (getf theme :caption))

(defun theme-value-color (&optional (theme *current-theme*))
  (getf theme :value))

(defun color-values (color)
  "return the encoded alpha, red, green, blue values of a color"
  (let ((alpha (ash (logand color #xFF000000) -24))
        (red (ash (logand color #x00FF0000) -16))
        (green (ash (logand color #x0000FF00) -8))
        (blue (logand color #x000000FF)))
    (values alpha red green blue)))

(defun set-color-from-palette (renderer color-key &optional (*colors* *colors*))
  (declare (type keyword color-key))
  (set-color-from-code renderer (getf *colors* color-key)))

(defun set-color-from-code (renderer color-code)
  "Set the color from a hexadecimal code, like #xFF0000FF.
  The order is a r g b."
  (multiple-value-bind (a r g b)
      (color-values color-code)
    (sdl2:set-render-draw-color renderer r g b a)))

(defun set-color-from-theme (renderer color-key &optional (*current-theme* *current-theme*))
  (declare (type keyword color-key))
  (set-color-from-code renderer (getf *current-theme* color-key)))
