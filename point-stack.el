;; -- POINT STACK -----------------------------------------------------------------------
;; matt harrison (matthewharrison@gmail.com)
;; dmitry gutov  (dgutov@yandex.ru)
;;
;; Provides forward/back stack for point.  I use load it like so:
;;
;; (add-to-list 'load-path "/home/matt/work/emacs/point-stack")
;; (require 'point-stack)
;; (global-set-key '[(f5)] 'point-stack-push)
;; (global-set-key '[(f6)] 'point-stack-pop)
;; (global-set-key '[(f7)] 'point-stack-forward-stack-pop)
;;
;; Then when I know I'm going to want to come back to where I am I hit
;; f5.  This stores the location of of the point.  When I want to come
;; back to that point hit f6.  I can go forward by hitting f7.
;;
;; based on http://www.emacswiki.org/emacs/JohnConnors
;; enhanced with forward stack and made both local to the current window

(defun point-stack-push ()
  "Push current buffer, point, and scroll position onto stack."
  (interactive)
  (point-stack-store 'stack)
  (point-stack-value 'forward 'set nil) ; new step resets forward history
  (message "Location marked."))

(defun point-stack-pop ()
  "Push current location onto forward stack, move to previous location."
  (interactive)
  (if (point-stack-value 'stack 'null)
      (message "Stack is empty.")
    (point-stack-store 'forward)
    (point-stack-go 'stack)
    (point-stack-value 'stack 'shift)))

(defun point-stack-forward-stack-pop ()
  "Push current location onto stack, pop and move to location from forward stack."
  (interactive)
  (if (point-stack-value 'forward 'null)
      (message "forward Stack is empty.")
    (point-stack-store 'stack)
    (point-stack-go 'forward)
    (point-stack-value 'forward 'shift)))

(defun point-stack-store (stack)
  (let ((loc (point-stack-value stack 'car)))
    ;; don't push the same location twice
    (unless (and (eq (current-buffer) (car loc))
                 (eq (point) (cadr loc)))
      (point-stack-value stack 'push
                         (list (current-buffer) (point) (window-start))))))

(defun point-stack-go (stack)
  (let ((loc (point-stack-value stack 'car)))
    (switch-to-buffer (car loc))
    (set-window-start nil (caddr loc))
    (goto-char (cadr loc))))

(defun point-stack-value (name action &optional arg)
  (let* ((parameter (intern (concat "point-stack-" (symbol-name name))))
         (value (window-parameter nil parameter)))
    (cond ((eq action 'car)
           (car value))
          ((eq action 'null)
           (null value))
          (t (set-window-parameter nil parameter
                                   (case action
                                     ('set arg)
                                     ('push (cons arg value))
                                     ('shift (cdr value))))))))

(provide 'point-stack)
