
(in-package editor)

(defcommand "Insert Tilde" (p)
     (declare (ignore p))
     (let ((point (buffer-point (current-buffer))))
       (insert-character point #\~)))

(bind-key "Insert Tilde" #\meta-\z)

