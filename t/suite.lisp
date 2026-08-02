(in-package :agento11y-cl/t)

(defmacro with-test-suite ((name) &body body)
  "Run test cases within a named suite. Provides local CHECK function.
Returns (values ok-p pass-count fail-count)."
  (let ((pass (gensym "PASS-"))
        (fail (gensym "FAIL-"))
        (suite-name (gensym "NAME-")))
    `(let ((,pass 0)
           (,fail 0)
           (,suite-name ,name))
       (format t "~%=== ~a ===~%" ,suite-name)
       (flet ((check (label condition)
                (if condition
                    (progn (incf ,pass)
                           (format t "  ✓ ~a~%" label))
                    (progn (incf ,fail)
                           (format t "  ✗ ~a~%" label)))))
         ,@body)
       (format t "--- ~a: ~d passed, ~d failed ---~%" ,suite-name ,pass ,fail)
       (values (zerop ,fail) ,pass ,fail))))
