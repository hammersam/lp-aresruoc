; The leaves are the atoms in the code which are not special forms.
;
; A term(anything in the language) is either:
; - An atom, e.g., #t, #f, 34, "hi", null, 4.0, x, ...
; - A special form, e.g., define, lambda, if
;   * Macros will let us define our own
; - A sequence of terms in parens:
;   * If t1 a special form, semantics of sequence is special
;   * Else a function call
;
; * Example: (+ 3 (car xs))
; * Example: (lambda (x) (...))
;
; A good historian wouldn't refuse to study a country where he/she didn't
; like people's accents.
