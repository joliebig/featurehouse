(define primitive-procedures
  (append original
    (list (list 'inc (lambda (x) (+ x 1) )) ;only increment in this feature
    )
  )
)