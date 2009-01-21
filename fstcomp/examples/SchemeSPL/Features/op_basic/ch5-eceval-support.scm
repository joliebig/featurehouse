(define primitive-procedures
  (append original
    (list (list 'car car) ;Basic
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?)
    )
  )
)