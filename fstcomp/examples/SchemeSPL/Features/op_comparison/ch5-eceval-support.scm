(define primitive-procedures
  (append original
    (list	(list '= =) ; Comparison
          (list '> >)
          (list '< <)
    )
  )
)