(use sxml-serializer)
(use srfi-9)
(use srfi-13)
(use miscmacros)

(define a4-width 210)
(define a4-height 297)

(define page-width (- a4-width 10))
(define page-height (- a4-height 10))

(define boxes-x 1)
(define boxes-y 5)

(define box-width (/ page-width boxes-x))
(define box-height (/ page-height boxes-y))

(define margin 3)

(define qr-margin 1)
(define qr-height (- box-height margin margin qr-margin qr-margin))
(define qr-width qr-height)

(define things-per-page (* boxes-x boxes-y))

(define *text-cell-height* 20)
(define *text-cell-width* 10)

(define (wrap-svg width height content)
  `(*TOP*
    (@@
     (*NAMESPACES*
      (xlink "http://www.w3.org/1999/xlink"))
     (*PI* xml "version=\"1.0\""))

    (svg (@ (xmlns "http://www.w3.org/2000/svg")
            (version "1.2")
            (baseProfile "tiny")
            (viewBox ,(sprintf "0 0 ~Smm ~Smm" width height)))
         ,@content)))

;; coords is a list of things to turn into strings, and append with spaces between
(define (coords-to-path coords)
  (string-join (map ->string coords) " "))

(define (make-line . coords)
  `(path (@ (d ,(coords-to-path coords))
            (stroke-width "1")
            (stroke "black")
            (fill "none"))))

(define (dim x)
  (string-append (->string x) "mm"))

(define (make-text x y* text size style)
  (let ((all-lines (string-split text "\n" #t)))
    (let loop ((lines all-lines)
               (y y*)
               (result '()))
      (if (null? lines)
          (cons 'g result) ; Wrap result in an SVG group for neatness
          (loop
           (cdr lines)
           (+ y size)
           (cons
            `(text (@ (x ,(dim x))
                      (y ,(dim y))
                      (font-size ,(dim size))
                      ,@style) ,(car lines))
            result))))))

(define (make-rect x y w h style)
  `(rect (@ (x ,(dim x))
            (y ,(dim y))
            (fill "none")
            (stroke-width "1")
            ,@style
            (width ,(dim w))
            (height ,(dim h)))))

(define (make-rounded-blob x y w h style)
  `(rect (@ (x ,(dim x))
            (y ,(dim y))
            (rx ,(dim (/ h 2)))
            (ry ,(dim (/ h 2)))
            (fill "none")
            (stroke-width "1")
            ,@style
            (width ,(dim w))
            (height ,(dim h)))))

(define (make-image url x y w h)
  `(image (@ (x ,(dim x))
             (y ,(dim y))
             (width ,(dim w))
             (height ,(dim h))
             (xlink:href ,url))))

(define (make-group . paths)
  (list (cons 'g (append (filter list? paths)))))

(define-record-type thing
  (make-thing* tag name owner maintainer induction? location warnings)
  thing?
  (tag thing-tag)
  (name thing-name)
  (owner thing-owner)
  (maintainer thing-maintainer)
  (induction? thing-induction?)
  (location thing-location)
  (warnings thing-warnings))

(define (make-thing tag name owner maintainer induction? location)
  (make-thing* tag name owner maintainer induction? location #f))

(define (thing-url thing)
  (string-append "http://bristol.hackspace.org.uk/wiki/doku.php/" (symbol->string (thing-tag thing))))

(define (thing-qr-path thing)
  (string-append "qr-" (symbol->string (thing-tag thing)) ".png"))

(define (generate-box thing xofs yofs)
  (let* ((x1 (+ xofs margin))
         (y1 (+ yofs margin))
         (x2 (+ xofs box-width (- margin) (- margin)))
         (y2 (+ yofs box-height (- margin) (- margin)))
         (w (- box-width margin margin))
         (h (- box-height margin margin))
         (logo-size (- h 15 margin)))
   (make-group
    (make-rect x1 y1 w h '((stroke "black")))
    (make-rounded-blob (+ x1 margin) (+ y1 margin) 8 8 '((stroke "grey") (stroke-dasharray "5,5")))
    (make-image "hslogo.png" (+ x1 margin) (+ y1 margin 10) logo-size logo-size)
    (make-text (+ x1 margin 10) (+ y1 margin 8) (thing-name thing)
               8 '((font-family "sans")))
    (let* ((warnings (list
                      (if (thing-owner thing)
                          (string-append "OWNER: " (thing-owner thing))
                          #f)
                      (if (thing-maintainer thing)
                          (string-append "MAINTAINER: " (thing-maintainer thing))
                          #f)
                      (if (thing-induction? thing)
                          (string-append "INDUCTED USERS ONLY!")
                          #f)
                      (if (thing-warnings thing)
                          (thing-warnings thing)
                          #f)))
           (warnings-str
            (string-join
             (filter string? warnings)
             "\n\n")))
      (if (> (string-length warnings-str) 0)
          (make-text (+ x1 margin margin logo-size) (+ y1 margin 16)
                     warnings-str
                     4 '((font-weight "bold") (font-family "sans")))))
    (make-text (+ x1 margin) y2 (thing-url thing)
               2 '((font-weight "bold") (font-family "monospace")))
    (system (string-append "qrencode -m 1 -o " (thing-qr-path thing) " \"" (thing-url thing) "\""))
    (make-image (thing-qr-path thing) (- x2 qr-width qr-margin) (+ y1 qr-margin) qr-width qr-height)
;;    (make-rect (- x2 qr-width qr-margin) (+ y1 qr-margin) qr-width qr-height '((stroke "black")))
     )))

(define (generate-filler xofs yofs)
  (make-group '()))

(define (paginate all-things page)
  (let* ((l (drop all-things (* page things-per-page)))
         (v (make-vector things-per-page #f)))
    (let loop ((remaining l)
               (index 0))
      (if (or (null? remaining) (>= index things-per-page))
          v
          (begin
            (vector-set! v index (car remaining))
            (loop (cdr remaining) (+ index 1)))))))

(define (make-page things)
  (map (lambda (x)
         (make-group
          (map
           (lambda (y)
             (let ((thing (vector-ref
                           things
                           (+ x (* boxes-x y)))))
               (if thing
                   (generate-box
                    thing
                    (* x box-width)
                    (* y box-height))
                   (generate-filler
                    (* x box-width)
                    (* y box-height)))))
           (iota boxes-y))))
       (iota boxes-x)))

;; Input reader

(include "list-of-things.scm")

;; Output driver

(define pages (quotient (length all-things) things-per-page))

(dotimes (page pages)
         (let ((content (make-group
                          (make-page (paginate all-things page)))))

           (serialize-sxml (wrap-svg page-width page-height content)
                           output: (string-append "page-" (number->string page) ".svg"))
           (system (string-append
                    "inkscape --export-pdf=page-"
                    (number->string page)
                    ".pdf page-"
                    (number->string page)
                    ".svg --export-text-to-path"))))

(with-output-to-file "index.wiki"
  (lambda ()
    (printf "^ Thing ^ Needs Induction? ^ Owner/Maintainer ^ Location ^\n")
    (for-each
     (lambda (thing)
       (printf "|[[~A|~A]]|~A|~A|~A|\n"
               (thing-tag thing)
               (thing-name thing)
               (if (thing-induction? thing)
                   "YES"
                   "no")
               (if (thing-owner thing)
                   (thing-owner thing)
                   (if (thing-maintainer thing)
                       (string-append "Hackspace (maintained by "
                                      (thing-maintainer thing)
                                      ")")
                       "Hackspace"))
               (thing-location thing)))
     all-things)))
