(define main
  (lambda ()
    (%funcall vtable_init)
    (define dog (%funcall dog_new (%funcall malloc 24)
			  (%symbol-addr str_jack) 0))
    (define cat (%funcall cat_new (%funcall malloc 24)
			  (%symbol-addr str_rose) 1))
    (%funcall/r (%get-quad (%get-quad dog 0) 0)
	        dog)
    (%funcall/r (%get-quad (%get-quad cat 0) 0)
	        cat)
    (%funcall free dog)
    (%funcall free cat)
    (%funcall vtable_destroy)
    0))

(define dog_vtable 0)
(define cat_vtable 0)

(define str_jack "Jack")
(define str_rose "Rose")

(define animal_new
  (lambda (this name id)
    (%set-quad! this 8 name)
    (%set-quad! this 16 id)
    this))

(define dog_new
  (lambda (this name id)
    (%funcall animal_new this name id)
    (%set-quad! this 0 (%symbol-val dog_vtable))
    this))

(define str_dog_shout_fmt "<Dog#%ld>@%p: Hai, I am %s\n")
(define dog_shout
  (lambda (this)
    (%funcall printf (%symbol-addr str_dog_shout_fmt)
	      (%get-quad this 16) this (%get-quad this 8))))

(define cat_new
  (lambda (this name id)
    (%funcall animal_new this name id)
    (%set-quad! this 0 (%symbol-val cat_vtable))
    this))

(define str_cat_shout_fmt "<Cat#%ld>@%p: Hai, I am %s\n")
(define cat_shout
  (lambda (this)
    (%funcall printf (%symbol-addr str_cat_shout_fmt)
	      (%get-quad this 16) this (%get-quad this 8))))

(define vtable_init
  (lambda ()
    (define dog-vtable (%funcall malloc 8))
    (define cat-vtable (%funcall malloc 8))
    (%set-quad! dog-vtable 0 (%symbol-addr dog_shout))
    (%set-quad! cat-vtable 0 (%symbol-addr cat_shout))
    (%set-quad! (%symbol-addr dog_vtable) 0 dog-vtable)
    (%set-quad! (%symbol-addr cat_vtable) 0 cat-vtable)
    0))

(define vtable_destroy
  (lambda ()
    (%funcall free (%symbol-val dog_vtable))
    (%funcall free (%symbol-val cat_vtable))))

