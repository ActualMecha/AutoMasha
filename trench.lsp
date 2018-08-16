(vl-load-com)

(setq am:int16 1070)

(defun am:default (value default)
  (if value value default))

(defun am:get-dictionary (doc
			  / dictionary-name dictionaries dictionary)
  (setq dictionary-name "amDictionary"
  	dictionaries (vla-get-Dictionaries doc)
  	dictionary (vl-catch-all-apply 'vla-Item (list dictionaries dictionary-name)))
  (if (= (type dictionary) 'vla-object)
    dictionary
    (vla-Add dictionaries dictionary-name)))

(defun am:get-xrecord (dictionary name
		       / record)
  (setq record (vl-catch-all-apply 'vla-GetObject (list dictionary name)))
  (if (= (type record) 'vla-object)
    record
    (vla-AddXRecord dictionary name)))

(defun am:variant->list (variant-array)
  (vlax-safearray->list (vlax-variant-value variant-array)))

(defun am:pairs (flat-list
		 / pair rest)
  (if flat-list
    (progn
      (setq pair (list (car flat-list) (cadr flat-list))
	    rest (cddr flat-list))
      (append (list pair) (am:pairs rest)))))

(defun am:generate-segments (points
			     / first second rest)
  (setq first (car points)
	second (cadr points)
	rest (cdr points))
  (if second
    (append (list first second) (am:generate-segments rest))))

(defun am:flatten-list (list-of-lists) 
  (if list-of-lists
    (append (car list-of-lists) (am:flatten-list (cdr list-of-lists)))))

;;; Math

(defun am:scalar-op (operation arg1 arg2
		     / left-fn right-fn func list-art)
  (setq left-fn '(lambda (x) (apply operation (list x arg2)))
	right-fn '(lambda (x) (apply operation (list arg1 x))))
  (cond ((listp arg1)
	 (setq func left-fn
	       list-arg arg1))
	((listp arg2)
	 (setq func right-fn
	       list-arg arg2)))
  (mapcar func list-arg))
	
(defun am:sign (number)
  (cond ((< number 0) -1)
	((> number 0) 1)
	(t 0)))

(defun am:vector (point1 point2)
  (mapcar '- point2 point1))

(defun am:line->vector (line)
  (am:vector (car line) (cadr line)))

(defun am:dot (vector1 vector2)
  (apply '+ (mapcar '* vector1 vector2)))

(defun am:length (vector)
  (sqrt (am:dot vector vector)))

(defun am:normalize (vector
		     / len)
  (setq len (am:length vector))
  (mapcar '(lambda (x) (/ x len)) vector))

(defun am:rotate90 (vector)
  (list (- (cadr vector))
	(car vector)))
  
(defun am:side (reference vector)
  (am:sign (am:dot reference (am:rotate90 vector))))

(defun am:same-side (reference vector1 vector2 strict
		     / side1 side2)
  (setq side1 (am:side reference vector1)
	side2 (am:side reference vector2))
  (cond ((= side1 side2) t)
	((or (= side1 0) (= side2 0)) (not strict))
	(t ())))	 

(defun am:get-variable (doc name
			/ dictionary record out-type out-value)
  (setq dictionary (am:get-dictionary doc)
	record (am:get-xrecord dictionary name))
  (vla-GetXRecordData record 'out-type 'out-value)
  (if (/= out-value nil)
    (vlax-variant-value (vlax-safearray-get-element out-value 0))))

(defun am:set-variable (doc name var-type var-value
			/ dictionary record record-type record-value)
  (setq dictionary (am:get-dictionary doc)
	record (am:get-xrecord dictionary name)
	record-type (vlax-make-safearray vlax-vbInteger '(0 . 0))
	record-value (vlax-make-safearray vlax-vbVariant '(0 . 0)))
  (vlax-safearray-put-element record-type 0 var-type)
  (vlax-safearray-put-element record-value 0 var-value)
  (vla-SetXRecordData record record-type record-value))

(defun am:create-block (doc
			/ id-name id block-name block)
  (setq id-name "free-block-id"
	id (am:default (am:get-variable doc id-name) 1)
	block-name (strcat "Trench " (itoa id))
	block (vla-Add (vla-get-Blocks doc)
		       (vlax-3d-point 0 0 0)
		       block-name))
  (am:set-variable doc id-name am:int16 (1+ id))
  block)

(defun am:flatten (3dpt)
  (list (car 3dpt) (cadr 3dpt)))

(defun am:append-tail (polyline last-position len block-ref
		       / next-position point)
  (setq next-position (am:flatten (getpoint last-position "Next segment"))
	point (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (vlax-safearray-fill point next-position)
  (vla-AddVertex polyline len point)
  (vla-Update block-ref)
  (if (< len 1)
    (am:append-tail polyline next-position (1+ len) block-ref)))

(defun am:construct-main-line (doc block block-ref
			       / start first points polyline)
  (setq start (am:flatten (getpoint "Trench start"))
	first (am:flatten (getpoint start "First segment"))
	points (vlax-make-safearray vlax-vbDouble '(0 . 3)))
  (vlax-safearray-fill points (append start first))
  (setq polyline (vla-AddLightweightPolyline block points))
  (vla-Update block-ref)
  (am:append-tail polyline first 2 block-ref)
  polyline)

(defun am:add-width (main-line start block-ref
		     / offset offset+ offset-)
  (setq offset (getdist start "Half-width")
	offset+ (vlax-variant-value (vla-Offset main-line offset))
	offset- (vlax-variant-value (vla-Offset main-line (- offset))))
  (vla-Update block-ref)
  (list (vlax-safearray-get-element offset+ 0)
	(vlax-safearray-get-element offset- 0)))

(defun am:extrude-point (point block block-ref
			 / upper-point)
  (setq upper-point (getpoint point "Height")
	new-line (vla-AddLine block
		   	      (vlax-3d-point (append point '(0)))
		              (vlax-3d-point upper-point)))
  (vla-Update block-ref)
  upper-point)

(defun am:connect-extruded (points block block-ref
			    / array flat-list)
  (setq array (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 2 (length points)))))
	flat-list (am:flatten-list (mapcar 'am:flatten points)))
  (vlax-safearray-fill array flat-list)
  (vla-AddLightweightPolyline block array)
  (vla-Update block-ref))

(defun am:plan-extrusion (prev-point point next-point direction
			  / next-segment-v prev-segment-v curve-direction)
  (setq next-segment-v (am:vector point next-point)
	prev-segment-v (am:vector prev-point point))
  (cond ((= prev-point ()) (list (list point (am:normalize (am:scalar-op '* direction (am:rotate90 next-segment-v))))))
	((= next-point ()) (list (list point (am:normalize (am:scalar-op '* direction (am:rotate90 prev-segment-v))))))
	(t (progn
	     (setq curve-direction (am:side next-segment-v prev-segment-v)
		   same-direction (or (= curve-direction direction) (= curve-direction 0)))
	     (if same-direction
	       (list (list point (am:normalize (mapcar '+
						       (am:normalize (mapcar '- prev-segment-v))
						       (am:normalize next-segment-v)))))
	       (list (list point (am:normalize (am:scalar-op '* direction (am:rotate90 prev-segment-v))))
		       (list point (am:normalize (am:scalar-op '* direction (am:rotate90 next-segment-v))))))))))

(defun am:plan-extrusions (edge direction
			   / segment segment-v next-segment next-segment-v
			   extrusion-v curve-inside point)
  (am:flatten-list
    (mapcar '(lambda (a b c) (am:plan-extrusion a b c direction))
	    (append (list nil) edge)
	    edge
	    (append (cdr edge) (list nil)))))

(defun am:do-extrusion (extrusion block block-ref
			/ point direction dist upper-point)
  (setq point (car extrusion)
	direction (cadr extrusion)
	dist (getdist point "Height")
	upper-point (mapcar '+ point (mapcar '(lambda (x) (* dist x)) direction)))
  (vla-AddLine block
    (vlax-3d-point (append point '(0)))
    (vlax-3d-point upper-point))
  (vla-Update block-ref)
  upper-point)

(defun am:extrude-edge (edge direction block block-ref
			    / edge-points extrusions extruded-points extrusion)
  (setq edge-points (am:pairs (am:variant->list (vla-get-Coordinates edge)))
	extrusions (am:plan-extrusions edge-points direction)
	extruded-points (mapcar '(lambda (extrusion)
				   (am:do-extrusion extrusion block block-ref))
				extrusions))
  (am:connect-extruded extruded-points block block-ref)
  extruded-points)

(defun am:trench (/ acad-object doc start end model-space
		  block block-ref start main-line	
		  bottom-edges right-slope-bottom left-slope-bottom
		  right-slope-top left-slope-top)
  (setq acad-object (vlax-get-acad-object)
	doc (vla-get-ActiveDocument acad-object)
	model-space (vla-get-ModelSpace doc)
	block (am:create-block doc)
	block-ref (vla-InsertBlock model-space
                  (vlax-3d-point 0 0 0)
                  (vla-get-Name block)
                  1 1 1 0)
	main-line (am:construct-main-line doc block block-ref)
	start (am:variant->list (vla-get-Coordinate main-line 0))
	bottom-edges (am:add-width main-line start block-ref)
	right-slope-bottom (car bottom-edges)
	left-slope-bottom (cadr bottom-edges)
	right-slope-top (am:extrude-edge right-slope-bottom -1 block block-ref)
	left-slope-top (am:extrude-edge left-slope-bottom 1 block block-ref)))

;allow closed main lines
;correct slope marking
;insert hypotenuse for each point
;remove the first point in (getdist) and ask for full width
;delete block on object removal
;ghost images
;control points
;the shading on slopes should stretch over the whole edge