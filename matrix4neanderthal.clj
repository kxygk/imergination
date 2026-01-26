(ns matrix4neanderthal
  "Implements the Matrix Protocol using the Neanderthal library"
  (:require matrix
            [uncomplicate.neanderthal.core :as ncore]
            [uncomplicate.neanderthal.native :as neand]
            [uncomplicate.neanderthal.linalg :as linalg]
            [uncomplicate.neanderthal.vect-math :as vect-math]))

(defrecord Matrix [neand-matrix]
  matrix/Primitives
  (data [_]
    (vec (flatten (seq neand-matrix))))
  (mrows [_] (ncore/mrows neand-matrix))
  (ncols [_] (ncore/ncols neand-matrix))
  (cols  [_] (->> neand-matrix
                  ncore/cols
                  (mapv #(-> %
                             seq
                             vec))))
  (row [_
        index]
    (-> neand-matrix
        (ncore/row index)
        seq
        vec))
  (col [_
        index]
    (-> neand-matrix
        (ncore/col index)
        seq
        vec))
  (diag [_]
    (-> neand-matrix
        ncore/dia
        seq
        vec))
  (set-value! [_
              x
              y
              value]
    (->Matrix (ncore/alter! neand-matrix
                  x
                  y
                  (fn ^double  
                    [^double _]
                    ;; the type annotations are required for some reason
                    ;; crashed without them..
                    value))))
  (scale-to-value [_
                   factor]
    (->Matrix (ncore/scal factor
                          neand-matrix)))
  (mult-elementwise [_
                     other-matrix]
    (->Matrix (vect-math/mul (:neand-matrix other-matrix)
                             neand-matrix)))
  (mm [_
       other] 
    ;; 'other' is also a Matrix record,
    ;; And it must be of the same type!
    ;; we reach in to get its :impl
    (->Matrix (ncore/mm neand-matrix
                        (:neand-matrix other))))
  (svd [_]
    (let [svd-map (linalg/svd neand-matrix
                              true
                              true)]
      {:u     (->Matrix (:u svd-map))
       :vt    (->Matrix (:vt svd-map))
       ;; Keep sigma as a clojure vec since it's just a list of weights
       :sigma (->Matrix(:sigma svd-map))})))

(defrecord Factory []
  matrix/Factory
  (build-matrix [_
                 num-rows
                 num-cols
                 data-vec]
    (->Matrix
      (neand/dge num-rows
                 num-cols
                 data-vec)))
  (build-inverter [_
                   size]
    (->Matrix
      (neand/dgd size
                 (into [-1]
                       (repeat (dec size)
                               1.0))))))
;; This causes:
;; a "self-register" as the matrix backend
;; Note that you need to pass an instance of Factory Record
;; and not the class itself :))
(matrix/set-factory! (->Factory))


#_
(matrix/data (matrix/build-matrix (->Factory)
                                  2
                                  2
                                  [1,2,3,4]))
