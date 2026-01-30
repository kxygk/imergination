(ns kxygk.imergination.matrix4ojalgo
  "Implements the Matrix Protocol using the ojAlgo library
  This was generated from the Neanderthal backend with an LLM.
  Results were then confirmed to be the same"
  (:require [kxygk.imergination.matrix :as matrix])
  (:import [org.ojalgo.matrix.store Primitive64Store MatrixStore]
           [org.ojalgo.function.constant PrimitiveMath]
           [org.ojalgo.function BinaryFunction]))

(defn- ensure-primitive [store]
  (if (instance? org.ojalgo.matrix.store.Primitive64Store store)
    store
    ;; This converts RawStore, DiagonalStore, etc., into a standard 
    ;; Primitive64Store that HAS the methods we need.
    (.copy org.ojalgo.matrix.store.Primitive64Store/FACTORY store)))

(defrecord Matrix [oj-matrix]
  matrix/Primitives
  (data [_]
    (vec (.toRawCopy1D oj-matrix)))
  (mrows [_]
    (int (.countRows oj-matrix)))
  (ncols [_]
    (int (.countColumns oj-matrix)))
  #_
  (cols [_]
    (let [c-count (.countColumns oj-matrix)]
      (mapv (fn [c] 
              (vec (for [r (range (.countRows oj-matrix))]
                     (.doubleValue (.get oj-matrix (long r) (long c))))))
            (range c-count))))
  (col [_ index]
    (let [target   (ensure-primitive oj-matrix)
          ;; column() returns a ColumnView which implements Access1D
          col-view (.column target (long index))]
      (vec (.toRawCopy1D col-view))))
  (row [_ index]
    (let [target   (ensure-primitive oj-matrix)
          ;; column() returns a ColumnView which implements Access1D
          row-view (.row target (long index))]
      (vec (.toRawCopy1D row-view))))
  (diag [_]
    (let [dim    (min (int (.countRows oj-matrix)) (int (.countColumns oj-matrix)))
          target (ensure-primitive oj-matrix)]
      (loop [i 0 res []]
        (if (< i dim)
          (recur (inc i) (conj res (.doubleValue (.get target (long i) (long i)))))
          res))))
  (set-value! [_ x y value]
    (let [pm (ensure-primitive oj-matrix)]
      (.set pm (long x) (long y) (double value))
      (->Matrix pm)))
  (scale-to-value [_ value]
    (let [a (ensure-primitive oj-matrix)]
      ;; ojAlgo's .multiply(double) is the standard scaling method
      (->Matrix (.multiply a (double value)))))
  (mult-elementwise [_ other-matrix]
    (let [a (ensure-primitive oj-matrix)
          b (ensure-primitive (:oj-matrix other-matrix))
          ;; Create a result store of the same shape
          res (.copy a)] 
      ;; We call the method directly on the 'res' object.
      ;; This is a concrete method on Primitive64Store.
      (.fillMatching res a org.ojalgo.function.constant.PrimitiveMath/MULTIPLY b)
      (->Matrix res)))
  (mm [_ other]
    (let [a (ensure-primitive oj-matrix)
          b (ensure-primitive (:oj-matrix other))]
      (->Matrix (.multiply a b))))
  (svd [_]
    (let [factory    org.ojalgo.matrix.decomposition.SingularValue/PRIMITIVE
          svd-engine (.make factory)]
      (if (.decompose svd-engine (ensure-primitive oj-matrix))
        {:u     (->Matrix (ensure-primitive (.getU svd-engine)))
         :vt    (->Matrix (.transpose (ensure-primitive (.getV svd-engine)))) 
         :sigma (->Matrix (ensure-primitive (.getD svd-engine)))}
        (throw (Exception. "SVD failed to converge"))))))

(defrecord Factory []
  matrix/Factory
  (build-matrix [_ num-rows num-cols data-vec]
    (let [r (long num-rows)
          c (long num-cols)
          factory org.ojalgo.matrix.store.Primitive64Store/FACTORY
          store (.make factory r c)
          arr (double-array (flatten data-vec))
          len (count arr)]
      ;; Manual loop over the primitive array is the most 
      ;; compatible way to ensure types match perfectly.
      (loop [i 0]
        (when (< i len)
          (.set store (long i) (aget arr i))
          (recur (unchecked-inc i))))
      (->Matrix store)))
  (build-inverter [_ size]
    (let [s (long size)
          factory org.ojalgo.matrix.store.Primitive64Store/FACTORY
          store (.make factory s s)]
      (.set store 0 0 -1.0)
      (dotimes [i (dec s)]
        (let [idx (inc i)]
          (.set store (long idx) (long idx) 1.0)))
      (->Matrix store))))

;; Register the ojAlgo backend
(matrix/set-factory! (->Factory))
