(ns ribelo.stade
  #?(:clj  (:refer-clojure :exclude [min max reverse])
     :cljs (:refer-clojure :exclude [min max reverse clone]))
  (:require
   [ribelo.halle :as h]
   [ribelo.kemnath :as math]
   #?(:cljs [goog.array :as garray])
   #?(:cljs [goog.math :as gmath])))

#?(:clj (set! *warn-on-reflection* true))
#?(:clj (set! *unchecked-math* :warn-on-boxed))

(comment
  (do (def data (vec (repeatedly 1e5 #(* 100 (- 0.5 ^double (rand))))))
      (def arr  (h/->double-array data))
      (require '[taoensso.encore :as enc])))

(defn clone
  ^doubles [xs]
  (h/->double-array-or-copy xs))

(defn sum
  "sum elements in collection"
  ^double [xs]
  (let [axs (h/->double-array xs)]
    (loop [i 0 r 0.0]
      (if (< i (alength axs))
        (recur (inc i) (+ r (aget axs i)))
        r))))

(defn add
  ^doubles [xs x]
  (cond
    (or (h/double-array? x) (vector? x))
    (let [a1 (h/->double-array xs)
          a2 (h/->double-array x)]
      (when-not (== (alength a1) (alength a2))
        (throw (ex-info "arrays have different length" {:xs (alength a1) :ys (alength a2)})))
      (loop [i 0 acc (h/double-array (alength a1))]
        (if (< i (alength a1))
          (recur (unchecked-inc-int i) (doto acc (aset i (+ (aget a1 i) (aget a2 i)))))
          acc)))
    ;;
    (number? x)
    (let [a1 (h/->double-array xs)]
      (loop [i 0 acc (h/double-array (alength a1))]
        (if (< i (alength a1))
          (recur (unchecked-inc-int i) (doto acc (aset i (+ (aget a1 i) ^double x))))
          acc)))))

(defn sub
  ^doubles [xs x]
  (cond
    (or (h/double-array? x) (vector? x))
    (let [a1 (h/->double-array xs)
          a2 (h/->double-array x)]
      (when-not (== (alength a1) (alength a2))
        (throw (ex-info "arrays have different length" {:xs (alength a1) :ys (alength a2)})))
      (loop [i 0 acc (h/double-array (alength a1))]
        (if (< i (alength a1))
          (recur (unchecked-inc-int i) (doto acc (aset i (- (aget a1 i) (aget a2 i)))))
          acc)))
    ;;
    (number? x)
    (let [a1 (h/->double-array xs)]
      (loop [i 0 acc (h/double-array (alength a1))]
        (if (< i (alength a1))
          (recur (unchecked-inc-int i) (doto acc (aset i (- (aget a1 i) ^double x))))
          acc)))))

(defn scale
  ^doubles [xs x]
  (cond
    (or (h/double-array? x) (vector? x))
    (let [a1 (h/->double-array xs)
          a2 (h/->double-array x)]
      (when-not (== (alength a1) (alength a2))
        (throw (ex-info "arrays have different length" {:axs (alength a1) :ays (alength a2)})))
      (loop [i 0 acc (h/double-array (alength a1))]
        (if (< i (alength a1))
          (recur (unchecked-inc-int i) (doto acc (aset i (* (aget a1 i) (aget a2 i)))))
          acc)))
    ;;
    (number? x)
    (let [a1 (h/->double-array xs)]
      (loop [i 0 acc #?(:clj (double-array (alength a1)) :cljs #js [])]
        (if (< i (alength a1))
          (recur (unchecked-inc-int i) (doto acc (aset i (* (aget a1 i) ^double x))))
          acc)))))

(defn div
  ^doubles [xs ^double x]
  (cond
    (or (h/double-array? x) (vector? x))
    (let [a1 (h/->double-array xs)
          a2 (h/->double-array x)]
      (when-not (== (alength a1) (alength a2))
        (throw (ex-info "arrays have different length" {:axs (alength a1) :ays (alength a2)})))
      (loop [i 0 acc (h/double-array (alength a1))]
        (if (< i (alength a1))
          (recur (unchecked-inc-int i) (doto acc (aset i (/ (aget a1 i) (aget a2 i)))))
          acc)))
    ;;
    (number? x)
    (let [^doubles a1 (h/->double-array xs)]
      (loop [i 0 acc (double-array (alength a1))]
        (if (< i (alength a1))
          (recur (unchecked-inc-int i) (doto acc (aset i (/ (aget a1 i) x))))
          acc)))))

(defn axpy
  "return array by adding a multiple of another array"
  ^doubles [^double a xs ys]
  (let [a1 (h/->double-array xs)
        a2 (h/->double-array ys)]
    (when-not (== (alength a1) (alength a2))
      (throw (ex-info "arrays have different length" {:axs (alength a1) :ays (alength a2)})))
    (loop [i 0]
      (when (< i (alength a1))
        (aset a2 i (+ (aget a2 i) (* a (aget a1 i))))
        (recur (unchecked-inc-int i))))
    a2))

(defn dot
  "returns the dot product between two vectors"
  ^double [xs ys]
  (let [a1 (h/->double-array xs)
        a2 (h/->double-array ys)]
    (when-not (== (alength a1) (alength a2))
      (throw (ex-info "arrays have different length" {:axs (alength a1) :ays (alength a2)})))
    (loop [i 0 r 0.0]
      (if (< i (alength a1))
        (recur (unchecked-inc-int i) (+ r (* (aget a1 i) (aget a2 i))))
        r))))

(defn sq
  ^doubles [xs]
  (scale xs xs))

(defn equals?
  "check if x element-wisely equals y with default epsilon 1E-10"
  [xs ys]
  (let [a1 (h/->double-array xs)
        a2 (h/->double-array ys)]
    (if (== (alength a1) (alength a2))
      (loop [i 0]
        (if (< i (alength a1))
          (if (math/equal? (aget a1 i) (aget a2 i))
            (recur (unchecked-inc-int i))
            false)
          true))
      false)))

(defn min
  "smallest element in collection"
  ^double [xs]
  (let [a1 (h/->double-array xs)]
    (loop [i 0 acc math/MAX-DOUBLE]
      (if (< i (alength a1))
        (recur (unchecked-inc-int i) (math/min acc (aget a1 i)))
        acc))))

(defn max
  "largest element in collection"
  ^double [xs]
  (let [a1 (h/->double-array xs)]
    (loop [i 0 acc math/MIN-DOUBLE]
      (if (< i (alength a1))
        (recur (unchecked-inc-int i) (math/max acc (aget a1 i)))
        acc))))

(defn mean
  "average value of collection"
  ^double [xs]
  (let [a1 (h/->double-array xs)]
    (/ (sum a1) (alength a1))))

(defn variance
  "variance of sample"
  (^double [     xs] (variance :sample xs))
  (^double [mode xs]
   (let [a1 (h/->double-array xs)
         mu  (mean a1)]
     (loop [i 0 acc (h/double-array (alength a1))]
       (if (< i (alength a1))
         (recur (unchecked-inc-int i) (doto acc (aset i (-> (- (aget a1 i) mu) math/abs math/sq))))
         (/ (sum acc) (- (alength a1) (case mode :sample 1 :population 0))))))))

(defn std
  "standard deviation of sample"
  (^double [     xs] (std :sample xs))
  (^double [mode xs]
   (math/sqrt (variance mode xs))))

(defn linear-interpolate
  ^double [xs ys ^double x]
  (let [a1  (h/->double-array xs)
        a2  (h/->double-array ys)
        pos (h/binary-search a1 x)
        pos (if (neg? pos) (- (math/abs pos) 2) pos)
        pos (math/clamp pos 0.0 (- (alength a1) 2))
        p   (/ (- x (aget a1 pos)) (- (aget a1 (inc pos)) (aget a1 pos)))]
    (math/lerp (aget a2 pos) (aget a2 (inc pos)) p)))

(defn percentile
  ^double [^double p xs]
  {:pre [(and (>= p 0.0) (<= p 100.0))]}
  (let [a1 (doto (h/->double-array xs) h/sort)
        a2 (h/range 0.5 (inc (- (alength a1) 0.5)))
        pq (-> (scale a2 100.0) (div (alength a1)))
        pq (h/concat 0.0 pq 100.0)
        a1 (h/concat (aget a1 0) a1 (aget a1 (dec (alength a1))))]
    (linear-interpolate pq a1 p)))

(defn quantile
  ^double [^double p xs]
  (percentile (* 100.0 p) xs))

(defn median
  "median value of collection"
  ^double [xs]
  (let [a1  (doto (h/->double-array xs) h/sort)
        n   (dec (alength a1))
        idx (math/max 1 (math/floor (/ n 2)))]
    (if (zero? ^long (mod n 2))
      (aget a1 idx)
      (/ (+ (aget a1 idx) (aget a1 (inc idx))) 2))))

(defn mode
  "most frequent value in an array of elements"
  ^double [xs]
  (let [a1 (doto (h/->double-array xs) (h/sort))
        n  (alength a1)]
    (loop [i 1 cv (aget a1 0) c 0 cc 1 m -1.0]
      (if (< i n)
        (let [v (aget a1 i)]
          (if (not= v cv)
            (if (> cc c)
              (recur (unchecked-inc-int i) cv cc cc cv)
              (recur (unchecked-inc-int i) v c 1 m))
            (recur (unchecked-inc-int i) v c (inc cc) m)))
        (if (> cc c) cv m)))))

(defn q1
  ^double [xs]
  (percentile 25.0 xs))

(defn q2
  ^double [xs]
  (median xs))

(defn q3
  ^double [xs]
  (percentile 75.0 xs))

(defn quartile
  "quartilies of a sample"
  ^doubles [xs]
  (h/->double-array [(q1 xs) (q2 xs) (q3 xs)]))

(defn iqr
  ^double [xs]
  (- (q3 xs) (q1 xs)))

(defn moment
  ^double [^double k xs]
  (let [mu (mean xs)]
    (mean (h/map (fn [x] (math/pow (- ^double x mu) k)) xs))))

(defn covariance
  "covariance of sample"
  (^double [      xs ys] (covariance :sample xs ys))
  (^double [mode  xs ys]
   (let [a1 (h/->double-array xs)
         a2 (h/->double-array ys)]
     (when-not (== (alength a1) (alength a2))
       (throw (ex-info "arrays have different length" {:x (alength a1) :y (alength a2)})))
     (when (< (alength a1) 3)
       (throw (ex-info "array length has to be at least 3" {:n (alength a1)})))
     (let [ma1 (mean a1)
           ma2 (mean a2)]
       (loop [i 0 sxy 0.0]
         (if (< i (alength a1))
           (let [dx (- (aget a1 i) ma1)
                 dy (- (aget a2 i) ma2)]
             (recur (unchecked-inc-int i) (+ sxy (* dx dy))))
           (/ sxy (- (alength a1) (case mode :sample 1 :population 0)))))))))

(defn kurtosis
  ^double [xs]
  (let [arr (h/->double-array xs)]
    (/ (moment 4.0 arr) (math/sq (moment 2.0 arr)))))

(defn xkurtosis
  "excess kurtosis"
  ^double [xs]
  (- (kurtosis xs) 3.0))

(defn skewness
  ^double [xs]
  (let [arr (h/->double-array xs)]
    (/ (moment 3.0 arr) (math/pow (moment 2.0 arr) 1.5))))

(defn correlation
  "pearsons correlation"
  (^double [     xs ys] (correlation :sample xs ys))
  (^double [mode xs ys]
   (let [a1 (h/->double-array xs)
         a2 (h/->double-array ys)

         _ (when-not (== (alength a1) (alength a2))
             (throw (ex-info "arrays have different length" {:x (alength a1) :y (alength a2)})))
         _ (when (< (alength a1) 3)
             (throw (ex-info "array length has to be at least 3" {:n (alength a1)})))

         sxy (covariance mode a1 a2)
         sxx (variance mode a1)
         syy (variance mode a2)]
     (if-not (or (zero? sxx) (zero? syy))
       (/ sxy (math/sqrt (* sxx syy)))
       math/NaN))))

(defn mse
  ^double [truth predict]
  (let [atruth (h/->double-array predict)
        apred  (h/->double-array truth)
        ntruth (alength atruth)
        npred  (alength apred)]

    (when-not (== (alength atruth) (alength apred))
      (throw (ex-info "arrays have different length" {:truth ntruth :pred npred})))

    (loop [i 0 acc 0.0]
      (if (< i ntruth)
        (recur (unchecked-inc-int i) (+ acc (math/sq (- (aget atruth i) (aget apred i)))))
        (/ acc ntruth)))))

(defn rmse
  ^double [truth predict]
  (math/sqrt (mse truth predict)))

(defn mad
  "calculates the mean absolute deviation error"
  ^double [truth predict]
  (let [atruth (h/->double-array predict)
        apred  (h/->double-array truth)
        ntruth (alength atruth)
        npred  (alength apred)]

    (when-not (== (alength atruth) (alength apred))
      (throw (ex-info "arrays have different length" {:truth ntruth :pred npred})))

    (loop [i 0 acc 0.0]
      (if (< i ntruth)
        (recur (unchecked-inc-int i) (+ acc (math/abs (- (aget atruth i) (aget apred i)))))
        (/ acc ntruth)))))

(defn transpose
  "returns the matrix transpose"
  ^doubles [coll2d]
  #?(:clj
     (let [^"[[D" a2d (h/->double-double-array coll2d)
           m          (alength a2d)
           n          (alength ^doubles (aget a2d 0))
           ^"[[D" acc (make-array h/double-type n m)]
       (dotimes [i m]
         (dotimes [j n]
           (let [v (aget ^"[D" (aget a2d i) j)]
             (aset ^"[D" (aget acc j) i v))))
       acc)
     :cljs
     (h/->double-double-array (apply mapv vector coll2d))))

(defn squared-distance
  ^double [xs ys]
  (let [a1 (h/->double-array xs)
        a2 (h/->double-array ys)]

    (when-not (== (alength a1) (alength a2))
      (throw (ex-info "arrays have different length" {:xs (alength a1) :ys (alength a2)})))

    (loop [i 0 sum 0.0]
      (if (< i (alength a1))
        (let [d (- (aget a1 i) (aget a2 i))]
          (recur (unchecked-inc-int i) (+ sum (math/sq d))))
        sum))))

(defn distance
  ^doubles [xs ys]
  (math/sqrt (squared-distance xs ys)))

(defn norm2
  "l2 vector norm."
  ^double [xs]
  (let [a1 (h/->double-array xs)]
    (loop [i 0  acc 0.0]
      (if (< i (alength a1))
        (recur (unchecked-inc-int i) (+ acc (math/sq (aget a1 i))))
        (math/sqrt acc)))))

(def norm norm2)

(defn standarize
  ^doubles [xs]
  (let [a1 (h/->double-array-or-copy xs)
        mu (mean a1)
        o  (std  a1)]
    (if-not (zero? o)
      (loop [i 0 acc (h/double-array (alength a1))]
        (if (< i (alength a1))
          (recur (unchecked-inc-int i)
                 (doto acc (aset i (/ (- (aget a1 i) mu) o))))
          acc))
      a1)))

(defn sigmoid
  "Logistic sigmoid function"
  ^double [^double x]
  (let [x' (math/max -36, (math/min x 36))]
    (/ 1.0 (+ 1.0 (math/exp (- x'))))))

#?(:clj
   (defn unique
     [xs]
     (-> (h/->double-array xs)
         (java.util.Arrays/stream)
         .distinct
         .toArray)))
