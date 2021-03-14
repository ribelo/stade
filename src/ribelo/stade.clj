(ns ribelo.stade
  (:refer-clojure :exclude [min max reverse])
  (:require
   [ribelo.halle :as h]
   [ribelo.kemnath :as math])
  (:import
   (smile.math MathEx)
   (smile.sort QuickSelect)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(comment
  (do (def data (vec (repeatedly 100000 #(* 100 (- 0.5 ^double (rand))))))
      (def arr  (double-array data))
      (require '[criterium.core :refer [quick-bench]])))

(defn clone
  ^doubles [xs]
  (let [^doubles arr (h/->double-array xs)]
    (h/->double-array-or-copy arr)))

(defn add
  "element-wise sum of two arrays"
  ^doubles [xs ys]
  (let [^doubles arr1 (h/->double-array-or-copy xs)
        ^doubles arr2 (h/->double-array ys)]
    (MathEx/add arr1 arr2)
    arr1))

(defn axpy
  "return array by adding a multiple of another array"
  ^doubles [^double a xs ys]
  (let [^doubles axs (h/->double-array-or-copy xs)
        ^doubles ayx (h/->double-array ys)]
    (MathEx/axpy a axs ayx)
    axs))

(defn choose
  "the n choose k"
  ^doubles [^long n ^long k]
  (MathEx/choose n k))

(defn dot
  "returns the dot product between two vectors"
  ^double [xs ys]
  (let [^doubles axs (h/->double-array xs)
        ^doubles ayx (h/->double-array ys)]
    (MathEx/dot axs ayx)))

(defn equals
  "check if x element-wisely equals y with default epsilon 1E-10"
  [xs ys]
  (let [^doubles axs (h/->double-array xs)
        ^doubles ayx (h/->double-array ys)]
    (MathEx/equals axs ayx)))

(defn factorial
  "the factorial of n"
  [^long n]
  (MathEx/factorial n))

(defn min
  "smallest element in collection"
  ^double [xs]
  (let [arr (h/->double-array xs)]
    (MathEx/min ^doubles arr)))

(defn max
  "largest element in collection"
  ^double [xs]
  (let [arr (h/->double-array xs)]
    (MathEx/max ^doubles arr)))

(defn mean
  "average value of collection"
  ^double [xs]
  (let [arr (h/->double-array xs)]
    (MathEx/mean ^doubles arr)))

(defn variance
  "variance of sample"
  ^double [xs]
  (let [arr (h/->double-array xs)]
    (MathEx/var ^doubles arr)))

(defn std
  "standard deviation of sample"
  ^double [xs]
  (let [arr (h/->double-array xs)]
    (MathEx/sd ^doubles arr)))

(defn quantile
  ^double [xs ^double p]
  (let [^doubles arr (h/->double-array-or-copy xs)]
    (QuickSelect/select arr (long (* (alength arr) p)))))

(defn percentile
  ^double [xs ^double p]
  (quantile xs (/ p 100.0)))

(defn reverse
  ^doubles [xs]
  (let [^doubles arr (h/->double-array-or-copy xs)]
    (MathEx/reverse arr)
    arr))

(defn q1
  ^double [xs]
  (let [^doubles arr (h/->double-array-or-copy xs)]
    (MathEx/q1 arr)))

(defn q2
  ^double [xs]
  (let [^doubles arr (h/->double-array-or-copy xs)]
    (MathEx/median arr)))

(defn q3
  ^double [xs]
  (let [^doubles arr (h/->double-array-or-copy xs)]
    (MathEx/q3 arr)))

(defn quartile
  "quartilies of a sample"
  ^doubles [xs]
   (let [^doubles arr (h/->double-array xs)]
     (double-array [(MathEx/q1 arr) (MathEx/median arr) (MathEx/q3 arr)])))

(defn iqr
  ^double [xs]
   (let [^doubles arr (h/->double-array xs)]
     (- (MathEx/q3 arr) (MathEx/q1 arr))))

(defn median
  "median value of collection"
  ^double [xs]
  (let [^doubles arr (h/->double-array-or-copy xs)]
    (MathEx/median arr)))

(defn moment
  ^double [^double k xs]
  (let [mu (mean xs)]
    (mean (h/map (fn [x] (math/pow (- ^double x mu) k)) xs))))

(defn covariance
  "covariance of sample"
  ^double [xs ys]
   (let [^doubles axs (h/->double-array xs)
         ^doubles ayx (h/->double-array ys)]
     (MathEx/cov axs ayx)))

(defn kurtosis
  ^double [xs]
   (let [arr  (h/->double-array xs)]
     (/ (moment 4.0 arr) (math/sq (moment 2.0 arr)))))

(defn xkurtosis
  ^double [xs]
  (- (kurtosis xs) 3.0))

(defn skewness
  ^double [xs]
  (let [arr (h/->double-array xs)]
    (/ (moment 3.0 arr) (math/pow (moment 2.0 arr) 1.5))))

(defn correlation
  "Pearsons correlation"
  ^double [xs ys]
   (let [^doubles axs (h/->double-array xs)
         ^doubles ayx (h/->double-array ys)]
     (MathEx/cor axs ayx)))

(defn mse
  ^double [x-pred y-true]
  (let [^doubles pred-arr (h/->double-array x-pred)
        ^doubles true-arr (h/->double-array y-true)]
     (smile.validation.metric.MSE/of ^doubles true-arr ^doubles pred-arr)))

(defn rmse
  ^double [x-pred y-true]
  (let [^doubles pred-arr (h/->double-array x-pred)
        ^doubles true-arr (h/->double-array y-true)]
    (smile.validation.metric.RMSE/of true-arr pred-arr)))

(defn mad
  "mean absolute deviation"
  ^double [xs]
  (let [^doubles arr (h/->double-array xs)]
    (MathEx/mad arr)))

(defn mul
  ^doubles [xs ys]
  (let [a1 (h/->double-array xs)
        a2 (h/->double-array ys)]
    (h/map #(* ^double %1 ^double %2) a1 a2)))

;; (defn histc                             ;TODO
;;   "or array arr counts the number of values in arr that fall between the elements
;;   in the bins seq. values outside the range in bins are not counted."
;;   ^double [bins xs]
;;   (let [arr  (h/->double-array xs)
;;         min' (when (number? bins)
;;                [(min arr) (max arr)])
;;         max'
;;         binw (/ (- min' max') bins)]))

(defn transpose
  "returns the matrix transpose"
  ^doubles [coll2d]
  (let [^doubles arr (h/->double-double-array coll2d)]
    (MathEx/transpose arr)))

(defn distance
  ^doubles [xs ys]
  (let [^doubles axs (h/->double-array xs)
        ^doubles ayx (h/->double-array ys)]
    (MathEx/distance axs ayx)))

(defn power-of-two?
  "returns true if x is a power of 2"
  [^long x]
  (MathEx/isPower2 x))

(defn norm
  "l2 vector norm."
  ^double [xs]
  (let [^doubles axs (h/->double-array xs)]
    (MathEx/norm axs)))

(defn scale
  ^doubles [a xs]
  (let [^doubles axs (h/->double-array-or-copy xs)]
    (MathEx/scale a axs)
    axs))

(defn sqr
  ^double [^double x]
  (MathEx/sqr x))

(defn sub
  ^doubles [xs ys]
  (let [^doubles axs (h/->double-array-or-copy xs)
        ^doubles ays (h/->double-array ys)]
    (MathEx/sub axs ays)
    axs))

(defn standarize
  ^doubles [xs]
  (let [^doubles axs (h/->double-array-or-copy xs)]
    (MathEx/standardize axs)
    axs))

(defn sum
  ^doubles [xs]
  (let [^doubles axs (h/->double-array xs)]
    (MathEx/sum axs)))

(defn pdist-euclidean
  "pairwise distance between two sets of observations"
  (^doubles [arr2d]
   (MathEx/pdist ^"[[D" arr2d))
  (^doubles [xs1 xs2]
   (let [^doubles axs   (h/->double-array xs1)
         ^doubles ayx   (h/->double-array xs2)
         ^"[[D"   arr2d (h/->double-double-array [axs ayx])]
     (.toArray ^smile.math.matrix.Matrix (MathEx/pdist arr2d)))))

(defn pdist-manhatan
  "pairwise distance between two sets of observations"
  (^doubles [xs1 xs2]
   (let [^doubles axs (h/->double-array xs1)
         ^doubles ayx (h/->double-array xs2)
         md (smile.math.distance.ManhattanDistance.)]
     (.toArray ^smile.math.matrix.Matrix (.d md axs ayx)))))

(defn pdist-chebychev
  "pairwise distance between two sets of observations"
  (^doubles [xs1 xs2]
   (let [^doubles axs (h/->double-array xs1)
         ^doubles ayx (h/->double-array xs2)
         cd (smile.math.distance.ChebyshevDistance.)]
     (.toArray ^smile.math.matrix.Matrix (.d cd axs ayx)))))

(defn pdist-hamming
  "pairwise distance between two sets of observations"
  (^doubles [xs1 xs2]
   (let [^doubles axs (h/->double-array xs1)
         ^doubles ayx (h/->double-array xs2)
         hd (smile.math.distance.HammingDistance.)]
     (.toArray ^smile.math.matrix.Matrix (.d hd axs ayx)))))
