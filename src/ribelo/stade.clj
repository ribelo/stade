(ns ribelo.stade
  (:refer-clojure :exclude [min max reverse])
  (:require
   [ribelo.halle :as h])
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
  (let [^doubles arr (h/seq->double-array xs)]
    (h/seq->double-array-or-copy arr)))

(defn add
  "element-wise sum of two arrays"
  ^doubles [xs ys]
  (let [^doubles arr1 (h/seq->double-array-or-copy xs)
        ^doubles arr2 (h/seq->double-array ys)]
    (MathEx/add arr1 arr2)
    arr1))

(defn axpy
  "return array by adding a multiple of another array"
  ^doubles [^double a xs ys]
  (let [^doubles axs (h/seq->double-array-or-copy xs)
        ^doubles ayx (h/seq->double-array ys)]
    (MathEx/axpy a axs ayx)
    axs))

(defn choose
  "the n choose k"
  ^doubles [^long n ^long k]
  (MathEx/choose n k))

(defn dot
  "returns the dot product between two vectors"
  ^double [xs ys]
  (let [^doubles axs (h/seq->double-array xs)
        ^doubles ayx (h/seq->double-array ys)]
    (MathEx/dot axs ayx)))

(defn equals
  "check if x element-wisely equals y with default epsilon 1E-10"
  [xs ys]
  (let [^doubles axs (h/seq->double-array xs)
        ^doubles ayx (h/seq->double-array ys)]
    (MathEx/equals axs ayx)))

(defn factorial
  "the factorial of n"
  [^long n]
  (MathEx/factorial n))

(defn min
  "Smallest element in collection"
  ^double [xs]
  (let [arr (h/seq->double-array xs)]
    (MathEx/min ^doubles arr)))

(defn max
  "Largest element in collection"
  ^double [xs]
  (let [arr (h/seq->double-array xs)]
    (MathEx/max ^doubles arr)))

(defn mean
  "Average value of collection"
  ^double [xs]
  (let [arr (h/seq->double-array xs)]
    (MathEx/mean ^doubles arr)))

(defn variance
  "Variance of sample"
  ^double [xs]
  (let [arr (h/seq->double-array xs)]
    (MathEx/var ^doubles arr)))

(defn std
  "Standard deviation of sample"
  ^double [xs]
  (let [arr (h/seq->double-array xs)]
    (MathEx/sd ^doubles arr)))

(defn quantile
  ^double [xs ^double p]
  (let [^doubles arr (h/seq->double-array-or-copy xs)]
    (QuickSelect/select arr (long (* (alength arr) p)))))

(defn percentile
  ^double [xs ^double p]
  (quantile xs (/ p 100.0)))

(defn reverse
  ^doubles [xs]
  (let [^doubles arr (h/seq->double-array-or-copy xs)]
    (MathEx/reverse arr)
    arr))

(defn q1
  ^double [xs]
  (let [^doubles arr (h/seq->double-array-or-copy xs)]
    (MathEx/q1 arr)))

(defn q2
  ^double [xs]
  (let [^doubles arr (h/seq->double-array-or-copy xs)]
    (MathEx/median arr)))

(defn q3
  ^double [xs]
  (let [^doubles arr (h/seq->double-array-or-copy xs)]
    (MathEx/q3 arr)))

(defn quartile
  "Quartilies of a sample"
  ^doubles [xs]
   (let [^doubles arr (h/seq->double-array xs)]
     (double-array [(MathEx/q1 arr) (MathEx/median arr) (MathEx/q3 arr)])))

(defn iqr
  ^double [xs]
   (let [^doubles arr (h/seq->double-array xs)]
     (- (MathEx/q3 arr) (MathEx/q1 arr))))

(defn median
  "Median value of collection"
  ^double [xs]
  (let [^doubles arr (h/seq->double-array-or-copy xs)]
    (MathEx/median arr)))

(defn covariance
  "Covariance of sample"
  ([coll1 coll2]
   (let [^doubles axs (h/seq->double-array coll1)
         ^doubles ayx (h/seq->double-array coll2)]
     (MathEx/cov axs ayx))))

(defn correlation
  "Pearsons correlation"
  ^double [coll1 coll2]
   (let [^doubles axs (h/seq->double-array coll1)
         ^doubles ayx (h/seq->double-array coll2)]
     (MathEx/cor axs ayx)))

(defn mse
  ^double [x-pred y-true]
  (let [^doubles pred-arr (h/seq->double-array x-pred)
        ^doubles true-arr (h/seq->double-array y-true)]
     (smile.validation.metric.MSE/of ^doubles true-arr ^doubles pred-arr)))

(defn rmse
  ^double [x-pred y-true]
  (let [^doubles pred-arr (h/seq->double-array x-pred)
        ^doubles true-arr (h/seq->double-array y-true)]
    (smile.validation.metric.RMSE/of true-arr pred-arr)))

(defn mad
  "Mean absolute deviation"
  ^double [xs]
  (let [^doubles arr (h/seq->double-array xs)]
    (MathEx/mad arr)))

(defn transpose
  "Returns the matrix transpose"
  ^doubles [coll2d]
  (let [^doubles arr (h/seq->double-double-array coll2d)]
    (MathEx/transpose arr)))

(defn distance
  ^doubles [xs ys]
  (let [^doubles axs (h/seq->double-array xs)
        ^doubles ayx (h/seq->double-array ys)]
    (MathEx/distance axs ayx)))

(defn power-of-two?
  "returns true if x is a power of 2"
  [^long x]
  (MathEx/isPower2 x))

(defn norm
  "l2 vector norm."
  ^double [xs]
  (let [^doubles axs (h/seq->double-array xs)]
    (MathEx/norm axs)))

(defn random
  ([]
   (MathEx/random))
  ([^double lo ^double hi]
   (MathEx/random lo hi)))

(defn random-int
  ([^long n]
   (MathEx/randomInt n))
  ([^long lo ^long hi]
   (MathEx/randomInt lo hi)))

(defn round
  (^double [^double x]
   (MathEx/round x 0))
  (^double [^double x ^long p]
   (MathEx/round x p)))

(defn round2
  ^double [^double x]
  (MathEx/round x 2))

(defn scale
  ^doubles [a xs]
  (let [^doubles axs (h/seq->double-array-or-copy xs)]
    (MathEx/scale a axs)
    axs))

(defn sqr
  ^double [^double x]
  (MathEx/sqr x))

(defn sub
  ^doubles [xs ys]
  (let [^doubles axs (h/seq->double-array-or-copy xs)
        ^doubles ays (h/seq->double-array ys)]
    (MathEx/sub axs ays)
    axs))

(defn standarize
  ^doubles [xs]
  (let [^doubles axs (h/seq->double-array-or-copy xs)]
    (MathEx/standardize axs)
    axs))

(defn sum
  ^doubles [xs]
  (let [^doubles axs (h/seq->double-array xs)]
    (MathEx/sum axs)))

(defn pdist-euclidean
  "Pairwise distance between two sets of observations"
  (^doubles [arr2d]
   (MathEx/pdist ^"[[D" arr2d))
  (^doubles [xs1 xs2]
   (let [^doubles axs   (h/seq->double-array xs1)
         ^doubles ayx   (h/seq->double-array xs2)
         ^"[[D"   arr2d (h/seq->double-double-array [axs ayx])]
     (MathEx/pdist arr2d))))

(defn pdist-manhatan
  "Pairwise distance between two sets of observations"
  (^doubles [xs1 xs2]
   (let [^doubles axs (h/seq->double-array xs1)
         ^doubles ayx (h/seq->double-array xs2)
         md (smile.math.distance.ManhattanDistance.)]
     (.d md axs ayx))))

(defn pdist-chebychev
  "Pairwise distance between two sets of observations"
  (^doubles [xs1 xs2]
   (let [^doubles axs (h/seq->double-array xs1)
         ^doubles ayx (h/seq->double-array xs2)
         cd (smile.math.distance.ChebyshevDistance.)]
     (.d cd axs ayx))))

(defn pdist-hamming
  "Pairwise distance between two sets of observations"
  (^doubles [xs1 xs2]
   (let [^doubles axs (h/seq->double-array xs1)
         ^doubles ayx (h/seq->double-array xs2)
         hd (smile.math.distance.HammingDistance.)]
     (.d hd axs ayx))))
