(ns test-check-walkthrough.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer (defspec)]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [test-check-walkthrough.core :refer :all]))

;; Consider a normal unit test:

(deftest a-sample-unit-test
    (testing "an edge case"
      (= []
         (reverse [])))

    (testing "a base case"
      (= [1]
         (reverse [1])))

    (testing "that a specific case holds true"
      (= [0 1 2 3]
         (reverse [3 2 1 0]))))

;; We've provided 3 examples to "cover" the scenarios in which
;; a list could contain 0, 1, or N elements. We're implicitly
;; asserting that if each of these pass then all other lists
;; of 0, 1, or N elements will exhibit the same expected behavior.

;; Two important questions:
;; 1. Do all lists fall into one of these categories? And if they do,
;;    will they, in fact, behave the same way?
;; 2. Have we missed any classes of lists to test?

;; Now imagine writing unit tests for something like clojure's
;; reader and printer. How many tests would you need, and how would
;; you be sure you've covered all classes of bugs?

;; Example-based tests have a lot of value, but these two questions
;; show us that another complementary strategy could help us find
;; more problems with our code.

;; What are we really trying to accomplish with an example-based test?

;; The parts of a unit test suite:
;; 1. The values we are testing against
;; 2. A property that holds for those values
;; 3. Some number of examples/properties

;; Really, we're not trying to test a single example; rather, we're
;; trying to show that a *property* holds *in general*.

;; Enter test.check (and property-based testing)!

;; test.check is a property-based testing library like QuickCheck in
;; other languages. Instead of supplying specific values to test against,
;; you show test.check *how* to generate values randomly. test.check
;; then can randomly generate as many examples as is desired.

;; Randomly generating examples means you can't manually supply all
;; the "answers" to test against. Instead we'll have to be more clever
;; about the properties we check.

;; To see how this works, let's use test.check on clojure's reverse.
;; As a first property, we can assert that when reversing a collection,
;; the first element becomes the last element (this cheats a bit for
;; empty collections, since first and last are both nil and still equal).

(def first-becomes-last
  (prop/for-all [v (gen/vector gen/int)]
                (= (first v)
                   (last (reverse v)))))

(tc/quick-check 100 first-becomes-last) ; Manually run our property

;; It works! Our property held for 100 random examples.

;; We can also integrate test.check into our normal automated
;; clojure.test suite:

(defspec automated-first-becomes-last
  100
  (prop/for-all [v (gen/vector gen/int)]
                (= (first v)
                   (last (reverse v)))))

;; That's property was ok, but let's think of stronger properties that
;; hold for reversing collections:

;; reverse preserves the count of a collection:
(tc/quick-check 100
                (prop/for-all [v (gen/vector gen/int)]
                              (= (count v)
                                 (count (reverse v)))))

;; reverse-ing a collection twice is the same as doing nothing
(tc/quick-check 100
                (prop/for-all [v (gen/vector gen/int)]
                              (= v
                                 (reverse (reverse v)))))



;; for-all is macro sugar that adds bindings to for-all*.
;; for-all* specifies the arguments to generate and then uses
;; apply to apply the arguments to your function. For instance,
;; you could write the above property like so:

(tc/quick-check 100
                (prop/for-all* [(gen/vector gen/int)]
                               (fn [v]
                                 (= v
                                    (reverse (reverse v))))))


;; a + b >= a, right?
(tc/quick-check 100
                (prop/for-all* [gen/int gen/int]
                               (fn [a b] (>= (+ a b) a))))


;; Works now! We only support positive ints!
(tc/quick-check 100
                (prop/for-all* [gen/pos-int gen/pos-int]
                               (fn [a b] (>= (+ a b) a))))


;; The original haskell version is built on a simple
;; function called choose. Conceptually this is all you
;; need to build the other generators, and many (all?) of the
;; other generators rely on it.

(gen/sample (gen/choose 5 10)) ; Numbers 5-10 inclusive

;; You can see how you could easily build:
(gen/sample (gen/elements ["clojure" "haskell" "erlang" "scala" "python"]))


;; Using gen/sample is an effective way to build up test cases
;; for use in test.check. Let's slowly work our way up to complex
;; data structures.

;; Simple data types:
(gen/sample gen/int)
(gen/sample gen/int 30)
(gen/sample gen/nat)
(gen/sample gen/pos-int)
(gen/sample gen/neg-int)
(gen/sample gen/ratio)
(gen/sample gen/boolean)
(gen/sample gen/string-alpha-numeric)
(gen/sample gen/string-ascii)
(gen/sample gen/byte)
(gen/sample gen/bytes)
(gen/sample gen/any-printable)
;; (..and more are included in test.check)

;; Compound data types:
(gen/sample (gen/return nil))
(gen/sample (gen/vector gen/nat))
(gen/sample (gen/map gen/keyword gen/nat))
(gen/sample (gen/tuple gen/string-alpha-numeric gen/int))
(gen/sample (gen/container-type gen/int))

;; Mixing things up:
(gen/sample (gen/elements [:a :b :c]))               ; choose one of values
(gen/sample (gen/one-of [gen/int
                         (gen/return nil)]))         ; choose one of generators
(gen/sample (gen/frequency [[9 gen/int]              ; 9 parts int
                            [1 (gen/return nil)]]))  ; 1 parts nil


;; Quick review:
;; - What is the difference between gen/one-of and gen/elements?
;; - Why would you want to use gen/frequency?


;; Generator combinators:

;; fmap applies a function to each element of a generator
(gen/sample (gen/fmap set (gen/vector gen/nat)))

;; such-that takes a predicate and acts like clojure's filter
(gen/sample (gen/such-that (comp not empty?) (gen/vector gen/nat)))

;; bind allows us to create a new generator based on the value of a previously
;; created generator (copied from README)
;;
;; takes a keyword vector and returns a random element + keyword vector
(def keyword-vector (gen/such-that not-empty (gen/vector gen/keyword)))
(def vec-and-elem
  (gen/bind keyword-vector
            (fn [v] (gen/tuple (gen/elements v) (gen/return v)))))
(gen/sample vec-and-elem 4)



;; Intermission: time to code!
;;
;; If we wanted to tests a bowling game scorer, we might decide
;; to generate random games for it to score.  Let's do that now!
;;
;; Final data structures should look something like this:
;;
;; [[3 6]           ; Frame 1
;;  [9 0]           ; Frame 2
;;  :strike         ; Frame 3
;;  [8 1]           ; Frame 4
;;  [3 :spare]      ; Frame 5
;;  [3 1]           ; Frame 6
;;  [4 3]           ; Frame 7
;;  [2 6]           ; Frame 8
;;  [9 0]           ; Frame 9
;;  [3 :spare 8]]   ; Frame 10 (note: 10th frames sometimes have a third roll)

;; Assignment: try to generate data structures like the above example. When
;; finished, scroll down to see some other possible solutions (no peeking!)

;; (gen/sample (gen/vector ...???))



























































;; Done? Ok, compare and contrast:


;; One possible solution:

(def first-roll
  (gen/one-of [(gen/choose 0 9)
               (gen/return :strike)]))

(gen/sample first-roll) ; Oh wait...do we want that many strikes?

(def first-roll
  (gen/frequency [[10 (gen/choose 0 9)]
                  [1 (gen/return :strike)]]))

(gen/sample first-roll) ; Much better


(defn second-roll ; Now define second-roll as a function of the first..
  [roll]
  (if (= :strike roll)
    (gen/return :strike)
    (let [pins-left (- 10 roll)]
      (gen/tuple (gen/return roll)
                 (gen/one-of [(gen/choose 0 (dec pins-left))
                              (gen/return :spare)])))))

(def frame
  (gen/bind first-roll
            second-roll)) ;  .. and bind them together

(gen/sample frame) ; Oversamples spares a bit - can you see why?

;; A full game generator (ignoring the last-frame extra roll possibility):
(def full-game
  (gen/tuple frame
             frame
             frame
             frame
             frame
             frame
             frame
             frame
             frame
             frame))

(first (gen/sample full-game 1))



;; Another possible way to build up our frames:

(def non-spare-frame
  (gen/one-of [(gen/tuple (gen/return 0) (gen/choose 0 9))
               (gen/tuple (gen/return 1) (gen/choose 0 8))
               (gen/tuple (gen/return 2) (gen/choose 0 7))
               (gen/tuple (gen/return 3) (gen/choose 0 6))
               (gen/tuple (gen/return 4) (gen/choose 0 5))
               (gen/tuple (gen/return 5) (gen/choose 0 4))
               (gen/tuple (gen/return 6) (gen/choose 0 3))
               (gen/tuple (gen/return 7) (gen/choose 0 2))
               (gen/tuple (gen/return 8) (gen/choose 0 1))
               (gen/tuple (gen/return 9) (gen/return 0))]))

(gen/sample non-spare-frame)


(def spare-frame
  (gen/tuple (gen/choose 0 9) (gen/return :spare)))

(gen/sample spare-frame)


(def strike-frame
  (gen/return :strike))

(gen/sample strike-frame)


(def frame
  (gen/frequency [[9 non-spare-frame]
                  [3 spare-frame]
                  [1 strike-frame]]))
(gen/sample frame)


;; End of intermission, back to the main program.



;; We can generator Records too!

(defrecord User [user-name user-id email active?])

;; recall that a helper function is automatically generated
;; for us
(->User "reiddraper" 15 "reid@example.com" true)

;; Make an email
(def domain (gen/elements ["gmail.com" "hotmail.com" "computer.org"]))
(def email-gen
  (gen/fmap (fn [[name domain-name]]
              (str name "@" domain-name))
            (gen/tuple (gen/not-empty gen/string-alpha-numeric) domain)))

(last (gen/sample email-gen))

;; And now a user
(def user-gen
  (gen/fmap (partial apply ->User)
            (gen/tuple (gen/not-empty gen/string-alpha-numeric)
                       gen/nat
                       email-gen
                       gen/boolean)))

(last (gen/sample user-gen))


;; We can generator recursive data structures, but there are some
;; gotchas. (Let's put this in a comment, because they don't all work!)

(comment

  ;; This won't work: can't refer to itself in its definition
  (def tree1
    (gen/one-of ;; choose either a natural number, or a node
                [gen/nat
                 (gen/tuple gen/nat
                            (gen/one-of [(gen/return nil) tree1])
                            (gen/one-of [(gen/return nil) tree1]))]))

  (gen/sample tree1) ;; => NullPointerException

  ;; This won't work: can't stop it from recursing
  (defn tree2
    []
    (gen/one-of ;; choose either a natural number, or a node
                [gen/nat
                 (gen/tuple gen/nat
                            (gen/one-of [(gen/return nil) (tree2)])
                            (gen/one-of [(gen/return nil) (tree2)]))]))

  (gen/sample tree2) ;; => StackOverflow

  ;; Luckily, test.check has a sized function, which will pass a size
  ;; into your definition.  You decide how to size a reasonable structure
  ;; basd on this incoming size integer.
  (defn tree3
    [size]
    (if (= size 0)
      gen/nat
      (let [new-size (quot size 2)
            smaller-tree (gen/resize new-size (gen/sized tree3))]
        (gen/one-of ;; choose either a natural number, or a node
                    [gen/nat
                     (gen/tuple gen/nat
                                (gen/one-of [(gen/return nil) smaller-tree])
                                (gen/one-of [(gen/return nil) smaller-tree]))]))))

  (gen/sample (gen/sized tree3)) ;; => Works!

  ;; Notice this still doesn't work:
  (gen/sample tree3)

  ;; Notice that we're getting lots of small or single element trees. You
  ;; can fix this by replacing gen/one-of with gen/frequency

  )

;; A few tips from the original QuickCheck paper:

;; TIP 1

;; As a general rule, we want to check the distribution of our data, but
;; especially when we have preconditions. Preconditions subtly skew
;; distribution. For instance, let's say we only support sorted vectors of
;; ints.  The obvious generator is:

(defn ascending? ; stolen from test.check intro
  "clojure.core/sorted? doesn't do what we might expect, so we write our
  own function"
  [coll]
  (every? (fn [[a b]] (<= a b))
          (partition 2 1 coll)))

(gen/sample (gen/such-that ascending? (gen/vector gen/nat)))

;; Which will always produce sorted vectors of natural numbers...except
;; we've introduced a bias towards small vectors. Note how many empty
;; vectors or vectors of a single element there are! This is because
;; a vector of 0 or 1 elements is always sorted, but vectors of 2 elements
;; are only sorte 50% of the time!
;;
;; Here's a better way to generate sorted vectors:

(gen/sample (gen/fmap (comp vec sort) (gen/vector gen/nat)))


;; TIP 2

;; Let's say we want to test properties of the cycle function, which
;; repeats a list infinitely

(take 10 (cycle [1 2 3]))

;; Now clearly the following property holds:
;;
;; (= (cycle xs)
;;    (cycle (concat xs xs)))

;; But we do NOT want to run this:

(comment
  (tc/quick-check 100
                  (prop/for-all [xs (gen/such-that (comp not empty?) (gen/list gen/int))]
                                (= (cycle xs)
                                   (cycle (concat xs xs))))))

;; These are infinite lists and this will never terminate when it
;; tries to compare all the elements! So instead, let's also randomize
;; how far we check by adding another gen/int:

(tc/quick-check 100
                (prop/for-all [xs (gen/such-that (comp not empty?) (gen/list gen/int))
                               n  (gen/one-of [gen/int])]
                              (= (take n (cycle xs))
                                 (take n (cycle (concat xs xs))))))

;; Tip 3
;;
;; When all tests pass, be sure to examine the generated test data to
;; make sure it's distributed the way you expect.


;; And now a few patterns (from Reid Draper's Clojure/West talk)

;; Pattern 1: round-trip
;; (= x
;;    (decode (encode x)))
;;
;; Note that this is not a sufficient property for complete correctness
;; (probably none are), but it's still a very useful property to add
;; for fns where this holds true.


;; Pattern 2: trusted-implementation
;; this is useful when a correct known working fn exists, or
;; when the obviously correct implentation is not performant.
;; We can check a more efficient but perhaps harder to implement
;; algorithm against a simpler but obviously correct version.

;; Pattern 3: input/output relation
(tc/quick-check 10
                (prop/for-all [xs (gen/vector gen/any)
                               ys (gen/vector gen/any)]
                              (= (count (concat xs ys))
                                 (+ (count xs)
                                    (count ys)))))


;; Shrinking:

;; Of course the following will fail, since we're supplying
;; vectors of any integers, not just positive ones.
(tc/quick-check 100
                (prop/for-all [xs (gen/vector gen/int)]
                              (every? pos? xs)))

;; Let's find a failing case we can examine directly
(let [tc-output (tc/quick-check 100
                                (prop/for-all [xs (gen/vector gen/int)]
                                              (every? pos? xs)))]
  (->> tc-output
       :fail
       first))

;; Now, it's not so hard to examine the failing cases here, but
;; if we have much larger data structures, it could be difficult
;; to tell what's wrong.  We'd like to have a minimal failing example,
;; not a vector of 1000 integers and all but one (the last one) be positive.
(let [tc-output (tc/quick-check 100
                                (prop/for-all [xs (gen/vector gen/int)]
                                              (every? pos? xs)))]
  (->> tc-output
       :shrunk
       :smallest
       first))

;; Notice our smallest failing example shrinks to [0]. test.check automatically
;; removes pieces of a larger failing example until it passes.  It continues
;; to do this until removing any piece causes the test to pass, and then it knows
;; it has the smallest possible failing example.
