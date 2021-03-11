(ns notespace-sicmutils-example.ex1-5
  (:refer-clojure :exclude [+ - * / compare zero? ref partial run!])
  (:require [aerial.hanami.common :as hanami-common]
            [aerial.hanami.templates :as hanami-templates]
            [notespace-sicmutils.hanami-extras :as hanami-extras]
            [notespace.api :as notespace]
            [notespace.kinds :as kind]
            [notespace-sicmutils.setup]
            [sicmutils.env :as e]))

^kind/hidden
(comment
  ;;  Initialize notespace and open the browser view:
  (notespace/init-with-browser)

  ;; evaluate all notes in this namespace, and update the browser view:
  (notespace/eval-this-notespace)
  ;; Rendering current browser view into a static html file (under the `docs` directory):
  (notespace/render-static-html))

^kind/hidden
(sicmutils.env/bootstrap-repl!)

["# Exercise 1.5: Finding trajectories that minimize action"]

(defn L-harmonic [m k]
  (fn [local]
    (let [q (e/coordinate local)
          v (e/velocity local)]
      (- (* 1/2 m (square v)) (* 1/2 k (square q))))))

(def q (find-path (L-harmonic 1.0 1.0) 0.0 1.0 6 0.0 8))


(defn actual-q [t] (cos t))





(defn to->map [v]
  (let [x (#(* 1/8 e/pi %) v)]
    {:x x :y (q x)}))

(defn to->map-error [v]
  (let [x (#(* 1/8 e/pi %) v)]
    {:x x :y (- (actual-q x) (q x))}))

(def data (mapv to->map-error (range 0 10 0.1)))

^kind/vega (hanami-common/xform
            hanami-templates/point-chart
            :DATA  data)

(comment
  (defn T-harm-ossy [m]
    (fn [[_ _ qdot]]
      (* 1/2 m (square qdot))))

  (defn V-harm-ossy [m g]
    (fn [[_ q _]]
      (- (* m g q))))

  (defn L-harm-ossy [m g]
    ((T-harm-ossy m) - (V-harm-ossy m g)))


  (def q2 (up (literal-function 'x)))


  (Lagrangian-action L-harm-ossy q 't_1 't_2))




["Excercise 1.6 Minimizing action"]


(defn L-free-particle [mass]
  (fn [local]
    (let [v (e/velocity local)]
      (* 1/2 mass (square v)))))

(def q_x (find-path (L-free-particle 1.0) 0.0 1 2 2 4))


(defn to->map-free [v]

  {:x v :y (q v)})

(def data-free (mapv to->map-free (range 0 10 0.01)))

^kind/vega (hanami-common/xform
            hanami-templates/point-chart
            :DATA  data-free)

(defn test-path [t]
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))



(Lagrangian-action (L-free-particle 3) test-path 0.0 10.0)

(Lagrangian-action (L-harmonic 1.0 1.0) q 0.0 (/ e/pi 2))