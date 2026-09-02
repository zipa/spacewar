(ns spacewar.ui.config-hardening-spec
  (:require [spacewar.ui.config :as uic]
            [speclj.core :refer [describe it should=]]))

(describe "named colors"
  (it "keeps exact RGB triples"
    (should= [0 0 0] uic/black)
    (should= [255 255 0] uic/yellow)
    (should= [100 100 0] uic/dark-yellow)
    (should= [255 0 0] uic/red)
    (should= [100 0 0] uic/dark-red)
    (should= [0 255 0] uic/green)
    (should= [0 0 255] uic/blue)
    (should= [200 0 50] uic/klingon-color)
    (should= [0 255 50] uic/enterprise-color)
    (should= [0 100 20] uic/enterprise-vector-color)
    (should= [0 50 255] uic/weapon-factory-color)
    (should= [255 100 0] uic/antimatter-factory-color)
    (should= [250 200 0] uic/dilithium-factory-color)
    (should= [0 255 0] uic/corbomite-factory-color)
    (should= [240 240 0] uic/status-panel-mercury-color)
    (should= [255 0 0] uic/kinetic-color)
    (should= [255 200 0] uic/klingon-kinetic-color)
    (should= [0 255 100 64] uic/transport-route-color)))
