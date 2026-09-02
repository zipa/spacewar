(ns spacewar.ui.config)

(def white [255 255 255])
(def black [0 0 0])
(def dark-grey [50 50 50])
(def grey [128 128 128])
(def light-grey [200 200 200])
(def yellow [255 255 0])
(def dark-yellow [100 100 0])
(def orange [255 150 50])
(def red [255 0 0])
(def dark-red [100 0 0])
(def green [0 255 0])
(def blue [0 0 255])
(def klingon-color [200 0 50])
(def enterprise-color [0 255 50])
(def enterprise-vector-color [0 100 20])
(def velocity-vector-scale 20)
(def weapon-factory-color [0 50 255])
(def antimatter-factory-color [255 100 0])
(def dilithium-factory-color [250 200 0])
(def corbomite-factory-color [0 255 0])

(def scan-panel-color [150 200 255])
(def scan-panel-button-color [100 150 255])
(def scan-panel-selection-color [70 100 200])
(def engine-panel-color [150 255 150])
(def engine-panel-button-color [80 255 80])
(def engine-panel-selection-color [30 200 30])
(def weapons-panel-color [255 200 50])
(def weapons-panel-button-color [255 150 50])
(def weapons-panel-selection-color [200 100 30])
(def damage-panel-color [255 100 100])
(def deploy-panel-button-color [80 80 255])
(def deploy-panel-color [120 120 255])
(def status-panel-color [255 255 200])
(def status-panel-mercury-color [240 240 0])
(def new-game-color [128 128 128])
(def new-game-button-color [100 100 100])

(def banner-width 40)
(def stringer-width 15)
(def button-gap 10)
(def button-h 40)
(def slider-width 50)
(def engage-width 100)
(def button-width 150)

(def phaser-length 30)
(def kinetic-color [255 0 0])
(def klingon-kinetic-color [255 200 0])

(def phaser-target 1000)
(def torpedo-target 1000)
(def kinetic-target 1000)

(def transport-route-color [0 255 100 64])

(def star-colors {:o [200 200 255]
                  :b [220 220 255]
                  :a [240 240 240]
                  :f [250 250 200]
                  :g [250 250 150]
                  :k [255 200 150]
                  :m [255 150 150]
                  :pulsar [20 150 20]})

(def star-sizes {:o 6
                 :b 5
                 :a 5
                 :f 4
                 :g 4
                 :k 3
                 :m 3
                 :pulsar 3})

(def explosion-profiles
  {:phaser {:explosion-profile [{:velocity 0.5 :until 100}
                                {:velocity -0.05 :until 1000}]
            :explosion-color-profile [{:until 100 :colors [white white]}
                                      {:until 1000 :colors [white black]}]
            :fragment-color-profile [{:until 100 :colors [white white]}
                                     {:until 300 :colors [white yellow]}
                                     {:until 500 :colors [yellow red]}
                                     {:until 1000 :colors [red black]}]}
   :torpedo {:explosion-profile [{:velocity 0.8 :until 100}
                                 {:velocity 0.6 :until 200}
                                 {:velocity 0.4 :until 300}
                                 {:velocity 0.2 :until 400}
                                 {:velocity 0.1 :until 500}
                                 {:velocity 0.0 :until 700}
                                 {:velocity -0.05 :until 2000}]
             :explosion-color-profile [{:until 100 :colors [white white]}
                                       {:until 400 :colors [white yellow]}
                                       {:until 1500 :colors [yellow dark-red]}
                                       {:until 2000 :colors [dark-red black]}]
             :fragment-color-profile [{:until 500 :colors [white white]}
                                      {:until 800 :colors [white yellow]}
                                      {:until 1200 :colors [yellow red]}
                                      {:until 2000 :colors [red black]}]}
   :kinetic {:explosion-profile [{:velocity 0.5 :until 50}
                                 {:velocity -0.05 :until 800}]
             :explosion-color-profile [{:until 100 :colors [white white]}
                                       {:until 300 :colors [white yellow]}
                                       {:until 600 :colors [yellow red]}
                                       {:until 800 :colors [red black]}]
             :fragment-color-profile [{:until 100 :colors [white white]}
                                      {:until 300 :colors [white yellow]}
                                      {:until 500 :colors [yellow red]}
                                      {:until 800 :colors [red black]}]}
   :klingon {:explosion-profile [{:velocity 0.8 :until 100}
                                 {:velocity 0.9 :until 200}
                                 {:velocity 1 :until 400}
                                 {:velocity 0.2 :until 600}
                                 {:velocity 0.1 :until 600}
                                 {:velocity -0.3 :until 800}
                                 {:velocity 1 :until 1000}
                                 {:velocity -0.05 :until 4000}]
             :explosion-color-profile [{:until 100 :colors [white white]}
                                       {:until 700 :colors [white yellow]}
                                       {:until 2500 :colors [yellow dark-red]}
                                       {:until 4000 :colors [dark-red black]}]
             :fragment-color-profile [{:until 500 :colors [white white]}
                                      {:until 800 :colors [white yellow]}
                                      {:until 2000 :colors [yellow red]}
                                      {:until 4000 :colors [red black]}]}
   :romulan {:explosion-profile [{:velocity 0.8 :until 100}
                                 {:velocity 0.9 :until 200}
                                 {:velocity 1 :until 400}
                                 {:velocity 0.2 :until 600}
                                 {:velocity 0.1 :until 600}
                                 {:velocity -0.3 :until 800}
                                 {:velocity 1 :until 1000}
                                 {:velocity -0.05 :until 4000}]
             :explosion-color-profile [{:until 100 :colors [white white]}
                                       {:until 700 :colors [white orange]}
                                       {:until 2500 :colors [orange dark-red]}
                                       {:until 4000 :colors [dark-red black]}]
             :fragment-color-profile [{:until 500 :colors [white white]}
                                      {:until 800 :colors [white orange]}
                                      {:until 2000 :colors [yellow red]}
                                      {:until 4000 :colors [red black]}]}
   :klingon-kinetic {:explosion-profile [{:velocity 0.5 :until 50}
                                         {:velocity -0.03 :until 800}]
                     :explosion-color-profile [{:until 100 :colors [white green]}
                                               {:until 300 :colors [green yellow]}
                                               {:until 600 :colors [yellow red]}
                                               {:until 800 :colors [red black]}]
                     :fragment-color-profile [{:until 100 :colors [grey white]}
                                              {:until 300 :colors [white yellow]}
                                              {:until 500 :colors [yellow red]}
                                              {:until 800 :colors [red black]}]}
   :klingon-phaser {:explosion-profile [{:velocity 0.5 :until 100}
                                        {:velocity -0.05 :until 1000}]
                    :explosion-color-profile [{:until 100 :colors [white white]}
                                              {:until 500 :colors [white green]}
                                              {:until 1000 :colors [green black]}]
                    :fragment-color-profile [{:until 100 :colors [white white]}
                                             {:until 300 :colors [white green]}
                                             {:until 500 :colors [green yellow]}
                                             {:until 1000 :colors [yellow black]}]}
   :klingon-torpedo {:explosion-profile [{:velocity 0.8 :until 100}
                                         {:velocity 0.6 :until 200}
                                         {:velocity 0.4 :until 300}
                                         {:velocity 0.2 :until 400}
                                         {:velocity 0.1 :until 500}
                                         {:velocity 0.0 :until 700}
                                         {:velocity -0.05 :until 2000}]
                     :explosion-color-profile [{:until 100 :colors [white white]}
                                               {:until 400 :colors [white green]}
                                               {:until 1500 :colors [green dark-red]}
                                               {:until 2000 :colors [dark-red black]}]
                     :fragment-color-profile [{:until 500 :colors [white white]}
                                              {:until 800 :colors [white yellow]}
                                              {:until 1200 :colors [yellow red]}
                                              {:until 2000 :colors [red black]}]}
   :romulan-blast {:explosion-profile [{:velocity 0.8 :until 100}
                                       {:velocity 0.9 :until 200}
                                       {:velocity 1 :until 400}
                                       {:velocity 0.2 :until 600}
                                       {:velocity 0.1 :until 600}
                                       {:velocity -0.3 :until 800}
                                       {:velocity 1 :until 1000}
                                       {:velocity -0.05 :until 4000}]
                   :explosion-color-profile [{:until 100 :colors [white white]}
                                             {:until 700 :colors [white blue]}
                                             {:until 2500 :colors [blue orange]}
                                             {:until 2500 :colors [orange dark-red]}
                                             {:until 4000 :colors [dark-red black]}]
                   :fragment-color-profile [{:until 500 :colors [white white]}
                                            {:until 800 :colors [white orange]}
                                            {:until 2000 :colors [yellow red]}
                                            {:until 4000 :colors [red black]}]}
   :ship {:explosion-profile [{:velocity 0.8 :until 100}
                              {:velocity 0.9 :until 200}
                              {:velocity 1 :until 400}
                              {:velocity 0.2 :until 600}
                              {:velocity 0.1 :until 600}
                              {:velocity -0.3 :until 800}
                              {:velocity 1 :until 1000}
                              {:velocity -0.01 :until 8000}]
          :explosion-color-profile [{:until 100 :colors [white white]}
                                    {:until 2000 :colors [white yellow]}
                                    {:until 7000 :colors [yellow dark-red]}
                                    {:until 8000 :colors [dark-red black]}]
          :fragment-color-profile [{:until 500 :colors [white white]}
                                   {:until 2000 :colors [white yellow]}
                                   {:until 4000 :colors [yellow red]}
                                   {:until 8000 :colors [red black]}]}
   :corbomite-device {:explosion-profile [{:velocity 0.8 :until 100}
                                          {:velocity 0.9 :until 200}
                                          {:velocity 0.0 :until 300}
                                          {:velocity -0.1 :until 2000}
                                          {:velocity 0.0 :until 4000}]
                      :explosion-color-profile [{:until 400 :colors [green green]}
                                                {:until 800 :colors [green white]}
                                                {:until 2000 :colors [white red]}
                                                {:until 2500 :colors [red dark-red]}
                                                {:until 4000 :colors [dark-red black]}]
                      :fragment-color-profile [{:until 500 :colors [white green]}
                                               {:until 800 :colors [green orange]}
                                               {:until 3000 :colors [orange red]}
                                               {:until 4000 :colors [red black]}]}
   })

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:13:15.877891-05:00", :module-hash "-5075702", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "779387029"} {:id "def/white", :kind "def", :line 3, :end-line nil, :hash "69201930"} {:id "def/black", :kind "def", :line 4, :end-line nil, :hash "-1848966216"} {:id "def/dark-grey", :kind "def", :line 5, :end-line nil, :hash "291263422"} {:id "def/grey", :kind "def", :line 6, :end-line nil, :hash "843145396"} {:id "def/light-grey", :kind "def", :line 7, :end-line nil, :hash "1608614327"} {:id "def/yellow", :kind "def", :line 8, :end-line nil, :hash "485901183"} {:id "def/dark-yellow", :kind "def", :line 9, :end-line nil, :hash "-2006339828"} {:id "def/orange", :kind "def", :line 10, :end-line nil, :hash "2112251131"} {:id "def/red", :kind "def", :line 11, :end-line nil, :hash "1134865829"} {:id "def/dark-red", :kind "def", :line 12, :end-line nil, :hash "1007667276"} {:id "def/green", :kind "def", :line 13, :end-line nil, :hash "-1636386504"} {:id "def/blue", :kind "def", :line 14, :end-line nil, :hash "-2095682735"} {:id "def/klingon-color", :kind "def", :line 15, :end-line nil, :hash "1927754412"} {:id "def/enterprise-color", :kind "def", :line 16, :end-line nil, :hash "1821423542"} {:id "def/enterprise-vector-color", :kind "def", :line 17, :end-line nil, :hash "788803350"} {:id "def/velocity-vector-scale", :kind "def", :line 18, :end-line nil, :hash "-1251612331"} {:id "def/weapon-factory-color", :kind "def", :line 19, :end-line nil, :hash "1376495046"} {:id "def/antimatter-factory-color", :kind "def", :line 20, :end-line nil, :hash "143359395"} {:id "def/dilithium-factory-color", :kind "def", :line 21, :end-line nil, :hash "-802812265"} {:id "def/corbomite-factory-color", :kind "def", :line 22, :end-line nil, :hash "1312110980"} {:id "def/scan-panel-color", :kind "def", :line 24, :end-line nil, :hash "-1641442480"} {:id "def/scan-panel-button-color", :kind "def", :line 25, :end-line nil, :hash "1290529900"} {:id "def/scan-panel-selection-color", :kind "def", :line 26, :end-line nil, :hash "1586253299"} {:id "def/engine-panel-color", :kind "def", :line 27, :end-line nil, :hash "1141089803"} {:id "def/engine-panel-button-color", :kind "def", :line 28, :end-line nil, :hash "1625807705"} {:id "def/engine-panel-selection-color", :kind "def", :line 29, :end-line nil, :hash "-1410962308"} {:id "def/weapons-panel-color", :kind "def", :line 30, :end-line nil, :hash "-756517027"} {:id "def/weapons-panel-button-color", :kind "def", :line 31, :end-line nil, :hash "1782946421"} {:id "def/weapons-panel-selection-color", :kind "def", :line 32, :end-line nil, :hash "-1642425537"} {:id "def/damage-panel-color", :kind "def", :line 33, :end-line nil, :hash "104675270"} {:id "def/deploy-panel-button-color", :kind "def", :line 34, :end-line nil, :hash "2079258551"} {:id "def/deploy-panel-color", :kind "def", :line 35, :end-line nil, :hash "1861123825"} {:id "def/status-panel-color", :kind "def", :line 36, :end-line nil, :hash "1026709430"} {:id "def/status-panel-mercury-color", :kind "def", :line 37, :end-line nil, :hash "1163832562"} {:id "def/new-game-color", :kind "def", :line 38, :end-line nil, :hash "-171253803"} {:id "def/new-game-button-color", :kind "def", :line 39, :end-line nil, :hash "-853815623"} {:id "def/banner-width", :kind "def", :line 41, :end-line nil, :hash "-1493584413"} {:id "def/stringer-width", :kind "def", :line 42, :end-line nil, :hash "-625098137"} {:id "def/button-gap", :kind "def", :line 43, :end-line nil, :hash "-1605534339"} {:id "def/button-h", :kind "def", :line 44, :end-line nil, :hash "-924371094"} {:id "def/slider-width", :kind "def", :line 45, :end-line nil, :hash "1410869552"} {:id "def/engage-width", :kind "def", :line 46, :end-line nil, :hash "-1126821112"} {:id "def/button-width", :kind "def", :line 47, :end-line nil, :hash "343855299"} {:id "def/phaser-length", :kind "def", :line 49, :end-line nil, :hash "-1240419912"} {:id "def/kinetic-color", :kind "def", :line 50, :end-line nil, :hash "-1810188752"} {:id "def/klingon-kinetic-color", :kind "def", :line 51, :end-line nil, :hash "-241453605"} {:id "def/phaser-target", :kind "def", :line 53, :end-line nil, :hash "952196417"} {:id "def/torpedo-target", :kind "def", :line 54, :end-line nil, :hash "1959336861"} {:id "def/kinetic-target", :kind "def", :line 55, :end-line nil, :hash "323477822"} {:id "def/transport-route-color", :kind "def", :line 57, :end-line nil, :hash "1963368305"} {:id "def/star-colors", :kind "def", :line 59, :end-line nil, :hash "1830541919"} {:id "def/star-sizes", :kind "def", :line 68, :end-line nil, :hash "-847059248"} {:id "def/explosion-profiles", :kind "def", :line 77, :end-line nil, :hash "597544890"}]}
;; clj-mutate-manifest-end
