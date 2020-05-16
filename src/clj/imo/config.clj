(ns imo.config
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :refer [expound-str defmsg]])
  (:import (imo ImoException)))

(s/def ::width (s/and integer? #(<= 50 % 200)))
(defmsg ::width "Width must be an integer between 50 and 200 chars")

(s/def ::cache (s/or :file string? :disabled false?))
(defmsg ::cache "Cache must be either a filename or `false` to disable caching entirely")

(def config-spec
  (s/keys :req-un [::width ::cache]))

(defn check
  "Checks whether the given config is valid or not. If config is not
   valid, throws an `ImoException`. Otherwise returns the config."
  [config]
  (when-not (s/valid? config-spec config)
    (let [msg (expound-str config-spec config {:print-specs? false})]
      (throw (ImoException. msg))))
  config)

(def defaults
  "Default configuration options"
  {:width 80
   :cache ".imo/cache.json"})

(defn merge-config
  "Merges config from defaults, file and cli and return final
   merged configuration. If resulting config is not valid,
   throws an `ImoException`."
  [config-from-file config-from-cli]
  (letfn [(m [a b]
            (if (and (map? a) (map? b))
              (merge-with m a b)
              b))
          (check-input [c msg]
            (when-not (or (map? c) (nil? c))
              (throw (ImoException. msg))))]
    (check-input config-from-file "Config file options must be a map")
    (check-input config-from-cli "CLI options must be a map")
    (-> (merge-with m defaults config-from-file config-from-cli)
        (check))))
