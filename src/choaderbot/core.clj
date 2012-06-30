(ns choaderbot.core
  (:use irclj.core
        clj-http.core)
  (:require [clojure.string :as str]
            swank.swank)
  (:import (java.net URLEncoder URLDecoder)))

(defonce config (atom {}))

(defn load-config [& [f]]
  (swap! config
         (fn [_ x] x)
         (read-string (slurp (or f "config.clj")))))

;; stolen from ring.util.codec - https://github.com/mmcgrana/ring/

(defn- double-escape [^String x]
  (.replace x "\\" "\\\\"))

(defn percent-encode
  "Percent-encode every character in the given string using either the specified
  encoding, or UTF-8 by default."
  [unencoded & [encoding]]
  (->> (.getBytes unencoded (or encoding "UTF-8"))
       (map (partial format "%%%02X"))
       (str/join)))

(defn- parse-bytes [encoded-bytes]
  (->> (re-seq #"%.." encoded-bytes)
       (map #(subs % 1))
       (map #(.byteValue (Integer/parseInt % 16)))
       (byte-array)))

(defn percent-decode
  "Decode every percent-encoded character in the given string using the
  specified encoding, or UTF-8 by default."
  [encoded & [encoding]]
  (str/replace encoded
               #"(?:%..)+"
               (fn [chars]
                 (-> (parse-bytes chars)
                     (String. (or encoding "UTF-8"))
                     (double-escape)))))

(defn url-encode
  "Returns the url-encoded version of the given string, using either a specified
  encoding or UTF-8 by default."
  [unencoded & [encoding]]
  (str/replace
    unencoded
    #"[^A-Za-z0-9_~.+-]+"
    #(double-escape (percent-encode % encoding))))

(defn url-decode
  "Returns the url-decoded version of the given string, using either a specified
  encoding or UTF-8 by default. If the encoding is invalid, nil is returned."
  [encoded & [encoding]]
  (percent-decode encoded encoding))

;; end stolen code

(defn google-url [s]
  (str "http://google.com/search?q=" (url-encode s)))

(declare bot)

(defn tell-victim [victim link]
  (send-message bot victim link))

(defn handle-message [{:keys [message nick]}]
  (println nick " " message)
  (when ((:victims @config) nick)
    (tell-victim nick (google-url message))))

(defn handle-message-indirect [& args]
  (apply handle-message args))

(defn connect-bot []
  (connect bot :channels (:channels @config)))

(defn disconnect-bot []
  (close bot))

(defn -main
  (load-config) 
  (def bot-params
       {:name (:name @config)
        :server (:server @config)
        :fnmap {:on-message handle-message-indirect
                :on-connect (fn [& _] true)}})
  (def bot (create-irc bot-params))
  (connect-bot)
  (swank.swank/start-repl (:swank-port @config)))
