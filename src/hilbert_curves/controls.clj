(ns hilbert-curves.controls
  (:require [quil.core :as q]))

; keeps track of current input mode, has to respond to a key in state
(def input-mode (atom :none))

; holds handler that is invoked when text buffer is flushed
(def current-handler (atom nil))

; holds input value when in input mode, will be flushed an written to state when :enter is pressed
(def text-buffer (atom ""))

(defn flush-buffer
  "Flushes the text buffer, returning new state."
  [state]
  (let [mode @input-mode
        handler @current-handler
        text @text-buffer]
    (reset! input-mode :none)
    (reset! current-handler nil)
    (reset! text-buffer "")
    (try
      (if (nil? handler)
        state
        (assoc state mode (handler text)))
      (catch Exception e
        (println (.getMessage e))
        state))))

(defn make-action
  "Helper to make actions that take no params and do not return state not break the app."
  [f]
  (fn [state]
    (f) ; invoke f, which needs no params
    state)) ; return state as it was


(def controls (atom [{:label "Space: Pause / Resume"
                      :action (make-action #(if (q/looping?) (q/no-loop) (q/start-loop)))
                      :mode nil
                      :key :space}
                     {:label "O: Set order"
                      :action #(Integer/parseInt %)
                      :mode :order
                      :key :o}
                     {:label "C: Set counter increment"
                      :action #(Integer/parseInt %)
                      :mode :counter-increments
                      :key :c}]))

(defn is-enter?
  "I just couldn't get the key code for enter to work, so I'm using this workaround."
  [raw-key]
  (= (str (format "%04x" (int raw-key))) "000a"))

(defn handle-input-mode-press [state event]
  (if (is-enter? (:raw-key event))
    (flush-buffer state) ; if enter was pressed flush buffer and return new state
    (do (swap! text-buffer #(str % (:raw-key event))) ; add input to text buffer
        state)))

(defn show-controls []
  (q/text-size 14)
  (when (not (= @input-mode :none))
    (q/text (str "Input (Enter to confirm): " @text-buffer) 10 20))
  (doseq [[idx item] (map-indexed (fn [idx item] [idx item]) @controls)]
    (q/text (:label item) (+ 10 (* 100 (int (/ idx 4)))) (+ 25 (* 20 (mod (inc idx) 4))))))

(defn key-press [state event]
  (let [key (:key event)]
    ; if we're not in input mode see if the key maps to a control
    (if (= @input-mode :none)
      (if-let [control (first (filter #(= (:key %) key) @controls))]
        (if (nil? (:mode control))
          ; if the control has no mode, just invoke the action and return state
          ((:action control) state)
          ; if the control has a mode, enter input mode and return state
          (do (reset! input-mode (:mode control))
              (reset! current-handler (:action control))
              state))
        state) ; just return state when no mapped key was pressed
      (handle-input-mode-press state event))))
