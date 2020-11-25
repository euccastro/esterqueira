(ns esterqueira.core
  (:require
   [clojure.java.io :as io])
  (:import
   (org.HdrHistogram Histogram)
   (org.lwjgl.glfw GLFW
                   GLFW$Functions
                   GLFWErrorCallback
                   GLFWFramebufferSizeCallback
                   GLFWFramebufferSizeCallbackI
                   GLFWKeyCallback)
   (org.lwjgl.nanovg NanoVG NanoVGGL3 NVGColor)
   (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)
   (org.lwjgl.system MemoryStack MemoryUtil)))


(defmacro with-glfw [& body]
  `(do
     (.set (GLFWErrorCallback/createPrint System/err))
     (when-not (GLFW/glfwInit)
       (throw (IllegalStateException. "Unable to initialize GLFW")))
     (try
       ~@body
       (finally
         (.free (GLFW/glfwSetErrorCallback nil))
         (GLFW/glfwTerminate)))))


(defn on-resize [w width height]
  (GL11/glViewport 0 0 width height)
  (println "resized!" w width height))


(defn create-glfw-window [width height title]
  (GLFW/glfwDefaultWindowHints)
  (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GL11/GL_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 3)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 2)
  (let [w (GLFW/glfwCreateWindow width height title 0 0)
        kcb (proxy [GLFWKeyCallback] []
              (invoke [window key scancode action mods]
                (when (and (= key GLFW/GLFW_KEY_ESCAPE)
                           (= action GLFW/GLFW_RELEASE))
                  (GLFW/glfwSetWindowShouldClose window true))))
        cb (proxy [GLFWFramebufferSizeCallback] []
             (invoke [window width height]
               (on-resize window width height)))]
    (GLFW/glfwSetKeyCallback w kcb)
    (GLFW/glfwSetFramebufferSizeCallback w cb)
    (GLFW/glfwMakeContextCurrent w)
    (GLFW/glfwSwapInterval 1)
    (GLFW/glfwShowWindow w)
    w))


(defn with-window [{:keys [width height title]} f]
  (let [w (create-glfw-window width height title)]
    (try
      (f w)
      (finally
        (GLFW/glfwDestroyWindow w)))))


(defn init-gl []
  (GL/createCapabilities)
  (println "OpenGL version:" (GL11/glGetString GL11/GL_VERSION))
  (GL11/glClearColor 0.5 0.5 0.5 0.0)
  (GL11/glEnable GL11/GL_STENCIL_TEST))


(defn sec->ns [s]
  (* s 1000000000))


(defn with-nvg [f]
  (if-let [nvg
           (NanoVGGL3/nvgCreate 0)]
    (try
      (NanoVG/nvgCreateFont nvg "hack" (.getFile (io/resource "Hack-Regular.ttf")))
      (f nvg)
      (finally
        (NanoVGGL3/nvgDelete nvg)))
    (throw (RuntimeException. "Failed to create NanoVG"))))


(set! *warn-on-reflection* true)


(defn framebuffer-size [^long w]
  (with-open [stack (MemoryStack/stackPush)]
    (let [width (.mallocInt stack 1)
          height (.mallocInt stack 1)]
      (GLFW/glfwGetFramebufferSize w width height)
      [(.get width 0) (.get height 0)])))


(defn stack-nvg-color [r g b a]
  (doto (NVGColor/mallocStack)
    (.r r)
    (.g g)
    (.b b)
    (.a a)))


(defn draw [window ^long nvg]
  (with-open [_ (MemoryStack/stackPush)]
    (let [[width height] (framebuffer-size window)]
      (NanoVG/nvgBeginFrame nvg width height 1))
    (do
      (NanoVG/nvgBeginPath nvg)
      (NanoVG/nvgRoundedRect nvg, 10, 10, 100, 100, 5)
      (NanoVG/nvgFillColor nvg (stack-nvg-color 1.0 1.0 0.0 1.0))
      (NanoVG/nvgFill nvg))
    (do
      (NanoVG/nvgFontFace nvg "hack")
      (NanoVG/nvgFillColor nvg (stack-nvg-color 1.0 0.0 0.0 1.0))
      (NanoVG/nvgText nvg 50.0 50.0 "Hello world!"))
    (NanoVG/nvgEndFrame nvg)))


(defn main-loop [window nvg]
  (let [histogram (Histogram. 1 (sec->ns 1) 3)]
    (loop [frame-t0 nil]
      (draw window nvg)
      (when frame-t0
        (.recordValue histogram (- (System/nanoTime) frame-t0)))
      (GLFW/glfwSwapBuffers window)
      (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT  GL11/GL_DEPTH_BUFFER_BIT GL11/GL_STENCIL_BUFFER_BIT))
      (let [t (System/nanoTime)]
        (GLFW/glfwPollEvents)
        (when-not (GLFW/glfwWindowShouldClose window)
          (recur t))))
    (.outputPercentileDistribution histogram System/out 1000000.0)))


(set! *warn-on-reflection* false)


(defn run [width height]
  (with-glfw
    (with-window {:width width
                  :height height
                  :title "Janela"}
      (fn [w]
        (init-gl)
        (apply on-resize w (framebuffer-size w))
        (with-nvg
          (fn [nvg]
            (main-loop w nvg)))))))


(comment

  (with-glfw
    (with-window {:width 123 :height 456 :title "prova"}
      #(println "size si" (framebuffer-size %))))

  (run 800 800)

  (def cb (proxy [GLFWFramebufferSizeCallback] []
            (invoke [window width height]
              (println "resized!" width height))))

  (instance? GLFWFramebufferSizeCallbackI cb)
  (MemoryUtil/memAddressSafe cb)
  GLFW$Functions/SetFramebufferSizeCallback
  (stack-nvg-color 1.0 1.0 1.0 1.0)

  (future (run 400 400))
  )
