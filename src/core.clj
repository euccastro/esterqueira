(ns esterqueira.core
  (:import
   (org.HdrHistogram Histogram)
   (org.lwjgl.glfw GLFW GLFWErrorCallback GLFWKeyCallback)
   (org.lwjgl.nanovg NanoVG NanoVGGL3 NVGColor)
   (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)))


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


(defn glfw-window [width height title]
  (GLFW/glfwDefaultWindowHints)
  (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GL11/GL_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 3)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 2)
  (let [w (GLFW/glfwCreateWindow width height title 0 0)]
    (GLFW/glfwMakeContextCurrent w)
    (GLFW/glfwSwapInterval 1)
    (GLFW/glfwShowWindow w)
    w))


(defn with-window [{:keys [width height title]}
                   f
                   & args]
  (let [w (glfw-window width height title)]
    (try
      (apply f w args)
      (finally
        (GLFW/glfwDestroyWindow w)))))


(defn init-gl [width height]
  (GL/createCapabilities)
  (println "OpenGL version:" (GL11/glGetString GL11/GL_VERSION))
  (GL11/glClearColor 0.5 0.5 0.5 0.0)
  (GL11/glViewport 0 0 width height)
  (GL11/glEnable GL11/GL_STENCIL_TEST))


(defn sec->ns [s]
  (* s 1000000000))


(set! *warn-on-reflection* true)


(defn draw []
  nil)


(defn main-loop [window width height]
  (init-gl width height)
  (let [histogram (Histogram. 1 (sec->ns 1) 3)]
    (loop [frame-t0 nil]
      (draw)
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
      main-loop
      width
      height)))


(comment

  (run 800 800)

  )
