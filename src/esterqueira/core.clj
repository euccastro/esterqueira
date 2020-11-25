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
   (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)
   (org.lwjgl.system MemoryStack MemoryUtil)))


(def vertex-shader-src
  "#version 330 core

layout(location = 0) in vec3 vertexPosition_modelspace;
void main(){
  gl_Position.xyz = vertexPosition_modelspace;
  gl_Position.w = 1.0;
}
")

(def fragment-shader-src
  "#version 330 core
out vec3 color;
void main(){
  color = vec3(1,0,0);
}")

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


(defn create-square-vert-array []
  (let [array (GL30/glGenVertexArrays)
        buff (GL15/glGenBuffers)]
    (prn "array" array "buff" buff)
    (GL30/glBindVertexArray array)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER buff)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER
                       (float-array [-1.0 -1.0 0.0
                                     1.0 -1.0 0.0
                                     0.0 1.0 0.0])
                       GL15/GL_STATIC_DRAW)))


(defn create-shaders []
  (let [vid (GL30/glCreateShader GL30/GL_VERTEX_SHADER)
        fid (GL30/glCreateShader GL30/GL_FRAGMENT_SHADER)
        pid (GL30/glCreateProgram)]
    (GL30/glShaderSource vid vertex-shader-src)
    (GL30/glCompileShader vid)
    (prn "vertex ret is" (GL30/glGetShaderi vid GL30/GL_COMPILE_STATUS) (GL30/glGetShaderi vid GL30/GL_INFO_LOG_LENGTH))
    (GL30/glShaderSource fid fragment-shader-src)
    (GL30/glCompileShader fid)
    (prn "fragment ret is" (GL30/glGetShaderi vid GL30/GL_COMPILE_STATUS) (GL30/glGetShaderi vid GL30/GL_INFO_LOG_LENGTH))
    (GL30/glAttachShader pid vid)
    (GL30/glAttachShader pid fid)
    (GL30/glLinkProgram pid)
    (prn "link status si" (GL30/glGetProgrami pid GL30/GL_LINK_STATUS))
    pid))


(defn init-gl []
  (GL/createCapabilities)
  (println "OpenGL version:" (GL11/glGetString GL11/GL_VERSION))
  (GL11/glClearColor 0.5 0.5 0.5 0.0)
  (create-square-vert-array))


(defn sec->ns [s]
  (* s 1000000000))


(set! *warn-on-reflection* true)


(defn framebuffer-size [^long w]
  (with-open [stack (MemoryStack/stackPush)]
    (let [width (.mallocInt stack 1)
          height (.mallocInt stack 1)]
      (GLFW/glfwGetFramebufferSize w width height)
      [(.get width 0) (.get height 0)])))


(defn draw [window program-id]
  (prn "err 1" (GL11/glGetError))
  (GL30/glUseProgram program-id)
  (GL30/glEnableVertexAttribArray 0)
  (GL30/glVertexAttribPointer 0 3 GL11/GL_FLOAT false 0 0)

  (prn "err 2" (GL11/glGetError))
  (GL30/glDrawArrays GL11/GL_TRIANGLES 0 3)

  (prn "err 3" (GL11/glGetError))
  (GL30/glDisableVertexAttribArray 0))

(defn main-loop [window]
  (let [histogram (Histogram. 1 (sec->ns 1) 3)
        program-id (create-shaders)]
    (loop [frame-t0 nil]
      (draw window program-id)
      (when frame-t0
        (.recordValue histogram (- (System/nanoTime) frame-t0)))
      (GLFW/glfwSwapBuffers window)
      (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT  GL11/GL_DEPTH_BUFFER_BIT))
      (let [t (System/nanoTime)]
        (GLFW/glfwPollEvents)
        (when-not (GLFW/glfwWindowShouldClose window)
          (recur t))))
    (.outputPercentileDistribution histogram System/out 1000000.0)))


(set! *warn-on-reflection* false)


(defn run [{:keys [width height]}]
  (prn "running with" width height)
  (with-glfw
    (with-window {:width width
                  :height height
                  :title "Janela"}
      (fn [w]
        (init-gl)
        (apply on-resize w (framebuffer-size w))
        (main-loop w)))))

(defmacro quick-test [& body]
  `(with-glfw
     (with-window {:width 200 :height 100 :title "prova"}
       (fn [~'w]
         (init-gl)
         ~@body
         (Thread/sleep 3000)))))

(comment


  (quick-test
   )

  (with-glfw
    (with-window {:width 123 :height 456 :title "prova"}
      (fn [w]
        (init-gl))
      #(println "size si" (framebuffer-size %))))

  (run 800 800)

  (def cb (proxy [GLFWFramebufferSizeCallback] []
            (invoke [window width height]
              (println "resized!" width height))))

  (instance? GLFWFramebufferSizeCallbackI cb)
  (MemoryUtil/memAddressSafe cb)
  GLFW$Functions/SetFramebufferSizeCallback

  (future (run 200 100))
  )
