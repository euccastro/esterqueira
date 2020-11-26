(ns esterqueira.gl
  (:import
   (org.HdrHistogram Histogram)
   (org.lwjgl.glfw GLFW
                   GLFW$Functions
                   GLFWErrorCallback
                   GLFWFramebufferSizeCallback
                   GLFWFramebufferSizeCallbackI
                   GLFWKeyCallback)
   (org.lwjgl.opengl GL GL45)
   (org.lwjgl.system MemoryStack)))


(def vertex-shader-src
  "#version 450 core

layout(location = 0) in vec2 modelPos;
layout(location = 1) uniform vec2 scale;
layout(location = 2) uniform vec2 translation;
void main(){
  gl_Position.x = modelPos.x * scale.x + translation.x;
  gl_Position.y = modelPos.y * scale.y + translation.y;
  gl_Position.z = 0.0;
  gl_Position.w = 1.0;
}
")

(def fragment-shader-src
  "#version 330 core
out vec3 color;
void main(){
  color = vec3(1,1,1);
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
  (GL45/glViewport 0 0 width height)
  (println "resized!" w width height))


(defn create-glfw-window [width height title]
  (GLFW/glfwDefaultWindowHints)
  (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GL45/GL_TRUE)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 4)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 5)
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
  (let [array (GL45/glGenVertexArrays)
        buff (GL45/glGenBuffers)]
    (GL45/glBindVertexArray array)
    (GL45/glBindBuffer GL45/GL_ARRAY_BUFFER buff)
    (GL45/glBufferData GL45/GL_ARRAY_BUFFER
                       (float-array [-1.0 -1.0
                                     1.0 -1.0
                                     1.0 1.0
                                     -1.0 1.0])
                       GL45/GL_STATIC_DRAW)))

(defn- ok-if-1 [x]
  (if (= x 1) "OK" x))

(defn create-shaders []
  (let [vid (GL45/glCreateShader GL45/GL_VERTEX_SHADER)
        fid (GL45/glCreateShader GL45/GL_FRAGMENT_SHADER)
        pid (GL45/glCreateProgram)]
    (GL45/glShaderSource vid vertex-shader-src)
    (GL45/glCompileShader vid)
    (prn "vertex ret is" (ok-if-1 (GL45/glGetShaderi vid GL45/GL_COMPILE_STATUS)))
    (GL45/glShaderSource fid fragment-shader-src)
    (GL45/glCompileShader fid)
    (prn "fragment ret is" (ok-if-1 (GL45/glGetShaderi vid GL45/GL_COMPILE_STATUS)))
    (GL45/glAttachShader pid vid)
    (GL45/glAttachShader pid fid)
    (GL45/glLinkProgram pid)
    (prn "link status si" (ok-if-1 (GL45/glGetProgrami pid GL45/GL_LINK_STATUS)))
    pid))


(defn init-gl []
  (GL/createCapabilities)
  (println "OpenGL version:" (GL45/glGetString GL45/GL_VERSION))
  (GL45/glClearColor 0.5 0.5 0.5 0.0)
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
  (GL45/glUseProgram program-id)
  (GL45/glEnableVertexAttribArray 0)
  (GL45/glVertexAttribPointer 0 2 GL45/GL_FLOAT false 0 0)
  (GL45/glUniform2f 1 0.5 0.5)
  (GL45/glUniform2f 2 0.2 0.5)
  (GL45/glDrawArrays GL45/GL_TRIANGLE_FAN 0 4)
  (GL45/glDisableVertexAttribArray 0))

(defn main-loop [window]
  (let [histogram (Histogram. 1 (sec->ns 1) 3)
        program-id (create-shaders)]
    (loop [frame-t0 nil]
      (draw window program-id)
      (when frame-t0
        (.recordValue histogram (- (System/nanoTime) frame-t0)))
      (GLFW/glfwSwapBuffers window)
      (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT  GL45/GL_DEPTH_BUFFER_BIT))
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

  (future (run 200 100))

  )
