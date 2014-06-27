(define-cond-expand-feature compile-to-c)
(define-cond-expand-feature debug)
(define-cond-expand-feature android)
(define-cond-expand-feature mobile)
(include "/usr/local/Gambit-C/spheres/core/src/ffi-prelude.scm")
(c-declare "#include \"GLES2/gl2.h\"")
(c-define-type GLbitfield unsigned-int)
(c-define-type GLvoid void)
(c-define-type GLvoid* (pointer void #f))
(c-define-type GLvoid** (pointer GLvoid* #f))
(c-define-type GLbyte signed-char)
(c-define-type GLshort short)
(c-define-type GLint int)
(c-define-type GLubyte unsigned-char)
(c-define-type GLushort unsigned-short)
(c-define-type GLuint unsigned-int)
(c-define-type GLsizei int)
(c-define-type GLintptr int)
(c-define-type GLsizeiptr int)
(c-define-type GLfloat float)
(c-define-type GLclampf float)
(c-define-type GLclampd double)
(c-define-type GLenum unsigned-int32)
(c-define-type* GLenum)
(c-define-sizeof GLenum)
(c-define-array GLenum scheme-vector: u32)
(c-define-type GLboolean unsigned-int8)
(c-define-type* GLboolean)
(c-define-sizeof GLboolean)
(c-define-array GLboolean scheme-vector: u8)
(c-define-type GLchar char)
(c-define-type GLchar** nonnull-char-string-list)
(c-define-type* GLchar)
(c-define-sizeof GLchar)
(c-define-array GLchar scheme-vector: s8)
(c-define-type* GLbyte)
(c-define-sizeof GLbyte)
(c-define-array GLbyte scheme-vector: s8)
(c-define-type* GLubyte)
(c-define-sizeof GLubyte)
(c-define-array GLubyte scheme-vector: u8)
(c-define-type* GLshort)
(c-define-sizeof GLshort)
(c-define-array GLshort scheme-vector: s16)
(c-define-type* GLushort)
(c-define-sizeof GLushort)
(c-define-array GLushort scheme-vector: u16)
(c-define-type* GLint)
(c-define-sizeof GLint)
(c-define-array GLint scheme-vector: s32)
(c-define-type* GLuint)
(c-define-sizeof GLuint)
(c-define-array GLuint scheme-vector: u32)
(c-define-type* GLsizei)
(c-define-sizeof GLsizei)
(c-define-array GLsizei scheme-vector: s64)
(c-define-type* GLfloat)
(c-define-sizeof GLfloat)
(c-define-array GLfloat scheme-vector: f32)
(c-define-constants
 GL_ES_VERSION_2_0
 GL_DEPTH_BUFFER_BIT
 GL_STENCIL_BUFFER_BIT
 GL_COLOR_BUFFER_BIT
 GL_FALSE
 GL_TRUE
 GL_POINTS
 GL_LINES
 GL_LINE_LOOP
 GL_LINE_STRIP
 GL_TRIANGLES
 GL_TRIANGLE_STRIP
 GL_TRIANGLE_FAN
 GL_ZERO
 GL_ONE
 GL_SRC_COLOR
 GL_ONE_MINUS_SRC_COLOR
 GL_SRC_ALPHA
 GL_ONE_MINUS_SRC_ALPHA
 GL_DST_ALPHA
 GL_ONE_MINUS_DST_ALPHA
 GL_DST_COLOR
 GL_ONE_MINUS_DST_COLOR
 GL_SRC_ALPHA_SATURATE
 GL_FUNC_ADD
 GL_BLEND_EQUATION
 GL_BLEND_EQUATION_RGB
 GL_BLEND_EQUATION_ALPHA
 GL_FUNC_SUBTRACT
 GL_FUNC_REVERSE_SUBTRACT
 GL_BLEND_DST_RGB
 GL_BLEND_SRC_RGB
 GL_BLEND_DST_ALPHA
 GL_BLEND_SRC_ALPHA
 GL_CONSTANT_COLOR
 GL_ONE_MINUS_CONSTANT_COLOR
 GL_CONSTANT_ALPHA
 GL_ONE_MINUS_CONSTANT_ALPHA
 GL_BLEND_COLOR
 GL_ARRAY_BUFFER
 GL_ELEMENT_ARRAY_BUFFER
 GL_ARRAY_BUFFER_BINDING
 GL_ELEMENT_ARRAY_BUFFER_BINDING
 GL_STREAM_DRAW
 GL_STATIC_DRAW
 GL_DYNAMIC_DRAW
 GL_BUFFER_SIZE
 GL_BUFFER_USAGE
 GL_CURRENT_VERTEX_ATTRIB
 GL_FRONT
 GL_BACK
 GL_FRONT_AND_BACK
 GL_TEXTURE_2D
 GL_CULL_FACE
 GL_BLEND
 GL_DITHER
 GL_STENCIL_TEST
 GL_DEPTH_TEST
 GL_SCISSOR_TEST
 GL_POLYGON_OFFSET_FILL
 GL_SAMPLE_ALPHA_TO_COVERAGE
 GL_SAMPLE_COVERAGE
 GL_NO_ERROR
 GL_INVALID_ENUM
 GL_INVALID_VALUE
 GL_INVALID_OPERATION
 GL_OUT_OF_MEMORY
 GL_CW
 GL_CCW
 GL_LINE_WIDTH
 GL_ALIASED_POINT_SIZE_RANGE
 GL_ALIASED_LINE_WIDTH_RANGE
 GL_CULL_FACE_MODE
 GL_FRONT_FACE
 GL_DEPTH_RANGE
 GL_DEPTH_WRITEMASK
 GL_DEPTH_CLEAR_VALUE
 GL_DEPTH_FUNC
 GL_STENCIL_CLEAR_VALUE
 GL_STENCIL_FUNC
 GL_STENCIL_FAIL
 GL_STENCIL_PASS_DEPTH_FAIL
 GL_STENCIL_PASS_DEPTH_PASS
 GL_STENCIL_REF
 GL_STENCIL_VALUE_MASK
 GL_STENCIL_WRITEMASK
 GL_STENCIL_BACK_FUNC
 GL_STENCIL_BACK_FAIL
 GL_STENCIL_BACK_PASS_DEPTH_FAIL
 GL_STENCIL_BACK_PASS_DEPTH_PASS
 GL_STENCIL_BACK_REF
 GL_STENCIL_BACK_VALUE_MASK
 GL_STENCIL_BACK_WRITEMASK
 GL_VIEWPORT
 GL_SCISSOR_BOX
 GL_COLOR_CLEAR_VALUE
 GL_COLOR_WRITEMASK
 GL_UNPACK_ALIGNMENT
 GL_PACK_ALIGNMENT
 GL_MAX_TEXTURE_SIZE
 GL_MAX_VIEWPORT_DIMS
 GL_SUBPIXEL_BITS
 GL_RED_BITS
 GL_GREEN_BITS
 GL_BLUE_BITS
 GL_ALPHA_BITS
 GL_DEPTH_BITS
 GL_STENCIL_BITS
 GL_POLYGON_OFFSET_UNITS
 GL_POLYGON_OFFSET_FACTOR
 GL_TEXTURE_BINDING_2D
 GL_SAMPLE_BUFFERS
 GL_SAMPLES
 GL_SAMPLE_COVERAGE_VALUE
 GL_SAMPLE_COVERAGE_INVERT
 GL_NUM_COMPRESSED_TEXTURE_FORMATS
 GL_COMPRESSED_TEXTURE_FORMATS
 GL_DONT_CARE
 GL_FASTEST
 GL_NICEST
 GL_GENERATE_MIPMAP_HINT
 GL_BYTE
 GL_UNSIGNED_BYTE
 GL_SHORT
 GL_UNSIGNED_SHORT
 GL_INT
 GL_UNSIGNED_INT
 GL_FLOAT
 GL_FIXED
 GL_DEPTH_COMPONENT
 GL_ALPHA
 GL_RGB
 GL_RGBA
 GL_LUMINANCE
 GL_LUMINANCE_ALPHA
 GL_UNSIGNED_SHORT_4_4_4_4
 GL_UNSIGNED_SHORT_5_5_5_1
 GL_UNSIGNED_SHORT_5_6_5
 GL_FRAGMENT_SHADER
 GL_VERTEX_SHADER
 GL_MAX_VERTEX_ATTRIBS
 GL_MAX_VERTEX_UNIFORM_VECTORS
 GL_MAX_VARYING_VECTORS
 GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
 GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
 GL_MAX_TEXTURE_IMAGE_UNITS
 GL_MAX_FRAGMENT_UNIFORM_VECTORS
 GL_SHADER_TYPE
 GL_DELETE_STATUS
 GL_LINK_STATUS
 GL_VALIDATE_STATUS
 GL_ATTACHED_SHADERS
 GL_ACTIVE_UNIFORMS
 GL_ACTIVE_UNIFORM_MAX_LENGTH
 GL_ACTIVE_ATTRIBUTES
 GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
 GL_SHADING_LANGUAGE_VERSION
 GL_CURRENT_PROGRAM
 GL_NEVER
 GL_LESS
 GL_EQUAL
 GL_LEQUAL
 GL_GREATER
 GL_NOTEQUAL
 GL_GEQUAL
 GL_ALWAYS
 GL_KEEP
 GL_REPLACE
 GL_INCR
 GL_DECR
 GL_INVERT
 GL_INCR_WRAP
 GL_DECR_WRAP
 GL_VENDOR
 GL_RENDERER
 GL_VERSION
 GL_EXTENSIONS
 GL_NEAREST
 GL_LINEAR
 GL_NEAREST_MIPMAP_NEAREST
 GL_LINEAR_MIPMAP_NEAREST
 GL_NEAREST_MIPMAP_LINEAR
 GL_LINEAR_MIPMAP_LINEAR
 GL_TEXTURE_MAG_FILTER
 GL_TEXTURE_MIN_FILTER
 GL_TEXTURE_WRAP_S
 GL_TEXTURE_WRAP_T
 GL_TEXTURE
 GL_TEXTURE_CUBE_MAP
 GL_TEXTURE_BINDING_CUBE_MAP
 GL_TEXTURE_CUBE_MAP_POSITIVE_X
 GL_TEXTURE_CUBE_MAP_NEGATIVE_X
 GL_TEXTURE_CUBE_MAP_POSITIVE_Y
 GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
 GL_TEXTURE_CUBE_MAP_POSITIVE_Z
 GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
 GL_MAX_CUBE_MAP_TEXTURE_SIZE
 GL_TEXTURE0
 GL_TEXTURE1
 GL_TEXTURE2
 GL_TEXTURE3
 GL_TEXTURE4
 GL_TEXTURE5
 GL_TEXTURE6
 GL_TEXTURE7
 GL_TEXTURE8
 GL_TEXTURE9
 GL_TEXTURE10
 GL_TEXTURE11
 GL_TEXTURE12
 GL_TEXTURE13
 GL_TEXTURE14
 GL_TEXTURE15
 GL_TEXTURE16
 GL_TEXTURE17
 GL_TEXTURE18
 GL_TEXTURE19
 GL_TEXTURE20
 GL_TEXTURE21
 GL_TEXTURE22
 GL_TEXTURE23
 GL_TEXTURE24
 GL_TEXTURE25
 GL_TEXTURE26
 GL_TEXTURE27
 GL_TEXTURE28
 GL_TEXTURE29
 GL_TEXTURE30
 GL_TEXTURE31
 GL_ACTIVE_TEXTURE
 GL_REPEAT
 GL_CLAMP_TO_EDGE
 GL_MIRRORED_REPEAT
 GL_FLOAT_VEC2
 GL_FLOAT_VEC3
 GL_FLOAT_VEC4
 GL_INT_VEC2
 GL_INT_VEC3
 GL_INT_VEC4
 GL_BOOL
 GL_BOOL_VEC2
 GL_BOOL_VEC3
 GL_BOOL_VEC4
 GL_FLOAT_MAT2
 GL_FLOAT_MAT3
 GL_FLOAT_MAT4
 GL_SAMPLER_2D
 GL_SAMPLER_CUBE
 GL_VERTEX_ATTRIB_ARRAY_ENABLED
 GL_VERTEX_ATTRIB_ARRAY_SIZE
 GL_VERTEX_ATTRIB_ARRAY_STRIDE
 GL_VERTEX_ATTRIB_ARRAY_TYPE
 GL_VERTEX_ATTRIB_ARRAY_NORMALIZED
 GL_VERTEX_ATTRIB_ARRAY_POINTER
 GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
 GL_IMPLEMENTATION_COLOR_READ_TYPE
 GL_IMPLEMENTATION_COLOR_READ_FORMAT
 GL_COMPILE_STATUS
 GL_INFO_LOG_LENGTH
 GL_SHADER_SOURCE_LENGTH
 GL_SHADER_COMPILER
 GL_SHADER_BINARY_FORMATS
 GL_NUM_SHADER_BINARY_FORMATS
 GL_LOW_FLOAT
 GL_MEDIUM_FLOAT
 GL_HIGH_FLOAT
 GL_LOW_INT
 GL_MEDIUM_INT
 GL_HIGH_INT
 GL_FRAMEBUFFER
 GL_RENDERBUFFER
 GL_RGBA4
 GL_RGB5_A1
 GL_RGB565
 GL_DEPTH_COMPONENT16
 GL_STENCIL_INDEX
 GL_STENCIL_INDEX8
 GL_RENDERBUFFER_WIDTH
 GL_RENDERBUFFER_HEIGHT
 GL_RENDERBUFFER_INTERNAL_FORMAT
 GL_RENDERBUFFER_RED_SIZE
 GL_RENDERBUFFER_GREEN_SIZE
 GL_RENDERBUFFER_BLUE_SIZE
 GL_RENDERBUFFER_ALPHA_SIZE
 GL_RENDERBUFFER_DEPTH_SIZE
 GL_RENDERBUFFER_STENCIL_SIZE
 GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
 GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
 GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
 GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
 GL_COLOR_ATTACHMENT0
 GL_DEPTH_ATTACHMENT
 GL_STENCIL_ATTACHMENT
 GL_NONE
 GL_FRAMEBUFFER_COMPLETE
 GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
 GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
 GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
 GL_FRAMEBUFFER_UNSUPPORTED
 GL_FRAMEBUFFER_BINDING
 GL_RENDERBUFFER_BINDING
 GL_MAX_RENDERBUFFER_SIZE
 GL_INVALID_FRAMEBUFFER_OPERATION)
(define glActiveTexture (c-lambda (GLenum) void "glActiveTexture"))
(define glAttachShader (c-lambda (GLuint GLuint) void "glAttachShader"))
(define glBindAttribLocation
  (c-lambda (GLuint GLuint char-string) void "glBindAttribLocation"))
(define glBindBuffer (c-lambda (GLenum GLuint) void "glBindBuffer"))
(define glBindFramebuffer (c-lambda (GLenum GLuint) void "glBindFramebuffer"))
(define glBindRenderbuffer
  (c-lambda (GLenum GLuint) void "glBindRenderbuffer"))
(define glBindTexture (c-lambda (GLenum GLuint) void "glBindTexture"))
(define glBlendColor
  (c-lambda (GLclampf GLclampf GLclampf GLclampf) void "glBlendColor"))
(define glBlendEquation (c-lambda (GLenum) void "glBlendEquation"))
(define glBlendEquationSeparate
  (c-lambda (GLenum GLenum) void "glBlendEquationSeparate"))
(define glBlendFunc (c-lambda (GLenum GLenum) void "glBlendFunc"))
(define glBlendFuncSeparate
  (c-lambda (GLenum GLenum GLenum GLenum) void "glBlendFuncSeparate"))
(define glBufferData
  (c-lambda (GLenum GLsizeiptr GLvoid* GLenum) void "glBufferData"))
(define glBufferSubData
  (c-lambda (GLenum GLintptr GLsizeiptr GLvoid*) void "glBufferSubData"))
(define glCheckFramebufferStatus
  (c-lambda (GLenum) GLenum "glCheckFramebufferStatus"))
(define glClear (c-lambda (GLbitfield) void "glClear"))
(define glClearColor
  (c-lambda (GLclampf GLclampf GLclampf GLclampf) void "glClearColor"))
(define glClearDepthf (c-lambda (GLclampf) void "glClearDepthf"))
(define glClearStencil (c-lambda (GLint) void "glClearStencil"))
(define glColorMask
  (c-lambda (GLboolean GLboolean GLboolean GLboolean) void "glColorMask"))
(define glCompileShader (c-lambda (GLuint) void "glCompileShader"))
(define glCompressedTexImage2D
  (c-lambda
   (GLenum GLint GLenum GLsizei GLsizei GLint GLsizei GLvoid*)
   void
   "glCompressedTexImage2D"))
(define glCompressedTexSubImage2D
  (c-lambda
   (GLenum GLint GLint GLint GLsizei GLsizei GLenum GLsizei GLvoid*)
   void
   "glCompressedTexSubImage2D"))
(define glCopyTexImage2D
  (c-lambda
   (GLenum GLint GLenum GLint GLint GLsizei GLsizei GLint)
   void
   "glCopyTexImage2D"))
(define glCopyTexSubImage2D
  (c-lambda
   (GLenum GLint GLint GLint GLint GLint GLsizei GLsizei)
   void
   "glCopyTexSubImage2D"))
(define glCreateProgram (c-lambda () GLuint "glCreateProgram"))
(define glCreateShader (c-lambda (GLenum) GLuint "glCreateShader"))
(define glCullFace (c-lambda (GLenum) void "glCullFace"))
(define glDeleteBuffers (c-lambda (GLsizei GLuint*) void "glDeleteBuffers"))
(define glDeleteFramebuffers
  (c-lambda (GLsizei GLuint*) void "glDeleteFramebuffers"))
(define glDeleteProgram (c-lambda (GLuint) void "glDeleteProgram"))
(define glDeleteRenderbuffers
  (c-lambda (GLsizei GLuint*) void "glDeleteRenderbuffers"))
(define glDeleteShader (c-lambda (GLuint) void "glDeleteShader"))
(define glDeleteTextures (c-lambda (GLsizei GLuint*) void "glDeleteTextures"))
(define glDepthFunc (c-lambda (GLenum) void "glDepthFunc"))
(define glDepthMask (c-lambda (GLboolean) void "glDepthMask"))
(define glDepthRangef (c-lambda (GLclampf GLclampf) void "glDepthRangef"))
(define glDetachShader (c-lambda (GLuint GLuint) void "glDetachShader"))
(define glDisable (c-lambda (GLenum) void "glDisable"))
(define glDisableVertexAttribArray
  (c-lambda (GLuint) void "glDisableVertexAttribArray"))
(define glDrawArrays (c-lambda (GLenum GLint GLsizei) void "glDrawArrays"))
(define glDrawElements
  (c-lambda (GLenum GLsizei GLenum GLvoid*) void "glDrawElements"))
(define glEnable (c-lambda (GLenum) void "glEnable"))
(define glEnableVertexAttribArray
  (c-lambda (GLuint) void "glEnableVertexAttribArray"))
(define glFinish (c-lambda () void "glFinish"))
(define glFlush (c-lambda () void "glFlush"))
(define glFramebufferRenderbuffer
  (c-lambda (GLenum GLenum GLenum GLuint) void "glFramebufferRenderbuffer"))
(define glFramebufferTexture2D
  (c-lambda (GLenum GLenum GLenum GLuint GLint) void "glFramebufferTexture2D"))
(define glFrontFace (c-lambda (GLenum) void "glFrontFace"))
(define glGenBuffers (c-lambda (GLsizei GLuint*) void "glGenBuffers"))
(define glGenerateMipmap (c-lambda (GLenum) void "glGenerateMipmap"))
(define glGenFramebuffers
  (c-lambda (GLsizei GLuint*) void "glGenFramebuffers"))
(define glGenRenderbuffers
  (c-lambda (GLsizei GLuint*) void "glGenRenderbuffers"))
(define glGenTextures (c-lambda (GLsizei GLuint*) void "glGenTextures"))
(define glGetActiveAttrib
  (c-lambda
   (GLuint GLuint GLsizei GLsizei* GLint* GLenum* GLchar*)
   void
   "glGetActiveAttrib"))
(define glGetActiveUniform
  (c-lambda
   (GLuint GLuint GLsizei GLsizei* GLint* GLenum* GLchar*)
   void
   "glGetActiveUniform"))
(define glGetAttachedShaders
  (c-lambda (GLuint GLsizei GLsizei* GLuint*) void "glGetAttachedShaders"))
(define glGetAttribLocation
  (c-lambda (GLuint char-string) GLint "glGetAttribLocation"))
(define glGetBooleanv (c-lambda (GLenum GLboolean*) void "glGetBooleanv"))
(define glGetBufferParameteriv
  (c-lambda (GLenum GLenum GLint*) void "glGetBufferParameteriv"))
(define glGetError (c-lambda () GLenum "glGetError"))
(define glGetFloatv (c-lambda (GLenum GLfloat*) void "glGetFloatv"))
(define glGetFramebufferAttachmentParameteriv
  (c-lambda
   (GLenum GLenum GLenum GLint*)
   void
   "glGetFramebufferAttachmentParameteriv"))
(define glGetIntegerv (c-lambda (GLenum GLint*) void "glGetIntegerv"))
(define glGetProgramiv (c-lambda (GLuint GLenum GLint*) void "glGetProgramiv"))
(define glGetProgramInfoLog
  (c-lambda (GLuint GLsizei GLsizei* GLchar*) void "glGetProgramInfoLog"))
(define glGetRenderbufferParameteriv
  (c-lambda (GLenum GLenum GLint*) void "glGetRenderbufferParameteriv"))
(define glGetShaderiv (c-lambda (GLuint GLenum GLint*) void "glGetShaderiv"))
(define glGetShaderInfoLog
  (c-lambda (GLuint GLsizei GLsizei* GLchar*) void "glGetShaderInfoLog"))
(define glGetShaderPrecisionFormat
  (c-lambda (GLenum GLenum GLint* GLint*) void "glGetShaderPrecisionFormat"))
(define glGetShaderSource
  (c-lambda (GLuint GLsizei GLsizei* GLchar*) void "glGetShaderSource"))
(define glGetString (c-lambda (GLenum) GLubyte* "glGetString"))
(define glGetTexParameterfv
  (c-lambda (GLenum GLenum GLfloat*) void "glGetTexParameterfv"))
(define glGetTexParameteriv
  (c-lambda (GLenum GLenum GLint*) void "glGetTexParameteriv"))
(define glGetUniformfv
  (c-lambda (GLuint GLint GLfloat*) void "glGetUniformfv"))
(define glGetUniformiv (c-lambda (GLuint GLint GLint*) void "glGetUniformiv"))
(define glGetUniformLocation
  (c-lambda (GLuint char-string) GLint "glGetUniformLocation"))
(define glGetVertexAttribfv
  (c-lambda (GLuint GLenum GLfloat*) void "glGetVertexAttribfv"))
(define glGetVertexAttribiv
  (c-lambda (GLuint GLenum GLint*) void "glGetVertexAttribiv"))
(define glGetVertexAttribPointerv
  (c-lambda (GLuint GLenum GLvoid**) void "glGetVertexAttribPointerv"))
(define glHint (c-lambda (GLenum GLenum) void "glHint"))
(define glIsBuffer (c-lambda (GLuint) GLboolean "glIsBuffer"))
(define glIsEnabled (c-lambda (GLenum) GLboolean "glIsEnabled"))
(define glIsFramebuffer (c-lambda (GLuint) GLboolean "glIsFramebuffer"))
(define glIsProgram (c-lambda (GLuint) GLboolean "glIsProgram"))
(define glIsRenderbuffer (c-lambda (GLuint) GLboolean "glIsRenderbuffer"))
(define glIsShader (c-lambda (GLuint) GLboolean "glIsShader"))
(define glIsTexture (c-lambda (GLuint) GLboolean "glIsTexture"))
(define glLineWidth (c-lambda (GLfloat) void "glLineWidth"))
(define glLinkProgram (c-lambda (GLuint) void "glLinkProgram"))
(define glPixelStorei (c-lambda (GLenum GLint) void "glPixelStorei"))
(define glPolygonOffset (c-lambda (GLfloat GLfloat) void "glPolygonOffset"))
(define glReadPixels
  (c-lambda
   (GLint GLint GLsizei GLsizei GLenum GLenum GLvoid*)
   void
   "glReadPixels"))
(define glReleaseShaderCompiler (c-lambda () void "glReleaseShaderCompiler"))
(define glRenderbufferStorage
  (c-lambda (GLenum GLenum GLsizei GLsizei) void "glRenderbufferStorage"))
(define glSampleCoverage
  (c-lambda (GLclampf GLboolean) void "glSampleCoverage"))
(define glScissor (c-lambda (GLint GLint GLsizei GLsizei) void "glScissor"))
(define glShaderBinary
  (c-lambda (GLsizei GLuint* GLenum GLvoid* GLsizei) void "glShaderBinary"))
(define glShaderSource
  (c-lambda (GLuint GLsizei GLchar** GLint*) void "glShaderSource"))
(define glStencilFunc (c-lambda (GLenum GLint GLuint) void "glStencilFunc"))
(define glStencilFuncSeparate
  (c-lambda (GLenum GLenum GLint GLuint) void "glStencilFuncSeparate"))
(define glStencilMask (c-lambda (GLuint) void "glStencilMask"))
(define glStencilMaskSeparate
  (c-lambda (GLenum GLuint) void "glStencilMaskSeparate"))
(define glStencilOp (c-lambda (GLenum GLenum GLenum) void "glStencilOp"))
(define glStencilOpSeparate
  (c-lambda (GLenum GLenum GLenum GLenum) void "glStencilOpSeparate"))
(define glTexImage2D
  (c-lambda
   (GLenum GLint GLint GLsizei GLsizei GLint GLenum GLenum GLvoid*)
   void
   "glTexImage2D"))
(define glTexParameterf
  (c-lambda (GLenum GLenum GLfloat) void "glTexParameterf"))
(define glTexParameterfv
  (c-lambda (GLenum GLenum GLfloat*) void "glTexParameterfv"))
(define glTexParameteri
  (c-lambda (GLenum GLenum GLint) void "glTexParameteri"))
(define glTexParameteriv
  (c-lambda (GLenum GLenum GLint*) void "glTexParameteriv"))
(define glTexSubImage2D
  (c-lambda
   (GLenum GLint GLint GLint GLsizei GLsizei GLenum GLenum GLvoid*)
   void
   "glTexSubImage2D"))
(define glUniform1f (c-lambda (GLint GLfloat) void "glUniform1f"))
(define glUniform1fv (c-lambda (GLint GLsizei GLfloat*) void "glUniform1fv"))
(define glUniform1i (c-lambda (GLint GLint) void "glUniform1i"))
(define glUniform1iv (c-lambda (GLint GLsizei GLint*) void "glUniform1iv"))
(define glUniform2f (c-lambda (GLint GLfloat GLfloat) void "glUniform2f"))
(define glUniform2fv (c-lambda (GLint GLsizei GLfloat*) void "glUniform2fv"))
(define glUniform2i (c-lambda (GLint GLint GLint) void "glUniform2i"))
(define glUniform2iv (c-lambda (GLint GLsizei GLint*) void "glUniform2iv"))
(define glUniform3f
  (c-lambda (GLint GLfloat GLfloat GLfloat) void "glUniform3f"))
(define glUniform3fv (c-lambda (GLint GLsizei GLfloat*) void "glUniform3fv"))
(define glUniform3i (c-lambda (GLint GLint GLint GLint) void "glUniform3i"))
(define glUniform3iv (c-lambda (GLint GLsizei GLint*) void "glUniform3iv"))
(define glUniform4f
  (c-lambda (GLint GLfloat GLfloat GLfloat GLfloat) void "glUniform4f"))
(define glUniform4fv (c-lambda (GLint GLsizei GLfloat*) void "glUniform4fv"))
(define glUniform4i
  (c-lambda (GLint GLint GLint GLint GLint) void "glUniform4i"))
(define glUniform4iv (c-lambda (GLint GLsizei GLint*) void "glUniform4iv"))
(define glUniformMatrix2fv
  (c-lambda (GLint GLsizei GLboolean GLfloat*) void "glUniformMatrix2fv"))
(define glUniformMatrix3fv
  (c-lambda (GLint GLsizei GLboolean GLfloat*) void "glUniformMatrix3fv"))
(define glUniformMatrix4fv
  (c-lambda (GLint GLsizei GLboolean GLfloat*) void "glUniformMatrix4fv"))
(define glUseProgram (c-lambda (GLuint) void "glUseProgram"))
(define glValidateProgram (c-lambda (GLuint) void "glValidateProgram"))
(define glVertexAttrib1f (c-lambda (GLuint GLfloat) void "glVertexAttrib1f"))
(define glVertexAttrib1fv
  (c-lambda (GLuint GLfloat*) void "glVertexAttrib1fv"))
(define glVertexAttrib2f
  (c-lambda (GLuint GLfloat GLfloat) void "glVertexAttrib2f"))
(define glVertexAttrib2fv
  (c-lambda (GLuint GLfloat*) void "glVertexAttrib2fv"))
(define glVertexAttrib3f
  (c-lambda (GLuint GLfloat GLfloat GLfloat) void "glVertexAttrib3f"))
(define glVertexAttrib3fv
  (c-lambda (GLuint GLfloat*) void "glVertexAttrib3fv"))
(define glVertexAttrib4f
  (c-lambda (GLuint GLfloat GLfloat GLfloat GLfloat) void "glVertexAttrib4f"))
(define glVertexAttrib4fv
  (c-lambda (GLuint GLfloat*) void "glVertexAttrib4fv"))
(define glVertexAttribPointer
  (c-lambda
   (GLuint GLint GLenum GLboolean GLsizei GLvoid*)
   void
   "glVertexAttribPointer"))
(define glViewport (c-lambda (GLint GLint GLsizei GLsizei) void "glViewport"))
(define (matrix->GLfloat* mat)
  (let* ((rows (vector-length mat))
         (columns (vector-length (vector-ref mat 0)))
         (max-i (fx- rows 1))
         (max-j (fx- columns 1))
         (buf-size (fx* 2 rows columns))
         (buf (alloc-GLfloat* buf-size)))
    (let loop ((i 0) (i-offset 0) (j 0))
      (GLfloat*-set! buf (fx+ i-offset j) (vector-ref (vector-ref mat j) i))
      (if (< j max-j)
          (loop i i-offset (fx+ 1 j))
          (if (< i max-i) (loop (fx+ 1 i) (fx+ i-offset columns) 0) buf)))))
