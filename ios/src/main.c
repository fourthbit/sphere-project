#define ___VERSION 407002

#include "gambit.h"

#define SCHEME_LIBRARY_LINKER ____20_linkfile__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;



#include <string.h>
#include <stdio.h>



void ___run_gambit()
{
    printf("%s", "Setting up Gambit...\n");
	// Taken from gambit, lib/main.c.
	int debug_settings = ___DEBUG_SETTINGS_INITIAL;
    
    // -:d- (force repl io to be stdin/stdout since terminal isn't
    // -attached)
    debug_settings =
    (debug_settings
     & ~___DEBUG_SETTINGS_REPL_MASK)
    | (___DEBUG_SETTINGS_REPL_STDIO
       << ___DEBUG_SETTINGS_REPL_SHIFT);
    // -:da
    debug_settings =
    (debug_settings
     & ~___DEBUG_SETTINGS_UNCAUGHT_MASK)
    | (___DEBUG_SETTINGS_UNCAUGHT_ALL
       << ___DEBUG_SETTINGS_UNCAUGHT_SHIFT);
    // -:dr
    debug_settings =
    (debug_settings
     & ~___DEBUG_SETTINGS_ERROR_MASK)
    | (___DEBUG_SETTINGS_ERROR_REPL
       << ___DEBUG_SETTINGS_ERROR_SHIFT);
    
    ___setup_params_reset (&setup_params);
	setup_params.version = ___VERSION;
	setup_params.linker = SCHEME_LIBRARY_LINKER;
    setup_params.debug_settings = debug_settings;
	
    printf("%s", "Running Gambit...\n");
    
	___setup(&setup_params);
    
    printf("%s", "Cleaning up Gambit...\n");
    ___cleanup();
}

/// XXX
//#include "png_image.h"
#include <OpenGLES/ES2/gl.h>
#include <OpenGLES/ES2/glext.h>

#include "SDL.h"
#include "SDL_image.h"

#include <assert.h>


/// XXX


//GLuint load_texture(
//                    const GLsizei width, const GLsizei height,
//                    const GLenum type, const GLvoid* pixels);



#define LOGGING_ON 1
#define DEBUG_INFO printf

static void log_v_fixed_length(const GLchar* source, const GLint length)
{
    if (LOGGING_ON) {
        char log_buffer[length + 1];
        memcpy(log_buffer, source, length);
        log_buffer[length] = '\0';
        
        DEBUG_INFO("%s\n",log_buffer);
    }
}

static void log_shader_info_log(GLuint shader_object_id)
{
    if (LOGGING_ON) {
        GLint log_length;
        glGetShaderiv(shader_object_id, GL_INFO_LOG_LENGTH, &log_length);
        GLchar log_buffer[log_length];
        glGetShaderInfoLog(shader_object_id, log_length, NULL, log_buffer);
        
        DEBUG_INFO("%s\n",log_buffer);
    }
}

static void log_program_info_log(GLuint program_object_id)
{
    if (LOGGING_ON) {
        GLint log_length;
        glGetProgramiv(program_object_id, GL_INFO_LOG_LENGTH, &log_length);
        GLchar log_buffer[log_length];
        glGetProgramInfoLog(program_object_id, log_length, NULL, log_buffer);
        
        DEBUG_INFO("%s\n",log_buffer);
    }
}

GLuint compile_shader(const GLenum type, const GLchar* source, const GLint length) {
    assert(source != NULL);
    GLuint shader_object_id = glCreateShader(type);
    GLint compile_status;
    
    assert(shader_object_id != 0);
    
    glShaderSource(shader_object_id, 1, (const GLchar **)&source, &length);
    glCompileShader(shader_object_id);
    glGetShaderiv(shader_object_id, GL_COMPILE_STATUS, &compile_status);
    
    if (LOGGING_ON)
    {
        DEBUG_INFO("Results of compiling shader source:\n");
        log_v_fixed_length(source, length);
        log_shader_info_log(shader_object_id);
    }
    
    assert(compile_status != 0);
    
    return shader_object_id;
}

GLuint link_program(const GLuint vertex_shader, const GLuint fragment_shader)
{
    GLuint program_object_id = glCreateProgram();
    GLint link_status;
    
    assert(program_object_id != 0);
    
    glAttachShader(program_object_id, vertex_shader);
    glAttachShader(program_object_id, fragment_shader);
    glLinkProgram(program_object_id);
    glGetProgramiv(program_object_id, GL_LINK_STATUS, &link_status);
    
    if (LOGGING_ON)
    {
        DEBUG_INFO("Results of linking program:\n");
        log_program_info_log(program_object_id);
    }
    
    assert(link_status != 0);
    
    return program_object_id;
}

GLuint build_program(const GLchar * vertex_shader_source, const GLint vertex_shader_source_length,
                     const GLchar * fragment_shader_source, const GLint fragment_shader_source_length)
{
    assert(vertex_shader_source != NULL);
    assert(fragment_shader_source != NULL);
    
    GLuint vertex_shader = compile_shader(GL_VERTEX_SHADER, vertex_shader_source, vertex_shader_source_length);
    GLuint fragment_shader = compile_shader(GL_FRAGMENT_SHADER, fragment_shader_source, fragment_shader_source_length);
    return link_program(vertex_shader, fragment_shader);
}


GLint validate_program(const GLuint program)
{
    if (LOGGING_ON) // XXX LOGGING
    {
        int validate_status;
        glValidateProgram(program);
        glGetProgramiv(program, GL_VALIDATE_STATUS, &validate_status);
        //DEBUG_LOG_PRINT_D(TAG, "Results of validating program: %d", validate_status);
        printf("Results of validating program:\n %d\n", validate_status);
        //
        log_program_info_log(program);
        return validate_status;
    }
    return 0;
}

GLuint build_program_from_assets(const char* vertex_shader_path, const char* fragment_shader_path)
{
    assert(vertex_shader_path != NULL);
    assert(fragment_shader_path != NULL);
    GLchar* vertex_shader_data;
    size_t vertex_shader_size;
    GLchar* fragment_shader_data;
    size_t fragment_shader_size;
    SDL_RWops *vertex_shader_file = SDL_RWFromFile(vertex_shader_path, "rb");
    if(vertex_shader_file != NULL) {
        vertex_shader_size = vertex_shader_file->size(vertex_shader_file);
        vertex_shader_data = malloc(vertex_shader_size);
        SDL_RWread(vertex_shader_file, vertex_shader_data, vertex_shader_size, 1);
        SDL_RWclose(vertex_shader_file);
    } else {
        printf("Error loading vertex shader\n");
        return 0;
    }
    SDL_RWops *fragment_shader_file = SDL_RWFromFile(fragment_shader_path, "rb");
    if(fragment_shader_file != NULL) {
        fragment_shader_size = fragment_shader_file->size(fragment_shader_file);
        fragment_shader_data = malloc(fragment_shader_size);
        SDL_RWread(fragment_shader_file, fragment_shader_data, fragment_shader_size, 1);
        SDL_RWclose(fragment_shader_file);
    } else {
        printf("Error loading fragment shader\n");
        return 0;
    }
    const GLuint program_object_id =
        build_program(vertex_shader_data, vertex_shader_size, fragment_shader_data, fragment_shader_size);
    
    free(vertex_shader_data);
    free(fragment_shader_data);

    return program_object_id;
}


GLuint create_vbo(const GLsizeiptr size, const GLvoid* data, const GLenum usage)
{
    assert(data != NULL);
    GLuint vbo_object;
    glGenBuffers(1, &vbo_object);
    assert(vbo_object != 0);
    
    glBindBuffer(GL_ARRAY_BUFFER, vbo_object);
    glBufferData(GL_ARRAY_BUFFER, size, data, usage);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    
    return vbo_object;
}





//////////////////////////

/*

GLuint load_texture(const GLsizei width, const GLsizei height,
                    const GLenum type, const GLvoid* pixels)
{
    GLuint texture_object_id;
    glGenTextures(1, &texture_object_id);
    assert(texture_object_id != 0);
    
    glBindTexture(GL_TEXTURE_2D, texture_object_id);
    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, type, width, height, 0, type, GL_UNSIGNED_BYTE, pixels);
    glGenerateMipmap(GL_TEXTURE_2D);
    
    glBindTexture(GL_TEXTURE_2D, 0);
    return texture_object_id;
}


GLuint load_png_asset_into_texture(const char* relative_path) {
    assert(relative_path != NULL);
    
    SDL_RWops *png_file = SDL_RWFromFile(relative_path, "rb");
    if(png_file != NULL) {
        // Read PNG data
        size_t png_file_size = png_file->size(png_file);
        //printf("And the size is... %zu\n", png_file_size);
        unsigned char* png_data = malloc(png_file_size);
        SDL_RWread(png_file, png_data, png_file_size, 1);
        SDL_RWclose(png_file);
        // Convert PNG to raw
        const raw_image_data_t raw_image_data = get_raw_image_data_from_png(png_data, png_file_size);
        
        const GLuint texture_object_id = load_texture(raw_image_data.width, raw_image_data.height,
                                                      raw_image_data.gl_color_format, raw_image_data.data);
        // Free data
        free(png_data);
        release_raw_image_data( &raw_image_data );
        printf("successful loading\n");
        return texture_object_id;
    } else {
        printf("error loading file\n");
        return 0;
    }
}
*/


//////////////////////////




int HandleAppEvents(void *userdata, SDL_Event *event)
{
    switch (event->type)
    {
        case SDL_APP_TERMINATING:
            return 0;
        case SDL_APP_LOWMEMORY:
            return 0;
        case SDL_APP_WILLENTERBACKGROUND:
            return 0;
        case SDL_APP_DIDENTERBACKGROUND:
            return 0;
        case SDL_APP_WILLENTERFOREGROUND:
            return 0;
        case SDL_APP_DIDENTERFOREGROUND:
            return 0;
        default:
            return 1;
    }
}

int
randomInt(int min, int max)
{
    return min + rand() % (max - min + 1);
}

float
randomFloat(float min, float max)
{
    return rand() / (float) RAND_MAX *(max - min) + min;
}


#define BUFFER_OFFSET(i) ((char *)NULL + (i))


static GLuint texture;
static GLuint buffer;
static GLuint program;

static GLint a_position_location;
static GLint a_texture_coordinates_location;
static GLint u_texture_unit_location;


void render(void* params) {
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        if (event.type == SDL_QUIT) {
            //done = 1;
        }
    }
    glClearColor(randomFloat(0.0, 1.0), 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    
    ///
    /*
    glUseProgram(program);
    
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture);
    glUniform1i(u_texture_unit_location, 0);
    
    glBindBuffer(GL_ARRAY_BUFFER, buffer);
    glVertexAttribPointer(a_position_location, 2, GL_FLOAT, GL_FALSE,4 * sizeof(GL_FLOAT), BUFFER_OFFSET(0));
    glVertexAttribPointer(a_texture_coordinates_location, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GL_FLOAT), BUFFER_OFFSET(2 * sizeof(GL_FLOAT)));
    glEnableVertexAttribArray(a_position_location);
    glEnableVertexAttribArray(a_texture_coordinates_location);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    
    glBindBuffer(GL_ARRAY_BUFFER, 0);
     */
    ///
    
    SDL_GL_SwapWindow((SDL_Window*)params);
    //SDL_Delay(1);
}


int SDL_main(int argc, char *argv[])
{

    // position X, Y, texture S, T
    static const float rect[] = {-1.0f, -1.0f, 0.0f, 0.0f,
        -1.0f,  1.0f, 0.0f, 1.0f,
        1.0f, -1.0f, 1.0f, 0.0f,
        1.0f,  1.0f, 1.0f, 1.0f};
    
    printf( "Initializing in SDL\n\n");
    
    SDL_Init(SDL_INIT_EVERYTHING);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
    SDL_Window *win = SDL_CreateWindow(NULL, 0, 0, 0, 0, SDL_WINDOW_OPENGL | SDL_WINDOW_BORDERLESS);
    assert(win);
    SDL_GLContext context = SDL_GL_CreateContext(win);
    assert(context);
    SDL_Log( "%s\n", glGetString(GL_VERSION));
    
    int IMGflags = IMG_INIT_JPG|IMG_INIT_PNG;
    int initted = IMG_Init(IMGflags);
    if(initted & IMGflags != IMGflags) {
        SDL_Log("IMG_Init: Failed to init required jpg and png support!\n");
        SDL_Log("IMG_Init: %s\n", IMG_GetError());
        // handle error
    }
    
    SDL_Surface *image;
    image=IMG_Load("lambda.png");
    if(!image) {
        printf("IMG_Load: %s\n", IMG_GetError());
        // handle error
    } else {
        printf("IMG_Load: LOADED\n");
    }
    
    /*
    printf( "Initializing in PNG loading\n\n");
    
    const GLuint tex = load_png_asset_into_texture("lambda.png");
    
    buffer = create_vbo(sizeof(rect), rect, GL_STATIC_DRAW);
    program = build_program_from_assets("shader.vsh", "shader.fsh");
    
    a_position_location = glGetAttribLocation(program, "a_Position");
    a_texture_coordinates_location = glGetAttribLocation(program, "a_TextureCoordinates");
    u_texture_unit_location = glGetUniformLocation(program, "u_TextureUnit");
*/
     
    // Events
     SDL_SetEventFilter(HandleAppEvents, NULL);
    
    // DRAW
    SDL_iPhoneSetAnimationCallback(win, 1, render, win);


    
    //___run_gambit();
    
    return 0;
}
