//
//  png_image.h
//  SchemeSpheres
//
//  Created by Alvaro on 7/24/14.
//
//

#ifndef SchemeSpheres_png_image_h
#define SchemeSpheres_png_image_h

#include <OpenGLES/ES2/gl.h>
#include <OpenGLES/ES2/glext.h>



typedef struct {
    const int width;
    const int height;
    const int size;
    const GLenum gl_color_format;
    const void* data;
} raw_image_data_t;

/* Returns the decoded image data, or aborts if there's an error during decoding. */
raw_image_data_t get_raw_image_data_from_png(const void* png_data, const int png_data_size);

void release_raw_image_data(const raw_image_data_t* data);


#endif
