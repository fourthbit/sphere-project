#include <string.h>
#include <stdio.h>

#define ___VERSION 407002

#include "gambit.h"

#define LINKER ____20_linkfile__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;


//void scheme_main();


void SDL_main(int argc, char** argv)
{
  printf("SchemeSpheres", "Setting up Gambit...");
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
	setup_params.linker = LINKER;
  setup_params.debug_settings = debug_settings;
	
	___setup(&setup_params);

  printf("SchemeSpheres", "Running (main)...");
  //scheme_main();
}
