(in-package :sdl2-ffi)

(autowrap:c-include '(sdl2-ttf autowrap-spec "SDL2_ttf.h")
                    :function-package :sdl2-ffi.functions
                    :spec-path '(sdl2-ttf autowrap-spec)
                    :exclude-sources ("/usr/include/")
                    :include-sources ("SDL_ttf.h")
		    :sysincludes `,(uiop:split-string (uiop:getenv "EXTRA_INCLUDES") :separator " ")
                    :symbol-exceptions (("SDL_RWops" . "SDL-RWOPS"))
                    :exclude-constants ("^+__")
		    :exclude-definitions ("_inline$"
					  "^_mm_"
					  "^__"
					  "va_list"
					  "_gnuc_va_list")
                    ;;We're mostly dealing with SDL-surface which contains accessors in the main cl-sdl2 library
                    :no-accessors cl:t
		    :release-p cl:t)
