! Copyright (C) 2018 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: ;
IN: ci.clang

TUPLE: compiler
    warnings
    optimize
    dash-c
    fflags
    cpp
    arch

    macosx-version-min
    frameworks
    link-libraries

    ! libs
    dynamiclib?
    single_module
    current_version
    compatibility_version
    

    defines
    outfile ;