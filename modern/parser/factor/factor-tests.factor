! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: ascii io.encodings.utf8 io.files io.streams.document
kernel modern.parser modern.parser.factor multiline sequences
sets tools.test vocabs.hierarchy vocabs.loader ;
IN: modern.parser.factor.tests

{
    {
        T{ marray
            { texts
                V{
                    T{ document-object { object "{" } }
                    T{ document-object
                        { position
                            T{ document-position { column 2 } }
                        }
                        { object "1" }
                    }
                    T{ document-object
                        { position
                            T{ document-position { column 4 } }
                        }
                        { object "2" }
                    }
                    T{ document-object
                        { position
                            T{ document-position { column 6 } }
                        }
                        { object "3" }
                    }
                    T{ document-object
                        { position
                            T{ document-position { column 8 } }
                        }
                        { object "}" }
                    }
                }
            }
            { elements
                {
                    T{ mnumber { n "1" } }
                    T{ mnumber { n "2" } }
                    T{ mnumber { n "3" } }
                }
            }
        }
    }
} [ "{ 1 2 3 }" parse-source-string ] unit-test


: check-parsed-file ( path -- ? )
    [ utf8 file-contents [ blank? ] trim-tail ]
    [ parse-modern-file second write-parsed-string ] bi sequence= ;

{ t } [ "resource:core/alien/alien.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/alien/strings/strings.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/arrays/arrays.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/assocs/assocs.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/byte-arrays/byte-arrays.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/combinators/combinators.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/compiler/units/units.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/kernel/kernel.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/layouts/layouts.factor" check-parsed-file ] unit-test

{ t } [ "resource:core/lexer/lexer.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/make/make.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/math/math.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/parser/parser.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/parser/notes/notes.factor" check-parsed-file ] unit-test

{ t } [ "resource:core/sequences/sequences.factor" check-parsed-file ] unit-test


{ t } [
    "resource:core" vocabs-in-root
    ! [ vocab? ] filter
    [ vocab-source-path ] map sift
    {
        "resource:core/vocabs/loader/test/a/a.factor"
        "resource:core/vocabs/loader/test/b/b.factor"
        "resource:core/vocabs/loader/test/c/c.factor"
    } diff
    ! [ parse-modern-file ] map
    [ check-parsed-file ] all?
] unit-test

/*
{ } [
    "resource:basis" vocabs-in-root
    [ vocab? ] filter
    [ vocab-source-path ] map sift
    {

    } diff
    [ dup . flush parse-modern-file ] map
] unit-test



    "resource:basis" vocabs-in-root
    ! [ vocab? ] filter
    [ vocab-source-path ] map sift
    {
"resource:basis/specialized-vectors/specialized-vectors.factor"


    } diff [ dup  check-parsed-file ] { } map>assoc
    [ second not ] filter keys [ . ] each
"resource:basis/ascii/ascii.factor"
"resource:basis/bit-vectors/bit-vectors.factor"
"resource:basis/bitstreams/bitstreams.factor"
"resource:basis/boxes/boxes.factor"
"resource:basis/calendar/calendar.factor"
"resource:basis/db/db.factor"
"resource:basis/delegate/delegate.factor"
"resource:basis/editors/editors.factor"
"resource:basis/farkup/farkup.factor"
"resource:basis/generalizations/generalizations.factor"
"resource:core/hashtables/hashtables.factor"
"resource:basis/help/help.factor"
"resource:basis/http/http.factor"
"resource:basis/interval-maps/interval-maps.factor"
"resource:basis/inverse/inverse.factor"
"resource:basis/iokit/iokit.factor"
"resource:basis/lcs/lcs.factor"
"resource:basis/listener/listener.factor"
"resource:basis/logging/logging.factor"
"resource:basis/pack/pack.factor"
"resource:basis/quoted-printable/quoted-printable.factor"
"resource:basis/see/see.factor"
"resource:basis/serialize/serialize.factor"
"resource:basis/suffix-arrays/suffix-arrays.factor"
"resource:basis/system-info/system-info.factor"
"resource:basis/timers/timers.factor"
"resource:basis/validators/validators.factor"
"resource:basis/xml/xml.factor"
"resource:basis/alien/c-types/c-types.factor"
"resource:basis/alien/endian/endian.factor"
"resource:basis/alien/parser/parser.factor"
"resource:basis/alien/prettyprint/prettyprint.factor"
"resource:basis/bootstrap/handbook/handbook.factor"
"resource:basis/cairo/ffi/ffi.factor"
"resource:basis/calendar/model/model.factor"
"resource:basis/channels/examples/examples.factor"
"resource:basis/channels/remote/remote.factor"
"resource:basis/checksums/hmac/hmac.factor"
"resource:basis/checksums/sha/sha.factor"
"resource:basis/cocoa/dialogs/dialogs.factor"
"resource:basis/cocoa/runtime/runtime.factor"
"resource:basis/combinators/smart/smart.factor"
"resource:basis/compiler/cfg/comparisons/comparisons.factor"
"resource:basis/compiler/cfg/dependence/dependence.factor"
"resource:basis/compiler/cfg/scheduling/scheduling.factor"
"resource:basis/compiler/cfg/useless-conditionals/useless-cond..."
"resource:basis/compiler/cfg/builder/alien/alien.factor"
"resource:basis/compiler/cfg/intrinsics/simd/backend/backend.f..."
"resource:basis/compiler/cfg/value-numbering/expressions/expre..."
"resource:basis/compiler/cfg/value-numbering/simd/simd.factor"
"resource:basis/compiler/tree/debugger/debugger.factor"
"resource:basis/compiler/tree/late-optimizations/late-optimiza..."
"resource:basis/compiler/tree/propagation/simple/simple.factor"
"resource:basis/compression/huffman/huffman.factor"
"resource:basis/compression/inflate/inflate.factor"
"resource:basis/compression/run-length/run-length.factor"
"resource:basis/compression/snappy/snappy.factor"
"resource:basis/concurrency/conditions/conditions.factor"
"resource:basis/concurrency/count-downs/count-downs.factor"
"resource:basis/concurrency/exchangers/exchangers.factor"
"resource:basis/concurrency/futures/futures.factor"
"resource:basis/concurrency/locks/locks.factor"
"resource:basis/concurrency/messaging/messaging.factor"
"resource:basis/concurrency/promises/promises.factor"
"resource:basis/concurrency/semaphores/semaphores.factor"
"resource:basis/core-foundation/file-descriptors/file-descript..."
"resource:basis/core-foundation/strings/strings.factor"
"resource:basis/db/queries/queries.factor"
"resource:basis/db/sqlite/sqlite.factor"
"resource:basis/db/errors/postgresql/postgresql.factor"
"resource:basis/db/postgresql/ffi/ffi.factor"
"resource:basis/db/sqlite/lib/lib.factor"
"resource:basis/editors/jedit/jedit.factor"
"resource:basis/editors/macvim/macvim.factor"
"resource:basis/ftp/server/server.factor"
"resource:basis/furnace/actions/actions.factor"
"resource:basis/furnace/auth/auth.factor"
"resource:basis/furnace/db/db.factor"
"resource:basis/furnace/auth/basic/basic.factor"
"resource:basis/furnace/auth/login/login.factor"
"resource:basis/furnace/auth/providers/providers.factor"
"resource:basis/furnace/auth/features/deactivate-user/deactiva..."
"resource:basis/furnace/auth/features/edit-profile/edit-profil..."
"resource:basis/furnace/auth/login/permits/permits.factor"
"resource:basis/furnace/auth/providers/assoc/assoc.factor"
"resource:basis/furnace/auth/providers/null/null.factor"
"resource:basis/game/input/gtk/gtk.factor"
"resource:basis/game/input/iokit/iokit.factor"
"resource:basis/game/input/x11/x11.factor"
"resource:basis/gobject-introspection/ffi/ffi.factor"
"resource:basis/gobject-introspection/loader/loader.factor"
"resource:basis/hash-sets/identity/identity.factor"
"resource:basis/hash-sets/identity/prettyprint/prettyprint.factor"
"resource:basis/hashtables/identity/identity.factor"
"resource:basis/hashtables/identity/mirrors/mirrors.factor"
"resource:basis/hashtables/identity/prettyprint/prettyprint.fa..."
"resource:basis/help/apropos/apropos.factor"
"resource:basis/help/handbook/handbook.factor"
"resource:basis/http/parsers/parsers.factor"
"resource:basis/http/server/cgi/cgi.factor"
"resource:basis/http/server/static/static.factor"
"resource:basis/images/processing/processing.factor"
"resource:basis/images/loader/gdiplus/gdiplus.factor"
"resource:basis/io/servers/servers.factor"
"resource:basis/io/timeouts/timeouts.factor"
"resource:basis/io/encodings/gb18030/gb18030.factor"
"resource:basis/io/launcher/windows/windows.factor"
"resource:basis/io/sockets/windows/windows.factor"
"resource:basis/io/streams/limited/limited.factor"
"resource:basis/logging/analysis/analysis.factor"
"resource:basis/logging/insomniac/insomniac.factor"
"resource:basis/logging/parser/parser.factor"
"resource:basis/logging/server/server.factor"
"resource:basis/math/polynomials/polynomials.factor"
"resource:basis/math/statistics/statistics.factor"
"resource:basis/math/vectors/vectors.factor"
"resource:basis/math/floats/half/half.factor"
"resource:basis/math/vectors/conversion/conversion.factor"
"resource:basis/math/vectors/simd/simd.factor"
"resource:basis/models/arrow/arrow.factor"
"resource:basis/models/delay/delay.factor"
"resource:basis/models/mapping/mapping.factor"
"resource:basis/models/product/product.factor"
"resource:basis/models/range/range.factor"
"resource:basis/opengl/framebuffers/framebuffers.factor"
"resource:basis/opengl/gl/gl.factor"
"resource:basis/opengl/shaders/shaders.factor"
"resource:basis/opengl/gl/extensions/extensions.factor"
"resource:basis/pango/ffi/ffi.factor"
"resource:basis/pango/cairo/ffi/ffi.factor"
"resource:basis/peg/ebnf/ebnf.factor"
"resource:basis/peg/parsers/parsers.factor"
"resource:basis/persistent/heaps/heaps.factor"
"resource:basis/prettyprint/backend/backend.factor"
"resource:basis/random/windows/windows.factor"
"resource:basis/regexp/classes/classes.factor"
"resource:basis/regexp/compiler/compiler.factor"
"resource:basis/regexp/disambiguate/disambiguate.factor"
"resource:basis/regexp/negation/negation.factor"
"resource:basis/regexp/nfa/nfa.factor"
"resource:basis/regexp/parser/parser.factor"
"resource:basis/sequences/cords/cords.factor"
"resource:basis/smtp/server/server.factor"
"resource:basis/suffix-arrays/words/words.factor"
"resource:basis/system-info/windows/windows.factor"
"resource:basis/tools/continuations/continuations.factor"
"resource:basis/tools/coverage/coverage.factor"
"resource:basis/tools/deploy/deploy.factor"
"resource:basis/tools/deprecation/deprecation.factor"
"resource:basis/tools/disassembler/disassembler.factor"
"resource:basis/tools/files/files.factor"
"resource:basis/tools/threads/threads.factor"
"resource:basis/tools/walker/walker.factor"
"resource:basis/tools/deploy/shaker/shaker.factor"
"resource:basis/tools/deploy/windows/windows.factor"
"resource:basis/tools/deploy/test/1/1.factor"
"resource:basis/tools/deploy/test/17/17.factor"
"resource:basis/tools/deploy/test/2/2.factor"
"resource:basis/tools/deploy/test/3/3.factor"
"resource:basis/tools/walker/debug/debug.factor"
"resource:basis/ui/baseline-alignment/baseline-alignment.factor"
"resource:basis/ui/commands/commands.factor"
"resource:basis/ui/debugger/debugger.factor"
"resource:basis/ui/pixel-formats/pixel-formats.factor"
"resource:basis/ui/backend/cocoa/cocoa.factor"
"resource:basis/ui/backend/windows/windows.factor"
"resource:basis/ui/gadgets/editors/editors.factor"
"resource:basis/ui/gadgets/glass/glass.factor"
"resource:basis/ui/gadgets/scrollers/scrollers.factor"
"resource:basis/ui/gadgets/slots/slots.factor"
"resource:basis/ui/gadgets/worlds/worlds.factor"
"resource:basis/ui/text/uniscribe/uniscribe.factor"
"resource:basis/ui/tools/deploy/deploy.factor"
"resource:basis/ui/tools/error-list/error-list.factor"
"resource:basis/ui/tools/walker/walker.factor"
"resource:basis/unicode/collation/collation.factor"
"resource:basis/unix/groups/groups.factor"
"resource:basis/unix/types/types.factor"
"resource:basis/unix/users/users.factor"
"resource:basis/unix/linux/inotify/inotify.factor"
"resource:basis/unix/types/linux/linux.factor"
"resource:basis/vocabs/hierarchy/hierarchy.factor"
"resource:basis/windows/advapi32/advapi32.factor"
"resource:basis/windows/directx/directx.factor"
"resource:basis/windows/gdi32/gdi32.factor"
"resource:basis/windows/gdiplus/gdiplus.factor"
"resource:basis/windows/iphlpapi/iphlpapi.factor"
"resource:basis/windows/kernel32/kernel32.factor"
"resource:basis/windows/messages/messages.factor"
"resource:basis/windows/registry/registry.factor"
"resource:basis/windows/shell32/shell32.factor"
"resource:basis/windows/streams/streams.factor"
"resource:basis/windows/types/types.factor"
"resource:basis/windows/user32/user32.factor"
"resource:basis/windows/winmm/winmm.factor"
"resource:basis/windows/com/wrapper/wrapper.factor"
"resource:basis/windows/directx/d2d1/d2d1.factor"
"resource:basis/windows/directx/d2dbasetypes/d2dbasetypes.factor"
"resource:basis/windows/directx/d3d10/d3d10.factor"
"resource:basis/windows/directx/d3d10_1shader/d3d10_1shader.fa..."
"resource:basis/windows/directx/d3d10effect/d3d10effect.factor"
"resource:basis/windows/directx/d3d10misc/d3d10misc.factor"
"resource:basis/windows/directx/d3d9/d3d9.factor"
"resource:basis/windows/directx/d3d9caps/d3d9caps.factor"
"resource:basis/windows/directx/d3d9types/d3d9types.factor"
"resource:basis/windows/directx/d3dcompiler/d3dcompiler.factor"
"resource:basis/windows/directx/d3dx10async/d3dx10async.factor"
"resource:basis/windows/directx/d3dx11async/d3dx11async.factor"
"resource:basis/windows/directx/d3dx9anim/d3dx9anim.factor"
"resource:basis/windows/directx/d3dx9core/d3dx9core.factor"
"resource:basis/windows/directx/d3dx9effect/d3dx9effect.factor"
"resource:basis/windows/directx/d3dx9mesh/d3dx9mesh.factor"
"resource:basis/windows/directx/d3dx9shader/d3dx9shader.factor"
"resource:basis/windows/directx/d3dx9shape/d3dx9shape.factor"
"resource:basis/windows/directx/d3dx9tex/d3dx9tex.factor"
"resource:basis/windows/directx/dinput/dinput.factor"
"resource:basis/windows/directx/dwrite/dwrite.factor"
"resource:basis/windows/directx/dxfile/dxfile.factor"
"resource:basis/windows/directx/dxgi/dxgi.factor"
"resource:basis/windows/directx/xact3/xact3.factor"
"resource:basis/windows/directx/xapo/xapo.factor"
"resource:basis/windows/directx/xaudio2/xaudio2.factor"
"resource:basis/x11/constants/constants.factor"
"resource:basis/x11/xlib/xlib.factor"
"resource:basis/xml/dtd/dtd.factor"
"resource:basis/xml/elements/elements.factor"
"resource:basis/xml/writer/writer.factor"
"resource:basis/xml/syntax/inverse/inverse.factor"
"resource:basis/xmode/marker/marker.factor"
"resource:basis/xmode/code2html/responder/responder.factor"

    

    "resource:extra" vocabs-in-root
    ! [ vocab? ] filter
    [ vocab-source-path ] map sift
    {
"resource:extra/irc/messages/messages.factor"
"resource:extra/yaml/conversion/conversion.factor"

    } diff [ dup flush check-parsed-file ] { } map>assoc
    [ second not ] filter keys [ . ] each
"resource:extra/asn1/asn1.factor"
"resource:extra/backtrack/backtrack.factor"
"resource:extra/balloon-bomber/balloon-bomber.factor"
"resource:extra/boids/boids.factor"
"resource:basis/calendar/calendar.factor"
"resource:extra/codebook/codebook.factor"
"resource:extra/constructors/constructors.factor"
"resource:extra/coroutines/coroutines.factor"
"resource:extra/couchdb/couchdb.factor"
"resource:extra/cursors/cursors.factor"
"resource:extra/decimals/decimals.factor"
"resource:extra/dns/dns.factor"
"resource:extra/dwarf/dwarf.factor"
"resource:extra/ecdsa/ecdsa.factor"
"resource:extra/fastcgi/fastcgi.factor"
"resource:extra/fluids/fluids.factor"
"resource:extra/fullscreen/fullscreen.factor"
"resource:extra/geobytes/geobytes.factor"
"resource:extra/hashcash/hashcash.factor"
"resource:core/hashtables/hashtables.factor"
"resource:basis/help/help.factor"
"resource:extra/id3/id3.factor"
"resource:extra/jamshred/jamshred.factor"
"resource:extra/jvm-summit-talk/jvm-summit-talk.factor"
"resource:extra/libudev/libudev.factor"
"resource:extra/libusb/libusb.factor"
"resource:extra/log-viewer/log-viewer.factor"
"resource:extra/lua/lua.factor"
"resource:extra/lunar-rescue/lunar-rescue.factor"
"resource:extra/macho/macho.factor"
"resource:extra/minneapolis-talk/minneapolis-talk.factor"
"resource:extra/model-viewer/model-viewer.factor"
"resource:extra/morse/morse.factor"
"resource:extra/native-thread-test/native-thread-test.factor"
"resource:extra/nested-comments/nested-comments.factor"
"resource:extra/noise/noise.factor"
"resource:extra/ogg/ogg.factor"
"resource:extra/openal/openal.factor"
"resource:extra/opencl/opencl.factor"
"resource:extra/pair-methods/pair-methods.factor"
"resource:extra/persistency/persistency.factor"
"resource:extra/pong/pong.factor"
"resource:extra/quadtrees/quadtrees.factor"
"resource:extra/robots/robots.factor"
"resource:extra/s3/s3.factor"
"resource:extra/site-watcher/site-watcher.factor"
"resource:core/slots/slots.factor"
"resource:extra/space-invaders/space-invaders.factor"
"resource:extra/spheres/spheres.factor"
"resource:extra/svg/svg.factor"
"resource:extra/tc-lisp-talk/tc-lisp-talk.factor"
"resource:extra/terrain/terrain.factor"
"resource:extra/tetris/tetris.factor"
"resource:extra/trails/trails.factor"
"resource:extra/trees/trees.factor"
"resource:extra/twitter/twitter.factor"
"resource:extra/variables/variables.factor"
"resource:extra/wordtimer/wordtimer.factor"
"resource:extra/zoneinfo/zoneinfo.factor"
"resource:extra/alien/fortran/fortran.factor"
"resource:extra/alien/data/map/map.factor"
"resource:extra/asn1/ldap/ldap.factor"
"resource:extra/audio/aiff/aiff.factor"
"resource:extra/audio/engine/engine.factor"
"resource:extra/benchmark/dispatch5/dispatch5.factor"
"resource:extra/benchmark/fib6/fib6.factor"
"resource:extra/benchmark/nbody/nbody.factor"
"resource:extra/benchmark/nbody-simd/nbody-simd.factor"
"resource:extra/benchmark/nsieve-bits/nsieve-bits.factor"
"resource:extra/benchmark/ring/ring.factor"
"resource:extra/benchmark/spectral-norm-simd/spectral-norm-sim..."
"resource:extra/benchmark/mandel/params/params.factor"
"resource:extra/bitcoin/client/client.factor"
"resource:extra/bson/reader/reader.factor"
"resource:extra/bson/writer/writer.factor"
"resource:extra/bunny/outlined/outlined.factor"
"resource:extra/c/preprocessor/preprocessor.factor"
"resource:extra/calendar/holidays/holidays.factor"
"resource:extra/calendar/holidays/us/us.factor"
"resource:extra/chipmunk/demo/demo.factor"
"resource:extra/combinators/tuple/tuple.factor"
"resource:extra/compiler/graphviz/graphviz.factor"
"resource:extra/compiler/cfg/gvn/expressions/expressions.factor"
"resource:extra/compiler/cfg/gvn/simd/simd.factor"
"resource:extra/cpu/8080/8080.factor"
"resource:extra/cpu/8080/emulator/emulator.factor"
"resource:extra/cpu/8080/test/test.factor"
"resource:extra/crypto/aes/aes.factor"
"resource:extra/crypto/passwd-md5/passwd-md5.factor"
"resource:extra/crypto/rsa/rsa.factor"
"resource:extra/ctags/etags/etags.factor"
"resource:extra/cuda/ffi/ffi.factor"
"resource:extra/cuda/gl/gl.factor"
"resource:extra/cuda/libraries/libraries.factor"
"resource:extra/cuda/ptx/ptx.factor"
"resource:extra/cuda/types/types.factor"
"resource:extra/elf/nm/nm.factor"
"resource:extra/euler/b-rep/b-rep.factor"
"resource:extra/euler/modeling/modeling.factor"
"resource:extra/euler/operators/operators.factor"
"resource:extra/euler/b-rep/subdivision/subdivision.factor"
"resource:extra/euler/b-rep/io/obj/obj.factor"
"resource:extra/game/debug/debug.factor"
"resource:extra/game/debug/tests/tests.factor"
"resource:extra/game/input/demos/key-caps/key-caps.factor"
"resource:extra/game/models/half-edge/half-edge.factor"
"resource:extra/gml/modeling/modeling.factor"
"resource:extra/gml/runtime/runtime.factor"
"resource:extra/gml/ui/ui.factor"
"resource:extra/gml/viewer/viewer.factor"
"resource:extra/gpu/buffers/buffers.factor"
"resource:extra/gpu/framebuffers/framebuffers.factor"
"resource:extra/gpu/render/render.factor"
"resource:extra/gpu/shaders/shaders.factor"
"resource:extra/gpu/state/state.factor"
"resource:extra/gpu/textures/textures.factor"
"resource:extra/gpu/util/util.factor"
"resource:extra/gpu/demos/bunny/bunny.factor"
"resource:extra/gpu/demos/raytrace/raytrace.factor"
"resource:extra/gpu/effects/blur/blur.factor"
"resource:extra/gpu/util/wasd/wasd.factor"
"resource:extra/gtk-samples/hello-world/hello-world.factor"
"resource:extra/gtk-samples/opengl/opengl.factor"
"resource:extra/images/atlas/atlas.factor"
"resource:extra/images/gif/gif.factor"
"resource:extra/images/viewer/viewer.factor"
"resource:extra/io/serial/serial.factor"
"resource:extra/io/encodings/detect/detect.factor"
"resource:extra/jamshred/game/game.factor"
"resource:extra/jamshred/player/player.factor"
"resource:extra/llvm/core/core.factor"
"resource:extra/llvm/types/types.factor"
"resource:extra/math/affine-transforms/affine-transforms.factor"
"resource:extra/math/binpack/binpack.factor"
"resource:extra/math/derivatives/derivatives.factor"
"resource:extra/math/dual/dual.factor"
"resource:extra/math/numerical-integration/numerical-integrati..."
"resource:extra/math/splines/splines.factor"
"resource:basis/math/vectors/vectors.factor"
"resource:extra/math/blas/ffi/ffi.factor"
"resource:extra/math/blas/matrices/matrices.factor"
"resource:extra/math/blas/vectors/vectors.factor"
"resource:extra/math/derivatives/syntax/syntax.factor"
"resource:extra/math/matrices/simd/simd.factor"
"resource:extra/math/splines/viewer/viewer.factor"
"resource:extra/math/vectors/homogeneous/homogeneous.factor"
"resource:extra/models/conditional/conditional.factor"
"resource:extra/models/history/history.factor"
"resource:extra/modern/loader/loader.factor"
"resource:extra/mongodb/benchmark/benchmark.factor"
"resource:extra/mongodb/cmd/cmd.factor"
"resource:extra/mongodb/connection/connection.factor"
"resource:extra/mongodb/driver/driver.factor"
"resource:extra/mongodb/msg/msg.factor"
"resource:extra/mongodb/operations/operations.factor"
"resource:extra/mongodb/tuple/tuple.factor"
"resource:extra/mongodb/tuple/collection/collection.factor"
"resource:extra/mongodb/tuple/persistent/persistent.factor"
"resource:extra/nehe/5/5.factor"
"resource:extra/ogg/theora/theora.factor"
"resource:extra/ogg/vorbis/vorbis.factor"
"resource:extra/openal/example/example.factor"
"resource:extra/opencl/ffi/ffi.factor"
"resource:extra/opengl/demo-support/demo-support.factor"
"resource:extra/opengl/glu/glu.factor"
"resource:extra/parser-combinators/simple/simple.factor"
"resource:extra/peg/expr/expr.factor"
"resource:extra/peg/javascript/parser/parser.factor"
"resource:extra/peg/javascript/tokenizer/tokenizer.factor"
"resource:extra/pop3/server/server.factor"
"resource:extra/project-euler/051/051.factor"
"resource:extra/project-euler/062/062.factor"
"resource:extra/project-euler/081/081.factor"
"resource:extra/project-euler/102/102.factor"
"resource:extra/random/lagged-fibonacci/lagged-fibonacci.factor"
"resource:extra/reports/noise/noise.factor"
"resource:extra/rosetta-code/animate-pendulum/animate-pendulum..."
"resource:extra/rosetta-code/animation/animation.factor"
"resource:extra/rosetta-code/bitmap/bitmap.factor"
"resource:extra/rosetta-code/bitmap-bezier/bitmap-bezier.factor"
"resource:extra/rosetta-code/bitmap-line/bitmap-line.factor"
"resource:extra/rosetta-code/bulls-and-cows/bulls-and-cows.factor"
"resource:extra/rosetta-code/gray-code/gray-code.factor"
"resource:extra/rosetta-code/image-noise/image-noise.factor"
"resource:extra/rosetta-code/knapsack/knapsack.factor"
"resource:extra/rosetta-code/opengl/opengl.factor"
"resource:extra/rosetta-code/pythagorean-triples/pythagorean-t..."
"resource:extra/rosetta-code/top-rank/top-rank.factor"
"resource:extra/rosetta-code/tree-traversal/tree-traversal.factor"
"resource:extra/sequences/n-based/n-based.factor"
"resource:extra/site-watcher/db/db.factor"
"resource:extra/site-watcher/email/email.factor"
"resource:extra/slots/macros/macros.factor"
"resource:extra/slots/syntax/syntax.factor"
"resource:extra/smalltalk/compiler/compiler.factor"
"resource:extra/smalltalk/parser/parser.factor"
"resource:extra/taxes/usa/federal/federal.factor"
"resource:extra/terrain/generation/generation.factor"
"resource:extra/terrain/shaders/shaders.factor"
"resource:extra/tetris/board/board.factor"
"resource:extra/tetris/game/game.factor"
"resource:extra/tetris/tetromino/tetromino.factor"
"resource:extra/time/windows/windows.factor"
"resource:extra/tools/dns/public/public.factor"
"resource:extra/trees/avl/avl.factor"
"resource:extra/trees/splay/splay.factor"
"resource:extra/twitter/prettyprint/prettyprint.factor"
"resource:basis/ui/gadgets/worlds/worlds.factor"
"resource:extra/ui/render/test/test.factor"
"resource:extra/units/constants/constants.factor"
"resource:extra/units/imperial/imperial.factor"
"resource:extra/update/backup/backup.factor"
"resource:extra/vocabs/git/git.factor"
"resource:extra/webapps/fjsc/fjsc.factor"
"resource:extra/webapps/imagebin/imagebin.factor"
"resource:extra/webapps/irc-log/irc-log.factor"
"resource:extra/webapps/todo/todo.factor"
"resource:extra/webapps/wee-url/wee-url.factor"
"resource:extra/webapps/wiki/wiki.factor"
"resource:extra/yaml/config/config.factor"
*/