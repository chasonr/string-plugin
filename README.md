string-plugin
=============

A Clang plugin to assist with modernizing code that writes to unbounded strings, and with using wide strings.

Building may require a few adjustments to the Makefile, according to how LLVM and Clang are installed on your system.  LLVM and Clang are the only dependencies.

Once built, you can load the plugin with these options to clang:

    -Xclang -load -Xclang path-to-plugin/string-plugin.so

You can then enable either of the two plugins in the library with these options to clang:

    -Xclang -add-plugin -Xclang string-plugin
    -Xclang -add-plugin -Xclang format-plugin

Be sure to use -add-plugin, and not -plugin, as -plugin will prevent any object file from being generated.

If string-plugin is enabled, you can accept the default list of unbounded string functions, or define your own list and pass it to the plugin as follows:

    -Xclang -plugin-arg-string-plugin -Xclang -list=functions.txt

See the file string-functions.txt for the default list and a description of the format.  This will find all calls to functions on the list and indicate which ones are writing to arrays, and which to pointers.  The ones that write to arrays can then be quickly converted to a bounded equivalent, such as snprintf instead of printf, and the rest can be converted by providing the output size some other way.

Where writes to arrays are converted, the size should be determined by an expression of the form ```sizeof(x)/sizeof(x[0])```, even if x is an array of char.  This expression may be bundled into a macro.  string-plugin looks for expressions of the form ```sizeof(x)/sizeof(y)``` and raises a warning if x is a pointer.  If a pointer is passed to such an expression, it will compile, but the result will be meaningless; the string-plugin warning finds such mistakes.

If format-plugin is enabled, you can accept the default list of wide-string formatting functions (such as wprintf), or define your own list and pass it to the plugin as follows:

    -Xclang -plugin-arg-format-plugin -Xclang -list=functions.txt

See the file format-functions.txt for the default list and a description of the format.  This enables format checking on functions that accept wide strings, including UTF-16 and UTF-32 strings.
