flatcurry-annotated
===================

This package contain libraries to represent FlatCurry programs with
arbitrary annotations. Furthermore, it contains libraries to annotate
each expression occurring in a given FlatCurry program with type
information. For this purpose, the library `FlatCurry.Annotated.TypeInference`
exports a main operation

    inferProg :: Prog -> IO (Either String (AProg TypeExpr))

which annotates a FlatCurry program with type information.


A previous version of these libraries were part of the
PAKCS/KiCS2 distributions.
