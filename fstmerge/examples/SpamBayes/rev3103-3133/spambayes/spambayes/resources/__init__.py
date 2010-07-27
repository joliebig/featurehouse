"""Design-time __init__.py for resourcepackage
This is the scanning version of __init__.py for your
resource modules. You replace it with a blank or doc-only
init when ready to release.
"""

try:

    __file__

except NameError:

    pass

else:

    import os

    if os.path.splitext(os.path.basename( __file__ ))[0] == "__init__":

        try:

            from resourcepackage import package, defaultgenerators

            generators = defaultgenerators.generators.copy()

        except ImportError:

            pass

        else:

            package = package.Package(
                    packageName = __name__,
                    directory = os.path.dirname( os.path.abspath(__file__) ),
                    generators = generators,
            )

            package.scan(
            )



try:

    __file__

except NameError:

    pass

else:

    import os

    if os.path.splitext(os.path.basename( __file__ ))[0] == "__init__":

        try:

            from resourcepackage import package, defaultgenerators

            generators = defaultgenerators.generators.copy()

        except ImportError:

            pass

        else:

            package = package.Package(
                    packageName = __name__,
                    directory = os.path.dirname( os.path.abspath(__file__) ),
                    generators = generators,
            )

            package.scan(
            )



