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



__license__ = """
ResourcePackage License
        Copyright (c) 2003, Michael C. Fletcher, All rights reserved.
        Redistribution and use in source and binary forms, with or without
        modification, are permitted provided that the following conditions
        are met:
                Redistributions of source code must retain the above copyright
                notice, this list of conditions and the following disclaimer.
                Redistributions in binary form must reproduce the above
                copyright notice, this list of conditions and the following
                disclaimer in the documentation and/or other materials
                provided with the distribution.
                The name of Michael C. Fletcher, or the name of any Contributor,
                may not be used to endorse or promote products derived from this
                software without specific prior written permission.
        THIS SOFTWARE IS NOT FAULT TOLERANT AND SHOULD NOT BE USED IN ANY
        SITUATION ENDANGERING HUMAN LIFE OR PROPERTY.
        THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
        ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
        LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
        FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
        COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
        INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
        (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
        SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
        HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
        STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
        ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
        OF THE POSSIBILITY OF SUCH DAMAGE.
"""

