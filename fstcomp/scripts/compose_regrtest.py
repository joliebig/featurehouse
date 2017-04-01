'''

composes a number of products for regression testing

You may have to run `ant jar` beforehand, to create the featurehouse jar file.
Make sure, you have the featurehouse_fstcomp_examples checked out at fstcomp/examples.
Also, this script requires the variability encoding example product lines.
To get those:

$ cd fstcomp/scripts
$ sh introduce_varenc_examples.sh

This will fetch those examples using wget and places them in the example directory.
Important: make sure, you run the script from within the scripts directory.


For regression tests:


- establish a baseline using a "known, good version of featureHouse":

$ cd fstcomp/scripts
$ python compose_regrtest.py ../../jar/featurehouse_xxx.jar ../examples ./baseline 2>baseline_errors.txt 1>baseline_output.txt


- hack, hack, hack, compile, and compose products again:

$ ant jar
$ cd fstcomp/scripts
$ python compose_regrtest.py ../../jar/featurehouse_xxx_new.jar ../examples ./ 2>myversion_errors.txt 1>myversion_output.txt

- use your favourite diff-tool to compare baseline and myversion to make sure your edits do not break anything (meld works well).

'''

from batch_fstcomp import *
import compose_examples

VARENC_EXAMPLES = '''VarEncC/elevator/maxproduct.features
VarEncC/email/maxproduct.features
VarEncC/minepump/maxproduct.features
VarEncJava/elevator/maxproduct.features
VarEncJava/email/maxproduct.features
VarEncJava/minepump/maxproduct.features
VarEncJava/zipme/maxproduct.features'''.split()


COMPOSITION_PLAN = (
    compose_examples.COMPOSITION_PLAN
    + products(
        VARENC_EXAMPLES,
        product_filter=lambda e: e.startswith('VarEncC/'),
        #output_prefix='Varenc',
        extra_args='--liftC'
    )
    + products(
        VARENC_EXAMPLES,
        product_filter=lambda e: e.startswith('VarEncJava/'),
        #output_prefix='Varenc',
        extra_args='--liftJava'
    )
)

if __name__ == '__main__':
    batch_compose_from_cmdline(
        COMPOSITION_PLAN
    )
