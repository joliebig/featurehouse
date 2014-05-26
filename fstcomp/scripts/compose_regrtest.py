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
