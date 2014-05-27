'''

batch_fstcomp.py

batch compose products using fstcomp, e.g. for regression tests.

See compose_examples.py and compose_regrtest.py for examples on how to use this.
'''

import os
import sys

DEFAULT_SOURCE_DIR = '../examples'
DEFAULT_OUTPUT_DIR = './output'

def products(productlist, product_filter=None, output_prefix='', extra_args='', base_dir=None):
    '''
    construct a list of products to compose from a list of product configuration files;
    the result is a list containing a dict per product that encapsulates product configuration and composer configuration options.

    productlist - list of paths to product configuration files

    product_filter - callable that is applied to each item of productlist; can be used to select a subset of products

    output_prefix - prefix output directory of products: this is usefull if the same product is composed multiple times e.g. using different composer configuration options

    extra_args - pass extra arguments to fstcomposer when composing those products

    base_dir - specify a base-directory; leave this empty to use the directory the product configuration file is located
    '''
    result = []

    for productfile in productlist:
        if product_filter:
            if not product_filter(productfile): continue

        product=dict(
            name = output_prefix + productfile.split('.')[0].replace('/', '_'),
            productfile=productfile,
            extra_args=extra_args,
            base_dir=base_dir
        )

        result.append(product)

    return result



def fstcomp(jar_path, args):
    '''
    call fstcomp with args using the jar specified by jar_path
    '''
    cmdline = 'java -cp %s composer.FSTGenComposer %s' % (jar_path, args)
    print ' ### CALLING : ' + cmdline
    os.system(cmdline)


def batch_compose(jar_path, composition_plan, source_dir=None, out_dir=None):
    '''
    batch compose products specified by composition_plan using the selected fstcomp jar

    jar_path - path to featureHouse jar
    composition_plan - list of product dicts in the format returned by products

    source_dir - all product configuration files are relative to this directory

    out_dir - directory where composed products will be placed
    '''

    if not source_dir:
        source_dir = DEFAULT_SOURCE_DIR

    if not out_dir:
        out_dir = DEFAULT_OUTPUT_DIR

    if not os.path.isdir(out_dir):
        print 'creating output directory "%s"' % out_dir
        os.mkdir(out_dir)

    print
    print 'using composer: ' + jar_path
    print '-' * 60
    for product in composition_plan:

        name = product['name']
        expression = os.path.join(source_dir, product['productfile'])
        base_dir = product['base_dir'] or os.path.dirname(expression)
        extra_args = product['extra_args']
        comp_dir = os.path.join(out_dir, name)

        if os.path.isdir(comp_dir):
            print ('Directory %s already exists! Skipping!' % comp_dir)
        else:
            os.mkdir(comp_dir)

            args ='--base-directory %s --expression %s %s --output-directory %s' % (
                base_dir,
                expression,
                extra_args,
                comp_dir
            )

            fstcomp(jar_path, args)


def batch_compose_from_cmdline(composition_plan):
    '''
    batch compose products specified by composition_plan (in the format returned by products)

    path to featurehouse jar, source_dir and out_dir are assumed to be given on the command line
    '''

    if len(sys.argv) != 4:
        print 'Usage: %s <path_to_featurehouse_jar> <source_dir> <output_dir>' % sys.argv[0]
        print
        if '__main__' in sys.modules:
            main_module = sys.modules['__main__']
            if hasattr(main_module, '__doc__'):
                print main_module.__doc__

    else:

        jar_path = sys.argv[1]
        src_dir = sys.argv[2]
        out_dir = sys.argv[3]
        if not os.path.isfile(jar_path):
            print 'Jar not found: %s' % jar_path

        elif not os.path.isdir(src_dir):
            print 'Source directory not found: %s' % src_dir

        else:
            batch_compose(jar_path, composition_plan, source_dir=src_dir, out_dir=out_dir)

