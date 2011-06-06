from matplotlib import rcParams, rcdefaults, use
_multiprocess_can_split_ = True
def setup():
    use('Agg', warn=False) # use Agg backend for these tests
    rcdefaults() # Start with all defaults
    rcParams['font.family'] = 'Bitstream Vera Sans'
    rcParams['text.hinting'] = False
