import hotshot, hotshot.stats, test.pystone
from example import main
prof = hotshot.Profile("useinstance.prof")
prof.runcall(main)
prof.close()
