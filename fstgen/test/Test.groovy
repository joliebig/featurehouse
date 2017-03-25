def lista = ["a","b","c"]
def devMap = []
devMap = [name:'John', framework:'Grails', 'language':'Groovy']
devMap.put('lastName','John')
print devMap

array = (0..20).toArray()
x = 0
for (i in array) {
	x += i
}

def soma(Integer x, Integer y) { x + y }