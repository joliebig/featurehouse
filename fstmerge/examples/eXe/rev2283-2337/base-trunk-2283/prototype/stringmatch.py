"""
char *search( k, pat, text )
int k;
char *pat, *text;
{
    int j, m, count;
    m = strlen(pat);
    if( m <= k )
        return( text );
    for( ; *text != EOS; text++ ) {
        for( count=j=0; j < m && count <= k; j++ )
            if( pat[j] != text[j] )
                count++;
        if( count <= k ) 
            return( text );
    }
    return( NULL );
}
char *search( maxMisses, guess, answer )
int maxMisses;
char *guess, *answer;
{
    int i, ln, misses;
    ln = strlen(guess);
    if( ln <= maxMisses )
        return( answer );
    for( ; *answer != EOS; answer++ ) {
        for( misses=i=0; i < ln && misses <= maxMisses; i++ )
            if( guess[i] != answer[i] )
                misses++;
        if( misses <= maxMisses ) 
            return( answer );
    }
    return( NULL );
}
"""
def match(guess, answer, verbose=False):
    maxMisses = len(answer)/4+1
    print maxMisses,
    if len(guess) <= maxMisses:
        misses = abs(len(guess) - len(answer))
        for ch in guess:
            if ch not in answer:
                misses += 1
        if misses <= maxMisses:
            return (0, misses)
        else:
            return False
    iterations = 0
    for string1, string2 in [(answer, guess), (guess, answer)]:
        while string1:
            iterations += 1
            print len(string1), len(string2) 
            print string1, string2,
            misses = (abs(len(string1) - len(string2)) +
                     (abs(len(guess) - len(answer)))) / 2
            print misses
            if verbose:
                print 'checking:', string1
            for a, b in zip(string2, string1):
                if a != b:
                    misses += 1
                if misses > maxMisses:
                    break
            if misses <= maxMisses:
                return iterations, misses
            string1 = string1[1:]
    return False
strings = [
    ('fisio', 'phisiotherapist'),
    ]
for guess, answer in strings:
    print guess, answer,
    print match(guess, answer) or 'FAIL'
