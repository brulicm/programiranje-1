# 3. naloga
from functools import lru_cache

def simetricen(niz):
    return niz[::-1] == niz

@lru_cache(maxsize=None)
def stevilo_delov(w, je_simetricen):
    if w == []:
        return 0
    if je_simetricen(w):
        return 1

    options = [stevilo_delov(w[:i], je_simetricen) +
               stevilo_delov(w[i:], je_simetricen) for i in range(1, len(w))]

    return min(options)


def vsotno_simetricen(niz):
    if len(niz) <= 1:
        return true
    seznam = list(niz)
    n = len(seznam)

    prvi_del = seznam[:: n // 2 + 1]
    drugi_del = seznam[n // 2 + 1 ::]
    return sum(int(x) for x in prvi_del) == sum(int(x) for x in drugi_del)