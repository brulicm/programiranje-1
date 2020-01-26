from functools import lru_cache

def skoki(seznam):
    k = len(seznam)

    @lru_cache(maxsize=None)
    def pomozna(pozicija, e):
        if pozicija >= k - 1:
            return 0
        e += seznam[pozicija]
        moznosti = [pomozna(pozicija + i, e - i)  for i in range(1, e + 1)]
        return 1 + min(moznosti)
    return pomozna(0, 0)
        
            