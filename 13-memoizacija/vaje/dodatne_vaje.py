from functools import lru_cache

###############################################################################
# Napisite funkcijo [najdaljse_narascajoce_podazporedje], ki sprejme seznam in
# poisce najdaljse (ne strogo) narascajoce podzaporedje stevil v seznamu.
#
# Na primer: V seznamu [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9] je najdaljse naj vrne
# rezultat [2, 3, 4, 4, 6, 7, 8, 9].
###############################################################################


def najdaljse_narascajoce_podzaporedje(sez):

    @lru_cache(maxsize=256)
    def najdaljse(i, meja):
        #Iščemo najdaljše podzaporedje od indeksa i naprej
        if i >= len(sez):
            return []
        #Ali sprejmemo i-ti element
        elif sez[i] < meja:
        #Preskočimo
            return najdaljse(i+1, meja)
        else:
            sprejmemo = [sez[i]] + najdaljse(i+1, sez[i])
            zavrnemo = najdaljse(i+1, meja)
            if len(sprejmemo) > len(zavrnemo):
                return sprejmemo
            else:
                return zavrnemo
    return najdaljse(0, sez[0])




###############################################################################
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati

# Robotek se lahko premika le gor, dol, levo in desno, ter ima omejeno količino
# goriva. Napišite funkcijo [pobeg], ki sprejme matriko, ki predstavlja sobo,
# začetno pozicijo in pa število korakov, ki jih robotek lahko naredi z
# gorivom, in izračuna ali lahko robotek pobegne. Soba ima vedno vsaj eno
# polje.
#
# Na primer za laboratorij:
# [[0, 1, 0, 0, 2],
#  [0, 2, 2, 0, 0],
#  [0, 0, 2, 2, 0],
#  [2, 0, 0, 2, 0],
#  [0, 2, 2, 0, 0],
#  [0, 0, 0, 2, 2]]
#
# robotek iz pozicije (3, 1) pobegne čim ima vsaj 5 korakov, iz pozicije (5, 0)
# pa v nobenem primeru ne more, saj je zagrajen.
###############################################################################

soba = [[0, 1, 0, 0, 2],
        [0, 2, 2, 0, 0],
        [0, 0, 2, 2, 0],
        [2, 0, 0, 2, 0],
        [0, 2, 2, 0, 0],
        [0, 0, 0, 2, 2]]


def pobeg(soba, pozicija, koraki):
    dimx = len(soba)
    dimy = len(soba[0])

    @lru_cache(maxsize=None)
    def pobegni(vrsta, stolpec, koraki):
        if not (0 <= vrsta < dimx) or not (0 <= stolpec < dimy):
            return False #Nisem v sobi
        elif soba[vrsta][stolpec] == 1:
            return True
        elif koraki > 0 and soba[vrsta][stolpec] == 0:
            return any(
                [pobegni(vrsta + 1, stolpec, koraki - 1),
                 pobegni(vrsta - 1, stolpec, koraki - 1),
                 pobegni(vrsta, stolpec + 1, koraki - 1),
                 pobegni(vrsta, stolpec - 1, koraki - 1)]) # ce katerikoli od teh uspe mi je uspelo
        return False #ce je na oviri
    return pobegni(pozicija[0], pozicija[1], koraki)
